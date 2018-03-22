


#' Run GLMS for each of matched and unmatched cohorts
#'
#'
#' @param m_dfs Matched data.frames
#' @param dfs Unmatched data.frames
#'
#'@import purrr
#'@export
do_glms <- function(m_dfs, dfs){
  if(!(exists("otl"))){
    warning("Outcomes not specified, falling back to defaults.")
    otl <- outcomes_list <- list()
    otl[["everyone"]] <- paste0(c("returnor", "supinfec"), "_01")
    otl[["cosmetics"]] <- paste0(c("returnor", "supinfec"), "_01")
    otl[["no_cosmetic"]] <- paste0(c("returnor", "supinfec"), "_01")
  }
  
  do_non_prop_glm <- function(df, the_outcome){
    the_form <- 
      paste0(the_outcome, " ~ ", "admqtr + pgy_bin", collapse = " + ")
    glm(the_form, "binomial", df)
  }
  
  do_prop_glm <- function(df, the_outcome){
    the_form <- 
      paste0(the_outcome, " ~ ", 
             #paste0(c("pgy_bin", paper_stuff$mona$predictors, "propensity_score"), 
             paste0(c("admqtr + pgy_bin", "propensity_score"),
                    collapse = " + "))
    glm(the_form, "binomial", df)
  }
  
  glms <- list()
  df_names <- names(dfs)
  for(i in seq_along(dfs)){
    df_name <- df_names[i]
    np_name <- paste0("no_prop_", df_names[i], collapse = "")
    glms[[df_name]] <- map(otl[[df_name]], ~ list(do_prop_glm(m_dfs[[df_name]], .x), .x))
    glms[[np_name]] <- map(otl[[df_name]], ~ list(do_non_prop_glm(dfs[[df_name]], .x), .x))
  }
  glms
}



#' Get c-statistic and Hosmer-Lemeshow statistics for GLMs
#'
#' @param dflist A list of data.frames
#' @param dfnm The name of each data.frame
#'
#' @return A data.frame with one row
#' @export
#'
classification_metrics <- function(dflist, dfnm) {
  library(pROC)
  library(ResourceSelection)
  map(dflist, function(mdl) {
    predictions <- predict(mdl[[1]], type = "response")
    the_outcome <- mdl[[2]]
    outcome_values <- mdl[[1]][["data"]][[the_outcome]]
    test <- hoslem.test(outcome_values, predictions, g = 5)
    h_l <- hoslem.test(outcome_values, predictions, g = 5)[["p.value"]]
    c_s <- auc(roc(outcome_values ~ predictions))
    data.frame(c_stat = c_s, 
               h_l_stat =  h_l, 
               glm = dfnm, 
               outcome = the_outcome, 
               stringsAsFactors = FALSE
    )
  })
}


