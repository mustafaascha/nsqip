



#' Title
#'
#' @param which_df 
#'
#' @return
#' @export
#'
#' @examples
make_je_nsqip <- function(which_df) {
  make_nsqip(1:6, which_df)
}

#' Title
#'
#' @param which_df 
#'
#' @return
#' @export
#'
#' @examples
make_july_effect_nsqip <- function(which_df) {
  warning("This function was not used for the manuscript, 
          and is retained solely for exposition.")
  make_nsqip(1:6, which_df)
}


#' @param df 
#'
#'@import tidyverse
model_vars <- function(df){
  list(select(df, "pgy01", "pgy_bin", "admqtr", "admqtr23_01",
              one_of(nsqipr::paper_stuff$mona$predictors, 
                     unique(unlist(otl)))), 
       select(df,  "propensity_score"))
}


#'
#'
#'
#'
#' @param df 
#'
#'@import MatchIt
#'@export
match_fn <- function(df) {
  MatchIt::match.data(MatchIt::matchit(pgy01 ~ 
                       age + sex + bmi + race + smoke + 
                       diabetes + hypermed + workrvu + 
                       optime + inout + tothlos + wndclas + 
                       asaclas + attend, 
                     method = "nearest", data = df))
}


#' Title
#'
#' @param df 
#' @param the_outcome 
#'
#' @return
#' @export
#'
#' @examples
do_prop_glm <- function(df, the_outcome){
  the_form <- 
    paste0(the_outcome, " ~ ", 
           #paste0(c("pgy_bin", paper_stuff$mona$predictors, "propensity_score"), 
           paste0(c("admqtr + pgy_bin", "propensity_score"),
                  collapse = " + "))
  glm(the_form, "binomial", df)
}


#' Title
#'
#' @param df 
#' @param the_outcome 
#'
#' @return
#' @export
#'
#' @examples
do_non_prop_glm <- function(df, the_outcome){
  the_form <- 
    paste0(the_outcome, " ~ ", "admqtr + pgy_bin", collapse = " + ")
  glm(the_form, "binomial", df)
}

#'
#'
#'
#' @param m_dfs 
#' @param dfs 
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


#' @param matrices_list 
#'
#'@import tidyverse
#'@export
flatten_matrices_df <- function(matrices_list){
  matrices_list <- flatten(matrices_list)
  the_rownames <- unlist(map(matrices_list, rownames))
  the_df <- bind_rows(map(matrices_list, data.frame, stringsAsFactors = FALSE))
  the_df[["rownames"]] <- the_rownames
  names(the_df) <- c("conf.lower", "conf.upper", "glm", "outcome", "term")
  the_df
}



#' Title
#'
#' @param dflist 
#' @param dfnm 
#'
#' @return
#' @export
#'
#' @examples
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


