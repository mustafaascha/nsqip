


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
             #paste0(c("pgy_bin", paper$mona$predictors, "propensity_score"), 
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




#' Calculate missingness
#'
#' @param df A data.frame whose missingness is to be calculated
#'
#' @return A named vector of double
#' @export
#'
#' @examples
get_missing_prop <- function(df) {
  map_dbl(df, ~ length(which(is.na(.x))) / length(.x))
}

#' Bootstrapped stepwise regression
#'
#' @param mdl A fitted model
#' @param df A data.frame used to produce a fitted model 
#' @param n number of iterations
#' @param n_fraction fraction of df to use per iteration
#'
#' @return
#' @export
#'
#' @examples
boot_step <- function(mdl, df, n, n_fraction = 0.8){
  mdl_form <- mdl$formula
  data_vars <- unlist(strsplit(as.character(mdl_form), "\\ \\+\\ "))[-1]
  full_data <- df[,data_vars]
  full_data <- full_data[complete.cases(full_data),]
  size_to_sample <- n_fraction * nrow(full_data)
  form_chr <- function(frm) unlist(strsplit(as.character(frm),"\\ \\+\\ "))[-1]
  do_model <- function(df2) step(glm(mdl_form, "binomial", df2), trace = FALSE)
  sample_model <- function(df1) do_model(sample_n(df1, size_to_sample))
  mdl_forms <- 
    lapply(seq_len(n), function(x) sample_model(full_data)[["formula"]])
  chr_forms <- lapply(mdl_forms, form_chr)
  table(unlist(chr_forms)) / n
}


#' Make a model pretty for show
#'
#' @param mdl 
#'
#' @return
#' @export
#'
#' @examples
for_show <- function(mdl) {
  to_return <- 
    tbl_df(cbind(broom::tidy(mdl), confint(mdl))) %>% 
    select(term, estimate, p.value, conf.low = `2.5 %`, conf.high = `97.5 %`) %>% 
    mutate(odds_ratio = exp(estimate), 
           conf.low = exp(conf.low),
           conf.high = exp(conf.high)) %>% 
    select(-estimate) %>% 
    select(term, odds_ratio, conf.low, conf.high, p.value) %>% 
    filter(term != "(Intercept)")
  map_if(to_return, is_double, function(x) sprintf("%.3f", x)) %>% tbl_df
}

#' Put model results into a vector of chars
#'
#' @param mdf 
#'
#' @return
#' @export
#'
#' @examples
paste_glm <- function(mdf) {
  library(zeallot)
  mdf[,c("open", "middle", "end")] %<-% list(" (", "-", ")")
  vars_to_paste <- 
    c("odds_ratio", "open", "conf.low", "middle", "conf.high", "end")
  for_paste <- mdf[,vars_to_paste]
  apply(for_paste, 1, paste, collapse = "")
}

#' Do the Epi::twoby2
#'
#' @param x 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
two_by_two <- function(x, y){
  to_return <- Epi::twoby2(x, y, print = FALSE)
  to_return[[2]] <- to_return[[2]][1:2,]
  to_return[[3]] <- to_return[[3]][1]
  to_return
}

#' Tidy a glm
#'
#' @param mdl 
#'
#' @return
#' @export
#'
#' @examples
tidy_glm <- function(mdl){
  bind_cols(broom::tidy(mdl), 
            broom::confint_tidy(mdl)) %>% 
    select(term, estimate, conf.low, conf.high, p.value) %>% 
    modify_at(c("estimate", "conf.low", "conf.high"), exp) %>% 
    filter(term != "(Intercept)")
}



#' Rubin rules
#'
#' @param frmla A formula used to construct a glm propensity-score model
#' @param u_df An unmatched data frame
#' @param m_df A matched data frame
#'
#' @return
#' @export
#'
#' @examples
#' @import zeallot
rubin_rules <- function(frmla, u_df, m_df) {
  library(zeallot)
  
  ps_mdl <- glm(frmla, data = m_df, family = "binomial")
  
  c(u_df, m_df) %<-% 
    map(list(u_df, m_df), 
        function(df) mutate(df, pscore = predict(ps_mdl, df))
    )
  
  matched_on <- formula_predictors(frmla)[1]
  stopifnot(all(unique(u_df[[matched_on]]) %in% c(0, 1, NA)) & 
              all(unique(u_df[[matched_on]]) %in% c(0, 1, NA))
  )
  
  each_groups_pscore <- 
    function(df) map(c(p_0 = 0, p_1 = 1), 
                     ~ df[["pscore"]][df[[matched_on]] == .x]
                     )
  
  map(list(unmatched = u_df, matched = m_df),
      function(df) {
        c(p_0, p_1) %<-% each_groups_pscore(df)
        list(
          rule_1 =
            abs(
              100 * (mean(p_0, na.rm = T) - mean(p_1, na.rm = T)) /
                sd(c(p_0, p_1), na.rm = T)
            ),
          rule_2 = (var(p_0, na.rm = T) / var(p_1, na.rm = T))
          )
      }
  )
}
