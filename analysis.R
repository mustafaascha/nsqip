
#library(Matching)
library(MatchIt)
library(mice)

source("munge.R")

make_je_nsqip <- function(which_df) make_nsqip(1:6, which_df)
nsqip <- 
  map(c("everyone", "cosmetics", "no cosmetics"), make_je_nsqip) 
names(nsqip) <- c("everyone", "cosmetics", "no_cosmetic")

#tbl_ones_pgy <- map(nsqip, tbl_one, strat = "pgy_bin")
tbl_ones <- map(nsqip, tbl_one, strat = "admqtr")

nsqip <- nsqip %>% map(propensity_score)
paper_stuff$mona[["predictors"]] <-
   c(paper_stuff$mona[["predictors"]], "propensity_score")

nsqip <- nsqip %>% map(binarize_outcomes)
#check which variables are cool
#map(nsqip, function(x) map(x[paper_stuff$mona$outcomes], table))

#glm prep
otl <- outcomes_list <- list()
#nsqip[[1]]
otl[["everyone"]] <- paste0(paper_stuff$mona$outcomes, "_01")
otl[["cosmetics"]] <- paste0(c("returnor", "supinfec", "wndinfd"), "_01")
otl[["no_cosmetic"]] <- paste0(c("returnor", "supinfec", "wndinfd"), "_01")

model_vars <- function(df){
  select(df, "pgy01", "pgy_bin", "propensity_score", "admqtr",
         one_of(paper_stuff$mona$predictors, 
                unique(unlist(otl))))
}
nsqip <- map(nsqip, model_vars)

impute_fn <- function(df) {
  mice(df, m = 1, maxit = 100, seed = 400, ridge = 0.01)
}
#imputes <- map(nsqip, impute_fn)
impute_df <- function(df) mice::complete(impute_fn(df), 1)
nsqip <- map(nsqip, impute_df)

match_fn <- function(df) {
  match.data(matchit(pgy01 ~ 
                       age + sex + bmi + race + smoke + 
                       diabetes + hypermed + workrvu + 
                       optime + inout + tothlos + wndclas + 
                       asaclas + attend, 
                     method = "nearest", data = df))
}
matched_data <- map(nsqip, match_fn)

#match_fn(nsqip[[1]])
do_prop_glm <- function(df, the_outcome){
  the_form <- 
    paste0(the_outcome, " ~ ", 
           #paste0(c("pgy_bin", paper_stuff$mona$predictors, "propensity_score"), 
           paste0(c("pgy_bin*admqtr", "propensity_score"),
                  collapse = " + "))
  glm(the_form, "binomial", df)
}

do_bi_glm <- function(df, the_outcome){
  the_form <- 
    paste0(the_outcome, " ~ ", "pgy_bin*admqtr", collapse = " + ")
  glm(the_form, "binomial", df)
}

do_glms <- function(m_dfs, dfs){
  library(zeallot)
  c(glms, df_names) %<-% list(list(), names(dfs))
  for(i in seq_along(dfs)){
    df_name <- df_names[i]
    bi_name <- paste0("bi_", df_names[i], collapse = "")
    glms[[df_name]] <- map(otl[[df_name]], ~ list(do_prop_glm(dfs[[df_name]], .x), .x))
    glms[[bi_name]] <- map(otl[[df_name]], ~ list(do_bi_glm(dfs[[df_name]], .x), .x))
  }
  glms
}

glms <- do_glms(matched_data, nsqip)



#==============================
#goal: make a propensity-adjusted model for each outcome, for each population
tidied_models <-
  map2(glms, names(glms),
       function(dflist, dfnm) {
         map(dflist, function(mdl) {
           mutate(broom::tidy(mdl[[1]]), glm = dfnm, outcome = mdl[[2]])
             })
         }) %>% 
  flatten_dfr

flatten_matrices_df <- function(matrices_list){
  matrices_list <- flatten(matrices_list)
  the_rownames <- unlist(map(matrices_list, rownames))
  the_df <- bind_rows(map(matrices_list, data.frame, stringsAsFactors = FALSE))
  the_df[["rownames"]] <- the_rownames
  names(the_df) <- c("conf.lower", "conf.upper", "glm", "outcome", "term")
  the_df
}

confint_models <-
  map2(glms, names(glms),
       function(dflist, dfnm) {
         map(dflist, function(mdl) {
           cbind(confint(mdl[[1]]), glm = dfnm, outcome = mdl[[2]])
           })
         }) %>% 
  flatten_matrices_df


models_df <- full_join(tidied_models, confint_models)
models_df <-
  models_df[,!grepl("1$", names(models_df))] %>%
  select(-statistic, -std.error)

to_exp <- c("estimate", "conf.lower", "conf.upper")
models_df[,to_exp] <- lapply(models_df[,to_exp], function(x) {
  sprintf("%.4f", exp(as.numeric(x)))})

for_show <- c("term", "glm", "outcome", "estimate", "conf.lower", "conf.upper", "p.value")
models_df <- models_df[,for_show]
models_df$outcome <- gsub("_01$", "", models_df$outcome)

models_df <- models_df %>% 
  filter(!(term %in% c("(Intercept)", "propensity_score"))) %>% 
  arrange(desc(outcome))

#===================
test_diff <- function(x, df) UseMethod("test_diff")
test_diff.numeric <- function(x, df){
  t.test(x ~ pgy_bin, data = df)
}

cats <- c("admqtr", "sex", "race", "smoke", "diabetes",        
          "hypermed", "inout", "wndclas", "asaclas", "attend")
          
conts <- c("propensity_score", "age", "bmi", "workrvu", 
           "optime", "tothlos")
outs <- c("returnor_01", "supinfec_01", "wndinfd_01")












