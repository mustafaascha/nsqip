#' Return the matching regex
#' 
#' I suspect there's a better way to do this.
#' 
#' @param x The thing to examine
#' @param p The pattern to grep
#' @param v Whether to return non-matching characters

grep_raw <- function(x, p, v = FALSE){
  regmatches(x, regexpr(p, x), invert = v)
}

names_to_lower <- function(df) {
  names(df) <- tolower(names(df))
  df
}

change_names_to <- function(x, df){
  names(df) <- x
  df
}

read_nsqip <- function(filepath) {
  read_tsv(file = filepath, 
           col_types = cols(.default = "c"), guess_max = 0, 
           na = c("9999", "999", "-99", "NULL"))
}

tbl_one <- function(df, strat) {
  print(CreateTableOne(c("surgspec", paper_stuff$mona$predictors, paper_stuff$vti$outcomes), 
                       strata = strat, data = df), printToggle = FALSE)
}

full_df <- function(df, the_outcome) {
  df <- df[,c("pgy01", the_outcome, paper_stuff$mona$predictors)]
  df[complete.cases(df),]
}

propensity_score <- function(df){
  the_form <-
    paste0("pgy01 ~ ",
           paste0(paper_stuff$mona$predictors,
                  collapse = " + "))
  df[["propensity_score"]] <-
    predict(glm(the_form, "binomial", df), df, type = "response")
  df
}

binarize <- function(fct){
  the_levels <- unique(fct[!is.na(fct)])
  stopifnot(length(the_levels) == 2)
  as.numeric(fct == the_levels[1])
}
binarize_outcomes <- function(df){
  df[,paste0(paper_stuff$mona$outcomes, "_01")] <- 
    lapply(df[,paper_stuff$mona$outcomes], binarize)
  df
}



match_pop <- function(df, the_outcome, M = 1) {
  set.seed(432431)
  library(zeallot)
  df <- full_df(df, the_outcome)
  c(Tr, Y, X) %<-% list(df[["pgy01"]], df[[the_outcome]], 
                        data.matrix(df[,paper_stuff$mona$predictors]))
  Match(Y = Y, Tr = Tr, X = X, M = M, version = "fast")
}


get_pgy <- function(the_data) UseMethod("get_pgy")
get_pgy.data.frame <- function(the_data) filter(the_data, grepl("pgy_bin", term))
get_pgy.matrix <- function(the_data){
  term <- rownames(the_data)
  colnames(the_data) <- c("conf.lower", "conf.upper", "glm")
  to_return <- data.frame(the_data, term = term)
  filter(to_return, grepl("pgy_bin", term))
}








