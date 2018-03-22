#' Return the matching regex
#' 
#' I suspect there's a better way to do this.
#' 
#' @param x The thing to examine
#' @param p The pattern to grep
#' @param v Whether to return non-matching characters
#' 
#' @export

grep_raw <- function(x, p, v = FALSE){
  regmatches(x, regexpr(p, x), invert = v)
}



#' Title
#'
#' @param x 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
change_names_to <- function(x, df){
  warning("This function was not used for the manuscript, 
          and is retained solely for exposition.")
  names(df) <- x
  df
}

#'Convert Data Frame Names to Lower Case 
#'
#'@param df The dataframe the function takes.
#'
names_to_lower <- function(df) {
  names(df) <- tolower(names(df))
  df
}


#' Read NSQIP file
#' 
#' @param filepath This is the filepath pointing to our NSQIP TXT file. 
#' 
#'@import readr
#'@export
read_nsqip <- function(filepath) {
  read_tsv(file = filepath, 
           col_types = cols(.default = "c"), guess_max = 0, 
           na = c("9999", "999", "-99", "NULL"))
}

#' Title
#'
#' @param df 
#' @param strat 
#'
#' @return
#' @export
#'
#' @examples
tbl_one <- function(df, strat) {
  print(tableone::CreateTableOne(c("surgspec", "pgy_bin", 
                                   paper_stuff$mona$predictors, 
                                   paper_stuff$vti$outcomes), 
                       strata = strat, data = df), printToggle = FALSE)
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
full_df <- function(df, the_outcome) {
  df <- df[,c("pgy01", the_outcome, paper_stuff$mona$predictors)]
  df[complete.cases(df),]
}



#' Title
#'
#' @param fct 
#'
#' @return
#' @export
#'
#' @examples
binarize <- function(fct){
  the_levels <- unique(fct[!is.na(fct)])
  stopifnot(length(the_levels) == 2)
  as.numeric(fct == the_levels[1])
}


#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
binarize_outcomes <- function(df){
  df[,paste0(paper_stuff$mona$outcomes, "_01")] <- 
    lapply(df[,paper_stuff$mona$outcomes], binarize)
  df
}


#' @param df 
#'
#' @param the_outcome 
#' @param M 
#'
#'@import zeallot
#'@import Matching
match_pop <- function(df, the_outcome, M = 1) {
  warning("This function was not used for the manuscript, 
          and is retained solely for exposition.")
  set.seed(432431)
  df <- full_df(df, the_outcome)
  c(Tr, Y, X) %<-% list(df[["pgy01"]], df[[the_outcome]],
                        data.matrix(df[,paper_stuff$mona$predictors]))
  Match(Y = Y, Tr = Tr, X = X, M = M, version = "fast")
}


#' Title
#'
#' @param the_data 
#'
#' @return
#' @export
#'
#' @examples
get_pgy <- function(the_data) {
  warning("This function was not used for the manuscript, 
          and is retained solely for exposition.")
  UseMethod("get_pgy")
}

#' @param the_data 
#'
#'@import tidyverse
get_pgy.data.frame <- function(the_data) {
  filter(the_data, grepl("pgy_bin", term))
}

#' @param the_data 
#'
#'@import tidyverse
get_pgy.matrix <- function(the_data){
  term <- rownames(the_data)
  colnames(the_data) <- c("conf.lower", "conf.upper", "glm")
  to_return <- data.frame(the_data, term = term)
  filter(to_return, grepl("pgy_bin", term))
}








