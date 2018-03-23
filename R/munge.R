



#' Make a NSQIP data.frame
#'
#'
#' @param which_files This is a numeric vector specifying which NSQIP data,
#'                    in alphabetical order, is to be read
#' @param study_pop Three cohorts were used for this work: everyone, people
#'   having cosmetics procedures, and people having non-cosmetic procedures
#'
#'@import tidyverse
#'
make_nsqip <- function(which_files = "default", study_pop = "everyone"){
  paper_stuff <- nsqipr::paper_stuff
  stopifnot(study_pop %in% c("everyone", "cosmetics", "no cosmetics"))
  if(exists("the_cache")) {
    if("nsqip" %in% ls(.GlobalEnv[["the_cache"]])){
      nsqip <- .GlobalEnv[["the_cache"]][["nsqip"]]
      if(study_pop == "everyone") {
        to_return <- nsqip
      } else if(study_pop == "cosmetics") {
        to_return <- nsqip %>% filter(cpt %in% paper_stuff$mona$cosmetics)
      } else if(study_pop == "no cosmetics") {
        to_return <- nsqip %>% filter(!(cpt %in% paper_stuff$mona$cosmetics))
      }
      return(to_return)
    }
  } else {
    the_cache <<- list()
  }
  for_io <- list()
  #remove year 2015 because it looks like there's no pgy
  for_io[["to_read"]] <- list.files("data/txt", pattern = "txt.gz", full.names = TRUE)
  #this logic probably doesn't need to be so complicated
  if(which_files != "default"){
    for_io[["to_read"]] <- for_io[["to_read"]][which_files]#[-length(for_io[["to_read"]])]
  } else {
    for_io[["to_read"]] <- for_io[["to_read"]]#[-length(for_io[["to_read"]])]
  }
  grep_raw <- function(x, p, v = FALSE) regmatches(x, regexpr(p, x), invert = v)
  for_io[["yrs"]] <- grep_raw(for_io[["to_read"]], "[0-9]+")
  for_io[["new_names"]] <- paste0("ACS_", for_io[["yrs"]])
  the_data <- 
    map(for_io$to_read, 
        function(filepath) {
          read_tsv(file = filepath, 
                   col_types = cols(.default = "c"), guess_max = 0, 
                   na = c("9999", "999", "-99", "NULL"))
        }) %>%  
    map(function(df) {
          names(df) <- tolower(names(df))
          df
        })
  names(the_data) <- for_io$new_names
  
  merge_race <- function(df){
    if(any(grepl("race_new", names(df)))){
      df[["race"]] <- df[["race_new"]]
    }
    df
  }
  the_data <- map(the_data, merge_race)
  
  for_merge <- list()
  for_merge[["data_names"]] <- map(the_data, names)
  names(for_merge[["data_names"]]) <- for_io$new_names
  for_merge[["vars_in_all_dfs"]] <- with(for_merge, reduce(data_names, intersect))
  nsqip <- with(for_merge, map(the_data, ~ .x[,vars_in_all_dfs]))
  nsqip <- bind_rows(nsqip)
  
  nsqip$bmi <- with(nsqip, 
    (as.numeric(weight) * 0.453592) / ((as.numeric(height) * 0.0254) ^ 2))
  nsqip$pgy_bin <- with(nsqip, 
         ifelse(pgy %in% as.character(0:3), "Three or lower", 
                ifelse(pgy %in% as.character(4:11), "Four or above", NA)))
  nsqip$pgy01 <- as.numeric(nsqip$pgy %in% as.character(0:3))
  # exclusions
  nsqip <- nsqip %>% 
    filter(surgspec == "Plastics" & proper30 != "Yes" &#admqtr %in% c(2, 3) & 
             age != "90+" & age > 17 & 
             asaclas != "5-Moribund" & asaclas != "4-Life Threat" & 
             attend != "Attending Not Present, but Available" & 
             attend != "Not entered" & 
             attend != "Attending Alone" & 
             !is.na(pgy_bin)
           )
  nsqip[["attend"]][nsqip[["attend"]] == "Attending in OR Suite"] <- 
    "Attending in OR"
  to_numeric <- c("tothlos", "age", "optime", "workrvu")
  nsqip[,to_numeric] <- lapply(nsqip[,to_numeric], as.numeric)
  nsqip[["race"]] <- 
    forcats::fct_collapse(nsqip[["race"]], 
                          "White" = c("White", "White, Not of Hispanic Origin"), 
                          "Black" = c("Black or African American",  
                                      "Black, Not of Hispanic Origin"), 
                          "Asian" = c("Asian", "Asian or Pacific Islander", 
                                      "Native Hawaiian or Pacific Islander"), 
                          "Other" = c("American Indian or Alaska Native", 
                                      "Hispanic, White", "Hispanic, Black",
                                      "Hispanic, Color Unknown", "Unknown")) 
  nsqip[["othbleed"]] <- 
    forcats::fct_collapse(nsqip[["othbleed"]],
                          "Bleeding/Transfusion" = c("Bleeding/Transfusions", "Transfusions/Intraop/Postop"))
  nsqip[["othdvt"]] <- 
    forcats::fct_collapse(nsqip[["othdvt"]],
                          "DVT" = c("DVT Requiring Therapy", "DVT/Thrombophlebitis"))
  nsqip[["any_complication"]] <- 
    as.numeric(nsqip[["returnor"]] == "Yes" | 
                 nsqip[["supinfec"]] == "Superficial Incisional SSI" | 
                 nsqip[["wndinfd"]] == "Deep Incisional SSI")
  nsqip[["admqtr23_01"]] <- ifelse(nsqip[["admqtr"]] == "2", 0, 
                                   ifelse(nsqip[["admqtr"]] == "3", 1, 
                                          NA))
  nsqip[["admqtr"]] <- ifelse(nsqip[["admqtr"]] %in% c("1", "2", "4"), 0, 
                                   ifelse(nsqip[["admqtr"]] == "3", 3, 
                                          NA)) %>% as.character
  the_cache[["nsqip"]] <<- nsqip
  if(study_pop == "everyone") {
    return(nsqip)
  } else if(study_pop == "cosmetics") {
    nsqip %>% filter(cpt %in% paper_stuff$mona$cosmetics)
  } else if(study_pop == "no cosmetics") {
    nsqip %>% filter(!(cpt %in% paper_stuff$mona$cosmetics))
  }
}

  



#' Make outcomes binary
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
binarize_outcomes <- function(df){
  binarize <- function(fct){
    the_levels <- unique(fct[!is.na(fct)])
    stopifnot(length(the_levels) == 2)
    as.numeric(fct == the_levels[1])
  }
  df[,paste0(paper_stuff$mona$outcomes, "_01")] <- 
    lapply(df[,paper_stuff$mona$outcomes], binarize)
  df
}







