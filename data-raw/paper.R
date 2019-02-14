
mona <- 
  list(
    ftr = list(to_exclude = "refs/CPT Codes TO EXCLUDE FROM ALL COHORTS.docx", 
               cosmetic = "refs/Cosmetic CPT Codes1.docx")
              )

mona <- rlist::list.append(mona, 
  predictors = c("age", "sex",
    "bmi","race","smoke","diabetes","hypermed" ,"workrvu",
    "optime","inout","tothlos","wndclas","asaclas","attend"),
  outcomes = c("returnor",
    "supinfec","wndinfd"
    ),
  exclusion = c("surgspec", "admqtr", "proper30"),
  cosmetics = gsub("\\ .*", "", 
                   textreadr::read_docx(mona$ftr$cosmetic)),
  to_exclude = gsub("\\ .*", "",  
                    textreadr::read_docx(mona$ftr$to_exclude))
)

vti <- vars_to_investigate <- list(
  predictors = 
    c("pgy", "admqtr", "admyr", 
      "proper30", "age", "sex", "race", "smoke", "bmi", "race_new", 
      "diabetes",  "dyspnea", "cpneumon", "ascites", "esovar", "prvpci",
      "hypermed", "hxangina", "hxchf", "hxmi", "hxcopd", "attend",
      "optime", "inout", "tothlos", "wndclas", "asaclas", "workrvu"),
  labs = 
    c("prsodm", "prbun", "prcreat", "pralbum", "prbili", "prsgot", "pralph", 
      "prwbc", "prhct", "prplate", "prptt", "prinr", "prpt"),
  outcomes = 
    c("returnor",  
      "cdarrest",   "cdmi",        "cnscoma",            
      "cnscva",     "neurodef", "othbleed", 
      "othdvt",     "othsysep",    "urninfec",           
      "oprenafl",   "oupneumo",   "reintub", 
      "pulembol",   "supinfec",    "wndinfd",            
      "orgspcssi",  "dehis"))
vti$tbl_one_vars <- with(vti, c(predictors, outcomes, labs))

#is organ space infection the same as a deep infection? 
#Fischer, Wes, Kovach 2014 J Plast Surg Hand Surg "The impact of 
# surgical resident participation in breast reduction surgery – 
# Outcome analysis from the 2005–2011 ACS-NSQIP datasets"
fischer <- list(
  any_surgical_complication = c("supinfec", "wndinfd",
                             "orgspcssi",    "dehis", "reoperation",  
                             "reoperation1", "reoperation2", "reoperation3"),
  major_surgical_complication = c("wndinfd",  "orgspcssi",
                              "reoperation",  "reoperation1", "reoperation2", "reoperation3"),
  any_surgical_complication = c("supinfec",  "dehis"))


paper <- 
  list(
    mona = mona, 
    fischer = fischer, 
    vti = vti)
rm(mona, fischer, vars_to_investigate, vti)

devtools::use_data(paper)

#=====================================================

#https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html

#names(nsqip)[grep("rea", names(nsqip))]

# jordan stuff
# Resident Involvement and Plastic Surgery
# Outcomes: An Analysis of 10,356 Patients from
# the American College of Surgeons National Surgical Quality Improvement 
# Program Database. Sumanas W. Jordan, M.D., Lauren M. Mioton, B.S., 
# John Smetona, B.A., B.S. Apas Aggarwal, Edward Wang, Ph.D., 
# Gregory A. Dumanian, M.D., John Y. S. Kim, M.D. PRS Recon Outcomes 2014
# jordan <- list()
# jordan[["table_one_vars"]] <- 
#   c("age", "bmi", "race_new", "admyr", "fnstatus2", "smoke", 
#     "etoh", "asaclas", "inout", "optime", "wndclas")
# 
# jordan[["comorbidities"]] <- 
#   c("diabetes", "hxcopd", "hxchf", "hxmi", "hxangina", 
#     "hxpvd", "hxtia")
