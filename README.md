# ACS-NSQIP analysis for a study of the "July Effect"

[![DOI](https://zenodo.org/badge/117899615.svg)](https://zenodo.org/badge/latestdoi/117899615)

# Purpose

This package is meant to document and increase the reproducibility of a paper examining the "July Effect" among procedures recorded in the [American College of Surgeons National Surgical Quality Improvement Program (ACS-NSQIP).](https://www.facs.org/quality-programs/acs-nsqip)

# Instructions

After extracting the ACS-NSQIP data, place the .txt data files into the project folder "data/txt". By default, the report will use the first six files in the "txt" data folder. This is because the "July Effect" analysis requires resident PGY data, which was available only for a select number of years.

This work depends on R, pandoc, and several R packages: tidyverse, pander, knitr, rmarkdown, mice, devtools, zeallot, Epi, and broom.  

Assuming the years in the ACS-NSQIP "data/txt/" folder correspond to those reported in our manuscript, "report.Rmd" may be knit to reproduce that manuscript. Alternatively, Mac or Linux users may navigate to the working directory and reproduce these results using a single command: `make`. 


# Project structure

Before adding the data to the correct folder, the project structure looks like this:

nsqipr    
├── data/txt  
│       └── PUT-DATA-HERE   
├── data-raw   
│   └── paper_things.R   
...
├── README.md   
└── report.Rmd   


After adding the data, the project directory should have (about) the following structure:

nsqipr   
├── data/txt    
│       ├── ACS_NSQIP_PUF_05_06_vr1.txt.gz    
│       ├── ACS_NSQIP_PUF07_TXT.txt.gz    
│       ├── ACS_NSQIP_PUF08_TXT.txt.gz    
│       ├── ACS_NSQIP_PUF09_TXT.txt.gz    
│       ├── ACS_NSQIP_PUF10_TXT.txt.gz    
│       ├── ACS_NSQIP_PUF11_TXT.txt.gz    
│       ├── acs_nsqip_puf12.txt.gz    
│       ├── acs_nsqip_puf13.txt.gz    
│       ├── acs_nsqip_puf14.txt.gz    
│       ├── acs_nsqip_puf15_v2.txt.gz    
│       └── PUT-DATA-HERE    
├── data-raw    
│   └── paper_things.R    
...
├── README.md    
└── report.Rmd    






