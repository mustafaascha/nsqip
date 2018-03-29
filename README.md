# ACS-NSQIP analysis for a study of the "July Effect"

[![DOI](https://zenodo.org/badge/117899615.svg)](https://zenodo.org/badge/latestdoi/117899615)

# Purpose

This package is meant to document and increase the reproducibility of a paper examining the "July Effect" among procedures recorded in the [American College of Surgeons National Surgical Quality Improvement Program (ACS-NSQIP).](https://www.facs.org/quality-programs/acs-nsqip)

# Instructions

After extracting the ACS-NSQIP data, place the folder titled "txt" into the project folder "data". By default, the report will use the first six files in the "txt" data folder. This is because the "July Effect" analysis requires resident PGY data, which was available only for a select number of years.

Assuming the years in the ACS-NSQIP "data/txt/" folder correspond to those reported in our manuscript, "report.Rmd" may be knit to reproduce that manuscript. 
