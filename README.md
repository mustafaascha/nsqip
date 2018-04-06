# ACS-NSQIP analysis for a study of the "July Effect"

[![DOI](https://zenodo.org/badge/117899615.svg)](https://zenodo.org/badge/latestdoi/117899615)

# Purpose

This package is meant to document and increase the reproducibility of a paper examining the "July Effect" among procedures recorded in the [American College of Surgeons National Surgical Quality Improvement Program (ACS-NSQIP).](https://www.facs.org/quality-programs/acs-nsqip)

# Instructions

After extracting the ACS-NSQIP data, place the folder titled "txt" into the project folder "data". By default, the report will use the first six files in the "txt" data folder. This is because the "July Effect" analysis requires resident PGY data, which was available only for a select number of years.

Assuming the years in the ACS-NSQIP "data/txt/" folder correspond to those reported in our manuscript, "report.Rmd" may be knit to reproduce that manuscript. 


# Project structure

Before adding the data to the correct folder, you should have the following directory structure: 

nsqipr    
<pre>├── </pre>data   
<pre>│   └── </pre>txt   
<pre>│       └── </pre>PUT-DATA-HERE   
<pre>├── </pre>data-raw   
<pre>│   └── </pre>paper_things.R   
<pre>├── </pre>DESCRIPTION   
<pre>├── </pre>LICENSE   
<pre>├── </pre>NAMESPACE   
<pre>├── </pre>nsqipr.Rproj   
<pre>├── </pre>R   
<pre>│   ├── </pre>analysis.R   
<pre>│   ├── </pre>data.R   
<pre>│   └── </pre>munge.R   
<pre>├── </pre>README.md   
<pre>└── </pre>report.Rmd   


After adding the data, the project directory should have the following structure:

nsqipr   
<pre>├── </pre>data    
<pre>│   └── </pre>txt    
<pre>│       ├── </pre>ACS_NSQIP_PUF_05_06_vr1.txt.gz    
<pre>│       ├── </pre>ACS_NSQIP_PUF07_TXT.txt.gz    
<pre>│       ├── </pre>ACS_NSQIP_PUF08_TXT.txt.gz    
<pre>│       ├── </pre>ACS_NSQIP_PUF09_TXT.txt.gz    
<pre>│       ├── </pre>ACS_NSQIP_PUF10_TXT.txt.gz    
<pre>│       ├── </pre>ACS_NSQIP_PUF11_TXT.txt.gz    
<pre>│       ├── </pre>acs_nsqip_puf12.txt.gz    
<pre>│       ├── </pre>acs_nsqip_puf13.txt.gz    
<pre>│       ├── </pre>acs_nsqip_puf14.txt.gz    
<pre>│       ├── </pre>acs_nsqip_puf15_v2.txt.gz    
<pre>│       └── </pre>PUT-DATA-HERE    
<pre>├── </pre>data-raw    
<pre>│   └── </pre>paper_things.R    
<pre>├── </pre>DESCRIPTION    
<pre>├── </pre>LICENSE    
<pre>├── </pre>NAMESPACE    
<pre>├── </pre>nsqipr.Rproj    
<pre>├── </pre>R    
<pre>│   ├── </pre>analysis.R    
<pre>│   ├── </pre>data.R    
<pre>│   └── </pre>munge.R    
<pre>├── </pre>README.md    
<pre>└── </pre>report.Rmd    






