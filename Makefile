all: data/paper_stuff.rda report.pdf





data/paper_stuff.rda: 
	Rscript data-raw/paper_stuff.R

report.pdf:
	Rscript -e 'rmarkdown::render("report.Rmd", output_format = "all")'


