all: data/paper.rda report.pdf

VPATH = \
  data\
  data-raw

data/paper.rda : paper.R
	Rscript $<

report.pdf: report.Rmd paper.rda R/*
	Rscript -e 'rmarkdown::render("$<", output_format = "all")'


