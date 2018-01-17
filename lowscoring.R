


source("analysis.R")

ggplot(nsqip[[1]], aes(propensity_score, fill = pgy_bin)) + geom_histogram()




library(rpart)

frm <- paste0("pgy_bin ~", paste0(paper_stuff$mona$predictors[-15], collapse = "+"))

plot(rpart(frm, nsqip[[1]] %>% select(c("pgy_bin", paper_stuff$mona$predictors[-15]))))



