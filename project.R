fileloc <- "cumulative.csv"

df <- read.csv(fileloc)

library(sqldf)

#These are the main variables that are relevant
td <- sqldf("SELECT koi_disposition, koi_pdisposition, koi_period, koi_duration, koi_depth, koi_prad, koi_model_snr, koi_steff, koi_srad FROM df WHERE koi_disposition!='CANDIDATE' AND koi_depth IS NOT NULL")

#This will kill your computer for a while
#pairs(td)

#Verify that the data is cleaned
#library(VIM)
#aggr_plot <- aggr(td, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

summary(td)

library(randomForest)

rf <- randomForest(koi_pdisposition ~ koi_period + koi_duration + koi_depth + koi_prad + koi_model_snr + koi_steff + koi_srad, data=td)
rf

importance(rf)

varImpPlot(rf)

