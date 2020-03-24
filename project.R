fileloc <- "cumulative.csv"

df <- read.csv(fileloc)

library(sqldf)

#These are the main variables that are relevant
td <- sqldf("SELECT koi_disposition, koi_pdisposition, koi_period, koi_duration, koi_depth, koi_prad, koi_model_snr, koi_steff, koi_srad FROM df WHERE koi_disposition!='CANDIDATE' AND koi_depth IS NOT NULL")

summary(td)

hist(td$koi_period, breaks=10)
hist(td$koi_depth, breaks=10)
hist(td$koi_duration, breaks=50)
hist(td$koi_prad, breaks=100)
boxplot(td$koi_prad)
boxplot(td$koi_duration)

boxplot(log(td$koi_duration))

hist(log(td$koi_duration), breaks=100)
mean(log(td$koi_duration))
sd(log(td$koi_duration))
library(moments)
skewness(log(td$koi_duration))

ggplot(td) + geom_point(aes(x=log(koi_model_snr), y=log(koi_prad), color=koi_pdisposition))
ggplot(td) + geom_point(aes(x=log(koi_period), y=log(koi_prad), color=koi_pdisposition))
ggplot(td) + geom_point(aes(x=log(koi_model_snr), y=log(koi_period), color=koi_pdisposition))
ggplot(td) + geom_point(aes(x=log(koi_duration), y=log(koi_srad), color=koi_pdisposition))

library(plotly)
plot_ly(x=log(td$koi_prad), y=log(td$koi_model_snr), z=log(td$koi_period), type="scatter3d", mode="markers", color=td$koi_pdisposition)

hist(log(td$koi_prad)) #

hist(log(td$koi_model_snr)) #

hist(log(td$koi_depth)) #

hist(log(td$koi_period))

hist(log(td$koi_srad))

hist(log(td$koi_steff))

plot(log(td$koi_duration) ~ log(td$koi_steff))



td[which(td$koi_prad > 100),]
sd(td$koi_prad)

sum(td$koi_pdisposition=="FALSE POSITIVE")/nrow(td)

library(mosaic)
tally(td$koi_pdisposition)

summary(td)

#This will kill your computer for a while
pairs(td)

#Verify that the data is cleaned
#library(VIM)
#aggr_plot <- aggr(td, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

summary(td)

library(randomForest)

rf <- randomForest(koi_pdisposition ~ koi_period + koi_duration + koi_depth + koi_prad + koi_model_snr + koi_steff + koi_srad, data=td)
rf

importance(rf)

varImpPlot(rf)


train_ind <- sample(1:nrow(td), nrow(td)*0.7, replace=FALSE)
train <- td[train_ind,]
test <- td[-train_ind,]
library(caret)
rft <- randomForest(koi_pdisposition ~ koi_period + koi_duration + koi_depth + koi_prad + koi_model_snr + koi_steff + koi_srad, data=train)
rft
varImpPlot(rft)
pred <- predict(rft, test)
confusionMatrix(pred, test$koi_pdisposition)


#Try some SVMs, but they weren't very good.

library(e1071)
svmm <- svm(koi_pdisposition ~ koi_period + koi_duration + koi_depth + koi_prad + koi_model_snr + koi_steff + koi_srad, data=train)
predsvm <- predict(svmm, test)
confusionMatrix(predsvm, test$koi_pdisposition)

library(kernlab)
ksvmm <- ksvm(koi_pdisposition ~ koi_period + koi_duration + koi_depth + koi_prad + koi_model_snr + koi_steff + koi_srad, train, method="svmLinear", metric="RMSE",
              tuneGrid=expand.grid(.sigma=seq(0.1, 3.5, by=0.2), .C=seq(1, 5, by=1)),
              trControl=trainControl(method="cv", number=5))
ksvm_predicted <- predict(ksvmm, test)
confusionMatrix(ksvm_predicted, test$koi_pdisposition)

