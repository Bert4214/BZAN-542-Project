## This file can be used as a template for data mining and predictive analytics
## with each decade file. Some of the more important info would be found with the
## RMSE values found with each model, as well as the variable importance plots.
## This can help answer the question of whether or not we can accurately predict
## the poopularity score of a song, and what variables are at play the most in
## predicting that score. The Random Forest and Linear SVM models take the 
## longest to run, so it might be helpful to take a random sample of the full
## data to try to run them. By having a file by decade, we want to see if there
## is a change in how the popularity score is affected by decade, or if the same
## factors are always at play here.

## Data Modeling for 2020s Songs
spotify_2010s = read.csv("spotify_2010s.csv")

# Data Prep for Modeling
spotify_2010s1 = spotify_2010s[,-c(1:4,8:10,24:25)]
spotify2 = spotify_2010s1
spotify2 = spotify2[complete.cases(spotify2),]
summary(spotify2$popularity)
class(spotify2$popularity)

spotify2 = spotify2[sample(nrow(spotify2),5000),]

library(regclass)
library(robustbase)
library(MASS)
library(caret)
library(gbm)

suggest_transformation <- function(x,powers=seq(0,3,by=0.5),add=0) {
  require(robustbase)
  if(add!=0) { x <- x + add }
  if(min(x)<=0) { powers <- powers[which(powers>0)] }
  skewnesses <- rep(0,length(powers))
  for (p in powers) {
    if(p==0) { x.trans <- log10(x) } else { x.trans <- x^p }
    skewnesses[which(powers==p)] <- mc(x.trans)
  }
  best.p <- powers[which.min(skewnesses)]
  if(best.p==0) { return("log10")} else { return(best.p) }
}

#NZV and ZV
infodensity <- nearZeroVar(spotify2, saveMetrics= TRUE)
infodensity
#No columns have near zero or zero variance

#Training and Holdout
set.seed(123)
train.rows <- sample(1:nrow(spotify2),0.7*nrow(spotify2))
TRAIN1 <- spotify2[train.rows,]
HOLDOUT1 <- spotify2[-train.rows,]

#Fit Control
fitControl <- trainControl(method="cv",number=5,verboseIter = FALSE)

#Naive
NAIVE1 <- mean(TRAIN1$popularity)
sqrt(mean((NAIVE1-HOLDOUT1$popularity)^2))
postResample(rep(NAIVE1,nrow(HOLDOUT1)),HOLDOUT1$popularity)

#Regularized Linear Regression
glmnetGrid <- expand.grid(alpha = seq(0,.1,.25),lambda = 10^seq(-1,-.5,by=.5))
set.seed(123); GLMnet1 <- train(popularity~.,data=TRAIN1,method='glmnet', tuneGrid=glmnetGrid,
                               trControl=fitControl, preProc = c("center", "scale"))

plot(GLMnet1)  #See how error changes with parameters
GLMnet1$bestTune #Gives best parameters
GLMnet1$results[rownames(GLMnet1$bestTune),]  #Just the row with the optimal choice of tuning parameter
postResample(predict(GLMnet1,newdata=HOLDOUT1),HOLDOUT1$popularity)  #RMSE on holdout

#K Nearest Neighbors
knnGrid <- expand.grid(k=1:10)
set.seed(123); KNN1 <- train(popularity~.,data=TRAIN1,method='knn', tuneGrid=knnGrid,
                            trControl=fitControl, preProc = c("center", "scale"))

plot(KNN1)  #See how error varies with k
KNN1$bestTune #Gives best parameters
KNN1$results[rownames(KNN1$bestTune),]  #Just the row with the optimal choice of tuning parameter
postResample(predict(KNN1,newdata=HOLDOUT1),HOLDOUT1$popularity)  #RMSE on holdout

#Vanilla Partition
rpartGrid <- expand.grid(cp=10^seq(-1,0.15,length=50))
set.seed(123); RPART1 <- train(popularity~.,data=TRAIN1,method="rpart",trControl=fitControl,
                              tuneGrid=rpartGrid,preProc=c("center","scale"))

plot(RPART1)  #See how performance varies with cp parameters
RPART1$results[rownames(RPART1$bestTune),]   #best choice
varImp(RPART1)  #Variable importance between 0-100
postResample(predict(RPART1,newdata=HOLDOUT1),HOLDOUT1$popularity)  #RMSE on holdout
# RMSE = 20.8625031, R2 = 0.2659425, cp = 0.1, not overfit

#Random Forest
forestGrid <- expand.grid(mtry=c(1:17))
fitControl <- trainControl(method = "cv",number = 5, allowParallel = TRUE)
set.seed(123); FOREST1 <- train(popularity~.,data=TRAIN1,method="rf",preProc=c("center","scale"),
                               trControl=fitControl,tuneGrid=forestGrid, importance=TRUE)

FOREST1$results[rownames(FOREST1$bestTune),]   #best choice
varImp(FOREST1)  #Variable importance between 0-100
postResample(predict(FOREST1,newdata=HOLDOUT1),HOLDOUT1$popularity)  #RMSE on holdout

#Boosted Tree
gbmGrid <- expand.grid(n.trees=c(350,400,300),
                       interaction.depth=9:11,
                       shrinkage=c(.045,.05,.055),
                       n.minobsinnode=c(4:5))
fitControl <- trainControl(method = "cv",number = 5, allowParallel = TRUE)
set.seed(123); GBM1 <- train(popularity~.,data=TRAIN1,method="gbm",trControl=fitControl,
                            tuneGrid=gbmGrid,preProc=c("center","scale"),verbose=FALSE)

plot(GBM1)
GBM1$results[rownames(GBM1$bestTune),]
varImp(GBM1)  #need to have library(gbm) loaded up
postResample(predict(GBM1,newdata=HOLDOUT1),HOLDOUT1$popularity)  #RMSE on holdout

#Linear SVM
paramGrid <- expand.grid(C=10^seq(-2,3,length=21))
set.seed(123); SVM1.linear <- train(popularity~.,data=TRAIN1, method='svmLinear',
                                   trControl=fitControl,tuneGrid=paramGrid,preProc=c("center", "scale"))

plot(SVM1.linear)
SVM1.linear$results[rownames(SVM1.linear$bestTune),]
postResample(HOLDOUT1$popularity,predict(SVM1.linear,newdata=HOLDOUT1))

## DALEX Plots
# DALEX plots help further breakdown how each variable is affecting the 
# popularity score of a song. I'm randomly sampling from the original data
# to develop the plots using the GBM model.

##Understanding
library("DALEX")

#Make special datasets for examining models
TRAIN1.PREDICTORS <- TRAIN1
TRAIN1.PREDICTORS$popularity <- NULL
TRAIN1.TARGET <- TRAIN1$popularity

#Example building an explainer with GBM
shop_explainer1 <- explain(GBM1, data = TRAIN1.PREDICTORS,  y = TRAIN1.TARGET)
shop_vi1 <- model_parts(shop_explainer1, loss_function = loss_root_mean_square)
shop_vi1
plot(shop_vi1)

# Predicting Specific Songs
specific.alumnus2 <- spotify_2010s1[sample(nrow(spotify_2010s1),1),]
plot( predict_parts(explainer = shop_explainer1, new_observation = specific.alumnus2, type = "break_down") )
spotify_2010s[61753,]

specific.alumnus3 <- spotify_2010s1[sample(nrow(spotify_2010s1),1),]
plot( predict_parts(explainer = shop_explainer1, new_observation = specific.alumnus3, type = "break_down") )
spotify_2010s[83575,]




