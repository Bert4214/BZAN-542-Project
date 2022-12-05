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
spotify_2020s = read.csv("spotify_2020s.csv")

# Data Prep for Modeling
spotify_2020s1 = spotify_2020s[,-c(1:4,8:10,24:25)]
spotify1 = spotify_2020s1
spotify1 = spotify1[complete.cases(spotify1),]
summary(spotify1$popularity)
class(spotify1$popularity)

spotify1 = spotify1[sample(nrow(spotify1),5000),]

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
infodensity <- nearZeroVar(spotify1, saveMetrics= TRUE)
infodensity
#No columns have near zero or zero variance

#Training and Holdout
set.seed(123)
train.rows <- sample(1:nrow(spotify1),0.7*nrow(spotify1))
TRAIN <- spotify1[train.rows,]
HOLDOUT <- spotify1[-train.rows,]

#Fit Control
fitControl <- trainControl(method="cv",number=5,verboseIter = FALSE)

#Naive
NAIVE <- mean(TRAIN$popularity)
sqrt(mean((NAIVE-HOLDOUT$popularity)^2))
postResample(rep(NAIVE,nrow(HOLDOUT)),HOLDOUT$popularity)

#Regularized Linear Regression
glmnetGrid <- expand.grid(alpha = seq(0,.1,.25),lambda = 10^seq(-1,-.5,by=.5))
set.seed(123); GLMnet <- train(popularity~.,data=TRAIN,method='glmnet', tuneGrid=glmnetGrid,
                               trControl=fitControl, preProc = c("center", "scale"))

plot(GLMnet)  #See how error changes with parameters
GLMnet$bestTune #Gives best parameters
GLMnet$results[rownames(GLMnet$bestTune),]  #Just the row with the optimal choice of tuning parameter
postResample(predict(GLMnet,newdata=HOLDOUT),HOLDOUT$popularity)  #RMSE on holdout

#K Nearest Neighbors
knnGrid <- expand.grid(k=1:10)
set.seed(123); KNN <- train(popularity~.,data=TRAIN,method='knn', tuneGrid=knnGrid,
                            trControl=fitControl, preProc = c("center", "scale"))

plot(KNN)  #See how error varies with k
KNN$bestTune #Gives best parameters
KNN$results[rownames(KNN$bestTune),]  #Just the row with the optimal choice of tuning parameter
postResample(predict(KNN,newdata=HOLDOUT),HOLDOUT$popularity)  #RMSE on holdout

#Vanilla Partition
rpartGrid <- expand.grid(cp=10^seq(-1,0.15,length=50))
set.seed(123); RPART <- train(popularity~.,data=TRAIN,method="rpart",trControl=fitControl,
                              tuneGrid=rpartGrid,preProc=c("center","scale"))

plot(RPART)  #See how performance varies with cp parameters
RPART$results[rownames(RPART$bestTune),]   #best choice
varImp(RPART)  #Variable importance between 0-100
postResample(predict(RPART,newdata=HOLDOUT),HOLDOUT$popularity)  #RMSE on holdout
# RMSE = 20.8625031, R2 = 0.2659425, cp = 0.1, not overfit

#Random Forest
forestGrid <- expand.grid(mtry=c(1:17))
fitControl <- trainControl(method = "cv",number = 5, allowParallel = TRUE)
set.seed(123); FOREST <- train(popularity~.,data=TRAIN,method="rf",preProc=c("center","scale"),
                               trControl=fitControl,tuneGrid=forestGrid, importance=TRUE)

FOREST$results[rownames(FOREST$bestTune),]   #best choice
varImp(FOREST)  #Variable importance between 0-100
postResample(predict(FOREST,newdata=HOLDOUT),HOLDOUT$popularity)  #RMSE on holdout

#Boosted Tree
gbmGrid <- expand.grid(n.trees=c(350,400,300),
                       interaction.depth=9:11,
                       shrinkage=c(.045,.05,.055),
                       n.minobsinnode=c(4:5))
fitControl <- trainControl(method = "cv",number = 5, allowParallel = TRUE)
set.seed(123); GBM <- train(popularity~.,data=TRAIN,method="gbm",trControl=fitControl,
                            tuneGrid=gbmGrid,preProc=c("center","scale"),verbose=FALSE)

plot(GBM)
GBM$results[rownames(GBM$bestTune),]
varImp(GBM)  #need to have library(gbm) loaded up
postResample(predict(GBM,newdata=HOLDOUT),HOLDOUT$popularity)  #RMSE on holdout

#Linear SVM
paramGrid <- expand.grid(C=10^seq(-2,3,length=21))
set.seed(123); SVM.linear <- train(popularity~.,data=TRAIN, method='svmLinear',
                                   trControl=fitControl,tuneGrid=paramGrid,preProc=c("center", "scale"))

plot(SVM.linear)
SVM.linear$results[rownames(SVM.linear$bestTune),]
postResample(HOLDOUT$popularity,predict(SVM.linear,newdata=HOLDOUT))

## DALEX Plots
# DALEX plots help further breakdown how each variable is affecting the 
# popularity score of a song. I'm randomly sampling from the original data
# to develop the plots using the GBM model.

##Understanding
library("DALEX")

#Make special datasets for examining models
TRAIN.PREDICTORS <- TRAIN
TRAIN.PREDICTORS$popularity <- NULL
TRAIN.TARGET <- TRAIN$popularity

#Example building an explainer with GBM
shop_explainer <- explain(GBM, data = TRAIN.PREDICTORS,  y = TRAIN.TARGET)
shop_vi <- model_parts(shop_explainer, loss_function = loss_root_mean_square)
shop_vi
plot(shop_vi)

# Predicting Specific Songs
specific.alumnus <- spotify_2020s1[sample(nrow(spotify_2020s1),1),]
plot( predict_parts(explainer = shop_explainer, new_observation = specific.alumnus, type = "break_down") )
spotify_2020s[891,]

specific.alumnus1 <- spotify_2020s1[sample(nrow(spotify_2020s1),1),]
plot( predict_parts(explainer = shop_explainer, new_observation = specific.alumnus1, type = "break_down") )
spotify_2020s[1444,]





