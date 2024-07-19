
library(caret)
library(ggplot2)

# split the data frame in train set and test set
set.seed(180)
index = createDataPartition(df[ ,1], p=0.7, list = F)
trainSet <- df[index, c(rfe_features, 'diagnosis')]
testSet <- df[-index, c(rfe_features, 'diagnosis')]

folds <- createFolds(df$diagnosis, k = 10) # Create folds

# Apply cross validation to logistic regression
cvLogReg <- lapply(folds, function(x){
  training_fold <- trainSet[-x, ]
  test_fold <- trainSet[x, ]
  clasificador <- glm(diagnosis ~ ., family = binomial, data = training_fold)
  y_pred <- predict(clasificador, type = 'response', newdata = test_fold)
  y_pred <- ifelse(y_pred > 0.5, 1, 0)
  y_pred <- factor(y_pred, levels = c("0", "1"), labels = c("B", "M"))
  cm <- table(test_fold$diagnosis, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  sensibilidad <- cm[2,2]/(cm[2,2]+cm[1,2])
  return(c(precision, sensibilidad))
})
logregPresicion<-mean(as.numeric(cvLogReg[1]))
logregSensibilidad<-mean(as.numeric(cvLogReg[2]))

# k-NN
cvkNN <- lapply(folds, function(x){
  training_fold <- trainSet[-x, ]
  test_fold <- trainSet[x, ]
  y_pred <- knn(training_fold[, -13], 
                test_fold[, -13], 
                cl = training_fold[, 13], 
                k = 10)
  cm <- table(test_fold$diagnosis, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  return(precision)
})
precisionkNN <- mean(as.numeric(cvkNN))

# Kernel-SVM
cvKernelSVM <- lapply(folds, function(x){
  training_fold <- trainSet[-x, ]
  test_fold <- trainSet[x, ]
  clasificador <- svm(diagnosis ~ .,
                      data = training_fold, 
                      type = 'C-classification', 
                      kernel = 'radial')
  y_pred <- predict(clasificador, newdata = test_fold)
  cm <- table(test_fold$diagnosis, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  return(precision)
})

precisionKernelSVM <- mean(as.numeric(cvKernelSVM))

# Naive Bayes
cvNaiveBayes <- lapply(folds, function(x){
  training_fold <- trainSet[-x, ]
  test_fold <- trainSet[x, ]
  clasificador <- naiveBayes(diagnosis ~ ., data = training_fold)
  y_pred <- predict(clasificador, newdata = test_fold)
  cm <- table(test_fold$diagnosis, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  return(precision)
})
precisionNaiveBayes <- mean(as.numeric(cvNaiveBayes))

# Decision Tree
cvDecisionTree <- lapply(folds, function(x){
  training_fold <- trainSet[-x, ]
  test_fold <- trainSet[x, ]
  clasificador <- rpart(diagnosis ~ ., data = training_fold)
  y_pred <- predict(clasificador, newdata = test_fold, type = 'class')
  cm <- table(test_fold$diagnosis, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  return(precision)
})
precisionDecisionTree <- mean(as.numeric(cvDecisionTree))

# Random Forest
cvRandomForest <- lapply(folds, function(x){
  training_fold <- trainSet[-x, ]
  test_fold <- trainSet[x, ]
  clasificador <- randomForest(diagnosis ~ ., data = training_fold, ntree = 250)
  y_pred <- predict(clasificador, newdata = test_fold)
  cm <- table(test_fold$diagnosis, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  return(precision)
})
precisionRandomForest <- mean(as.numeric(cvRandomForest))

dfModels<-data.frame(RF=c(1,0.925,0.975,0.975,0.975,0.875,1,0.974359,0.975,0.95),
                     DT=c(0.95,0.875,0.925,1,1,0.85,1,0.8974359,0.975,0.9),
                     NB=c(1,0.875,0.975,0.975,0.95,0.85,1,0.9487179,0.975,0.95),
                     SVM=c(1,0.925,0.975,1,0.95,0.925,1,0.9487179,0.975,0.975),
                     KNN=c(0.95,0.875,0.925,0.925,0.975,0.925,0.975,0.974359,0.95,0.925),
                     LR=c(1,0.95,0.95,1,0.95,0.925,1,0.974359,0.95,0.95),
                     folds=c('Fold01','Fold02','Fold03','Fold04','Fold05','Fold06','Fold07','Fold08','Fold09','Fold10'))

dfModels_long<-data.frame(x=seq_along(dfModels[,1]),dfModels)
dfModels_long<-melt(dfModels)

ggplot(dfModels_long, aes(folds, value, group=1, color=variable))+
  geom_line()+
  geom_point()+
  theme_bw()
rbind(cvLogReg)

# Apply cross validation to Cohen's d features

df2 <- df[, cohenVar[2]>0.8]

set.seed(800)
index<-createDataPartition(df2$diagnosis, p=0.7, list = FALSE)
trainSet_df2<-df2[index, ]
testSet_df2<-df2[-index, ]

regLog2<-glm(diagnosis~., family = 'binomial', data = trainSet_df2)
y_predic_regLog2 <- predict(regLog2, type = 'response', newdata=testSet_df2)
y_predic_regLog2 <- ifelse(y_predic_regLog2 > 0.5, 1, 0)
y_predic_regLog2 <- factor(y_predic_regLog2, levels = c("0", "1"), labels = c("B", "M"))
summary(regLog2)
confusionMatrix(y_predic_regLog2, testSet_df2$diagnosis)
'Sensitivity : 0.9439          
Specificity : 0.9524'

y_predict_knn2 <- knn(train = trainSet_df2[ , -21], test = testSet_df2[,-21],
                      cl = trainSet_df2[,21], k=11)
confusionMatrix(y_predict_knn2, testSet_df2$diagnosis)
'Sensitivity : 0.9720          
Specificity : 0.8889'

svm2 <- svm(diagnosis ~ .,data = trainSet_df2, type = 'C-classification', 
            kernel = 'radial')
y_predict_svm2 <- predict(svm2, newdata = testSet_df2)
confusionMatrix(y_predict_svm2, testSet_df2$diagnosis)
'Sensitivity : 0.9720          
Specificity : 0.9841'

naive_bayes2 <- naiveBayes(diagnosis ~ ., data = trainSet_df2)
y_predict_naive_bayes2 <- predict(naive_bayes2, newdata = testSet_df2)
confusionMatrix(y_predict_naive_bayes2, testSet_df2$diagnosis)
'Sensitivity : 0.9533         
Specificity : 0.9683'

arbol2 <- rpart(diagnosis ~ ., data = trainSet_df2)
y_predict_arbol2 <- predict(arbol2, newdata = testSet_df2, type = 'class')
summary(arbol2)
confusionMatrix(y_predict_arbol2, testSet_df2$diagnosis)
'Sensitivity : 0.9065          
Specificity : 0.9841'

rm2<-randomForest(diagnosis ~ ., data = trainSet_df2, ntree = 250)
y_predict_rm2 <- predict(rm2, newdata = testSet_df2)
confusionMatrix(y_predict_rm2, testSet_df2$diagnosis)
'Sensitivity : 0.9813          
Specificity : 0.9683'