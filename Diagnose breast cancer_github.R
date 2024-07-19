library(caret, ggplot2)
library(e1071)
library(dplyr)

# Load the dataset from Github
df <- read.csv("https://raw.githubusercontent.com/Nicolas-Masino/Effect-size/main/breast-cancer-data.csv")

# Eliminate ID column
df <- df[2:32] 

# Convert diagnosis column as a factor
df$diagnosis <- factor(df$diagnosis)

benignData <- df[df$diagnosis=='B', ]
malignantData <- df[df$diagnosis=='M', ]

###############################################################
#################                            ##################
#################   FEATURE SELECTION TASK   ##################
#################                            ##################
###############################################################

# LVQ feature selection
set.seed(7)

# Prepare training scheme
control <- trainControl(method = "repeatedcv", number = 10, 
                        repeats = 10)

# Train the model
model <- train(diagnosis~., data = df, method = 'lvq', 
               preProcess = "scale", trControl = control)

# Estimate variable importance
importance <- varImp(model, scale = FALSE)
print(importance)

# Plot importance
plot(importance)

lvq_features <- c('concave.points_mean', 'perimeter_mean','radius_worst', 
                  'area_worst', 'concave.points_worst', 'perimeter_worst')

# RFE feature selection
set.seed(7)

# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10, 
                      repeats = 10)

# Run the RFE algorithm
results <- rfe(df[,2:31], df[,1], sizes = c(1:30), rfeControl = control)

# Summarize the results
print(results)

# Plot the results
plot(results, type=c("g", "o"))

rfe_features <- c(results$optVariables,'diagnosis')

# Cohen's d feature selection
# Compute Cohen's d
library(lsr)

cohenvalue<-c(
  cohensD(benignData$radius_mean, malignantData$radius_mean),
  cohensD(benignData$texture_mean, malignantData$texture_mean),
  cohensD(benignData$perimeter_mean, malignantData$perimeter_mean),
  cohensD(benignData$area_mean, malignantData$area_mean),
  cohensD(benignData$smoothness_mean, malignantData$smoothness_mean),
  cohensD(benignData$compactness_mean, malignantData$compactness_mean),
  cohensD(benignData$concavity_mean, malignantData$concavity_mean),
  cohensD(benignData$concave.points_mean, malignantData$concave.points_mean),
  cohensD(benignData$symmetry_mean, malignantData$symmetry_mean),
  cohensD(benignData$fractal_dimension_mean, malignantData$fractal_dimension_mean),
  
  cohensD(benignData$radius_se, malignantData$radius_se),
  cohensD(benignData$texture_se, malignantData$texture_se),
  cohensD(benignData$perimeter_se, malignantData$perimeter_se),
  cohensD(benignData$area_se, malignantData$area_se),
  cohensD(benignData$smoothness_se, malignantData$smoothness_se),
  cohensD(benignData$compactness_se, malignantData$compactness_se),
  cohensD(benignData$concavity_se, malignantData$concavity_se),
  cohensD(benignData$concave.points_se, malignantData$concave.points_se),
  cohensD(benignData$symmetry_se, malignantData$symmetry_se),
  cohensD(benignData$fractal_dimension_se, malignantData$fractal_dimension_se),
  
  cohensD(benignData$radius_worst, malignantData$radius_worst),
  cohensD(benignData$texture_worst, malignantData$texture_worst),
  cohensD(benignData$perimeter_worst, malignantData$perimeter_worst),
  cohensD(benignData$area_worst, malignantData$area_worst),
  cohensD(benignData$smoothness_worst, malignantData$smoothness_worst),
  cohensD(benignData$compactness_worst, malignantData$compactness_worst),
  cohensD(benignData$concavity_worst, malignantData$concavity_worst),
  cohensD(benignData$concave.points_worst, malignantData$concave.points_worst),
  cohensD(benignData$symmetry_worst, malignantData$symmetry_worst),
  cohensD(benignData$fractal_dimension_worst, malignantData$fractal_dimension_worst))

# Convert Cohen values in a dataframe
cohenVar <- data.frame('variable' = colnames(df[2:31]),
                       'cohen d' = cohenvalue)

# Graph cohen values per feature in order. Note that the red line
# represent d = 0.8, so the selected feature are those exceed the red line
ggplot(cohenVar, aes(x = reorder(variable, -cohen.d), y = cohen.d))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  ggtitle("Cohen's d values per feature in order")+
  labs(x = "Variables", y = "Cohen's d")+
  geom_hline(yintercept = 0.8, color=2)

ggplot(df, aes(concave.points_worst, colour = diagnosis, fill = diagnosis))+
  xlab('Worst concave points')+
  ylab('Density')+
  theme_bw()+
  geom_density(alpha = 0.5)+
  ggtitle("Worst concave points density for each class")

ggplot(df, aes(texture_se, colour = diagnosis, fill = diagnosis))+
  xlab('Se exture')+
  ylab('Density')+
  theme_bw()+
  geom_density(alpha = 0.5)+
  ggtitle("Se texture density for each class")

# Select the features that its d is over 0.8
cohen_features <- cohenVar[cohenVar[2]>0.8, 1]

##########################################################
#############                               ##############
#############    BREAST CANCER DIAGNOSIS    ##############
#############                               ##############
##########################################################

library(pROC)

# Split data in 70% to train and 30% to test 
splitData <- function(handle, splitRatio){
  index = createDataPartition(handle[ ,1], p = splitRatio, list = FALSE )
  return(index)
}


train_and_evaluate <- function(train_data, test_data) {
  # Train the model
  model <- svm(diagnosis ~ ., data = train_data, kernel = "radial",
               type = 'C-classification', probability = T)
  
  # Diagnose the test cases
  predictions <- predict(model, newdata = test_data %>% select(-diagnosis),
                         probability = T)
  
  # Get the probabilities of each class
  probs <- attr(predictions, 'probabilities')[,2]
  
  # Compute the metrics
  confusion <- confusionMatrix(predictions, test_data$diagnosis)
  accuracy <- confusion[["overall"]][["Accuracy"]]
  sensitivity <- confusion[["byClass"]][["Sensitivity"]]
  specificity <- confusion[["byClass"]][["Specificity"]]
  f1 <- confusion[["byClass"]][["F1"]]
  
  # Compute ROC curve
  roc_obj <- roc(test_data$diagnosis, probs)
  auc_value = auc(roc_obj)
  
  return(c(Accuracy = accuracy, Sensitivity = sensitivity, 
           Specificity = specificity, F1 = f1, auc = auc_value))
}

# Define the subset of features
subsets <- list(cohen = cohen_features, RFE = rfe_features, 
                LVQ = lvq_features)

# Creat a data frame to save the results
model_metrics <- data.frame(
  Subset = character(300),
  Iteration = integer(300),
  Accuracy = numeric(300),
  Sensitivity = numeric(300),
  Specificity = numeric(300),
  F1score = numeric(300),
  AUC = numeric(300)
)

# Iterate the subset of features
idx <- 1
for (subset_name in names(subsets)) {
  subset_feature <- subsets[[subset_name]]
  
  # Create a SVM model 100 times 
  for (i in 1:100) {
    
    # Reshape the test data and the train data randomly 
    index <- splitData(handle = df, splitRatio = 0.7)
    train_data <- df[index, c(subset_feature, 'diagnosis')]
    test_data <- df[-index, c(subset_feature, 'diagnosis')]
    
    metrics <- train_and_evaluate(train_data, test_data)
    
    # Save the results in the data frame
    model_metrics[idx, "Subset"] <- subset_name
    model_metrics[idx, "Iteration"] <- i
    model_metrics[idx, "Accuracy"] <- metrics['Accuracy']
    model_metrics[idx, "Sensitivity"] <- metrics['Sensitivity']
    model_metrics[idx, "Specificity"] <- metrics['Specificity']
    model_metrics[idx, "F1score"] <- metrics['F1']
    model_metrics[idx, "AUC"] <- metrics['auc']
    
    idx <- idx + 1
  }
}

# Separate the results depending on the feature selection method used
model_metrics_cohen <- subset(model_metrics, Subset == 'cohen') %>% select(-c(Subset, Iteration))
model_metrics_rfe <- subset(model_metrics, Subset == 'RFE') %>% select(-c(Subset, Iteration))
model_metrics_lvq <- subset(model_metrics, Subset == 'LVQ') %>% select(-c(Subset, Iteration))

# Compute the mean and the standard deviation of each metric
means_model_metrics_cohen <- colMeans(model_metrics_cohen)
sds_model_metrics_cohen <- apply(model_metrics_cohen, 2, sd)

means_model_metrics_rfe <- colMeans(model_metrics_rfe)
sds_model_metrics_rfe <- apply(model_metrics_rfe, 2, sd)

means_model_metrics_lvq <- colMeans(model_metrics_lvq)
sds_model_metrics_lvq <- apply(model_metrics_lvq, 2, sd)

# Save the mean and the standard deviation of each metric in the data frame
resultsModel <- data.frame(metrics = names(model_metrics_cohen),
                           means_cohen = means_model_metrics_cohen,
                           sds_cohen = sds_model_metrics_cohen,
                           means_rfe = means_model_metrics_rfe,
                           sds_rfe = sds_model_metrics_rfe,
                           means_lvq = means_model_metrics_lvq,
                           sds_lvq = sds_model_metrics_lvq)
