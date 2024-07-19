library(lsr)

# Compute Cohen's d
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

# convert Cohen values in a dataframe
cohenVar <- data.frame('variable'=colnames(df[2:31]),
                       'cohen d'=cohenvalue)

library(ggplot2)

# Graph cohen values per feature in order. Note that the red line
# represent d = 0.8, so the selected feature are those over the red line
ggplot(cohenVar, aes(x = reorder(variable, -cohen.d), y = cohen.d))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  ggtitle("Cohen's d values per feature in order")+
  labs(x = "Variables", y = "Cohen's d")+
  geom_hline(yintercept = 0.8, color=2)

###################### EXPLORATORY ANALYSIS #####################

classEst <- data.frame('cohen' = cohenvalue,
                      'benign_mean' = sapply(benignData[2:31], mean),
                      'malign_mean' = sapply(malignantData[2:31], mean),
                      'benign_sd' = sapply(benignData[2:31], sd),
                      'malign_sd' = sapply(malignantData[2:31], sd),
                      'benign_var' = sapply(benignData[2:31], var),
                      'malign_var' = sapply(malignantData[2:31], var))

diff <- function(a,b){
  subtraction = format(((a-b)**2)**1/2, scientific=F)
  return(subtraction)
}

classEst['diff_mean'] <- diff(classEst[["benign_mean"]], classEst[["malign_mean"]])
classEst['diff_sd'] <- diff(classEst[["benign_sd"]], classEst[["malign_sd"]])
classEst['diff_var'] <- diff(classEst[["benign_var"]], classEst[["malign_var"]])

div <- function(a,b){
  ratio = format(as.numeric(a)/as.numeric(b), scientific=F)
  return(ratio)
}

classEst['ratio_MV'] <- div(classEst[['diff_mean']], classEst[['diff_var']])
classEst['ratio_MS'] <- div(classEst[['diff_mean']], classEst[['diff_sd']])

ggplot(df, aes(concave.points_se, fill=diagnosis, colour=diagnosis))+
  geom_density(alpha=0.5)+
  geom_vline(xintercept = classEst['concave.points_se', 'benign_mean'], 
             color = 2)+
  geom_vline(xintercept = classEst['concave.points_se', 'malign_mean'],
             color = 5)+
  geom_vline(xintercept = classEst['concave.points_se', 'benign_var'],
             color = 2, linetype = 2)+
  geom_vline(xintercept = classEst['concave.points_se', 'malign_var'],
             color = 5, linetype = 2)+
  geom_vline(xintercept = classEst['concave.points_se', 'malign_sd'],
             color = 5, linetype = 3, lwd = 1)+
  geom_vline(xintercept = classEst['concave.points_se', 'benign_sd'],
             color = 2, linetype = 3, lwd = 1)

ggplot(classEst, aes(as.numeric(diff_sd), cohen))+
  geom_point()+
  geom_hline(yintercept = 0.8, color=2)+
  ggtitle("Standard Deviation difference vs Cohen's d")+
  labs(y = "Cohen's d", x = "Standadr deviation difference")

classEst$mgm <- sapply(malignantData[2:31], mean)>sapply(benignData[2:31], max)

ggplot(classEst, aes(rownames(classEst), cohen, fill=mgm))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  scale_fill_manual(values = c('FALSE' = "gray", 'TRUE' = "red"))+
  ggtitle('Features which malignant mean is over benign maximum')+
  labs(x = "Variables", y = "Cohen's d")
