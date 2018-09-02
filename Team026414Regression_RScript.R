#####################################################################################
################################ ISYE 6414 - Project ################################
#####################################################################################

# Clear log
rm(list=ls())

# Set working directory
setwd('C:\\Users\\mansiarora\\Documents\\ISyE 6414\\Project')

###### I. Data Cleaning #####
data_raw = read.csv("Team2_Dataset.csv")
head(data_raw)
summary(data_raw)

# As we can see, some columns have 1872 missing values. We're going to remove those columns
# Also, we have selected ViolentCrimesPerPop, so we have deleted other measures of crime rate
# We have also deleted the descriptors - communityname, state and fold columns

# Removing columns

droplist = c("countyCode",	"communityCode",	"LemasSwornFT",	"LemasSwFTPerPop",	"LemasSwFTFieldOps",
             "LemasSwFTFieldPerPop",	"LemasTotalReq",	"LemasTotReqPerPop",	"PolicReqPerOffic",	"PolicPerPop",
             "RacialMatchCommPol",	"PctPolicWhite",	"PctPolicBlack",	"PctPolicHisp",	"PctPolicAsian",
             "PctPolicMinor",	"OfficAssgnDrugUnits",	"NumKindsDrugsSeiz",	"PolicAveOTWorked",	"PolicCars",
             "PolicOperBudg",	"LemasPctPolicOnPatr",	"LemasGangUnitDeploy",	"LemasPctOfficDrugUn",
             "PolicBudgPerPop",	"murders",	"murdPerPop",	"rapes",	"rapesPerPop",	"robberies",	"robbbPerPop",
             "assaults",	"assaultPerPop",	"burglaries",	"burglPerPop",	"larcenies",	"larcPerPop",	"autoTheft",
             "autoTheftPerPop",	"arsons",	"arsonsPerPop",	"nonViolPerPop", "communityname",	"state",	"fold")

data_raw = data_raw[, !(colnames(data_raw) %in% droplist)]

summary(data_raw)

# There are 221 rows where we don't have the value of our response value, and one row where the value
# our response is 0. These rows have been deleted.

data_raw = data_raw[!(data_raw$ViolentCrimesPerPop %in% c("?",0)),]
summary(data_raw)
# One column of OtherPerCap is  ?, changing this value to 0 
data_raw[data_raw$OtherPerCap == "?",]$OtherPerCap = 0

# 2 columns - OtherPerCap and ViolentcrimesPerPop - are convered to numeric
data_raw$OtherPerCap = as.numeric(as.character(data_raw$OtherPerCap))
data_raw$ViolentCrimesPerPop = as.numeric(as.character(data_raw$ViolentCrimesPerPop))

dim(data_raw)
data = data_raw

###### II. Data Transformation #####
# We have transformed some of the variables from absolute to a percent of population
# This will give us a better interpretation

# Also, there are some variables which are available in percent as well as absolute form
# We are keeping the ones which are in percent form

# Converting NumStreet, NumInShelters, NumImmig to Pct
data$PctInShelters = data$NumInShelters/data$population*100
data$PctStreet = data$NumStreet/data$population*100
data$PctImmig = data$NumImmig/data$population*100

# Removing numUrban, NumUnderPov, NumKidsBornNeverMar, and the 3 columns we transformed above   
data$numbUrban = NULL
data$NumUnderPov = NULL
data$NumKidsBornNeverMar = NULL
data$NumInShelters = NULL
data$NumStreet = NULL
data$NumImmig = NULL

# Since our response variable (ViolentCrimesPerPop) has already been controlled for population,
# we don't need to keep 'population' as a predicting variable. 
data$population = NULL

###### III. Exploratory Data Analysis #####
hist(data$ViolentCrimesPerPop, main = "ViolentCrimesPerPop", xlab="")
# Our response variable shows a very strong right tailed skew, hence we have used a log transformation
# to satisfy the normality assumption
hist(log(data$ViolentCrimesPerPop), main = "Log(ViolentCrimesPerPop)", xlab="")

# Taking log of the response variable
data$LogViolentCrimesPerPop = log(data$ViolentCrimesPerPop)


###### IV. Building Basic Models with and without Log and perform Residual Analysis (EDA) ####

############################### Without Log ####################################
basic_lin_model1 = lm(ViolentCrimesPerPop ~ ., data=data[,!(colnames(data) %in% "LogViolentCrimesPerPop")])

# 1. Check constant variance
plot(basic_lin_model1$fitted,basic_lin_model1$residuals, xlab="Fitted values", ylab="Residual values")  # Constant variance assumption violated

# 2.Check normality of residuals
hist(basic_lin_model1$residuals,breaks=50) # Fairly symmetric, normal distribution
qqnorm(basic_lin_model1$residuals)
qqline(basic_lin_model1$residuals)

###################################### With Log ####################################

# Retry linear regression basic model with Log
basic_lin_model2 = lm(LogViolentCrimesPerPop ~ ., data=data[,!(colnames(data) %in% "ViolentCrimesPerPop")])

# 1. Check constant variance
plot(basic_lin_model2$fitted,basic_lin_model2$residuals, xlab="Fitted values", ylab="Residual values")  
# Constant variance assumption holds though still marginally questionable due to skew on the right but acceptable

# 2.Check normality of residuals
hist(basic_lin_model2$residuals,breaks=50) # Fairly symmetric, normal distribution
qqnorm(basic_lin_model2$residuals) # QQ plot traces the theoretical quantiles' line
qqline(basic_lin_model2$residuals)

# Conclusion = Taking Log makes sense. We are going to remove the ViolentCrimesPerPop response
data$ViolentCrimesPerPop = NULL

# Checking relationship between predicting variables and response
for(i in colnames(data[,!(colnames(data) %in% c("ViolentCrimesPerPop", "LogViolentCrimesPerPop"))])){
  plot(data[,i], data$LogViolentCrimesPerPop, main = i)
}
  
####### IV. Outlier removal using Cook's distance (Response = log(ViolentCrimesPerPop))######

model = lm(LogViolentCrimesPerPop ~ ., data = data)
cook = cooks.distance(model) # Vector with Cook's distance values
plot(cook,type="h",lwd=2, xlab="Data Points", ylab = "Cook's Distance") # Plot of Cook's distance to identify outliers

# Removing outliers
data_wo_outliers = data[-as.integer(row.names(as.data.frame(cook[cook>0.02]))),] # Delete outlier points
data_wt_outliers = data
dim(data_wo_outliers)

############ V. Checking for correlation between predicting variables ##################
#install.packages("corrplot")
library(corrplot)
corrplot(cor(as.matrix(data_wo_outliers)), tl.cex = .1)
cor(data_wo_outliers)
# As we can see, there are a lot of correlated variables, and hence we must perform variable 
# selection to account for the same

#################### Checking if taking log of some variables makes sense ##############

for(i in colnames(data_wo_outliers)) {
  par(mfrow=c(1,2))
  hist(data_wo_outliers[,i], main = i, xlab = "")
  hist(log(data_wo_outliers[,i]), main = paste0("Log(",i, ")"), xlab = "")
}

# Log-Transformed 6 predictor variables, which show a skewed distribution
data_wo_outliers_log = data_wo_outliers
data_wo_outliers_log$LogblackPerCap = log(data_wo_outliers_log$blackPerCap+1)
data_wo_outliers_log$LogindianPerCap = log(data_wo_outliers_log$indianPerCap+1)
data_wo_outliers_log$LogOtherPerCap = log(data_wo_outliers_log$OtherPerCap+1)
data_wo_outliers_log$LogHousVacant = log(data_wo_outliers_log$HousVacant+1)
data_wo_outliers_log$LogLandArea = log(data_wo_outliers_log$LandArea+1)
data_wo_outliers_log$LogPopDens = log(data_wo_outliers_log$PopDens+1)
data_wo_outliers_log$blackPerCap = NULL
data_wo_outliers_log$indianPerCap = NULL
data_wo_outliers_log$OtherPerCap = NULL
data_wo_outliers_log$HousVacant = NULL
data_wo_outliers_log$LandArea = NULL
data_wo_outliers_log$PopDens = NULL

###### VI. Scaling #######
# Required prior to Variable Selection
# R 'scale' function carries out standardization based on mean and SD
data_wo_scaled_log = as.data.frame(sapply(data_wo_outliers_log, scale))
data_wo_scaled = as.data.frame(sapply(data_wo_outliers, scale))
data_wt_scaled = as.data.frame(sapply(data_wt_outliers, scale))

###### VII. Variable Selection #######

##### Regularized Regression Approaches - LASSO & Elastic Net #####

# Define set.seed(100) for all steps to get the same output
set.seed(100)

library(glmnet) # Import glmnet for LASSO, Elastic Net

# Define the linear regression model for non-log predicting variables
basic_lm_scaled1 = lm(LogViolentCrimesPerPop ~ ., data=data_wo_outliers)
X_1 = model.matrix(basic_lm_scaled1); X_1 = X_1[,-c(1)]
y_1 = as.vector(data_wo_outliers[,'LogViolentCrimesPerPop'])

# Define the linear regression model for log predicting variables
basic_lm_scaled2 = lm(LogViolentCrimesPerPop ~ ., data=data_wo_outliers_log)
X_2 = model.matrix(basic_lm_scaled2); X_2 = X_2[,-c(1)]
y_2 = as.vector(data_wo_outliers_log[,'LogViolentCrimesPerPop'])

##### LASSO for non-log predicting variables #####
model_cv_lasso_1 = cv.glmnet(X_1, y_1, alpha=1, family = "gaussian", nfolds=10) # 10-fold cross-validation, k=10 standard
model_lasso_1 = glmnet(X_1, y_1, alpha=1, family = "gaussian", lambda = model_cv_lasso_1$lambda.min)
sum(model_lasso_1$beta == 0) # 47 coefficients with 0 beta values found

##### Elastic Net for non-log predicting variables #####
model_cv_elnet_1 = cv.glmnet(X_1, y_1, alpha=0.5, family = "gaussian", nfolds=10) # 10-fold cross-validation, k=10 standard
model_elnet_1 = glmnet(X_1, y_1, alpha=0.5, family = "gaussian", lambda = model_cv_elnet_1$lambda.min)
sum(model_elnet_1$beta == 0) # 21 coefficients with 0 beta values found

##### LASSO for log predicting variables #####
model_cv_lasso_2 = cv.glmnet(X_2, y_2, alpha=1, family = "gaussian", nfolds=10) # 10-fold cross-validation, k=10 standard
model_lasso_2 = glmnet(X_2, y_2, alpha=1, family = "gaussian", lambda = model_cv_lasso_2$lambda.min)
sum(model_lasso_2$beta == 0) # 47 coefficients with 0 beta values found

##### Elastic Net for log predicting variables #####
model_cv_elnet_2 = cv.glmnet(X_2, y_2, alpha=0.5, family = "gaussian", nfolds=10) # 10-fold cross-validation, k=10 standard
model_elnet_2 = glmnet(X_2, y_2, alpha=0.5, family = "gaussian", lambda = model_cv_elnet_2$lambda.min)
sum(model_elnet_2$beta == 0) # 21 coefficients with 0 beta values found

# Variables Removed by Lasso and ElasticNet for non-log predicting variables
droplist_lasso_1 = row.names(model_lasso_1$beta)[as.vector(model_lasso_1$beta == 0)]
droplist_elnet_1 = row.names(model_elnet_1$beta)[as.vector(model_elnet_1$beta == 0)]

# Variables Removed by Lasso and ElasticNet for log predicting variables
droplist_lasso_2 = row.names(model_lasso_2$beta)[as.vector(model_lasso_2$beta == 0)]
droplist_elnet_2 = row.names(model_elnet_2$beta)[as.vector(model_elnet_2$beta == 0)]

# Output from Regularized Regression Step for log predicting variables
data_fnl_regr_log = data_wo_outliers_log[,!(colnames(data_wo_outliers_log) %in% droplist_lasso_2)]
# Final dataset after LASSO has 1992X30 dimensions
dim(data_fnl_regr_log) 

###### 3. Stepwise Regression #######

# Stepwise Regression for log 
full_2 = lm(LogViolentCrimesPerPop ~ ., data = data_fnl_regr_log) # Define full model with all variables
null_2 = lm(LogViolentCrimesPerPop ~ 1, data = data_fnl_regr_log) # Define null model with no variables
model_both_2 = step(null_2, scope = list(upper=full_2), data=data_wo_outliers_log, direction="both")
summary(model_both_2)
length(model_both_2$coefficients)
# 23 variables, Multiple R-squared:  0.6596,	Adjusted R-squared:  0.6556 

##### VII. Multiple Linear Regression Model #####

# Selected variables for log
vars_select_2 = c((names(model_both_2$coefficients[2:length(model_both_2$coefficients)])),'LogViolentCrimesPerPop')
# Unscaled dataset consisting of 23 selected predictor variables and 1 response variable
data_mlr_unscaled_2 = data_wo_outliers_log[,vars_select_2]
dim(data_mlr_unscaled_2)
# 1992 x 24

# Multiple Linear Regression model fitted using unscaled data for log
mlr_model_2 = lm(LogViolentCrimesPerPop ~., data= data_mlr_unscaled_2) 
summary(mlr_model_2)
# MLR Model - Summary
# Multiple R-squared:  0.6596,	Adjusted R-squared:  0.6556
# p-value = 2.2e-16, ~= 0

##### VII. Goodness of Fit #####

# For log
# Constant variance & Independence
plot(mlr_model_2$fitted, mlr_model_2$residuals, xlab="Fitted values",ylab="Residuals") 
# Constant variance assumption holds though a narrowing is observed as we increase the fitted values
# Normality of residuals
hist(mlr_model_2$residuals, breaks=50)
qqnorm(mlr_model_2$residuals)
qqline(mlr_model_2$residuals)

