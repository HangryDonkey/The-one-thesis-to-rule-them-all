setwd("M:/My Documents/R/Masteroppgave/Input")


#Downloading necessary packages to conduct interpolation
#install.packages("stats")
library(stats)
#Install random sampling tools
install.packages("caTools")
library(caTools)
#Installing packages for xgboost and creating matrices
install.packages("Matrix")
library(Matrix)
install.packages("xgboost")
library(xgboost)
install.packages("igraph")
library(igraph)
#Installing packages to perform gbp
install.packages("gbm")
library(gbm)
#Installing packages for testing model accuracy (rmse, nrmse and nmae)
install.packages("perry")
library(perry)
install.packages("hydroGOF")
library(hydroGOF)
install.packages("tdr")
library(tdr)

#Loading gold price data
goldprice <- read.csv("M:/My Documents/R/Masteroppgave/Input/Gold price daily 29.12.1978-08.09.2018_2.csv",
                      header = TRUE, sep = ";", dec = ",")
#Loading input data ()
wti <- read.csv("M:/My Documents/R/Masteroppgave/Input/wti.csv", 
                header = TRUE, sep = ";", dec = ",")

#Replacing commas with dots 
goldprice$Gold <- sapply(goldprice$Gold, gsub, pattern = ",", replacement= ".")
wti$wti <- sapply(wti$wti, gsub, pattern = ",", replacement= ".")

#Removing whitespace
goldprice$Gold <- gsub('\\s+', '', goldprice$Gold)
wti$wti <- gsub('\\s+', '', wti$wti)

#Converting all factor variables to numeric
goldprice$Gold <- sapply(goldprice$Gold, as.numeric)
wti$wti <- sapply(wti$wti, as.numeric)


#Interpolation of data with fewer data points (usinflation, durgoods, goldsupply, unemployment)
#Obtaining x and y coordinates for the plotting
#usinflation
xy.coords(usinflation$X.TIME., usinflation$X.Value.)
plotstruc_usinlation <- xy.coords(usinflation$X.TIME., usinflation$X.Value.)

#Running spline with the xycoord
#usinflation
spline(plotstruc_usinlation, n = 10356, method = "fmm")
splineout_usinflation <- spline(plotstruc_usinlation, n = 10356, method = "fmm")

#Plotting new estimated observations (line) original observations (dots)
#usinflation
plot(plotstruc_usinlation)
points(splineout_usinflation$x, splineout_usinflation$y, type = "l", col = "red")


#Merge data frames by "Time"
goldprice_merge1 <- merge.data.frame(goldprice, wti, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)

#Subdividing data frame on time
#Pre-crisis: 1999-01-01 -> 2007-12-31
pre_goldmerge <- goldprice_merge1[5221:7567,]
set.seed(200)
sample = sample.split(pre_goldmerge, SplitRatio = 0.75)
train_pre_goldmerge = subset(pre_goldmerge, sample == TRUE)
test_pre_goldmerge = subset(pre_goldmerge, sample == FALSE)

#Removing NAs
row.has.na <- apply(train_pre_goldmerge, 1, function(x){any(is.na(x))})
sum(row.has.na)
pre_train_goldmerge_filtered <- train_pre_goldmerge[!row.has.na,]
row.has.na <- apply(test_pre_goldmerge, 1, function(x){any(is.na(x))})
sum(row.has.na)
pre_test_goldmerge_filtered <- test_pre_goldmerge[!row.has.na,]

#Excluding specific columns
#"Time"
pre_train_goldmerge_filtered$Time <- NULL
pre_test_goldmerge_filtered$Time <- NULL

#"usdcny" (in pre_goldmerge_filtered as cny was pegged to usd until 2005)
pre_train_goldmerge_filtered$usdcny <- NULL
pre_test_goldmerge_filtered$usdcny <- NULL


#XGBoost
#Converting data frame into a xgb.Dmatrix as input for the xgboost model
pre_dtrain <- xgb.DMatrix(data = pre_train_goldmerge_filtered, label = pre_train_goldmerge_filtered$Gold)
pre_dtest <- xgb.DMatrix(data = pre_test_goldmerge_filtered, label = pre_test_goldmerge_filtered$Gold)

#Finding optimal number of nrounds, and running XGb-model
#Pre-crisis
pre_params <- list(booster ="gbtree", eta = 0.015, gamma = 0, max_depth = 6,
                   min_child_weight = 1, subsample = 1, colsample_bytree = 1)
pre_bstgold <- xgboost(params = pre_params, data = pre_dtrain, nround = 160)
pre_xgcv <- xgb.cv(data = pre_dtrain, nround = 500, nfold = 7, 
                   early_stopping_rounds = 8, maximize = FALSE)

#Feature importance plot
pre_impmat <- xgb.importance(model = pre_bstgold)
xgb.plot.importance(pre_impmat)
pre_impmat

#Prediction XGBoost
pred_pre_xgb <- predict(pre_bstgold, pre_dtest)


#GBM
#Performing gradient boosting on the data frames
#Pre-crisis
pre_gbm <- gbm(formula = Gold ~ ., data = pre_train_goldmerge_filtered,distribution = "gaussian", 
               cv.folds = 7,n.trees = 500, shrinkage = 0.0020,interaction.depth = 3) 
#Optimising number of trees for the pre-crisis gbm
pre_opt_trees <- gbm.perf(pre_gbm, method = "cv")
pre_opt_trees

#Feature importance plot
plot(pre_gbm, 'wti', return.grid = FALSE)
summary(pre_gbm)

#Prediction GBM
pred_pre_gbm <- predict(pre_gbm, newdata = pre_test_goldmerge_filtered, n.trees = 500)


#Performance measures
#Mean Absolute Error (MAE)
#XGboosting (MAE)
MAE.XGb_pre <- function(actual, predicted) {mean(abs(actual- predicted))}
MAE.XGb_pre(pre_test_goldmerge_filtered$Gold, pred_pre_xgb)

#Gradient boosting (MAE)
MAE.gbm_pre <- function(actual, predicted) {mean(abs(actual- predicted))}
MAE.gbm_pre(pre_test_goldmerge_filtered$Gold, pred_pre_gbm)

#Root Mean Squared Error (RMSE)
#XGboosting (RMSE)
RMSE.XGb_pre <- rmspe(pre_test_goldmerge_filtered$Gold, pred_pre_xgb, includeSE = TRUE)
RMSE.XGb_pre

#Gradient boosting (RMSE)
RMSE.gbm_pre <- rmspe(pre_test_goldmerge_filtered$Gold, pred_pre_gbm, includeSE = TRUE)
RMSE.gbm_pre

#R-squared (R2)
#XGboosting (R2)
R2.XGb_pre <- 1 - (sum((pre_test_goldmerge_filtered$Gold - pred_pre_xgb)^2)
                   /sum((pre_test_goldmerge_filtered$Gold-mean(pre_test_goldmerge_filtered$Gold))^2))
R2.XGb_pre

#Gradient boosting (R2)
R2.gbm_pre <- 1 - (sum((pre_test_goldmerge_filtered$Gold - pred_pre_gbm)^2)
                   /sum((pre_test_goldmerge_filtered$Gold-mean(pre_test_goldmerge_filtered$Gold))^2))
R2.gbm_pre

#Normalized MAE and RMSE
#XGBoosting (NMAE and NRMSE)
NMAE.XGb_pre <- tdStats(pred_pre_xgb, pre_test_goldmerge_filtered$Gold)
NMAE.XGb_pre

#Gradient boosting (NMAE and NRMSE)
NMAE.gbm_pre <- tdStats(pred_pre_gbm, pre_test_goldmerge_filtered$Gold)
NMAE.gbm_pre



