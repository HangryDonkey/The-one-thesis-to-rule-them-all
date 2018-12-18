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
#Loading input data
usinflation <- read.csv("M:/My Documents/R/Masteroppgave/Input/US monthly inflation dec1978-aug2018_2.csv", 
                        header = TRUE, sep = ";", dec = ",")
snp500 <- read.csv("M:/My Documents/R/Masteroppgave/Input/GSPC_2.csv", 
                   header = TRUE, sep = ";", dec = ",")
n225 <- read.csv("M:/My Documents/R/Masteroppgave/Input/N225.csv", 
                 header = TRUE, sep = ";", dec = ",")
dji <- read.csv("M:/My Documents/R/Masteroppgave/Input/DJI.csv", 
                header = TRUE, sep = ";", dec = ",")
fedf <- read.csv("M:/My Documents/R/Masteroppgave/Input/FED Funds Rate Effective.csv", 
                 header = TRUE, sep = ";", dec = ",")
tcm10 <- read.csv("M:/My Documents/R/Masteroppgave/Input/10-years Treasury Constant Maturity.csv", 
                  header = TRUE, sep = ";", dec = ",")
hsi <- read.csv("M:/My Documents/R/Masteroppgave/Input/hsi.csv", 
                header = TRUE, sep = ";", dec = ",")
shcomp <- read.csv("M:/My Documents/R/Masteroppgave/Input/shcomp.csv", 
                   header = TRUE, sep = ";", dec = ",")
dxy <- read.csv("M:/My Documents/R/Masteroppgave/Input/dxy.csv", 
                header = TRUE, sep = ";", dec = ",")
wti <- read.csv("M:/My Documents/R/Masteroppgave/Input/wti.csv", 
                header = TRUE, sep = ";", dec = ",")
usdcny <- read.csv("M:/My Documents/R/Masteroppgave/Input/usdcny.csv", 
                   header = TRUE, sep = ";", dec = ",")
durgoods <- read.csv("M:/My Documents/R/Masteroppgave/Input/Durable Goods.csv", 
                     header = TRUE, sep = ";", dec = ",")
vix <- read.csv("M:/My Documents/R/Masteroppgave/Input/vix.csv", 
                header = TRUE, sep = ";", dec = ",")
goldsupply <- read.csv("M:/My Documents/R/Masteroppgave/Input/Gold supply.csv", 
                       header = TRUE, sep = ";", dec = ",")
unemployment<- read.csv("M:/My Documents/R/Masteroppgave/Input/Unemployment rate.csv", 
                        header = TRUE, sep = ";", dec = ",")
mswi <- read.csv("M:/My Documents/R/Masteroppgave/Input/mswi.csv", 
                 header = TRUE, sep = ";", dec = ",")

#Replacing commas with dots 
goldprice$Gold <- sapply(goldprice$Gold, gsub, pattern = ",", replacement= ".")
n225$N225 <- sapply(n225$N225, gsub, pattern = ",", replacement= ".")
dji$dji <- sapply(dji$dji, gsub, pattern = ",", replacement= ".")
fedf$FF_eff <- sapply(fedf$FF_eff, gsub, pattern = ",", replacement= ".")
tcm10$TCMNOM_Y10 <- sapply(tcm10$TCMNOM_Y10, gsub, pattern = ",", replacement= ".")
hsi$hsi <- sapply(hsi$hsi, gsub, pattern = ",", replacement= ".")
shcomp$shcomp <- sapply(shcomp$shcomp, gsub, pattern = ",", replacement= ".")
dxy$dxy <- sapply(dxy$dxy, gsub, pattern = ",", replacement= ".")
wti$wti <- sapply(wti$wti, gsub, pattern = ",", replacement= ".")
usdcny$usdcny <- sapply(usdcny$usdcny, gsub, pattern = ",", replacement= ".")
durgoods$Durable.goods <- sapply(durgoods$Durable.goods, gsub, pattern = ",", replacement= ".")
vix$vix <- sapply(vix$vix, gsub, pattern = ",", replacement= ".")
goldsupply$Gold.supply <- sapply(goldsupply$Gold.supply, gsub, pattern = ",", replacement= ".")
unemployment$Unemployment <- sapply(unemployment$Unemployment, gsub, pattern = ",", replacement= ".")
mswi$mswi <- sapply(mswi$mswi, gsub, pattern = ",", replacement= ".")

#Removing whitespace
goldprice$Gold <- gsub('\\s+', '', goldprice$Gold)
n225$N225 <- gsub('\\s+', '', n225$N225)
dji$dji <- gsub('\\s+', '', dji$dji)
fedf$FF_eff <- gsub('\\s+', '', fedf$FF_eff)
tcm10$TCMNOM_Y10 <- gsub('\\s+', '', tcm10$TCMNOM_Y10)
hsi$hsi <- gsub('\\s+', '', hsi$hsi)
shcomp$shcomp <- gsub('\\s+', '', shcomp$shcomp)
dxy$dxy <- gsub('\\s+', '', dxy$dxy)
wti$wti <- gsub('\\s+', '', wti$wti)
usdcny$usdcny <- gsub('\\s+', '', usdcny$usdcny)
durgoods$Durable.goods <- gsub('\\s+', '', durgoods$Durable.goods)
vix$vix <- gsub('\\s+', '', vix$vix)
goldsupply$Gold.supply <- gsub('\\s+', '', goldsupply$Gold.supply)
unemployment$Unemployment <- gsub('\\s+', '', unemployment$Unemployment)
mswi$mswi <- gsub('\\s+', '', mswi$mswi)

#Converting all factor variables to numeric
goldprice$Gold <- sapply(goldprice$Gold, as.numeric)
n225$N225 <- sapply(n225$N225, as.numeric)
dji$dji <- sapply(dji$dji, as.numeric)
fedf$FF_eff <- sapply(fedf$FF_eff, as.numeric)
tcm10$TCMNOM_Y10 <- sapply(tcm10$TCMNOM_Y10, as.numeric)
hsi$hsi <- sapply(hsi$hsi, as.numeric)
shcomp$shcomp <- sapply(shcomp$shcomp, as.numeric)
dxy$dxy <- sapply(dxy$dxy, as.numeric)
wti$wti <- sapply(wti$wti, as.numeric)
usdcny$usdcny <- sapply(usdcny$usdcny, as.numeric)
durgoods$Durable.goods <- sapply(durgoods$Durable.goods, as.numeric)
vix$vix <- sapply(vix$vix, as.numeric)
goldsupply$Gold.supply <- sapply(goldsupply$Gold.supply, as.numeric)
unemployment$Unemployment <- sapply(unemployment$Unemployment, as.numeric)
mswi$mswi <- sapply(mswi$mswi, as.numeric)


#Interpolation of data with fewer data points (usinflation, durgoods, goldsupply, unemployment)
#Obtaining x and y coordinates for the plotting
#usinflation
xy.coords(usinflation$X.TIME., usinflation$X.Value.)
plotstruc_usinlation <- xy.coords(usinflation$X.TIME., usinflation$X.Value.)
#durgoods
xy.coords(durgoods$Time, durgoods$Durable.goods)
plotstruc_durgoods <- xy.coords(durgoods$Time, durgoods$Durable.goods)
#goldsupply
xy.coords(goldsupply$Time, goldsupply$Gold.supply)
plotstruc_goldsupply <- xy.coords(goldsupply$Time, goldsupply$Gold.supply)
#unemployment
xy.coords(unemployment$Time, unemployment$Unemployment)
plotstruc_unemployment <- xy.coords(unemployment$Time, unemployment$Unemployment)

#Running spline with the xycoord
#usinflation
spline(plotstruc_usinlation, n = 10356, method = "fmm")
splineout_usinflation <- spline(plotstruc_usinlation, n = 10356, method = "fmm")
#durgoods
spline(plotstruc_durgoods, n = 6921, method = "fmm")
splineout_durgoods <- spline(plotstruc_durgoods, n = 6921, method = "fmm")
#goldsupply
spline(plotstruc_goldsupply, n = 5181, method = "fmm")
splineout_goldsupply <- spline(plotstruc_goldsupply, n = 5181, method = "fmm")
#unemployment
spline(plotstruc_unemployment, n = 10356, method = "fmm")
splineout_unemployment <- spline(plotstruc_unemployment, n = 10356, method = "fmm")

#Plotting new estimated observations (line) original observations (dots)
#usinflation
plot(plotstruc_usinlation)
points(splineout_usinflation$x, splineout_usinflation$y, type = "l", col = "red")
#durgoods
plot(plotstruc_durgoods)
points(splineout_durgoods$x, splineout_durgoods$y, type = "l", col = "red")
#goldsupply
plot(plotstruc_goldsupply)
points(splineout_goldsupply$x, splineout_goldsupply$y, type = "l", col = "red")
#unemployment
plot(plotstruc_unemployment)
points(splineout_unemployment$x, splineout_unemployment$y, type = "l", col = "red")


#Merge data frames by "Time"
goldprice_merge1 <- merge.data.frame(goldprice, snp500, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge2 <- merge.data.frame(goldprice_merge1, n225, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge3 <- merge.data.frame(goldprice_merge2, fedf, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge4 <- merge.data.frame(goldprice_merge3, dji, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge5 <- merge.data.frame(goldprice_merge4, tcm10, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge6 <- merge.data.frame(goldprice_merge5, hsi, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge7 <- merge.data.frame(goldprice_merge6, shcomp, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge8 <- merge.data.frame(goldprice_merge7, dxy, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge9 <- merge.data.frame(goldprice_merge8, wti, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge10 <- merge.data.frame(goldprice_merge9, usdcny, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge11 <- merge.data.frame(goldprice_merge10, vix, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge12 <- merge.data.frame(goldprice_merge11, durgoods, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge13 <- merge.data.frame(goldprice_merge12, goldsupply, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)
goldprice_merge14 <- merge.data.frame(goldprice_merge13, mswi, by.x = c("Time"), by.y = c("Time"), all.x = TRUE, all.y = TRUE)


#Subdividing data frame on time
#Pre-crisis: 1999-01-01 -> 2007-12-31
pre_goldmerge <- goldprice_merge14[5221:7567,]
set.seed(200)
sample = sample.split(pre_goldmerge, SplitRatio = 0.75)
train_pre_goldmerge = subset(pre_goldmerge, sample == TRUE)
test_pre_goldmerge = subset(pre_goldmerge, sample == FALSE)

#Intercrisis: 2008-01-01 -> 2009-06-30
inter_goldmerge <- goldprice_merge14[7568:7958,]
set.seed(200)
sample = sample.split(inter_goldmerge, SplitRatio = 0.75)
train_inter_goldmerge = subset(inter_goldmerge, sample == TRUE)
test_inter_goldmerge = subset(inter_goldmerge, sample == FALSE)

#Post-crisis: 2009-07-01 -> 2018-09-07
post_goldmerge <- goldprice_merge14[7959:10356,]
set.seed(200)
sample = sample.split(post_goldmerge, SplitRatio = 0.75)
train_post_goldmerge = subset(post_goldmerge, sample == TRUE)
test_post_goldmerge = subset(post_goldmerge, sample == FALSE)


#Removing NAs
row.has.na <- apply(train_pre_goldmerge, 1, function(x){any(is.na(x))})
sum(row.has.na)
pre_train_goldmerge_filtered <- train_pre_goldmerge[!row.has.na,]
row.has.na <- apply(test_pre_goldmerge, 1, function(x){any(is.na(x))})
sum(row.has.na)
pre_test_goldmerge_filtered <- test_pre_goldmerge[!row.has.na,]

row.has.na <- apply(train_inter_goldmerge, 1, function(x){any(is.na(x))})
sum(row.has.na)
inter_train_goldmerge_filtered <- train_inter_goldmerge[!row.has.na,]
row.has.na <- apply(test_inter_goldmerge, 1, function(x){any(is.na(x))})
sum(row.has.na)
inter_test_goldmerge_filtered <- test_inter_goldmerge[!row.has.na,]

row.has.na <- apply(train_post_goldmerge, 1, function(x){any(is.na(x))})
sum(row.has.na)
post_train_goldmerge_filtered <- train_post_goldmerge[!row.has.na,]
row.has.na <- apply(test_post_goldmerge, 1, function(x){any(is.na(x))})
sum(row.has.na)
post_test_goldmerge_filtered <- test_post_goldmerge[!row.has.na,]


#Excluding specific columns
#"Time"
pre_train_goldmerge_filtered$Time <- NULL
pre_test_goldmerge_filtered$Time <- NULL
inter_train_goldmerge_filtered$Time <- NULL
inter_test_goldmerge_filtered$Time <- NULL
post_train_goldmerge_filtered$Time <- NULL
post_test_goldmerge_filtered$Time <- NULL


#"usdcny" (in pre_goldmerge_filtered as cny was pegged to usd until 2005)
pre_train_goldmerge_filtered$usdcny <- NULL
pre_test_goldmerge_filtered$usdcny <- NULL


#XGBoost
#Converting data frame into a xgb.Dmatrix as input for the xgboost model
pre_dtrain <- xgb.DMatrix(data = pre_train_goldmerge_filtered, label = pre_train_goldmerge_filtered$Gold)
pre_dtest <- xgb.DMatrix(data = pre_test_goldmerge_filtered, label = pre_test_goldmerge_filtered$Gold)

inter_dtrain <- xgb.DMatrix(data = inter_train_goldmerge_filtered, label = inter_train_goldmerge_filtered$Gold)
inter_dtest <- xgb.DMatrix(data = inter_test_goldmerge_filtered, label = inter_test_goldmerge_filtered$Gold)

post_dtrain <- xgb.DMatrix(data = post_train_goldmerge_filtered, label = post_train_goldmerge_filtered$Gold)
post_dtest <- xgb.DMatrix(data = post_test_goldmerge_filtered, label = post_test_goldmerge_filtered$Gold)

#Finding optimal number of nrounds, and running XGb-model
#Pre-crisis
pre_params <- list(booster ="gbtree", eta = 0.015, gamma = 0, max_depth = 6,
                   min_child_weight = 1, subsample = 1, colsample_bytree = 1)
pre_bstgold <- xgboost(params = pre_params, data = pre_dtrain, nround = 160)
pre_xgcv <- xgb.cv(data = pre_dtrain, nround = 500, nfold = 7, 
                   early_stopping_rounds = 8, maximize = FALSE)

#Inter-crisis
inter_params <- list(booster ="gbtree", eta = 0.025, gamma = 0, max_depth = 6,
                   min_child_weight = 1, subsample = 1, colsample_bytree = 1)
inter_bstgold <- xgboost(params = inter_params, data = inter_dtrain, nround = 120)
inter_xgcv <- xgb.cv(data = inter_dtrain, nround = 500, nfold = 7, 
                     early_stopping_rounds = 8, maximize = FALSE)

#Post-crisis
post_params <- list(booster ="gbtree", eta = 0.015, gamma = 0, max_depth = 6,
                   min_child_weight = 1, subsample = 1, colsample_bytree = 1)
post_bstgold <- xgboost(params = post_params, data = post_dtrain, nround = 180)
post_xgcv <- xgb.cv(data = post_dtrain, nround = 500, nfold = 7, 
                    early_stopping_rounds = 8, maximize = FALSE)

#Feature importance plot
pre_impmat <- xgb.importance(model = pre_bstgold)
xgb.plot.importance(pre_impmat)
pre_impmat

inter_impmat <- xgb.importance(model = inter_bstgold)
xgb.plot.importance(inter_impmat)
inter_impmat

post_impmat <- xgb.importance(model = post_bstgold)
xgb.plot.importance(post_impmat)
post_impmat

#Prediction xgboost
pred_pre_xgb <- predict(pre_bstgold, pre_dtest)
pred_inter_xgb <- predict(inter_bstgold, inter_dtest)
pred_post_xgb <- predict(post_bstgold, post_dtest)



#GBM
#Performing gradient boosting on the data frames
#Pre-crisis
pre_gbm <- gbm(formula = Gold ~ ., data = pre_train_goldmerge_filtered,distribution = "gaussian", 
               cv.folds = 7,n.trees = 500, shrinkage = 0.0020,interaction.depth = 3) 
#Optimising number of trees for the pre-crisis gbm
pre_opt_trees <- gbm.perf(pre_gbm, method = "cv")
pre_opt_trees

#Inter-crisis
inter_gbm <- gbm(formula = Gold ~ ., data = inter_train_goldmerge_filtered, distribution = "gaussian", 
                 cv.folds = 7, n.trees = 250, shrinkage = 0.0010, interaction.depth = 3) 
#Optimising number of trees for the inter-crisis gbm
inter_opt_trees <- gbm.perf(inter_gbm, method = "cv")
inter_opt_trees

#Post-crisis
post_gbm <- gbm(formula = Gold ~ ., data = post_train_goldmerge_filtered, distribution = "gaussian", 
                cv.folds = 7, n.trees = 500, shrinkage = 0.0015, interaction.depth = 3) 
#Optimising number of trees for the post-crisis gbm
post_opt_trees <- gbm.perf(post_gbm, method = "cv")
post_opt_trees

#Feature importance plot
plot(pre_gbm, 'wti', return.grid = FALSE)
summary(pre_gbm)
plot(inter_gbm, 'unemployment', return.grid = FALSE)
summary(inter_gbm)
plot(post_gbm, 'wti', return.grid = FALSE)
summary(post_gbm)

#Prediction GBM
pred_pre_gbm <- predict(pre_gbm, newdata = pre_test_goldmerge_filtered, n.trees = 500)
pred_inter_gbm <- predict(inter_gbm, newdata = inter_test_goldmerge_filtered, n.trees = 250)
pred_post_gbm <- predict(post_gbm, newdata = post_test_goldmerge_filtered, n.trees = 500)


#Model performance
#Performance measures
#Mean Absolute Error (MAE)
#XGboosting (MAE)
MAE.XGb_pre <- function(actual, predicted) {mean(abs(actual- predicted))}
MAE.XGb_pre(pre_test_goldmerge_filtered$Gold, pred_pre_xgb)
MAE.XGb_inter <- function(actual, predicted) {mean(abs(actual- predicted))}
MAE.XGb_inter(inter_test_goldmerge_filtered$Gold, pred_inter_xgb)
MAE.XGb_post <- function(actual, predicted) {mean(abs(actual- predicted))}
MAE.XGb_post(post_test_goldmerge_filtered$Gold, pred_post_xgb)

#Gradient boosting (MAE)
MAE.gbm_pre <- function(actual, predicted) {mean(abs(actual- predicted))}
MAE.gbm_pre(pre_test_goldmerge_filtered$Gold, pred_pre_gbm)
MAE.gbm_inter <- function(actual, predicted) {mean(abs(actual- predicted))}
MAE.gbm_inter(inter_test_goldmerge_filtered$Gold, pred_inter_gbm)
MAE.gbm_post <- function(actual, predicted) {mean(abs(actual- predicted))}
MAE.gbm_post(post_test_goldmerge_filtered$Gold, pred_post_gbm)

#Root Mean Squared Error (RMSE)
#XGboosting (RMSE)
RMSE.XGb_pre <- rmspe(pre_test_goldmerge_filtered$Gold, pred_pre_xgb, includeSE = TRUE)
RMSE.XGb_pre
RMSE.XGb_inter <- rmspe(inter_test_goldmerge_filtered$Gold, pred_inter_xgb, includeSE = TRUE)
RMSE.XGb_inter
RMSE.XGb_post <- rmspe(post_test_goldmerge_filtered$Gold, pred_post_xgb, includeSE = TRUE)
RMSE.XGb_post

#Gradient boosting (RMSE)
RMSE.gbm_pre <- rmspe(pre_test_goldmerge_filtered$Gold, pred_pre_gbm, includeSE = TRUE)
RMSE.gbm_pre
RMSE.gbm_inter <- rmspe(inter_test_goldmerge_filtered$Gold, pred_inter_gbm, includeSE = TRUE)
RMSE.gbm_inter
RMSE.gbm_post <- rmspe(post_test_goldmerge_filtered$Gold, pred_post_gbm, includeSE = TRUE)
RMSE.gbm_post

#R-squared (R2)
#XGboosting (R2)
R2.XGb_pre <- 1 - (sum((pre_test_goldmerge_filtered$Gold - pred_pre_xgb)^2)
                   /sum((pre_test_goldmerge_filtered$Gold-mean(pre_test_goldmerge_filtered$Gold))^2))
R2.XGb_pre
R2.XGb_inter <- 1 - (sum((inter_test_goldmerge_filtered$Gold - pred_inter_xgb)^2)
                     /sum((inter_test_goldmerge_filtered$Gold-mean(inter_test_goldmerge_filtered$Gold))^2))
R2.XGb_inter
R2.XGb_post <- 1 - (sum((post_test_goldmerge_filtered$Gold - pred_post_xgb)^2)
                    /sum((post_test_goldmerge_filtered$Gold-mean(post_test_goldmerge_filtered$Gold))^2))
R2.XGb_post

#Gradient boosting (R2)
R2.gbm_pre <- 1 - (sum((pre_test_goldmerge_filtered$Gold - pred_pre_gbm)^2)
                   /sum((pre_test_goldmerge_filtered$Gold-mean(pre_test_goldmerge_filtered$Gold))^2))
R2.gbm_pre
R2.gbm_inter <- 1 - (sum((inter_test_goldmerge_filtered$Gold - pred_inter_gbm)^2)
                     /sum((inter_test_goldmerge_filtered$Gold-mean(inter_test_goldmerge_filtered$Gold))^2))
R2.gbm_inter
R2.gbm_post <- 1 - (sum((post_test_goldmerge_filtered$Gold - pred_post_gbm)^2)
                    /sum((post_test_goldmerge_filtered$Gold-mean(post_test_goldmerge_filtered$Gold))^2))
R2.gbm_post

#Normalized MAE and RMSE
#XGBoosting (NMAE and NRMSE)
NMAE.XGb_pre <- tdStats(pred_pre_xgb, pre_test_goldmerge_filtered$Gold)
NMAE.XGb_pre
NMAE.XGb_inter <- tdStats(pred_inter_xgb, inter_test_goldmerge_filtered$Gold)
NMAE.XGb_inter
NMAE.XGb_post <- tdStats(pred_post_xgb, post_test_goldmerge_filtered$Gold)
NMAE.XGb_post

#Gradient boosting (NMAE and NRMSE)
NMAE.gbm_pre <- tdStats(pred_pre_gbm, pre_test_goldmerge_filtered$Gold)
NMAE.gbm_pre
NMAE.gbm_inter <- tdStats(pred_inter_gbm, inter_test_goldmerge_filtered$Gold)
NMAE.gbm_inter
NMAE.gbm_post <- tdStats(pred_post_gbm, post_test_goldmerge_filtered$Gold)
NMAE.gbm_post


