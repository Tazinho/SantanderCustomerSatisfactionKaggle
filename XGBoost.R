getwd()
#dir()
ls()
rm(list = ls(all = TRUE))
# Get Data ---------------------------------------------------------------------
library(data.table)
library(bit64)
train_load <- fread(input = "train.csv", data.table = FALSE)
train_load <- train_load[sample(nrow(train_load)),]
new_load <- fread(input = "test.csv", data.table = FALSE)
new_load <- new_load[sample(nrow(new_load)),]
train_raw <- train_load
new_raw <- new_load
# Preprocessing ----------------------------------------------------------------
n_train <- nrow(train_raw)
n_new <- nrow(new_raw)
# Summary ----------------------------------------------------------------------
dim(train_raw)
dim(new_raw)
all(names(train_raw)[-ncol(train_raw)] == names(new_raw))
all(sapply(train_raw, function(x) is.numeric(x)))
mean(train_raw$TARGET)
# Create Testset
library(caret)
#cdp <- createDataPartition(1:n_train, 1, p = 0.7, list = FALSE)
train_xgb <- train_raw#[cdp,]
#test_xgb <- train_raw[-cdp,]
new_xgb <- new_raw
# XGB Preprocessing
library(xgboost)
train <- list()
train$data <- as.matrix(train_xgb[, -c(1,which(names(train_xgb)=="TARGET"))])
train$label <- train_xgb[, which(names(train_xgb)=="TARGET")]
dtrain <- xgb.DMatrix(data = train$data, label = train$label, missing = NaN)

#test <- list()
#test$data <- as.matrix(test_xgb[, -c(1,which(names(test_xgb)=="TARGET"))])
#test$label <- test_xgb[, which(names(test_xgb)=="TARGET")]
#dtest <- xgb.DMatrix(data = test$data, label = test$label, missing = NaN)

new_id <- new_xgb$ID
new_xgb <- new_xgb[,-c(which(names(new_xgb)=="ID"))]
new_xgb$TARGET <- rep(NA_integer_, n_new)
new <- list()
new$data <- as.matrix(new_xgb[, -c(1,which(names(new_xgb)=="TARGET"))])
new$label <- new_xgb[, which(names(new_xgb)=="TARGET")]
dnew <- xgb.DMatrix(data = new$data, label = new$label, missing = NaN)

# XGB Parameter
nround = 500
param_tree <- list(booster = "gbtree",
                   silent = 1,
                   "objective" = "binary:logistic", 
                   base_score = 0.5,
                   "eval_metric" = "auc",
                   "eta" = 0.075, 
                   "gamma" = 0.1,
                   "max_depth" = 20,
                   "min_child_weight" = 25,
                   "subsample" = 0.6,
                   "colsample_bytree" = 0.5)
param_linear <- list(booster = "gblinear",
                     silent = 1,
                     "objective" = "binary:logistic", 
                     base_score = 0.5,
                     "eval_metric" = "auc",
                     #"eta" = 0.1, 
                     "gamma" = 0.1,
                     "max_depth" = 17,
                     "min_child_weight" = 100,
                     "subsample" = 0.5,
                     "colsample_bytree" = 0.5)
# XGB Crossvalidation
xgb.cv(params = param_tree, data = dtrain, nrounds = nround, nfold =4,
       prediction = FALSE, showsd = TRUE, metrics = list("auc"),
       stratified = TRUE,
       verbose = F, print.every.n = 1L)
# Training
bst <- xgb.train(params = param_tree, data = dtrain, nrounds = nround)
# Predictions
pred <- predict(object = bst, newdata = dnew)
# Submission
library(dplyr)
submission <- cbind.data.frame(ID = new_id, TARGET = pred) %>% arrange(ID)
dim(submission)
# Write Data
write.csv(submission, "sub1.csv", na="", quote = FALSE, row.names = FALSE)

