getwd()
dir()
ls()
rm(list = ls(all = TRUE))
# Get Data ---------------------------------------------------------------------
library(data.table)
library(bit64)
train_load <- fread(input = "train.csv", data.table = FALSE)
train_load <- train_load[sample(nrow(train_load)),]
test_load <- fread(input = "test.csv", data.table = FALSE)
train_raw <- train_load
test_raw <- test_load
### Preprocessing
n_train <- nrow(train_raw)
n_test <- nrow(test_raw)
#
dim(train_raw)
dim(test_raw)
all(names(train_raw)[-ncol(train_raw)] == names(test_raw))
all(sapply(train_raw, function(x) is.numeric(x)))
