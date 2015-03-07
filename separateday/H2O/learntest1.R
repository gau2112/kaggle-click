# initialization ----------------------------------------------------------
# The following two commands remove any previously installed H2O packages for R.
#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
#install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1601/R", getOption("repos"))))

library(h2o)
#library(plyr)
# java -Xmx16G -jar h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision
localH2O <- h2o.init(ip = 'localhost', port = 54321)

myseed=1988
days <- commandArgs(trailingOnly = TRUE)
days <- as.numeric(days)
# the day used to be tested
testday <- days[1]
# the day whose models to test
day <- days[2]
#day <- 28
path <- "/home/whale/Documents/click"

# splite ------------------------------------------------------------------
cat("\nSplitting into train/validation")
train <- h2o.getFrame(localH2O, "train.hex")
# Random split
rand <- h2o.runif(train, seed = myseed)
rand <- h2o.assign(rand, key = "rand")
train_train <- train[rand <= 0.8, ]
train_train <- h2o.assign(train_train, key = "train_train")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
train_valid <- train[rand > 0.8, ]
train_valid <- h2o.assign(train_valid, key = "train_valid")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "rand", x = h2o.ls(localH2O)$Key, value = TRUE))

# testing -----------------------------------------------------------------

#submission <- read.csv(paste0(path,"/submission",testday,".csv"), colClasses = c("character"))
submission <- read.csv(paste0(path,"/submission",testday,".csv"))

cat("\nTesting data: deep learning 1")
dlmodel1 <- h2o.loadModel(localH2O, paste0(path,"/dlmodel1count",day))
test_preddl1 <- h2o.predict(dlmodel1, train)[,3]
colnames(test_preddl1) <- "dl1"
submission[,2] <- as.data.frame(test_preddl1)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste0(path,"/dlmodel1countorigin",day,"_",testday,".csv"), quote = F, row.names = F)
h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(dlmodel1,test_preddl1)

cat("\nTesting data: deep learning 2")
dlmodel2 <- h2o.loadModel(localH2O, paste0(path,"/dlmodel2count",day))
test_preddl2 <- h2o.predict(dlmodel2, train)[,3]
colnames(test_preddl2) <- "dl2"
submission[,2] <- as.data.frame(test_preddl2)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste0(path,"/dlmodel2countorigin",day,"_",testday,".csv"), quote = F, row.names = F)
h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(dlmodel2,test_preddl2)

cat("\nTesting data: GBM")
gbmmodel <- h2o.loadModel(localH2O, paste0(path,"/gbmmodelcount",day))
test_predgbm <- h2o.predict(gbmmodel, train)[,3]
colnames(test_predgbm) <- "gbm"
submission[,2] <- as.data.frame(test_predgbm)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste0(path,"/gbmmodelcountorigin",day,"_",testday,".csv"), quote = F, row.names = F)
h2o.rm(localH2O, grep(pattern = "GBM", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(gbmmodel,test_predgbm)
