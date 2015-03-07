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

non_cols <- c("id","click")
names <- colnames(train)
names <- names[! names %in% non_cols]

cat("\nTesting data: autoencoder and GBM 1")
cat("\nautoencoder 1:")
automodel1 <- h2o.loadModel(localH2O, paste0(path,"/automodel1count",day))
train_auto <- h2o.deepfeatures(train[,names], automodel1, layer=-1)
train_auto <- h2o.assign(train_auto, "train_auto")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "DeepFeatures", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(automodel1)
cat("\nGBM 1:")
gbmautomodel1 <- h2o.loadModel(localH2O, paste0(path,"/autogbmmodel1count",day))
test_predgbmauto <- h2o.predict(gbmautomodel1, train_auto)[,3]
colnames(test_predgbmauto) <- "gbmauto"
submission[,2] <- as.data.frame(test_predgbmauto)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste0(path,"/autogbmmodel1count",day,"_",testday,".csv"), quote = F, row.names = F)
h2o.rm(localH2O, grep(pattern = "GBM", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train_auto", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(gbmautomodel1,test_predgbmauto)

cat("\nTesting data: autoencoder and GBM 2")
cat("\nautoencoder 2:")
automodel2 <- h2o.loadModel(localH2O, paste0(path,"/automodel2count",day))
train_auto <- h2o.deepfeatures(train[,names], automodel2, layer=-1)
train_auto <- h2o.assign(train_auto, "train_auto")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train.hex", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "DeepFeatures", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(automodel2)
cat("\nGBM:")
gbmautomodel2 <- h2o.loadModel(localH2O, paste0(path,"/autogbmmodel2count",day))
test_predgbmauto <- h2o.predict(gbmautomodel2, train_auto)[,3]
colnames(test_predgbmauto) <- "gbmauto"
submission[,2] <- as.data.frame(test_predgbmauto)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste0(path,"/autogbmmodel2count",day,"_",testday,".csv"), quote = F, row.names = F)
h2o.rm(localH2O, grep(pattern = "GBM", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train_auto", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(gbmautomodel2,test_predgbmauto)
