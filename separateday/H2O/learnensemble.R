# initialization ----------------------------------------------------------

library(h2o)
# java -Xmx16G -jar h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision
localH2O <- h2o.init(ip = 'localhost', port = 54321)

myseed=1988
testday <- 30
path <- "/home/whale/Documents/click"

#submission <- read.csv(paste0(path,"/submission",testday,".csv"),colClasses = c("character"))
submission <- read.csv(paste0(path,"/submission",testday,".csv"))
numrow <- nrow(submission)

data <- as.data.frame(matrix(0, ncol = 28, nrow = numrow))

data[,1] <- as.numeric(submission[,2])
daylist <- c(21,22,23,24,25,26,27,28,29)
col <- 1
filelist <- c("/dlmodel1count","/dlmodel2count","/gbmmodelcount")
#,"/autogbmmodel1count","/autogbmmodel2count"
for (day in daylist) {
    for (file in filelist) {
        col <- col+1
        readdata <- read.csv(paste0(path,file,day,"_",testday,".csv"))
        data[,col] <- readdata[,2]
    }  
}

data <- as.h2o(localH2O, data)
data <- h2o.assign(data, "data")

h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))

# splite ------------------------------------------------------------------

cat("\nSplitting into train/validation")
# Random split
rand <- h2o.runif(data, seed = myseed)
rand <- h2o.assign(rand, key = "rand")
train_train <- data[rand <= 0.8, ]
train_train <- h2o.assign(train_train, key = "train_train")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
train_valid <- data[rand > 0.8, ]
train_valid <- h2o.assign(train_valid, key = "train_valid")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "rand", x = h2o.ls(localH2O)$Key, value = TRUE))

# testing -----------------------------------------------------------------

cat("\nTesting data: deep learning ")
dlmodel <- h2o.loadModel(localH2O, paste0(path,"/dlmodelcountfinal"))
#dlmodel <- h2o.loadModel(localH2O, paste0(path,"/dlmodel1countfinal"))
test_preddl <- h2o.predict(dlmodel, data)[,3]
submission[,2] <- as.data.frame(test_preddl)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste0(path,"/dlmodelcountfinal",testday,".csv"), quote = F, row.names = F)
#write.csv(as.data.frame(submission), file = paste0(path,"/dlmodel1countfinal",testday,".csv"), quote = F, row.names = F)
h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(dlmodel,test_preddl)

cat("\nTesting data: GBM")
gbmmodel <- h2o.loadModel(localH2O, paste0(path,"/gbmmodelcountfinal"))
test_predgbm <- h2o.predict(gbmmodel, data)[,3]
submission[,2] <- as.data.frame(test_predgbm)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste0(path,"/gbmmodelcountfinal",testday,".csv"), quote = F, row.names = F)
h2o.rm(localH2O, grep(pattern = "GBM", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(gbmmodel,test_predgbm)

#cat("\nTesting data: RF")
#rfmodel <- h2o.loadModel(localH2O, paste0(path,"/rfmodelcountfinal"))
#test_predrf <- h2o.predict(rfmodel, data)[,3]
#submission[,2] <- as.data.frame(test_predrf)
#colnames(submission) <- c("id", "click")
#cat("\nWriting predictions on test data.")
#write.csv(as.data.frame(submission), file = paste0(path,"/rfmodelcountfinal",testday,".csv"), quote = F, row.names = F)
#h2o.rm(localH2O, grep(pattern = " SpeeDRF", x = h2o.ls(localH2O)$Key, value = TRUE))
#remove(rfmodel,test_predrf)

cat("\nTesting data: GLM")
glmmodel <- h2o.loadModel(localH2O, paste0(path,"/glmmodelcountfinal"))
test_predglm <- h2o.predict(glmmodel, data)[,3]
submission[,2] <- as.data.frame(test_predglm)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste0(path,"/glmmodelcountfinal",testday,".csv"), quote = F, row.names = F)
h2o.rm(localH2O, grep(pattern = "GLM", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(glmmodel,test_predglm)

# average -----------------------------------------------------------------

average <- (data[,1]+data[,6]+data[,11]+data[,16]+data[,21]+data[,26]+data[,31]+data[,36]+data[,41])/9
average <- (data[,2]+data[,7]+data[,12]+data[,17]+data[,22]+data[,27]+data[,32]+data[,37]+data[,42])/9
average <- (data[,1]+data[,6]+data[,11]+data[,16]+data[,21]+data[,26]+data[,31]+data[,36]+data[,41]
            +data[,2]+data[,7]+data[,12]+data[,17]+data[,22]+data[,27]+data[,32]+data[,37]+data[,42])/18
submission[,2] <- as.data.frame(average)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste(path,"/dlmodel1count212223242526272829.csv", sep = ''), quote = F, row.names = F)
write.csv(as.data.frame(submission), file = paste(path,"/dlmodel2count212223242526272829.csv", sep = ''), quote = F, row.names = F)
write.csv(as.data.frame(submission), file = paste(path,"/dlmodel1_2count212223242526272829.csv", sep = ''), quote = F, row.names = F)

# other
readsub1 <- read.csv(paste0(path,"/dlmodelcountfinal",testday,".csv"))
readsub3 <- read.csv(paste0(path,"/gbmmodelcountfinal",testday,".csv"))
readsub4 <- read.csv(paste0(path,"/glmmodelcountfinal",testday,".csv"))
#readsub5 <- read.csv(paste0(path,"/dlmodel1_2count212223242526272829.csv"))

average <- (readsub1[,2]+readsub3[,2]+readsub4[,2])/3
submission[,2] <- as.data.frame(average)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste(path,"/allaverage.csv", sep = ''), quote = F, row.names = F)

#all average
average <- data[,1]
for (i in (2:45)) {
    average <- average + data[,i]
}
average <- average/45
submission[,2] <- as.data.frame(average)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste(path,"/average.csv", sep = ''), quote = F, row.names = F)
