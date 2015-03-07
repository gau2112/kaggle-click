# logloss -----------------------------------------------------------------

library(h2o)
# java -Xmx16G -jar h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision
localH2O <- h2o.init(ip = 'localhost', port = 54321)

h2o.logLoss <- function(preds, resp) {
    tpc <- preds
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc > 1e-15, tpc, 1e-15))
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc < 1-1e-15, tpc, 1-1e-15))
    LL <- h2o.exec(localH2O,expr=mean(-resp*log(tpc)-(1-resp)*log(1-tpc)))
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    LL
}

logloss <- c()
for (i in (2:10)) {
    logloss[i-1] <- h2o.logLoss(data[,i], data[,1])
}
logloss <- matrix(logloss, ncol = 3, nrow = 3, byrow=FALSE)
logloss

rand <- h2o.runif(data, seed = myseed)
datapart <- data[rand > 0.8, ]
datapart <- h2o.assign(datapart, key = "datapart")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))

logloss1 <- c()
for (i in (2:46)) {
    logloss1[i-1] <- h2o.logLoss(datapart[,i], datapart[,1])
}
logloss1 <- matrix(logloss1, ncol = 9, nrow = 5, byrow=FALSE)

average <- data[,6]+data[,11]+data[,16]+data[,21]+data[,26]+data[,31]+data[,36]+data[,41]+data[,46]
average <- average/9
aveloss <- h2o.logLoss(average, data[,1])

#31 day
submission <- read.csv(paste0(path,"/submission_31.csv"),colClasses = c("character"))
numrow <- nrow(submission)
readdata <- as.data.frame(matrix(0, ncol = 18, nrow = numrow))
col <- 9
for (day in daylist){
    col <- col+1
    readdata[,col] <- read.csv(paste0(path,"/autogbmmodel1count",day,"_31.csv"))[,2]
}

average <- readdata[,1]+readdata[,2]+readdata[,3]+readdata[,4]+readdata[,5]
+readdata[,6]+readdata[,7]+readdata[,8]+readdata[,9]
average1 <- readdata[,10]+readdata[,11]+readdata[,12]+readdata[,13]+readdata[,14]
+readdata[,15]+readdata[,16]+readdata[,17]+readdata[,18]
average <- average / 9
average1 <- average1 / 9
all <- (average+average1)/2
submission[,2] <- as.data.frame(all)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste0(path,"/autogbmallaverage31.csv"), quote = F, row.names = F)

h2o.logLoss <- function(preds, resp) {
    tpc <- preds
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc > 1e-15, tpc, 1e-15))
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc < 1-1e-15, tpc, 1-1e-15))
    LL <- h2o.exec(localH2O,expr=mean(-resp*log(tpc)-(1-resp)*log(1-tpc)))
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    LL
}

LogLoss<-function(actual, predicted)
{
    predicted<-(pmax(predicted, 1e-15))
    predicted<-(pmin(predicted, 1-1e-15))
    result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
    return(result)
}

train_resp <- train_train[,'click'] #actual label
train_resp <- h2o.assign(train_resp, "train_resp")
#dlmodel <- h2o.getModel(localH2O, 'DeepLearning_9d9203349524ce3c946ece580204a12')
train_preds <- h2o.predict(gbmmodel, train_train)[,3] #[,3] is probability for class 1
train_preds <- h2o.assign(train_preds, "train_preds")
cat("\nLogLoss on training data:", h2o.logLoss(train_preds, train_resp))

valid_resp <- train_valid[,'click'] #actual label
valid_resp <- h2o.assign(valid_resp, "valid_resp")
#dlmodel <- h2o.getModel(localH2O, 'DeepLearning_9d9203349524ce3c946ece580204a12')
valid_preds <- h2o.predict(gbmmodel, train_valid)[,3] #[,3] is probability for class 1
valid_preds <- h2o.assign(valid_preds, "valid_preds")
cat("\nLogLoss on validation data:", h2o.logLoss(valid_preds, valid_resp))
