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
arg <- commandArgs(trailingOnly = TRUE)
arg <- as.numeric(arg)
m <- arg[1]
l <- arg[2]
# day <- commandArgs(trailingOnly = TRUE)
# day <- as.numeric(day)
day <- 27
path <- "/home/whale/Documents/click"

# models ------------------------------------------------------------------

train_train <- h2o.getFrame(localH2O, "train_train")
train_valid <- h2o.getFrame(localH2O, "train_valid")
train_test <- h2o.getFrame(localH2O, "train_test")

#train_train <- train_train[,-match(c("C1","C15","C16","C18","device_type"),colnames(train_train))]
#train_valid <- train_valid[,-match(c("C1","C15","C16","C18","device_type"),colnames(train_valid))]

h2o.logLoss <- function(preds, resp) {
    tpc <- preds
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc > 1e-15, tpc, 1e-15))
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc < 1-1e-15, tpc, 1-1e-15))
    LL <- h2o.exec(localH2O,expr=mean(-resp*log(tpc)-(1-resp)*log(1-tpc)))
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    LL
}

non_cols <- c("id","click")
names <- colnames(train_train)
names <- names[! names %in% non_cols]

#data=train_train, validation=train_valid,
# model: MLP
cat("\nDeep learning model 1.")
dlmodel1 = h2o.deeplearning(data=train_train, validation=train_valid, x=names, y='click',
                            hidden=c(200,100,50), activation ="Rectifier", 
                            train_samples_per_iteration=-2, score_validation_samples=10000, 
                            score_validation_sampling='Stratified',
                            l1=1e-5, l2=1e-5, epsilon=1e-10,
                            input_dropout_ratio = 0.1, hidden_dropout_ratios = c(0.2,0.2,0.2),
                            balance_classes=TRUE, epochs = 20, seed=myseed)
                            #variable_importances=TRUE)

train_resp <- train_train[,'click'] #actual label
train_preds <- h2o.predict(dlmodel1, train_train)[,3] #[,3] is probability for class 1
logloss1 <- h2o.logLoss(train_preds, train_resp)
cat("\nLogLoss on training data:", logloss1)

valid_resp <- train_valid[,'click'] #actual label
valid_preds <- h2o.predict(dlmodel1, train_valid)[,3] #[,3] is probability for class 1
logloss2 <- h2o.logLoss(valid_preds, valid_resp)
cat("\nLogLoss on validation data:", logloss2)

test_resp <- train_test[,'click'] #actual label
test_preds <- h2o.predict(dlmodel1, train_test)[,3] #[,3] is probability for class 1
logloss3 <- h2o.logLoss(test_preds, test_resp)
cat("\nLogLoss on test data:", logloss3)

dllogloss <- c(logloss1,logloss2,logloss3)
#save(dllogloss,file=paste0("dllogloss_",m,"_",l,".Rda"))

# path_model <- paste0(path,"/dlmodel1count",day)
# h2o.saveModel(object = dlmodel1, name = path_model, force = TRUE)
# h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))
# 
# cat("\nDeep learning model 2.")
# dlmodel2 = h2o.deeplearning(data=train_train, validation=train_valid, x=names, y='click',
#                             hidden=c(95,325,130,60), activation ="Rectifier", 
#                             train_samples_per_iteration=-2, score_validation_samples=10000,
#                             score_validation_sampling='Stratified',
#                             l1=1e-5, l2=1e-5, epsilon=1e-10,
#                             input_dropout_ratio = 0.1, hidden_dropout_ratios = c(0.2,0.2,0.2,0.2),
#                             balance_classes=TRUE, epochs = 20, seed=myseed)
#
# path_model <- paste0(path,"/dlmodel2count",day)
# h2o.saveModel(object = dlmodel2, name = path_model, force = TRUE)
# h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))

# model: GBM
cat("\nGradiant boosting machine model.")
gbmmodel <- h2o.gbm(data=train_train, validation=train_valid, x=names, y='click',
                    n.tree=100, interaction.depth=10, n.minobsinnode = 40, 
                    shrinkage = 0.4, n.bins = 20, balance.classes = TRUE,
                    importance = TRUE)

train_resp <- train_train[,'click'] #actual label
train_preds <- h2o.predict(gbmmodel, train_train)[,3] #[,3] is probability for class 1
logloss1 <- h2o.logLoss(train_preds, train_resp)
cat("\nLogLoss on training data:", logloss1)

valid_resp <- train_valid[,'click'] #actual label
valid_preds <- h2o.predict(gbmmodel, train_valid)[,3] #[,3] is probability for class 1
logloss2 <- h2o.logLoss(valid_preds, valid_resp)
cat("\nLogLoss on validation data:", logloss2)

test_resp <- train_test[,'click'] #actual label
test_preds <- h2o.predict(gbmmodel, train_test)[,3] #[,3] is probability for class 1
logloss3 <- h2o.logLoss(test_preds, test_resp)
cat("\nLogLoss on test data:", logloss3)

gbmlogloss <- c(logloss1,logloss2,logloss3)
#save(gbmlogloss,file=paste0("gbmlogloss_",m,"_",l,".Rda"))

# path_model <- paste0(path,"/gbmmodelcount",day)
# h2o.saveModel(object = gbmmodel, name = path_model, force = TRUE)
# h2o.rm(localH2O, grep(pattern = "GBM", x = h2o.ls(localH2O)$Key, value = TRUE))

# glmmodel <- h2o.glm(data=train_train, x=names, y='click',
#                     family="binomial",
#                     alpha = 0.5, 
#                     #lambda_search = TRUE, 
#                     #nlambda = 30, 
#                     lambda = 1.3E-5,
#                     #has_intercept = FALSE,lambda = 2.17E-3,
#                     #lambda.min.ratio = ,  
#                     standardize = FALSE, variable_importances = TRUE,
#                     iter.max = 500)
