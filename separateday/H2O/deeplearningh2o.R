sink("CTR.log", split = T)

# The following two commands remove any previously installed H2O packages for R.
#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
#install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1601/R", getOption("repos"))))

library(h2o)
#java -Xmx16G -jar h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6
localH2O <- h2o.init(ip = 'localhost', port = 54321)

# data preprocessing ------------------------------------------------------------

### START
myseed=1988
#day <- 29
path <- "/home/whale/Documents/click"

#path_submission <- paste0(path,"/sampleSubmission.csv")
#21-30
#path_train <- paste0(path,"/train_day",day,".csv")
path_train <- paste0(path,"/test.csv")
#path_valid <- paste0(path, "/train_day22.csv")
#path_test <- paste0(path,"/test.csv")

cat("\nReading data.")
train_hex <- h2o.importFile(localH2O, path = path_train)
#valid_hex <- h2o.importFile(localH2O, path = path_valid)
#test_hex <- h2o.importFile(localH2O, path = path_test)

target_table <- h2o.table(train_hex[,"click"])

## Feature engineering

## all columns
#intcols <- c("C1","banner_pos","device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")
#factorcols <- c("site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model")
intcols_smallcates <- c("C1","banner_pos","device_type","device_conn_type","C15","C16","C18","C19","C21")
intcols_largecates <- c("C14","C17","C20")
factorcols_smallcates <- c("site_category","app_category",)
factorcols_largecates <- c("site_id","site_domain","app_id","app_domain","device_id","device_ip","device_model")
allcols <- c("C1","banner_pos","site_id","site_domain","site_category","app_id",
             "app_domain","app_category","device_id","device_ip","device_model",
             "device_type","device_conn_type","C14","C15","C16","C17","C18",
             "C19","C20","C21")

cat("\nAdding time related features")
h2o.addTimeFeatures <- function(frame, timecol, key) {
    cat("\nFeature engineering for time column.")
    hour <- frame[,timecol] %% 100
    colnames(hour) <- "hour"
    day <- ((frame[,timecol] - hour) %% 10000)/100
    colnames(day) <- "day"
    dow <- day %% 7
    colnames(dow) <- "dayofweek"
    frame <- cbind(frame[,-match(timecol,colnames(frame))], day, dow, hour, as.factor(day), as.factor(dow), as.factor(hour))
    
    # Store frame under designated key
    frame <- h2o.assign(frame, key)
    
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    frame
}
train <- h2o.addTimeFeatures(train_hex, "hour", "train.hex")
#valid <- h2o.addTimeFeatures(valid_hex, "hour", "valid.hex")
#test <- h2o.addTimeFeatures(test_hex, "hour", "test.hex")

cat("\nFeature engineering for int columns with small levels (~< 100).")
h2o.int2factor <- function(frame, intcols) {
    cat("\ninteger columns to factor.")
    for (int in intcols) {
        frame[[int]] <- as.factor(frame[[int]])
    }
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
}
h2o.int2factor(train, intcols_smallcates)
#h2o.int2factor(valid, intcols_smallcates)
#h2o.int2factor(test, intcols_smallcates)

cat("\nFeature engineering for columns with large levels (~> 100).")
# turn integers into factors, keep top n levels
h2o.int2factorTrim <- function(frame, col, max_factors, min_occurrence=1) {
    frame[[col]] <- h2o.interaction(as.factor(frame[,col]), factors = 1, pairwise = FALSE, max_factors = max_factors, min_occurrence = min_occurrence)
    
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
}

h2o.int2factorTrim(train, col='C14', max_factors=200, min_occurrence=1)
#h2o.int2factorTrim(valid, col='C14', max_factors=200, min_occurrence=1)
#h2o.int2factorTrim(test, col='C14', max_factors=200, min_occurrence=1)

h2o.int2factorTrim(train, col='C17', max_factors=20, min_occurrence=1)
#h2o.int2factorTrim(valid, col='C17', max_factors=20, min_occurrence=1)
#h2o.int2factorTrim(test, col='C17', max_factors=20, min_occurrence=1)

h2o.int2factorTrim(train, col='C20', max_factors=10, min_occurrence=1)
#h2o.int2factorTrim(valid, col='C20', max_factors=10, min_occurrence=1)
#h2o.int2factorTrim(test, col='C20', max_factors=10, min_occurrence=1)

# keep top n levels
h2o.factorTrim <- function(frame, col, max_factors, min_occurrence=1) {
    frame[[col]] <- h2o.interaction(frame[,col], factors = 1, pairwise = FALSE, max_factors = max_factors, min_occurrence = min_occurrence)
    
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
}

h2o.factorTrim(train, col='site_id', max_factors=300, min_occurrence=1)
#h2o.factorTrim(valid, col='site_id', max_factors=300, min_occurrence=1)
#h2o.factorTrim(test, col='site_id', max_factors=300, min_occurrence=1)

h2o.factorTrim(train, col='site_domain', max_factors=300, min_occurrence=1)
#h2o.factorTrim(valid, col='site_domain', max_factors=300, min_occurrence=1)
#h2o.factorTrim(test, col='site_domain', max_factors=300, min_occurrence=1)

h2o.factorTrim(train, col='app_id', max_factors=300, min_occurrence=1)
#h2o.factorTrim(valid, col='app_id', max_factors=300, min_occurrence=1)
#h2o.factorTrim(test, col='app_id', max_factors=300, min_occurrence=1)

h2o.factorTrim(train, col='app_domain', max_factors=50, min_occurrence=1)
#h2o.factorTrim(valid, col='app_domain', max_factors=50, min_occurrence=1)
#h2o.factorTrim(test, col='app_domain', max_factors=50, min_occurrence=1)

h2o.factorTrim(train, col='device_id', max_factors=2000, min_occurrence=1)
#h2o.factorTrim(valid, col='device_id', max_factors=2000, min_occurrence=1)
#h2o.factorTrim(test, col='device_id', max_factors=2000, min_occurrence=1)

h2o.factorTrim(train, col='device_ip', max_factors=2000, min_occurrence=1)
#h2o.factorTrim(valid, col='device_ip', max_factors=2000, min_occurrence=1)
#h2o.factorTrim(test, col='device_ip', max_factors=2000, min_occurrence=1)

h2o.factorTrim(train, col='device_model', max_factors=300, min_occurrence=1)
#h2o.factorTrim(valid, col='device_model', max_factors=300, min_occurrence=1)
#h2o.factorTrim(test, col='device_model', max_factors=300, min_occurrence=1)

cat("\nFeature engineering for column interactions.")
h2o.addInteractionFeatures <- function(frame, key) {
    cat("\nFeature Interaction...")
    #allcols <- c("C1","banner_pos","site_id","site_domain","site_category",
    #"app_id","app_domain","app_category","device_id","device_ip","device_model",
    #"device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")
    #newfactors <- c()
    # create pair-wise interaction between factors, keep top 100 levels
    factor_interactions <- h2o.interaction(frame, factors = allcols, pairwise = TRUE, max_factors = 100, min_occurrence = 2)
    factor_list <- list(c("site_id","site_domain","site_category"),c("app_id","app_domain","app_category"),
                        c("device_id","device_ip","device_model","device_type","device_conn_type"))
    factor_combines <- h2o.interaction(frame, factors = factor_list, pairwise = FALSE, max_factors = 100, min_occurrence = 2)
    frame <- cbind(frame, factor_interactions, factor_combines)
    
    # Store frame under designated key
    frame <- h2o.assign(frame, key)
    
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    
    frame
}
train <- h2o.addInteractionFeatures(train, "train.hex")
#valid <- h2o.addInteractionFeatures(valid, "valid.hex")
#test <- h2o.addInteractionFeatures(test, "test.hex")
h2o.rm(localH2O, grep(pattern = "train.hex.interaction.", x = h2o.ls(localH2O)$Key, value = TRUE))
#h2o.rm(localH2O, grep(pattern = "valid.hex.interaction.", x = h2o.ls(localH2O)$Key, value = TRUE))
#h2o.rm(localH2O, grep(pattern = "test.hex.interaction.", x = h2o.ls(localH2O)$Key, value = TRUE))
#remove(test)

cat("\nSplitting into train/validation")
# Random split
rand <- h2o.runif(train, seed = myseed)
train_train <- train[rand <= 0.8, ]
train_train <- h2o.assign(train_train, key = "train_train")
train_valid <- train[rand > 0.8, ]
train_valid <- h2o.assign(train_valid, key = "train_valid")

## Split into train/validation based on training days (first 9 days: train, last day: test)
#train <- h2o.assign(train_hex[train_hex$day<30,], 'train')
#valid <- h2o.assign(train_hex[train_hex$day==30,], 'valid')

h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))

# models ------------------------------------------------------------------
h2o.rm(localH2O, grep(pattern = "train_day", x = h2o.ls(localH2O)$Key, value = TRUE))

cat("\nTraining H2O model on training/validation splits days <30/30")
## Note: This could be grid search models, after which you would obtain the best model with model <- cvmodel@model[[1]]
#hidden_layers = list(c(200,200), c(100,300,100),c(500,500,500))
#cvmodel <- h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
#                            hidden=c(200,50), activation = "Rectifier", max_categorical_features=100000, 
#                            train_samples_per_iteration=-2, score_validation_samples=100000, 
#                            l1=1e-5, l2=1e-4, input_dropout_ratio = 0.2, 
#                            balance_classes=TRUE, epochs = 12, seed=myseed)

# model: MLP
dlmodel = h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train_train)), y=2,
                           hidden=c(200,50), activation ="Rectifier", 
                           max_categorical_features=100000, 
                           train_samples_per_iteration=16384, score_validation_samples=100000, 
                           l1=1e-5, l2=1e-5, input_dropout_ratio = 0.2, epsilon=1e-10,
                           balance_classes=TRUE, epochs = 8, seed=myseed)

path_model <- paste0(path,"/dlmodel",day)
h2o.saveModel(object = dlmodel, name = path_model, force = TRUE)

# model: GBM
gbmmodel <- h2o.gbm(data=train_train, validation=train_valid, x=c(3:ncol(train_train)), y=2,
                    n.tree=400, interaction.depth=10, n.minobsinnode = 40, 
                    shrinkage = 0.1, n.bins = 10, balance.classes = TRUE)

path_model <- paste0(path,"/gbmmodel",day)
h2o.saveModel(object = gbmmodel, name = path_model, force = TRUE)

# model more --------------------------------------------------------------
# model: RF
# really a bad model
#rfmodel <- h2o.randomForest(data=train_train, validation=train_valid, x=c(3:ncol(train_train)), y=2,
#                            type="fast", sample.rate = 2/3, mtries = -1, ntree=1000, 
#                            depth=20, nbins = 10, stat.type = "ENTROPY",
#                            balance.classes = FALSE, seed=myseed)
#path_model <- paste0(path,"/rfmodel",day)
#h2o.saveModel(object = rfmodel, name = path_model, force = TRUE)

# model: AutoEncoder
automodel = h2o.deeplearning(data=train, x=c(3:ncol(train)), y=2,
                                 activation = "Tanh", hidden = c(200,100,50),
                                 classification=F, autoencoder=T,
                                 max_categorical_features=100000, 
                                 train_samples_per_iteration=16384, score_training_samples=100000, 
                                 l1=1e-5, l2=1e-5, input_dropout_ratio = 0.2, epsilon=1e-10,
                                 balance_classes=TRUE, epochs = 0.05, seed=myseed)

automodel1 = h2o.deeplearning(data=train, x=c(3:ncol(train)), y=2,
                             activation = "Tanh", hidden = c(1000,500,100),
                             classification=F, autoencoder=T,
                             max_categorical_features=100000, 
                             train_samples_per_iteration=-2, score_training_samples=100000, 
                             l1=1e-5, l2=1e-5, input_dropout_ratio = 0.2, epsilon=1e-10,
                             balance_classes=TRUE, epochs = 0.5, seed=myseed)

path_model <- paste0(path,"/automodel",day)
h2o.saveModel(object = automodel, name = path_model, force = TRUE)

#auto_data <- h2o.predict(automodel, train[1:33353])

auto_features <- h2o.deepfeatures(train, automodel, key="auto_feature.hex" layer=-1)

# Random split
rand <- h2o.runif(auto_features, seed = myseed)
train_auto <- auto_features[rand <= 0.8, ]
train_auto_target <- train[,2][rand <= 0.8, ]
valid_auto <- auto_features[rand > 0.8, ]
valid_auto_target <- train[,2][rand > 0.8, ]

train_auto <- cbind(train_auto_target, train_auto)
train_auto <- h2o.assign(train_auto, "train_auto.hex")
valid_auto <- cbind(valid_auto_target, valid_auto)
valid_auto <- h2o.assign(valid_auto, "valid_auto.hex")

h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))

# AutoEncoder with GBM
autogbmmodel <- h2o.gbm(data=train_auto, validation=valid_auto, x=c(2:ncol(train_auto)), y=1,
                   n.tree=50, interaction.depth=10, n.minobsinnode = 40, 
                   shrinkage = 0.1, n.bins = 10, balance.classes = FALSE)

path_model <- paste0(path,"/autogbmmodel",day)
h2o.saveModel(object = autogbmmodel, name = path_model, force = TRUE)

# AutoEncoder with Random Forests
autorfmodel <- h2o.randomForest(data=train_auto, validation=valid_auto, x=c(2:ncol(train_auto)), y=1,
                            type="fast", ntree=50, depth=20, mtries = -1, 
                            sample.rate = 2/3, nbins = 20, seed=myseed, 
                            balance.classes = FALSE, stat.type = "ENTROPY")

path_model <- paste0(path,"/autorfmodel",day)
h2o.saveModel(object = autorfmodel, name = path_model, force = TRUE)

# logloss -----------------------------------------------------------------

h2o.logLoss <- function(preds, resp) {
    tpc <- preds
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc > 1e-15, tpc, 1e-15))
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc < 1-1e-15, tpc, 1-1e-15))
    LL <- h2o.exec(localH2O,expr=mean(-resp*log(tpc)-(1-resp)*log(1-tpc)))
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    LL
}

train_resp <- train_auto[,1] #actual label
train_preds <- h2o.predict(autogbmmodel, train_auto)[,3] #[,3] is probability for class 1
cat("\nLogLoss on training data:", h2o.logLoss(train_preds, train_resp))

valid_resp <- train_valid[,2]
valid_preds <- h2o.predict(dlmodel1, train_valid)[,3]
cat("\nLogLoss on validation data:", h2o.logLoss(valid_preds, valid_resp))

h2o.rm(localH2O, grep(pattern = "DeepLearningPredict", x = h2o.ls(localH2O)$Key, value = TRUE))

# ensemble ----------------------------------------------------------------

path_model <- paste0(path,"/dlmodel29")
dlmodel21 <- h2o.loadModel(localH2O, path_model)
test_preds21 <- h2o.predict(dlmodel21, train)[,3]
colnames(test_preds21) <- "dl29"

gbmmodel21 <- h2o.loadModel(localH2O, paste0(path,"/gbmmodel29"))
test_preds21 <- h2o.predict(gbmmodel21, train)
test_preds21 <- test_preds21[,3]
colnames(test_preds21) <- "gbm29"

path_model <- paste0(path,"/dlmodel22")
dlmodel22 <- h2o.loadModel(localH2O, path_model)
test_preds22 <- h2o.predict(dlmodel22, train)[,3]
colnames(test_preds22) <- "dl22"

path_model <- paste0(path,"/dlmodel23")
dlmodel23 <- h2o.loadModel(localH2O, path_model)
test_preds23 <- h2o.predict(dlmodel23, train_valid)[,3]
colnames(test_preds23) <- "dl23"

test_resp <- train_valid[,2] #actual label
cat("\nLogLoss on training data:", h2o.logLoss(test_preds23, test_resp))

pred = cbind(test_preds21, test_preds22, test_preds23, train[["click"]])
pred <- h2o.assign(pred, "pred.hex")

h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))

glmmodel <- h2o.glm(x=c(1,2,3),
                    y=4,
                    data=train_train, 
                    validation=train_valid,
                    key = "glm_model",
                    family="binomial",
                    alpha = 0.5,
                    lambda_search = TRUE,
                    nlambda = 10,
                    #lambda.min.ratio = ,
                    #lambda = 1e-5,
                    standardize = FALSE,
                    use_all_factor_levels = TRUE,
                    variable_importances = TRUE)

# model average -----------------------------------------------------------
path_submission <- paste0(path,"/dlday21.csv")
dl21 <- read.csv(path_submission)
dl21 <- as.data.frame(dl21)

path_submission <- paste0(path,"/dlday22.csv")
dl22 <- read.csv(path_submission)
dl22 <- as.data.frame(dl22)

path_submission <- paste0(path,"/dlday23.csv")
dl23 <- read.csv(path_submission)
dl23 <- as.data.frame(dl23)

path_submission <- paste0(path,"/dlday24.csv")
dl24 <- read.csv(path_submission)
dl24 <- as.data.frame(dl24)

path_submission <- paste0(path,"/dlday25.csv")
dl25<- read.csv(path_submission)
dl25 <- as.data.frame(dl25)

path_submission <- paste0(path,"/dlday26.csv")
dl26<- read.csv(path_submission)
dl26 <- as.data.frame(dl26)

path_submission <- paste0(path,"/dlday27.csv")
dl27<- read.csv(path_submission)
dl27 <- as.data.frame(dl27)

path_submission <- paste0(path,"/dlday28.csv")
dl28<- read.csv(path_submission)
dl28 <- as.data.frame(dl28)

path_submission <- paste0(path,"/dlday29.csv")
dl29<- read.csv(path_submission)
dl29 <- as.data.frame(dl29)

average <- (dl21[,2]+dl22[,2]+dl23[,2]+dl24[,2]+dl25[,2]+dl27[,2]+dl28[,2])/7
submission[,2] <- as.data.frame(average)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste(path,"/dlday21222324252728.csv", sep = ''), quote = F, row.names = F)

# submission --------------------------------------------------------------
path_submission <- paste0(path,"/submission.csv")
submission <- read.csv(path_submission, colClasses = c("character"))
submission[,2] <- as.data.frame(test_preds21)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste(path,"/gbmday29.csv", sep = ''), quote = F, row.names = F)

# final model -------------------------------------------------------------

best_cvmodel.continue = h2o.deeplearning(checkpoint=best_cvmodel)

usefullmodel = T #Set to TRUE for higher accuracy 
if (usefullmodel) {
    cat("\nTraining H2O model on all the training data.")
    #   fullmodel <- h2o.randomForest(data=train_hex, x=c(3:ncol(train)), y=2,
    #                                 type=cvmodel@model$params$type,
    #                                 ntree=cvmodel@model$params$ntree,
    #                                 depth=cvmodel@model$params$depth,
    #                                 seed=cvmodel@model$params$seed)
    fullmodel <- h2o.gbm(data=train_hex, x=c(3:ncol(train)), y=2,
                         n.tree=cvmodel@model$params$n.tree,
                         interaction.depth=cvmodel@model$params$interaction.depth)
    #  fullmodel <- h2o.deeplearning(data=train_hex, x=c(3:ncol(train)), y=2,
    #                                hidden=cvmodel@model$params$hidden,
    #                                max_categorical_features=cvmodel@model$params$max_categorical_features,
    #                                train_samples_per_iteration=cvmodel@model$params$train_samples_per_iteration,
    #                                score_validation_samples=cvmodel@model$params$score_validation_samples)
    
    cat("\nMaking predictions on test data with model trained on full data.")
    pred <- h2o.predict(fullmodel, test_hex)[,3]
} else {
    cat("\nMaking predictions on test data with model trained on train/validation splits.")
    pred <- h2o.predict(cvmodel, test_hex)[,3]
}

submission <- read.csv(path_submission, colClasses = c("character"))
submission[,2] <- as.data.frame(pred)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste(path,"./submission.csv", sep = ''), quote = F, row.names = F)
sink()