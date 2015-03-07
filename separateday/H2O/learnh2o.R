sink("CTR.log", split = T)

# The following two commands remove any previously installed H2O packages for R.
#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
#install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1601/R", getOption("repos"))))

library(h2o)
#java -Xmx16G -jar h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6
localH2O <- h2o.init(ip = 'localhost', port = 54321)

################################################################################
# cleaning functions

rmLastValues <- function(pattern = "Last.value.") {
    keys <- h2o.ls(localH2O, pattern = pattern)$Key
    if (!is.null(keys))
        h2o.rm(localH2O, keys)
    invisible(keys)
}

## Clear H2O Cluster
library(stringr)
ls_temp <- h2o.ls(localH2O)
for (n_ls in 1:nrow(ls_temp)) {
    if (str_detect(ls_temp[n_ls, 1], "DeepLearning")) {
        h2o.rm(localH2O, keys = as.character(ls_temp[n_ls, 1]))
    } else if (str_detect(ls_temp[n_ls, 1], "GLM")) {
        h2o.rm(localH2O, keys = as.character(ls_temp[n_ls, 1]))
    } else if (str_detect(ls_temp[n_ls, 1], "GBM")) {
        h2o.rm(localH2O, keys = as.character(ls_temp[n_ls, 1]))
    } else if (str_detect(ls_temp[n_ls, 1], "Last.value")) {
        h2o.rm(localH2O, keys = as.character(ls_temp[n_ls, 1]))
    }
}
################################################################################

################################################################################
### START
myseed=1988
path <- "/home/whale/Documents/click"

path_submission <- paste0(path,"/sampleSubmission.csv")
path_train <- paste0(path,"/train_day21.csv")
#path_valid <- paste0(path, "/train_day22.csv")
path_test <- paste0(path,"/test.csv")

cat("\nReading data.")
train_hex <- h2o.importFile(localH2O, path = path_train)
#valid_hex <- h2o.importFile(localH2O, path = path_valid)
test_hex <- h2o.importFile(localH2O, path = path_test)

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
test <- h2o.addTimeFeatures(test_hex, "hour", "test.hex")

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
h2o.int2factor(test, intcols_smallcates)

cat("\nFeature engineering for columns with large levels (~> 100).")
# turn integers into factors, keep top n levels
h2o.int2factorTrim <- function(frame, col, max_factors, min_occurrence=1) {
    frame[[col]] <- h2o.interaction(as.factor(frame[,col]), factors = 1, pairwise = FALSE, max_factors = max_factors, min_occurrence = min_occurrence)
    
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
}

h2o.int2factorTrim(train, col='C14', max_factors=200, min_occurrence=1)
#h2o.int2factorTrim(valid, col='C14', max_factors=200, min_occurrence=1)
h2o.int2factorTrim(test, col='C14', max_factors=200, min_occurrence=1)

h2o.int2factorTrim(train, col='C17', max_factors=20, min_occurrence=1)
#h2o.int2factorTrim(valid, col='C17', max_factors=20, min_occurrence=1)
h2o.int2factorTrim(test, col='C17', max_factors=20, min_occurrence=1)

h2o.int2factorTrim(train, col='C20', max_factors=10, min_occurrence=1)
#h2o.int2factorTrim(valid, col='C20', max_factors=10, min_occurrence=1)
h2o.int2factorTrim(test, col='C20', max_factors=10, min_occurrence=1)

# keep top n levels
h2o.factorTrim <- function(frame, col, max_factors, min_occurrence=1) {
    frame[[col]] <- h2o.interaction(frame[,col], factors = 1, pairwise = FALSE, max_factors = max_factors, min_occurrence = min_occurrence)
    
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
}

h2o.factorTrim(train, col='site_id', max_factors=300, min_occurrence=1)
#h2o.factorTrim(valid, col='site_id', max_factors=300, min_occurrence=1)
h2o.factorTrim(test, col='site_id', max_factors=300, min_occurrence=1)

h2o.factorTrim(train, col='site_domain', max_factors=300, min_occurrence=1)
#h2o.factorTrim(valid, col='site_domain', max_factors=300, min_occurrence=1)
h2o.factorTrim(test, col='site_domain', max_factors=300, min_occurrence=1)

h2o.factorTrim(train, col='app_id', max_factors=300, min_occurrence=1)
#h2o.factorTrim(valid, col='app_id', max_factors=300, min_occurrence=1)
h2o.factorTrim(test, col='app_id', max_factors=300, min_occurrence=1)

h2o.factorTrim(train, col='app_domain', max_factors=50, min_occurrence=1)
#h2o.factorTrim(valid, col='app_domain', max_factors=50, min_occurrence=1)
h2o.factorTrim(test, col='app_domain', max_factors=50, min_occurrence=1)

h2o.factorTrim(train, col='device_id', max_factors=2000, min_occurrence=1)
#h2o.factorTrim(valid, col='device_id', max_factors=2000, min_occurrence=1)
h2o.factorTrim(test, col='device_id', max_factors=2000, min_occurrence=1)

h2o.factorTrim(train, col='device_ip', max_factors=2000, min_occurrence=1)
#h2o.factorTrim(valid, col='device_ip', max_factors=2000, min_occurrence=1)
h2o.factorTrim(test, col='device_ip', max_factors=2000, min_occurrence=1)

h2o.factorTrim(train, col='device_model', max_factors=300, min_occurrence=1)
#h2o.factorTrim(valid, col='device_model', max_factors=300, min_occurrence=1)
h2o.factorTrim(test, col='device_model', max_factors=300, min_occurrence=1)

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
test <- h2o.addInteractionFeatures(test, "test.hex")
h2o.rm(localH2O, grep(pattern = "train.hex.interaction.", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "valid.hex.interaction.", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "test.hex.interaction.", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(test)

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

h2o.logLoss <- function(preds, resp) {
    tpc <- preds
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc > 1e-15, tpc, 1e-15))
    tpc <- h2o.exec(localH2O,expr=ifelse(tpc < 1-1e-15, tpc, 1-1e-15))
    LL <- h2o.exec(localH2O,expr=mean(-resp*log(tpc)-(1-resp)*log(1-tpc)))
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    LL
}

cat("\nTraining H2O model on training/validation splits days <30/30")
## Note: This could be grid search models, after which you would obtain the best model with model <- cvmodel@model[[1]]
#cvmodel <- h2o.randomForest(data=train, validation=valid, x=c(3:ncol(train)), y=2,
#                            type="BigData", ntree=50, depth=20, seed=myseed)
#cvmodel <- h2o.gbm(data=train, validation=valid, x=c(3:ncol(train)), y=2,
#                   n.tree=100, interaction.depth=10)
#hidden_layers = list(c(200,200), c(100,300,100),c(500,500,500))
cvmodel <- h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
                            hidden=c(200,50), activation = "Rectifier", max_categorical_features=100000, 
                            train_samples_per_iteration=-2, score_validation_samples=100000, 
                            l1=1e-5, l2=1e-4, input_dropout_ratio = 0.2, 
                            balance_classes=TRUE, epochs = 1, seed=myseed)

train_resp <- train_train[,2] #actual label
train_preds <- h2o.predict(cvmodel, train_train)[,3] #[,3] is probability for class 1
cat("\nLogLoss on training data:", h2o.logLoss(train_preds, train_resp))

valid_resp <- train_valid[,2]
valid_preds <- h2o.predict(cvmodel, train_valid)[,3]
cat("\nLogLoss on validation data:", h2o.logLoss(valid_preds, valid_resp))

cvmodel.continue = h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
                                    hidden=c(200,50), activation = "Rectifier", max_categorical_features=100000, 
                                    train_samples_per_iteration=-2, score_validation_samples=100000, 
                                    l1=1e-5, l2=1e-4, input_dropout_ratio = 0.2, 
                                    balance_classes=TRUE, epochs = 1, seed=myseed, 
                                    checkpoint=cvmodel)

train_resp <- train_train[,2] #actual label
train_preds <- h2o.predict(cvmodel.continue, train_train)[,3] #[,3] is probability for class 1
cat("\nLogLoss on training data:", h2o.logLoss(train_preds, train_resp))

valid_resp <- train_valid[,2]
valid_preds <- h2o.predict(cvmodel.continue, train_valid)[,3]
cat("\nLogLoss on validation data:", h2o.logLoss(valid_preds, valid_resp))

cvmodel.continue1 = h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
                                    hidden=c(200,50), activation = "Rectifier", max_categorical_features=100000, 
                                    train_samples_per_iteration=-2, score_validation_samples=100000, 
                                    l1=1e-5, l2=1e-4, input_dropout_ratio = 0.2, 
                                    balance_classes=TRUE, epochs = 1, seed=myseed, 
                                    checkpoint=cvmodel.continue)

cvmodel.continue2 = h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
                                     hidden=c(200,50), activation = "Rectifier", max_categorical_features=100000, 
                                     train_samples_per_iteration=-2, score_validation_samples=100000, 
                                     l1=1e-5, l2=1e-4, input_dropout_ratio = 0.2, 
                                     balance_classes=TRUE, epochs = 6, seed=myseed, 
                                     checkpoint=cvmodel.continue1)

train_resp <- train_train[,2] #actual label
train_preds <- h2o.predict(cvmodel.continue2, train_train)[,3] #[,3] is probability for class 1
cat("\nLogLoss on training data:", h2o.logLoss(train_preds, train_resp))

valid_resp <- train_valid[,2]
valid_preds <- h2o.predict(cvmodel.continue2, train_valid)[,3]
cat("\nLogLoss on validation data:", h2o.logLoss(valid_preds, valid_resp))

cvmodel.continue3 = h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
                                     hidden=c(200,50), activation = "Rectifier", max_categorical_features=100000, 
                                     train_samples_per_iteration=-2, score_validation_samples=100000, 
                                     l1=1e-5, l2=1e-4, input_dropout_ratio = 0.2, 
                                     balance_classes=TRUE, epochs = 3, seed=myseed, 
                                     checkpoint=cvmodel.continue2)

h2o.rm(localH2O, grep(pattern = "DeepLearningPredict", x = h2o.ls(localH2O)$Key, value = TRUE))

# grid search
gsmodel = h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
                           hidden=list(c(200,50), c(200,200),c(200,50,50)), activation = c("Rectifier","MaxoutWithDropout"), 
                           max_categorical_features=100000, 
                           train_samples_per_iteration=-2, score_validation_samples=100000, 
                           l1=1e-5, l2=1e-5, input_dropout_ratio = 0.2, epsilon=c(1e-6,1e-4),
                           balance_classes=TRUE, epochs = 0.2, seed=myseed)
# smaller epsilon
epmodel = h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
                           hidden=c(200,50), activation ="Rectifier", 
                           max_categorical_features=100000, 
                           train_samples_per_iteration=-2, score_validation_samples=100000, 
                           l1=1e-5, l2=1e-5, input_dropout_ratio = 0.2, epsilon=1e-10,
                           balance_classes=TRUE, epochs = 9, seed=myseed)

path_model <- paste0(path,"/dlmodel21")
h2o.saveModel(object = epmodel, name = path_model, force = TRUE)
best_cvmodel = h2o.loadModel(localH2O, path_model)
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