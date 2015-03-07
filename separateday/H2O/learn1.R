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
#day <- commandArgs(trailingOnly = TRUE)
#day <- as.numeric(day)
day <- 28
path <- "/home/whale/Documents/click"

# models ------------------------------------------------------------------

train_train <- h2o.getFrame(localH2O, "train_train")
train_valid <- h2o.getFrame(localH2O, "train_valid")

non_cols <- c("id","click")
names <- colnames(train_train)
names <- names[! names %in% non_cols]

#cat("\nTraining H2O model on training/validation splits.")
## Note: This could be grid search models, after which you would obtain the best model with model <- cvmodel@model[[1]]
#hidden_layers = list(c(200,200), c(100,300,100),c(500,500,500))
#cvmodel <- h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
#                            hidden=c(200,50), activation = "Rectifier", max_categorical_features=100000, 
#                            train_samples_per_iteration=16384, score_validation_samples=100000, 
#                            l1=1e-5, l2=1e-4, input_dropout_ratio = 0.2, 
#                            balance_classes=TRUE, epochs = 12, seed=myseed)

# model: MLP
cat("\nDeep learning model 1.")
dlmodel1 = h2o.deeplearning(data=train_train, validation=train_valid, x=names, y='click',
                            hidden=c(200,100,50), activation ="Rectifier", 
                            train_samples_per_iteration=-2, score_validation_samples=10000, 
                            score_validation_sampling='Stratified',
                            l1=1e-5, l2=1e-5, epsilon=1e-10,
                            input_dropout_ratio = 0.1, hidden_dropout_ratios = c(0.2,0.2,0.2),
                            balance_classes=TRUE, epochs = 10, seed=myseed)

path_model <- paste0(path,"/dlmodel1count",day)
h2o.saveModel(object = dlmodel1, name = path_model, force = TRUE)
h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))

cat("\nDeep learning model 2.")
dlmodel2 = h2o.deeplearning(data=train_train, validation=train_valid, x=names, y='click',
                            hidden=c(95,325,130,60), activation ="Rectifier", 
                            train_samples_per_iteration=-2, score_validation_samples=10000,
                            score_validation_sampling='Stratified',
                            l1=1e-5, l2=1e-5, epsilon=1e-10,
                            input_dropout_ratio = 0.1, hidden_dropout_ratios = c(0.2,0.2,0.2,0.2),
                            balance_classes=TRUE, epochs = 10, seed=myseed)

path_model <- paste0(path,"/dlmodel2count",day)
h2o.saveModel(object = dlmodel2, name = path_model, force = TRUE)
h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))

# model: GBM
cat("\nGradiant boosting machine model.")
gbmmodel <- h2o.gbm(data=train_train, validation=train_valid, x=names, y='click',
                     n.tree=400, interaction.depth=10, n.minobsinnode = 40, 
                     shrinkage = 0.1, n.bins = 20, balance.classes = TRUE,
                     importance = TRUE)

path_model <- paste0(path,"/gbmmodelcount",day)
h2o.saveModel(object = gbmmodel, name = path_model, force = TRUE)
h2o.rm(localH2O, grep(pattern = "GBM", x = h2o.ls(localH2O)$Key, value = TRUE))

# model: AutoEncoder
cat("\nAutoencoder model 1.")
automodel1 = h2o.deeplearning(data=train_train, x=names, y='click',
                             activation = "Tanh", hidden = c(200,300,150,20),
                             classification=F, autoencoder=T,
                             train_samples_per_iteration=262144, score_training_samples=10000, 
                             score_validation_sampling='Stratified',
                             l1=1e-5, l2=1e-5, epsilon=1e-10,
                             balance_classes=TRUE, epochs=1, seed=myseed)

path_model <- paste0(path,"/automodel1count",day)
h2o.saveModel(object = automodel1, name = path_model, force = TRUE)

train_auto <- h2o.deepfeatures(train_train[,-match('click',colnames(train_train))], automodel1, layer=-1)
train_auto <- cbind(train_train[,"click"], train_auto)
train_auto <- h2o.assign(train_auto, "train_auto")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "DeepFeatures", x = h2o.ls(localH2O)$Key, value = TRUE))

valid_auto <- h2o.deepfeatures(train_valid[,-match('click',colnames(train_valid))], automodel1, layer=-1)
valid_auto <- cbind(train_valid[,"click"], valid_auto)
valid_auto <- h2o.assign(valid_auto, "valid_auto")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "DeepFeatures", x = h2o.ls(localH2O)$Key, value = TRUE))

h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))

autonon_cols <- c('click')
autonames <- colnames(train_auto)
autonames <- autonames[! autonames %in% autonon_cols]

# AutoEncoder with GBM
cat("\nGBM in Autoencoder model 1.")
autogbmmodel1 <- h2o.gbm(data=train_auto, validation=valid_auto, x=autonames, y='click',
                        n.tree=50, interaction.depth=10, n.minobsinnode = 40, 
                        shrinkage = 0.2, n.bins = 20, balance.classes = TRUE, 
                        importance = TRUE)

path_model <- paste0(path,"/autogbmmodel1count",day)
h2o.saveModel(object = autogbmmodel1, name = path_model, force = TRUE)
h2o.rm(localH2O, grep(pattern = "GBM", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train_auto", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "valid_auto", x = h2o.ls(localH2O)$Key, value = TRUE))

cat("\nAutoencoder model 2.")
automodel2 = h2o.deeplearning(data=train_train, x=names, y='click',
                             activation = "Tanh", hidden = c(125,335,110),
                             classification=F, autoencoder=T,
                             train_samples_per_iteration=262144, score_training_samples=10000, 
                             score_validation_sampling='Stratified',
                             l1=1e-5, l2=1e-5, epsilon=1e-10,
                             balance_classes=TRUE, epochs=10, seed=myseed)

path_model <- paste0(path,"/automodel2count",day)
h2o.saveModel(object = automodel2, name = path_model, force = TRUE)

train_auto <- h2o.deepfeatures(train_train, automodel2, layer=-1)
train_auto <- cbind(train_train[,"click"], train_auto)
train_auto <- h2o.assign(train_auto, "train_auto")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train_train", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "DeepFeatures", x = h2o.ls(localH2O)$Key, value = TRUE))

valid_auto <- h2o.deepfeatures(train_valid, automodel2, layer=-1)
valid_auto <- cbind(train_valid[,"click"], valid_auto)
valid_auto <- h2o.assign(valid_auto, "valid_auto")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train_valid", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "DeepFeatures", x = h2o.ls(localH2O)$Key, value = TRUE))

h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))

autonon_cols <- c('click')
autonames <- colnames(train_auto)
autonames <- autonames[! autonames %in% autonon_cols]

# AutoEncoder with GBM
autogbmmodel2 <- h2o.gbm(data=train_auto, validation=valid_auto, x=c(2:ncol(train_auto)), y=1,
                        n.tree=50, interaction.depth=10, n.minobsinnode = 40, 
                        shrinkage = 0.2, n.bins = 20, balance.classes = TRUE)

path_model <- paste0(path,"/autogbmmodel2count",day)
h2o.saveModel(object = autogbmmodel2, name = path_model, force = TRUE)
h2o.rm(localH2O, grep(pattern = "GBM", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train_auto", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "valid_auto", x = h2o.ls(localH2O)$Key, value = TRUE))
