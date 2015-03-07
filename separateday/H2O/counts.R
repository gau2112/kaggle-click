# initialization ----------------------------------------------------------
#sink("CTR1.log", split = T)

# The following two commands remove any previously installed H2O packages for R.
#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
#install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1601/R", getOption("repos"))))

library(h2o)
#library(plyr)
#java -Xmx16G -jar h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6
localH2O <- h2o.init(ip = 'localhost', port = 54321)
myseed=1988

# data preprocessing ------------------------------------------------------------
### START
day <- 28
path <- "/home/whale/Documents/click"

#path_submission <- paste0(path,"/sampleSubmission.csv")
#21-30
path_train <- paste0(path,"/train_day",day,".csv")
#path_train <- paste0(path,"/test.csv")
#path_valid <- paste0(path, "/train_day22.csv")
#path_test <- paste0(path,"/test.csv")

cat("\nReading data.")
train_hex <- h2o.importFile(localH2O, path = path_train)
#valid_hex <- h2o.importFile(localH2O, path = path_valid)
#test_hex <- h2o.importFile(localH2O, path = path_test)

## Feature engineering

## all columns
#intcols <- c("C1","banner_pos","device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")
#factorcols <- c("site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model")
intcols_smallcates <- c("C1","banner_pos","device_type","device_conn_type","C15","C16","C18","C19","C21")
intcols_largecates <- c("C14","C17","C20")
factorcols_smallcates <- c("site_category","app_category")
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
h2o.rm(localH2O, grep(pattern = "train_day", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(train_hex)

# likelihood --------------------------------------------------------------

#train <- h2o.getFrame(localH2O, "train.hex")

# global likelihood
table_target <- h2o.table(train[,"click"])
glh <- table_target[table_target[,1] == 1][,2]/sum(table_target[,"Count"])
glh <- sum(glh)
remove(table_target)

# columns to be transformed
non_counts <- c("id","click","day","dayofweek","hour","day_0","dayofweek_0","hour_0")
col_names <- colnames(train)
col_names <- col_names[! col_names %in% non_counts]
col_length <- length(col_names)

# values in likelihood calculating
m=1
l=1

# get tables of each feature
#tables <- list()

# 1:47, 48:94, 95:141, 142:188, 189:col_length
for (name in col_names) {
    table_name <- c(name,"click")
    tables <- h2o.table(train[,table_name])
    tables <- as.data.frame(tables)
    colnames(tables) <- c(name,"noclick","click")
    tables$count <- tables$noclick+tables$click
    #otherrow <- sum(which(tables[[name]][,col_names[7]]=="other"))
    #tables[[name]][otherrow,"count"] <- 1
    tables$clickrate <- tables$click/tables$count
    keeps <- c(name,"count","clickrate")
    tables <- tables[keeps]
    tables$likelihood <- (l/(tables$count+m))*glh+(1-l/(tables$count+m))*tables$clickrate
    
    # training data factor replacement
    train_col <- as.data.frame(train[,name])
    train_col[,1] <- as.factor(train_col[,1])
    for (i in (1:nrow(tables))) {
        ele <- tables[i,]
        level_pos <- match(as.character(ele[[name]]), levels(train_col[,1]))
        
        levels(train_col[,1])[level_pos] <- as.character(ele[["likelihood"]])
    }
    
    #levels(train_col[,1])[level_pos] <- "7801e8d9"
    #train_col[,1] <- as.numeric(as.character(train_col[,1]))
    train_col[,1] <- as.numeric(levels(train_col[,1]))[train_col[,1]]
    
    train_col <- as.h2o(localH2O, train_col)
    
    #train <- h2o.getFrame(localH2O, "train.hex")
    train[,name] <- train_col
    #train <- cbind(train[,-match(name,colnames(train))], train_col)
    #new_col <- grep(pattern="Last.value", x=colnames(train), value=FALSE)
    #colnames(train)[new_col] <- name
    # Store train under designated key
    #train <- h2o.assign(train, "train.hex")
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    remove(train_col)
    save(tables,file=paste0("tables",day,name,".Rda"))
    remove(tables)
    gc()
}

# splite ------------------------------------------------------------------
#save(tables,file=paste0("tables",day,".Rda"))
#load(paste0("tables",day,".Rda"))

train <- h2o.getFrame(localH2O, "train.hex")

train[,'hour'] <- train[,'hour']/23

cat("\nSplitting into train/validation")
# Random split
rand <- h2o.runif(train, seed = myseed)
rand <- h2o.assign(rand, key = "rand")
train_train <- train[rand <= 0.8, ]
train_train <- h2o.assign(train_train, key = "train_train")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
train_valid <- train[rand > 0.8, ]
train_valid <- h2o.assign(train_valid, key = "train_valid")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train.hex", x = h2o.ls(localH2O)$Key, value = TRUE))
## Split into train/validation based on training days (first 9 days: train, last day: test)
#train <- h2o.assign(train_hex[train_hex$day<30,], 'train')
#valid <- h2o.assign(train_hex[train_hex$day==30,], 'valid')

# models ------------------------------------------------------------------


non_cols <- c("id","click")
names <- colnames(train)
names <- names[! names %in% non_cols]

cat("\nTraining H2O model on training/validation splits days <30/30")
## Note: This could be grid search models, after which you would obtain the best model with model <- cvmodel@model[[1]]
#hidden_layers = list(c(200,200), c(100,300,100),c(500,500,500))
#cvmodel <- h2o.deeplearning(data=train_train, validation=train_valid, x=c(3:ncol(train)), y=2,
#                            hidden=c(200,50), activation = "Rectifier", max_categorical_features=100000, 
#                            train_samples_per_iteration=16384, score_validation_samples=100000, 
#                            l1=1e-5, l2=1e-4, input_dropout_ratio = 0.2, 
#                            balance_classes=TRUE, epochs = 12, seed=myseed)

# model: MLP
dlmodel = h2o.deeplearning(data=train_train, validation=train_valid, x=names, y='click',
                           hidden=c(100,300,150,50), activation ="Rectifier", 
                           train_samples_per_iteration=-2, score_validation_samples=10000, 
                           l1=1e-5, l2=1e-5, epsilon=1e-10,
                           balance_classes=TRUE, epochs = 10, seed=myseed)

path_model <- paste0(path,"/dlmodel",day)
h2o.saveModel(object = dlmodel, name = path_model, force = TRUE)

# model: GBM
gbmmodel <- h2o.gbm(data=train_train, validation=train_valid, x=names, y='click',
                    n.tree=400, interaction.depth=10, n.minobsinnode = 40, 
                    shrinkage = 0.1, n.bins = 10, balance.classes = TRUE)

path_model <- paste0(path,"/gbmmodel",day)
h2o.saveModel(object = gbmmodel, name = path_model, force = TRUE)

# model more --------------------------------------------------------------
# model: RF
# really a bad model
rfmodel <- h2o.randomForest(data=train_train, validation=train_valid, x=names, y='click',
                            type="fast", sample.rate = 2/3, mtries = -1, ntree=1000, 
                            depth=20, nbins = 10, stat.type = "ENTROPY",
                            balance.classes = TRUE, seed=myseed)
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

path_submission <- paste0(path,"/dlmodel1count28_30.csv")
dl21 <- read.csv(path_submission)
dl21 <- as.data.frame(dl21)

train_resp <- train_train[,1] #actual label
train_resp <- h2o.assign(train_resp, "train_resp")
dlmodel <- h2o.getModel(localH2O, 'DeepLearning_9d9203349524ce3c946ece580204a12')
train_preds <- h2o.predict(dlmodel, train_train)[,3] #[,3] is probability for class 1
train_preds <- h2o.assign(train_preds, "train_preds")
cat("\nLogLoss on training data:", h2o.logLoss(train_preds, train_resp))

train_resp <- train_auto[,1] #actual label
train_resp <- h2o.assign(train_resp, "train_resp")
train_preds <- h2o.predict(autogbmmodel1, train_auto)[,3] #[,3] is probability for class 1
train_preds <- h2o.assign(train_preds, "train_preds")
cat("\nLogLoss on training data:", h2o.logLoss(train_preds, train_resp))

valid_resp <- train_valid[,2]
valid_preds <- h2o.predict(rfmodel, train_valid)[,3]
cat("\nLogLoss on validation data:", h2o.logLoss(valid_preds, valid_resp))

h2o.rm(localH2O, grep(pattern = "DeepLearningPredict", x = h2o.ls(localH2O)$Key, value = TRUE))


# testing codes --------------------------------------------------------------------
table_name <- c(col_names[7],"click")
tables[[1]] <- h2o.table(train[,table_name])
tables[[1]] <- as.data.frame(tables[[1]])
colnames(tables[[1]]) <- c(col_names[7],"noclick","click")
tables[[1]]$count <- tables[[1]]$noclick+tables[[1]]$click
#otherrow <- sum(which(tables[[1]][,col_names[7]]=="other"))
#tables[[1]][otherrow,"count"] <- 1
tables[[1]]$clickrate <- tables[[1]]$click/tables[[1]]$count
keeps <- c(col_names[7],"count","clickrate")
tables[[1]] <- tables[[1]][keeps]
tables[[1]]$likelihood <- (l/(tables[[1]]$count+m))*glh+(1-l/(tables[[1]]$count+m))*tables[[1]]$clickrate

# training data factor replacement
train_col <- as.data.frame(train[,col_names[7]])
for (i in (1:nrow(tables[[1]]))) {
    ele <- tables[[1]][i,]
    level_pos <- match(as.character(ele[[col_names[7]]]), levels(train_col[,1]))
    levels(train_col[,1])[level_pos] <- as.character(ele[["likelihood"]])
}

#levels(train_col[,1])[level_pos] <- "7801e8d9"
#train_col[,1] <- as.numeric(as.character(train_col[,1]))
train_col[,1] <- as.numeric(levels(train_col[,1]))[train_col[,1]]

train_col <- as.h2o(localH2O, train_col)

#train <- h2o.getFrame(localH2O, "train.hex")
train <- cbind(train[,-match(col_names[7],colnames(train))], train_col)
new_col <- grep(pattern="Last.value", x=colnames(train), value=FALSE)
colnames(train)[new_col] <- col_names[7]
# Store train under designated key
train <- h2o.assign(train, "train.hex")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(train_col, table_target)

save(foo,file="data.Rda")
load("data.Rda")

# too slow
#rowswhere <- which(train[,col_names[7]] == as.character(ele[[col_names[7]]]))
#rowswhere <- as.data.frame(rowswhere)
#rowswhere <- as.data.frame(train[,col_names[7]] == as.character(ele[[col_names[7]]]))
#train[,"new1"][train[,col_names[7]] == as.character(ele[[col_names[7]]])]<- ele[["likelihood"]]
# dummy real column initilization
#train$new1 <- 0.1
#for (row in rowswhere[["which"]]) {
#    train[row,"new1"]<- ele[["likelihood"]]
#}    

install.packages("plyr")

UCBAdmissions ## already a contingency table
DF <- as.data.frame(UCBAdmissions)

prosPath = system.file("extdata", "prostate.csv", package="h2o")
prostate.hex = h2o.importFile(localH2O, path = prosPath, key = "prostate.hex")

#create a new variable (column) & assign each element an "id"
# from 1 to the number of rows of df_1
prostate.hex$ids <- 1:nrow(prostate.hex) 

# merge the two data frames using the label column without sorting
merged <- merge(prostate.hex, agetable, sort = FALSE)

# order the merged data set using "id" & assign it to "ordered"
ordered <- merged[order(merged$ids), ]

h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
