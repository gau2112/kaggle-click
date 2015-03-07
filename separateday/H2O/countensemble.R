# initialization ----------------------------------------------------------

library(h2o)
# java -Xmx16G -jar h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision
localH2O <- h2o.init(ip = 'localhost', port = 54321)

myseed=1988
testday <- 29
path <- "/home/whale/Documents/click"

target <- read.csv(paste0(path,"/submission",testday,".csv"))
numrow <- nrow(target)

data <- as.data.frame(matrix(0, ncol = 10, nrow = numrow))

data[,1] <- target[,2]
#daylist <- c(21,22,23,24,25,26,27,28,29)
daylist <- c(27,28,29)
col <- 1
filelist <- c("/dlmodel1countorigin","/dlmodel2countorigin","/gbmmodelcountorigin")
              #,"/autogbmmodel1count","/autogbmmodel2count")
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
h2o.rm(localH2O, grep(pattern = "data", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "rand", x = h2o.ls(localH2O)$Key, value = TRUE))

# new average -------------------------------------------------------------

average1 <- (data[,2]+data[,7]+data[,12]+data[,17]+data[,22]+data[,27]+data[,32]+data[,37]+data[,42])/9
average2 <- (data[,3]+data[,8]+data[,13]+data[,18]+data[,23]+data[,28]+data[,33]+data[,38]+data[,43])/9
average3 <- (data[,4]+data[,9]+data[,14]+data[,19]+data[,24]+data[,29]+data[,34]+data[,39]+data[,44])/9
average4 <- (data[,5]+data[,10]+data[,15]+data[,20]+data[,25]+data[,30]+data[,35]+data[,40]+data[,45])/9
average5 <- (data[,6]+data[,11]+data[,16]+data[,21]+data[,26]+data[,31]+data[,36]+data[,41]+data[,46])/9

averagetarget <- as.h2o(localH2O, target[,2])
modelaverage <- cbind(averagetarget,average1,average2,average3,average4,average5)
modelaverage <- h2o.assign(modelaverage, "modelaverage")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))

average1 <- (data[,2]+data[,3]+data[,4]+data[,5]+data[,6])/5
average2 <- (data[,7]+data[,8]+data[,9]+data[,10]+data[,11])/5
average3 <- (data[,12]+data[,13]+data[,14]+data[,15]+data[,16])/5
average4 <- (data[,17]+data[,18]+data[,19]+data[,20]+data[,21])/5
average5 <- (data[,22]+data[,23]+data[,24]+data[,25]+data[,26])/5
average6 <- (data[,27]+data[,28]+data[,29]+data[,30]+data[,31])/5
average7 <- (data[,32]+data[,33]+data[,34]+data[,35]+data[,36])/5
average8 <- (data[,37]+data[,38]+data[,39]+data[,40]+data[,41])/5
average9 <- (data[,42]+data[,43]+data[,44]+data[,45]+data[,46])/5

averagetarget <- as.h2o(localH2O, target[,2])
dayaverage <- cbind(averagetarget,average1,average2,average3,average4,average5,average6,average7,average8,average9)
dayaverage <- h2o.assign(dayaverage, "dayaverage")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))

average1 <- (data[,2]+data[,3]+data[,4])/3
average2 <- (data[,7]+data[,8]+data[,9])/3
average3 <- (data[,12]+data[,13]+data[,14])/3
average4 <- (data[,17]+data[,18]+data[,19])/3
average5 <- (data[,22]+data[,23]+data[,24])/3
average6 <- (data[,27]+data[,28]+data[,29])/3
average7 <- (data[,32]+data[,33]+data[,34])/3
average8 <- (data[,37]+data[,38]+data[,39])/3
average9 <- (data[,42]+data[,43]+data[,44])/3

averagetarget <- as.h2o(localH2O, target[,2])
dayaverage <- cbind(averagetarget,average1,average2,average3,average4,average5,average6,average7,average8,average9)
dayaverage <- h2o.assign(dayaverage, "dayaverage")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
# average splite -----------------------------------------------------------

cat("\nSplitting into train/validation")
# Random split
rand <- h2o.runif(modelaverage, seed = myseed)
rand <- h2o.assign(rand, key = "rand")

train_train <- dayaverage[rand <= 0.8, ]
train_train <- h2o.assign(train_train, key = "train_train")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
train_valid <- dayaverage[rand > 0.8, ]
train_valid <- h2o.assign(train_valid, key = "train_valid")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "dayaverage", x = h2o.ls(localH2O)$Key, value = TRUE))

train_train <- modelaverage[rand <= 0.8, ]
train_train <- h2o.assign(train_train, key = "train_train")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
train_valid <- modelaverage[rand > 0.8, ]
train_valid <- h2o.assign(train_valid, key = "train_valid")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "modelaverage", x = h2o.ls(localH2O)$Key, value = TRUE))

h2o.rm(localH2O, grep(pattern = "rand", x = h2o.ls(localH2O)$Key, value = TRUE))

# ensemble ----------------------------------------------------------------

# model: MLP
cat("\nDeep learning model.")
#ncol(train_train)
#hidden=c(100,50)
dlmodel = h2o.deeplearning(data=train_train, validation=train_valid, x=c(2:ncol(train_train)), y=1,
                            hidden=c(100,50), activation ="Rectifier", 
                            train_samples_per_iteration=-2, score_validation_samples=10000, 
                            score_validation_sampling='Stratified',
                            l1=1e-5, l2=1e-5, epsilon=1e-10,
                            #override_with_best_model=FALSE,
                            input_dropout_ratio = 0.2, hidden_dropout_ratios = c(0.5,0.5),
                            balance_classes=TRUE, epochs = 20, seed=myseed)

path_model <- paste0(path,"/dlmodelcountfinalday3")
#path_model <- paste0(path,"/dlmodel1countfinal")
h2o.saveModel(object = dlmodel, name = path_model, force = TRUE)
h2o.rm(localH2O, grep(pattern = "DeepLearning", x = h2o.ls(localH2O)$Key, value = TRUE))

cat("\nGradiant boosting machine model.")
gbmmodel <- h2o.gbm(data=train_train, validation=train_valid, x=c(2:ncol(train_train)), y=1,
                    n.tree=100, interaction.depth=10, n.minobsinnode = 40, 
                    shrinkage = 0.4, n.bins = 20, balance.classes = TRUE,
                    importance = TRUE)

path_model <- paste0(path,"/gbmmodelcountfinalday3")
h2o.saveModel(object = gbmmodel, name = path_model, force = TRUE)
h2o.rm(localH2O, grep(pattern = "GBM", x = h2o.ls(localH2O)$Key, value = TRUE))

# model: RF
#cat("\Random Forest model.")
#rfmodel <- h2o.randomForest(data=train_train, validation=train_valid, x=c(2:ncol(train_train)), y=1,
#                            type="fast", sample.rate = 2/3, mtries = -1, ntree=1000, 
#                            depth=20, nbins = 10, stat.type = "ENTROPY",
#                            balance.classes = TRUE, seed=myseed)

#path_model <- paste0(path,"/rfmodelcountfinal")
#h2o.saveModel(object = rfmodel, name = path_model, force = TRUE)
#h2o.rm(localH2O, grep(pattern = " SpeeDRF", x = h2o.ls(localH2O)$Key, value = TRUE))

cat("\nGeneralized Linear model.")
glmmodel <- h2o.glm(data=train_train, x=c(2:ncol(train_train)), y=1,
                    family="binomial",
                    alpha = 0.5, 
                    lambda_search = TRUE, 
                    nlambda = 50, 
                    #lambda = 1.74E-3,
                    #has_intercept = FALSE,lambda = 2.17E-3,
                    #lambda.min.ratio = ,  
                    standardize = FALSE, variable_importances = TRUE,
                    iter.max = 500)

path_model <- paste0(path,"/glmmodelcountfinalday3")
h2o.saveModel(object = glmmodel, name = path_model, force = TRUE)
h2o.rm(localH2O, grep(pattern = "GLM", x = h2o.ls(localH2O)$Key, value = TRUE))

