# initialization ----------------------------------------------------------
# initialize H2O
library(h2o)
# java -Xmx16G -jar h2o.jar -port 54321 -data_max_factor_levels 10000000 -nthreads 6 -single_precision
localH2O <- h2o.init(ip = 'localhost', port = 54321)

myseed=1988
testday <- 31
path <- "/home/whale/Documents/click"

# read test data file
target <- read.csv(paste0(path,"/submission",testday,".csv"),colClasses = c("character"))
numrow <- nrow(target)

# results from different models to be ensembled
data <- as.data.frame(matrix(0, ncol = 4, nrow = numrow))

data[,1] <- as.numeric(target[,2])
col <- 1
filelist <- c("/dlmodelcountfinal","/gbmmodelcountfinal","/glmmodelcountfinal")
#"/autogbmmodel1count","/autogbmmodel2count"

for (file in filelist) {
    col <- col+1
    readdata <- read.csv(paste0(path,file,testday,".csv"))
    data[,col] <- readdata[,2]
}  

# data from R environment to H2O environment
data <- as.h2o(localH2O, data)
data <- h2o.assign(data, "data")

h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))

# ensemble ----------------------------------------------------------------

cat("\nGeneralized Linear model.")
glmmodel <- h2o.glm(data=data, x=c(2:ncol(data)), y=1,
                    family="binomial",
                    alpha = 0, lambda_search = FALSE,  
                    lambda = 2.16E-3,
                    #nlambda = 100, 
                    #has_intercept = FALSE,
                    #lambda.min.ratio = ,  
                    standardize = FALSE, variable_importances = TRUE,
                    iter.max = 500)

path_model <- paste0(path,"/glmmodelcountfinal4models")
h2o.saveModel(object = glmmodel, name = path_model, force = TRUE)
h2o.rm(localH2O, grep(pattern = "GLM", x = h2o.ls(localH2O)$Key, value = TRUE))

# testing -----------------------------------------------------------------

cat("\nTesting data: GLM")
glmmodel <- h2o.loadModel(localH2O, paste0(path,"/glmmodelcountfinal4models"))
test_predglm <- h2o.predict(glmmodel, data)[,3]
target[,2] <- as.data.frame(test_predglm)
colnames(target) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(target), file = paste0(path,"/glmmodelcountfinal4models",testday,".csv"), quote = F, row.names = F)
h2o.rm(localH2O, grep(pattern = "GLM", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(glmmodel,test_predglm)

