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

# data preprocessing ------------------------------------------------------------
### START
myseed=1988
# arg <- commandArgs(trailingOnly = TRUE)
# arg <- as.numeric(arg)
# m <- arg[1]
# l <- arg[2]
day <- 27
path <- "/home/whale/Documents/click"

rate <- 10

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
h2o.addTimeFeatures <- function(frame, timecol) {
    cat("\nFeature engineering for time column.")
    hour <- frame[,timecol] %% 100
    frame[,timecol] <- hour
    
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    frame
}
train <- h2o.addTimeFeatures(train_hex, "hour")
train[,'hour'] <- train[,'hour']/23
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

#h2o.int2factor(train, intcols_largecates)
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

# cat("\nFeature engineering for column interactions.")
# h2o.addInteractionFeatures <- function(frame, key) {
#     cat("\nFeature Interaction...")
#     #allcols <- c("C1","banner_pos","site_id","site_domain","site_category",
#     #"app_id","app_domain","app_category","device_id","device_ip","device_model",
#     #"device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")
#     #newfactors <- c()
#     # create pair-wise interaction between factors, keep top 100 levels
#     #factor_interactions <- h2o.interaction(frame, factors = allcols, pairwise = TRUE, max_factors = 100, min_occurrence = 2)
#     factor_list <- list(c("site_id","site_domain","site_category"),c("app_id","app_domain","app_category"),
#                         c("device_id","device_ip","device_model","device_type","device_conn_type"))
#     factor_combines <- h2o.interaction(frame, factors = factor_list, pairwise = FALSE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines1 <- h2o.interaction(frame, factors = factor_list[1], pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines2 <- h2o.interaction(frame, factors = factor_list[2], pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines3 <- h2o.interaction(frame, factors = factor_list[3], pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines4 <- h2o.interaction(frame, factors = c("banner_pos","device_conn_type"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines5 <- h2o.interaction(frame, factors = c("banner_pos","site_category"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines6 <- h2o.interaction(frame, factors = c("banner_pos","app_category"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines7 <- h2o.interaction(frame, factors = c("banner_pos","device_type"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines8 <- h2o.interaction(frame, factors = c("C14","C19"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines9 <- h2o.interaction(frame, factors = c("C20","C21"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines10 <- h2o.interaction(frame, factors = c("C17","site_domain"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines11 <- h2o.interaction(frame, factors = c("C14","app_id"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines12 <- h2o.interaction(frame, factors = c("C19","device_id"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines13 <- h2o.interaction(frame, factors = c("C21","site_id"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines14 <- h2o.interaction(frame, factors = c("C20","device_model"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines15 <- h2o.interaction(frame, factors = c("C14","C21"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines16 <- h2o.interaction(frame, factors = c("C14","device_ip"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines17 <- h2o.interaction(frame, factors = c("C17","site_id"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines18 <- h2o.interaction(frame, factors = c("C14","device_id"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines19 <- h2o.interaction(frame, factors = c("C19","site_id"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines20 <- h2o.interaction(frame, factors = c("C17","app_id"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines21 <- h2o.interaction(frame, factors = c("C21","C19"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
#     factor_combines22 <- h2o.interaction(frame, factors = c("C20","C17"), pairwise = TRUE, max_factors = 100*rate, min_occurrence = 2)
# 
#     #frame <- cbind(frame[,-match(c("device_id","device_ip"),colnames(frame))], 
#     #               factor_combines, factor_combines1, factor_combines2, 
#     #               factor_combines3, factor_combines4, factor_combines5,
#     #               factor_combines6, factor_combines7)
#     frame <- cbind(frame, 
#                    factor_combines, factor_combines1, factor_combines2, 
#                    factor_combines3, factor_combines4, factor_combines5,
#                    factor_combines6, factor_combines7, factor_combines8,
#                    factor_combines9, factor_combines10, factor_combines11,
#                    factor_combines12, factor_combines13, factor_combines14,
#                    factor_combines15, factor_combines16, factor_combines17,
#                    factor_combines18, factor_combines19, factor_combines20,
#                    factor_combines21, factor_combines22)
#     
#     # Store frame under designated key
#     frame <- h2o.assign(frame, key)
#     
#     h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
#     
#     frame
# }
# train <- h2o.addInteractionFeatures(train, "train.hex")
#valid <- h2o.addInteractionFeatures(valid, "valid.hex")
#test <- h2o.addInteractionFeatures(test, "test.hex")
#h2o.rm(localH2O, grep(pattern = "train.hex.interaction.", x = h2o.ls(localH2O)$Key, value = TRUE))
#h2o.rm(localH2O, grep(pattern = "valid.hex.interaction.", x = h2o.ls(localH2O)$Key, value = TRUE))
#h2o.rm(localH2O, grep(pattern = "test.hex.interaction.", x = h2o.ls(localH2O)$Key, value = TRUE))
#remove(test)
#train <- train[,-match(c('device_id','device_ip'),colnames(train))]
train <- h2o.assign(train, "train.hex")
train_copy <- train
train_copy <- h2o.assign(train_copy, "train_copy.hex")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train_day", x = h2o.ls(localH2O)$Key, value = TRUE))
remove(train_hex)

# likelihood --------------------------------------------------------------
#train <- h2o.getFrame(localH2O, "train.hex")

# global likelihood
cat("\nCalculate likelihood.")
table_target <- h2o.table(train[,"click"])
glh <- table_target[table_target[,1] == 1][,2]/sum(table_target[,"Count"])
glh <- sum(glh)
remove(table_target)
save(glh,file=paste0("tables",day,"glh.Rda"))

# columns to be transformed
non_counts <- c("id","click","hour")
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
    train_col_copy <- train_col
    for (i in (1:nrow(tables))) {
        ele <- tables[i,]
        level_pos <- match(as.character(ele[[name]]), levels(train_col[,1]))
        level_pos_copy <- match(as.character(ele[[name]]), levels(train_col_copy[,1]))
        levels(train_col[,1])[level_pos] <- as.character(ele[["likelihood"]])
        levels(train_col_copy[,1])[level_pos_copy] <- as.character(ele[["count"]])
    }
    
    #levels(train_col[,1])[level_pos] <- "7801e8d9"
    #train_col[,1] <- as.numeric(as.character(train_col[,1]))
    train_col[,1] <- as.numeric(levels(train_col[,1]))[train_col[,1]]
    train_col_copy[,1] <- as.numeric(levels(train_col_copy[,1]))[train_col_copy[,1]]
    
    train_col <- as.h2o(localH2O, train_col)
    train_col_copy <- as.h2o(localH2O, train_col_copy)
    #train <- h2o.getFrame(localH2O, "train.hex")
    train[,name] <- train_col
    train_copy[,name] <- train_col_copy
    #train <- cbind(train[,-match(name,colnames(train))], train_col)
    #new_col <- grep(pattern="Last.value", x=colnames(train), value=FALSE)
    #colnames(train)[new_col] <- name
    # Store train under designated key
    #train <- h2o.assign(train, "train.hex")
    h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
    remove(train_col)
    remove(train_col_copy)
    save(tables,file=paste0("tables",day,name,".Rda"))
    remove(tables)
    gc()
}

# count modification ------------------------------------------------------
for (name in col_names) {
    train_copy[,name] <- log(train_copy[,name])
    mincol <- min(train_copy[,name])
    maxcol <- max(train_copy[,name])
    train_copy[,name] <- (train_copy[,name]-mincol)/(maxcol-mincol)
}

h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))

train_copy <- train_copy[,col_names]

newcolnames <- c()
for (oldname in colnames(train_copy)) {
    newname <- paste0(oldname,'_count')
    newcolnames <- c(newcolnames,newname)
}

colnames(train_copy) <- newcolnames

# combine count and clikerate ---------------------------------------------

train <- cbind(train, train_copy)
train <- h2o.assign(train, "train.hex")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train_copy.hex", x = h2o.ls(localH2O)$Key, value = TRUE))

# splite ------------------------------------------------------------------
#save(tables,file=paste0("tables",day,".Rda"))
#load(paste0("tables",day,".Rda"))
#train <- h2o.getFrame(localH2O, "train.hex")

cat("\nSplitting into train/validation")
# Random split
rand <- h2o.runif(train, seed = myseed)
rand <- h2o.assign(rand, key = "rand")
train_test <- train[rand > 0.9, ]
train_test <- h2o.assign(train_test, key = "train_test")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
train_train <- train[rand <= 0.9, ]
train <- train_train
train <- h2o.assign(train, key = "train")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "train.hex", x = h2o.ls(localH2O)$Key, value = TRUE))
h2o.rm(localH2O, grep(pattern = "rand", x = h2o.ls(localH2O)$Key, value = TRUE))

rand <- h2o.runif(train, seed = myseed)
rand <- h2o.assign(rand, key = "rand")
train_valid <- train[rand > 0.8, ]
train_valid <- h2o.assign(train_valid, key = "train_valid")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
train_train <- train[rand <= 0.8, ]
train_train <- h2o.assign(train_train, key = "train_train")
h2o.rm(localH2O, grep(pattern = "Last.value", x = h2o.ls(localH2O)$Key, value = TRUE))
#h2o.rm(localH2O, "train")
h2o.rm(localH2O, grep(pattern = "rand", x = h2o.ls(localH2O)$Key, value = TRUE))
#remove(train)
## Split into train/validation based on training days (first 9 days: train, last day: test)
#train <- h2o.assign(train_hex[train_hex$day<30,], 'train')
#valid <- h2o.assign(train_hex[train_hex$day==30,], 'valid')