# libfm -------------------------------------------------------------------
path <- "/home/whale/Documents/click"
submission <- read.csv(paste0(path,"/submission_31.csv"),colClasses = c("character"))

pred <- read.csv(paste0(path,"/xgboost1.txt"), header = F)
submission[,2] <- as.data.frame(pred)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste(path,"/xgboost1.csv", sep = ''), quote = F, row.names = F)