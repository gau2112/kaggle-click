# vowpal wabbit -----------------------------------------------------------
path <- "/home/whale/Documents/click"
submission <- read.csv(paste0(path,"/submission_31.csv"),colClasses = c("character"))

pred <- read.csv(paste0(path,"/polymore.pred"), header = F)
pred[,1] <- 1 / (1 + exp(-pred[,1]))

submission[,2] <- as.data.frame(pred)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")
write.csv(as.data.frame(submission), file = paste(path,"/vwpolymore.csv", sep = ''), quote = F, row.names = F)