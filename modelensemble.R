# average -----------------------------------------------------------------
myseed=1988
testday <- 31
path <- "/home/whale/Documents/click"
submission <- read.csv(paste0(path,"/submission",testday,".csv"),colClasses = c("character"))

model1<- read.csv(paste0(path,"/fm4_106deviceip.csv"))
model1 <- as.data.frame(model1)

model2<- read.csv(paste0(path,"/fm8_106deviceip.csv"))
model2 <- as.data.frame(model2)

model3<- read.csv(paste0("/logistic2+.csv"))
model3 <- as.data.frame(model3)

model4<- read.csv(paste0(path,"/vworigintest.csv"))
model4 <- as.data.frame(model4)

model5<- read.csv(paste0(path,"/vworiginpoly.csv"))
model5 <- as.data.frame(model5)

model6<- read.csv(paste0(path,"/vworiginpolymask1.csv"))
model6 <- as.data.frame(model6)

model7<- read.csv(paste0(path,"/fm.csv"))
model7 <- as.data.frame(model7)

model8 <- read.csv(paste0(path,"/fm4_107.csv"))
model8 <- as.data.frame(model8)

model9 <- read.csv(paste0(path,"/vwpoly.csv"))
model9 <- as.data.frame(model9)

model10 <- read.csv(paste0(path,"/fm4onehot3008.csv"))
model10 <- as.data.frame(model10)

model11 <- read.csv(paste0(path,"/fm8onehot3r002.csv"))
model11 <- as.data.frame(model11)

model12 <- read.csv(paste0(path,"/vwpoly2++fmaverage+longlogorifm7.csv"))
model12 <- as.data.frame(model12)

model13 <- read.csv(paste0(path,"/vwpoly24.csv"))
model13 <- as.data.frame(model13)

model14 <- read.csv(paste0(path,"/vworiginonehot3.csv"))
model14 <- as.data.frame(model14)

model15 <- read.csv(paste0(path,"/vwnn.csv"))
model15 <- as.data.frame(model15)

model16 <- read.csv(paste0(path,"/vwpolyqual001.csv"))
model16 <- as.data.frame(model16)

model17 <- read.csv(paste0(path,"/vwpolymore.csv"))
model17 <- as.data.frame(model17)

model18 <- read.csv(paste0(path,"/xgboost1.csv"))
model18 <- as.data.frame(model18)

#average <- (model1[,2]+model2[,2]+model3[,2]+model4[,2]+model5[,2]+model6[,2]+model7[,2]+model8[,2])/8
#average <- (model1[,2]+model2[,2]+model7[,2]+model8[,2])/4

#average <- (2/15)*model3[,2]+(3/15)*model8[,2]+(2/15)*model14[,2]+(1/15)*model15[,2]+(4/15)*model17[,2]+(3/15)*model16[,2]
#average1 <- (model11[,2]+model10[,2])/2
#average <- (1/7)*average+(2/7)*average1+(2/7)*model9[,2]+(2/7)*model13[,2]
#average <- (average+model9[,2]+model13[,2])/3

#average <- (average1*model9[,2]*model13[,2])^(1/3)
average <- (model4[,2]+model3[,2])/2

#d <- mean(model12[,2]) - 0.1698
#average <- model12[,2]-d
submission[,2] <- as.data.frame(average)
colnames(submission) <- c("id", "click")
cat("\nWriting predictions on test data.")

write.csv(as.data.frame(submission), file = paste(path,"/logistic2++vwgorgintest.csv", sep = ''), quote = F, row.names = F)
