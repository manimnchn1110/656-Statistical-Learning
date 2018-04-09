library(zoo)
library(TTR)
library(xts)
library(quantmod)
library(rpart)
library(rpart.plot)
library(lattice)
library(ggplot2)
library(caret)
library(randomForest)
library(tree)
library(e1071)
library(neuralnet)
library(nnet)

startDate = as.Date("2010-01-01")
endDate = as.Date("2018-04-01")
#1. AAPL
getSymbols('AAPL', src ="yahoo",from=startDate, to=endDate)
head(AAPL)

#2. GOOGL
getSymbols("GOOGL", src ="yahoo", from=startDate, to=endDate)
head(GOOGL)

#3. GOOG
getSymbols('GOOG', src ="yahoo", from=startDate, to=endDate)
head(GOOG)

#4. MSFT
symbol_4 = "MSFT"
getSymbols(symbol_4, src ="yahoo", from=startDate, to=endDate)
head(MSFT)

#5. FB
symbol_5 = "IBM"
getSymbols(symbol_5, src ="yahoo", from=startDate, to=endDate)
head(IBM)

#Plot
x<-cbind(AAPL,GOOGL, GOOG, MSFT, IBM)
y<-cbind(x[,'AAPL.Adjusted'],
         x[,'GOOGL.Adjusted'],
         x[,'GOOG.Adjusted'],
         x[,'MSFT.Adjusted'],
         x[,'IBM.Adjusted'])
plot(as.zoo(y),main = "AAPL,GOOGL, GOOG, MSFT, IBN",ylab = c("AAPL","GOOGL", "GOOG", "MSFT", "IBM")) 

#Dependent Variable: Tendency: The stock price Up/Down
tendency <- function(x){
  pricechange <- Cl(x)-Op(x)
  tendency <- ifelse(pricechange>0,1,0)
  return(tendency)
}


#Basic Variables:
###
#1. Day Close(Daily)
#2. Day Open(Daily)
#3. Highest Price(Daily)
#4. Lowest price(Daily)
#5. Adjusted close price(Daily)
###
#Technical Variables:
#6. MACD
myMACD <- function(x) MACD(Cl(x))[,'signal']

#7. RSI
myRSI <- function(x) RSI(Cl(x))

#8. TRIX
myTRIX <- function(x) TRIX(Cl(x))[,'signal']

#9. SAR
mySAR <- function(x) SAR(cbind(Hi(x),Cl(x)))[,'sar']

#10. ROC - Volume
myROC.Vo <- function(x) ROC(Vo(x))

#11. ROC - Cl
myROC.Cl <- function(x) ROC(Cl(x))

#12. CCI
myCCI <- function(x) CCI(HLC(x))

#13. ATR
myATR        <- function(x) ATR(HLC(x))[,'atr']

#14. SMI
mySMI        <- function(x) SMI(HLC(x))[, "SMI"]

#15. ADX
myADX        <- function(x) ADX(HLC(x))[,'ADX']

#16. Aroon 
myAroon      <- function(x) aroon(cbind(Hi(x),Lo(x)))[,'oscillator']

#17. BB  
myBB         <- function(x) BBands(HLC(x))[,'pctB']

#18. ChaikinVol
myChaikinVol <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))
head(myChaikinVol(AAPL))

#19. CLV 
myCLV        <- function(x) EMA(CLV(HLC(x)))

#20. EMV 
myEMV        <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2]

#21. MFI 
myMFI        <- function(x) MFI(HLC(x),  Vo(x))

#22. Volat
myVolat      <- function(x) volatility(OHLC(x),calc="garman")
#23. CMO - Adjusted Price
myCMO.Ad <- function(x) CMO(Ad(x))

#24. CMO - Volume
myCMO.Vo <- function(x) CMO(Vo(x))

#25. RunMean - Close
myRunMean <- function(x) runMean(Cl(x))

#26. runSD - Close
myRunSD <- function(x) runSD(Cl(x))

#27. GMMA - Volume
myGMMA.Vo <- function(x) GMMA(Vo(x))[,1]


#28. KST - Close
myKST <- function(x) KST(Cl(x))[,'signal']

#29. DPO - Volume
myDPO <-  function(x) DPO(Vo(x))

#30. DVI - Adjusted Price
myDVI <- function(x) DVI(Ad(x))[,'dvi']

#Training Set/Test Set
#combine 5 stocks to 1 set of training set
raw_data <- rbind(AAPL,GOOGL, GOOG, MSFT, IBM)
raw_data <- as.data.frame(raw_data)
row.names(raw_data) <- NULL
raw_data <- na.omit(raw_data)

colnames(raw_data)[1] <- 'Open'
colnames(raw_data)[2] <- 'High'
colnames(raw_data)[3] <- 'Low'
colnames(raw_data)[4] <- 'Close'
colnames(raw_data)[5] <- 'Volume'
colnames(raw_data)[6] <- 'Adjusted'

dim(raw_data)
first(raw_data)

new_data <- function(raw_data){
  df <- data.frame(Tendency = tendency(raw_data),
                   raw_data,
                   ATR = myATR(raw_data),
                   SMI = mySMI(raw_data),
                   ADX = myADX(raw_data),
                   Aroon = myAroon(raw_data),
                   BB = myBB(raw_data),
                   ChaikinVol = myChaikinVol(raw_data),
                   CLV = myCLV(raw_data),
                   CMO.Ad = myCMO.Ad(raw_data),
                   CMO.Vo = myCMO.Vo(raw_data),
                   EMA = EMA(Delt(Cl(raw_data))),
                   EMV = myEMV(raw_data),
                   Volat = myVolat(raw_data),
                   MACD = myMACD(raw_data),
                   MFI = myMFI(raw_data),
                   RSI = myRSI(raw_data),
                   SAR = mySAR(raw_data),
                   RunMean = myRunMean(raw_data),
                   runSD = myRunSD(raw_data),
                   TRIX = myTRIX(raw_data),
                   ROC.Vo = myROC.Vo(raw_data),
                   ROC.Cl = myROC.Cl(raw_data),
                   CCI = myCCI(raw_data),
                   GMMA.Vo.lag3 = myGMMA.Vo(raw_data),
                   KST = myKST(raw_data),
                   DPO = myDPO(raw_data),
                   DVI = myDVI(raw_data))
  return(df)
  }

new_data <- na.omit(new_data(raw_data))
dim(new_data)
Training_set <- new_data[1:8000, ]
Test_set <- new_data[8000:10008, ]


AAPL <- as.data.frame(AAPL)
colnames(AAPL)[1] <- 'Open'
colnames(AAPL)[2] <- 'High'
colnames(AAPL)[3] <- 'Low'
colnames(AAPL)[4] <- 'Close'
colnames(AAPL)[5] <- 'Volume'
colnames(AAPL)[6] <- 'Adjusted'
row.names(AAPL) <- NULL
AAPL <- na.omit(AAPL)
AAPL_Set <- na.omit(new_data(AAPL))


GOOGL <- as.data.frame(GOOGL)
colnames(GOOGL)[1] <- 'Open'
colnames(GOOGL)[2] <- 'High'
colnames(GOOGL)[3] <- 'Low'
colnames(GOOGL)[4] <- 'Close'
colnames(GOOGL)[5] <- 'Volume'
colnames(GOOGL)[6] <- 'Adjusted'
row.names(GOOGL) <- NULL
GOOGL <- na.omit(GOOGL)
GOOGL_Set <- na.omit(new_data(GOOGL))

GOOG <- as.data.frame(GOOG)
colnames(GOOG)[1] <- 'Open'
colnames(GOOG)[2] <- 'High'
colnames(GOOG)[3] <- 'Low'
colnames(GOOG)[4] <- 'Close'
colnames(GOOG)[5] <- 'Volume'
colnames(GOOG)[6] <- 'Adjusted'
row.names(GOOG) <- NULL
GOOG <- na.omit(GOOG)
GOOG_Set <- na.omit(new_data(GOOG))

MSFT <- as.data.frame(MSFT)
colnames(MSFT)[1] <- 'Open'
colnames(MSFT)[2] <- 'High'
colnames(MSFT)[3] <- 'Low'
colnames(MSFT)[4] <- 'Close'
colnames(MSFT)[5] <- 'Volume'
colnames(MSFT)[6] <- 'Adjusted'
row.names(MSFT) <- NULL
MSFT <- na.omit(MSFT)
MSFT_Set <- na.omit(new_data(MSFT))

IBM <- as.data.frame(IBM)
colnames(IBM)[1] <- 'Open'
colnames(IBM)[2] <- 'High'
colnames(IBM)[3] <- 'Low'
colnames(IBM)[4] <- 'Close'
colnames(IBM)[5] <- 'Volume'
colnames(IBM)[6] <- 'Adjusted'
row.names(IBM) <- NULL
IBM <- na.omit(IBM)
IBM_Set <- na.omit(new_data(IBM))


#####Tree
tree = tree(Tendency~.,data = Training_set)
tree.predict = predict(tree,Test_set,type="class")
xtab <- table(tree.predict,Test_set[,1])
confusionMatrix(xtab)

tree.predict = predict(tree,AAPL_Set,type="class")
xtab <- table(tree.predict,AAPL_Set[,1])
confusionMatrix(xtab)

tree.predict = predict(tree,GOOGL_Set,type="class")
xtab <- table(tree.predict,GOOGL_Set[,1])
confusionMatrix(xtab)

tree.predict = predict(tree,GOOG_Set,type="class")
xtab <- table(tree.predict,GOOG_Set[,1])
confusionMatrix(xtab)

tree.predict = predict(tree,MSFT_Set,type="class")
xtab <- table(tree.predict,MSFT_Set[,1])
confusionMatrix(xtab)

tree.predict = predict(tree,IBM_Set,type="class")
xtab <- table(tree.predict,IBM_Set[,1])
confusionMatrix(xtab)

#####Random Forest
rf <- randomForest(Tendency~., data=Training_set,mtry=6, importance=TRUE)

rf.predict = predict(rf,newdata=Test_set)
Important.predictors <- data.frame(importance(rf))
Important.predictors
varImpPlot(rf)
xtab<-table(rf.predict, Test_set[,1])
confusionMatrix(xtab)


rf.predict = predict(rf,newdata=AAPL_Set)
xtab<-table(rf.predict, AAPL_Set[,1])
confusionMatrix(xtab)

rf.predict = predict(rf,newdata=GOOGL_Set)
xtab<-table(rf.predict, GOOGL_Set[,1])
confusionMatrix(xtab)

rf.predict = predict(rf,newdata=GOOG_Set)
xtab<-table(rf.predict, GOOG_Set[,1])
confusionMatrix(xtab)

rf.predict = predict(rf,newdata=MSFT_Set)
xtab<-table(rf.predict, MSFT_Set[,1])
confusionMatrix(xtab)

rf.predict = predict(rf,newdata=IBM_Set)
xtab<-table(rf.predict, IBM_Set[,1])
confusionMatrix(xtab)

#####SVM-Support Vector Classifier
set.seed(1)
svm <- svm(Tendency~., data = Training_set,kernel ="radial", gamma =0.5, cost =2)

svm.predict <- predict(svm, newdata=Test_set)
xtab<-table(svm.predict, Test_set[,1])
confusionMatrix(xtab)

tune <- tune.svm(Tendency~., data=Training_set, gamma = 2^(-1:1), cost = 2^(1:3))
bestmod =tune$best.model
summary (bestmod)
###bestmod
#SVM-Kernel:  radial 
#cost:  2 
#gamma:  0.5 
###
svm.predict <- predict(bestmod, newdata=Test_set)
xtab<-table(svm.predict, Test_set[,1])
confusionMatrix(xtab)


svm.predict <- predict(svm, newdata=AAPL_Set)
xtab<-table(svm.predict, AAPL_Set[,1])
confusionMatrix(xtab)

svm.predict <- predict(svm, newdata=GOOGL_Set)
xtab<-table(svm.predict, GOOGL_Set[,1])
confusionMatrix(xtab)

svm.predict <- predict(svm, newdata=GOOG_Set)
xtab<-table(svm.predict, GOOG_Set[,1])
confusionMatrix(xtab)

svm.predict <- predict(svm, newdata=MSFT_Set)
xtab<-table(svm.predict, MSFT_Set[,1])
confusionMatrix(xtab)

svm.predict <- predict(svm, newdata=IBM_Set)
xtab<-table(svm.predict, IBM_Set[,1])
confusionMatrix(xtab)


#####Neural Network
maxs <- apply(new_data, 2, max) 
mins <- apply(new_data, 2, min)

scaled_data <- as.data.frame(scale(new_data, scale = maxs - mins))
nn_Training_set <- scaled_data[1:8000,]
nn_Test_set <- scaled_data[8001:10370,]

n <- names(nn_Training_set)
f <- as.formula(paste("Tendency ~", paste(n[!n %in% "Tendency"], collapse = " + ")))

nn <- neuralnet(f, data = nn_Training_set, hidden=c(3,3),stepmax=1e6, threshold=0.5, linear.output = FALSE)
plot(nn)
pr.nn <- compute(nn,nn_Test_set[,1])

pr.nn_ <- pr.nn$net.result*(max(nn_Training_set$Tendency)-min(nn_Training_set$Tendency))+min(nn_Training_set$Tendency)
test.r <- (nn_Test_set$Tendency)*(max(nn_Training_set$Tendency)-min(nn_Training_set$Tendency))+min(nn_Training_set$Tendency)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(nn_Test_set)

print(paste(MSE.lm,MSE.nn))

