library(ggplot2)
library(dplyr)
library(caret)
library(fscaret)
library(plyr)
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
d4=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus",
                    "Medu","Fedu","Mjob","Fjob","reason",
                    "guardian","traveltime","studytime","failures",
                    "schoolsup","famsup","activities","nursery","higher","internet","romantic",
                    "famrel","freetime","goout","Dalc","Walc","health","absences"))
print(nrow(d4)) # 85 students
str(d4)
#multivariate analysis
#average math and port scores
d4$meanMath <- rowMeans(subset(d4, select = c(G1.x, G2.x,G3.x)), na.rm = TRUE)
d4$meanPort <- rowMeans(subset(d4, select = c(G1.y, G2.y,G3.y)), na.rm = TRUE)
#plot(d4$meanMath,d4$meanPort, col=d4$Walc)
d3<-rbind(d1,d2) #combine the two datasets
# and eliminate the repeats:
df.merged<-d3 %>% distinct(school,sex,age,address,famsize,Pstatus,
                             Medu,Fedu,Mjob,Fjob,reason,
                             guardian,traveltime,studytime,failures,
                             schoolsup, famsup,activities,nursery,higher,internet,
                             romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)
#add a column with average passs (math or Portuguese, whichever is available)
df.merged$avgpasss=rowMeans(cbind(df.merged$G1,df.merged$G2,df.merged$G3))
df.merged<-df.merged[,-31:-32]
str(df.merged)
# and drop passs in 3 marking periods.
#df.merged<-df.merged[,-(31:33)]
# passs vs Weekly alcohol
str(df.merged$Medu)
df.merged$pass<- ifelse(df.merged$G3>=9,1,0)
# fix all the yes and nos as they are reserved words
df.merged$activities<-as.character(df.merged$activities)
df.merged$romantic<-as.character(df.merged$romantic)
df.merged$internet<-as.character(df.merged$internet)
df.merged$higher<-as.character(df.merged$higher)
df.merged$nursery<-as.character(df.merged$nursery)
df.merged$famsup<-as.character(df.merged$famsup)
df.merged$schoolsup<-as.character(df.merged$schoolsup)
df.merged$activities<-ifelse(df.merged$activities=="no","N","Y")
df.merged$romantic<-ifelse(df.merged$romantic=="no","N","Y")
df.merged$internet<-ifelse(df.merged$internet=="no","N","Y")
df.merged$higher<-ifelse(df.merged$higher=="no","N","Y")
df.merged$nursery<-ifelse(df.merged$nursery=="no","N","Y")
df.merged$paid<-ifelse(df.merged$paid=="no","N","Y")
df.merged$famsup<-ifelse(df.merged$famsup=="no","N","Y")
df.merged$schoolsup<-ifelse(df.merged$schoolsup=="no","N","Y")
df.merged$activities<-as.factor(df.merged$activities)
df.merged$romantic<-as.factor(df.merged$romantic)
df.merged$internet<-as.factor(df.merged$internet)
df.merged$higher<-as.factor(df.merged$higher)
df.merged$nursery<-as.factor(df.merged$nursery)
df.merged$famsup<-as.factor(df.merged$famsup)
df.merged$schoolsup<-as.factor(df.merged$schoolsup)
df.merged$paid<-as.factor(df.merged$paid)
##
df.merged$reason<-as.character(df.merged$reason)
df.merged$reason[df.merged$reason == "home"] <- "athome"
df.merged$reason<-as.factor(df.merged$reason)
df.merged$reason<-as.character(df.merged$reason)
df.merged$reason[df.merged$reason == "home"] <- "athome"
df.merged$Mjob<-as.factor(df.merged$Mjob)
df.merged$Mjob<-as.character(df.merged$Mjob)
df.merged$Mjob[df.merged$Mjob == "at_home"] <- "stayhome"
df.merged$Mjob<-as.factor(df.merged$Mjob)
df.merged$Fjob<-as.character(df.merged$Fjob)
df.merged$Fjob[df.merged$Fjob == "at_home"] <- "stayhome"
df.merged$Fjob<-as.factor(df.merged$Fjob)
## Medu
df.merged$Medu[df.merged$Medu == "0"] <- "No-Grade"
df.merged$Medu[df.merged$Medu == "1"] <- "forththPass"
df.merged$Medu[df.merged$Medu == "2"] <- "fifth-9th-Grade"
df.merged$Medu[df.merged$Medu == "3"] <- "Secondary-Education"
df.merged$Medu[df.merged$Medu == "4"] <- "Higher-Education"
df.merged$Medu<-as.factor(df.merged$Medu)
# Fedu
df.merged$Fedu[df.merged$Fedu == "0"] <- "No-Grade"
df.merged$Fedu[df.merged$Fedu == "1"] <- "forththPass"
df.merged$Fedu[df.merged$Fedu == "2"] <- "fifth-9th-Grade"
df.merged$Fedu[df.merged$Fedu == "3"] <- "Secondary-Education"
df.merged$Fedu[df.merged$Fedu == "4"] <- "Higher-Education"
df.merged$Fedu<-as.factor(df.merged$Fedu)
#recode traveltime
df.merged$traveltime[df.merged$traveltime == "1"] <- "under15mins"
df.merged$traveltime[df.merged$traveltime == "2"] <- "fifteen-30mins"
df.merged$traveltime[df.merged$traveltime == "3"] <- "thirtymin-1hour"
df.merged$traveltime[df.merged$traveltime == "4"] <- "over1hour"
df.merged$traveltime<-as.factor(df.merged$traveltime)
#recode studytime
df.merged$studytime[df.merged$studytime == "1"] <- "under2hours"
df.merged$studytime[df.merged$studytime == "2"] <- "two-5hours"
df.merged$studytime[df.merged$studytime == "3"] <- "thirtymin-1hour"
df.merged$studytime[df.merged$studytime == "4"] <- "five-10hours"
df.merged$studytime<-as.factor(df.merged$studytime)
# central tendency
boxplot(df.merged$G3, main='Final Score Central Tendency')
#spread of outcome variable
prop.table(table(df.merged$pass))
hist(df.merged$G3, main="Final passs Spread", xlab="Final Score")
ggplot(df.merged, aes(x=Walc,y=G3, group=Walc)) +
  geom_boxplot() +
  xlab("Weekly Alcohol") +
  ylab("Final passs")
ggtitle("Weekly Alcohol Consumption vs Final passs")
#weekly alcohol levels vs passing pass
#plot(is.character(df.merged$pass)~df.merged$Walc)
#table(df.merged$age)
boxplot(df.merged$G3~df.merged$age, main='Final Score Variance by Age', xlab="Age")
# school support vs passs
ggplot(df.merged, aes(x=schoolsup, y=G3, group=schoolsup)) +
  geom_boxplot() +
  xlab("School Support") +
  ylab("Final pass")
  ggtitle("School Support vs Final pass")
# passs vs daily alcohol
ggplot(df.merged, aes(x=Dalc, y=G3, group=Dalc)) +
  geom_boxplot()+
  xlab("Daily Alcohol Consumption") +
  ylab("Final pass")
ggtitle("Daily Alcohol Consumption vs Final pass")
#histogram of average passs
# passs vs romance
ggplot(df.merged, aes(x=romantic, y=G3, group=romantic)) +
  geom_boxplot()
# schools vs passs
ggplot(df.merged, aes(x=school, y=G3, group=school)) +
  geom_boxplot()
# ages vs passs
ggplot(df.merged, aes(x=age, y=G3, group=age)) +
  geom_boxplot()
# internet vs passs
ggplot(df.merged, aes(x=internet, y=G3, group=internet)) +
  geom_boxplot()
# absences vs Dalc
ggplot(df.merged, aes(x=Dalc, y=absences, group=Dalc)) +
  geom_boxplot()
#acohol vs age
# univariate analysis of pass
ggplot(df.merged, aes(x=pass)) +
  geom_bar()
str(df.merged)
df.merged$pass <- as.integer(df.merged$pass)
# create dummy data
df.Dummy <- dummyVars("~.",data=df.merged,fullRank=T)
df.schools <- as.data.frame(predict(df.Dummy,df.merged))
#print(names(df.merged))
str(df.schools)
#distribution of outcome variable.names()
prop.table(table(df.schools$pass))
#df.schools$G3=as.factor((df.schools$G3))
hist(df.schools$G3)
str(df.schools)
cor(df.schools)
# correlations
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}


corMasterList <- flattenSquareMatrix (cor.prob(df.schools))
print(head(corMasterList,20))

corList <- corMasterList[order(-abs(corMasterList$cor)),]
print(head(corList,60))

selectedSub <- subset(corList, (abs(cor) > 0.10 & j == 'G3'))
print(selectedSub)
#str(df.schools)
#sort out outcome variable
outcomeName <- 'pass'
predictorsNames <- names(df.schools)[names(df.schools) != outcomeName]
#classification
df.schools$pass <- as.factor(ifelse(df.schools$pass==1,'P','F'))
#split data into test and training
set.seed(1234)
splitIndex <- createDataPartition(df.schools[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- df.schools[ splitIndex,]
testDF  <- df.schools[-splitIndex,]
#
# run fscaret ensemble
myFS.class <-fscaret(trainDF, testDF, myTimeLimit = 20,
                     preprocessData=TRUE, with.labels=TRUE,
                     classPred=TRUE,
                     regPred=FALSE,
                     Used.funcClassPred=c("gbm","glmnet","rf"),
                     supress.output=FALSE, no.cores=NULL,
                     saveModel=FALSE)
##
#
# #validation harness
# objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
# #train model
# objModel <- train(trainDF[,predictorsNames], as.factor(trainDF$pass), 
#                   method='gbm', 
#                   trControl=objControl,  
#                   metric = "ROC",
#                   preProc = c("center", "scale"))
# #view variable importance
# summary(objModel)
# #?make.names
# #str(df.schools)
results <-  myFS.class$VarImp$matrixVarImp.MeasureError
results$Input_no <- as.numeric(results$Input_no)
results <- results[,setdiff(names(results), c('SUM%','ImpGrad'))]
myFS.class$PPlabels$Input_no <-  as.numeric(rownames(myFS.class$PPlabels))
results <- merge(x=results, y=myFS.class$PPlabels, by="Input_no", all.x=T)
results <- results[order(-results$SUM),] 
print(head(results),10)






