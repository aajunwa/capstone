library(ggplot2)
library(dplyr)
library(caret)
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
#add a column with average grades (math or Portuguese, whichever is available)
df.merged$avggrades=rowMeans(cbind(df.merged$G1,df.merged$G2,df.merged$G3))
df.merged<-df.merged[,-31:-32]
str(df.merged)
# and drop grades in 3 marking periods.
#df.merged<-df.merged[,-(31:33)]
# grades vs Weekly alcohol
str(df.merged$Medu)
df.merged$grade<- ifelse(df.merged$G3>=9,1,0)

df.merged$Medu[df.merged$Medu == "0"] <- "None"
df.merged$Medu[df.merged$Medu == "1"] <- "4th grade"
df.merged$Medu[df.merged$Medu == "2"] <- "5th to 9th grade"
df.merged$Medu[df.merged$Medu == "3"] <- "Secondary Education"
df.merged$Medu[df.merged$Medu == "4"] <- "Higher Education"
df.merged$Medu<-as.factor(df.merged$Medu)
#recode traveltime
df.merged$traveltime[df.merged$traveltime == "1"] <- "<15 min"
df.merged$traveltime[df.merged$traveltime == "2"] <- "15-30 mins"
df.merged$traveltime[df.merged$traveltime == "3"] <- "30min-1hour"
df.merged$traveltime[df.merged$traveltime == "4"] <- ">1 hour"
df.merged$traveltime<-as.factor(df.merged$traveltime)
#recode studytime
df.merged$studytime[df.merged$studytime == "1"] <- "<2hours"
df.merged$studytime[df.merged$studytime == "2"] <- "2-5hours"
df.merged$studytime[df.merged$studytime == "3"] <- "30min-1hour"
df.merged$studytime[df.merged$studytime == "4"] <- "5-10hours"
df.merged$studytime<-as.factor(df.merged$studytime)
# central tendency
boxplot(df.merged$G3, main='Final Score Central Tendency')
#spread of outcome variable
prop.table(table(df.merged$grade))
hist(df.merged$G3, main="Final Grades Spread", xlab="Final Score")
ggplot(df.merged, aes(x=Walc,y=G3, group=Walc)) +
  geom_boxplot() +
  xlab("Weekly Alcohol") +
  ylab("Final Grades")
ggtitle("Weekly Alcohol Consumption vs Final Grades")
#weekly alcohol levels vs passing grade
#plot(is.character(df.merged$grade)~df.merged$Walc)
#table(df.merged$age)
boxplot(df.merged$G3~df.merged$age, main='Final Score Variance by Age', xlab="Age")
# school support vs grades
ggplot(df.merged, aes(x=schoolsup, y=G3, group=schoolsup)) +
  geom_boxplot() +
  xlab("School Support") +
  ylab("Final Grade")
  ggtitle("School Support vs Final Grade")
# grades vs daily alcohol
ggplot(df.merged, aes(x=Dalc, y=G3, group=Dalc)) +
  geom_boxplot()+
  xlab("Daily Alcohol Consumption") +
  ylab("Final Grade")
ggtitle("Daily Alcohol Consumption vs Final Grade")
#histogram of average grades
# grades vs romance
ggplot(df.merged, aes(x=romantic, y=G3, group=romantic)) +
  geom_boxplot()
# schools vs grades
ggplot(df.merged, aes(x=school, y=G3, group=school)) +
  geom_boxplot()
# ages vs grades
ggplot(df.merged, aes(x=age, y=G3, group=age)) +
  geom_boxplot()
# internet vs grades
ggplot(df.merged, aes(x=internet, y=G3, group=internet)) +
  geom_boxplot()
# absences vs Dalc
ggplot(df.merged, aes(x=Dalc, y=absences, group=Dalc)) +
  geom_boxplot()
#acohol vs age
# univariate analysis of Grade
ggplot(df.merged, aes(x=grade)) +
  geom_bar()
str(df.merged)
df.merged$grade <- as.integer(df.merged$grade)
# create dummy data
df.Dummy <- dummyVars("~.",data=df.merged,fullRank=T)
df.schools <- as.data.frame(predict(df.Dummy,df.merged))
#print(names(df.merged))
str(df.schools)
#distribution of outcome variable.names()
prop.table(table(df.schools$grade))
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
#
#
#
#sort out outcome variable
outcomeName <- 'grade'
predictorsNames <- names(df.schools)[names(df.schools) != outcomeName]
#classification
df.schools$grade2 <- ifelse(df.schools$grade=="1",'pass','fail')
df.schools$grade2 <- as.factor(df.schools$grade)
outcomeName <- 'grade2'
#split data into test and training
set.seed(1234)
splitIndex <- createDataPartition(df.schools[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- df.schools[ splitIndex,]
testDF  <- df.schools[-splitIndex,]
#validation harness
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
#train model
objModel <- train(df.schools[,predictorsNames], df.schools[,outcomeName], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))
#view variable importance
summary(objModel)







