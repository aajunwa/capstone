library(ggplot2)
library(dplyr)
library(caret)
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
d4=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus",
                    "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
                    "guardian","traveltime","studytime","failures",
                    "schoolsup","famsup","activities","higher","romantic",
                    "famrel","freetime","goout","Dalc","Walc","health","absences"))
print(nrow(d4)) # 85 students
str(d4)
#multivariate analysis
#average math and port scores
d4$meanMath <- rowMeans(subset(d4, select = c(G1.x, G2.x,G3.x)), na.rm = TRUE)
d4$meanPort <- rowMeans(subset(d4, select = c(G1.y, G2.y,G3.y)), na.rm = TRUE)
plot(d4$meanMath,d4$meanPort, col=d4$Walc)
library(ggplot2)
d3<-rbind(d1,d2) #combine the two datasets
# and eliminate the repeats:
df.merged<-d3 %>% distinct(school,sex,age,address,famsize,Pstatus,
                             Medu,Fedu,Mjob,Fjob,reason,
                             guardian,traveltime,studytime,failures,
                             schoolsup, famsup,activities,nursery,higher,internet,
                             romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)
#add a column with average grades (math or Portuguese, whichever is available)
df.merged$avggrades=rowMeans(cbind(df.merged$G1,df.merged$G2,df.merged$G3))
# and drop grades in 3 marking periods.
#df.merged<-df.merged[,-(31:33)]
# grades vs Weekly alcohol
df.merged$grade<- ifelse(df.merged$G3>=9,1,0)
str(df.merged)
summary(df.merged)
table(df.merged$grade)
boxplot(df.merged$G3, main='Final Score Central Tendency')
df.merged<-df.merged[,-31:-32]
# create pass/fail grade
#df.merged$grade<-as.factor(df.merged$grade)
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
print(head(corList,10))

selectedSub <- subset(corList, (abs(cor) > 0.15 & j == 'grade'))
print(selectedSub)


