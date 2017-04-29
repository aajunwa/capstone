library(ggplot2)
library(dplyr)
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
ggplot(df.merged, aes(x=Walc,y=avggrades, group=Walc)) +
  geom_boxplot()
table(df.merged$age)
boxplot(df.merged$avggrades~df.merged$age)
# school support vs grades
ggplot(df.merged, aes(x=schoolsup, y=avggrades, group=schoolsup)) +
  geom_boxplot()
# grades vs daily alcohol
ggplot(df.merged, aes(x=Dalc, y=avggrades, group=Dalc)) +
  geom_boxplot()
#histogram of average grades
# grades vs romance
ggplot(df.merged, aes(x=romantic, y=avggrades, group=romantic)) +
  geom_boxplot()
# schools vs grades
ggplot(df.merged, aes(x=school, y=avggrades, group=school)) +
  geom_boxplot()
# ages vs grades
ggplot(df.merged, aes(x=age, y=avggrades, group=age)) +
  geom_boxplot()
# internet vs grades
ggplot(df.merged, aes(x=internet, y=avggrades, group=internet)) +
  geom_boxplot()
# absences vs walc
ggplot(df.merged, aes(x=Dalc, y=absences, group=Dalc)) +
  geom_boxplot()
mosthighlyratedcorrelated?
plot(df.merged$Mjob)
table(df.merged$Mjob)
