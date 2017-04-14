d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu",
                    "Mjob","Fjob","reason","nursery","internet"))
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
