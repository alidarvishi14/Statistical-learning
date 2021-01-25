#library(readr)
college = read.csv("Library/Mobile Documents/com~apple~CloudDocs/SUT/98-2/statistical learning/data/College.csv")
View(college)

rownames(college)=college[,1]
college=college[,-1]
View(college)

summary(college)
pairs(college[,1:10])
plot(college$Private,college$Outstate)

Elite=rep("No",nrow(college))
Elite[college$Top10perc >50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college ,Elite)

summary(college$Elite)
plot(college$Elite,college$Outstate)

par(mfrow=c(2,2))
hist(college$Grad.Rate)
hist(college$perc.alumni)
plot(college$Apps,college$Accept)
plot(college$Private,college$Books)
par(mfrow=c(1,1))
