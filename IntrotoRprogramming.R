#Problem1(a)
x<-c(3,12,6,-5,0,8,15,1,-10,7)
x
#Problem1(b)
y<-seq(min(x),max(x),length.out=length(x))
y
#Problem1(c)
sum(x,y)
summary(x)
summary(y)
mean(x,y,trim=0)
mad(x,y)
quantile(x,c(0.25,0.5,0.75),type=1)
quantile(y,c(0.25,0.5,0.75),type=1)
mean(x)
mean(y)
sum(x)
sum(y)
sd(x,na.rm=FALSE)
sd(y,na.rm=FALSE)
mad(x)
mad(y)
var(x)
var(y)
#Problem1d
z<-sample(x,7,replace=TRUE)
z
#Problem1e
install.packages("moments")
library(moments)
skewness(x)
kurtosis(x,na.rm=FALSE)
#Problem1d
t.test(x,y)
# the difference in means is not signifcant
#Problem1e
x_sort<-sort(x)
x_sort
?t.test
t.test(x_sort,y,paired=TRUE)
#Problem1f
x[x<0]
#Problem1g
x_new<-x[x>0]
x_new
#Problem2a
college<-read.csv(file="college.csv", header=TRUE,sep=",")
college
rownames(college)<-college[,1]
View(college)
college<-college[,-1]
View(college)
#Problem2b
#summary numerical variable
summary(college[,c(2:18)])
?pairs
pairs(college[,c(1:10)])

outstate<-college$Outstate
private<-college$Private
answer<-c("Yes","No")
?boxplot
boxplot(outstate~private,main="Out-of-state tuition versus Private",ylab="Out-of-State Tuition",col=c("blue","red"))

?rep
#make varaible Elite replicate elements "NO" with numbers of row same as dataframe college
Elite <-rep("No",nrow(college))
#for each elements in Elite, if any row has the corresponding row in dataframe college having proportion of students coming from top 10% of their high school classes exceeds 50%
#the assing value Yes 
Elite[college$Top10perc>50]<-"Yes"
#encode vector Elite as a factor
Elite<-as.factor(Elite)
#add variable Elite to dataframe college
college<-data.frame(college,Elite)
summary(college$Elite)
boxplot(outstate~Elite,main="Out-of-state tuition versus Elite",ylab="Out-of-State Tuition",col=c("blue","red"))
#hist() function
?hist()
par(mfrow = c(2, 2))
hist(college$Accept,main = paste("Histogram of Number of applicant accepted"),xlab="Number of applicant accepted")
hist(college$Enroll,main = paste("Histogram of Number of applicant enrolled"),xlab="Number of applicant enrolled")
hist(college$Top10perc,main = paste("Histogram of new students from top 10"),xlab="Number of new students from top 10")
hist(college$Top25perc ,main = paste("Histogram of new students from top 25"),xlab="Number of new students from top 25")
par(mfrow = c(2, 2))
hist(college$F.Undergrad,main = paste("Histogram of Number of full-time undergraduates"),xlab="Number of full-time undergraduates")
hist(college$P.Undergrad,main = paste("Histogram of Number of part-time undergraduates"),xlab="Number of part-time undergraduates")
hist(college$Outstate,main = paste("Histogram of Out-of-state tuition"),xlab="Number of Out-of-state tuition")
hist(college$Room.Board ,main = paste("Histogram of room and board costs"),xlab="Number of  room and board costs")
par(mfrow = c(2, 2))
hist(college$Books,main = paste("Histogram of Estimated book costs"),xlab=" Estimated book costs")
hist(college$Personal,main = paste("Histogram of Estimated personal spending"),xlab="Estimated personal spending")
hist(college$PhD,main = paste("Histogram of Percents of faculty with PhD"),xlab="Percents of faculty with PhD")
hist(college$S.F.Ratio ,main = paste("Histogram of student/faculty ratio"),xlab="student/faculty ratio")
par(mfrow = c(2, 2))
hist(college$perc.alumni,main = paste("Histogram of Percent of alumni who donate"),xlab=" Percent of alumni who donate")
hist(college$Expend,main = paste("Histogram of Instructional expenditure per student"),xlab="Instructional expenditure per student")
hist(college$Grad.Rate,main  = paste("Histogram of Graduation rate"),xlab="Graduation rate")
#Problem3
#load package plyr
install.packages("plyr")
library(plyr)
??baseball
View(baseball)
#assign sf ->0
baseball$sf[baseball$year<1954]<-0
#set missing hbp ->0
is.na(baseball$hbp)
baseball$hbp[is.na(baseball$hbp)]<-0
nrow(baseball)
#exclude ab<50
baseball<-baseball[!(baseball$ab<50),]
#compute on base percentage
baseball$obp<-(baseball$h+baseball$bb+baseball$hbp)/(baseball$ab+baseball$bb+baseball$hbp+baseball$sf)
#sort data, print year, player name, obp for top
baseball_new<-baseball[order(-baseball$obp),]
?head
?baseball
baseball_new1<-head(baseball_new,5)
?ncolum
ncol(baseball_new1)
baseball_new1[,c(1,2,23)]
#Problem4
?aggregate
#load quakes data from datasets package
install.packages("datasets")
library(datasets)
View(quakes)
#plot 
?plot
par(mfrow = c(1, 1))
plot(quakes$mag,quakes$depth,xlab="earthquakes magnitude",ylab="depth")
quakeAvgDepth<-aggregate(quakes$depth,by=list(magnitude=quakes$mag),mean)
?aggregate      
quakeAvgDepth
#rename
names(quakeAvgDepth)<-c("Maginitude","AverageDepth")
#Plot
plot(quakeAvgDepth$Maginitude,quakeAvgDepth$AverageDepth,xlab="earthquakes magnitude",ylab="average depth",main="AverageDepth")

