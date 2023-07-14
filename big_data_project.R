#install.packages("tidyverse")
library("tidyverse")
hr_data<-read.csv("aug_train.csv",na.strings = "")
test <- read.csv("aug_test.csv",na.strings = "")

head(hr_data)
summary(hr_data)
str(hr_data)
dim(hr_data)
names(hr_data)
table(is.na(hr_data$enrollee_id))
table(is.na(hr_data$gender))
res<-NULL
for (i in 1:ncol(hr_data)){
  temp<-sum(is.na(hr_data[,i]))
  temp<-as.data.frame(temp)
  temp$var<-colnames(hr_data)[i]
  
  res<-rbind(res,temp)
}
print(res)
head(hr_data)
hr<-hr_data
names(hr)
#Plotting numeric data with histogram
hist(hr$city_development_index,col="blue",main="city_development_index")#left skewed
hist(hr$training_hours,col="red",main="training_hours")#right skewed
#we can transform using log for better visualization
hist(log(hr$training_hours),col="green")#mode between 3 and 4 hours
hist(hr$target,main="target",col=rainbow(2))#data is highly imbalanced
hr_data%>%group_by(target)%>%count()
#visualize the outliers
boxplot(hr$city_development_index,main="city_development_index")
boxplot(hr$training_hours)#outliers at values above 250
#gender
barplot(table(hr_data$gender),col=rainbow(3),main="gender")#data is highly imbalanced
hr_data%>%group_by(gender)%>%count()

#relevent_experience
barplot(table(hr$relevent_experience),main="relavent_experience", col=rainbow(2))
hr_data%>%group_by(relevent_experience)%>%count()
#enrolled_university
barplot(table(hr$enrolled_university),main="enrolled_university",col=rainbow(3))
hr%>%group_by(enrolled_university)%>%count()
#education_level
barplot(table(hr$education_level),main="education_level",col=rainbow(5))
hr%>%group_by(education_level)%>%count()

#major_discipline
barplot(table(hr$major_discipline),main="major_discipline",col=rainbow(6))#stem is the most data
hr%>%group_by(major_discipline)%>%count()

#experience
unique(hr$experience)
#too many values for experience so let us divide it into sections
hr$experience[hr$experience=="<1"|hr$experience=="1"|hr$experience=="2"|hr$experience=="3"|hr$experience=="4"|hr$experience=="5"]<-"0-5"
hr$experience[hr$experience=="6"|hr$experience=="7"|hr$experience=="8"|hr$experience=="9"|hr$experience=="10"]<-"5-10"
hr$experience[hr$experience=="11"|hr$experience=="12"|hr$experience=="13"|hr$experience=="14"|hr$experience=="15"]<-"10-15"
hr$experience[hr$experience=="16"|hr$experience=="17"|hr$experience=="18"|hr$experience=="19"|hr$experience=="20"|hr$experience==">20"]<-"15-20"
barplot(table(hr$experience),col=rainbow(4),main="experience in intervals")#most people have five years of experience

#company_type
barplot(table(hr$company_type),col=rainbow(6))#the most people have pvt ltd
unique(hr$company_type)
hr%>%group_by(company_type)%>%count()

#company size
barplot(table(hr$company_size),col=rainbow(5),main="company_size")
#last new job
barplot(table(hr$last_new_job),col=rainbow(6),main="last_new_job")
hr%>%group_by(last_new_job)%>%count()


#drop column enrollee id and city and relevant experience
hr<-hr[-c(1,2,5)]
names(hr)
hr<-hr[!is.na(hr$major_discipline)|hr$target==1,]
hr$major_discipline[hr$major_discipline!="STEM"&hr$major_discipline!="No Major"]<-"Other"
hr<-hr[!is.na(hr$enrolled_university)|hr$target==1,]
hr<-hr[!is.na(hr$education_level)|hr$target==1,]
hr<-hr[!is.na(hr$last_new_job)|hr$target==1,]
hr<-hr[!is.na(hr$experience)|hr$target==1,]
dim(hr)
hr
#install.packages("missRanger")
library(missRanger)
new_hr<-missRanger(
  hr,
  formula = . ~ .,
  num.trees=100,
  seed=3
)
hr%>%group_by(enrolled_university)%>%count()
new_hr%>%group_by(enrolled_university)%>%count()
hr%>%group_by(company_type)%>%count()
new_hr%>%group_by(company_type)%>%count()
hr%>%group_by(gender)%>%count()
new_hr%>%group_by(gender)%>%count()

#Don't forget to draw heatmap after label encoding
# label encoding
train <- new_hr
#train$city <- as.factor(train$city)
train$gender <- as.numeric(factor(train$gender))
#train$relevent_experience <- as.factor(train$relevent_experience)
train$enrolled_university <- as.numeric(factor(train$enrolled_university))
train$education_level <- as.numeric(factor(train$education_level))
train$major_discipline <- as.numeric(factor(train$major_discipline))
train$experience <- as.numeric(factor(train$experience))
train$company_size <- as.numeric(factor(train$company_size))
train$company_type <- as.numeric(factor(train$company_type))
train$last_new_job <- as.numeric(factor(train$last_new_job))
train$training_hours <- as.integer(train$training_hours)
train$target <- as.factor(train$target)

str(train)
head(train)
