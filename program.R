#for reading data
library(readr)
suicide_data <- read_csv("r-internship/suicidedata.csv") 
View(suicidedata) 
#for dropping columns that are not needed in our analysis
suicide_newer<- suicidedata[,c(4,6,11,7)]
View(suicide_newer)
#for finding the number of missing values
summary(suicide_newer)
#for cleaning the data by replacing the missing values by the median of all the values in that column
suicide_newer$avg_age[is.na(suicide_newer$avg_age)]<-median(na.omit((
  suicide_newer$avg_age)))



#for drawing the box plots
boxplot(suicide_newer$avg_age,main = "distribution of ages", xlab = "age group", col = "orange", border = "brown", horizontal = TRUE, notch = TRUE)
boxplot(suicide_newer$gdp,main = "distribution of gdp", xlab = "gdp", 
        col = "orange",border = "brown", horizontal = TRUE, notch = TRUE)
boxplot(suicide_newer$population,main = "distribution of population", xlab = "population", col = "orange", border = "brown", horizontal = TRUE, notch = TRUE)
#for drawing the scatter plots
plot(suicide_newer$suicides100k~suicide_newer$avg_age, xlab="avg_age", ylab="suicide per 100k")
plot(suicide_newer$suicides100k~suicide_newer$population, xlab="population", ylab="suicide per 100k")
plot(suicide_newer$suicides100k~suicide_newer$gdp, xlab="gdp", ylab="suicide per 100k")
#for splitting the data for training and testing
n=nrow(suicide_newer)
trainIndex=sample(1:n,size = round(0.7*n),replace = FALSE)
train=suicide_newer[trainIndex, ]
test=suicide_newer[-trainIndex, ]

#making regression model

#Using avg_age as predictor variable
linmod1=lm(suicide_newer$suicides100k~suicide_newer$avg_age,suicide_newer=train)
summary(linmod1)
#Using gdp as predictor variable
linmod2=lm(suicide_newer$suicides100k~suicide_newer$gdp,suicide_newer=train)
summary(linmod2)
#Using population as predictor variable
linmod3=lm(suicide_newer$suicides100k~suicide_newer$population,suicide_newer=train)
summary(linmod3)







#for testing the linear regression model

#Using avg_age
new_data_test1=data.frame(suicideper100k=test$suicides100k,avg_age=test$avg_age)
pred1=predict(linmod1,new_data_test1)
View(pred1)
#Using gdp
new_data_test2=data.frame(suicideper100k=test$suicides100k,gdpcapita=test$gdp)
pred2=predict(linmod2,new_data_test2)
View(pred2)
#Using population
new_data_test3=data.frame(suicideper100k=test$suicides100k,population=test$population)
pred3=predict(linmod3,new_data_test3)
View(pred3)

#for finding the root mean squared error

#Using avg_age
rmse1=sqrt(sum((pred1-test$suicides100k)^2)/length(test$avg_age))
print(rmse1)
#Using gdp
rmse2=sqrt(sum((pred2-test$suicides100k)^2)/length(test$gdp))
print(rmse2)
#Using population
rmse3=sqrt(sum((pred3-test$suicides100k)^2)/length(test$population))
print(rmse3)

