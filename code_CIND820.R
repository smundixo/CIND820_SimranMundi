data <- read.csv("Reported Crimes .csv")

#Data analysis

#total number of n/a datapoints
sum(is.na(data))

#first few data rows
head(data)

#get statistical summary figures of all columns.
summary(data)

#get total number of rows in dataset.
nrow(data)

#get total number of columns in dataset.
ncol(data)

#get names of columns of dataset.
columns<-colnames(data, do.NULL = TRUE, prefix = "col") 
columns

#visualise simple crime count trend and crime cleared with years. 
plot(data$ReportedYear, data$Count_)
plot(data$ReportedYear, data$CountCleared)



MODEL<- lm(formula =data$Count_~ data$ReportedYear, data=data)
summary(MODEL)

#k mean clustering
library(useful)
set.seed(20)
kmeans.result <- kmeans( data[,7], 5)
summary(kmeans.result)
kmeans.result
#plot(kmeans.result,data$Count_)

# Is there different GeoDivisions within the city that are more prone to 
# crime and if so is there any trends in the type of 
# crime categories being seen in those parts?
#D51 D52 HAVE TOO MANY CRIMES and property crimes are max in both

library(dplyr)
result  <- aggregate(data$Count_ ~ data$GeoDivision, data, sum)

colnames(result) <- c("GeoDivision","Count_")
head(result)
plot(factor(result$GeoDivision),result$Count_)


divisions <- unique(data$GeoDivision)


for(i in 1:length(divisions))
{
  GeoDivison_s <- data[which(data$GeoDivision==divisions[i]),]
  
  result  <- aggregate(GeoDivison_s$Count_ ~ GeoDivison_s$Category, GeoDivison_s, sum)
  
  colnames(result) <- c("Category","Count_")
  
  result$Category[result$Category=='Controlled Drugs and Substances Act'] <- 'Drugs'
  result$Category[result$Category=='Crimes Against Property'] <- 'Property'
  result$Category[result$Category=='Crimes Against the Person'] <- 'Person'
  result$Category[result$Category=='Criminal Code Traffic'] <- 'Traffic'
  result$Category[result$Category=='Other Criminal Code Violations'] <- 'Code V'
  result$Category[result$Category=='Other Federal Statute Violations'] <- 'Statute V'
  
  
  #s<-plot(factor(result$Category),result$Count_)
  
  pic_name <- paste0("plot",i,".jpg")
  jpeg(pic_name, width = 800, height = 500)
  plot(factor(result$Category),result$Count_,main=divisions[i])
  dev.off()
  
  
}


# Is there geographical locations that have seen a big increase in crime 
# over the years and/or is season a variable to consider? 


divisions <- unique(data$GeoDivision)
years <- unique(data$ReportedYear)

for(i in 1:length(divisions))
{
  GeoDivison_s <- data[which(data$GeoDivision==divisions[i]),]
  result  <- aggregate(GeoDivison_s$Count_ ~ GeoDivison_s$ReportedYear, GeoDivison_s, sum)
  head(result)
  colnames(result) <- c("Year","Count_")
  
  
  
  pic_name <- paste0("plot_Q2",i,".jpg")
  jpeg(pic_name, width = 800, height = 500)
  plot(factor(result$Year),result$Count_,main=divisions[i])
  dev.off()
  
  
}


#Predicting the future of crimes. 
# Outcome of regression was not good enough. The testing dataset was predicted badly by the model.


#Converting Categorical to Numerical
library(caret)
library(caTools)
library(data.table)
library(mltools)
library(tidyverse)

dataset <- data[c("ReportedYear","GeoDivision","Category","Count_")]
head(dataset)

divisions <- unique(dataset$GeoDivision)
str_sort(divisions,decreasing = FALSE,na_last = TRUE,locale = "en",numeric = FALSE,)

category <- unique(dataset$Category)
str_sort(category,decreasing = FALSE,na_last = TRUE,locale = "en",numeric = FALSE,)

for(i in 1:length(divisions))
{
  dataset$GeoDivision[dataset$GeoDivision==divisions[i]] <- i
}

for(i in 1:length(category))
{
  dataset$Category[dataset$Category==category[i]] <- i
}

head(dataset)
dataset

set.seed(2)
split <- sample.split(dataset,SplitRatio=0.7)
train <- subset(dataset,split="TRUE")
test <- subset(dataset,split="FALSE")

model <- lm(train$Count_~train$ReportedYear+train$GeoDivision+train$Category)
summary(model)

pred <- predict(model,test[,1:3])


head(test,20)
head(pred,20)


dataset$ReportedYear<-as.numeric(dataset$ReportedYear)
dataset$GeoDivision <- as.numeric(dataset$GeoDivision)
dataset$Category <- as.numeric(dataset$Category)
dataset$Count_<- as.numeric(dataset$Count_)


#correlation between columns is too low. Means this is the reason of low accuracy of regression.
cor(dataset)

