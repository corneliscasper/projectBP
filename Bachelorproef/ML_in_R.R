library(datasets)
library(ggplot2)
library(lattice)
library(caret)
head(iris)
summary(iris) # Describe
plot(iris)
str(iris) #Variables(columns),Objects(rows)...

iris[1,] #Extract 1st row
iris[,2] #Extract second column

#FILTERING DATA FRAME

setosa=subset(iris,subset = iris["Species"]=="setosa") # Creating subset on condition

#SORTING VALUES IN DATA FRAME

sorted.Sepal.Width <- order(iris["Sepal.Width"]) #Sorting values of column Sepal.Width ASC
iris[sorted.Sepal.Width,]

sorted.Sepal.Width <- order(-iris$Sepal.Width) #Sorting values of column Sepal.Width DESC
iris[sorted.Sepal.Width,]

#cbind(df1,df2) Merging columnwise
#rbind(df1,df2) Merging rowwise

#aggregate(df$column,df$column,sum,na.rm=T)
aggregate(iris$Sepal.Length,list(iris$Species),mean,na.rm=T)
aggregate(iris$Sepal.Width,list(iris$Species),mean,na.rm=T)

#HELP
?hist

iris <- read.csv("/Users/casper/Desktop/Bachelorproef/iris.csv")
iris
summary(iris) #We kunnen zien dat er geen uitschieters aanwezig zijn in de dataset.
plot(iris)
iris <- na.omit(iris) #REMOVE NA'S
#Analyze
str(iris)

#OneHotEncoding
dummy <- dummyVars(" ~ .", data=iris)
iris <- data.frame(predict(dummy, newdata=iris))

#Training & test set
dt = sort(sample(nrow(iris), nrow(iris)*.7))
train<-iris[dt,]
test<-iris[-dt,]

train$species.setosa <- as.factor(train$species.setosa) # Tell R species is categorical so convert them to factors
train$species.versicolor <- as.factor(train$species.versicolor) # Tell R species is categorical so convert them to factors
train$species.virginica <- as.factor(train$species.virginica) # Tell R species is categorical so convert them to factors
#Logistic regression
myModel_versicolor <- glm(species.versicolor ~ train$sepal_length + train$sepal_width+ train$petal_length+ train$petal_width,family = binomial,data=train)
myModel_setosa <- glm(species.setosa ~ train$sepal_length + train$sepal_width+ train$petal_length+ train$petal_width,family = binomial,data=train)
myModel_virginica<- glm(species.virginica ~ train$sepal_length + train$sepal_width+ train$petal_length+ train$petal_width,family = binomial,data=train)

# creating the model
res <- predict(myModel_versicolor,test,type="response")
res <- predict(myModel_versicolor,train,type="response")

#Confusion matrix
confmatrix <- table(Actual_value=train$species.versicolor, Predicted_value= res >0.5)
confmatrix
#Accuracy
acc1<-(confmatrix[[1,1]]+confmatrix[[2,2]])/ sum(confmatrix)

res <- predict(myModel_setosa,test,type="response")
res <- predict(myModel_setosa,train,type="response")

confmatrix <- table(Actual_value=train$species.setosa, Predicted_value= res >0.5)
confmatrix
#Accuracy
acc2<-(confmatrix[[1,1]]+confmatrix[[2,2]])/ sum(confmatrix)

res <- predict(myModel_virginica,test,type="response")
res <- predict(myModel_virginica,train,type="response")

confmatrix <- table(Actual_value=train$species.virginica, Predicted_value= res >0.5)
confmatrix
#Accuracy
acc3<-(confmatrix[[1,1]]+confmatrix[[2,2]])/ sum(confmatrix)

acc=(acc1+acc2+acc3)/3
acc


