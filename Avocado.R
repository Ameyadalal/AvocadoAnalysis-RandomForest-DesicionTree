Avo <- read.csv('avocado.csv')
head(Avo)
str(Avo)
any(is.na(Avo))
print(str(Avo))
print(summary(Avo))
levels(Avo$type)
Avo$year <-as.numeric(Avo$year)


## Exploratory Data Analysis ###
library(plotrix)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(quantmod)
library(corrgram)
library(corrplot)
library(Amelia)

# checking the missing values 
missmap(Avo,y.at=c(1),y.labels = c(''),col=c('red','peachpuff'))

## Use label encoder for region column to convert it into numerical data
Avo$region <- as.numeric(Avo$region)

# count of avocados 
slices <- c(1,2)
label <- c("Conventional", "Organic")
pie3D(slices,labels=label,explode=0.1,
      main="Pie Chart of Avocado types ")

## Dropping the column Unnamed as it has only index values.
Avo <- subset(Avo, select = -1)

# Q-Q plot
qqnorm(Avo$AveragePrice, pch = 1, frame = FALSE)
qqline(Avo$TotalVolume, col = "black", lwd = 2)

# Correlation plot
num.cols <- sapply(Avo, is.numeric)
print(num.cols)
corr <- cor(Avo[,num.cols])
print(corr)
corrplot(corr, method='color')

# Density of avocado types
options(repr.plot.width=7, repr.plot.height=3)
ggplot(Avo, aes(x=AveragePrice, fill=type)) + geom_density() + facet_wrap(~type) + theme_minimal() + theme(plot.title=element_text(hjust=0.5), legend.position="bottom") + labs(title="Avocado Price by Type") + scale_fill_brewer(palette="Set2")

# Type of avocado vs Average price
pl <- ggplot(Avo,aes(x=factor(type),y=AveragePrice))
print(pl + geom_boxplot(aes(fill=factor(type)))+ theme_bw())

# average price distribution
x <- Avo$AveragePrice
h<-hist(x, col="peachpuff", xlab="Average Price",
        main="Average Price of Avocado types")
xaxisfit<-seq(min(x),max(x),length=40)
yaxisfit<-dnorm(xaxisfit,mean=mean(x),sd=sd(x))
yaxisfit <- yaxisfit*diff(h$mids[1:2])*length(x)
lines(xaxisfit, yaxisfit, col="black")

# Average price varies vs years
plot <- ggplot(Avo,aes(x=factor(year),y=AveragePrice))
print(plot + geom_boxplot(aes(fill=factor(year)))+ theme_bw())

## Change the date column from factor to date
Avo$Date <- as.Date(Avo$Date, "%Y-%m-%d")
class(Avo$Date)

# Sorting the dates
Avo <- Avo[order(as.Date(Avo$Date, format="%Y-%m-%d")),]

price_diff <- Avo %>% select(Date, AveragePrice, type) %>%
  ggplot(aes(x=Date, y=AveragePrice)) + geom_area(aes(color=type, fill=type), alpha = 0.4, position = position_dodge(0.7)) + 
  theme_minimal() +  scale_color_manual(values = c("green", "orange")) + scale_fill_manual(values = c("green", "orange"))
price_diff

## Displaying price of each type
ggplot(data = Avo, aes(x = Date, y = AveragePrice, col=type)) +geom_line() + facet_wrap(~ type) + theme(legend.position="bottom")

## building out our model
library(caTools)
str(Avo)

# SPLIT Train and Test 
set.seed(101)
sample <- sample.split(Avo$type,SplitRatio = 0.70)
train <- subset(Avo,sample== TRUE)
test <- subset(Avo,sample == FALSE)

# Decision Tree 
library(rpart)
tree <- rpart(type ~ .,method = 'class',data = train)
summary(tree)

## Predict and confusion matrix 
test$tree.pred <- predict(tree,newdata =test,type = 'class')
tab <- table(test$type,test$tree.pred) 
print(tab)
library(rpart)
library(rpart.plot) 
prp(tree)

# accuracy of the model 
(2659+2594)/(2659+79+143+2594)

#Specificity
# (TN/(TN+FP))
(2594)/(2594+143)

#recall and sentivity
(2659)/(2659+79)

#precision 
(2659) / (2659 + 143)

#missclassification 
(79 + 143)/(2659+143+79+2594)

##### RANDOM FOREST ###### 
library(caTools)
set.seed(101)
sample <- sample.split(Avo$type,SplitRatio = 0.80)
train <- subset(Avo,sample== TRUE)
test <- subset(Avo,sample == FALSE)

## Modelling the data
library(randomForest)
str(Avo)
rf.model <- randomForest(type ~., data = train,importance= TRUE)
rf.model$confusion
rf.model$importance

## Predictions 
rf.preds <- predict(rf.model,test)
table(rf.preds,test$type)

# accuracy of the model 
(1821+1816)/(1821+9+4+1816)

#Specificity
(1816)/(1816+9)

#recall or sensitivity
(1821)/(1821+9)

#precision 
(1821)/(1821+4)

#missclassification 
(4+9)/(1821+4+9+1816)
