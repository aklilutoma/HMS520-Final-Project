# set wroking direcroty
setwd('C:/Users/aketomy/OneDrive - UW/Desktop/files/class/HMS520/HMS520-Final-Project')
rm(list = ls())

# the data is extracted form Kagle website:
# https://www.kaggle.com/c/bike-sharing-demand/data

# VARIABLES
#datetime - hourly date + timestamp
#season - 1 = spring, 2 = summer, 3 = fall, 4 = winter
#holiday - whether the day is considered a holiday
#workingday - whether the day is neither a weekend nor holiday
#weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
      #2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
    #3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
    #4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
#temp - temperature in Celsius
#atemp - temperature in Celsius
#humidity - relative humidity
#windspeed - wind speed
#casual - number of non-registered user rentals initiated
#registered - number of registered user rentals initiated
#count - number of total rentals

# laod the data
bike<-read.csv('bikeshare.csv')
# check the first few rows of the data
head(bike)
# Count is to be predicted uisng linear regression model

# Exploratory Data Analysis -----

library(ggplot2)
#scatter plot for count vs temp
# alpha: adjusts transparency and color-coding the points by temperature
#theme_bw() function sets a black-and-white theme for the plot

ggplot(bike,aes(temp,count)) + geom_point(alpha=0.2,
    aes(color=temp)) + theme_bw()

# time series plot:
# convert the datetime column  into a POSIXct date-time format. 
#  to enable date-time calculations
#POSIXct formatwhich is a common date-time format in R. 
#conversion is helpful for plotting time series data

bike$datetime<-as.POSIXct(bike$datetime)

# scatter plot for count versus datetime with a color gradient based on temperature
#scale_color_continuous() to set a color gradient from low to high temperature values 
# alpha for transparency
ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)+
    scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()

# two things to notice: 
#1. seasonality to the data, for winter and summer
# 2. bike rental counts are increasing in general. 


# Exploring the season data 
# boxplot for count vs season.
#scale_x_discrete(): Modifies the x-axis labels to display the seasons' names
# scale_color_discrete(): Adjusts the legend's title and labels for seasons.
#labs(color="Season"): Renames the legend for clarity.

ggplot(bike, aes(factor(season), count)) +
  geom_boxplot(aes(color=factor(season))) +
  scale_x_discrete(labels=c('Spring', 'Summer', 'Fall', 'Winter')) +
  scale_color_discrete(name="Season", labels=c('Spring', 'Summer', 'Fall', 'Winter')) +
  labs(color="Season") + theme_bw() + xlab('Season')

#The plot indicates more rentals in winter than in spring

# Feature Engineering ----
# we can create an hour column that takes the hour from the datetime column.
# The sapply(): used to apply the format() function to each element of the 'datetime' column. 
#The %H in the format() function specifies that we want to extract the hour component 

bike$hour<-sapply(bike$datetime,function(x){format(x,"%H")})
head(bike)

# We can create a scatterplot of count versus hour

#filter data where workingday==1.
df<-filter(bike, workingday==1)
head(df)
# scatter plot for hour vs count
# position=position_jitter(w=1, h=0): Adds jittered points to the plot,
#so that points are adjusted to avoid overlapping. 
#aes(color=temp): points are color-coded by the 'temp' variable 
#alpha value of 0.5 for transparency.
# scale_color_gradientn(): Sets a custom gradient color scheme for the 'temp' variable,
#ranging from dark blue to red.

ggplot(df,aes(hour,count))+
  geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)+
  scale_color_gradientn(colours = c('dark blue','blue','light blue',
          'light green','yellow','orange','red'))+theme_bw()

# Working days have peak activity during the morning (~8am)
#and right after work gets out (~5pm)


#Repeating the above scatter plot for non working days

# filter the data for non working days
df2<-filter(bike, workingday==0)
ggplot(df2,aes(hour,count))+
  geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)+
  scale_color_gradientn(colours = c('dark blue','blue','light blue',
           'light green','yellow','orange','red'))+ theme_bw()

# non-working days have a steady rise and fall for the afternoon

# BUILDING THE MODEL ----
# correlation between temp and count
cor(bike[,c('temp','count')])
# correlation between tem and count =0.39

# fitting linear model using lm() function:
temp.model <- lm(count~temp,data=bike)
# the model predicts count based on the temp 
summary(temp.model)$coefficients


#Intercept (β0=6.4): It is the value of y when x=0. Thus, it is 
#the estimated number of rentals when the temperature is 0 degrees Celsius.

#The "temp" coefficient (β1=9.17): a temperature difference of 1 degree Celsius
#is associated with a rental difference of 9.17 bikes. 

###########################################
#Building Multiple Linear Regression Model

str(bike)
# hour column is character
# we can use sapply() and as.numeric function to change the hour column to numeric 

bike$hour<-sapply(bike$hour,as.numeric)
str(bike)

# Building MLRM that predicts count based off of 
#season, holiday, workingday, weather, temp, humidity, windspeed and hour

model<-lm(count ~season+holiday+workingday+weather+temp+humidity+
            windspeed+hour, data = bike)

summary(model)

#OLS model doesn't work well given our seasonal and time series data

# We can test the performance of the model by spliting the data in to training and testing set
# Load catet library for train-test split
library(caret)

# Train-Test Split
# createDataPartition function from the caret library was used to create a train-test split

# createDataPartition():a 70-30 train-test split
#p = 0.7: he proportion of dataw want in the training set (70% in this case)
# the remaining 30% will be in the test set.
#list = FALSE: the output will be in vector format rathter than lists
#times = 1: Specifies the number of times the data should be split. 

set.seed(123) 

trainIndex<-createDataPartition(bike$count, p = 0.7, list = FALSE, times = 1)
trainData<-bike[trainIndex, ]
testData<-bike[-trainIndex, ]

# Fit a linear model on the training set
lmModel<-lm(count ~ season + holiday + workingday + weather + 
                temp + humidity + windspeed + hour, data = trainData)
summary(lmModel)

#Model Evaluation
# Make predictions on the test set

testPredictions<-predict(lmModel, newdata = testData)

# Calculation of  performance metrics (Root Mean Squared Error (RMSE))
testRMSE<-sqrt(mean((testData$count-testPredictions)^2))

# Print the RMSE
print(testRMSE)

# RMSE of 150.87 is high
# OLS doesn't work well given our seasonal and time series data. 
# In this case, implemeting a model that can account for this type of trend would be benefitial














