---
title: |
  | Predicting Hotel Cancellation Rates
  | Group 11 Final Project Report
author: "Sidrah Ahmad, Sarah Handy, James Kadado, Kanav Kansal"
date: "11/25/2020"
output: html_document
---

## Executive Summary 

The hotel and hospitality industry are dependent on many factors that influence demand, sales, and cancellations. Each factor plays a different role on the outcome of a single booking. Our data analysis focuses on the cancellations of bookings and the factors affecting cancellation. Prediction of cancellations provides enough information for decision makers in the industry to navigate dynamic management of bookings, mitigate losses, and manage their workforces more effectively.

Our analysis investigates attributes of the guests themselves including whether individuals or families cancel more, if prior cancellations influence future cancellations, and whether repeat guests cancel more than new guests. It also checks if the booking lead time can be used to predict cancellation.

The overarching goal is to establish a predictive model of cancellations that can be used to manage bookings to maximize profits for hotel management while minimizing disturbance to guests from overbookings,

*Summarize conclusions here* Sarah

## Introduction

The dataset “Hotel Booking Demand” hosted on Kaggle and originally published by Nuno Antonio, Ana Almeida, and Luis Nunes in 2019, contains more than 100,000 entries across 32 variables of data which provide an excellent body of data for study. This data set contains booking information for a city hotel and a resort hotel and includes information such as when the booking was made, booking cancellations, length of stay, the number of adults, children, and/or babies, and more. 

We used this data to analyze booking cancellations based on various variables such as individuals versus families and repeat guests versus non-repeat guests. In addition, we analyzed if reservation details can be used to predict cancellation rates for hotels, and whether it varies by different factors such as guest type or booking time. These are valuable questions because analysis applied to these questions can be easily translated to other industries where revenues are accrued in advance. 

## Data Description

The dataset “Hotel booking demand” hosted on Kaggle with more than 100,000 entries across 32 variables of data which will provide an excellent body of data for study. This data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, length of stay, the number of adults, children, and/or babies, and the number of available parking spaces, and many more things.


```{r Load Packages, include = FALSE}
#Install packages if needed
#Code and output suppressed to avoid printing to pdf
if(!require("data.table")) install.packages("data.table")
if(!require("readr")) install.packages("readr")
#if(!require("readlxl")) install.packages("readlxl")
if(!require("tidyr")) install.packages("tidyr")
if(!require("tidyverse")) install.packages("tidyverse")
#if(!require("hmisc")) install.packages("hmisc")
if(!require("psych")) install.packages("psych")
if(!require("leaps")) install.packages("leaps")
if(!require("corrplot")) install.packages("corrplot")
if(!require("forecast")) install.packages("forecast")
if(!require("MASS")) install.packages("MASS")
if(!require("caret")) install.packages("caret")
if(!require("randomForest")) install.packages("randomForest")
if(!require("rpart")) install.packages("rpart")
if(!require("rpart.plot")) install.packages("rpart.plot")
if(!require("gains")) install.packages("gains")
if(!require("lift")) install.packages("lift")
library(lift)
library(gains)
library(data.table)
library(readr)
#library(readlxl)
library(tidyr)
library(tidyverse)
#library(hmisc)
library(psych)
library(leaps)
library(corrplot)
library(forecast)
library(randomForest)
library(rpart)
library(rpart.plot)
library(MASS)
library(caret)
```
## Preprocess Methodology

The hotel bookings data set contains 119,390 observations of data with 32 predictors for cancellation. To clean the data for analysis, we first removed lines with missing or na data. Fortunately, the data set was very clean, and only 4 rows of data were removed by this process.

Additionally, we wanted to consider the impact of party size to cancellations. For this reason, we generated a "families" column by summing the babies, adults, and children columns. We were interested in exploring this as a categorical variable so we further refined the category. Parties of 3 or greater individuals were considered families and assigned a value of 1 while parties of 1 or 2 people were assigned a value of 0.

Finally, in order to facilitate generation and test of machine learning models, we partitioned our data. We allocated 80% of the data to train and retained 20% for model validation.

```{r Data Input & Preprocessing, include = FALSE}

data <- read.csv("hotel_bookings.csv")

# Sarah replaced the below lines of code so commenting out
## Replace 4 missing values with 0 (N)
#hotel_bookings[is.na(hotel_bookings)] <- 0

#Remove na data from consideration
sum(is.na(data))
hotel_bookings <- na.omit(data)

##Merge babies, children, and adults to create families column
hotel_bookings$families <- hotel_bookings$babies + hotel_bookings$children + hotel_bookings$adults

# Changing values in Families (Less than 3 -> 0, More than 2 -> 1)
hotel_bookings$families[which(hotel_bookings$families < 3)] = 0
hotel_bookings$families[which(hotel_bookings$families > 2)] = 1
summary(hotel_bookings$families)

```

```{r Data Partition, include=FALSE}

set.seed(42)
train.index <- sample(c(1:95512))
train.df <- hotel_bookings[train.index, ]
valid.df <- hotel_bookings[-train.index, ]

```

## Exploratory Data Analysis

We are interested in reviewing the relationship between booking cancellation and several of the predictor variables included in the data set. We initially hypothesized that cancellations would be greater for individuals versus family due to planning flexibility. We also thought that repeat guests would demonstrate loyalty to the brand and cancel less than nonrepeat customers. For guests that had already canceled once, we predicted that the likelihood of canceling again would be reduced due to reluctance to make further changes. Finally, we expect that the farther in advance a customer books, the greater the propensity for cancellation for reasons such as unforeseen circumstances.

#### Correlation of numeric variables
An overall review of numeric correlations indicates related items that are logically apparent. Stays in week nights and weekend nights are correlated because there may be visit overlap. Maintaining prior bookings are of course correlated to repeat guests. Overall, the main goal is to determine whether there is a correlation to reservation cancellation so this initial plot was insufficient to meet our needs, and we reviewed correlation to cancellation separately.

``` {r EDA correlation plot, echo = FALSE}
cor_data <- setDT(hotel_bookings)
correlation <- cor(hotel_bookings[,-c(1,5,13:16,20:21,23:25,27,31:32)])
corrplot(correlation, tl.cex=0.5)
```
#### Correlation of cancellations to numeric variables 

Reviewing the correlation to cancellation specifically reveals a positive correlation to lead time of the reservation meaning that bookings made farther in advance are more likely to be canceled. This conclusion is in line with our initial hypothesis and warrants further investigation. It also reveals a negative correlation to special requests and required parking demonstrating that these may indicate a booking is less likely to be canceled. 

``` {r Correlation to Cancellations, echo = FALSE}
cor_data <- setDT(hotel_bookings)
cancelcorrelation <- cor(hotel_bookings[,2], hotel_bookings[,-c(1:2,5,13:16,20:21,23:25,27,31:32)])
cancelcorrelation
```
#### Bar Graph displaying number of cancellations for repeated guests and non repeated guests.
The graph is divided into 2 main categories; repeated guests as 1, non-repeated guests as 0. Inside the bins The 1.0's on the x axis are cancelled guests and 0.0's are the not cancelled guests. This shows us that more number of non-repeat guests are likely to cancel that repeat guests. Loyalty programs such as memberships and discounts help in the reduction of cancellation of number of cancellations.
```{r Bargraph}
ggplot(data = hotel_bookings , aes(is_repeated_guest)) + geom_histogram(binwidth = 0.5) + facet_wrap(~ is_canceled) + xlab("Is Repeated Guests or Not") + ylab("No. of Cancellations")
```

#### Boxplot displaying number of cancellations in relation to the lead time of bookings
The graph shows us that guests that have more lead time are more likely to cancel.The boxplot below takes into consideration another category of guests that did not show up , we will be exploring the categories further in our final model
```{r Boxplot}
ggplot(data = hotel_bookings, aes(
  x = reservation_status,
  y = lead_time,
  fill = factor(is_canceled)
)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Cancellation and lead time",
    x = "Cancellations",
    y = "Lead Time (Days)"
  ) +
  scale_fill_discrete(
    breaks = c("0", "1"),
    labels = c("Checked in", "Not checked in")
  ) 
```
## Model Selection - Sarah

We initially investigated use of normal linear regression but our interest in predicting a categorical variable violated the assumptions of this model.

## Emperical Data Analysis

#### LDA
```{r LDA}
hblda <- lda(formula = is_canceled ~ lead_time + arrival_date_week_number + stays_in_weekend_nights + babies+is_repeated_guest + booking_changes+days_in_waiting_list + adr + required_car_parking_spaces +arrival_date_year + previous_bookings_not_canceled + adults + previous_cancellations + total_of_special_requests + is_repeated_guest, data = train.df)
hblda

pred1.train <- predict(hblda, train.df)
ldahist(pred1.train$x[,1], 
        g=train.df$is_canceled,
        col = 6)
```

#### Regression Tree (Sidrah)
```{r Regression Tree}
default1.ct <- rpart(formula = is_canceled ~ lead_time + arrival_date_week_number + stays_in_weekend_nights + babies+is_repeated_guest + booking_changes+days_in_waiting_list + adr + required_car_parking_spaces +arrival_date_year + previous_bookings_not_canceled + adults + previous_cancellations + total_of_special_requests + is_repeated_guest, data = train.df , maxdepth = 4, minsplit = 2)
prp(default1.ct, type = 1, extra = 1, under = TRUE, roundint = TRUE, 
    split.font = 4, varlen = -25, box.palette = "BuOr")
```
# regression tree - cancellations based on market segment (Sidrah) 

```{r}

cancellations.market<- rpart(formula = is_canceled ~ hotel+ market_segment + is_repeated_guest+lead_time,data = train.df, method = "class")
rpart.plot (cancellations.market)

cancellations.market<- rpart(formula = is_canceled ~ hotel+ market_segment + is_repeated_guest+lead_time,data = train.df)
prp(cancellations.market, type = 1, extra = 1, under = TRUE,
    split.font = 4, varlen = -25, box.palette = "BuOr")

```
# validation dataset cancellations tree


```{r} 

cancellations.market<- rpart(formula = is_canceled ~ hotel+ market_segment + is_repeated_guest+lead_time,data = valid.df)
prp(cancellations.market, type = 1, extra = 1, under = TRUE,
    split.font = 4, varlen = -25, box.palette = "BuOr")

```

### Regression Tree - New with most variables included 
```{r}
train <- train.df[,-c(4,5,6,7,14,25,24,31,32)]
default1.ct <- rpart(formula = is_canceled ~ ., data = train, maxdepth = 4, minsplit = 2)
prp(default1.ct, type = 1, extra = 1, under = TRUE, roundint = TRUE, 
    split.font = 4, varlen = -25, box.palette = "BuOr")
```

### Logistic Regression - James and Kanav
```{r Logistic Regression , message=FALSE, warning=FALSE}

# logistic regression
logit.reg <- glm(formula = is_canceled ~ lead_time + arrival_date_week_number + stays_in_weekend_nights + babies+is_repeated_guest + booking_changes+days_in_waiting_list + adr + required_car_parking_spaces +arrival_date_year + previous_bookings_not_canceled + adults + previous_cancellations + total_of_special_requests + is_repeated_guest, data = train.df, family = binomial)

options(scipen=999)
summary(logit.reg)

# generate odds-ratios
exp(coef(logit.reg))

# model selection
logitnew <- stepAIC(logit.reg, trace = 0)
summary(logitnew)

# performance evaluation
logit.reg.pred <- predict(logit.reg, train.df, type = "response")

t(t(head(logit.reg.pred, 10)))

# generate confusion matrix
table(train.df$is_canceled , logit.reg.pred > 0.5)
```

```{r message=FALSE, warning=FALSE}
xx <- data.frame(train.df$is_canceled,logit.reg.pred)
names(xx)[1] <- "actual"
names(xx)[2] <- "prob"

gain <- gains(xx$actual, xx$prob, groups=dim(xx)[1])

plot(c(0, gain$cume.pct.of.total*sum(xx$actual)) ~ c(0, gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l",
     col="blue1")
lines(c(0,sum(xx$actual))~c(0,dim(xx)[1]), col="red1", lty=2)

gain <- gains(xx$actual, xx$prob)
barplot(gain$mean.resp / mean(xx$actual), names.arg = gain$depth, xlab = "Percentile", space = 1.3,
        ylab = "Mean Response", main = "Decile-wise lift chart", col = "seagreen", border = NA)
```

### LR on Valid dataset
```{r message=FALSE, warning=FALSE}
logit.reg.valid <- glm(formula = is_canceled ~ lead_time + arrival_date_week_number + stays_in_weekend_nights + babies+is_repeated_guest + booking_changes+days_in_waiting_list + adr + required_car_parking_spaces +arrival_date_year + previous_bookings_not_canceled + adults + previous_cancellations + total_of_special_requests + is_repeated_guest, data = valid.df, family = binomial)

logit.reg.pred.valid <- predict(logit.reg.valid, valid.df, type = "response")

xx <- data.frame(valid.df$is_canceled,logit.reg.pred.valid)
names(xx)[1] <- "actual"
names(xx)[2] <- "prob"

gain <- gains(xx$actual, xx$prob, groups=dim(xx)[1])

plot(c(0, gain$cume.pct.of.total*sum(xx$actual)) ~ c(0, gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l",
     col="blue1")
lines(c(0,sum(xx$actual))~c(0,dim(xx)[1]), col="red1", lty=2)

gain <- gains(xx$actual, xx$prob)
barplot(gain$mean.resp / mean(xx$actual), names.arg = gain$depth, xlab = "Percentile", space = 1.3,
        ylab = "Mean Response", main = "Decile-wise lift chart", col = "seagreen", border = NA)

# Accuracy
p = mean(valid.df$is_canceled)

logit.reg.pred.valid$predictions <- ifelse(logit.reg.pred.valid >p , 1,0)
mean(valid.df$is_canceled == logit.reg.pred.valid$predictions)

```

### Random Forest 
### Getting Accuracy of 88% for Training set and 89% for Validation set with Data Normalization
```{r}
train <- train.df[,-c(4,5,6,7,14,25,24,31,32)]
train$is_canceled <- as.character(train$is_canceled)
train$is_canceled  <- as.factor(train$is_canceled)

rf_model1 <- randomForest(is_canceled~.,
                        data = train,
                        ntree = 100)

prf_train<- predict(rf_model1,train)
confusionMatrix(prf_train, train$is_canceled)

valid <- valid.df[,-c(4,5,6,7,14,25,24,31,32)]

valid$is_canceled <- as.character(valid$is_canceled)
valid$is_canceled  <- as.factor(valid$is_canceled)

rf_model2 <- randomForest(is_canceled~.,
                        data = valid,
                        ntree = 500)

prf_valid<- predict(rf_model2,valid)
confusionMatrix(prf_valid, valid$is_canceled)
```

## Conclusions

After running multiple models, we unraveled some key findings for the hotel bookings data set. We conclude from the accuracy rate of 95% from the random forest algorithm,that it works best to predict future cancellation rates and can be used be by the hotel industry to classify bookings as cancel and non-cancel to make appropriate terms , conditions and discounts. A business suggestion could be that a waiting list can be made based on the chance of cancellations for bookings that are more likely to cancel.

From the model we also learn that the most important variables to predict cancellations are: 

From our initial analysis, we believed that the variable of lead time plays a big role in predicting cancellations but after running our models, we concluded that deposit type affects cancellations most. This may be due to the fact that consumers do not want to lose money while cancelling their bookings.

## Limitations

The hotel industry works such that it is seldom possible for a hotel to know a person’s nationality, exact number of people in the booking, and even their preferences or special requests until the person has actually checked in so it is possible that there may be a difference in the data of cancelled guests and their actual data, and this may be a source of inaccuracy in our predictions.  

Weekend vs weekday  visits are correalted and further analysis has to be done in this variable.

### References

Antonio, N., de Almeida, A., & Nunes, L. (2019). Hotel booking demand datasets. Data in Brief, 22, 41–49. https://doi.org/10.1016/j.dib.2018.11.126
Mostipak, J. (2020, February 13). Hotel booking demand. Retrieved September 09, 2020, from https://www.kaggle.com/jessemostipak/hotel-booking-demand
