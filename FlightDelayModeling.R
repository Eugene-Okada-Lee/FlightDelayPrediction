#Eugene Okada Lee


rm(list = ls()) # This is used to clear any type of environment that may be lingering in R studio
#insall all the packages needed 
install.packages("gamlr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("ranger")


#Import all the important libraries that are needed for analysis
library(gamlr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(ranger)

#Import data set that was downloaded to do analysis
delay_data  <- read.csv("C:\\Users\\eugen\\Downloads\\flight.csv")

# PREPROCESSING DATA SET  
# the code below is to clean up the data set: creating subset data, 
# changing characters to factor, ommitting NA observations, changing names,
# and 

delay_data <- sample_n(delay_data, 600000) #This code above randomly selects 600000 rows effectively cutting our data set by 90%

#Rename all the airlines to shorten and make my life easier
delay_data$CARRIER_NAME <- gsub("Alaska Airlines Inc.", "Alaska", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Allegiant Air", "Allegiant", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("American Airlines Inc.", "American", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("American Eagle Airlines Inc.", "American Eagle", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Atlantic Southeast Airlines", "Atlantic Southeast", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Comair Inc.", "Comair", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Delta Air Lines Inc.", "Delta", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Endeavor Air Inc.", "Endeavor Air", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Frontier Airlines Inc.", "Frontier", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Hawaiian Airlines Inc.", "Hawaiian", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("JetBlue Airways", "JetBlue", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Mesa Airlines Inc.", "Mesa", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Midwest Airline, Inc.", "Midwest", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("SkyWest Airlines Inc.", "SkyWest", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Southwest Airlines Co.", "Southwest", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("Spirit Air Lines", "Spirit", delay_data$CARRIER_NAME)
delay_data$CARRIER_NAME <- gsub("United Air Lines Inc.", "United", delay_data$CARRIER_NAME)

changeFactor <- c("DEP_TIME_BLK", "CARRIER_NAME", "DEPARTING_AIRPORT", "PREVIOUS_AIRPORT") # create a vector with all variables that need to be changed to factors
delay_data[changeFactor] <- lapply(delay_data[changeFactor], factor) #Change catagroical data to factor

delay_data <- na.omit(delay_data) #this is to omit any entries that are missing data points

delay_data$tookflight <- c(1) #all flights recorded took flight and were not canceled according to 


#SUMMARY STATISTICS AND RELEVANT PLOTS 
summary(delay_data)

dim(delay_data)
#barplot of delays per month
delay_per_month<- tapply(delay_data$DEP_DEL15 ==1, delay_data$MONTH, sum)
BAR<-barplot(delay_per_month, main="Number of Delays per Month in 2019", ylab ="Number of Delays", names.arg=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"),las=1, lwd=1)
text(x= BAR, y = (delay_per_month), label = (delay_per_month), pos = 1, cex = 0.8, col = "blue") #This code is used to print the values of people that use each media type


#barplot plotting number of delays by airline
par(mar = c(5, 8, 5, 4))
flights_per_airline <- tapply(delay_data$tookflight, delay_data$CARRIER_NAME, sum)
barplot(flights_per_airline, main="Barplot of Flight Delays per Airline in 2019", xlab="Number of Flights", col="black", xpd=TRUE, las=1, lwd=1, horiz = TRUE, space=1) #code for the barplot

delay_per_airline<- tapply(delay_data$DEP_DEL15 ==1, delay_data$CARRIER_NAME, sum) #tapply code that creates the array with the information needed in our barplot
barplot(delay_per_airline,col="red",xpd=TRUE, las=1, lwd=1, horiz = TRUE, space=1, add = TRUE) #code for the barplot

#Code below is for box in our graph with discriptions
legend("bottomright", legend=c("Total Flights Delayed", "Total Flights"), col=c("red", "blue"), fill = 2:1, cex=0.8,
       title="Group", text.font=4)


#MODEL BUILDING


gc()  #before going into the model building, I use this to clear up unused data to free up disk memory
#setting up a training and test set 
K <- 10
n <- 600000
folds <- rep(1:K,each=(n/K))
randomized <- sample(1:n)
random_folds <- folds[randomized]
training_set_indices <- random_folds!=1
#This is to generate our two groups
training_set <- delay_data[training_set_indices,] #random traing set with 90% of the US players
test_set <-  delay_data[!training_set_indices,] #random test set with 10% of the US players


#Linear Regression Using Lasso-Logit to calculate a model for delayed flights 


X <- model.matrix(~., data=training_set[ , !(names(training_set) %in% c("DEP_DEL15","AIRPORT_FLIGHTS_MONTH","AIRLINE_FLIGHTS_MONTH","AIRLINE_AIRPORT_FLIGHTS_MONTH",                                                                    "AVG_MONTHLY_PASS_AIRPORT","AVG_MONTHLY_PASS_AIRLINE", "PREVIOUS_AIRPORT"))])[,-1] 
Y<- training_set$DEP_DEL15
dim(X)
lasso_model <- cv.gamlr(X, Y, nfold=10, family="binomial")
lasso_model
plot(lasso_model$gamlr,main="Lambda Estimations on Beta", ylab="estimated betas")
plot(lasso_model) #

length(coef(lasso_model)) 

cat("number of nonzero coefficients for CV-optimal lambda:", length(which(coef(lasso_model, select='min')!=0)), "\n")


extracted_coeff <- coef(lasso_model, select="min")
extracted_coeff

pred_delay <- drop(predict(lasso_model, X, type="response"))
#in sample predictions
mean_y <- mean(training_set$DEP_DEL15)
y <- (training_set$DEP_DEL15)
training_pred <- predict(lasso_model, newdata = X, type="response")

IS_R2 <- 1 - (sum((y-training_pred)^2))/sum((y-mean_y)^2) 

#Out of Sample R^2

new_y <- test_set$DEP_DEL15
X1 <- model.matrix(~.,data=test_set[ , !(names(training_set) %in% c("DEP_DEL15","AIRPORT_FLIGHTS_MONTH","AIRLINE_FLIGHTS_MONTH","AIRLINE_AIRPORT_FLIGHTS_MONTH",
                                                                  "AVG_MONTHLY_PASS_AIRPORT","AVG_MONTHLY_PASS_AIRLINE", "PREVIOUS_AIRPORT"))])[,-1] 

test_pred <- predict(lasso_model, newdata = X1, type="response")

OOS_R2 <- 1 - (sum((new_y-test_pred)^2))/sum(new_y-mean_y)^2

R2 <-  1 - (sum((new_y-training_pred)^2))/sum(new_y-mean_y)^2


#Even smaller sub sample to see if anything changes 
delay_data1 <- sample_n(delay_data, 600) #This code above randomly selects 600000 rows effectively cutting our data set by 90%



K <- 10
n <- 600
folds <- rep(1:K,each=(n/K))
randomized <- sample(1:n)
random_folds <- folds[randomized]
training_set_indices <- random_folds!=1
#This is to generate our two groups
training_set <- delay_data1[training_set_indices,] #random traing set with 90% of the US players
test_set <-  delay_data1[!training_set_indices,] #random test set with 10% of the US players


#Linear Regression Using Lasso-Logit to calculate a model for delayed flights with smaller sub sample


X <- model.matrix(~.^2, data=training_set[ , !(names(training_set) %in% c("DEP_DEL15","AIRPORT_FLIGHTS_MONTH","AIRLINE_FLIGHTS_MONTH","AIRLINE_AIRPORT_FLIGHTS_MONTH",                                                                    "AVG_MONTHLY_PASS_AIRPORT","AVG_MONTHLY_PASS_AIRLINE", "PREVIOUS_AIRPORT"))])[,-1] 
Y<- training_set$DEP_DEL15
dim(X)
lasso_model <- cv.gamlr(X, Y, nfold=10, family="binomial")
lasso_model
plot(lasso_model$gamlr,main="Lambda Estimations on Beta", ylab="estimated betas")
plot(lasso_model) #

length(coef(lasso_model)) 

cat("number of nonzero coefficients for CV-optimal lambda:", length(which(coef(lasso_model, select='min')!=0)), "\n")


extracted_coeff <- coef(lasso_model, select="min")
extracted_coeff

pred_delay <- drop(predict(lasso_model, X, type="response"))
#in sample predictions
mean_y <- mean(training_set$DEP_DEL15)
y <- (training_set$DEP_DEL15)
training_pred <- predict(lasso_model, newdata = X, type="response")

IS_R2 <- 1 - (sum((y-training_pred)^2))/sum((y-mean_y)^2) 

#Out of Sample R^2

new_y <- test_set$DEP_DEL15
X1 <- model.matrix(~.,data=test_set[ , !(names(training_set) %in% c("DEP_DEL15","AIRPORT_FLIGHTS_MONTH","AIRLINE_FLIGHTS_MONTH","AIRLINE_AIRPORT_FLIGHTS_MONTH",
                                                                    "AVG_MONTHLY_PASS_AIRPORT","AVG_MONTHLY_PASS_AIRLINE", "PREVIOUS_AIRPORT"))])[,-1] 

test_pred <- predict(lasso_model, newdata = X1, type="response")

OOS_R2 <- 1 - (sum((new_y-test_pred)^2))/sum(new_y-mean_y)^2

R2 <-  1 - (sum((new_y-training_pred)^2))/sum(new_y-mean_y)^2



#Generate Random Forest for Delay
set.seed(0)

delaytree <- ranger(Y ~ ., data = training_set[ , !(names(training_set) %in% c("DEP_DEL15","AIRPORT_FLIGHTS_MONTH","AIRLINE_FLIGHTS_MONTH","AIRLINE_AIRPORT_FLIGHTS_MONTH",
                                                            "AVG_MONTHLY_PASS_AIRPORT","AVG_MONTHLY_PASS_AIRLINE", "PREVIOUS_AIRPORT","tookflight"))],
                    write.forest = TRUE, num.tree = 200, min.node.size = 5, importance = "impurity")

barplot(sort(importance(delaytree), decreasing = TRUE), las = 2, Main="Most Important Variables in Random Forest" )
head(delaytree)
delaytree$r.squared #check out of sample fit

preds <- predict(delaytree, data = test_set)$predictions

plot(test_set$DEP_DEL15, preds,
     ylab = "predicted outcome")
