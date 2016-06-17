# data.csv contains phone calling data to customers. Customer consumes services of business house and next month customer have to pay. Company dials their customers to collect the payment. 
# Data I have for every call "call_type, system_disposition, call_originate_time, num_attempts, customer_profile, customer_id, profession"
# Description ::::::::::::::::::::::::::
# call_type : these are the methods of calling a customer.
# call_originate_time : Timestamp when the customer is called
# num_attempts : Attempt number when the customer is called. Every attempt to a customer increases the value. 
# customer_profile : Customer are clustered based on their payment behavior. These are hash value of their actual profile
# customer_id : uniquely identifies a customer 
# profession : profession of a customer. These are hash value of their actual profession

# Problem Objective "num_attempts required to connect this customer" at a particular month.


#Read the data file from CSV. Data read is stored as "DataFramne" 
df <- read.csv("data.csv", header=TRUE, stringsAsFactors=FALSE, sep=",")

#Print the first 2 rows from the data frame
head(df, 2)
#Print the dimention of the DataFrame df
dim(df)

#how to get a value
df$call_type[5]


# If customer id for a call is not present then remove the data
df <-df[!(is.na(df$customer_id) == TRUE | is.null(df$customer_id) == TRUE | is.nan(df$customer_id) == TRUE),]

#The amount of data remained after cleanup
dim(df)


#Data cleanup --> either discard the data or put custom value if value is missing
#here putting CUSTOM_NA for null or NA or empty
ClassDataCleanup <- function(names, df)
{
  for (name in names) {
    df[, name] <-
      ifelse(is.na(df[, name]) | is.null(df[, name]), "CUSTOM_NA", ifelse(nchar(trimws(df[, name])) == 0, "CUSTOM_NA", df[, name]))
  }
  return(df)
}

classCleanupNames <-
  c(
    "call_type", "customer_profile", "profession"
  )

df = ClassDataCleanup(classCleanupNames, df)



# Take 10k rows from dataframe from the start
newDf <- head(df,10000)

dim(newDf)

#converting to date format for call_originate_time
newDf$call_originate_time <- strptime(newDf$call_originate_time, "%Y-%m-%d %H:%M:%S")

# sort the data on basis of call_originate_time
newDf <- newDf[with(newDf, order(call_originate_time)), ]

# year returns year after 1900
#example 116-0 (it count year after 1900 so 2016 is 116 and jan is 0)
newDf$yearmon <- paste(as.character(newDf$call_originate_time$year), as.character(newDf$call_originate_time$mon), sep="-" )

head(newDf)
tail(newDf)


#now will try to extract data for each customer,whether it got connected otherwise NA
#Defining some methods

# for a customer if a months call received then select first connected call
firstConnectedCall <- function(chunk) {
  # returns the first connected row because data is already sorted
  chunk[match("CONNECTED", chunk$system_disposition),]
}

# for a customer selects calls for customer
callForCustomer <- function(chunk) {
   # split the data based on year-month -> apply firstConnectedCall function to all of the splits and merge
   do.call(rbind, lapply(split(chunk,chunk$yearmon), firstConnectedCall))
}

# split the data based on customer_id -> apply callForCustomer function to all of the splits and merge
extractedDf <- do.call(rbind, lapply(split(newDf,newDf$customer_id), callForCustomer))

# For a customer in a month the customer may not have connected and firstConnectedCall function may return NA
extractedDf <-extractedDf[is.na(extractedDf$customer_id) == FALSE,]

names(extractedDf)
dim(extractedDf)


unique(df$customer_profile)

#as.factor will be required for feature not having order

extractedDf$customer_profile <- as.factor(extractedDf$customer_profile)
extractedDf$profession <- as.factor(extractedDf$profession)

extractedDf$num_attempts <- as.numeric(extractedDf$num_attempts)


#extracting only required fields from data
dataset <- extractedDf[,c("customer_profile","profession","num_attempts", "customer_id")]


# Remove data where field
dataset <- na.omit(dataset)

# One fifth of the data will used for testing and 4/5 portion will be training set
numberOfRows <- nrow(dataset)
trainSetLength <- as.integer((numberOfRows * 4) / 5);


# to get the same random set every time program runs and take sample data randomly
set.seed(1)  #setting a random number in context for sample to use, same random is given for same variable

#these are the index for which training will be done
trainIndex <- sample(x = numberOfRows, size = trainSetLength)

train <- dataset[trainIndex,]
test <- dataset[-trainIndex,]

nrow(train)
nrow(test)

#Not selecting customerId

MatrixModel <- function(train)
{
  xfactors <-
    model.matrix(
      train$num_attempts ~ train$customer_profile + train$profession
    )[,-1]
  
  x <-
    as.matrix(
      data.frame(xfactors)
    )
  
  return(x)
}



############4 fold validation (Data is split into 3:1 basis ) Doing CROSS VALIDATION
xtrain <-  MatrixModel(train)

require(glmnet)
#cv.glmnet is the function to train and crossvalidate linear regression returns the model for which best value returns
cv.glmmod <- cv.glmnet(x = xtrain, y = train$num_attempts, nfolds = 4)

#this prints the model summary created by crossvalidation
summary(cv.glmmod)

#This is the actual training, parameters chosen from cross validation. We choose lambda.min and lambda.1se 2 value and trainn model for these 2 value
glmnet_model <-
  glmnet(xtrain, y = train$num_attempts, lambda = c(cv.glmmod$lambda.min, cv.glmmod$lambda.1se, 0.5))


#####################Prediction on Test ############################
xtest <-  MatrixModel(test)

test$pd1se <- predict(glmnet_model, newx=xtest, s=cv.glmmod$lambda.1se)
test$pdmin <- predict(glmnet_model, newx=xtest, s=cv.glmmod$lambda.min)
test$pdmin05 <- predict(glmnet_model, newx=xtest, s=0.5)



test$ID<-seq.int(nrow(test))

ggplot(test, aes(x = test$ID)) + 
  geom_point(stat = "identity", aes(y = test$pd1se, color = "pd1se")) +
  geom_point(stat = "identity", aes(y = test$pdmin, color = "pdmin")) +
  geom_point(stat = "identity", aes(y = test$pdmin05, color = "pdmin05")) +
  geom_point(stat = "identity", aes(y = test$num_attempts, color = "num_attempts")) +
  ylab("Values of number of attempts") +
  xlab("Number Of Testing Example") +
  ggtitle("Actual vs Predicted")

