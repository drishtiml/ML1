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



# Take 100k rows from dataframe from the start
newDf <- head(df,100000)

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
data_ext <- na.omit(dataset)



numberOfRows <- nrow(data_ext)
calcuateTill <- as.integer(numberOfRows/2)
step <- 500

arrayLength <- as.integer((calcuateTill/step))
x <- array(dim=arrayLength)

#For glmnet model error and cross validation error
t <- array(dim=arrayLength)
cv <- array(dim=arrayLength)

#For lm model error and cross validation error
lmt <- array(dim=arrayLength)
lmcv <- array(dim=arrayLength)

calcuateTill
arrayLength
seq(step, calcuateTill, step)

for (m in seq(step, calcuateTill, step)) {
  
  set.seed(1010)
  trainIndex <- sample(x=nrow(data_ext), size=m)
  train <- data_ext[trainIndex,]
  
  leftoverAfterTrain <- data_ext[-trainIndex,]
  
  crossValidationIndex <- sample(x=nrow(leftoverAfterTrain), size=m)
  
  crossValidation <- leftoverAfterTrain[crossValidationIndex,]
  
  lm.train <- lm(train$num_attempts ~ train$customer_profile + train$profession)
  lm.predictTrain <- predict(lm.train, train[,c("customer_profile","profession")])
  lmCostTrain <- sum((lm.predictTrain - train$num_attempts) ^ 2) / (2 * nrow(train))
  
  lm.predictCV <- predict(lm.train, crossValidation[,c("customer_profile","profession")])
  lmCostCV <- sum((lm.predictCV - crossValidation$num_attempts) ^ 2) / (2 * nrow(train))
  
  
  
  glmnet_model <-
    glmnet(
      x = MatrixModel(train),y = train$num_attempts, lambda = c(1)
    )
  
  predTrain <-
    predict(
      glmnet_model,newx = MatrixModel(train),s = c(1), type = "response"
    )
  
  costTrain <- sum((predTrain - train$num_attempts) ^ 2) / (2 * nrow(train))
  
  predCv <-
    predict(
      glmnet_model,newx = MatrixModel(crossValidation),s = c(1), type = "response"
    )
  
  costCV <- sum((predCv - crossValidation$num_attempts) ^ 2) / (2 * nrow(crossValidation))
  
  print(paste("m:", m, " costTrain:", costTrain, " costCV:", costCV,  " lmTest:", lmCostTrain,  " lmcostCV:", lmCostCV))
  
  
  i <- as.integer(m/step)
  x[i] <- m
  t[i] <- costTrain
  cv[i] <- costCV
  
  lmt[i] <- lmCostTrain
  lmcv[i] <- lmCostCV
}

x = data.frame(NumberOfTrainingExample=x, TrainingError=t, CrossValidationError=cv, LMT=lmt, LMCV=lmcv)

ggplot(x, aes(x=NumberOfTrainingExample)) + 
  stat_smooth(aes(y = TrainingError, colour = "LR-Training Error"), method=loess, formula = y ~ x, n=80, level=0.8, se=FALSE) + 
  stat_smooth(aes(y = CrossValidationError, colour = "LR-Cross Validation Error"), method=loess, formula = y ~ x, n=80, level=0.8, se=TRUE) +
  stat_smooth(aes(y = LMT, colour = "LM-LR-Training Error"), method=loess, formula = y ~ x, n=80, level=0.8, se=FALSE) + 
  stat_smooth(aes(y = LMCV, colour = "LM-LR-Cross Validation Error"), method=loess, formula = y ~ x, n=80, level=0.8, se=FALSE) + 
  ylab("Training Error") +
  xlab("Number Of Training Example") +
  ggtitle("Model Algo Learning Capability- Connectivity Prediction")
