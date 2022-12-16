install.packages('randomForest')
install.packages('keras')
install.packages('glmnet')
install.packages('h2o')
library(keras)
library(caTools) 
library("readxl")

dataset <- read_excel("Data_Extract_From_Education_Statistics_-_All_Indicators_2015.xlsx")

# Clearing data, removing all the countries that are having missing value in any of the below indicators columns

ds <-dataset[complete.cases( 
                            dataset$`PISA: Mean performance on the science scale [LO.PISA.SCI]`,
                            dataset$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, 
                            dataset$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`), ]

ds$`Country Code`<- NULL
ds$`Country Name` <- NULL
ds$`Time Code`<- NULL
ds$Time<- NULL
ds$`GNI per capita, PPP (current international $) [NY.GNP.PCAP.PP.CD]` <- NULL
ds$`GNI (current US$) [NY.GNP.MKTP.CD]`  <- NULL
ds$`Initial government funding per primary student as a percentage of GDP per capita [UIS.XUNIT.GDPCAP.1.FSGOV]`<- NULL
ds$`Initial government funding per secondary student as a percentage of GDP per capita [UIS.XUNIT.GDPCAP.23.FSGOV]`<- NULL
ds$`PISA: Mean performance on the reading scale [LO.PISA.REA]`<- NULL
ds$`PISA: Mean performance on the mathematics scale [LO.PISA.MAT]`<- NULL


#Neural Network  Classification

#1. Creating categorical variable for PISA Science Score 1 when it is above median of 484.7 and 0 when below that threshold.

ds$PISA_score_science <- factor(ifelse(ds$`PISA: Mean performance on the science scale [LO.PISA.SCI]`<= 486.5, "Low", "High"))
ds$`PISA: Mean performance on the science scale [LO.PISA.SCI]` <- NULL

ds <- data.frame(ds)

# Feature Scaling

ds[-3] = scale(ds[-3])

#2. Creating test set and training set on dataframe

set.seed(123)
split = sample.split(ds$PISA_score_science, SplitRatio = 0.6)
training_set = subset(ds, split == TRUE)
test_set = subset(ds, split == FALSE)


#3 Creating deep learning model using h2o (open source,we connect to run code externally and has parameter tunning argument)
#3.1 Needed Java to be installed 
library(h2o)
h2o.init(nthreads = -1)

#4 Creating model using ReLU Activation function with 2 layers with 3 neurons, reinforced learning
classifier = h2o.deeplearning(y = 'PISA_score_science',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(5,5,5),
                         epochs = 20,
                         train_samples_per_iteration = -2)
summary(classifier)


# Predicting the Test set results
prob_pred = h2o.predict(classifier, newdata = as.h2o(test_set[-3]))
y_pred = (prob_pred> 0.5)
y_pred <- as.vector(prob_pred$predict)

# Making the Confusion Matrix
library(caret)
Pisa_expected <- as.vector(test_set$PISA_score_science)
Pisa_factor <- as.factor(test_set$PISA_score_science)
pred_factor <- as.factor(y_pred)
cm <- table(y_pred,Pisa_expected)
cm <- confusionMatrix(pred_factor,Pisa_factor)
cm



