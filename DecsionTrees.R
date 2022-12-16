install.packages('randomForest')
install.packages('rpart.plot')
library(caTools) 
library("readxl")
library("readxl")

dataset <- read_excel("Data_Extract_From_Education_Statistics_-_All_Indicators_2015.xlsx")

# Clearing data, removing all the countries that are having missing value in any of the below indicators columns
ds <-dataset[complete.cases(dataset$`PISA: Mean performance on the science scale [LO.PISA.SCI]`,
                        dataset$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, 
                        dataset$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`), ]

#scaling data
ds$Government_expenditure_scaled = scale(ds$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`)
ds$GDPpc_scaled = scale(ds$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`)
#ds$GNIpercapita_scaled = scale(ds$`GNI per capita, PPP (current international $) [NY.GNP.PCAP.PP.CD]`)
#ds$pri_edu_funding_scaled = scale(ds$`Initial government funding per primary student as a percentage of GDP per capita [UIS.XUNIT.GDPCAP.1.FSGOV]`)
#ds$sec_edu_funding_scaled = scale(ds$`Initial government funding per secondary student as a percentage of GDP per capita [UIS.XUNIT.GDPCAP.23.FSGOV]`)





#Decision Tree  Classification

#1. Creating categorical variable for PISA Science Score when it is above median of 484.7
library(tree)
PISA_score_science <- factor(ifelse(ds$`PISA: Mean performance on the science scale [LO.PISA.SCI]`<= 486.5, "Low", "High"))
ds$PISA_score_science <- PISA_score_science 


dataframe <- data.frame(ds)

#2. Creating test set and training set on dataframe

set.seed(123)
split = sample.split(dataframe$PISA_score_science, SplitRatio = 0.6)
df_training_set = subset(dataframe, split == TRUE)
df_test_set = subset(dataframe, split == FALSE)

#3 Creating decision tree based on 2 classifiers - GDP pp and expenditure
library(rpart)
library(rpart.plot)

tree.dataset <- rpart(formula = PISA_score_science ~GDPpc_scaled
                       +Government_expenditure_scaled,
                       data = df_training_set)

summary(tree.dataset)
rpart.plot(tree.dataset)
text(tree.dataset)


# Predicting the Test set results
y_pred = predict(tree.dataset, newdata = df_test_set[-16], type = 'class')

# Making the Confusion Matrix
cm = confusionMatrix(df_test_set[, 16], y_pred)
cm

