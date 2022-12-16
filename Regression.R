# Importing Dataset from World Bank database
install.packages("readxl")
install.packages('caTools')
install.packages('ggplot2')
install.packages("tree")
install.packages('rpart')
library(caTools)
library("readxl")

dataset <- read_excel("Data_Extract_From_Education_Statistics_-_All_Indicators_2015.xlsx")

# Clearing data, removing all the countries that are having missing value in any of the below indicators columns

ds <-dataset[complete.cases( 
                            dataset$`PISA: Mean performance on the science scale [LO.PISA.SCI]`,
                            dataset$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, 
                            dataset$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`), ]



summary(ds$`PISA: Mean performance on the science scale [LO.PISA.SCI]`)

#Checking correlation
plot(`PISA: Mean performance on the science scale [LO.PISA.SCI]`~ `Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, data = ds, label = `Country Name`)
cor(ds$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, ds$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`)

#Plot of the charts 
ggplot(ds, aes(x=`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, y=`PISA: Mean performance on the science scale [LO.PISA.SCI]`)) +
  geom_point() +
  geom_text(
    label=ds$`Country Name`,
    nudge_x=0.2, nudge_y=0,
    check_overlap=T,
    color = 'Blue'
  )


ggplot(ds, aes(x=`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, y=`GDP per capita (current US$) [NY.GDP.PCAP.CD]`)) +
  geom_point() +
  geom_text(
    label=ds$`Country Name`,
    nudge_x=0.15, nudge_y=0,
    color = 'Blue'
  )


# Training and Test split


set.seed(123)
split = sample.split(ds$`PISA: Mean performance on the science scale [LO.PISA.SCI]`, SplitRatio = 0.8)
training_set = subset(ds, split == TRUE)
test_set = subset(ds, split == FALSE)




regressor_exp = lm(formula = `PISA: Mean performance on the science scale [LO.PISA.SCI]` 
                   ~ `Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]` ,
                   data = training_set)
summary(regressor_exp)

library(tidyverse)
library(caret)
# Make predictions and compute the R2, RMSE and MAE
predictions <- regressor_exp %>% predict(test_set)
data.frame( R2 = R2(predictions, test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            RMSE = RMSE(predictions, test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            MAE = MAE(predictions, test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`))

#RSE & R^2
summary(regressor_exp)$sigma 

summary(regressor_exp)$r.sq

plot(predict(regressor_exp), residuals(regressor_exp)) 
plot(predict(regressor_exp), rstudent(regressor_exp))

# Visualising the Training set results - Expenditure
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]` , y = training_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, y = predict(regressor_exp, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Training set: PISA Science Scores vs Government expenditure on education as % of GDP (%)') +
  xlab('Government expenditure on education as % of GDP (%)') +
  ylab('PISA Science Scores')


# Visualising the Test set results - Expenditure on education
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]` , y = test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, y = predict(regressor_exp, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Test set: PISA Science Scores vs Government expenditure on education as % of GDP (%)') +
  xlab('Government expenditure on education as % of GDP (%)') +
  ylab('PISA Science Scores')


#### Non-Linear Transformation i^2 on expenditure


nonlinear_regressor_exp = lm(formula = `PISA: Mean performance on the science scale [LO.PISA.SCI]` ~ `Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]` +  I(`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`^2),
                             data = training_set)
summary(nonlinear_regressor_exp)

#RSE & R^2
summary(nonlinear_regressor_exp)$sigma 
summary(nonlinear_regressor_exp)$r.sq


# Visualising the Training set results - i^2 on expenditure on education
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]` , y = training_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, y = predict(nonlinear_regressor_exp, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Training set: PISA Science Scores vs Government expenditure on education as % of GDP (%)') +
  xlab('Government expenditure on education as % of GDP (%)') +
  ylab('PISA Science Scores')


# Visualising the Test set results - i^2 on expenditure on education
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]` , y = test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, y = predict(nonlinear_regressor_exp, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Test set: PISA Science Scores vs Government expenditure on education as % of GDP (%)') +
  xlab('Government expenditure on education as % of GDP (%)') +
  ylab('PISA Science Scores')




##### Degree 3 Polynomial Regression to the dataset


poly_reg = lm(formula = `PISA: Mean performance on the science scale [LO.PISA.SCI]` ~ poly(`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, degree = 3, raw = T) ,
              data = training_set)

summary(poly_reg)

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]` , y = training_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, y = predict(poly_reg, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Polynomial regression Training set: PISA Science Scores vs Government expenditure on education as % of GDP (%)') +
  xlab('Government expenditure on education as % of GDP (%)') +
  ylab('PISA Science Scores')

library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]` , y = test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`, y = predict(poly_reg, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Polynomial regression Test set: PISA Science Scores vs Government expenditure on education as % of GDP (%)') +
  xlab('Government expenditure on education as % of GDP (%)') +
  ylab('PISA Science Scores')

summary(poly_reg)$sigma 
summary(poly_reg)$r.sq

anova(regressor_exp,poly_reg, nonlinear_regressor_exp)

par(mfrow = c(2, 2)) 
plot(poly_reg)


# GDP per capita analysis

ggplot(ds, aes(x=`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, y=`PISA: Mean performance on the science scale [LO.PISA.SCI]`)) +
  geom_point() +
  geom_text(
    label=ds$`Country Name`,
    nudge_x=0.30, nudge_y=3.25,
    check_overlap=T,
    color = 'Blue'
  )



regressor_GDP = lm(formula = `PISA: Mean performance on the science scale [LO.PISA.SCI]` ~ `GDP per capita (current US$) [NY.GDP.PCAP.CD]` ,
                   data = training_set)
summary(regressor_GDP)
summary(regressor_GDP)$sigma 
summary(regressor_GDP)$r.sq

library(caret)
# Make predictions and compute the R2, RMSE and MAE
predictions_reg <- regressor_GDP %>% predict(test_set)
data.frame( R2 = R2(predictions_reg, test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            RMSE = RMSE(predictions_reg, test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            MAE = MAE(predictions_reg, test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`))


# Visualising the Training set results - GDP
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]` , y = training_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, y = predict(regressor_GDP, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Training set: PISA Science Scores vs GDP per capita (constant 2005 US$)') +
  xlab('GDP per capita (constant 2005 US$)') +
  ylab('PISA Science Scores')


# Visualising the Test set results - GDP
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]` , y = test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, y = predict(regressor_GDP, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Test set: PISA Science Scores vs GDP per capita (constant 2005 US$)') +
  xlab('GDP per capita (constant 2005 US$)') +
  ylab('PISA Science Scores')


#Non-Linear Transformation i^2 on GDPpc
nonlinear_regressor_GDP = lm(formula = `PISA: Mean performance on the science scale [LO.PISA.SCI]` ~ `GDP per capita (current US$) [NY.GDP.PCAP.CD]` +  I(`GDP per capita (current US$) [NY.GDP.PCAP.CD]`^2),
                             data = training_set)
summary(nonlinear_regressor_GDP)

summary(nonlinear_regressor_GDP)$sigma 
summary(nonlinear_regressor_GDP)$r.sq

# Make predictions and compute the R2, RMSE and MAE
predictions_non_lin <- nonlinear_regressor_GDP %>% predict(test_set)
data.frame( R2 = R2(predictions_non_lin, test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            RMSE = RMSE(predictions_non_lin, test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            MAE = MAE(predictions_non_lin, test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`))



#Non-Linear Transformation i^2 on GDPpc visualization: `training set` vs test set
ggplot() +
  geom_point(aes(x = training_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]` , y = training_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, y = predict(nonlinear_regressor_GDP , newdata = training_set)),
            colour = 'blue') +
  ggtitle('Training set: PISA Science Scores vs GDP per capita (constant 2005 US$)') +
  xlab('GDP per capita (constant 2005 US$)') +
  ylab('PISA Science Scores')

library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]` , y = test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, y = predict(nonlinear_regressor_GDP, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Test set: PISA Science Scores vs GDP per capita (constant 2005 US$)') +
  xlab('GDP per capita (constant 2005 US$)') +
  ylab('PISA Science Scores')



#Polynomial degree 3 regression on GDP

poly_reg_GDP = lm(formula = `PISA: Mean performance on the science scale [LO.PISA.SCI]` ~ poly(`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, degree = 3, raw = T) ,
              data = training_set)

summary(poly_reg_GDP)

summary(poly_reg_GDP)$sigma 
summary(poly_reg_GDP)$r.sq

# Make predictions and compute the R2, RMSE and MAE
predictions_poly <- poly_reg_GDP %>% predict(test_set)
data.frame( R2 = R2(predictions_poly , test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            RMSE = RMSE(predictions_poly , test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            MAE = MAE(predictions_poly , test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`))

#Non-Linear Transformation i^3 on GDPpc visualization: `training set` vs test set
ggplot() +
  geom_point(aes(x = training_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]` , y = training_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, y = predict(poly_reg_GDP , newdata = training_set)),
            colour = 'blue') +
  ggtitle('Training set: PISA Science Scores vs GDP per capita (constant 2005 US$)') +
  xlab('GDP per capita (constant 2005 US$)') +
  ylab('PISA Science Scores')

library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]` , y = test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
             colour = 'red') +
  geom_line(aes(x = training_set$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, y = predict(poly_reg_GDP, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Test set: PISA Science Scores vs GDP per capita (constant 2005 US$)') +
  xlab('GDP per capita (constant 2005 US$)') +
  ylab('PISA Science Scores')









#Scaling Data for interaction terms
ds$Government_expenditure_scaled = scale(ds$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`)
ds$GDPpc_scaled = scale(ds$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`)
set.seed(123)
split = sample.split(ds$`PISA: Mean performance on the science scale [LO.PISA.SCI]`, SplitRatio = 0.8)
training_set = subset(ds, split == TRUE)
test_set = subset(ds, split == FALSE)


#Multiple Linear Regression
multi_regressor = lm(formula = `PISA: Mean performance on the science scale [LO.PISA.SCI]` ~ Government_expenditure_scaled + GDPpc_scaled,
                     data = training_set)
summary(multi_regressor)

predictions_multi <- multi_regressor %>% predict(test_set)
data.frame( R2 = R2(predictions_multi  , test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            RMSE = RMSE(predictions_multi  , test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            MAE = MAE(predictions_multi , test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`))

#RSE
summary(multi_regressor)$sigma 
summary(multi_regressor)$r.sq

#Interaction terms 
interaction_regressor = lm(formula = `PISA: Mean performance on the science scale [LO.PISA.SCI]` ~ Government_expenditure_scaled * GDPpc_scaled,
                           data = training_set)
summary(interaction_regressor)

predictions_inter <- interaction_regressor %>% predict(test_set)
data.frame( R2 = R2(predictions_inter  , test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            RMSE = RMSE(predictions_inter  , test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`),
            MAE = MAE(predictions_inter , test_set$`PISA: Mean performance on the science scale [LO.PISA.SCI]`))

#RSE
summary(interaction_regressor)$sigma 
summary(interaction_regressor)$r.sq

#Decision Tree  Regression

#1. Creating dataframe


dataframe_reg <- data.frame(ds)

#2. Creating test set and training set on dataframe_reg

set.seed(123)
split = sample.split(dataframe_reg$PISA..Mean.performance.on.the.science.scale, SplitRatio = 0.6)
df_training_set1 = subset(dataframe_reg, split == TRUE)
df_test_set1 = subset(dataframe_reg, split == FALSE)

#3 Creating decision tree based on 2 classifiers - GDP pp and expenditure
library(tree)
tree.dataframe_reg <- tree(formula = PISA..Mean.performance.on.the.science.scale..LO.PISA.SCI. ~ 
                             #GNIpercapita_scaled 
                             +GDPpc_scaled
                           +Government_expenditure_scaled,
                           #pri_edu_funding_scaled
                           #sec_edu_funding_scaled,
                           data = df_training_set1)

summary(tree.dataframe_reg)$r.sq
plot(tree.dataframe_reg)
text(tree.dataframe_reg , pretty = 0)

predictions_tree <- tree.dataframe_reg %>% predict(df_test_set1)
data.frame( R2 = R2(predictions_tree , df_test_set1$PISA..Mean.performance.on.the.science.scale..LO.PISA.SCI.),
            RMSE = RMSE(predictions_tree , df_test_set1$PISA..Mean.performance.on.the.science.scale..LO.PISA.SCI.),
            MAE = MAE(predictions_tree , df_test_set1$PISA..Mean.performance.on.the.science.scale..LO.PISA.SCI.))

#Applying bagging
library('randomForest')
set.seed(123)
bag.dataframe_reg <- randomForest(formula = PISA..Mean.performance.on.the.science.scale..LO.PISA.SCI. ~ 
                                    #GNIpercapita_scaled 
                                    +GDPpc_scaled
                                  +Government_expenditure_scaled,
                                  #pri_edu_funding_scaled
                                  #sec_edu_funding_scaled,
                                  data = df_training_set1, importance = TRUE, mtry = 4)
summary(bag.dataframe_reg )
plot(bag.dataframe_reg )

yhat.bag <- predict(bag.dataframe_reg, newdata = df_test_set1)
abline(0,1)
df_test_set1
mean((yhat.bag - df_test_set1))
