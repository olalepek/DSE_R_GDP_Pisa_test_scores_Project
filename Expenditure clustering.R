library("readxl")

dataset <- read_excel("Data_Extract_From_Education_Statistics_-_All_Indicators_2015.xlsx")

# Clearing data, removing all the countries that are having missing value in any of the below indicators columns


ds <-dataset[complete.cases( 
  dataset$`PISA: Mean performance on the science scale [LO.PISA.SCI]`,
  dataset$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, 
  dataset$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`), ]

#Setting up countries as a row names
df<- data.frame(ds)
rownames(df) <- df$Country.Name


x <- df[,c(11,9)]
x = scale(x)



set.seed(6)
wcss = vector()
for (i in 1:15) wcss[i] = sum(kmeans(x, i)$withinss)
plot(1:15,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

#Applying the kmeans to dataset
set.seed(29)
kmeans = kmeans(x, 6, iter.max=300, nstart =4)
y_kmeans = kmeans$cluster
kmeans
#total within-cluster sum of squares
kmeans$tot.withinss

# Visualizing clusters with cusplot
library(cluster)
clusplot(x,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of countries'),
         xlab = 'Expenditure as % of GDP',
         ylab = 'PISA TEST Science Scores'
        )

#Visualizing clusters 
library(tidyverse)
x %>%
  as_tibble() %>%
  mutate(cluster = y_kmeans,
         state = row.names(x)) %>%
  ggplot(aes(Government.expenditure.on.education.as...of.GDP......SE.XPD.TOTL.GD.ZS., PISA..Mean.performance.on.the.science.scale..LO.PISA.SCI., color = factor(cluster), label = state)) +
  geom_text()



#Hierarchical clustering

dendrogram = hclust(d = dist(x, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Countries',
     ylab = 'Euclidean distances')


hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")


par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage")
plot(hc.average, main = "Average Linkage",
       xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage",
       xlab = "", sub = "", cex = .9)

hc.pruned <- cutree(hc.complete, 5)

plot(hc.pruned)

hc.pruned



#Visualizing clusters 
library(tidyverse)
x %>%
  as_tibble() %>%
  mutate(cluster = hc.pruned,
         state = row.names(x)) %>%
  ggplot(aes(Government.expenditure.on.education.as...of.GDP......SE.XPD.TOTL.GD.ZS., PISA..Mean.performance.on.the.science.scale..LO.PISA.SCI., color = factor(cluster), label = state)) +
  geom_text()

#Visualizing clusters  with clusplot

library(cluster)
clusplot(x,
         hc.pruned,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of countries'),
         xlab = 'Expenditure as % of GDP',
         ylab = 'PISA TEST Science Scores')

