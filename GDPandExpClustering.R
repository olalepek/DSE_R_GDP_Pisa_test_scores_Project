library("readxl")
install.packages('factoextra')
library(factoextra)


dataset <- read_excel("Data_Extract_From_Education_Statistics_-_All_Indicators_2015.xlsx")

# Clearing data, removing all the countries that are having missing value in any of the below indicators columns


ds <-dataset[complete.cases( 
  dataset$`PISA: Mean performance on the science scale [LO.PISA.SCI]`,
  dataset$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, 
  dataset$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`), ]

#Setting up countries as a row names
df<- data.frame(ds)
rownames(df) <- df$Country.Name


x <- df[,c(11,10,9)]
x <- scale(x)

df<- data.frame(x)


set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(x, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

#Applying the kmeans to dataset
set.seed(29)
kmeans = kmeans(x, 6, iter.max=100, nstart =50)
y_kmeans = kmeans$cluster
kmeans


fviz_cluster(kmeans, data = x)


distance <- get_dist(x)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

library(tidyverse)
x %>%
  as_tibble() %>%
  mutate(cluster = y_kmeans,
         state = row.names(x)) %>%
  ggplot(aes(GDP.per.capita..current.US....NY.GDP.PCAP.CD., PISA..Mean.performance.on.the.science.scale..LO.PISA.SCI., color = factor(cluster), label = state)) +
  geom_text()

#total within-cluster sum of squares
kmeans$tot.withinss

# Visualizing clusters

require(ggplot2)


library(cluster)
clusplot(x,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = TRUE,
         span = FALSE,
         main = paste('Clusters of countries'),
         xlab = 'Expenditure as % of GDP',
         ylab = 'GDP per capita')
kmeans


#Hierarchical clustering



hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

heatmap(x, scale = "none")

par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage")
plot(hc.average, main = "Average Linkage",
     xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage",
     xlab = "", sub = "", cex = .9)

hc.pruned <- cutree(hc.complete, 6)
plot(hc.pruned)

hc.pruned

#Visualizing 
library(tidyverse)
x %>%
  as_tibble() %>%
  mutate(cluster = hc.pruned,
         state = row.names(x)) %>%
  ggplot(aes(GDP.per.capita..current.US....NY.GDP.PCAP.CD., Government.expenditure.on.education.as...of.GDP......SE.XPD.TOTL.GD.ZS., color = factor(cluster), label = state)) +
  geom_text()

hc.pruned
library(tidyverse)
df %>%
  mutate(cluster = hc.pruned) %>%
  head

#Visualizing with Clusplot

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
         xlab = 'GDP per capita',
         ylab = 'PISA TEST Science Scores')

