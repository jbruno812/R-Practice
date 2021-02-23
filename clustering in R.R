library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms and visualizations


df <- USArrests
df <- na.omit(df)
df <- scale(df) # standardize measures

head(df)

distance <- get_dist(df) # computes distance matrix between rows
fviz_dist(distance, gradient = list(low="blue", mid="white", high = "red")) # visualizing distance

k2 <- kmeans(df, centers = 2, nstart = 25) 
str(k2) # shows structure

k2 
fviz_cluster(k2, data = df)

df %>%
  as_tibble()%>%
  mutate(cluster=k2$cluster, state=row.names(USArrests))%>%
  ggplot(aes(UrbanPop, Murder, color=factor(cluster), label=state))+geom_text() # Pairwise scatterplots

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = df)+ggtitle("k=2")
p2 <- fviz_cluster(k3, geom = "point", data = df)+ggtitle("k=3")
p3 <- fviz_cluster(k4, geom = "point", data = df)+ggtitle("k=4")
p4 <- fviz_cluster(k5, geom = "point", data = df)+ggtitle("k=5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow=2)

set.seed(123)

wss <- function(k) {kmeans(df, k, nstart=10)$tot.withinss}

k.values <- 1:15

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type = "b", pch=19, frame=FALSE,
     xlab="number of clusters k", 
     ylab = "total within-clusters sum of squares")
# looks like 4 clusters is best

set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)

USArrests %>%
  mutate(cluster=final$cluster)%>%
  group_by(cluster)%>%
  summarise_all("mean")