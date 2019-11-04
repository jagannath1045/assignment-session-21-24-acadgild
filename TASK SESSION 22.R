#TASK 22 
View(epi_r)
sum(is.na(epi_r))

library(tidyverse)

epi_r <- epi_r %>%
  filter(calories < 8000) %>% 
  filter(protein<200) %>%
  filter(fat < 600) %>% 
  filter(sodium<1500) %>% # remove outliers
  na.omit() # remove rows with NA values



#a.Apply k means clustering to identify similar recepies

rec<-epi_r[, 1:6]
set.seed(12345)
reci<-kmeans(rec[,c("calories","protein","fat","sodium")], centers = 10)
reci
o<-order(reci$cluster)
head(data.frame(rec$title[o],reci$cluster[o]))


library(cluster)
clusplot(rec,reci$cluster, main = "2d representation of recipe clustering", color=TRUE, shade = TRUE, labels = 2, lines = 0)

d<-dist(rec[,-1], method = "euclidean")
d
h.fit<-hclust(d, method = "ward.D2")

plot(h.fit)

groups<-cutree(h.fit, k=7)
rect.hclust(h.fit, k=7, border = "red")

table(rec[,1], groups)

recipes<-agnes(rec, metric = "euclidian", method = 'ward.D2')


#b.Apply k means clustering to identify similar Attributes


y<-attributes(rec)
y
x<-rec[, c(2:6)]

View(x)
epi_rrr<-scale(x)
df1<-kmeans(x,7)

c<-attributes(df1)
df1$centers
df1$cluster
df1$withinss
df1$tot.withinss
df1$size



df2<-kmeans(x,8)
df2$cluster

df2$withinss
df2$tot.withinss

df2$size


df3<-kmeans(x,9)
df3$cluster

df3$withinss
df3$tot.withinss

df3$size

#c.how many unique recepies that people order often

library(dplyr)

library(data.table)
t<-unique(rec,incomparables = FALSE)


#d. what are their typical profiles

library(dplyr)
f<-distinct(rec,.keep_all = FALSE)
dim_desc(rec)
desc(rec)
tx<-desc(rec)

par(mfrow=c(2,4))
boxplot(rec$sodium)
boxplot(rec$calories)
boxplot(rec$protein)
boxplot(rec$fat)

