rm(list=ls())
set.seed(200)
attach(mtcars)
head(mtcars)
data=mtcars[,c(-7,-8)]

wss=(nrow(data)-1)*sum(apply(data,2,var));wss
for(i in 2:10){
  wss[i]=sum(kmeans(data,centers = i)$withinss)
}
wss
plot(1:10,wss,type='b',xlab="no. of clusters",ylab='within sum of square',col="red")
#From the graph k=4
#
d=dist(data,method = "euclidean")

fit=hclust(d,method='ward.D')
fit
plot(fit,label=rownames(mtcars),cex=0.5)

groups=cutree(fit,k=4)

rect.hclust(fit,h=4,border = 2)
fit1=kmeans(data,4)
fit1
aggregate(data,by=list(fit1$cluster),FUN=mean)
