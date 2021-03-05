library(help = "datasets")

#  trees Database
data(trees)
head(trees)


# list types for each attribute
sapply(trees, class)

# distribution of class variable

y <- trees$Girth
cbind(freq=table(y), percentage=prop.table(table(y))*100)

y <- trees$Height
cbind(freq=table(y), percentage=prop.table(table(y))*100)

y <- trees$Volume
cbind(freq=table(y), percentage=prop.table(table(y))*100)

# summarize the dataset
summary(trees)


# calculate standard desviation for all attributes
sapply(trees[,1:3], sd)

# calculate a correlation matrix for numeric variables
library(corrplot)
correlations <- cor(trees[,1:3])
print(correlations)
corrplot(correlations, method="circle")



## Histograms
data("trees")
# create histograms for each attribute
par(mfrow=c(1,3))
for(i in 1:3) {
  hist(trees[,i], main=names(trees)[i], xlab="trees", ylab="Mediciones")  
}

## Density Plots
library(lattice)
par(mfrow=c(1,1))
for(i in 1:1) {
  plot(density(trees[,i]),main ="Trees" , xlab=names(trees)[i])
}

par(mfrow=c(1,1))
for(i in 2:2) {
  plot(density(trees[,i]),main ="Trees" , xlab=names(trees)[i])
}

par(mfrow=c(1,1))
for(i in 3:3) {
  plot(density(trees[,i]),main ="Trees" , xlab=names(trees)[i])
}


# Create separate boxplots for each attribute
par(mfrow=c(1,3))
for(i in 1:3) {
  boxplot(trees[,i], main=names(trees)[i])
}


plot(trees[,1], trees[,3], xlab = names(trees)[1], ylab = names(trees)[3])
plot(trees[,2], trees[,1], xlab = names(trees)[2], ylab = names(trees)[1])
plot(trees[,3], trees[,2], xlab = names(trees)[3], ylab = names(trees)[2])


