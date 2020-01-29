
#importing Libraries
library(GGally)
library(knitr)
library(class)
library(tidyverse)

 ##View the Dataxset 
head(iris)
tail(iris)
summary(iris)
str(iris)





# Histogram for each species
iris2 %>%
  gather(Attributes, Value, 1:4) %>%
  ggplot(aes(x=Value, fill=Attributes)) +
  geom_histogram(colour="black") +
  facet_wrap(~Species) +
  theme_bw() +
  labs(x="Values", y="Frequency",
       title="Iris data set",
       subtitle="Histogram for each species") +
  theme(legend.title=element_blank(),
        legend.position="bottom")

  
         
         ```{r fig.align='center', message=FALSE, warning=FALSE}
         # Density plot for each species
         iris2 %>%
           gather(Attributes, value, 1:4) %>%
           ggplot(aes(x=value, fill=Species)) +
           geom_density(colour="black", alpha=0.5) +
           facet_wrap(~Attributes, scales="free_x") +
           labs(x="Values", y="Density",
                title="Iris data set",
                subtitle="Density plot for each attribute") +
           theme_bw() +
           theme(legend.position="bottom",
                 legend.title=element_blank())
         
         
         
         ```{r fig.align='center', message=FALSE, warning=FALSE}
         
         # Violin plot for each attribute
         iris2 %>%
           gather(Attributes, value, 1:4) %>%
           ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
           geom_violin(show.legend=FALSE) +
           geom_boxplot(width=0.05, fill="white") +
           labs(title="Iris data set",
                subtitle="Violin plot for each attribute") +
           theme_bw() +
           theme(axis.title.y=element_blank(),
                 axis.title.x=element_blank())
         ```
         
         ```{r fig.align='center', message=FALSE, warning=FALSE}
         # Boxplot for each attribute
         iris2 %>%
           gather(Attributes, value, 1:4) %>%
           ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
           geom_boxplot(show.legend=FALSE) +
           labs(title="Iris data set",
                subtitle="Boxplot for each attribute") +
           theme_bw() +
           theme(axis.title.y=element_blank(),
                 axis.title.x=element_blank())
         ```
         
         ```{r fig.align='center', message=FALSE, warning=FALSE}
         # Scatter plot and correlations
         ggpairs(cbind(iris2, Cluster=as.factor(iris2$Species)),
                 columns=1:4, aes(colour=Cluster, alpha=0.5),
                 lower=list(continuous="points"),
                 axisLabels="none", switch="both") +
           theme_bw() 
         ```
         
         # **Data preparation** 
         
         
         ```{r message=FALSE, warning=FALSE}
         # Normalization of all columns except Species
         dataNorm <- iris
         dataNorm[, -5] <- scale(iris[, -5])
         ```
         
         ```{r message=FALSE, warning=FALSE}
         # Reproducible results
         set.seed(1234)
         
         # 70% train and 30% test
         ind <- sample(2, nrow(dataNorm), replace=TRUE, prob=c(0.7, 0.3))
         trainData <- dataNorm[ind==1,]
         testData <- dataNorm[ind==2,]

         
         # **k-NN execution**
         
         The [`knn()`](https://www.rdocumentation.org/packages/class/versions/7.3-15/topics/knn) function has the following main arguments:
           
           * `train`. Matrix or data frame of training set cases.
         
         * `test`. Matrix or data frame of test set cases. A vector will be interpreted as a row vector for a single case.
         
         * `cl`. Factor of true classifications of training set.
         
         * `k`. Number of neighbours considered.
         
         ```{r message=FALSE, warning=FALSE}
         # Execution of k-NN with k=1
         KnnTestPrediction_k1 <- knn(trainData[,-5], testData[,-5],
                                     trainData$Species, k=1, prob=TRUE)
         
         # Execution of k-NN with k=2
         KnnTestPrediction_k2 <- knn(trainData[,-5], testData[,-5],
                                     trainData$Species, k=2, prob=TRUE)
         
         # Execution of k-NN with k=3
         KnnTestPrediction_k3 <- knn(trainData[,-5], testData[,-5],
                                     trainData$Species, k=3, prob=TRUE)
         
         # Execution of k-NN with k=4
         KnnTestPrediction_k4 <- knn(trainData[,-5], testData[,-5],
                                     trainData$Species, k=4, prob=TRUE)
         ```
         
         ```{r fig.align='center', message=FALSE, warning=FALSE}
         # Empty variables
         KnnTestPrediction <- list()
         accuracy <- numeric()
         
         # From k=1 to k=100...
         for(k in 1:100){
           
           # KnnTestPrediction for each k
           KnnTestPrediction[[k]] <- knn(trainData[,-5], testData[,-5], trainData$Species, k, prob=TRUE)
           
           # Accuracy for each k   
           accuracy[k] <- sum(KnnTestPrediction[[k]]==testData$Species)/length(testData$Species)*100
           
         }
         
         # Accuracy vs Choice of k
         plot(accuracy, type="b", col="dodgerblue", cex=1, pch=20,
              xlab="k, number of neighbors", ylab="Classification accuracy", 
              main="Accuracy vs Neighbors")
         
         # Add lines indicating k with best accuracy
         abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)
         
         # Add line for max accuracy seen
         abline(h=max(accuracy), col="grey", lty=2)
         
         # Add line for min accuracy seen 
         abline(h=min(accuracy), col="grey",
                lty=2)
         ```
         
         x <- 4
         class(x)
         
         x <- c(4, "a", TRUE)
         class(x)
         
         x<- c(1,3,5)
         y<- c(3, 2, 10)
         rbind(x,y)
         
         x <- list(2, "a", "b", TRUE) 
         x[[1]]
         
         x <- 1:4
         y <- 2
          x+y
          
          x <- c(17, 14, 4, 5, 13, 12, 10) 
          x[x >= 11]<- 4
          
          
          x<- list(1, "a", TRUE, 1+4i)
          x
          m <- 1:24
          m
          dim(m) <- c(4, 6)
          m
          
          
          head(hw1_data)
          summary(hw1_data)
         
          x <- c(4, "a", TRUE)
          
          
          
          x <- c(1,3, 5)
          y <- c(3, 2, 10)
          cbind(x, y)
          
          x <- 4L
class(x)          

x <- c(3, 5, 1, 10, 12, 6)
x[x%in% 1:5]<-0

x <- list(2, "a", "b", TRUE)
x[[2]]

y <- data.frame(a = 1, b ="a")
dput(y)
dput(y, file = "y.R")
new.y<-dget("y.R")