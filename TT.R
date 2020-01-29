library(datasets)
head(iris)
?plot
plot(iris$Species)
plot(iris$Petal.Length)
plot(iris$Species,iris$Petal.Width)
plot(iris$Petal.Length, iris$Petal.Width)
plot(iris)
plot(iris$Species,iris$Petal.Width)
plot(iris$Petal.Length, iris$Petal.Width)
 colors = ("#cc0000") 
 poich =  19
 main = "Iris: Petal Length vs. Petal Width"
 xlab = "Petal length"
 ylab = "Petal Width"
plot(cos,  0, 2*pi)
plot(exp, 1, 5)
plot(dnorm, -3, +3)
 color = "red"
 lwd = 5
 main = "Standard Normal Distribution"
 xlab = "z-Scores"
 ylab = "Density"

 library(datasets)
?mtcars 
 require(graphics)
 pairs(mtcars, main = "mtcars data", gap = 1/4)
 coplot(mpg ~ disp | as.factor(cyl), data = mtcars,
        panel = panel.smooth, rows = 1)
 ## possibly more meaningful, e.g., for summary() or bivariate plots:
 mtcars2 <- within(mtcars, {
   vs <- factor(vs, labels = c("V", "S"))
   am <- factor(am, labels = c("automatic", "manual"))
   cyl  <- ordered(cyl)
   gear <- ordered(gear)
   carb <- ordered(carb)
 })
 summary(mtcars2)
head(mtcars)
head(mtcars$mpg)
head(mtcars, mpg)
barplot(mtcars$cyl)
Cylinders <- table(mtcars$cyl)
barplot(Cylinders)
plot(Cylinders)

library(iris)
head(iris)
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)
hist(iris$Species)

par(mfrow) = c(3, 1)

hist(iris$Petal.Width [iris$Species == "setosa"])
xlim = c(0,3)
breaks = 9
main = "Petal Width for Setosa" 
xlab = ""
col="red"help()


help()
?repositories()
library(KernSmooth)
