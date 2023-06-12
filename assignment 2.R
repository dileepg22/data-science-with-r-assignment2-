library(tidyverse)
library(rvest)

#a.
data("iris")
setosa <- iris[iris$Species=="setosa",]
versicolor <- iris[iris$Species=="versicolor",]
virginica <- iris[iris$Species=="virginica",]

boxplot(setosa$Sepal.Length, setosa$Sepal.Width,setosa$Petal.Length,setosa$Petal.Width,
        names = c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width"),
        main = "Boxplot of setosa Specie")

boxplot(versicolor$Sepal.Length, versicolor$Sepal.Width,versicolor$Petal.Length,versicolor$Petal.Width,
        names = c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width"),
        main = "Boxplot of versicolor Specie")

boxplot(virginica$Sepal.Length, virginica$Sepal.Width,virginica$Petal.Length,virginica$Petal.Width,
        names = c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width"),
        main = "Boxplot of virginica Specie")

index<- c(1:150)
for(i in 1:150){
  if(iris$Species[i]=="setosa"){
    index[i]=1
  }
 else if(iris$Species[i]=="versicolor"){
    index[i]=2
  }
 else{
   index[i]=3
 }
}

plot(iris$Sepal.Length, iris$Petal.Length,
     xlab = "Sepal Length", ylab = "Petal Length", 
     pch = 16, col = index)
legend("topright", pch = 16, col = c(1,2,3),
       legend = c("setosa", "versicolor","virginica"))

#for virginica specie both sepal length and petal length are high.
#for versicolor sepal length is more scattered.

#c.
boxplot(incidents ~ type, data = ships, xlab = "Ship Type", ylab = "Number of Incidents",
                main = "Damage Incidents by Ship Type", col = c("red", "blue", "green", "orange", "purple"))
#d.
url <- "https://stats.stackexchange.com/questions?tab=Votes"
site <- read_html(url)
nameData <- site %>% html_nodes(".s-post-summary--content-title") %>% html_text()
fullData <- site %>% html_nodes(".s-post-summary--stats-item-number") %>% html_text()
t=1
p=1
q=1
views<- c(1:15)
for(i in 1:45){
   if(i%%3==0){
         views[t]<- fullData[i]
      #print(t)
      t<-t+1
      }
   }
answers<- c(1:15)
for(i in 1:45){
  if(i%%2==0 & i%%3){
   answers[p]<- fullData[i]
    #print(p)
    p<-p+1
  }
}
votes<- c(1:15)
for(i in 1:45){
  if(i%%2 & i%%3){
   votes[q]<- fullData[i]
    #print(q)
    q<-q+1
  }
}

DataFRAME<- data.frame("The title of the questions"=nameData,"The number of views"=views,
                       "The number of answers"=answers,"The notes of votes"=votes)
View(DataFRAME)

#e.
tabs=100
#already taken a tablet
days <- 1
Total <- function(n,p){
  choo= rbinom(n =1, size = 1, prob = p/n)
   p<- p+1
  if(!choo){
    Total(n,p)
  }
  else {
    return(p-1) #days passed before it
  }
}
total <-range(1:1000)
add=0
for(i in 1:1000){
  total[i] <-  Total(tabs,days)
  add <- add+ total[i]
}
avg.Days <- add/1000
print(avg.Days)
print(total)
