#we need to load the following packages
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#built-in R data set USArrests
df <- USArrests

#To remove any missing value that might be present in the data
df <- na.omit(df)

#scaling/standardizing the data using the R function scale
df <- scale(df)
print(head(df))

#default distance computed is the Euclidean
distance <- get_dist(df)

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
print(k2)

func1 = function()
{
  #for visualizing a distance matrix use fviz()
  print(fviz_dist(distance, gradient = list(low = "orange", mid = "white", high = "green")))
  
}
func2 = function()
{
  ##We can also view our results by using fviz_cluster
  
  # 1.) Show points only
  s1 = fviz_cluster(k2, data = df, ellipse.type = "norm", geom = "point")
  
  # 2.) Show text only
  s2 = fviz_cluster(k2, data = df, ellipse.type = "norm", geom = "text")
  
  library(gridExtra)
  print(grid.arrange(s1,s2, nrow = 2))
}

func3 = function()
{
  #We can execute the same process for 3, 4, and 5 clusters
  k3 <- kmeans(df, centers = 3, nstart = 25)
  k4 <- kmeans(df, centers = 4, nstart = 25)
  k5 <- kmeans(df, centers = 5, nstart = 25)
  
  # plots to compare
  p1 <- fviz_cluster(k2, geom = "point", data = df,main = "k = 2",ellipse.type = "norm") 
  p2 <- fviz_cluster(k3, geom = "point",  data = df,main = "k = 3",ellipse.type = "norm") 
  p3 <- fviz_cluster(k4, geom = "point",  data = df,main = "k = 4",ellipse.type = "norm") 
  p4 <- fviz_cluster(k5, geom = "point",  data = df,main = "k = 5",ellipse.type = "norm") 
  
  library(gridExtra)
  print(grid.arrange(p1, p2, p3, p4, nrow = 2))
  
}
func4 = function()
{
  #The three most popular methods for determining the optimal clusters, which includes:
  
  #1.)Elbow method
  set.seed(123)
  oc1 = fviz_nbclust(df, kmeans, method = "wss")
  #2.)Silhouette method
  oc2 = fviz_nbclust(df, kmeans, method = "silhouette")
  #3.)Gap statistic
  set.seed(123)
  gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                      K.max = 10, B = 50)
  # Print the result
  print(gap_stat, method = "firstmax")
  oc3 = fviz_gap_stat(gap_stat)
  
  library(gridExtra)
  print(grid.arrange(oc1,oc2,oc3, nrow = 2))
  
}

func5 = function()
{
  # Compute k-means clustering with k = 4
  set.seed(123)
  result <- kmeans(df, 4, nstart = 25)
  print(result)
  print(fviz_cluster(result,geom = "point",  data = df,main = "Final for k=4",ellipse.type = "norm")) 
  
}
print("How you want to visualize the data:  
      Press: ")
print("1. To visualize a distance matrix  
       2. Viewing k-means clustering results 
       3. Comparison of plots for centroids 2,3,4 and 5
       4. Methods of determining optimal clusters
       5. On the basis of optimal cluster,final result
       6. exit
      ")


while (TRUE)
{
  response = readline(prompt = "Enter a number")
  if (response == '1')
  {
    func1()
  }else if (response == '2')
  {
    func2()
  }else if (response == 3)
  {
    func3()
  }else if (response == 4)
  {
    func4()
  }else if (response == 5)
  {
    func5()
  }else if(response == 6)
  {
    print("Terminate")
    break
  }
}
  





