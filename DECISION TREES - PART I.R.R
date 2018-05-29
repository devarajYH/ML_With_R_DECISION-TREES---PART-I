setwd("C:/Users/dhirannavara/Desktop/Acadgild")

getwd()
IrisDecisionTree<-function()
{
  str(iris)
  
  library("tree")
  
  modelRegTree<-tree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris)
  
  plot(modelRegTree)
  text(modelRegTree,pretty = 0,cex=0.75)
  
}


IrisDecisionTree()