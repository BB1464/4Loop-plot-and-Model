library(ggplot2)
library(agricolae)  # for mean separation

# Creating Visualization with For Loop
for (i in 1:4) {
  p<-ggplot(iris,aes(x=iris[,5],y=iris[,i],fill=Species))+
    geom_boxplot()+
    ylab(colnames(iris[i]))+
    xlab(colnames(iris[5]))+
    theme_classic()+
    theme(text = element_text(size=12,face = 'bold',family = 'serif'))
  print(p)
}



# Building a linear model with for loop
for (i in 1:4) {
  print(paste('-----',names(iris[i]),'-----'))
  model<-lm(iris[[i]]~Species,data=iris)
  print(anova(model))
  lala=HSD.test(model,'Species',console = FALSE)
  print(lala$groups)
}
