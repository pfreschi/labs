#lab 2
#you can find my slides here 
#https://docs.google.com/presentation/d/1GSqoO9xk4hDxuumeuYKlC2eA0TYqK2EBEaLqW6m03vQ/edit?usp=sharing

#ggplot2 cheat sheet
#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

#load in libraries
library(ggplot2)

#load in built in dataset
data <- iris

#basic scatter plot
g<-ggplot(data, aes(Sepal.Length, Sepal.Width))+geom_point(color="blue")

#lets get a title and axis labels in the mix
g<-g+labs(x="Sepal Length (cm)", y="Sepal Width (cm)", title='Sepal Length vs Sepal Width')

#lets say we want to then color the graph by species of iris
#color is then specified in aes because it is a function of the dataset
#otherwise color would be specified in geom_point()
g<-ggplot(data, aes(Sepal.Length, Sepal.Width, color=factor(Species)))+geom_point()
g<-g+labs(x="Sepal Length (cm)", y="Sepal Width (cm)", title='Sepal Length vs Sepal Width')

#legend title is ugly
g<- g+theme(legend.title=element_blank())

#and the legend dots are tiny
g+guides(colour = guide_legend(override.aes = list(size=4)))

#much better

#shall we change the background color?
g<- g+theme(panel.background = element_rect(fill = 'grey75'))

#hm you know I changed my mind about all the species being on one graph, lets put it on three
g<- g+facet_wrap(~Species, nrow=1)

#lets add a line to those plots!
g<-g+stat_smooth()

#that isn't quite what we were looking for, lets do a best fit instead
g<-g+stat_smooth(method="lm", se=FALSE, col="black")

#you can get all sorts of funky themes in the mix
#https://github.com/jrnold/ggthemes
library(ggthemes)
g<- g+theme_economist()+scale_colour_economist()
#NEAT

#plotting
g
