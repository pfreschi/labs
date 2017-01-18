#load in libraries
library(ggplot2)

#load in built in dataset
data <- iris

#basic scatter plot


#lets get a title and axis labels in the mix


#lets say we want to then color the graph by species of iris
#color is then specified in aes because it is a function of the dataset
#otherwise color would be specified in geom_point()


#legend title is ugly


#and the legend dots are tiny


#much better

#shall we change the background color?


#hm you know I changed my mind about all the species being on one graph, lets put it on three


#lets add a line to those plots!


#that isn't quite what we were looking for, lets do a best fit instead


#you can get all sorts of funky themes in the mix
#https://github.com/jrnold/ggthemes
library(ggthemes)

#NEAT

#plotting
