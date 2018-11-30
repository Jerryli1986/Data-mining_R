# Histogram for each attributes
library(gdata)
library(ggplot2)
library(reshape2)
mydata<-read.csv(file="C:/Users/Jerry/Desktop/Data Analystics and Mining/assignment/teeth(1).csv"
                ,header=TRUE, sep=",")
gg <- melt(mydata)
ggplot(gg, aes(x=value, fill=variable)) +
  geom_histogram(binwidth=1)+
  facet_grid(variable~.)