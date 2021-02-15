#Why Bar Plots Can Be The Worst
rm(list = ls())

library(datasauRus) #library containing data for demos
library(QuickEnvironment)
library(ggplot2)

data = datasaurus_dozen

summary(data)


#Plot as barplot (for simplicity will plot both DV on one)
select = c('dataset', 'y', 'se', 'dv') #the selection we'll be using

sum.y <- summarySE(data = data, measurevar = "y", groupvars = "dataset")
sum.y$dv <- 'y' 
sum.y <- sum.y[select] 

sum.x <- summarySE(data = data, measurevar = "x", groupvars = "dataset")
sum.x$y <- sum.x$x
sum.x$dv <- 'x'
sum.x <- sum.x[select]

sum <- rbind(sum.y, sum.x) #Combine the two summary dfs

ggplot(data = sum, aes(x = dv, y = y, fill = dataset)) + 
  geom_bar(stat = "identity", color = "black",  position=position_dodge()) +
  geom_errorbar(aes(ymin=(y-se), ymax=(y+se)), width = .2,  position=position_dodge(.9)) +
  coord_cartesian(ylim = c(20,60)) +
  ylab("x/y") +
  theme_classic()+
  theme(legend.position = "none")



#Let's actually look at the data to see whats happening
ggplot(data = data, aes(x = x, y = y, color = dataset)) +
  geom_point()+
  facet_wrap(~dataset, ncol = 3)+
  theme_void() + 
  theme(legend.position = "none")


#############################################################################################
#Okay let's have a look at another example
#############################################################################################
data2 <- datasauRus::box_plots
selection = c('y', 'group')

#Organize the data into long format

data.tmp.left       <- data2
data.tmp.left$y     <- data2$left
data.tmp.left$group <- 'A'
data.tmp.left       <- data.tmp.left[selection]

data.tmp.lines       <- data2
data.tmp.lines$y     <- data2$lines
data.tmp.lines$group <- 'B'
data.tmp.lines       <- data.tmp.lines[selection]

data.tmp.normal       <- data2
data.tmp.normal$y     <- data2$normal
data.tmp.normal$group <- 'C'
data.tmp.normal       <- data.tmp.normal[selection]

data.tmp.right       <- data2
data.tmp.right$y     <- data2$right
data.tmp.right$group <- 'D'
data.tmp.right       <- data.tmp.right[selection]

data.tmp.split       <- data2
data.tmp.split$y     <- data2$split
data.tmp.split$group <- 'E'
data.tmp.split       <- data.tmp.split[selection]


#Now combine the data
data2 <- rbind(data.tmp.left, data.tmp.lines, data.tmp.normal, data.tmp.right, data.tmp.split)

#and summarize
sum2  <- summarySE(data = data2, measurevar = 'y', groupvars =  'group')

ggplot(data2, aes(x = group, y = y, fill = group)) +
  geom_boxplot() +
  theme_classic()

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

ggplot(data2, aes(x = group, y = y, fill = group)) +
  geom_violin(stat = "ydensity") + 
  stat_summary(fun.data = data_summary)+
  theme_classic()

ggplot(sum2, aes(x = group, y = y, fill = group)) +
  geom_point(data=data2, aes(x = group, y = y, color = group), alpha = .4, position = 'jitter') +
  #geom_bar(stat = 'identity', alpha = .5) +
  #geom_errorbar(aes(ymin=(y-se), ymax=(y+se)), width = .2,  position=position_dodge(.9)) +
  theme_classic()
