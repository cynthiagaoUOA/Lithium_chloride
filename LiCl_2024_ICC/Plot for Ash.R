dataframe <- tibble(
  value = c(2.43, 5.59, 4.81, 0.42, 4.98, 29.48, 36.62, 26.31, 35.27, 24.61),
  donorID = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
  condition = c("G-Rex","G-Rex","G-Rex","G-Rex","G-Rex", 
                "Polystyrene plate","Polystyrene plate","Polystyrene plate","Polystyrene plate","Polystyrene plate"),
)


library(tidyverse)
library(ggplot2)

summariseddata<- dataframe %>% 
  group_by(condition) %>% 
  summarise(mean=mean(value), sd= sd(value), se=sd/sqrt(n()))

dataframe$donorID <- factor(dataframe$donorID)
 

ggplot()+
  geom_bar(
    data=summariseddata, aes(x=condition, y= mean), stat="identity", fill="azure3")+
  geom_errorbar(
    data=summariseddata, 
    aes(x=condition, ymin= mean-se, ymax= mean+se), width=0.2, color="black", linewidth= 0.7)+
  geom_point(data=dataframe, aes(x=condition, y=value, color=donorID), size=1.6, position=position_nudge(0.02), alpha=1) +
  theme_classic()  +xlab(NULL) +ylab("Percent CD89+ of adherent fraction") +scale_y_continuous(limits = c(0, 50),expand = c(0, 0))
  
  