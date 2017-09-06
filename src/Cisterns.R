#This script creates figure S3, cisterns

#Required packages:
library(ggplot2)
library(ggtern)
library(dplyr)

#Set project directory
setwd("E:/TrianglR/WaterBudgetTriangle")

#load dataset
Cs <- read.csv("data/Cisterns.csv")

#set color palette to color-blind-friendly
cbPalette <- c("#D55E00", #orange-red
               "#0072B2", #deepblue
               "#999999", #gray  
               "#000000", #black"
               "#009E73", #kellygreen
               "#E69F00", #yellow-orange
               "#F0E442", #lemonyellow
               "#CC79A7",  #pink
               "#56B4E9" #skyblue
) 

#plot on ternary diagram
pdf("results/FigS3Cisterns.pdf", height = 4, width = 6)
ggtern(data= Cs, aes(Q, I, ET)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
    Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") +
  geom_point(aes(color = Type, size = Rainfall.mm.), alpha = 0.3) +
  geom_mask() + 
  geom_point(aes(size = Rainfall.mm., color = Type, shape = Type)) + scale_shape_manual(values = c(16,7,3))+
  scale_size_area(name = "Precipitation\n(mm)") +
  scale_color_manual(values = cbPalette)
dev.off()

ggsave("results/FigS3Cisterns.png", height = 4, width = 6, dpi = 900, device = "png")
ggtern(data= Cs, aes(Q, I, ET)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") +
  geom_point(aes(color = Type, size = Rainfall.mm.), alpha = 0.3) +
  geom_mask() + 
  geom_point(aes(size = Rainfall.mm., color = Type, shape = Type)) + scale_shape_manual(values = c(16,7,3))+
  scale_size_area(name = "Precipitation\n(mm)") +
  scale_color_manual(values = cbPalette)
dev.off()

#calculate mean values            
mean(Cs$Q)
mean(Cs$I)
mean(Cs$ET)

#calculate median values
median(Cs$Q)
median(Cs$I)
median(Cs$ET)

#Get quantiles (round to nearest 5%)
round(quantile(Cs$Q/5,c(0.05,0.95)))*5
round(quantile(Cs$I/5,c(0.05,0.95)))*5
round(quantile(Cs$ET/5,c(0.05,0.95)))*5

#Get confidence intervals of dataset
#How many times do you want to sample?
R <- 1000
m <- numeric(R) #initialize vector of means

#resample Q
set.seed(1)
for(i in 1:R) {
  s <- sample(Cs$Q, size = length(Cs$Q), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#find the 5th and 95th percentiles of the distribution of means
#round to nearest 1%
round(quantile(m, c(0.05,0.95)))

#resample I
set.seed(1)
for(i in 1:R) {
  s <- sample(Cs$I, size = length(Cs$I), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#find the 5th and 95th percentiles of the distribution of means
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for(i in 1:R) {
  s <- sample(Cs$ET, size = length(Cs$ET), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#find the 5th and 95th percentiles of the distribution of means
round(quantile(m, c(0.05,0.95)))
