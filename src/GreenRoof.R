#This script creates figure 6, comparing green, blue and traditional roofs.

#Required packages:
library(ggplot2)
library(ggtern)
library(dplyr)

#Set project directory
setwd("E:/TrianglR/WaterBudgetTriangle")

#load dataset
GR <- read.csv("data/GreenRoofs.csv")
str(GR)

#Set Type factor order
GR$Type
GR%>% group_by(Type) %>% summarize(n = n())
GR$Type <- factor(GR$Type, levels = c("Green Roof","Lab Scale GR", "Control Roof","Blue Roof","GR Model"))

#set transparency for some types
GR <- GR %>% arrange(Type)
GR$alpha <- c(rep(0.6,59), rep(0.9,7), rep(0.6,6), rep(1,14))

#set color palette (first 5 are used)
cbPalette <- c("#009E73", #kellygreen
               "#E69F00", #yellow-orange
               "#D55E00", #orange-red
               "#120A8F", #indigo
               "#000000", #black"
               "#0072B2", #deepblue
               "#56B4E9", #skyblue
               "#999999", #gray  
               "#F0E442", #lemonyellow
               "#CC79A7"  #pink
) 

#plot on Ternary diagram
pdf("results/Fig6GreenRoofs.pdf", width = 5, height = 4)
ggtern(data= GR, aes(Q, I, ET, color = Type)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") + 
  geom_mask() +
  geom_point(size = 3, aes(shape = Type, alpha = alpha)) +
  scale_colour_manual(values=cbPalette) + 
  scale_alpha_continuous(limits = c(0,1), guide = F)
dev.off()

#calculate summary table
GR %>% group_by(Type) %>%
  summarize(avgQ = mean(Q), avgI = mean(I), avgET = mean(ET),
            medQ = median(Q), medI = median(I), medET = median(ET),
            n = n())

#Calculate quantile ranges and confidence intervals
GRsub <- GR %>%
  filter(Type == "Green Roof") #set category of interest
GRsub

#Get quantile ranges (rounded to nearest 5%)
round(quantile(GRsub$Q/5,c(0.05,0.95)))*5 #Q
round(quantile(GRsub$I/5,c(0.05,0.95)))*5 #I
round(quantile(GRsub$ET/5,c(0.05,0.95)))*5 #ET

#Get confidence intervals around mean values
#How many times do you want to sample?
R <- 1000
m <- numeric(R) #initialize vector of means

#resample Q
set.seed(1)
for(i in 1:R) {
  s <- sample(GRsub$Q, size = length(GRsub$Q), replace = T)
  m[i] <- mean(s)
}
hist(m) #histogram of resampled means
mean(m) #mean of means
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample I
set.seed(1)
for(i in 1:R) {
  s <- sample(GRsub$I, size = length(GRsub$I), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of I distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for(i in 1:R) {
  s <- sample(GRsub$ET, size = length(GRsub$ET), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of ET distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))
