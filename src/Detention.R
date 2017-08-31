#This script creates figure 3, detention ponds.

#Required packages:
library(ggplot2)
library(ggtern)
library(dplyr)

#Set project directory
setwd("E:/TrianglR/WaterBudgetTriangle")

#load data
D <- read.csv("data/Detention.csv")
str(D)

#Relevel factors to desired order
levels(D$Type)
D$Type <- factor(D$Type, levels = c("Dry detention pond",
                                    "Pumped detention pond",
                                    "Detention pond model",
                                    "Intermittent constructed wetland"))
D <- D %>% arrange(Type)

#summarize mean and median loss pathway ratios for detention ponds (not constructed wetland)
D %>%
  filter(shape == "Cumulative retention (>6 months)", Type != "Intermittent constructed wetland") %>%
  summarize(avgQ = mean(Q),avgI = mean(I),avgET = mean(ET), 
            medQ = median(Q), medI = median(I), medET = median(ET),
            n = n())

#set color palette to color-blind-friendly
cbPalette <- c("#009E73", #kellygreen
               "#56B4E9", #skyblue
               "#000000", #black"
               "#E69F00", #yellow-orange
               "#999999", #gray  
               "#0072B2", #deepblue
               "#F0E442", #lemonyellow
               "#CC79A7",  #pink
               "#D55E00" #orange-red
) 

#plot on Ternary diagram precipitation in mm, Q,I and ET in %
pdf("results/Fig3Detention.pdf", width = 6, height = 4)
ggtern(data= D, aes(Q, I, ET)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") +
  labs(size = "Precipitation (mm)",
       shape = "Time period") +
  geom_mask() +
  geom_point(aes(size = P, color = Type, shape = shape, alpha = alpha))+
  scale_shape_manual(values = c(16,3))+
  scale_colour_manual(values=cbPalette)+
  scale_alpha_continuous(limits = c(0,1), guide = F)
dev.off()

#A subset of the detention dataset is used to calculate summary statistics
#get subset:
Dsub <- D %>%
  filter(shape == "Cumulative retention (>6 months)", Type != "Intermittent constructed wetland")

#Get quantile ranges (rounded to nearest 5%)
round(quantile(Dsub$Q/5,c(0.05,0.95)))*5
round(quantile(Dsub$I/5,c(0.05,0.95)))*5
round(quantile(Dsub$ET/5,c(0.05,0.95)))*5

#Get confidence intervals around mean values
#How many times do you want to sample?
R <- 1000
m <- numeric(R) #initialize vector of means

#resample Q
set.seed(1)
for(i in 1:R) {
  s <- sample(Dsub$Q, size = length(Dsub$Q), replace = T)
  m[i] <- mean(s)
}
hist(m) #histogram of resampled means
mean(m) #mean of means
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample I
set.seed(1)
for(i in 1:R) {
  s <- sample(Dsub$I, size = length(Dsub$I), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of I distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for(i in 1:R) {
  s <- sample(Dsub$ET, size = length(Dsub$ET), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of ET distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))