#This script creates figure 1, comparing natural wetlands, lakes and constructed wetlands.

#Required packages:
library(ggplot2)
library(ggtern)
library(dplyr)

#Set project directory
setwd("E:/TrianglR/WaterBudgetTriangle")

#load dataset
W <- read.csv("data/Wetlands.csv")
str(W)

#Create category factors
W$Category <- factor(W$Category, levels = c("Natural wetland",
                                            "Natural lake",
                                            "Constructed wetland",
                                            "Model (natural)",
                                            "Model (constructed)"))
#Set color palette
cbPalette <- c("#009E73", #kellygreen
               "#56B4E9", #skyblue
               "#D55E00", #orange-red
               "#CC79A7", #pink
               "#000000", #black
               "#E69F00", #yellow-orange
               "#0072B2", #deepblue
               "#F0E442", #lemonyellow
               "#999999") #gray 
               
#plot on Ternary diagram
pdf("results/Fig1Wetlands.pdf", height = 4, width = 5)
ggtern(data= W, aes(Q, I, ET, color = Category)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") +
  geom_mask() + geom_point(aes(shape = Category), size = 2.5) + 
  scale_colour_manual(values=cbPalette)
dev.off()

#A subset of the wetlands dataset is used to calculate summary statistics
#load subset of wetland values
W.s <- read.csv("data/Wetlands_subset.csv")

#Calculate summary statistics: mean, median and count
W.s %>% group_by(Category) %>%
  summarize(avgQ = mean(Q),avgI = mean(I),avgET = mean(ET), 
            medQ = median(Q), medI = median(I), medET = median(ET),
            n = n())

#Calculate quantile ranges and confidence intervals
Wsub <- W.s %>%
  filter(Category == "Constructed wetland") #set category of interest
Wsub

#Get quantile ranges (rounded to nearest 5%)
round(quantile(Wsub$Q/5,c(0.05,0.95)))*5 #Q
round(quantile(Wsub$I/5,c(0.05,0.95)))*5 #I
round(quantile(Wsub$ET/5,c(0.05,0.95)))*5 #ET

#Get confidence intervals around mean values
#How many times do you want to sample?
R <- 1000
m <- numeric(R) #initialize vector of means

#resample Q
set.seed(1)
for(i in 1:R) {
  s <- sample(Wsub$Q, size = length(Wsub$Q), replace = T)
  m[i] <- mean(s)
}
hist(m) #histogram of resampled means
mean(m) #mean of means
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample I
set.seed(1)
for(i in 1:R) {
  s <- sample(Wsub$I, size = length(Wsub$I), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of I distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for(i in 1:R) {
  s <- sample(Wsub$ET, size = length(Wsub$ET), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of ET distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))
