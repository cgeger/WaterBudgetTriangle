#This script creates figure 4, comparing weighing lysimeters, lined and unlined bioretention cells, and bioretention models and constructed wetlands.

#Required packages:
library(ggplot2)
library(ggtern)
library(dplyr)

#Set project directory
setwd("E:/TrianglR/WaterBudgetTriangle")

#load dataset
BR <- read.csv("data/Bioretention.csv")
str(BR)

#Create category factors
levels(BR$Type)
BR$Type <- factor(BR$Type, levels = c("Short-term or Event-scale estimate",
                                      "Lysimeters",
                                      "Unlined Cell with underdrain",
                                      "Lined Cell with underdrain",
                                      "Undersized Retrofit",
                                      "Unlined Cell with ET model", 
                                      "Model",
                                      "Calculated estimate" ))
BR <- BR %>% arrange(Type)
summary(BR$Type)

#set color palette to color-blind-friendly
cbPalette <- c("#0072B2", #deepblue
               "#009E73", #kellygreen
               "#0072B2", #deepblue
               "#56B4E9", #skyblue
               "#CC79A7", #pink
               "#E69F00", #yellow-orange
               "#D55E00", #orange-red
               "#000000", #black"
               "#999999", #gray 
               "#F0E442") #lemonyellow"
               
#plot on ternary diagram
pdf("results/Fig4Bioretention.pdf", width = 8)
ggtern(data = BR, aes(Q,I,ET)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") +
  geom_mask() +
  labs(size = "Precipitation (mm)") +
  geom_point(aes(color = Type, shape = Type, size = P, alpha = Alpha)) +
  scale_colour_manual(values = cbPalette) +
  scale_shape_manual(values = c(3,16,16,16,16,16,7,7)) +
  scale_alpha_continuous(limits = c(0,1), guide = F)
dev.off()

#A subset of the bioretention dataset is used to calculate summary statistics
#load subset of bioretention values (long-term measurements only)
BRsubset <- BR[c(60:69),]

#Calculate summary statistics: mean, median and count
BRsubset %>% 
  summarize(avgQ = mean(Q), avgI = mean(I), avgET = mean(ET),
            medQ = median(Q), medI = median(I), medET = median(ET),
            n = n())
#Get quantiles (rounded to nearest 5%)
round(quantile(BRsubset$Q/5,c(0.05,0.95)))*5
round(quantile(BRsubset$I/5,c(0.05,0.95)))*5
round(quantile(BRsubset$ET/5,c(0.05,0.95)))*5

#Get confidence intervals of dataset
#How many times do you want to sample?
R <- 1000
m <- numeric(R) #initialize vector of means

#resample Q
set.seed(1)
for (i in 1:R) {
  s <- sample(BRsubset$Q, size = length(BRsubset$Q), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample I
set.seed(1)
for (i in 1:R) {
  s <- sample(BRsubset$I, size = length(BRsubset$I), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for (i in 1:R) {
  s <- sample(BRsubset$ET, size = length(BRsubset), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))
