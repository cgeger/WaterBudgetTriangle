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
pdf("results/Fig4Bioretention.pdf", width = 8, height = 5)
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


ggsave(file="results/Fig4Bioretention.tif", width = 8, height = 5, device = "tiff")
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
BRsubset$sum <- BRsubset$Q + BRsubset$I + BRsubset$ET
BRsubset$Qr <- BRsubset$Q/BRsubset$sum * 100
BRsubset$Ir <- BRsubset$I/BRsubset$sum * 100
BRsubset$ETr <- BRsubset$ET/BRsubset$sum * 100

#Calculate summary statistics: mean, median and count
BRsubset %>% 
  summarize(avgQ = mean(Qr), avgI = mean(Ir), avgET = mean(ETr),
            medQ = median(Qr), medI = median(Ir), medET = median(ETr),
            n = n())
#Get quantiles (rounded to nearest 5%)
round(quantile(BRsubset$Qr/5,c(0.05,0.95)))*5
round(quantile(BRsubset$Ir/5,c(0.05,0.95)))*5
round(quantile(BRsubset$ETr/5,c(0.05,0.95)))*5

#Get confidence intervals of dataset
#How many times do you want to sample?
R <- 1000
m <- numeric(R) #initialize vector of means

#resample Q
set.seed(1)
for (i in 1:R) {
  s <- sample(BRsubset$Qr, size = length(BRsubset$Qr), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample I
set.seed(1)
for (i in 1:R) {
  s <- sample(BRsubset$Ir, size = length(BRsubset$Ir), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for (i in 1:R) {
  s <- sample(BRsubset$ETr, size = length(BRsubset$ETr), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))
