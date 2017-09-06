#This script creates figure S4, sewer exfiltration budgets

#Required packages:
library(ggplot2)
library(ggtern)
library(dplyr)

#Set project directory
setwd("E:/TrianglR/WaterBudgetTriangle")

#Sewer exfiltration budgets
#data is in percentages
S <- read.csv("data/Sewer.csv")

#summarize mean and median loss pathway ratios
S %>% group_by(Type) %>%
  summarize(avgQ = mean(Q),avgI = mean(I),avgET = mean(ET), 
            medQ = median(Q), medI = median(I), medET = median(ET),
            n = n())

#set color palette to color-blind-friendly
cbPalette <- c("#D55E00", #orange-red
               "#E69F00", #yellow-orange
               "#009E73", #kellygreen
               "#0072B2", #deepblue
               "#000000", #black"
               "#999999", #gray  
               "#56B4E9", #skyblue
               "#F0E442", #lemonyellow
               "#CC79A7"  #pink
               ) 


#Relevel factors to desired order
levels(S$Type)
S$Type <- factor(S$Type, levels = c("Sewer section(s)","Sewershed","Long-distance freshwater conveyance","Salt tracer model","Experimental model"))
S <- S %>% arrange(Type)

#set transparency for individul types
S$alpha <- c(rep(0.8,26), rep(1,4))

#plot on Ternary diagram
#Q, I, & ET are all in %
pdf("results/FigS4Sewers.pdf", height = 4, width = 7)
ggtern(data= S, aes(Q, I, ET, color = Type)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") +
  geom_mask() + geom_point(size = 3, aes(shape = Type, alpha = alpha)) +
  scale_colour_manual(values=cbPalette) + 
  scale_alpha_continuous(limits = c(0,1), guide = F)
dev.off()

ggsave("results/FigS4Sewers.png", height = 4, width = 7, dpi = 900, device = "png")
ggtern(data= S, aes(Q, I, ET, color = Type)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") +
  geom_mask() + geom_point(size = 3, aes(shape = Type, alpha = alpha)) +
  scale_colour_manual(values=cbPalette) + 
  scale_alpha_continuous(limits = c(0,1), guide = F)
dev.off()

#Calculate quantile ranges and confidence intervals for whole sewersheds (SS)
SSsub <- S %>%
  filter(Type == "Sewershed") #set category of interest
SSsub

#Get quantile ranges (rounded to nearest 5%)
round(quantile(SSsub$Q/5,c(0.05,0.95)))*5 #Q
round(quantile(SSsub$I/5,c(0.05,0.95)))*5 #I
round(quantile(SSsub$ET/5,c(0.05,0.95)))*5 #ET

#Get confidence intervals around mean values
#How many times do you want to sample?
R <- 1000
m <- numeric(R) #initialize vector of means

#resample Q
set.seed(1)
for(i in 1:R) {
  s <- sample(SSsub$Q, size = length(SSsub$Q), replace = T)
  m[i] <- mean(s)
}
hist(m) #histogram of resampled means
mean(m) #mean of means
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample I
set.seed(1)
for(i in 1:R) {
  s <- sample(SSsub$I, size = length(SSsub$I), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of I distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for(i in 1:R) {
  s <- sample(SSsub$ET, size = length(SSsub$ET), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of ET distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

###
#Calculate quantile ranges and confidence intervals for sewer pipe sections (PS)
PSsub <- S %>%
  filter(Type == "Sewer section(s)") #set category of interest
PSsub

#Get quantile ranges (rounded to nearest 5%)
round(quantile(PSsub$Q/5,c(0.05,0.95)))*5 #Q
round(quantile(PSsub$I/5,c(0.05,0.95)))*5 #I
round(quantile(PSsub$ET/5,c(0.05,0.95)))*5 #ET

#Get confidence intervals around mean values
#How many times do you want to sample?
R <- 1000
m <- numeric(R) #initialize vector of means

#resample Q
set.seed(1)
for(i in 1:R) {
  s <- sample(PSsub$Q, size = length(PSsub$Q), replace = T)
  m[i] <- mean(s)
}
hist(m) #histogram of resampled means
mean(m) #mean of means
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample I
set.seed(1)
for(i in 1:R) {
  s <- sample(PSsub$I, size = length(PSsub$I), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of I distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for(i in 1:R) {
  s <- sample(PSsub$ET, size = length(PSsub$ET), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of ET distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))
