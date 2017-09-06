#This script creates figure 5, comparing various types of porous pavement.

#Required packages:
library(ggplot2)
library(ggtern)
library(dplyr)

#Set project directory
setwd("E:/TrianglR/WaterBudgetTriangle")

#load dataset
PP <- read.csv("data/PorousPavement.csv")
str(PP)

#Create category factors
levels(PP$Type)
PP$Type <- factor(PP$Type, levels = c("Porous pavement, cobblestone or interlocking pavers",
                                      "Event, monthly or seasonal estimate",
                                      "Grassed pavers",
                                      "Impervious control",
                                      "Lined PP System", 
                                      "Modeled estimate",
                                      "Climate forecast model") ) 

PP <- PP %>% arrange(Type)
PP %>% group_by("Type") %>%
  summary()

#set color palette to color-blind-friendly
cbPalette <- c("#009E73", #kellygreen
               "#0072B2", #deepblue
               "#E69F00", #yellow-orange
               "#999999", #gray 
               "#56B4E9", #skyblue
               "#000000", #black"
               "#0072B2", #deepblue
               "#F0E442", #lemonyellow
               "#CC79A7",  #pink
               "#D55E00") #orange-red
               
#plot on Ternary diagram
pdf("results/Fig5PorousPavement.pdf", width = 9, height = 5)              
ggtern(data = PP, aes(Q, I, ET)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") + geom_mask() +
  labs(size = "Precipitation (mm)") +
  geom_mask() +
  geom_point(aes(size = P, color = Type, shape = Type, alpha = Alpha)) +
  scale_colour_manual(values = cbPalette) +
  scale_shape_manual(values = c(16,3,17,18,8,7,7)) +
  scale_alpha_continuous(limits = c(0,1), guide = F)
dev.off()

ggsave("results/Fig5PorousPavement.png", height = 5, width = 9, dpi = 900, device = "png")
ggtern(data = PP, aes(Q, I, ET)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") + geom_mask() +
  labs(size = "Precipitation (mm)") +
  geom_mask() +
  geom_point(aes(size = P, color = Type, shape = Type, alpha = Alpha)) +
  scale_colour_manual(values = cbPalette) +
  scale_shape_manual(values = c(16,3,17,18,8,7,7)) +
  scale_alpha_continuous(limits = c(0,1), guide = F)
dev.off()

#A subset of the porous pavement dataset is used to calculate summary statistics
PP %>%
  filter(Type == "Porous pavement, cobblestone or interlocking pavers") %>%
  summarize(avgQ = mean(Q),avgI = mean(I),avgET = mean(ET), 
            medQ = median(Q), medI = median(I), medET = median(ET),
            n = n())

PP %>%
  group_by(Type) %>%
  summarize(avgQ = mean(Q),avgI = mean(I),avgET = mean(ET), 
            medQ = median(Q), medI = median(I), medET = median(ET),
            n = n())


PPsubset <- PP[PP$Type == "Porous pavement, cobblestone or interlocking pavers",]


#Get quantiles (round to nearest 5%)
round(quantile(PPsubset$Q/5,c(0.05,0.95)))*5
round(quantile(PPsubset$I/5,c(0.05,0.95)))*5
round(quantile(PPsubset$ET/5,c(0.05,0.95)))*5

#Get confidence intervals of dataset
#How many times do you want to sample?
R <- 1000
m <- numeric(R) #initialize vector of means

#resample Q
set.seed(1)
for(i in 1:R) {
  s <- sample(PPsubset$Q, size = length(PPsubset$Q), replace = T)
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
  s <- sample(PPsubset$I, size = length(PPsubset$I), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#find the 5th and 95th percentiles of the distribution of means
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for(i in 1:R) {
  s <- sample(PPsubset$ET, size = length(PPsubset$ET), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#find the 5th and 95th percentiles of the distribution of means
round(quantile(m, c(0.05,0.95)))
