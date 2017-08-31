#This script creates figure S5 Continental water budgets

#Required packages:
library(ggplot2)
library(ggtern)
library(dplyr)

#Set project directory
setwd("E:/TrianglR/WaterBudgetTriangle")

#Continental Water Balances
#"I" estimated as the magnitude of change in storage from Rodell et al (2015)
continents <- read.csv("data/Continent.csv")
names(continents) <- c("Continent","Coastline_km","Area_km2", "ArealRatio", "P_volume","P", "Q", "I","ET")

#calculate average precip (mm)
mean(continents$P)

#calculate ratio of Q, I and ET loss pathways
continents$sum <- continents$Q + continents$I + continents$ET
continents$Qr <- continents$Q/continents$sum * 100
continents$Ir <- continents$I/continents$sum * 100
continents$ETr <- continents$ET/continents$sum * 100

#summarize mean and median loss pathway ratios
continents %>% filter(Continent != "Antarctica") %>%
  summarize(avgQ = mean(Qr), avgI = mean(Ir), avgET = mean(ETr),
            medQ = median(Qr), medI = median(Ir), medET = median(ETr),
            n = n())

#set color palette to color-blind-friendly
cbPalette <- c("#CC79A7", #pink
               "#D55E00", #orange-red
               "#E69F00", #yellow-orange
               "#F0E442", #lemonyellow
               "#009E73", #kellygreen
               "#0072B2", #deepblue
               "#56B4E9", #skyblue
               "#000000", #black"
               "#999999"  #gray  
               ) 

#Plot on ternary diagram
#P, Q, I, & ET are all in mm
pdf("results/FigS5Continents.pdf", height = 4)
ggtern(data= continents, aes(Q, I, ET)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") + 
  geom_point(aes(size = P, color = Continent), alpha = 0.8) + #geom_point(aes(color = Continent))
  geom_mask() + scale_size_area("Precipitation\n(mm)") +
  scale_colour_manual(values=cbPalette)
dev.off()
