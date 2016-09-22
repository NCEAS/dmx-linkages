# map for Communities of Practice paper

library(sp)
library(rworldmap)
library(rworldxtra)
library(mapproj)
library(rgdal)
library(ggplot2)
library(grid)

# Inititate a blank map
world <- getMap('low',projection=NA)
worldB <- world[!is.na(world$continent),]
world2 <- worldB[worldB$continent=='North America' & worldB$LON<0,]
fWorld <- fortify(world2)
colMap <- c('dimgrey','black')

ggplot(data = fWorld) +
  geom_map(map = fWorld,aes(x = long, y = lat, map_id = id)) +
  coord_map(xlim = c(-172, -118), ylim = c(40, 63)) + # all sampling locations
  #coord_map(xlim = c(-170, -130),ylim = c(52, 62)) + # zoom to Gulf of Alaska
  #coord_map(xlim = c(-170, -155),ylim = c(53, 56)) + # zoom to Aleutian Shelf
  scale_fill_manual(values = colMap) +
  
  theme(axis.line=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.position='right',
        axis.text=element_text(size=8),
        title=element_text(size=12,face="bold"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) 

ggsave("AKmap.png", width = 10, height = 5)
ggsave("AKmap1.png", width = 25, height = 13)
ggsave("AKmap2.png", width = 45, height = 23)
