

library(maps)
library(mapdata)
library(maptools)
library(dplyr)
library(ggplot2)
library(rgeos)


if (!rgeosStatus()) gpclibPermit()


gshhs.f.b <- "/Users/eriq/Maps/gshhg-bin-2.3.3/gshhs_f.b"
sf1 <- getRgshhsMap(gshhs.f.b, xlim = c(-128, -120), ylim = c(36, 48))




states <- map_data("state")
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))

rivers <- map_data("rivers")

ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  geom_line(data = rivers, mapping = aes(x = long, y = lat, group = group), color = "black") +
  coord_fixed(xlim = c(-128, -120.0),  ylim = c(36, 48), ratio = 1.3)
