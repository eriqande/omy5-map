
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(readxl)
library(broom)
library(stringr)
library(dplyr)



#  all downloaded from <a href="http://www.naturalearthdata.com/downloads/">http://www.naturalearthdata.com/downloads/</a> using the
#  1:10m  scale data.

# NOTE readOGR can't do tilde expansion!!!

nat.earth <- stack('~/Documents/NaturalEarthData/HYP_LR_SR_W_DR/HYP_LR_SR_W_DR.tif')

ne_lakes <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_lakes",
                    "ne_10m_lakes")

ne_rivers <- readOGR('/Users/eriq/Maps/natural-earth-10m/ne_10m_rivers_lake_centerlines',
                     'ne_10m_rivers_lake_centerlines')

usgs_rivers <- readOGR('/Users/eriq/Maps/hydrogm020_nt00015',
                       'hydrogl020')

# and immediately restrict that to just the west coast
usgs_rivers <- usgs_rivers[usgs_rivers$STATE %in% c("CA", "OR", "WA", "ID", "NV", "MT"), ]

ne_coast <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_coastline",
                    "ne_10m_coastline")

state_prov <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_admin_1_states_provinces_lines",
                      "ne_10m_admin_1_states_provinces_lines")

# and immediately drop all but canada and the US:
state_prov <- state_prov[state_prov$adm0_name %in% c("United States of America", "Canada"),]

country_bound <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_admin_0_boundary_lines_land",
                         "ne_10m_admin_0_boundary_lines_land")

#### The usgs data set is massive, so we should be able to restrict it to the rivers we want
#### Here is a test of that
vent <- usgs_rivers[usgs_rivers$NAME == "Ventura River" & !is.na(usgs_rivers$NAME), ]

#  I have a domain I'm interested in, but there's no reason you can't define something else:
quick.subset <- function(x, longlat){
  
  # longlat should be a vector of four values: c(xmin, xmax, ymin, ymax)
  x@data$id <- rownames(x@data)
  
  x.f = fortify(x, region="id")
  x.join = join(x.f, x@data, by="id")
  
  x.subset <- subset(x.join, x.join$long > longlat[1] & x.join$long < longlat[2] &
                       x.join$lat > longlat[3] & x.join$lat < longlat[4])
  
  x.subset
}

# here eric implements the same thing but using tidyverse tools
tidy_subset <- function(x, longlat) {
  x@data$id <- rownames(x@data)
  x.f <- broom::tidy(x) %>%
    dplyr::left_join(., x@data, by = "id") %>%
    dplyr::tbl_df() %>%
    filter(long > longlat[1],
           long < longlat[2],
           lat > longlat[3],
           lat < longlat[4])
}


system.time(tmp1 <- tidy_subset(ne_lakes, domain))  # the tidyverse version is twice as fast
system.time(tmp2 <- quick.subset(ne_lakes, domain))


domain <- c(-129, -110, 28, 50)
lakes.subset <- tidy_subset(ne_lakes, domain)
river.subset <- tidy_subset(ne_rivers, domain)
coast.subset <- tidy_subset(ne_coast, domain)
vent.subset <- tidy_subset(vent, domain)
usgs.subset <- tidy_subset(usgs_rivers, domain) %>%
  filter(F_CODE %in% c(4,5))  # this retains only streams and canals
state.subset <- tidy_subset(state_prov, domain)
country.subset <- tidy_subset(country_bound, domain)

nat.crop <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))


rast.table$rgb <- with(rast.table, rgb(HYP_LR_SR_W_DR.1,
                                       HYP_LR_SR_W_DR.2,
                                       HYP_LR_SR_W_DR.3,
                                       1))


# grab the locations such as they are now
samps <- read_excel("data/Omy5survey_table_LC.xlsx") %>%
  mutate(lat = as.numeric(Lat),
         long = as.numeric(Long)) %>%
  filter(!is.na(lat)) %>%
  mutate(Number = paste(1:nrow(.)))


# for now, make a dummy data frame of above-below pairs
# so we can make barplot-like things with them 
set.seed(5)
npairs <- 30
pairs <- data.frame(pairnum = 1:npairs,
           resA = runif(npairs, .6, 1.0),
           resB = runif(npairs, 0.0, 0.6))
pairs$resA[c(1,6, 9, 18, 20)] <- NA
pairs$resB[c(5, 15, 25)] <- NA

# we have 49 to 29 degrees to play with.  That is 20 degrees of latitude.
# If we do each bar as twice the distance between, that is 2/3 to 1/3.  
# So, each unit is 20/npairs
rectwidth <- 0.5
xA <- -127
xB <- -128
height <- 20/npairs * .666667

pairs2 <- pairs %>%
  mutate(ymin = 49 - pairnum * 20/npairs,
         anad_ymax = ymin + height,
         resA_ymax = ymin + height * resA,
         resB_ymax = ymin + height * resB
         )

pairsA <- pairs2 %>% filter(!is.na(resA)) %>% mutate(xmin = xA - 0.5 * rectwidth, xmax = xA + 0.5 * rectwidth)
pairsB <- pairs2 %>% filter(!is.na(resB)) %>% mutate(xmin = xB - 0.5 * rectwidth, xmax = xB + 0.5 * rectwidth)
smidge <- 0.02
# et voila!
g <- ggplot() +
  geom_raster(data = rast.table, mapping = aes(x = x, y = y), fill = rast.table$rgb, interpolate = TRUE) +
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  geom_path(data=state.subset, aes(x = long, y = lat, group = group), color = 'gray30') +
  geom_path(data=country.subset, aes(x = long, y = lat, group = group), color = 'gray30') +
  geom_path(data=river.subset, aes(x = long, y = lat, group = group), color = 'blue', size = 0.2) +
  geom_path(data=coast.subset, aes(x = long, y = lat, group = group), color = 'blue', size = 0.1) +
  geom_path(data=usgs.subset, aes(x = long, y = lat, group = group), colour = "blue", size = 0.06) +
  geom_path(data=lakes.subset, aes(x = long, y = lat, group = group), color = 'blue', size = 0.1) +
  geom_rect(data = pairsA, mapping = aes(ymin = ymin, ymax = anad_ymax, xmin = xmin, xmax = xmax), colour = NA, fill = "navy") +
  geom_rect(data = pairsB, mapping = aes(ymin = ymin, ymax = anad_ymax, xmin = xmin, xmax = xmax), colour = NA, fill = "navy") +
  geom_rect(data = pairsA, mapping = aes(ymin = ymin, ymax = resA_ymax, xmin = xmin, xmax = xmax), colour = NA, fill = "orange") +
  geom_rect(data = pairsB, mapping = aes(ymin = ymin, ymax = resB_ymax, xmin = xmin, xmax = xmax), colour = NA, fill = "orange") +
  geom_rect(data = pairsA, mapping = aes(ymin = ymin - smidge, ymax = anad_ymax + smidge, xmin = xmin, xmax = xmax), colour = "white", fill = NA) + 
  geom_rect(data = pairsB, mapping = aes(ymin = ymin - smidge, ymax = anad_ymax + smidge, xmin = xmin, xmax = xmax), colour = "white", fill = NA) + 
#  geom_point(data = samps, aes(x = long, y = lat)) +
  geom_text(data = samps, aes(x = long, y = lat, label = Number), size = 0.4) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')

library(ggtree)
dd <- data.frame(x=LETTERS[1:3], y=1:3)
pie <- ggplot(dd, aes(x=1, y, fill=x)) + geom_bar(stat="identity", width=1) + coord_polar(theta="y") + theme_inset()

for(l in seq(30,48, by = 1.2)) {
  g <- subview(g, pie, -126, l, 0.12, 0.12)
  g <- subview(g, pie, -127, l, 0.12, 0.12)
}

ggsave(g, filename = "base_map_test.pdf", width = 6, height = 10)
