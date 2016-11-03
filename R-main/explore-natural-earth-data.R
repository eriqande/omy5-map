
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(readr)
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

# and immediately restrict that to just the western states
usgs_rivers <- usgs_rivers[usgs_rivers$STATE %in% c("CA", "OR", "WA", "ID", "NV", "MT", "UT", "AZ", "WY"), ]

ne_coast <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_coastline",
                    "ne_10m_coastline")

state_prov <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_admin_1_states_provinces_lines",
                      "ne_10m_admin_1_states_provinces_lines")

# and immediately drop all but canada and the US:
state_prov <- state_prov[state_prov$adm0_name %in% c("United States of America", "Canada"),]

country_bound <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_admin_0_boundary_lines_land",
                         "ne_10m_admin_0_boundary_lines_land")


#  function to select just stuff within a given lat-long box. This is superseded by my tidy version
# quick.subset <- function(x, longlat){
#   
#   # longlat should be a vector of four values: c(xmin, xmax, ymin, ymax)
#   x@data$id <- rownames(x@data)
#   
#   x.f = fortify(x, region="id")
#   x.join = join(x.f, x@data, by="id")
#   
#   x.subset <- subset(x.join, x.join$long > longlat[1] & x.join$long < longlat[2] &
#                        x.join$lat > longlat[3] & x.join$lat < longlat[4])
#   
#   x.subset
# }

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


# take subset of all that....It would be better to have all the shapefiles in a named list and lapply it
domain <- c(-129, -110, 28, 50)
lakes.subset <- tidy_subset(ne_lakes, domain)
river.subset <- tidy_subset(ne_rivers, domain)
coast.subset <- tidy_subset(ne_coast, domain)
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
samps <- read_delim("data/MapDataFrame.txt", delim = "\t") %>%
  mutate(lat = as.numeric(Lat),
         long = as.numeric(Long)) %>%
  filter(!is.na(lat)) %>%
  mutate(Number = 1:nrow(.))

npops <- nrow(samps)




# we have 49 to 29 degrees to play with.  That is 20 degrees of latitude.
# If we do each bar as twice the distance between, that is 2/3 to 1/3.  
# So, each unit is 20/npairs
rectwidth <- 0.5
xA <- -128
height <- 20/npops * .666667

samps2 <- samps %>%
  mutate(resA = (R08985_Total - R08985_4) / R08985_Total,
         xmin = xA,
         xmax = xA + rectwidth) %>%
  mutate(ymin = 49 - Number * 20/npops,
         anad_ymax = ymin + height,
         resA_ymax = ymin + height * resA,
         labely = ymin + 0.5 * height,
         labelx = -126.8, 
         dot_spot_x = -126.6,
         dot_spot_y = ymin + 0.5 * height
         )

smidge <- 0.02

geno_ones <- samps2 %>%
  filter(Reseq == 1)


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
  geom_rect(data = samps2, mapping = aes(ymin = ymin, ymax = anad_ymax, xmin = xmin, xmax = xmax), colour = NA, fill = "navy") +
#  geom_rect(data = pairsB, mapping = aes(ymin = ymin, ymax = anad_ymax, xmin = xmin, xmax = xmax), colour = NA, fill = "navy") +
  geom_rect(data = samps2, mapping = aes(ymin = ymin, ymax = resA_ymax, xmin = xmin, xmax = xmax), colour = NA, fill = "orange") +
#  geom_rect(data = pairsB, mapping = aes(ymin = ymin, ymax = resB_ymax, xmin = xmin, xmax = xmax), colour = NA, fill = "orange") +
  geom_rect(data = samps2, mapping = aes(ymin = ymin - smidge, ymax = anad_ymax + smidge, xmin = xmin, xmax = xmax), colour = "white", fill = NA) + 
#  geom_rect(data = pairsB, mapping = aes(ymin = ymin - smidge, ymax = anad_ymax + smidge, xmin = xmin, xmax = xmax), colour = "white", fill = NA) + 
#  geom_point(data = samps, aes(x = long, y = lat)) +
#  geom_text(data = samps2, aes(x = long, y = lat, label = Number), size = 0.4) +  # these are tiny numbers at the actual location
  geom_text(data = samps2, aes(x = labelx, y = labely, label = Number), hjust = 1) +  # BIG NUMBERS
  geom_point(data = samps2, aes(x = dot_spot_x, y = dot_spot_y), colour = "black") +  # NUMBER POINTS
  geom_segment(data = samps2, aes(x = dot_spot_x, xend = long, y = dot_spot_y, yend = lat), colour = "black", size = 0.2) + # LINES
  geom_text(data = geno_ones, aes(x = labelx, y = labely, label = Number), hjust = 1, colour = "red") +  # BIG NUMBERS
  geom_point(data = geno_ones, aes(x = dot_spot_x, y = dot_spot_y), colour = "red") +  # NUMBER POINTS
  geom_segment(data = geno_ones, aes(x = dot_spot_x, xend = long, y = dot_spot_y, yend = lat), colour = "red", size = 0.2) + # LINES
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')

# library(ggtree)
# dd <- data.frame(x=LETTERS[1:3], y=1:3)
# pie <- ggplot(dd, aes(x=1, y, fill=x)) + geom_bar(stat="identity", width=1) + coord_polar(theta="y") + theme_inset()
# 
# for(l in seq(30,48, by = 1.2)) {
#   g <- subview(g, pie, -126, l, 0.12, 0.12)
#   g <- subview(g, pie, -127, l, 0.12, 0.12)
# }

ggsave(g, filename = "base_map_test.pdf", width = 6, height = 10)
