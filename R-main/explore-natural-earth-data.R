
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(readr)
library(broom)
library(stringr)
library(dplyr)
library(ggsn)
library(mapdata)
library(grid)



#  all downloaded from <a href="http://www.naturalearthdata.com/downloads/">http://www.naturalearthdata.com/downloads/</a> using the
#  1:10m  scale data.

# NOTE readOGR can't do tilde expansion!!!

nat.earth <- stack('~/Documents/NaturalEarthData/HYP_HR_SR_W/HYP_HR_SR_W.tif')

ne_lakes <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_lakes",
                    "ne_10m_lakes")

# ne_rivers <- readOGR('/Users/eriq/Maps/natural-earth-10m/ne_10m_rivers_lake_centerlines',
#                     'ne_10m_rivers_lake_centerlines')

#usgs_rivers <- readOGR('/Users/eriq/Maps/hydrogm020_nt00015',
#                       'hydrogl020')


# try getting full NA large rivers:
na_rivers <- readOGR('/Users/eriq/Maps/Lakes_and_Rivers_Shapefile/NA_Lakes_and_Rivers/data/hydrography_l_rivers_v2/Lakes_and_Rivers_Shapefile/NA_Lakes_and_Rivers/data',
                     'hydrography_l_rivers_v2')
# note that these need to be re-projected to the same lat-long system as natural earth
na_rivers_ll <- spTransform(na_rivers, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# and then I think we can get polygons of the lakes too
#na_lakes <- readOGR('/Users/eriq/Maps/Lakes_and_Rivers_Shapefile/NA_Lakes_and_Rivers/data/hydrography_p_lakes_v2/Lakes_and_Rivers_Shapefile/NA_Lakes_and_Rivers/data',
#                     'hydrography_p_lakes_v2')

# na_lakes_ll <- spTransform(na_lakes, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# x <- na_lakes_ll
# x@data$id <- rownames(x@data)
# na_lakes_full <- broom::tidy(x) %>%
#   dplyr::left_join(., x@data, by = "id") %>%
#   dplyr::tbl_df()


# and immediately restrict that to just the western states
#usgs_rivers <- usgs_rivers[usgs_rivers$STATE %in% c("CA", "OR", "WA", "ID", "NV", "MT", "UT", "AZ", "WY"), ]

ne_coast <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_coastline",
                    "ne_10m_coastline")

state_prov <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_admin_1_states_provinces_lines",
                      "ne_10m_admin_1_states_provinces_lines")

# and immediately drop all but canada and the US:
state_prov <- state_prov[state_prov$adm0_name %in% c("United States of America", "Canada"),]

country_bound <- readOGR("/Users/eriq/Maps/natural-earth-10m/ne_10m_admin_0_boundary_lines_land",
                         "ne_10m_admin_0_boundary_lines_land")



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


#system.time(tmp1 <- tidy_subset(ne_lakes, domain))  # the tidyverse version is twice as fast
#system.time(tmp2 <- quick.subset(ne_lakes, domain))


# take subset of all that....It would be better to have all the shapefiles in a named list and lapply it
domain <- c(-141, -111.5, 27.5, 58)
lakes.subset <- tidy_subset(ne_lakes, domain)
#river.subset <- tidy_subset(ne_rivers, domain)
coast.subset <- tidy_subset(ne_coast, domain)

#usgs.subset <- tidy_subset(usgs_rivers, domain) %>%
#  filter(F_CODE %in% c(4,5))  # this retains only streams and canals

state.subset <- tidy_subset(state_prov, domain)
country.subset <- tidy_subset(country_bound, domain)

north_am_rivers_subset <- tidy_subset(na_rivers_ll, domain)
#north_am_lakes_subset <- tidy_subset(na_lakes_ll, domain)

nat.crop <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))


rast.table$rgb <- with(rast.table, rgb(HYP_HR_SR_W.1,
                                       HYP_HR_SR_W.2,
                                       HYP_HR_SR_W.3,
                                       1))


# Get the locations and inversion freqs of 84 pops
omy5_survey <- read_csv("data/omy-5-survey-83-pops.csv")

# Set up the region in which we put dots, numbers, and horizontal freq bars
dottop <- 57
dotbot <- 28 
rectwidth <- 2.2 # total x dim of bars
rectsep <- 0.75 # x separation between bars
bbxlo <- -142.5 # below barrrier lowest x value
rectheight <- 0.81 * (dottop - dotbot) / nrow(omy5_survey)  # silly to be har
samp3 <- omy5_survey %>%
  mutate(dot_y = dottop - (trial_map_order - 1) * (dottop - dotbot) / n(),
         dot_x = -136) %>%
  mutate(#xmin = ifelse(Migratory_Access == "Below barrier", bbxlo, bbxlo + rectwidth + rectsep),
         xmin = bbxlo + rectwidth + rectsep,
         xmax = xmin + rectwidth,
         ymin = dot_y - 0.5 * rectheight,
         ymax = dot_y + 0.5 * rectheight,
         xres = xmin + rectwidth * InversionFreq) %>%
  mutate(reseq = ifelse(is.na(Resequenced), FALSE, TRUE))



# Make the base map
base_map <- ggplot() +
  geom_raster(data = rast.table, mapping = aes(x = x, y = y), fill = rast.table$rgb, interpolate = TRUE) +
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  geom_path(data=state.subset, aes(x = long, y = lat, group = group), color = 'gray30') +
  geom_path(data=country.subset, aes(x = long, y = lat, group = group), color = 'gray30') +
  geom_path(data=coast.subset, aes(x = long, y = lat, group = group), color = 'gray30', size = 0.01) + 
  #  geom_polygon(data=north_am_rivers_subset, aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data=north_am_rivers_subset, aes(x = long, y = lat, group = group), colour = "gray30", size = 0.15) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_quickmap(xlim = domain[1:2],  ylim = domain[3:4]) +
  ylab("North Latitude") +
  xlab("Longitude") +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15))

# for experimenting with positions, here is a quickly-rendered base-map
if(FALSE) {
  north_am <- map_data("world")
  base_map <- ggplot() +
    geom_polygon(data = north_am, mapping = aes(x = long, y = lat), fill = NA, colour = "gray") + 
    coord_quickmap(xlim = domain[1:2],  ylim = domain[3:4]) +
    theme_bw()
}
#ggsave(base_map, filename = "base_map.pdf", width = 10, height = 15)

# scale_alpha_discrete(range=c(1,0)) +


# add to the base-map all the dots and bars and lines
colRR <- "#E31A1C"
colAR <- "#FF7F00"
colAA <- "royalblue4"

  
  
full_map <- base_map +
  geom_segment(data = samp3, mapping = aes(x = dot_x, xend = Longitude, y = dot_y, yend = Latitude, colour = reseq), size = 0.2) +
  geom_point(data = samp3, mapping = aes(x = dot_x, y = dot_y, colour = reseq)) +
  geom_text(data = samp3, mapping = aes(x = dot_x - 0.2, y = dot_y, label = trial_map_order, colour = reseq), hjust = 1, size = 3.5) +
  geom_rect(data = samp3, mapping = aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax), colour = NA, fill = colAA) +
  geom_rect(data = samp3, mapping = aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xres), colour = NA, fill = colRR) +
  scale_colour_manual(values = c("black", "darkorchid1")) +
  guides(colour = FALSE) +
  #north(x.min = domain[-1], x.max = domain[2], y.min = domain[3], y.max = domain[4], location = "bottomleft", anchor = c(x = -122.3, y = 28)) +
  scalebar(x.min = domain[-1], x.max = domain[2], y.min = domain[3], y.max = domain[4], 
           location = "bottomleft", dd2km = TRUE, model = "WGS84", dist = 400, anchor = c(x = -125.6, y = 28.5), st.size = 5.5)


### Now, work on the world-scale map with the inset:
wrld <- map_data("world")
domain_df <- data_frame(point = 1:length(domain), long = rep(domain[1:2], each = 2), lat = c(domain[3:4], rev(domain[3:4])))

inset_world <- ggplot() + 
  geom_path(data = wrld, aes(x=long, y=lat, group=group), colour="black", size = 0.1) +
  geom_polygon(data = domain_df, mapping = aes(x = long, y = lat), colour = "red", fill = "red", alpha = 0.3) +
  coord_map("ortho", orientation=c(41, -132, 0)) +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "mm"))


# make an inset window
vp <- viewport(width = 0.33, height = 0.163, x = 1.035, y = 0.99, just = c("right", "top"))

# now, see if we can make a pdf of that
svg(file = "map_with_inset.svg", width = 10, height = 15)
print(full_map)
print(inset_world, vp = vp)
dev.off()

# then use inkscape to place big-creek-inset-vector.svg into there.
# and also add the north-arrow.svg to it, then save it
# as mykiss-genome-map-figure.pdf

