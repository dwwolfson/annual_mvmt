# Make a map of capture locations


# package names
packages<-c("tidyverse", "here", "lubridate", "sf", 
            # "leaflet", "tmap",
            "ggmap", "ggspatial", "osmdata", "ggsn")

# install any packages not previously installed
# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

df<-read_csv(here("data/capture_coordinates.csv"))

# 
# df_sp<-st_as_sf(df, coords = c("Longitude", "Latitude"),
#              crs=4326)
# 
# # give a bounding box for custom zoom
# my_bbox<-st_bbox(c(xmin=(-101), xmax=(-79), ymax=52, ymin=33), crs=st_crs(4326))
# bbox1<-
# 
# map_out<-tm_shape(df_sp, bbox=my_bbox)+
#   tm_dots()+
#   tm_scale_bar()+
#   tm_compass()+
#   tm_legend(show=F)
# 
# tmap_plot<-tmap_leaflet(map_out) %>% 
#   addProviderTiles(providers$Stamen.TerrainBackground) %>% 
#   addLegend(color="black", labels="Capture Locations") %>% 
#   addScaleBar()

###########
# try with ggmap


# Exclude the Arkansas swans because they aren't in the annual movements analysis anyway
box<-make_bbox(c(-101,-79), c(52,39))

background<-get_map(location=box, 
             source = "stamen",
             maptype = "terrain-background")

my_map<-ggmap(background)+
  geom_point(data=df,
               aes(x=Longitude, y=Latitude),
               alpha=0.3,
               color="darkred",
               size=1.75)
  
my_map<-my_map+
  ggsn::scalebar(x.min=-101, x.max=-96,
                 y.min=39, y.max=40,
                 location="bottomleft",
                 dist=200, height=0.2,
                 st.dist=0.3, transform=T,
                 dist_unit="km", model="WGS84")

my_map+
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    ))


north2(my_map, symbol = 3, x=0.25, y=0.25, scale=0.12)
# see symbols with ggsn::northSymbols()
  
  # annotation_scale(location = "bl", width_hint = 0.4) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
  #                        style = north_arrow_fancy_orienteering)
