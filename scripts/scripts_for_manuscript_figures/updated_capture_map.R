# Capture map with inset map included

# package names
packages<-c("tidyverse", "here", "leaflet", 
            "mapview", "rnaturalearth", "webshot2")

# install any packages not previously installed
# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

df<-read_csv(here("data/capture_coordinates.csv"))

# state boundaries
states<-ne_states(country = "United States of America")
provinces<-ne_states(country="Canada")

# start basemap (note the argument to hide the zoom buttons)
leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
   
   # add basemap
   addProviderTiles(providers$Esri.WorldPhysical) %>% 
   #addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
   #addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 

   # add state boundaries and labels
   addPolygons(data=states,
               fill=NA,
               weight=2,
               color="lightgrey",
               opacity=1) %>% 
   
   # add province boundaries
   addPolygons(data=provinces,
               fill=NA,
               weight=2,
               color="lightgrey",
               opacity=1) %>% 
   
# focus map on proper area and zoom
   setView(-90, 45, zoom=5) %>% 
  #fitBounds(lng1=-90, lat1=52, lng2=-80, lat2=35) %>% 
   #fitBounds(-90, 35, -80, 52) %>% 
   #setMaxBounds(-90, 35, -80, 52) %>% 
   
   # add scale bar
   addScaleBar(position="bottomright") %>% 
   
  
  # add inset map
  addMiniMap(
    tiles = providers$Esri.WorldPhysical,
    position = 'topright', 
    width = 175, height = 175,
    toggleDisplay = FALSE) %>%
  
  
  # add graticules with nice labels (recommended for static plot)
   addSimpleGraticule(interval = 10) %>%
  #  addGraticule(interval=5,
  #               showLabel=T,
  #               style=list(color="white",
  #                          weight=0.9)) %>%
  
  
  # add points
  addCircleMarkers(data=df, ~Longitude, ~Latitude,
                   weight = 0.5,
                   col = 'black', 
                   fillColor = 'darkslategrey',
                   radius = 2, 
                   fillOpacity = 0.8)

  #mapshot2(m, file="figures/figs_for_manuscript/updated_map.png")
  
  # saveWidget(m, file="figures/figs_for_manuscript/updated_map.html")
  # webshot::webshot(url = here("figures/figs_for_manuscript/updated_map.html"),
  #                   file = here("figures/figs_for_manuscript/updated_map2.pdf"),
  #                   debug=T, vwidth = 2304, vheight = 4096)
  
  # webshot2::webshot(url = here("figures/figs_for_manuscript/updated_map.html"),
  #                   file = here("figures/figs_for_manuscript/updated_map2.png"),
  #                   vwidth = 2304, vheight = 4096)
 

