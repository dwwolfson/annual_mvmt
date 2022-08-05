# Prepare data subset to be tested in Migration Mapper


dependencies <- c("dplyr","here", "readr", "sf", "glue")
for (i in 1:length(dependencies)) {
  if (dependencies[i] %in% installed.packages() == FALSE) {
    install.packages(dependencies[i])
    require(dependencies[i], character.only = TRUE)
  } else {
    require(dependencies[i], character.only = TRUE)
  }
}

# MAPP needs input data in the format of a shapefile 

# mule<-st_read(here("../../!non-git_R_projects/Migration_Mapper/sampleData/MuleDeer_PlatteValley_testdata.shp"))
# str(mule)
# class(mule)
# st_crs(mule)

# read in full dataset
df <- read_csv(here("data/full_dataset_6_28_2022/full_w_nsd.csv"))

# convert to sf
df<-df %>% st_as_sf(., coords=c("lon", "lat"), crs=4326)

df$timestamp<-as.character(df$timestamp)

df<-df %>% select(id, timestamp, geometry)

# write out each swan to folder
# in order for migration mapper to be quickest, probably best to do one swan at a time
ids<-unique(df$id)
out<-here("data/all_swans_shapefiles_6_28_2022")

for(i in seq_along(ids)){
    swan<-df %>% filter(id==ids[[i]]) #filter swan
    dir.create(paste0(here("output/MAPP_output"), glue::glue("/{ids[[i]]}"))) #create empty folder for output
    
    st_write(swan, paste0(out, glue::glue("/{ids[[i]]}"), ".shp")) # write shapefile
}

# for just a test with 2 swans
st_write(df1, here("data/test_MAPP/two_swans.shp"))




