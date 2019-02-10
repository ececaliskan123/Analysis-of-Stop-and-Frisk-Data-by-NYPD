# ******************************
#   This code snippet transforms the coordinates (which are stored in an US
#   State Planing Format) into human-radable longitude/latitude data.
#
#   On top, for a possible further use, we store coordinates in UTM format.
#
#   The st-package is a healpful source dealing with spatial data.
#
#   This article was extremely helpful: https://ryanpeek.github.io/2017-08-03-converting-XY-data-with-sf-package/
# ******************************

df = readRDS("df.rds")

# Define Spatial Data
df = df[!is.na(df$xcoord) & !is.na(df$ycoord),]             # deletes NAs
df = st_as_sf(df,coords=c("xcoord","ycoord"),crs=102718)   #define spatial data, "102718" is the code for the US state planning

  # Storing coordinates
  df      = st_transform(df, crs=32610) # transform to UTM format
  df$utmE = st_coordinates(df)[,1] # save column UTM East
  df$utmN = st_coordinates(df)[,2] # save column UTM West
  df      = st_transform(df, crs=4326) # switch to long/lat format
  df$long = st_coordinates(df)[,1] # add longitude (= x value)
  df$lat  = st_coordinates(df)[,2]  # add latitude (= y value)
  
  df      = st_set_geometry(df, NULL) # remove the spatial component
  
saveRDS(df,file="df.rds")
