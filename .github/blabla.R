library(terra)
a <- rast("app/data/honeybee/map.tif") 

b <- vect(data.frame(lat = 10.698161, lon = 10.415933), geom = c("lon", "lat"), crs = "EPSG:4326") |> project(a)

terra::extract(a,b, na.rm = TRUE)
