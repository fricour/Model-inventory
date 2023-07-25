# library(shiny)
# library(shinydashboard)
# library(plyr)
# library(dplyr)
# library(ggplot2)
# library(leaflet)
# library(scales)
# library(leaflet.extras)
# library(sp)
# library(reactable)
# library(readxl)
# library(rgdal)
# library(sf)
# library(gdata)
library(magrittr)

# path to data file
fname <- './data-raw/EuroGOOS_act3_AC_13012020.xls'

# sheets of interest in the data file
shname_res <- 'Resp1'       # The actual survey content
shname_mod <- 'Models'      # Model software and references
shname_ins <- 'Institutes'  # Institutes, coordinates of headquarters, type

# read data
# institudes
df_ins <- readxl::read_xls(fname,shname_ins)
# add url for shiny popup
df_ins$url <- paste0('<a href =', df_ins$Website, '>',df_ins$Website,'</a>')

# models
df_mod <- readxl::read_xls(fname,shname_mod)

# survey content
df_res <- readxl::read_xls(fname,shname_res)
df_res <- df_res[-which(is.na(df_res$MaxLat)),]
df_res <- df_res[-95,] # buggy one
df_res <- df_res[-which(df_res$MaxLon == 180),]
df_res <- df_res[-33,] # buggy one
rownames(df_res) <- NULL
df_res <- df_res %>% dplyr::mutate(id = dplyr::row_number())

# Turns multiple entries columns of strings into columns of lists of strings.
df_res$ModelCore <- strsplit(df_res$ModelCore, ', ')
df_res$Region <- strsplit(df_res$Region, ', ')
df_res$POI <- strsplit(df_res$POI, ', ')
df_res$AssimEOV <- strsplit(df_res$AssimEOV, ', ')
df_res$AssimPlatform <- strsplit(df_res$AssimPlatform, ', ')
df_res$IncreaseAccuracy <- strsplit(df_res$IncreaseAccuracy, ', ')
df_res$ValidationPlatform <- strsplit(df_res$ValidationPlatform, ', ')

# polygon creation
# https://rstudio-pubs-static.s3.amazonaws.com/202536_7a122ff56e9f4062b6b012d9921afd80.html
create_polygon <- function(latmin, latmax, lonmin, lonmax){
  y_coord <- c(latmin,latmax,latmax,latmin,latmin)
  x_coord <- c(lonmin,lonmin,lonmax,lonmax,lonmin)
  xym <- cbind(x_coord, y_coord)
  #print(xym)
  p <- sp::Polygon(xym)
  ps <- sp::Polygons(list(p), 1)
  sps <- sp::SpatialPolygons(list(ps))
  #plot(sps)
  sp::proj4string(sps) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  data <- data.frame(f=99.9)
  spdf <- sp::SpatialPolygonsDataFrame(sps,data)
  #spplot(spdf)
  return(spdf)
}

list_polygons <- list()
for (i in 1:nrow(df_res)){
  print(i)
  tmp_poly <- create_polygon(df_res$MinLat[i], df_res$MaxLat[i], df_res$MinLon[i], df_res$MaxLon[i])
  list_polygons <- append(list_polygons, tmp_poly)
}

for (i in 1:nrow(df_res)){
  if(i==1){
    full_polygons <- list_polygons[[i]]
  }else{
    full_polygons <- rbind(full_polygons, list_polygons[[i]])
  }
}

# Define type of variables.
# May be useful to select which variables should appear in a menu for the plots.
dfvartype <- data.frame(var=colnames(df_res))
dfvartype$type <- 'unknown'

dfvartype[ which(dfvartype$var %in% c('ModelCore','Region','POI','AssimEOV','AssimPlatform','IncreaseAccuracy','ValidationPlatform','AtmosSource','LandSource')), 'type']<- 'cat.mult'

dfvartype[ which(dfvartype$var %in% c('InstituteName','InstituteType','VertCoord','Operational','ForecastFreq','Availability',
                                      'DataAssimilation','DAScheme','AtmosType','AtmosProcess','LandType','Tides','Uncertainty')),'type']<- 'cat.excl'

#save(full_polygons, list_polygons, file = 'polygons.Rdata')
usethis::use_data(dfvartype, overwrite = TRUE)
usethis::use_data(list_polygons, overwrite = TRUE)
usethis::use_data(full_polygons, overwrite = TRUE)
usethis::use_data(df_res, overwrite = TRUE)
usethis::use_data(df_mod, overwrite = TRUE)
usethis::use_data(df_ins, overwrite = TRUE)
