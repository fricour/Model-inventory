#' helpers
#'
#' @description Bar Plot function
#'
#' @return a ggplot
#'
#' @noRd
BarPlots <- function(df, v){
  if (subset(dfvartype,var==v)$type=='cat.mult'){
    ul <- unlist(df[,v])
    cats<-unique(ul)
    ll<-lapply(cats, function(cat){sum(ul==cat)})
    data <- data.frame(label=cats, count=unlist(ll))
  }
  else {
    stop(paste("Bar chars are only relevant for multiple-entry categorical variables."))
  }

  data$label<-factor(data$label, ordered=TRUE, levels= data[order(data$count, decreasing = FALSE),'label'])

  # Basic piechart
  p<- ggplot(data, aes(x=label, y=count, fill=label)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_flip()+
    theme_bw()+theme(legend.position="") # remove background, grid, numeric labels
  return(p)
}

#' @description Pie Plot function
#'
#' @return a ggplot
#'
#' @import ggplot2
#'
#' @noRd
#'
#' @example
#' Call as, e.g. " PiePlot(df_res,'VertCoord') "
PiePlot <- function(df, v){
  if (subset(dfvartype,var==v)$type=='cat.excl'){
    data <- ddply( df , c(v) , summarize, count = length(ModelName))
  }
  else { stop(paste("Pie char are only relevant for exclusive categorical variables.")) }

  data <- data %>%
    #    arrange(desc(count)) %>%
    dplyr::mutate(prop = count / sum(data$count) *100) %>%
    dplyr::mutate(ypos = 100-(cumsum(prop)- 0.5*prop ))

  # Basic piechart
  p<- ggplot(data, aes_string(x=0, y="prop", fill=v)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    geom_text(aes_string(y = "ypos", label = "count"), color = "white", size=6) +
    scale_fill_brewer(palette="Set1", name = MyLab(v))+
    theme_void() # remove background, grid, numeric labels
  return(p)
}


#' @description gives nice labels
#'
#' @noRd
#'

MyLab <- function(v){

  labeldic <- list('InstituteName' = 'Name of Institute',
                   'InstituteType' = 'Type of Institute',
                   'Region' = 'Region',
                   'Availability'='Data Availability',
                   'EOVs' = 'Essential Ocean Variables',
                   'POI' = 'Phenomenon of Interest',
                   'MinRes' = 'Spatial Resolution - [m]',
                   'ForecastLength' = 'Forecast Length - [d]',
                   'ForecastFreq' = 'Forecast Frequency',
                   'NLevels'='Number of Vertical Levels',
                   'VertCoord'='Vertical Coordinate System',
                   'ValidationPlatform'='Platform for Validation Data',
                   'AssimPlatform'='Platform for Assimilation Data',
                   'DAScheme'='Assimilation Scheme',
                   'AssimEOV'='Assimilated Variables',
                   'Uncertainty'='Dynamic Uncertainty',
                   'AtmosSource' = 'Source of Atmosperic Forcings',
                   'AtmosProcess' = 'Processing of Atmospheric Forcings',
                   'LandSource' = 'Source of Terrestrial Forcings',
                   'AtmosType' = 'Type of Atmospheric Forcings',
                   'LandType' = 'Type of Terrestrial Forcings',
                   'IncreaseAccuracy' = 'Means of Improvement',
                   'DataAssimilation' = 'Data Assimilation',
                   'MinBat' = "Shallowest Bathymetry - [m]",
                   'Tides' = 'Tidal dynamics',
                   'SourceOpen' = 'Source of offshore boundary forcings',
                   'SeaFloor' = 'Seafloor parameterization',
                   'ModelCore' = 'Model Core Engine',
                   'ModelType'='Model Type',
                   'AtmosProviders' = 'Atmospheric Forcings Providers',
                   'AtmosModels' = 'Atmospheric Forcings System'
  )

  return(labeldic[v])
}
