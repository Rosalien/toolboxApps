#' @title dygraphTypeVariable
#' @description Time trend with dygraphs library for different type of variable
#' @param subsetoutbd data.table from mod_accessdata
#' @param frequence : String of frequency (ex. "30 min", "hour", "day", "month")
#' @return dygraph of timetrend
#' @importFrom data.table setDT
#' @importFrom shiny tagList
#' @importFrom xts xts
#' @import dygraphs
#' @export

dygraphTypeVariable <- function(subsetoutbd,frequence){
  if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
  dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
  fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by=frequence)
  variables <- unique(subsetoutbd$variable)

  keyWord <- c("Soil temperature","Soil water content","Soil heat flux","Température du sol","Teneur en eau du sol","Flux de chaleur dans le sol","Actual evapotranspiration by eddy-covariance","CO2 flux measurements by eddy-covariance","CH4 flux measurements by eddy-covariance","Sensitive heat flow by eddy-covariance","Latent heat flow by eddy-covariance")
  keyWord <- paste(keyWord,collapse = "|")
  subsetoutbd[,typeVariable:=ifelse(grepl(keyWord, definition)==TRUE,gsub(paste0("(",keyWord,").*"),"\\1",definition),definition)]

  # Build time series by type of variable and site
  typeVariables <- unique(subsetoutbd$typeVariable)

  res <- lapply(1:length(unique(subsetoutbd$typeVariable)),function(z){#Loop for each type of variable
    # Select type of variable
    outp <- subsetoutbd[subsetoutbd$typeVariable %in% typeVariables[z],]    
    outp <- outp[order(outp$variable),]

    lapply(unique(outp$code_site_station),function(y){#For each site, build a time serie chart
            dataxts <- do.call("cbind", lapply(unique(outp$variable),function(x){#For each variable, build a dbtimes-series
              if(nrow(outp[code_site_station %in% y & variable %in% x,])==0){
                NULL
              }else{  
                tmp <-  outp[code_site_station %in% y & variable %in% x,list(Date,value)]
                tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
                db <- xts(tmp2[,value],order.by=tmp2[,Date])
                colnames(db) <- x
                db
              }
            }))
                if(grepl("year",frequence)){
                  axisYears<-"function(d){ return d.getFullYear() }"
                  tickerYears <- "function(a, b, pixels, opts, dygraph, vals) { 
                                  return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}"
                  DataTimezone <- FALSE
                }else{
                axisYears<-NULL
                tickerYears <- NULL
                DataTimezone <- TRUE
                }

                dygraph(dataxts,group="groupe",main = paste(unique(outp[,"typeVariable"])," (",y,")",sep=""),ylab = unique(outp[,"unite"])[[1]],height = 250,width="100%")%>%
                dyAxis("x", axisLabelFormatter=axisYears,
                ticker= tickerYears) %>%
                dyOptions(stackedGraph = FALSE) %>%
                dyRangeSelector(height = 20,strokeColor = "") %>%
                dyLegend(show = "onmouseover") %>% 
                dyOptions(colors = wes_palette("Zissou1", length(unique(outp$variable)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=TRUE)%>%
                dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
                dyCSScool() %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))
            })            
        })#end of res
  }
    dy_graph <- tagList(res)
    return(dy_graph)
}

#' @title dygraphSite
#' @description Time trend with dygraphs library for different sites
#' @param subsetoutbd data.table from mod_accessdata
#' @param frequence : String of frequency (ex. "30 min", "hour", "day", "month")
#' @return dygraph of timetrend
#' @importFrom data.table setDT
#' @importFrom shiny tagList
#' @importFrom xts xts
#' @import dygraphs
#' @export

dygraphSite <- function(subsetoutbd,frequence){
  if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
    dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
    fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by=frequence)
    variables <- unique(subsetoutbd$variable)

    res <- lapply(1:length(variables),function(z){
      outp <- subsetoutbd[subsetoutbd$variable == variables[z],]   
      # Build time serie for all sites
      dataxts <- do.call("cbind", lapply(unique(outp$code_site_station),function(x){
        if(nrow(outp[code_site_station %in% x,])==0){
        NULL
        }else{
        # Build complete time series
          tmp <-  outp[code_site_station %in% x,list(Date,value)]
          tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
          db <- xts(tmp2[,value],order.by=tmp2[,Date])
        colnames(db) <- x
        db
        }
        }))

      if(grepl("year",frequence)){
        axisYears<-"function(d){ return d.getFullYear() }"
        tickerYears <- "function(a, b, pixels, opts, dygraph, vals) { 
                          return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}"
        DataTimezone <- FALSE
      }else{
        axisYears<-NULL
        tickerYears <- NULL
        DataTimezone <- TRUE
      }
      #@source https://stackoverflow.com/questions/33885817/r-dygraphs-x-axis-granularity-from-monthly-to-yearly
      graph <- dygraph(dataxts,group="groupe",main = unique(outp[,definition]),ylab = unique(outp[,unite]),height = 250,width="100%") %>%
               dyAxis("x", axisLabelFormatter=axisYears,ticker= tickerYears) %>%
               dyOptions(stackedGraph = FALSE) %>%
               dyRangeSelector(height = 20,strokeColor = "") %>%  
               dyLegend(show = "onmouseover") %>% 
               dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
               dyCSScool() %>%
               dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))    %>%   
               dyOptions(colors = wes_palette("Zissou1", length(unique(outp$code_site_station)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=DataTimezone)

    # Condition to build barplot for precipitation
    if(unique(outp$variable)=="P_1_1_1" & length(unique(outp$code_site_station)) >=1){graph<-graph %>% dyBarChart()}else{graph}
    if(unique(outp$variable)=="P_1_1_1" & length(unique(outp$code_site_station)) >1){graph<- graph %>% dyMultiColumn()}else{graph}
    graph
    })
  }

    dy_graph <- tagList(res)
    return(dy_graph)
}

#' @title dygraphPiezo
#' @description Time trend with dygraphs library for different piezo for the same site
#' @param subsetoutbd data.table from mod_accessdata
#' @param frequence : String of frequency (ex. "30 min", "hour", "day", "month")
#' @return dygraph of timetrend
#' @importFrom data.table setDT
#' @importFrom shiny tagList
#' @importFrom xts xts
#' @import dygraphs
#' @export
dygraphPiezo <- function(subsetoutbd,frequence){
if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
  dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
  fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by=frequence)

  sitePiezo <- unique(subsetoutbd$site_description)
  
  res <- lapply(1:length(sitePiezo),function(z){#loop for each site

    # Select type of piezo
    outp <- subsetoutbd[subsetoutbd$site_description %in% sitePiezo[z],]    
    outp <- outp[order(outp$variable),]

    lapply(unique(outp$variable),function(x){#For each site, build time serie by variable
            dataxts <- do.call("cbind", lapply(unique(outp$station_nom),function(y){#For each piezo, build dbtimes-series
              if(nrow(outp[variable %in% x,])==0){
                NULL
              }else{  
                #Build complete time series
                tmp <-  outp[station_nom %in% y & variable %in% x,list(Date,value)]
                tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
                db <- xts(tmp2[,value],order.by=tmp2[,Date])
                colnames(db) <- y
                db
              }
            }))
            if(grepl("year",frequence)){
              axisYears<-"function(d){ return d.getFullYear() }"
              tickerYears <- "function(a, b, pixels, opts, dygraph, vals) { 
                              return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}"
              DataTimezone <- FALSE
                }else{
                  axisYears<-NULL
                  tickerYears <- NULL
                  DataTimezone <- TRUE
                }

                dygraph(dataxts,group=z,main = paste(unique(outp[variable %in% x,"definition"])," (",sitePiezo[z],")",sep=""),ylab = unique(outp[variable %in% x,"unite"])[[1]],height = 250,width="100%")%>%
                dyAxis("x", axisLabelFormatter=axisYears,
                ticker= tickerYears) %>%
                dyOptions(stackedGraph = FALSE) %>%
                dyRangeSelector(height = 20,strokeColor = "") %>%
                dyLegend(show = "onmouseover") %>% 
                dyOptions(sigFigs=3,colors = wes_palette("Zissou1", length(unique(outp$station_nom)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=TRUE)%>%
                dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
                dyCSScool() %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))
            })            
        })#end of res
}
    dy_graph <- tagList(res)
    return(dy_graph)
}

#' @title timelineDataAvailable
#' @description Chart of data disponibility (used in mod_welcome)
#' @param tableCarac data.table from mod_welcome
#' @param allSite Logical operator to select allSite or not (TRUE, FALSE)
#' @param facetWrapOption Logical operator to use facetWrap on site or site/station (TRUE, FALSE)
#' @param translator translator object for translation
#' @return dygraph of timetrend
#' @importFrom data.table setDT
#' @importFrom wesanderson wes_palette
#' @importFrom plotly ggplotly layout
#' @import ggplot2
#' @export
timelineDataAvailable <- function(tableCarac,allSite,facetWrapOption,translator){
  keyWord <- c("Soil temperature","Soil water content","Soil heat flux","Température du sol","Teneur en eau du sol","Flux de chaleur dans le sol")
  keyWord <- paste(keyWord,collapse = "|")
  tableCarac[,definition_simple:=ifelse(grepl(keyWord, definition)==TRUE,gsub(paste0("(",keyWord,").*"),"\\1",definition),definition)]

# Build ggplot chart
gg <- ggplot(unique(tableCarac),aes(colour=theme,text = 
                                                paste('<br>Begin:', mindate,
                                                      '<br>End: ', maxdate,
                                                      '<br>Variable: ', definition_simple)))+
      geom_segment(aes(x=mindate, xend=maxdate, y=variable, yend=variable),size=1)+
      labs(fill="",x="",y = "")+
      scale_colour_manual(values=wes_palette("Darjeeling1",length(unique(tableCarac$theme)),type="discrete"),name=translator$t("Thème")) +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      theme_chart()
      
      {if(allSite==TRUE & facetWrapOption==TRUE) gg <- gg+facet_wrap(~site_nom) else gg}
      {if(allSite==FALSE & facetWrapOption==TRUE) gg <- gg+facet_wrap(~station_nom) else gg}
    
return(ggplotly(gg,tooltip = c("text"))%>%layout(legend = list(orientation = "h", x = 0.4, y = -0.05))
)

}

#' @title sensorSelectedMap
#' @description Chart of disponibility data (welcome module)
#' @param mapSensorSelected data.table from mod_welcome
#' @param allSite Logical operator to select allSite or not (TRUE, FALSE)
#' @param translator translator object for translation
#' @return leaflet
#' @importFrom stringr str_extract_all
#' @import leaflet
#' @export
sensorSelectedMap <- function(mapSensorSelected,allSite=FALSE,translator){

    # Extraction of geographic coordinate of stations
    mapSensorSelected$lng <- matrix(as.numeric(unlist(
      str_extract_all(mapSensorSelected$zet_coordonnees_bbox, pattern = "-?\\d+\\.?\\d*")
    )), ncol = 2, byrow = T)[,1]
    
    mapSensorSelected$lat <- matrix(as.numeric(unlist(
      str_extract_all(mapSensorSelected$zet_coordonnees_bbox, pattern = "-?\\d+\\.?\\d*")
    )), ncol = 2, byrow = T)[,2]

    description_station <- paste0("<b>Site/Station : </b>",mapSensorSelected$code_site_station,"<br/>",
                                 "<b>Station : </b>",mapSensorSelected$station_description,"<br/>")
    
    # Condition to display all sites
    if(allSite==TRUE){
      lng <- 2.067
      lat <- 46
      zoom <- 5 

    mapSiteSelected <- mapSensorSelected[,.(lngSite = mean(lng,na.rm=TRUE),
                     latSite = mean(lat,na.rm=TRUE)),by=list(site_nom)]
    description_site <- paste0("<b>Site SNO-Tourbières : </b>",mapSiteSelected$site_nom,"<br/>")
    }else{
      # lng and lat on the last line
      lng <- mapSensorSelected[,.SD[.N]]$lng
      lat <- mapSensorSelected[,.SD[.N]]$lat
      zoom <- 16
    }

map <- leaflet(mapSensorSelected)%>%
      setView(lng = lng, lat = lat,zoom=zoom)%>%
      
      # add tules (see more http://leaflet-extras.github.io/leaflet-providers/preview/)
      addProviderTiles("OpenStreetMap.DE",group="Open Street Map")%>%
      addProviderTiles("Esri.WorldImagery",group = "Image satellite") %>%
      
      # Ajout des stations 
      addCircleMarkers(lng = mapSensorSelected$lng, lat = mapSensorSelected$lat, 
                       radius=6,
                       fillColor = mapSensorSelected$couleur,color="#000000",weight = 1.7,opacity = 0.8,
                       stroke = TRUE, fillOpacity = 0.8, popup = description_station)%>%

      # Add button to come back to France map
      addEasyButton(easyButton(
        icon="fa-globe", title="France",
        onClick=JS("function(btn, map){map.setZoom(6); }")))  %>%

      # For control layers
      addLayersControl(
        baseGroups = c("Open Street Map", "Image satellite"),
        options = layersControlOptions(collapsed = FALSE),position = "topleft"
      )%>%

      leaflet::addLegend(position = 'topright',
                          colors = unique(mapSensorSelected$couleur), 
                          labels = unique(mapSensorSelected$type),title=translator$t("Type de station"))
      
      # Add all sites
      {if(allSite==TRUE) map <- addAwesomeMarkers(map,lng = mapSiteSelected$lngSite,
       lat = mapSiteSelected$latSite,popup = description_site) else map}

      return(map)
}