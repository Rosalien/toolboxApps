#' @title robustCurl
#' @description tryCatch
#' @param x a httr::GET(url)
#' @importFrom httr GET timeout
#' @return Error or warning message if httr::GET() don't work
#' @examples
#' \dontrun{
#' robustCurl(httr::GET(
#' "http://localhost:8081/rest/resources/pivot?codes_jeu=lgt-ges-eddycovariance"),
#' httr::timeout(60)))
#' }
#' @export
#' 
robustCurl= function(x) {
    tryCatch(x,
            warning = function(w) {print("warning")},
            error = function(e) {print("Problem with creation of metadata")}) 
}

#' @title createLink
#' @description To creat an html link
#' @param val String of url (ex. "https://data-snot.cnrs.fr/")
#' @return String of HTML code of url with '_blank' target configuration
#' @examples
#' createLink("https://data-snot.cnrs.fr/")
#' @export
#' 
createLink <- function(val) {
  paste0('<a href="', val,'" target="_blank">', val ,'</a>')
}

#' @title variableToVariableSimple
#' @description Transform biometeorological variable code into simple variable code (ex. "G_1_1_1" into "G")
#' @param x String of biometeorological variable code with (ex. "G_1_1_1" )
#' @return String of variable code (ex. "G")
#' @importFrom stringr str_replace_all
#' @examples
#' variableToVariableSimple("G_1_1_1")
#' @export
#' 
variableToVariableSimple <- function(x){
  y <- str_replace_all(x, "[[:punct:]]", "")
  y <- str_replace_all(y , "[[1-9+]]", "")
  y
}

#' @title loadCaracDataAndCarto
#' @description Load data caracdata() and create 3 data.table used in application
#' @param pool data base configuration (from confConnexion function)
#' @param language String of language ("fr" or "en")
#' @return 3 data.table 
#' @importFrom data.table setDT
#' @export
#' 
loadCaracDataAndCarto <- function(pool,language){
  col_station <- read.csv("inst/app/www/csv/datatype_couleur.csv",sep=";",header=TRUE,stringsAsFactors=FALSE)
  caracDataSensor <- caracdata(pool,language)[order(variable)]
  
  variableCaracData <- c("code_jeu","code_site","code_station","code_site_station","site_nom","theme","datatype","variable","unite","mindate","maxdate","zet_coordonnees_bbox")
  metadataVariable <- c("definition","station_description","site_description","station_nom")
  metadataSensor <- c("fabricant","instrument","description_capteur","station_description")
  metadataMethod <- c("description_methode")

  caracData <- unique(caracDataSensor[,c(variableCaracData,metadataVariable),with=FALSE])
  caracCartoMethod <- unique(caracDataSensor[,c(variableCaracData,metadataSensor,metadataMethod),with=FALSE])
  caracCarto <- unique(caracDataSensor[,c(variableCaracData,metadataSensor),with=FALSE])
  
  ## Select column with language
  col_station <- col_station[,c("datatype","couleur",paste0("type_",language))]
  names(col_station) <- c(names(col_station[1:2]),"type")

  return(list(caracData=caracData,caracCarto=caracCarto,caracCartoMethod=caracCartoMethod,col_station=col_station))
}

#' @title dbDayNight
#' @description Create day and night parameter into 'dataSelected' data.table
#' @param dataSelected data.table from mod_accessdata
#' @return day and night parameter into 'dataSelected' data.table
#' @importFrom data.table setDT rbindlist setkey
#' @importFrom suncalc getSunlightTimes
#' @export
#' 
dbDayNight <- function(dataSelected){
  site <- c("lgt","bdz","ldm","frn")
  lat <- c(47.3218442722356,42.8027601354934,48.4418870264623,46.8249915808878)
  lon <- c(2.28190054512825,1.4235536727897,-1.18761537116659,6.16955686517857)
  siteCoord <- setDT(data.frame(site,lat,lon))

  # Day/Night parameters for each site. Calculate with geographics coordinate
  aggregateDBDayandNight <- rbindlist(lapply(unique(dataSelected$code_site),function(x){
  dataSelectedSite <- dataSelected[code_site %in% x,]
  dataSelectedSite$Date <- as.POSIXct(paste(dataSelectedSite$date,dataSelectedSite$time,sep=" "), "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers")
  fullSequence <- seq(min(dataSelectedSite$Date),max(dataSelectedSite$Date)+3600,by="day")
  sunSetRiseSeq <- setDT(getSunlightTimes(date = as.Date(fullSequence),keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),lat = siteCoord[site %in% x,lat], lon = siteCoord[site %in% x,lon], tz = "Africa/Algiers"))    
      
  setkey(sunSetRiseSeq, date)
  setkey(dataSelectedSite,date)

  # Merge with ifelse for day and night
  dbDayandNight <- sunSetRiseSeq[dataSelectedSite]
  dbDayandNight[,dayNight:=ifelse(Date > sunrise & Date < sunset, 'day', 'night')]
  }))
return(aggregateDBDayandNight)
}

#' @title dbselect
#' @description Calculate average of variable and site/station selected by frequency selected in application.
#' @param db data.table from mod_accessdata
#' @param dayNightSelected String of day, night or all the day selected in application ("day","night","day/night")
#' @param frequenceSelected : String of frequency (ex. "30 min", "hour", "day", "month")
#' @param siteSelected String of site/station code selected in application (ex. "lgt/ec1" or c("lgt/ec1","lgt/bm1"))
#' @param variableSelected String of variable code selected in application (ex. "FC" or c("FC","FCH4"))
#' @return data.table with average of values by frequenceSelected
#' @importFrom data.table setDT month year
#' @importFrom dplyr mutate
#' @export
#'
dbselect <- function(db,dayNightSelected,frequenceSelected,siteSelected,variableSelected){
  # Average calculation with confidition of frequency (frequenceSelected)
  if(dayNightSelected!="day/night"){
      db <- db[dayNight==dayNightSelected,]
    }else{}
   if(frequenceSelected=="30 min"){
     subsetoutbd <- db[code_site_station %in% siteSelected & variable %in% variableSelected,]
     subsetoutbd[,Date:=as.POSIXct(paste(date,time,sep=" "), "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers")]
   }else if(grepl("hour",frequenceSelected)==TRUE){  
    db[,Date:=cut(as.POSIXct(paste(date,time,sep=" "), "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers"),frequenceSelected)]
    subsetoutbd <- db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected!="P_1_1_1"],.(value = mean(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight),]#sunrise,sunset
    subsetoutbd <- rbind(subsetoutbd,db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected=="P_1_1_1"] & complete.cases(db$value),.(value = sum(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)])#,sunrise,sunset
    subsetoutbd[,Date:=as.POSIXct(Date, "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers")]
   }else if((grepl("day",frequenceSelected))|(grepl("week",frequenceSelected))){
    db[,Date:=cut(as.POSIXct(paste(date,time,sep=" "), "%Y-%m-%d",tz="Africa/Algiers"),frequenceSelected)]
    subsetoutbd <- db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected!="P_1_1_1"],.(value = mean(value,na.rm=TRUE)), by = list(date=Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)]
    subsetoutbd <- rbind(subsetoutbd,db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected=="P_1_1_1"] & complete.cases(db$value),.(value = sum(value,na.rm=TRUE)), by = list(date=Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)])
    subsetoutbd[,Date:= as.POSIXct(date, "%Y-%m-%d",tz="Africa/Algiers")]
   }else if(grepl("month",frequenceSelected)|grepl("year",frequenceSelected)){
    db[,Date:=cut(as.POSIXct(paste(date,time,sep=" "),tz="Africa/Algiers"),frequenceSelected)]
    subsetoutbd <- db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected!="P_1_1_1"],.(value = mean(value,na.rm=TRUE)), by = list(date=Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)]
    subsetoutbd <- rbind(subsetoutbd,db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected=="P_1_1_1"] & complete.cases(db$value),.(value = sum(value,na.rm=TRUE)), by = list(date=Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)])
    subsetoutbd[,Date:= as.POSIXct(as.Date(date,"%Y-%m-%d"),tz="Africa/Algiers")]
   }else{}

  # 
  subsetoutbd <- subsetoutbd %>% mutate(month = factor(month(Date),1:12,
                                        labels = c("Jan", "Feb", "Mar", "Apr",
                                        "May", "Jun", "Jul", "Aug",
                                        "Sep", "Oct", "Nov", "Dec"),ordered = TRUE),
                                        year= factor(year(Date)),
                                        season=factor(quarters(subsetoutbd$Date), levels = c("Q1", "Q2", "Q3", "Q4"), 
                                        labels = c("winter", "spring", "summer", "fall")))
  setDT(subsetoutbd)
  return(subsetoutbd)
}

#' @title checkboxVariablePedo
#' @description Create a list of variable with a simple definition (without soil profil definition)
#' @param dbCarac data.table from mod_accessdata
#' @param codeVariable String of soil variables (ex. "TS_1_1_1")
#' @param siteBM : String of biometeorological site/station code selected in application. Ex. "lgt/bm1" ou c("lgt/bm2","lgt/bm1")
#' @return data.table with a simple definition (without soil profil definition)
#' @export
#'
checkboxVariablePedo <- function(dbCarac,codeVariable,siteBM){

    listVariables <- unique(dbCarac[code_site_station %in% siteBM & grepl(codeVariable, dbCarac$variable)==TRUE,"variable"])
    listVariables <- listVariables[order(variable),]
    listVariables$definitionsimple <- gsub("[a-zA-Z]", "", listVariables$variable) 
    listVariables$definitionsimple <- substring(listVariables$definitionsimple,2)

    return(listVariables)
}

#' @title extracData
#' @description Fonction used to export data with horizontal format (for download it)
#' @param db data.table from mod_accessdata
#' @param frequence String of frequency (ex. "30 min", "hour", "day", "month")
#' @importFrom data.table setDT
#' @importFrom reshape cast
#' @return data.table with horizontal format
#' @export
#'
extracData <- function(db,frequence){

  fullSequence <- seq(min(db$Date),max(db$Date),by=frequence)
  extractionData <- lapply(unique(db$code_site_station),function(x){
    res <- cast(db[code_site_station %in% x,c("Date","variable","value")],Date~variable,sum)
    names(res) <- c("Date_old",paste(x,"_",names(res[-1]),sep=""))
    res <- setDT(data.frame(Date=fullSequence,with(res,res[match(fullSequence,res$Date_old),])))
    res[,-2]
    })

  # Jointure de la liste
   extractionDataCpt <- Reduce(function(x,y)merge(x, y, by.x="Date", by.y="Date",all=TRUE),extractionData)
      
  # Conversion des NA en -9999
  extractionDataCpt[is.na(extractionDataCpt)] <- -9999
  return(extractionDataCpt)
}

#' @title createFilePivot
#' @description Function to create data in Pivot/TheiaOzcar format
#' @param data data to be integrated into a file with Pivot/TheiaOzcar format
#' @param siteVariable data.table with code_site_station and a variable
#' @param caracDataset table of dataset characteristics (create with caracdata function)
#' @return Retourne une data.table au format Pivot/TheiaOzcar
#' @importFrom stringr str_extract_all 
#' @export
#'
createFilePivot <- function(data,siteVariable,caracDataset){
  
lapply(1:nrow(siteVariable),function(x){

      # Selection of caracDataSet
      siteInLoop <- siteVariable[x,code_site_station]
      variableInLoop <- siteVariable[x,variable]
      caracDatasetObservation <- unique(caracDataset[variable %in% variableInLoop & code_site_station %in% siteInLoop,list(code_site,code_station,theme,datatype,variable,titre,zet_coordonnees_bbox,zet_altitude)])
      dataInLoop <- data[code_site_station %in% siteInLoop,c("dateEnd",variableInLoop),with=FALSE]
      dataInLoop <- na.omit(dataInLoop,variableInLoop)
      
      # Variables header creation (don't used)
      #patternPath <- "^(.*)\\/(.*),(.*),(.*),(.*)-(.*)-(.*)-(.*)"
      #resRegex <- strapply(caracDatasetObservation$path, patternPath, c)[[1]]
      dateExtraction <- format(Sys.Date(), format='%Y-%m-%dT%H:%M:%SZ')
      
      observationId <- paste(caracDatasetObservation$code_site,"-",caracDatasetObservation$code_station,"-",caracDatasetObservation$theme,"-",caracDatasetObservation$datatype,"-",caracDatasetObservation$variable,sep="")
      datasetTitle <- caracDatasetObservation[,titre]
      coordonnees <- caracDatasetObservation[,"zet_coordonnees_bbox",with=FALSE][[1]]
      longitude <- matrix(as.numeric(unlist(
        str_extract_all(coordonnees, pattern = "-?\\d+\\.?\\d*")
      )), ncol = 2, byrow = T)[,1]
      latitude <- matrix(as.numeric(unlist(
      str_extract_all(coordonnees, pattern = "-?\\d+\\.?\\d*")
    )), ncol = 2, byrow = T)[,2]    
      altitude <- caracDatasetObservation[,zet_altitude]
      fileName <- paste("TOUR_OBS_",observationId,".txt",sep="")

      # Build header
      header <- paste("Date_of_Extraction;",dateExtraction,"\n",
        "Observation_ID;",observationId,"\n",
        "Dataset_title;",datasetTitle,"\n",
        "Variable_name;",variableInLoop,sep="")

      print(header[1])
      # Build table
      valueVariable <- dataInLoop[,list(dateEnd)]
      valueVariable$value <- dataInLoop[,variableInLoop,with=FALSE]
      valueVariable$dateBeg <- NA
      valueVariable$longitude <- longitude
      valueVariable$latitude <- latitude
      valueVariable$altitude <- altitude
      valueVariable$qualityFlags <- NA

      valueVariable <- valueVariable[,list(dateBeg,dateEnd,latitude,longitude,altitude,value,qualityFlags)]
    #  valueVariable[is.na(valueVariable$value)] <- -9999
      list(data=valueVariable,header=header,fileName=fileName)
})
}

#' @title writeHeaderFile
#' @description Create a file with header
#' @param x data.frame or data.table
#' @param file String of file name
#' @param header String of header
#' @return Write a file with header
#' @source https://stackoverflow.com/questions/12381117/add-header-to-file-created-by-write-csv
#' @importFrom readr write_csv
#' @export
#'
writeHeaderFile<- function(x, file, header){
# create and open the file connection
  datafile <- file(file, open = 'wt')
# close on exit
  on.exit(close(datafile))
# if a header is defined, write it to the file (@CarlWitthoft's suggestion)
if(!missing(header)) writeLines(header,con=datafile)
# write the file using the defined function and required addition arguments  
  write_csv(x, datafile)
}

#' @title sqlOutputDatasetArchive
#' @description Extract dataset by code_jeu attribute from data baseeCreate a file with header
#' @param pool data base configuration (from confConnexion function)
#' @param caracDataset data.table from caracdata(pool,language)
#' @param checkinJeu String of code_jeu (ex. "lgt-ges-eddycovariance")
#' @param archiveType String "OZCAR" to create archive type for SI Theia/OZCAR. If not, for ZENODO archive
#' @return data.table of a dataset
#' @export
#'
sqlOutputDatasetArchive <- function(pool,caracDataset,checkinJeu,archiveType="ZENODO")({
    # Load parameters for queryDataSNOT
    caracDataset <- caracDataset[code_jeu %in% checkinJeu,]
    variableJeu <- unique(caracDataset[,variable])
    siteJeu <- unique(caracDataset[,code_site_station])
    date_debut <- min(as.Date(unique(caracDataset[,mindate]),"%d-%m-%Y"))
    date_fin <- max(as.Date(unique(caracDataset[,maxdate]),"%d-%m-%Y"))
    periodeJeu <- c(date_debut,date_fin)

    # Launch query
    data <- queryDataSNOT(pool,variableJeu,siteJeu,periodeJeu,melted=FALSE)
    
    if(archiveType=="OZCAR")
      {
        data[,dateEnd:=paste0(date,'T',time,'Z')]
      }else{
        data[,c("datatype"):=NULL]
      }
  })
