#' @title confConnexion
#' @description Connexion configuration to database
#' @param pathConfFile path of yaml file database configuration
#' @return pool configuration
#' @importFrom yaml yaml.load_file
#' @importFrom pool dbPool
#' @importFrom DBI dbDriver
#' @importFrom RPostgreSQL PostgreSQL
#' @examples
#' \dontrun{
#' pool <- confConnexion("~/SI_SNOT/dbconfProd.yaml")
#' }
#' @export

confConnexion <- function(pathConfFile){
  confdb <- yaml.load_file(pathConfFile)
  pool <- dbPool(
    drv = dbDriver("PostgreSQL", max.con = 100),
    dbname = confdb$dbname,
    host = confdb$host,
    port = confdb$port,
    user = confdb$user,
    password = confdb$password,
    idleTimeout = 3600000)
  return(pool)
}

#' @title tableJeu
#' @description Sql query to extrat table jeu from database
#' @param pool data base configuration (from confConnexion function)
#' @return data.table of table jeu
#' @importFrom data.table setDT
#' @importFrom DBI dbGetQuery
#' @export
tableJeu <- function(pool){
  query <- paste("select * from jeu",sep="")
  jeu <- setDT(dbGetQuery(pool, query))
  return(jeu)
}

#' @title tableSousJeu
#' @description Sql query to extrat table sousjeu from database
#' @param pool data base configuration (from confConnexion function)
#' @return data.table of table sousjeu
#' @importFrom data.table setDT
#' @importFrom DBI dbGetQuery
#' @export
tableSousJeu <- function(pool){
  query <- paste("select j.code_jeu,j.titre as titre_dataset,sj.citation,date_debut,date_fin,sj.doi,sj.titre_licence,sj.url_licence
  from sousjeu as sj
  inner join jeu as j on j.jeu_id=sj.jeu_id",sep="")
  sousjeu <- setDT(dbGetQuery(pool, query))
  return(sousjeu)
}

#' @title caracsite
#' @description Sql query to extrat characteristics of sites
#' @param pool data base configuration (from confConnexion function)
#' @param language String of language ("fr" or "en")
#' @return data.table characteristics of sites
#' @importFrom data.table setDT
#' @importFrom DBI dbGetQuery
#' @export
caracsite <- function(pool,language){
variableCaracData <- c("code_site","site_id","altitude")
metadataVariable <- "site"
internationalVariable <- paste0(metadataVariable,"_",language,collapse = ",")

querysite <- paste0("
select ",paste0(variableCaracData,collapse=","),",",internationalVariable,"
from(
select description as site_fr,l.localestring as site_en, name as code_site, zet_altitude as altitude,site.site_id
from site 
inner join site_snot on site_snot.site_id=site.site_id
inner join localisation as l on l.defaultstring=description
where parent_site_id is null
and l.colonne like 'name')a
")

site <- setDT(dbGetQuery(pool, querysite))
names(site) <- c(variableCaracData,metadataVariable)
return(site)
}

#' @title caracdata
#' @description Sql query to extrat characteristics of variables and site for all datatype
#' @param pool data base configuration (from confConnexion function)
#' @param language String of language ("fr" or "en")
#' @return data.table of public.carac_data_sensor_method_prod
#' @importFrom data.table setDT
#' @importFrom data.table setkey
#' @importFrom DBI dbGetQuery
#' @export

caracdata <- function(pool,language){
variableCaracData <- c("code_jeu","code_site","code_station","code_site_station","site_nom","theme","datatype","variable","unite","mindate","maxdate","fabricant","instrument","zet_coordonnees_bbox")
metadataVariable <- c("definition","station_description","site_description","station_nom")
metadataSensor <- c("description_capteur","station_description")
metadataMethod <- c("description_methode")
metadataJeu <- c("titre","descriptionjeu","genealogie","doi","zet_altitude")

internationalVariable <- paste0(c(metadataVariable,metadataSensor,metadataMethod),"_",language,collapse = ",")
querydata <- paste("
select ",paste0(variableCaracData,collapse=","),",",internationalVariable,",",paste0(metadataJeu,collapse=","),"
from public.carac_data_sensor_method_prod
")
data <- setDT(dbGetQuery(pool, querydata))
names(data) <- c(variableCaracData,metadataVariable,metadataSensor,metadataMethod,metadataJeu)
setkey(data, variable,code_site_station, datatype)

return(data)
}

#' @title queryDataSNOT
#' @description Sql query to extrat values of variable from site/station and period selected
#' @param pool data base configuration (from confConnexion function)
#' @param siteSelected String of site/station code selected in application (ex. "lgt/ec1" or c("lgt/ec1","lgt/bm1"))
#' @param variableSelected String of variable code selected in application (ex. "FC" or c("FC","FCH4"))#'
#' @param periodeSelected Vector string of period selected with "yyyy-mm-dd" format (ex. c("2008-01-15","2014-05-25"))
#' @param melted Logical. If TRUE convert melt data.table. If FALSE, no melt (used for dataset-archive)
#' @return data.table of values
#' @importFrom data.table setDT setkey
#' @importFrom DBI dbGetQuery
#' @importFrom reshape melt
#' @export
queryDataSNOT <- function(pool,variableSelected,siteSelected,periodeSelected,melted=TRUE){
  variablequery <- as.matrix(paste("max(case when variable='",variableSelected,"' then value else null end) \"",variableSelected,"\"",sep=""))
  variablecharacter <- apply(variablequery,2,paste,collapse=",")

  queryValueSNOT <- paste("select code_site_station,date,time,datatype,
      ",variablecharacter,"
      from data_infraj_prod
      where date between '",periodeSelected[1],"' and '",periodeSelected[2],"'
      and code_site_station like any ('{",paste(siteSelected,collapse=","),"}' )
      group by code_site_station, date, time,datatype",sep="")

  data <- setDT(dbGetQuery(pool, queryValueSNOT))
  if(melted==TRUE){
    meltvalue <- setDT(reshape::melt(data,id=1:4,na.rm=TRUE))
    is.na(meltvalue$value) <- meltvalue$value==-9999  
    meltvalue$variable <- as.character(meltvalue$variable)
    setkey(meltvalue, variable, code_site_station, datatype)  
    return(meltvalue)
  }else{
      return(data)
  }
}

#' @title caracCarto
#' @description Sql query to extrat characteristics of variables for welcome module
#' @param pool data base configuration (from confConnexion function)
#' @param language String of language ("fr" or "en")
#' @return data.table of public.carac_data_sensor_method_prod
#' @importFrom data.table setDT
#' @importFrom DBI dbGetQuery
#' @importFrom DBI sqlInterpolate
#' @importFrom DBI ANSI
#' @export
caracCarto <- function(pool,language){
variableCaracData <- c("code_site_station","datatype","variables","unite","fabricant","instrument","infos_calibration","zet_coordonnees_bbox","parent_site_id")
metadataVariable <- c("description_capteur","station_description")
internationalVariable <- paste0(metadataVariable,"_",language,collapse = ",")

queryValueSiteStation <- sqlInterpolate(ANSI(),paste0("select ",paste0(variableCaracData,collapse = ","),",",internationalVariable,"
from(
select b[1] as code_site_station, b[2] as datatype,b[3] as variables,b[4] as unite,
  fabricant, dt.code as instrument, dt.description as description_capteur_fr,l.localestring as description_capteur_en,infos_calibration,zet_coordonnees_bbox,site.description as station_description_fr,l2.localestring as station_description_en,parent_site_id
                                   from(
                                   select regexp_split_to_array(a[4],'-'),fabricant, code, description, infos_calibration
                                   from(
                                   select regexp_split_to_array(path, ','),fabricant, i.code, description, infos_calibration
                                   from periode_utilisation_instrument as pui
                                   inner join instrument as i on i.instr_id=pui.instr_id
                                   inner join realnode as rn on rn.id=stdtvar_id
                                   ) as dt(a)
                                   ) as dt(b)
                                   inner join composite_nodeable as cn on cn.code=b[1]
                                   inner join site on site.site_id= cn.id
                                   inner join site_snot on site_snot.site_id=site.site_id
                                   inner join localisation as l on l.defaultstring=dt.description
                                   inner join localisation as l2 on l2.defaultstring=site.description
                                   where l.colonne like 'description'
                   and l2.colonne like 'description'
  )a"))
outdbmap <- setDT(dbGetQuery(pool, queryValueSiteStation))
names(outdbmap) <- c(variableCaracData,metadataVariable)

return(outdbmap)
}