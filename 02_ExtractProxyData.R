###EXTRACT PROXY VARIABLES FROM 12K DATABASE-------------------------------------------------------------------------------------------------------
library(xlsx)
library(berryFunctions)
library(terra)
library(rlist)
library(astrochron)

proxy.vars <- list()

for(i in seq(1,length(D.sel)))
{
  if(length(D.sel[[i]]$paleoData) == 1)##get number of paleodata tables
  {
    vars <- which(names(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]]) %in% c("d18O", "d13C", "temperature", "temperature-1", "temperature-2", "temperature_1", "temperature_2", "temperature_3", "temperature_4",
                                                                                "temperatureComposite", "precipitation"))
    
    if(length(vars) >= 1)
    {
      proxy.vars[[i]] <-as.data.frame(matrix(data=NA, nrow=length(vars), ncol=18))
      colnames(proxy.vars[[i]]) <- c("dataSetName", "paleoData.No", "archiveType", "Country", "Lon", "Lat", "listName", "maxYear", "minYear", 
                                     "variableName", "proxy", "units", "seasonality", "seasonalityOriginal", "variableGroup", "variableDetail", "modernTemperature", "age.units")
      
      for(q in seq(1, length(vars)))
      {
        proxy.vars[[i]][q,"dataSetName"] <- D.sel[[i]]$dataSetName
        proxy.vars[[i]][q,"paleoData.No"] <- 1
        proxy.vars[[i]][q,"archiveType"] <- D.sel[[i]]$archiveType
        if(!is.null(D.sel[[i]]$geo$countryOcean)) {proxy.vars[[i]][q,"Country"] <- D.sel[[i]]$geo$countryOcean}
        proxy.vars[[i]][q,"Lon"] <- D.sel[[i]]$geo$longitude
        proxy.vars[[i]][q,"Lat"] <- D.sel[[i]]$geo$latitude
        proxy.vars[[i]][q,"listName"] <- names(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]])[vars[q]]
        if(!is.null(D.sel[[i]]$maxYear)) {proxy.vars[[i]][q,"maxYear"] <- D.sel[[i]]$maxYear}
        if(!is.null(D.sel[[i]]$minYear)) {proxy.vars[[i]][q,"minYear"] <- D.sel[[i]]$minYear}
        if(!is.null(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$variableName)) {proxy.vars[[i]][q,"variableName"] <- D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$variableName}
        if(!is.null(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$proxy)) {proxy.vars[[i]][q,"proxy"] <- D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$proxy}
        if(!is.null(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$units)) {proxy.vars[[i]][q,"units"] <- D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$units}
        if(!is.null(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$seasonality)) {proxy.vars[[i]][q,"seasonality"] <- D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$seasonality}
        if(!is.null(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$seasonalityOriginal)) {proxy.vars[[i]][q,"seasonalityOriginal"] <- D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$seasonalityOriginal}
        if(!is.null(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$variableGroup)) {proxy.vars[[i]][q,"variableGroup"] <- D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$variableGroup}
        if(!is.null(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$variableDetail)) {proxy.vars[[i]][q,"variableDetail"] <- D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$variableDetail}
        if(!is.null(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$modernTemperature)) {proxy.vars[[i]][q,"modernTemperature"] <- D.sel[[i]]$paleoData[[1]]$measurementTable[[1]][[vars[q]]]$modernTemperature}
        if(!is.null(D.sel[[i]]$paleoData[[1]]$measurementTable[[1]]$age$units)) {proxy.vars[[i]][q,"age.units"] <- D.sel[[i]]$paleoData[[1]]$measurementTable[[1]]$age$units}
        
      }
    }
  }
  if(length(D.sel[[i]]$paleoData) > 1)#if there are more paleodata tables
  {
    sublist <- list()
    
    for(w in seq(1, length(D.sel[[i]]$paleoData)))#sequence for paleodata tables
    {
      vars <- which(names(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]]) %in% c("d18O", "d13C", "temperature", "temperature-1", "temperature-2", "temperature_1", "temperature_2", "temperature_3", "temperature_4",
                                                                                  "temperatureComposite", "precipitation"))
      
      if(length(vars) >= 1)
      {
        sublist[[w]] <-as.data.frame(matrix(data=NA, nrow=length(vars), ncol=18))
        colnames(sublist[[w]]) <- c("dataSetName", "paleoData.No", "archiveType", "Country", "Lon", "Lat", "listName", "maxYear", "minYear", 
                                    "variableName", "proxy", "units", "seasonality", "seasonalityOriginal", "variableGroup", "variableDetail", "modernTemperature", "age.units")
        
        for(q in seq(1, length(vars)))
        {
          sublist[[w]][q,"dataSetName"] <- D.sel[[i]]$dataSetName
          sublist[[w]][q,"paleoData.No"] <- w
          sublist[[w]][q,"archiveType"] <- D.sel[[i]]$archiveType
          if(!is.null(D.sel[[i]]$geo$countryOcean)) {sublist[[w]][q,"Country"] <- D.sel[[i]]$geo$countryOcean}
          sublist[[w]][q,"Lon"] <- D.sel[[i]]$geo$longitude
          sublist[[w]][q,"Lat"] <- D.sel[[i]]$geo$latitude
          sublist[[w]][q,"listName"] <- names(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]])[vars[q]]
          if(!is.null(D.sel[[i]]$maxYear)) {sublist[[w]][q,"maxYear"] <- D.sel[[i]]$maxYear}
          if(!is.null(D.sel[[i]]$minYear)) {sublist[[w]][q,"minYear"] <- D.sel[[i]]$minYear}
          if(!is.null(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$variableName)) {sublist[[w]][q,"variableName"] <- D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$variableName}
          if(!is.null(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$proxy)) {sublist[[w]][q,"proxy"] <- D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$proxy}
          if(!is.null(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$units)) {sublist[[w]][q,"units"] <- D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$units}
          if(!is.null(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$seasonality)) {sublist[[w]][q,"seasonality"] <- D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$seasonality}
          if(!is.null(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$seasonalityOriginal)) {sublist[[w]][q,"seasonalityOriginal"] <- D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$seasonalityOriginal}
          if(!is.null(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$variableGroup)) {sublist[[w]][q,"variableGroup"] <- D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$variableGroup}
          if(!is.null(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$variableDetail)) {sublist[[w]][q,"variableDetail"] <- D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$interpretation[[1]]$variableDetail}
          if(!is.null(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$modernTemperature)) {sublist[[w]][q,"modernTemperature"] <- D.sel[[i]]$paleoData[[w]]$measurementTable[[1]][[vars[q]]]$modernTemperature}
          if(!is.null(D.sel[[i]]$paleoData[[w]]$measurementTable[[1]]$age$units)) {sublist[[w]][q,"age.units"] <- D.sel[[i]]$paleoData[[w]]$measurementTable[[1]]$age$units}
          
        }
      }
    }
    
    proxy.vars[[i]] <- do.call(rbind.data.frame, sublist)
  }
}

proxy.vars <- do.call(rbind.data.frame, proxy.vars)
nrow(proxy.vars)

write.xlsx(proxy.vars, file="12k_proxy_variables.xlsx", sheetName = "12k_proxy_vars")

table(proxy.vars[, c("proxy", "seasonality")])
table(proxy.vars[, c("seasonality", "seasonalityOriginal")])

##unify temperature names
proxy.vars$my.variableName <- proxy.vars$variableName
proxy.vars$my.variableName[proxy.vars$variableName %in% c("temperature_1", "temperature_2", "temperature_3", "temperature_4", "temperatureComposite") ] <- "temperature" 
table(proxy.vars$my.variableName)

table(proxy.vars[proxy.vars$my.variableName == "temperature", c("seasonality", "seasonalityOriginal")])
table(proxy.vars[, c("seasonality", "seasonalityOriginal")])

###unify seasonality
proxy.vars$my.seasonality <- proxy.vars$seasonality
proxy.vars$my.seasonality[proxy.vars$seasonality == "1" ] <- "January" 
proxy.vars$my.seasonality[proxy.vars$seasonality == "7" ] <- "July"
proxy.vars$my.seasonality[proxy.vars$seasonality == "6 7 8"] <- "Summer" 
proxy.vars$my.seasonality[proxy.vars$seasonality == "6,7,8"] <- "Summer" 
proxy.vars$my.seasonality[proxy.vars$seasonality == "JJA"] <- "Summer"
proxy.vars$my.seasonality[proxy.vars$seasonality == "summer"] <- "Summer"
proxy.vars$my.seasonality[proxy.vars$seasonality == "Tann"] <- "annual"
proxy.vars$my.seasonality[proxy.vars$seasonality == "warmest + coldest months" & proxy.vars$seasonalityOriginal == "annual"] <- "annual" 
proxy.vars$my.seasonality[proxy.vars$seasonality == "warmest + coldest months" & proxy.vars$seasonalityOriginal == "1 2 3 4 5 6 7 8 9 10 11 12"] <- "annual" 

table(proxy.vars[, c("proxy", "my.seasonality")])

###EXTRACT PROXY DATA----------------------------------------------------------------------

##select proxy with units
table(proxy.vars$units)
proxy.vars.sel <- proxy.vars[proxy.vars$units %in% c("degC", "mm/yr", "mm"),]
nrow(proxy.vars.sel)

###select seasonalities
table(proxy.vars.sel[, c("proxy", "my.seasonality")])
proxy.vars.sel <- proxy.vars.sel[proxy.vars.sel$my.seasonality %in% c("January", "July", "annual", "coldest month", "Summer", "warmest month"),]
nrow(proxy.vars.sel)

##remove lake surfaces
proxy.vars.sel <- proxy.vars.sel[-c(which(proxy.vars.sel$variableDetail == "lake@surface")), ]
nrow(proxy.vars.sel)

###select proxy types
table(proxy.vars.sel[, c("proxy", "my.seasonality")])
table(proxy.vars.sel[, c("proxy", "my.variableName")])

proxy.vars.sel <- proxy.vars.sel[proxy.vars.sel$proxy %in% c("chironomid", "pollen"),]
nrow(proxy.vars.sel)

table(proxy.vars.sel[, c("proxy", "my.seasonality", "my.variableName")])

##annual precipitation
sites.pollen.Pannual <- proxy.vars.sel[proxy.vars.sel$my.variableName == "precipitation" & 
                                       proxy.vars.sel$proxy == "pollen" &
                                       proxy.vars.sel$my.seasonality == "annual", ]
nrow(sites.pollen.Pannual)

#annual temperature for pollen
sites.pollen.Tannual <- proxy.vars.sel[proxy.vars.sel$my.variableName == "temperature" & 
                                         proxy.vars.sel$proxy == "pollen" &
                                         proxy.vars.sel$my.seasonality == "annual", ]
nrow(sites.pollen.Tannual)

#temperature of the coldest month for pollen
#merge the coldest month and January
sites.pollen.Tcold <- proxy.vars.sel[proxy.vars.sel$my.variableName == "temperature" & 
                                       proxy.vars.sel$proxy == "pollen" &
                                       proxy.vars.sel$my.seasonality %in% c("coldest month", "January"), ]
nrow(sites.pollen.Tcold)

#temperature of the warmest month for pollen
#merge the warmest month and July
sites.pollen.Twarm <- proxy.vars.sel[proxy.vars.sel$my.variableName == "temperature" & 
                                       proxy.vars.sel$proxy == "pollen" &
                                       proxy.vars.sel$my.seasonality %in% c("warmest month", "July"), ]
nrow(sites.pollen.Twarm)


###check "duplicated" records from one site
length(sites.pollen.Pannual$dataSetName)
length(unique(sites.pollen.Pannual$dataSetName))

length(sites.pollen.Tannual$dataSetName)
length(unique(sites.pollen.Tannual$dataSetName))

sites.pollen.Tannual[duplicated(sites.pollen.Tannual$dataSetName),]
sites.pollen.Tannual[sites.pollen.Tannual$dataSetName %in% c("PeatlandKlukva.Novenko.2015", "StaroselskyMoch.Novenko.2018"), ]
sites.pollen.Tannual <- sites.pollen.Tannual[!duplicated(sites.pollen.Tannual$dataSetName),]

length(sites.pollen.Twarm$dataSetName)
length(unique(sites.pollen.Twarm$dataSetName))

length(sites.pollen.Tcold$dataSetName)
length(unique(sites.pollen.Tcold$dataSetName))

###ADD PROJECTED COORDINATES---------------------------------------------------------------

sites.pollen.Tannual[, c("X", "Y")] <- terra::project(x = as.matrix(sites.pollen.Tannual[,c("Lon", "Lat")]), from = "EPSG:4326", to = "EPSG:3035" )
sites.pollen.Tcold[, c("X", "Y")] <- terra::project(x = as.matrix(sites.pollen.Tannual[,c("Lon", "Lat")]), from = "EPSG:4326", to = "EPSG:3035" )
sites.pollen.Twarm[, c("X", "Y")] <- terra::project(x = as.matrix(sites.pollen.Tannual[,c("Lon", "Lat")]), from = "EPSG:4326", to = "EPSG:3035" )
sites.pollen.Pannual[, c("X", "Y")] <- terra::project(x = as.matrix(sites.pollen.Tannual[,c("Lon", "Lat")]), from = "EPSG:4326", to = "EPSG:3035" )

###INTERPOLATE PROXY-INFERRED DATA ALONG CCSM3 WINDOWS-------------------------------------

#ANNUAL TEMPERATURE
pollen.Tannual <- list()

for(i in seq(1,nrow(sites.pollen.Tannual)))
{
  l <- D.sel[[sites.pollen.Tannual$dataSetName[i]]]
  p <- l$paleoData[[sites.pollen.Tannual$paleoData.No[i]]]$measurementTable[[1]][[sites.pollen.Tannual$listName[i]]]$values
  a <- l$paleoData[[sites.pollen.Tannual$paleoData.No[i]]]$measurementTable[[1]]$age$values
  
  tw <- cut(a, breaks=seq(0,21000, 100), labels=seq(50, 20950, 100))
  p <- aggregate(p ~ tw, FUN=mean, na.rm=T)
  p$tw <- as.numeric(as.character(p$tw))
  
  pollen.Tannual[[i]] <- data.frame(tw = seq(min(p$tw), max(p$tw), 100),
                                    Tannual = NA)
  pollen.Tannual[[i]]$Tannual <- approx(x = p$tw, y = p$p,
                                        xout = pollen.Tannual[[i]]$tw)$y
  pollen.Tannual[[i]]$Tannual <- round(pollen.Tannual[[i]]$Tannual, 1)
  
}
names(pollen.Tannual) <- sites.pollen.Tannual$dataSetName

pollen.Tannual.long <- do.call(rbind.data.frame, pollen.Tannual)

#ANNUAL PRECIPITATION
pollen.Pannual <- list()

for(i in seq(1,nrow(sites.pollen.Pannual)))
{
  l <- D.sel[[sites.pollen.Pannual$dataSetName[i]]]
  p <- l$paleoData[[sites.pollen.Pannual$paleoData.No[i]]]$measurementTable[[1]][[sites.pollen.Pannual$listName[i]]]$values
  a <- l$paleoData[[sites.pollen.Pannual$paleoData.No[i]]]$measurementTable[[1]]$age$values
  
  tw <- cut(a, breaks=seq(0,21000, 100), labels=seq(50, 20950, 100))
  p <- aggregate(p ~ tw, FUN=mean, na.rm=T)
  p$tw <- as.numeric(as.character(p$tw))
  
  pollen.Pannual[[i]] <- data.frame(tw = seq(min(p$tw), max(p$tw), 100),
                                    Pannual = NA)
  pollen.Pannual[[i]]$Pannual <- approx(x = p$tw, y = p$p,
                                        xout = pollen.Pannual[[i]]$tw)$y
  pollen.Pannual[[i]]$Pannual <- round(pollen.Pannual[[i]]$Pannual, 1)
}
names(pollen.Pannual) <- sites.pollen.Pannual$dataSetName

pollen.Pannual.long <- do.call(rbind.data.frame, pollen.Pannual)

#TEMPERATURE OF THE COLDEST MONTH (JANUARY)
pollen.Tcold <- list()

for(i in seq(1,nrow(sites.pollen.Tcold)))
{
  l <- D.sel[[sites.pollen.Tcold$dataSetName[i]]]
  p <- l$paleoData[[sites.pollen.Tcold$paleoData.No[i]]]$measurementTable[[1]][[sites.pollen.Tcold$listName[i]]]$values
  a <- l$paleoData[[sites.pollen.Tcold$paleoData.No[i]]]$measurementTable[[1]]$age$values
  
  tw <- cut(a, breaks=seq(0,21000, 100), labels=seq(50, 20950, 100))
  p <- aggregate(p ~ tw, FUN=mean, na.rm=T)
  p$tw <- as.numeric(as.character(p$tw))
  
  pollen.Tcold[[i]] <- data.frame(tw = seq(min(p$tw), max(p$tw), 100),
                                  Tcold = NA)
  pollen.Tcold[[i]]$Tcold <- approx(x = p$tw, y = p$p,
                                    xout = pollen.Tcold[[i]]$tw)$y
  pollen.Tcold[[i]]$Tcold <- round(pollen.Tcold[[i]]$Tcold, 1)
  
}
names(pollen.Tcold) <- sites.pollen.Tcold$dataSetName

pollen.Tcold.long <- do.call(rbind.data.frame, pollen.Tcold)

#TEMPERATURE OF THE WARMEST MONTH (JULY)
pollen.Twarm <- list()

for(i in seq(1,nrow(sites.pollen.Twarm)))
{
  l <- D.sel[[sites.pollen.Twarm$dataSetName[i]]]
  p <- l$paleoData[[sites.pollen.Twarm$paleoData.No[i]]]$measurementTable[[1]][[sites.pollen.Twarm$listName[i]]]$values
  a <- l$paleoData[[sites.pollen.Twarm$paleoData.No[i]]]$measurementTable[[1]]$age$values
  
  tw <- cut(a, breaks=seq(0,21000, 100), labels=seq(50, 20950, 100))
  p <- aggregate(p ~ tw, FUN=mean, na.rm=T)
  p$tw <- as.numeric(as.character(p$tw))
  
  pollen.Twarm[[i]] <- data.frame(tw = seq(min(p$tw), max(p$tw), 100),
                                  Twarm = NA)
  pollen.Twarm[[i]]$Twarm <- approx(x = p$tw, y = p$p,
                                    xout = pollen.Twarm[[i]]$tw)$y
  pollen.Twarm[[i]]$Twarm <- round(pollen.Twarm[[i]]$Twarm, 1)
  
  ##extract ccsm3 data
  mod <- ccsm3[[sites.pollen.Twarm$dataSetName[i]]]
  
  pollen.Twarm[[i]]$tmean07 <- round(mod[as.character(pollen.Twarm[[i]]$tw), "tmean07"], 1)
  pollen.Twarm[[i]]$bio5 <- round(mod[as.character(pollen.Twarm[[i]]$tw), "bio5"], 1)
  
  ##anomalies
  pollen.Twarm[[i]]$D.Twarm <- round(pollen.Twarm[[i]]$Twarm - sites.obsClimate[sites.pollen.Twarm$dataSetName[i] ,"tmean07"], 1)
  pollen.Twarm[[i]]$D.tmean07 <- round(mod[as.character(pollen.Twarm[[i]]$tw), "D.tmean07"], 1)
  
}
names(pollen.Twarm) <- sites.pollen.Twarm$dataSetName

pollen.Twarm.long <- do.call(rbind.data.frame, pollen.Twarm)
