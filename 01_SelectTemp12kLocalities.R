###SELECT LOCALITIES FROM TEMP 12K DATABASE-------------------------------------
##Database in available at: https://www.ncei.noaa.gov/access/paleo-search/study/27330

tmp.env <- new.env()
load("Temp12k_v1_0_0.RData", envir=tmp.env)
D <- get("D", pos=tmp.env)
rm(tmp.env)

##Localities in Europe
sel <- vector("logical")
for(i in seq(1,length(D)))##select profiles in Europe
{
  lon <- D[[i]]$geo$longitude
  lat <- D[[i]]$geo$latitude
  
  if(lat > 32.5 & lat < 82.5 & lon > -32.5 & lon < 70)
  {
    sel[i] <- TRUE
  }
  else
  {
    sel[i] <- FALSE
  }
}

sel
sum(sel)

D.sel <- D[sel]

length(D.sel)

##Extract coordinates
lon <- vector("numeric")
lat <- vector("numeric")

for(i in seq(1,length(D.sel)))##extract coordinates of selected sites
{
  lon[i] <- D.sel[[i]]$geo$longitude
  lat[i] <- D.sel[[i]]$geo$latitude
}

##Extract archive type
sel <- vector("character")

for(i in seq(1,length(D.sel)))###extract archive type
{
  sel[i] <- D.sel[[i]]$archiveType
}

unique(sel)

##remove marine sediments
D.sel <- D.sel[sel != "MarineSediment"]

##Extract coordinates for selected sites
lon <- vector("numeric")
lat <- vector("numeric")

for(i in seq(1,length(D.sel)))
{
  lon[i] <- D.sel[[i]]$geo$longitude
  lat[i] <- D.sel[[i]]$geo$latitude
}

length(D.sel)
names(D.sel)

coords.sel <- as.data.frame(cbind(names(D.sel), lon, lat))
colnames(coords.sel)[1] <- "Name"

write.table(coords.sel, file="temp12k.selection.txt", sep="\t", dec=".", row.names = F)