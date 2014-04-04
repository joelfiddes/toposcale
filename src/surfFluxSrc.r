#surface flux functions source file
#13/6/2012


#############################################################################################
#				prepare IMIS data
#############################################################################################

#============================================
#make long/lat shape file of all IMIS points
#============================================
makePointShape<-function(data){
library(raster)
library(rgdal)
source("~/data/imis/src/imis_functions.r")
imisMeta<-get.terrainInfo(file=data)
imisLoc<-data.frame(imisMeta$lon, imisMeta$lat)
spoints<-SpatialPoints(imisLoc, proj4string= CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
return(spoints)
}


#============================================
#make long/lat shape file for generic points
#============================================
#options project=longlat, ch1903+, ch1903
makePointShapeGeneric<-function(lon,lat,data,proj='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'){
library(raster)
library(rgdal)
loc<-data.frame(lon, lat)

#p1 ='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

#p2 ='+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs' 

#p3 ='+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs'

#if (project=='longlat'){projection<-p1}
#if (project=='ch1903+'){projection<-p2}
#if (project=='ch1903'){projection<-p3}

spoints<-SpatialPointsDataFrame(loc,as.data.frame(data), proj4string= CRS(proj))
return(spoints)
}


#===================================================
#returns dataframe of imis ID and gridbox membership
#===================================================
getPointGridbox<-function(gridbox_raster='~/experiments/bigsim/sim/_master/extent_sml.asc', shp){
library(raster)
library(rgdal)
if(unlist(class(gridbox_raster))[1]=='RasterLayer'){gridBox<-gridbox_raster}else{gridBox<-raster(gridbox_raster)}
#can now give function raster directly or path to raster
ID<-extract(gridBox, shp)
stationID<-seq(1,length(ID),1)
pointGridboxID<-data.frame(stationID, ID)
return(pointGridboxID)
}

#=====================================================
#get valid imis data (with data ie remove wind station)
#returns dataframe of 
#-station ids
#-number datapoints
#-percent complete timeseries
#=====================================================
imisWithData<-function(var, file, startDate, endDate, origin, NAval=9.97e+36){
source("~/data/imis/src/imis_functions.r")
nc<-open.ncdf(file)
#set.missval.ncdf( nc=nc, varid=var, missval=-9999 ) 
stations = length(get.var.ncdf( nc, "ID") )
#read in all station data
snow=get.met.data  (stationNr=1,varName=var,startTime=startDate, endTime=endDate,fileName=file, origin=origin)
snow$data[snow$data>10e15]<-NA

snodfAll=data.frame(snow$data)
for (stationNr in 1:stations){
		print(stationNr)
snow=get.met.data (stationNr=stationNr,varName=var,startTime=startDate, endTime=endDate,fileName=file, origin=origin)
snow$data[snow$data>10e15]<-NA
snodfAll=data.frame(snodfAll,snow$data)
}
snodfAll[2:(stations+1)]->snodfAll
names(snodfAll)<-seq(1,stations,1)

#rm dataless stations
snodfSub=data.frame(snow$data)
for (i in 1:stations){
#if(colMeans(na.omit(snodfAll[i]),na.rm=T)<1000000 & is.na(mean((snodfAll[i]),na.rm=T))==F)
if( is.na(mean((snodfAll[i]),na.rm=T))==F)
{print(i);snodfSub<-data.frame(snodfSub,snodfAll[i])}
}

if (length(snodfSub)>1){
snodfSub[2:length(snodfSub)]->snodfSub

names(snodfSub)->stationNames
strsplit(stationNames, 'X')->n
as.numeric(unlist(n,2))->x

x[is.na(x)==F]->station_vec # removes "" strings from ctor

qual_vec=c()
for ( i in 1:length(station_vec)){
a<-length(snodfSub[,i][is.na(snodfSub[,i])==F])
qual_vec=c(qual_vec, a)
}

lengthAllData<-length(snodfSub[,1])
qual_percent<-qual_vec/lengthAllData

station_df<-data.frame(station_vec,qual_vec, qual_percent)

}else{station_df=NULL}
return(station_df)
}

#====================================================================
#remove stations with data timeseries less than GT [%] complete
#====================================================================
dataQuality<- function(station_df,QT=0.6 ){
validPoints<-station_df[1][station_df[3]> QT]
return(validPoints)
}


#=================================================================
#get gridbox ID of validPoints
#=================================================================
getValidPointGridbox<-function(validPoints, id_Df){
gridboxtag<- id_Df[,2][id_Df[,1] %in% validPoints]
pointsGridboxID<-data.frame(validPoints, gridboxtag)
return(pointsGridboxID)
}

#############################################################################################
#				gridbox
#############################################################################################

#==========================================
#get mean ele of gridbox_raster
#==========================================

getGridEle<-function(demPath){
require(raster)
dem<-raster(paste(demPath, '/ele.asc', sep=''))
boxMeanEle<-cellStats(dem, mean)
boxMeanEle<-round(boxMeanEle,0)
return(boxMeanEle)
}

#=======================================================
#get ele diff between grid and validation data station
#========================================================
caLcEleDiff<-function(station, nbox, file="/home/joel/data/imis/data/imis_data.nc"){

imisMeta<-get.terrainInfo(file)
eleStat<-imisMeta$ele[station]

simDir<-paste('~/experiments/bigsim/sim/',formatC(nbox,width=6,flag="0"), sep='')
eleGrid<-getGridEle(demPath=simDir)
eleDiff=eleGrid-eleStat

return(eleDiff)
}

#============================================================================================
#		AIR TEMPERATURE PRESSURE LEVEL INTERPOLATE
#=============================================================================================

#24/5/2012
#DESCRIPTION:
#using ERA pressure level data, interpolates temperature profile to provide AirT timeseries for any given point in ERA grid cell domain
#IN: 
# 1. ERA air temperature pressure level data (K)
# 2. ERA geopotential file (can be the same file) [m^2 s^-2]
# 3. height (m asl) of interpolation point
# 4. ERA grid box
# returns tair at stationHeight (a) interpolated to 3hr step or (b) aggregated to daily
# NEED TO MAKE ACCEPT NPRESSURE LEVELS ARGUMENT 
eraAirTPreprocess<-function(nbox,map=NULL,var,lm, k1=1, k2=1460, stationHeight, Tfile,Gfile ,origin, interpolate,daily){

source("~/data/imis/src/imis_functions.r")
source("~/experiments/bigsim/src/valTools.r")
source("~/experiments/bigsim/src/getGridMeta.r")

if(is.null(map)==T){
#setup for my weird 15 box grid (change to old automated code for sequence when update grid)
xgrid_vec<-c(2,3,4,5,6,2,3,4,5,6,2,3,4,5,6)
ygrid_vec<-c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
}else{
xgrid_vec<-map$xlab
ygrid_vec<-map$ylab
}

pl<-open.ncdf(Tfile)
pl2<-open.ncdf(Gfile)

z= get.var.ncdf( pl2, "z") #[lon, lat, pl, time]
t= get.var.ncdf( pl,var) 
time = get.var.ncdf( pl, "time")#get times hr since "1900-01-01 00:00:00"
long = get.var.ncdf( pl, "longitude")
lat = get.var.ncdf( pl, "latitude")
### suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
### (the origin used by SAS)
q <- time*60*60 #make seconds
## ways to convert this
date<-as.POSIXct(q, origin=origin)                # local

day=as.POSIXct(strptime(format(date,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# for aggegation

gph<-z/9.80665 # geopotential height = geopotential/9.80665 [gravity at sea level]


i<-xgrid_vec[nbox] # long cell
j<-ygrid_vec[nbox]# lat cell


gph_l1<-gph[i,j,1,k1:k2] #5500m/500mb
gph_l2<-gph[i,j,2,k1:k2] #3500/650
gph_l3<-gph[i,j,3,k1:k2] #2200/775
gph_l4<-gph[i,j,4,k1:k2] #1500/850
gph_l5<-gph[i,j,5,k1:k2] #800/925
gph_l6<-gph[i,j,6,k1:k2] #200/1000

gph_profile<-data.frame(gph_l6,gph_l5, gph_l4,gph_l3, gph_l2, gph_l1)

t_l1<-t[i,j,1,k1:k2] 
t_l2<-t[i,j,2,k1:k2] 
t_l3<-t[i,j,3,k1:k2] 
t_l4<-t[i,j,4,k1:k2] 
t_l5<-t[i,j,5,k1:k2] 
t_l6<-t[i,j,6,k1:k2] 

t_profile<-data.frame(t_l6,t_l5, t_l4,t_l3, t_l2, t_l1)

#tInterp<-function(n){

t_vec=c()
for(i in 1:(k2-k1+1)){

if(lm==FALSE){t<-approx( gph_profile[i,],t_profile[i,], xout=stationHeight)[2]; t<-t$y}

if(lm==TRUE){
a<-lm(as.numeric(t_profile[i,])~as.numeric(gph_profile[i,]))
c<-coefficients(a)
intercept<-c[1]
gradient<- c[2]
t=gradient*stationHeight+intercept
}

t_deg=t-273.15 # convert to degrees
t_vec=c(t_vec, t_deg)
#return(t_deg)}
}
t_vec<-as.numeric(t_vec)

if (daily==T){t_vec<-aggregate.ts(t_vec, nfrequency=0.25, FUN=mean)}

if(interpolate==T){
#interpolateto 3hrly

tpoint<-1:length(t_vec)
inbetweenpoint<-seq(1.5,length(tpoint-1),1)
interp<-approx(tpoint,t_vec,xout=inbetweenpoint )
tempTS<-interp$y
point<-c(interp$x, tpoint)
data<-c(t_vec, tempTS)
df<-data.frame(point, data)

ndx<-order(df$point, decreasing=F)
dat_sorted=df[ndx,]
lastvalue=dat_sorted$data[length(dat_sorted$data)]
t_vec=c(dat_sorted$data, lastvalue)
}
return(t_vec)


}

#############################################################################################
#		calc temperature pressure level interpolation
#############################################################################################

#=============================================
# get surface temperate 
getSurfaceDataAirT<-function(file, aggregate){

#read in era surface temp data
met<-read.table(file, sep=',', header=T)
surfT<-met$AirT

#surfDatDaily<-aggregate.ts(surfT, nfrequency = 1/8, FUN='mean')

#aggreate to daily
if(aggregate==T){
day=as.POSIXct(strptime(format(met$Date,format="%d/%m/%Y %H"),format="%d/%m/%Y"),tz="UTC")
surfDatDaily<-aggregate(surfT,by=list(day),FUN=mean,na.rm=TRUE)
surfDatDaily<-surfDatDaily$x}else{surfDatDaily<-surfT}

return(surfDatDaily)
}

#get surface precipitation
getSurfaceDataPre<-function(file, aggregate){

#read in era surface temp data
met<-read.table(file, sep=',', header=T)
surfT<-met$Iprec

#surfDatDaily<-aggregate.ts(surfT, nfrequency = 1/8, FUN='mean')

#aggreate to daily
if(aggregate==T){
day=as.POSIXct(strptime(format(met$Date,format="%d/%m/%Y %H"),format="%d/%m/%Y"),tz="UTC")
surfDatDaily<-aggregate(surfT,by=list(day),FUN=mean,na.rm=TRUE)
surfDatDaily<-surfDatDaily$x}else{surfDatDaily<-surfT}

return(surfDatDaily)
}

#############################################################################################
#		get validation data tair
#############################################################################################


#get daily mean or 3hr validtion data
getValDataDaily<-function(imisVar,startDate ,endDate,station,file,daily,fun){

imisDat=get.ncdf.data  (stationNr=station,varName=imisVar,startTime=startDate, endTime=endDate,fileName=file)

#imisDat<-aggregate.ts(imisDat$data, nfrequency=(1/6), FUN=mean)
if(daily==T){
day=as.POSIXct(strptime(format(imisDat$time,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")
imisDat<-aggregate(imisDat$dat,by=list(day),FUN=fun,na.rm=TRUE)
imisDat=imisDat$x
}
return(imisDat)

}

#============================================================================================
#		AIR TEMPERATURE TRAD LAPSE RATE
#=============================================================================================


eraAirTSurfPreprocess<-function(nbox,map=NULL, k1=1, k2=1460,step, stationHeight,boxHeight ,lapse,Tsfile ,origin, interpolate,daily){
#uses pressure level data for simplicity (i dataset needed) but extrapolates from 
source("~/data/imis/src/imis_functions.r")
source("~/experiments/bigsim/src/valTools.r")
source("~/experiments/bigsim/src/getGridMeta.r")

if(is.null(map)==T){
#setup for my weird 15 box grid (change to old automated code for sequence when update grid)
xgrid_vec<-c(2,3,4,5,6,2,3,4,5,6,2,3,4,5,6)
ygrid_vec<-c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
}else{
xgrid_vec<-map$xlab
ygrid_vec<-map$ylab
}

pl<-open.ncdf(Tsfile)



t= get.var.ncdf( pl, "t2m") 
time = get.var.ncdf( pl, "time")#get times hr since "1900-01-01 00:00:00"
long = get.var.ncdf( pl, "longitude")
lat = get.var.ncdf( pl, "latitude")
### suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
### (the origin used by SAS)
q <- time*60*60 #make seconds
## ways to convert this
date<-as.POSIXct(q, origin=origin)                # local

day=as.POSIXct(strptime(format(date,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# for aggegation


i<-xgrid_vec[nbox] # long cell
j<-ygrid_vec[nbox]# lat cell




t1<-t[i,j,k1:k2] 

eleDiff=boxHeight-stationHeight
t_vec=t1+eleDiff*lapse
t_deg=t_vec-273.15 # convert to degrees
t_vec<- t_deg


t_vec<-as.numeric(t_vec)
if(step==6){nfrequency=0.25}
if(step==3){nfrequency=0.125}
if (daily==T){t_vec<-aggregate.ts(t_vec, nfrequency=nfrequency, FUN=mean)}

if(interpolate==T){
#interpolateto 3hrly

tpoint<-1:length(t_vec)
inbetweenpoint<-seq(1.5,length(tpoint-1),1)
interp<-approx(tpoint,t_vec,xout=inbetweenpoint )
tempTS<-interp$y
point<-c(interp$x, tpoint)
data<-c(t_vec, tempTS)
df<-data.frame(point, data)

ndx<-order(df$point, decreasing=F)
dat_sorted=df[ndx,]
lastvalue=dat_sorted$data[length(dat_sorted$data)]
t_vec=c(dat_sorted$data, lastvalue)
}
return(t_vec)


}


#============================================================================================
#		RELATIVE  HUMIDITY PRESSURE LEVEL INTERPOLATE
#=============================================================================================

#24/5/2012
#DESCRIPTION:
#using ERA pressure level data, interpolates temperature profile to provide AirT timeseries for any given point in ERA grid cell domain
#IN: 
# 1. ERA air temperature pressure level data (K)
# 2. ERA geopotential file (can be the same file) [m^2 s^-2]
# 3. height (m asl) of interpolation point
# 4. ERA grid box
# returns tair at stationHeight (a) interpolated to 3hr step or (b) aggregated to daily
# NEED TO MAKE ACCEPT NPRESSURE LEVELS ARGUMENT 
eraRhPreprocess<-function(nbox,map=NULL,var,lm, k1=1, k2=1460, stationHeight, Tfile,Gfile ,origin, interpolate,daily){

source("~/data/imis/src/imis_functions.r")
source("~/experiments/bigsim/src/valTools.r")
source("~/experiments/bigsim/src/getGridMeta.r")

if(is.null(map)==T){
#setup for my weird 15 box grid (change to old automated code for sequence when update grid)
xgrid_vec<-c(2,3,4,5,6,2,3,4,5,6,2,3,4,5,6)
ygrid_vec<-c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
}else{
xgrid_vec<-map$xlab
ygrid_vec<-map$ylab
}

pl<-open.ncdf(Tfile)
pl2<-open.ncdf(Gfile)

z= get.var.ncdf( pl2, "z") #[lon, lat, pl, time]
t= get.var.ncdf( pl,var) 
time = get.var.ncdf( pl, "time")#get times hr since "1900-01-01 00:00:00"
long = get.var.ncdf( pl, "longitude")
lat = get.var.ncdf( pl, "latitude")
### suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
### (the origin used by SAS)
q <- time*60*60 #make seconds
## ways to convert this
date<-as.POSIXct(q, origin=origin)                # local

day=as.POSIXct(strptime(format(date,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# for aggegation

gph<-z/9.80665 # geopotential height = geopotential/9.80665 [gravity at sea level]


i<-xgrid_vec[nbox] # long cell
j<-ygrid_vec[nbox]# lat cell


gph_l1<-gph[i,j,1,k1:k2] #5500m/500mb
gph_l2<-gph[i,j,2,k1:k2] #3500/650
gph_l3<-gph[i,j,3,k1:k2] #2200/775
gph_l4<-gph[i,j,4,k1:k2] #1500/850
gph_l5<-gph[i,j,5,k1:k2] #800/925
gph_l6<-gph[i,j,6,k1:k2] #200/1000

gph_profile<-data.frame(gph_l6,gph_l5, gph_l4,gph_l3, gph_l2, gph_l1)

t_l1<-t[i,j,1,k1:k2] 
t_l2<-t[i,j,2,k1:k2] 
t_l3<-t[i,j,3,k1:k2] 
t_l4<-t[i,j,4,k1:k2] 
t_l5<-t[i,j,5,k1:k2] 
t_l6<-t[i,j,6,k1:k2] 

t_profile<-data.frame(t_l6,t_l5, t_l4,t_l3, t_l2, t_l1)

#tInterp<-function(n){

t_vec=c()
for(i in 1:(k2-k1+1)){

if(lm==FALSE){t<-approx( gph_profile[i,],t_profile[i,], xout=stationHeight)[2]; t<-t$y}

if(lm==TRUE){
a<-lm(as.numeric(t_profile[i,])~as.numeric(gph_profile[i,]))
c<-coefficients(a)
intercept<-c[1]
gradient<- c[2]
t=gradient*stationHeight+intercept
}

t_deg=t 
t_vec=c(t_vec, t_deg)
#return(t_deg)}
}
t_vec<-as.numeric(t_vec)

if (daily==T){t_vec<-aggregate.ts(t_vec, nfrequency=0.25, FUN=mean)}

if(interpolate==T){
#interpolateto 3hrly

tpoint<-1:length(t_vec)
inbetweenpoint<-seq(1.5,length(tpoint-1),1)
interp<-approx(tpoint,t_vec,xout=inbetweenpoint )
tempTS<-interp$y
point<-c(interp$x, tpoint)
data<-c(t_vec, tempTS)
df<-data.frame(point, data)

ndx<-order(df$point, decreasing=F)
dat_sorted=df[ndx,]
lastvalue=dat_sorted$data[length(dat_sorted$data)]
t_vec=c(dat_sorted$data, lastvalue)
}
return(t_vec)


}

#============================================================================================
#		RELATIVE  HUMIDITY TRAD METHOD [LISTON ELDER]
#=============================================================================================


eraRhSurfPreprocess<-function(nbox,map=NULL, var,isncdf, k1=1, k2=1460,step, stationHeight,boxHeight ,lapse,Tsfile ,origin, interpolate,daily){
#uses pressure level data for simplicity (i dataset needed) but extrapolates from 
source("~/data/imis/src/imis_functions.r")
source("~/experiments/bigsim/src/valTools.r")
source("~/experiments/bigsim/src/getGridMeta.r")

if(is.null(map)==T){
#setup for my weird 15 box grid (change to old automated code for sequence when update grid)
xgrid_vec<-c(2,3,4,5,6,2,3,4,5,6,2,3,4,5,6)
ygrid_vec<-c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
}else{
xgrid_vec<-map$xlab
ygrid_vec<-map$ylab
}

pl<-open.ncdf(Tsfile)

#parameters
a=611.21
b=17.502
c=240.97

#if(isncdf==T){
t= get.var.ncdf( pl, var) #}else{t=Tsfile}
time = get.var.ncdf( pl, "time")#get times hr since "1900-01-01 00:00:00"
long = get.var.ncdf( pl, "longitude")
lat = get.var.ncdf( pl, "latitude")
### suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
### (the origin used by SAS)
q <- time*60*60 #make seconds
## ways to convert this
date<-as.POSIXct(q, origin=origin)                # local

day=as.POSIXct(strptime(format(date,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# for aggegation


i<-xgrid_vec[nbox] # long cell
j<-ygrid_vec[nbox]# lat cell




t1<-t[i,j,k1:k2] 
t1=t1-273.15
eleDiff=boxHeight-stationHeight
lapse1=lapse*(c/b)
t_vec=t1+(eleDiff*lapse1)

t_vec<-as.numeric(t_vec)



if(step==6){nfrequency=0.25}
if(step==3){nfrequency=0.125}
if (daily==T){t_vec<-aggregate.ts(t_vec, nfrequency=nfrequency, FUN=mean)}

if(interpolate==T){
#interpolateto 3hrly

tpoint<-1:length(t_vec)
inbetweenpoint<-seq(1.5,length(tpoint-1),1)
interp<-approx(tpoint,t_vec,xout=inbetweenpoint )
tempTS<-interp$y
point<-c(interp$x, tpoint)
data<-c(t_vec, tempTS)
df<-data.frame(point, data)

ndx<-order(df$point, decreasing=F)
dat_sorted=df[ndx,]
lastvalue=dat_sorted$data[length(dat_sorted$data)]
t_vec=c(dat_sorted$data, lastvalue)
}
return(t_vec)


}





#============================================================================================
#		SWin radiation extrapolate
#=============================================================================================


eraSWin<-function(nbox,map=NULL,var, k1=1, k2=1460,step,Tsfile ,origin, interpolate,daily){
#uses pressure level data for simplicity (i dataset needed) but extrapolates from 
source("~/data/imis/src/imis_functions.r")
source("~/experiments/bigsim/src/valTools.r")
source("~/experiments/bigsim/src/getGridMeta.r")

if(is.null(map)==T){
#setup for my weird 15 box grid (change to old automated code for sequence when update grid)
xgrid_vec<-c(2,3,4,5,6,2,3,4,5,6,2,3,4,5,6)
ygrid_vec<-c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
}else{
xgrid_vec<-map$xlab
ygrid_vec<-map$ylab
}

pl<-open.ncdf(Tsfile)



t= get.var.ncdf( pl, var) 
time = get.var.ncdf( pl, "time")#get times hr since "1900-01-01 00:00:00"
long = get.var.ncdf( pl, "longitude")
lat = get.var.ncdf( pl, "latitude")
### suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
### (the origin used by SAS)
q <- time*60*60 #make seconds
## ways to convert this
date<-as.POSIXct(q, origin=origin)                # local

day=as.POSIXct(strptime(format(date,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# for aggegation


i<-xgrid_vec[nbox] # long cell
j<-ygrid_vec[nbox]# lat cell




t1<-t[i,j,k1:k2] 


t_vec<- t1


t_vec<-as.numeric(t_vec)
if(step==6){nfrequency=0.25}
if(step==3){nfrequency=0.125}
if (daily==T){t_vec<-aggregate.ts(t_vec, nfrequency=nfrequency, FUN=mean)}

if(interpolate==T){
#interpolateto 3hrly

tpoint<-1:length(t_vec)
inbetweenpoint<-seq(1.5,length(tpoint-1),1)
interp<-approx(tpoint,t_vec,xout=inbetweenpoint )
tempTS<-interp$y
point<-c(interp$x, tpoint)
data<-c( tempTS,t_vec)
df<-data.frame(point, data)

ndx<-order(df$point, decreasing=F)
dat_sorted=df[ndx,]
#lastvalue=dat_sorted$data[length(dat_sorted$data)]
#t_vec3=c(dat_sorted$data, lastvalue)
t_vec3=dat_sorted$data
}
return(t_vec2)


}

#============================================================================================
#		relative humidity function
#=============================================================================================

relHumCalc=function(tair,tdew){
#constants
es0=6.11 #reference saturation vapor pressure (es at a certain temp, usually 0 deg C)
T0=273.15 #(273.15 Kelvin,  Kelvin = degree C +273.15)
lv=2.5E6 #latent heat of vaporization of water (2.5 * 10^6 joules per kilogram)
Rv=461.5 #gas constant for water vapor (461.5 joules* Kelvin / kilogram)

#variables
#tdew=dewpoint temp (kelvin)
#tair=air temperature (kelvin)

e  = es0 * exp( lv/Rv * (1/T0 - 1/tdew))
es = es0 * exp( lv/Rv * (1/T0 - 1/tair))

RH = e/es * (100) 

return(RH)
}

