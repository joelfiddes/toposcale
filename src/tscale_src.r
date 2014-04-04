#GENERAL FUNCTIONS

###############################################################################################################
#					FUNCTION: timeDaily						      #
###############################################################################################################

#==============================================Description=====================================================
#PURPOSE = aggregates time variable to daily values, used as input to aggregate data
#IN = ncdf file
#OUT = daily ts of time variable
#DEPENDS ON = none
#==============================================Parameters======================================================
#SETUP PARAMETERS:

##ERA interim
#fileName='/home/joel/data/era/pressureLevels/tair/outfile.nc'
#origin=1900
#step='seconds'
#timeVar='time'

##METEOSWISS
#fileName='~/data/meteoswiss/meteoch.nc'
#origin=1970
#step='hours'
#timeVar='time'

##IMIS
#fileName="~/data/imis/data/imis_data.nc"
#origin=1970
#step='hours'
#timeVar='time'

#ASRB

#=============================================Function========================================================
timeDaily<-function(fileName='/home/joel/data/era/pressureLevels/tair/outfile.nc', origin=1900, step='seconds', timeVar='time'){
require(ncdf)
nc<-open.ncdf(fileName)
z = get.var.ncdf( nc, timeVar)
if(step=='seconds'){z <- z*60*60} #make seconds
time<-ISOdatetime(origin,1,1,0,0,0) + z  
dayTS=as.POSIXct(strptime(format(time,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# tz correct?

return(dayTS)
}



#================================================================================================================
#					FUNCTION: dataDaily						       
#================================================================================================================

#==============================================Description=====================================================
#PURPOSE = aggregates data to daily timeseries
#IN = dayTS 
#OUT = daily ts of data
#DEPENDS ON = timeDaily
#==============================================Parameters======================================================
#SETUP PARAMETERS:

##ERA interim
#fileName='/home/joel/data/era/pressureLevels/tair/outfile.nc'
##METEOSWISS
#fileName='~/data/meteoswiss/meteoch.nc'
##IMIS
#fileName="~/data/imis/data/imis_data.nc"
#ASRB

#GENERAL
#dat= vector variable timeseries to be aggregated 
#dayTS= daily time, timeseries from timeDaily
#FUN= mean(default)/max/min
#=============================================Function========================================================

#=============================================================================================================

################################################################################################################
#					FUNCTION: dataDailyApply					       #
################################################################################################################

#==============================================Description=====================================================
#PURPOSE = aggregates data in multidimensional form to daily timeseries in multidim form
#IN = dayTS 
#OUT = daily ts of data
#DEPENDS ON = dataDaily
#==============================================Parameters======================================================
#SETUP PARAMETERS:

##ERA interim
#fileName='/home/joel/data/era/pressureLevels/tair/outfile.nc'
##METEOSWISS
#fileName='~/data/meteoswiss/meteoch.nc'
##IMIS
#fileName="~/data/imis/data/imis_data.nc"
#ASRB

#GENERAL
#dat= multidim variable timeseries to be aggregated
#MARGIN = dimensions to remain constant (ie not aggregated)
#dayTS= daily time, timeseries from timeDaily
#FUN= dataDaily

##EXAMPPLE

#str(dat)
##num [1:7, 1:4, 1:6, 1:20456] 

##we aggregate dim 4, time, to daily values

##so
#z=apply(X=dat, MARGIN=c(1,2,3), FUN=dataDaily, dayTS=dayTS)

##gives a very strange data structure lis of z matrix of y cols and x rows:
##z[row,col,matrix]
##z[1:7, 1:4, 1:6]

##experiment to check dimensions transposed correctly ie dat=z
#col=7
#row=1
#pl=3
#dat1=dat[col,row,pl,]
#dat1agg=dataDaily(dat=dat1, dayTS=dayTS)
#str(dat1agg)
##'data.frame':	5114 obs. of  2 variables:
## $ Group.1: POSIXct, format: "1996-01-01" "1996-01-02" ...
## $ x      : num  250 250 248 251 251 ...
#c=z[col,row,pl] # col, row, pl
#v=as.data.frame(c)
#str(v)
##'data.frame':	5114 obs. of  2 variables:
## $ Group.1: POSIXct, format: "1996-01-01" "1996-01-02" ...
## $ x      : num  250 250 248 251 251 ...
#cor(dat1agg$x, v$x)
##[1] 1

#=============================================Function========================================================

dataDailyApply=function(X=dat, MARGIN=c(1,2,3), ts, fun){
z=apply(X=X, MARGIN=MARGIN, FUN=dataDaily, dayTS=ts, fn=fun)
return(z)
}

################################################################################################################
#					FUNCTION: dataDailyExtract					       #
################################################################################################################

#==============================================Description=====================================================
#PURPOSE = extracts single time series from z
#IN = z, multidim aggregated series
#OUT = single dim aggregated timeseries (not ts object, is dataframe)
#DEPENDS ON = dataDaily
#==============================================Parameters======================================================
#z=multidim aggregated series
#dims= vector corresponding to dims to select
#=============================================Function========================================================
dataDailyExtract=function(z, dims=c(1,1,1)){
c=z[1,1,1] # col, row, pl
dat=as.data.frame(c)
return(dat)
}



################################################################################################################
#					FUNCTION: dataCut						       #
################################################################################################################

#==============================================Description=====================================================
#PURPOSE = cuts data to given start end dates
#IN = (1) dates vector, (2) data vector
#OUT = cut dates/data dataframe
#DEPENDS ON = NULL
#==============================================Parameters======================================================

#dates=dat$Group.1 # dates vector, any class?
#data=dat$x # data vector
#beg="2009-06-17" # start cut in same interval as dates vector, day, hr, min etc
#end='2009-12-15' #end cut 
#=============================================Function========================================================
dataCut<-function(dates=dat$Group.1, data=dat$x, beg="2009-06-17", end="2009-12-15"){
nbeg=which(as.Date(dates)==beg)
nend=which(as.Date(dates)==end)

datCut=data[nbeg:nend]
dateCut=dates[nbeg:nend]

df=data.frame(dateCut, datCut)
return(df)
}

##############################################################################################################
#       					FUNCTION:getEraEle	NOT FINISHED!!!				     #				 	
##############################################################################################################

#============================================Description======================================================
#PURPOSE = vector of ERA rid ele based on geopotential file
#IN = geop.nc
#OUT = 
#DEPENDS ON = 
#=============================================Parameters======================================================
#SETUP PARAMETERS:

#=============================================Function========================================================
#geop='/home/joel/data/era/surface/geop.nc'
#const=9.80665
#require(ncdf)
#nc<-open.ncdf(geop)
#z = get.var.ncdf( nc, timeVar)
#=============================================================================================================

###############################################################################################################
#					FUNCTION: 	d2toRhum						#
###############################################################################################################

#============================================Description======================================================
#PURPOSE = convert ERA interim surface dewpoint temp to relative humidity
#IN = d2m and t2m
#OUT = rhum
#DEPENDS ON = 
#=============================================Parameters======================================================
#SETUP PARAMETERS:

#=============================================Function========================================================
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
#=============================================================================================================


###############################################################################################################
#					FUNCTION: 	getGridBoxID						#
###############################################################################################################
getGridBoxID	<- function(eraFile='/home/joel/data/era/surface/grid74/tair.nc', metaFile='/home/joel/data/transformedData/MET_meta.txt' ){
#fileEra='/home/joel/data/era/surface/grid74/tair.nc'
#metaFile='/home/joel/data/transformedData/ASRB_meta.txt' 
mf=read.table(metaFile, sep=',', header =T)
shp=makePointShapeGeneric(lon=mf$lon, lat=mf$lat, data=mf$id)


#make gridbox IDS raster
x=raster(eraFile)
gridBoxRst=init(x, v='cell')
eraCells=cellStats(gridBoxRst,max)

#get station box membership
boxID=getPointGridbox(gridbox_raster=gridBoxRst, shp=shp)

#concat and writetofile
boxID=boxID$ID
mf=cbind(mf, boxID)
write.table(mf, metaFile, sep=',', row.names=F)
}

###############################################################################################################
#					FUNCTION: 	constructASRB						#
###############################################################################################################
constructASRB	<- function(){
root='/home/joel/data/asrb/'
metaFile='/home/joel/data/transformedData/ASRB_meta.txt' 
mf=read.table(metaFile, sep=',', header =T)
names=mf$name
val_vec=c()
for (i in names){
dat=read.table(paste(root, i, '_hour.csv', sep=''), sep=',', header=T)
date=paste(dat$YEAR, dat$MONTH, dat$DAY, sep='/')
date=as.Date(date)
dat_day=aggregate(x=dat$LWin, by=list(date), FUN=mean)
datLWin=dat_day$x
val_vec=c(val_vec, datLWin)
}
}


################################################################################################################
#					ERA accumulated -> instant values
#only for lwin currently
################################################################################################################

accumToInstERA<-function(nbox, coordMap, file,origin, startDate, endDate, var, step){

#Tsfile='/home/joel/data/era/surface/tair/3hr/outfile.nc'
nc=open.ncdf(file)
z = get.var.ncdf( nc, "time")
origin=origin
z <- z*60*60 #make seconds
dates<-ISOdatetime(origin,0,0,0,0,0) + z 
startDate =startDate
endDate = endDate
nbeg=which(as.Date(dates, tz='CEST')==startDate)
nend=which(as.Date(dates)==endDate)
startData=nbeg[1]-1#era "1996-01-01 00:00:00"
endData=nend[length(nend)]#era  "2009-12-31 00:00:00"

xgrid_vec<-coordMap$xlab
ygrid_vec<-coordMap$ylab
i<-xgrid_vec[nbox] # long cell
j<-ygrid_vec[nbox]# lat cell

k1=startData
k2=endData

nc=open.ncdf(file)
strd = get.var.ncdf( nc, var)
strd.cell=strd[i, j, k1:k2]

step=step
dp_day<-(24/step) # datapoints per day

#=====================================accumulated values conversion=======================================


#======== subset data by step 3,6,9,12 and forecast 0 or 12:====================
tot_steps<-dp_day
#fld_names=data.frame(tp.cell, ssrd.cell, strd.cell)
fld_names<-c('strd.cell')

for (fld_name in fld_names){
#a<-fld_name

if(fld_name=='tp.cell'){fld<-'tp';a<-tp.cell}
if(fld_name=='ssrd.cell'){fld<-'ssrd';a<-ssrd.cell}
if(fld_name=='strd.cell'){fld<-'strd';a<-strd.cell}


#steps(3,6,9,12 from fc_step 0)
for (n in 1:(tot_steps/2)){
stepID=n*step
fc_start=0
assign(paste(fld,'_s', stepID,'_', fc_start, sep=''), a[seq(n, length(a), 8)])
}

#steps(3,6,9,12 from fc_step 12)
for (n in ((tot_steps/2)+1):(tot_steps)){
stepID=(n-4)*3
fc_start=12
assign(paste(fld,'_s', stepID,'_', fc_start, sep=''), a[seq(n, length(a), 8)])
}

}

#strd_s12_12=c(strd_s12_12,strd[length(strd_s12_12)]) duplicate last value as missing
#strd_s9_12=c(strd_s9_12,strd[length(strd_s9_12)]) 
#strd_s6_12=c(strd_s6_12,strd[length(strd_s6_12)]) 

strd_df<-data.frame(strd_s3_0 ,strd_s6_0 ,strd_s9_0,strd_s12_0,strd_s3_12,strd_s6_12,strd_s9_12,strd_s12_12)
#==========================================================



#=====================================accumulated values conversion=======================================

#basic formula 




#convert radiation from accumulation W m-2 s to to W m-2

step_length=step #(length of timestep in hr)
step_time=step_length*3600 #step time in s

#calculate strd instant values

#var name
var_df<-strd_df


inst_val_1<-var_df[,1]/step_time# first from forecast start
for(n in 2:(tot_steps/2)){assign(paste('inst_val_',n,sep=''),((var_df[,n]-var_df[,n-1])/step_time))}

inst_val_5<-var_df[,5]/step_time# first from forecast start
for(n in 6:tot_steps){assign(paste('inst_val_',n,sep=''),((var_df[,n]-var_df[,n-1])/step_time))}

strd_inst_val<-as.vector(rbind(inst_val_1, inst_val_2, inst_val_3, inst_val_4, inst_val_5, inst_val_6, inst_val_7, inst_val_8))
return(strd_inst_val)
}


################################################################################################################
#					extractVals
################################################################################################################

#shp
#dem100ll

#extractVals=function(shp,  dem){
#dem='/home/joel/data/dem_CH/aster30_ch1903.asc'
#shpFile='/home/joel/data/asrb/asrb_loc [Swiss. Obl.shp'
#proj='+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs'
#rst=raster(dem)
#projection(rst)<-proj
#shp=shapefile(shpFile)
#slp=terrain(rst, opt='slope',unit='degrees')
#asp=terrain(rst, opt='aspect', unit='degrees')

#slpData=extract(slp, shp)
#return(slpData)
#}


#extractVals=function(shp,  dem){
#dem='/home/joel/data/dem_CH/aster30_ch1903.asc'
#shpFile='/home/joel/data/asrb/asrb_loc [Swiss. Obl.shp'
#proj='+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs'
#rst=raster(dem)
#projection(rst)<-proj
#shp=shapefile(shpFile)

#asp=terrain(rst, opt='aspect', unit='degrees')

#extract(asp, shp)
#return(asp)
#}



#}
################################################################################################################
#					calc wind direction from vectors
################################################################################################################
#function calc wind speed [liston elder 2006]
#windDir<-function(u,v){
#wd=(((3*pi)/2)- atan (v/u))
#wd=wd*(180/pi)
##ws<-180/pi*s
#return(wd)
#}
#reduced range error 200--360 due to this liston elder 2006 formula.

windDir <- function(u, v) {
  (180 / pi) * atan(u/v) + ifelse(v>0,180,ifelse(u>0,360,0))
}
#source: http://stackoverflow.com/questions/8673137/calculating-wind-direction-from-u-and-v-components-of-the-wind-using-lapply-or-i

################################################################################################################
#					calc wind speed from vectors
################################################################################################################
#function calc wind speed [liston elder 2006]
windSpd<-function(u,v){

ws<-sqrt(u^2+v^2)
#ws<-180/pi*s
return(ws)
}



################################################################################################################
#					calc wind dir from vectors
################################################################################################################

#returns 6 hourly wind speed (m/s) for grid box
windDirGrid<-function(nbox, map,varu, varv,k1, k2 ,fileU, fileV,daily){

require('hydroGOF')
require('circular')
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

u<-open.ncdf(fileU)
uvec = get.var.ncdf( u, varu) 
v<-open.ncdf(fileV)
vvec = get.var.ncdf( v, varv) 



time = get.var.ncdf( u, "time")#get times hr since "1900-01-01 00:00:00"
long = get.var.ncdf( u, "lon")
lat = get.var.ncdf( u, "lat")
### suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
### (the origin used by SAS)
q <- time*60*60 #make seconds
## ways to convert this
date<-as.POSIXct(q, origin="1900-01-01")                # local
day=as.POSIXct(strptime(format(date,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# for aggegation
dayvec=day[k1:k2]
#function calc wind speed [liston elder 2006]
#windDir<-function(u,v){
#wd=(((3*pi)/2)- atan (v/u))
#wd=wd*(180/pi)
##ws<-180/pi*s
#return(wd)
#}

windDir <- function(u, v) {
  (180 / pi) * atan(u/v) + ifelse(v>0,180,ifelse(u>0,360,0))
}

t<-windDir(uvec, vvec)

i<-xgrid_vec[nbox] # long cell
j<-ygrid_vec[nbox]# lat cell

wd<-t[i,j,k1:k2] 

if (daily==T){
xc<-aggregate(cos(rad(wd)), by=list(dayvec), FUN=sum)
xs<-aggregate(sin(rad(wd)),by=list(dayvec), FUN=sum)
aspMean<-atan2(xs$x,xc$x)
#correct -ve values
aspM <- aspMean*(180/pi )
aspMean<-ifelse(aspM<0,aspM+360, aspM)# correct negative values
} # can take max or min but NOT mean!


return(aspMean)


}



################################################################################################################
#					calc wind speed from vectors
################################################################################################################

#returns 6 hourly wind speed (m/s) for grid box
windSpdGrid<-function(nbox, map,varu, varv,k1, k2 ,fileU, fileV, interpolate,nfreq,daily){

require('hydroGOF')

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

u<-open.ncdf(fileU)
uvec = get.var.ncdf( u, varu) 
v<-open.ncdf(fileV)
vvec = get.var.ncdf( v, varv) 



time = get.var.ncdf( u, "time")#get times hr since "1900-01-01 00:00:00"
long = get.var.ncdf( u, "lon")
lat = get.var.ncdf( u, "lat")
### suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
### (the origin used by SAS)
q <- time*60*60 #make seconds
## ways to convert this
date<-as.POSIXct(q, origin="1996-01-01")                # local
day=as.POSIXct(strptime(format(date,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# for aggegation
dayvec=day[k1:k2]
#function calc wind speed [liston elder 2006]
windSpd<-function(u,v){

ws<-sqrt(u^2+v^2)
#ws<-180/pi*s
return(ws)
}


t<-windSpd(uvec, vvec)

i<-xgrid_vec[nbox] # long cell
j<-ygrid_vec[nbox]# lat cell

ws<-t[i,j,k1:k2] 

if (daily==T){
t_vec<-aggregate(ws, by=list(dayvec), FUN=mean)
} # can take max or min but NOT mean!

return(t_vec$x)


}


################################################################################################################
#					Calculate Grid level wind speed and direction - needs to be 
################################################################################################################
#returns 6 hourly wind speed (m/s) for grid box
windSpdDirGrid<-function(out,nbox, map,varu, varv,k1, k2 ,fileU, fileV,daily){

require('hydroGOF')

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

#i and j
i<-xgrid_vec[nbox] # long cell
j<-ygrid_vec[nbox]# lat cell

u<-open.ncdf(fileU)
uvec = get.var.ncdf( u, varu) 
v<-open.ncdf(fileV)
vvec = get.var.ncdf( v, varv) 

#date/time
time = get.var.ncdf( u, "time")#get times hr since "1900-01-01 00:00:00"
long = get.var.ncdf( u, "lon")
lat = get.var.ncdf( u, "lat")
q <- time*60*60 #make seconds
date<-as.POSIXct(q, origin="1996-01-01")                # local
day1=as.POSIXct(strptime(format(date,format="%Y/%m/%d %H"),format="%Y/%m/%d"))# for aggegation
day=day1[k1:k2]

#extract box u and v
ubox<-uvec[i,j,k1:k2] 
vbox<-vvec[i,j,k1:k2] 

if (daily==T){
ubox_agg<-aggregate(x=ubox, by=list(day), FUN=mean)
vbox_agg<-aggregate(vbox, by=list(day), FUN=mean)
} 

if(out=='ws'){
#function calc wind speed [liston elder 2006]
windSpd<-function(u,v){
ws<-sqrt(u^2+v^2)
#ws<-180/pi*s
return(ws)
}
t<-windSpd(ubox_agg$x, vbox_agg$x)
}

if(out=='wd'){
windDir <- function(u, v) {
  (180 / pi) * atan(u/v) + ifelse(v>0,180,ifelse(u>0,360,0))
}
t<-windDir(ubox_agg$x, vbox_agg$x)
}

return(t)
}


################################################################################################################
#					ERA accumulated -> instant values ONLY
#accepts accumulated values vector and date vector.
################################################################################################################

accumToInstERA_simple<-function(inDat, step){

tot_steps<-(24/step) # datapoints per day

#======== subset data by step 3,6,9,12 and forecast 0 or 12:====================
fld<-'strd'

#steps(3,6,9,12 from fc_step 0)
for (n in 1:(tot_steps/2)){
stepID=n*step
fc_start=0
assign(paste(fld,'_s', stepID,'_', fc_start, sep=''), inDat[seq(n, length(inDat), 8)])
}

#steps(3,6,9,12 from fc_step 12)
for (n in ((tot_steps/2)+1):(tot_steps)){
stepID=(n-4)*3
fc_start=12
assign(paste(fld,'_s', stepID,'_', fc_start, sep=''), inDat[seq(n, length(inDat), 8)])
}

var_df<-data.frame(strd_s3_0 ,strd_s6_0 ,strd_s9_0,strd_s12_0,strd_s3_12,strd_s6_12,strd_s9_12,strd_s12_12)

#basic formula 
#( FLD( STEP2)- FLD( STEP1))/(( STEP2-STEP1)*3600).

step_time=step*3600 #step time in s

inst_val_1<-var_df[,1]/step_time# first from forecast start
for(n in 2:(tot_steps/2)){assign(paste('inst_val_',n,sep=''),((var_df[,n]-var_df[,n-1])/step_time))}

inst_val_5<-var_df[,5]/step_time# first from forecast start
for(n in 6:tot_steps){assign(paste('inst_val_',n,sep=''),((var_df[,n]-var_df[,n-1])/step_time))}

strd_inst_val<-as.vector(rbind(inst_val_1, inst_val_2, inst_val_3, inst_val_4, inst_val_5, inst_val_6, inst_val_7, inst_val_8))
return(strd_inst_val) #result is m/s for precip *1000*60*60 to make mm/hr
}




#=====================================================================================================================
#					INTERPOLATE 6-> 3hrs USINFG 'APPROX' TO FILL GAPS
#=====================================================================================================================
#accepts vector
#interpolates between existing datapoints ie returns vector of (2*original vector -1)
#WARNING: final 21UTC missing in some cases if 6h data  eg 01011996 00UTC -> 31122008 18UTC (should download +1 day in future to allow final 21UTC interpolation point)
interp6to3<-function(dat6){

tpoint<-1:length(dat6)
inbetweenpoint<-seq(1.5,length(tpoint-1),1)
interp<-approx(tpoint,dat6,xout=inbetweenpoint )
tempTS<-interp$y
point<-c(interp$x, tpoint)
data<-c( tempTS,dat6)
df<-data.frame(point, data)

ndx<-order(df$point, decreasing=F)
dat_sorted=df[ndx,]
#lastvalue=dat_sorted$data[length(dat_sorted$data)]
#t_vec3=c(dat_sorted$data, lastvalue)
dat3=dat_sorted$data
return(dat3)
}

#=====================================================================================================================
#					adjust accumulated timestep point USINFG 'APPROX' TO FILL GAPS
#=====================================================================================================================
#accepts vector
#accum vals are converted to step averages - but need to shift back to original timepoint.
#interpolates between existing datapoints ie returns vector of (original vector -1)
#shift 01.30UTC, 04.30UTC, 07.30UTC,... -> 3UTC, 6UTC, 9UTC....
#WARNING: LOSES LAST DATAPOINT DUE TO INTERPOLATION between points

adjAccum<-function(dat){

tpoint<-1:length(dat)
inbetweenpoint<-seq(1.5,length(tpoint-1),1)
interp<-approx(tpoint,dat,xout=inbetweenpoint )
point<-interp$y
return(point)
}

#=====================================================================================================================
#					RETRIEVE NAMEVEC FOR ORDER OF MODELLING STATIONS
#=====================================================================================================================



#make name vector
getNameVec<-function(mf1){
if(mf1=='imis'){metaFile='/home/joel/data/transformedData/IMIS_meta.txt'}
if(mf1=='anetz'){metaFile='/home/joel/data/transformedData/MET_meta.txt'}
if(mf1=='asrb'){metaFile='/home/joel/data/transformedData/ASRB_meta.txt'}
if(mf1=='asrb2'){metaFile='/home/joel/data/transformedData/ASRB_meta.txt'}
mf=read.table(metaFile, sep=',', header =T)
eraBoxEle=read.table('/home/joel/data/transformedData/eraBoxEle.txt', header=T, sep=',')
eraCells=28


nameVec=c()
nboxSeq=c(1:eraCells)
for (nbox in nboxSeq){

#get grid mean elev
gridEle<-eraBoxEle$masl[nbox]


#get valid stations in nbox

singleGridboxPoints_vec=mf$id[mf$boxID==nbox]
if (length(na.omit(singleGridboxPoints_vec))==0){next}
stations<-na.omit(singleGridboxPoints_vec)

#get station attributes
ls=mf[stations,]
ID=seq(1,length(ls$id),1)
lsp<-cbind(ID, ls)
names(lsp)[2]<-'stn'


for (i in 1:length(lsp$stn)){
stationElev=lsp$ele[i]
nameVec=c(nameVec,(lsp$stn[i]))
}}

if(mf1=='asrb'){
nameVec=c()
nboxSeq=c(1:eraCells)
for (nbox in nboxSeq){

#get grid mean elev
gridEle<-eraBoxEle$masl[nbox]


#get valid stations in nbox

singleGridboxPoints_vec=mf$id[mf$boxID==nbox]
if (length(na.omit(singleGridboxPoints_vec))==0){next}
stations<-na.omit(singleGridboxPoints_vec)

#get station attributes
ls=mf[stations,]
ID=seq(1,length(ls$id),1)
lsp<-cbind(ID, ls)
names(lsp)[2]<-'stn'


for (i in 1:length(lsp$stn)){
stationElev=lsp$ele[i]

nameVec=c(nameVec,as.character(lsp$name[i]))
}}}



return(nameVec)
}


#==============================================================================
#			separate snow/wind IMIS STATIONS (RETURNS NUMERIC ID)
#==============================================================================

imisWind=function(wind=T){
require(ncdf)
file="~/data/imis/data/imis_data.nc"
nc<-open.ncdf(file)
var='HSno'
dat = get.var.ncdf( nc, var)
threshold= 50000
dat[dat>threshold]<-NA

#select stations with mean Hsno ==NAN (ie wind stations)
datvec=c()
for(i in 1:161){
 x=mean(dat[i,],na.rm=T)
datvec=c(datvec,x)
}

windStation=which(is.na(datvec)==TRUE)
snowStation=which(is.na(datvec)==FALSE)

if(wind==T){return(windStation)}
if(wind==F){return(snowStation)}
}

#================================================================================================
#		WATER VAPOUR PRESSURE
#================================================================================================
Rh2Wvp=function(tair,RH){
#constants
es0=6.11 #reference saturation vapor pressure (es at a certain temp, usually 0 deg C)
T0=273.15 #(273.15 Kelvin,  Kelvin = degree C +273.15)
lv=2.5E6 #latent heat of vaporization of water (2.5 * 10^6 joules per kilogram)
Rv=461.5 #gas constant for water vapor (461.5 joules* Kelvin / kilogram)

#variables
#RH=relative humidity (%)
#tair=air temperature (kelvin)
es = es0 * exp( lv/Rv * (1/T0 - 1/tair))

e=(RH*es)/100

return(e)
}


#================================================================================================
#		EXTRACT GRID DATA BY POINTS
#================================================================================================

grid2points =function(grid='/home/joel/data/era/surface/grid74/tp.nc', points=shp){



}

################################################################################################################
#					Date stuff
################################################################################################################


#output of time daily

dayDates=function(fileName='/home/joel/data/era/pressureLevels/tair/outfile.nc', origin=1900, step='seconds', timeVar='time'){
require(ncdf)
nc<-open.ncdf(fileName)
z = get.var.ncdf( nc, timeVar)
if(step=='seconds'){z <- z*60*60} #make seconds
time<-ISOdatetime(origin,1,1,0,0,0) + z  
dayTS=as.POSIXct(strptime(format(time,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# tz correct?

c=as.factor(dayTS)
dates=levels(c)
d=as.Date(dates)
return(d)
}

#output months
monthDates=function(fileName='/home/joel/data/era/pressureLevels/tair/outfile.nc', origin=1900, step='seconds', timeVar='time'){
require(ncdf)
nc<-open.ncdf(fileName)
z = get.var.ncdf( nc, timeVar)
if(step=='seconds'){z <- z*60*60} #make seconds
time<-ISOdatetime(origin,1,1,0,0,0) + z  
dayTS=as.POSIXct(strptime(format(time,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# tz correct?

c=as.factor(dayTS)
dates=levels(c)
d=as.Date(dates)
mo <- strftime(d, "%m")
return(mo)
}

#output years
yearDates=function(fileName='/home/joel/data/era/pressureLevels/tair/outfile.nc', origin=1900, step='seconds', timeVar='time'){
require(ncdf)
nc<-open.ncdf(fileName)
z = get.var.ncdf( nc, timeVar)
if(step=='seconds'){z <- z*60*60} #make seconds
time<-ISOdatetime(origin,1,1,0,0,0) + z  
dayTS=as.POSIXct(strptime(format(time,format="%Y/%m/%d %H"),format="%Y/%m/%d"),tz="UTC")# tz correct?

c=as.factor(dayTS)
dates=levels(c)
d=as.Date(dates)
yr <- strftime(d, "%Y")
return(yr)
}

################################################################################################################
#					writeShape
################################################################################################################

writeShp=function(shp=valShp, out='/home/joel/data/shapefiles/asrb'){
writeOGR(shp,layer=out,dsn=paste(out, '.shp',sep=''), driver="ESRI Shapefile")
}

################################################################################################################
#					CALCULATE BIAS
################################################################################################################
bias=function(sim,obs){sum(sim-obs, na.rm=T)/length(obs)}


###############################################################################################################
#					COORD/GRIDBOX MAP
###############################################################################################################

#returns coordinates of gridboxes 1-n by row 

getCoordMap<-function(ncdomainfile){
nc=open.ncdf(ncdomainfile)
long = get.var.ncdf( nc, "lon")
lat = get.var.ncdf( nc, "lat")
x=length(long)
y=length(lat)

xlab=rep(1:x, y)
ylab=c()
for (i in 1:y){
yl=rep(i,x)
ylab=c(ylab, yl)
}
cells=seq(1, x*y)
coordMap=data.frame(cells, xlab, ylab)
return(coordMap)
}

#new surfFluxSrc

#============================================================================================
#		PRESSURE LEVEL INTERPOLATION
#=============================================================================================

#10/5/2013
#DESCRIPTION:
#using ERA pressure level data, interpolates temperature profile to provide AirT timeseries for any given point in ERA grid cell domain
#accepts 2d matrix of data by time*pressurelevels
#IN: 
# 1. ERA air temperature pressure level data (K)
# 2. ERA geopotential file (can be the same file) [m^2 s^-2]
# 3. height (m asl) of interpolation point
# returns tair at stationHeight 
 

plevel2point<-function(dat,gdat, stationEle){
require(Hmisc)
gph<-gdat/9.80665 # geopotential height = geopotential/9.80665 [gravity at sea level]
#gph[gph==0]<-0 #prevents negative m asl
t_vec=c()
for(i in 1:length(gph[,1])){
t<-approxExtrap( x=gph[i,],y=dat[i,], rule=2, xout=stationEle)[2]
t<-t$y
t_vec=c(t_vec, t)
}
t_vec<-as.numeric(t_vec)
#vectorise
#d=apply(X=dat, MARGIN=c(1,), FUN=interp6to3, x=gph, y=dat,xout=stationEle)


return(t_vec)
}

#============================================================================================
#		LWIN SCALING
#=============================================================================================

lwinTscale<-function( tpl,rhpl,rhGrid,tGrid, lwGrid, x1=0.484, x2=8){
#All T in kelvin

T_sub=tpl
RH=rhpl
#calculate water vapour pressure at sub
pv_sub=Rh2Wvp(tair=T_sub, RH=RH)

#parameters
x1=x1 # 0.484 or 0.43 Gubler 2012
x2=x2 # 8 or 5.7 Gubler 2012
sb=5.67*10^-8

#calculate clear-sky emissivity subgrid according Konzelmann
Ecl_sub=0.23+ x1*(pv_sub*100/T_sub)^(1/x2)

#calculate clear-sky LWin
lwcl=Ecl_sub*sb*T_sub^4

#T grid in Kelvin
T_grid=tGrid
RH=rhGrid
#calculate all-sky emissivity at grid from SB equation
Eas_grid=lwGrid/(sb*T_grid^4)	
#constrain Eas_grid to max. 1
Eas_grid[Eas_grid>1]<-1	

#calculate water vapour pressure at grid
pv_grid=Rh2Wvp(tair=T_grid,RH=RH )
#calculate clear-sky emissivity grid according Konzelmann
Ecl_grid=0.23+ x1*((pv_grid*100)/T_grid)^(1/x2)	#Ecl_grid

#calculate cloud emissivivity at grid
deltaE=Eas_grid-Ecl_grid

#calculate all-sky emissivity subgrid 
Eas_sub=Ecl_sub+deltaE				#Eas_sub
Eas_sub[Eas_sub>1]<-1	

#calculate lwin subgrid
lwas=Eas_sub*sb*T_sub^4

return(lwas)
}

#============================================================================================
#       				get ERA gridbox mean elevation			     			 	
#============================================================================================
#dem and ERA data in lon/lat

getEraEle=function(dem, eraFile){
d=raster(dem)
x=raster(eraFile)
gridBoxRst=init(x, v='cell')
poly=rasterToPolygons(gridBoxRst)
demc=crop(d,poly)

boxEle=extract(demc, poly,mean)
return(boxEle)
}
