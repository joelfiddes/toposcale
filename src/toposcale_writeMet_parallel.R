######################################################################################################
#	
#			TOPOSCALE WRITE MET FILES
#			
######################################################################################################

mf=read.table(paste(spath,'/listpoints.txt',sep=''),header=T,sep=',')
npoints=length(mf$id)
#========================================================================
#		MAKE SIM DIRS
#========================================================================
for ( i in 1:npoints){
gsimindex=formatC(mf$id[i], width=5,flag='0')
dir.create(paste(spath,'/result/S', gsimindex,sep=''))
dir.create(paste(spath,'/result/S', gsimindex,'/out',sep=''))
dir.create(paste(spath,'/result/S', gsimindex,'/rec',sep=''))
}
#========================================================================
#		CALC LW - (dependent on finished T/Rh)
#========================================================================
source(paste(root,'/src/TopoAPP/tscale_LW.R',sep=''))
#========================================================================
#		READ INDIVIDUAL FIELD FILES
#========================================================================
tPoint=read.table(paste(spath,  '/tPoint.txt',sep=''), header=T, sep=',')
rPoint=read.table(paste(spath,  '/rPoint.txt',sep=''), header=T, sep=',')
uPoint=read.table(paste(spath,  '/uPoint.txt',sep=''), header=T, sep=',')
vPoint=read.table(paste(spath,  '/vPoint.txt',sep=''), header=T, sep=',')
lwPoint=read.table(paste(spath,  '/lwPoint.txt',sep=''), header=T, sep=',')
sol=read.table(paste(spath,  '/sol.txt',sep=''), header=T, sep=',')
solDir=read.table(paste(spath,  '/solDir.txt',sep=''), header=T, sep=',')
solDif=read.table(paste(spath,  '/solDif.txt',sep=''), header=T, sep=',')

pSurf_lapse=read.table(paste(spath,  '/pSurf_lapse.txt',sep=''), header=T, sep=',')

#========================================================================
#		CALC WIND SPEED AND DIRECION FROM U/V
#========================================================================
u=uPoint
v=vPoint
wdPoint=windDir(u,v)
wsPoint=windSpd(u,v)
#========================================================================
#		MAKE MET FILES PER POINT
#========================================================================
load(paste(outRootmet,'/datesSurf',sep=''))
Date<-datesSurf_cut
Date<-format(as.POSIXct(Date), format="%d/%m/%Y %H:%M") #GEOtop format date

#stationOrder=mf$id[nameVec]
nameVec=1:npoints
#for(i in 1:length(stationOrder)){
for(i in 1:npoints){

Tair=round((tPoint[,i]-273.15),2) #K to deg
RH=round(rPoint[,i],2)
Wd=round(wdPoint[,i],2)
Ws=round(wsPoint[,i],2)
#SW=round(sol[,i],2)
sdir=round(solDir[,i],2)
sdif=round(solDif[,i],2)

LW=round(lwPoint[,i],2)
Prec=round(pSurf_lapse[,i],5)

#meteo=cbind(Date,Tair,RH,Wd,Ws,SW,LW,Prec)

meteo=cbind(Date,Tair,RH,Wd,Ws,sdif,sdir,LW,Prec)
if(length(which(is.na(meteo)==TRUE))>0){print(paste('WARNING:', length(which(is.na(meteo)==TRUE)), 'NANs found in meteofile',nameVec[i],sep=' '))}
if(length(which(is.na(meteo)==TRUE))==0){print(paste( length(which(is.na(meteo)==TRUE)), 'NANs found in meteofile',nameVec[i],sep=' '))}

gsimindex=formatC(nameVec[i], width=5,flag='0')
write.table(meteo, paste(spath, '/result/S',gsimindex,'/meteo0001.txt', sep=''), sep=',', row.names=F)
}

#========================================================================
#		REMOVE INDIVIDUAL FIELD FILES
#========================================================================

#EXTRACT DATA TO POINTS (sw and toa)
#returns 2d matrix with rows=stations in order of boxID (mf)
#boxID=mf$boxID
#swPoint=sw3hr[,boxID]
#toaPoint=toa3hr[,boxID]

