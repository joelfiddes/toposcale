######################################################################################################
#	
#			TOPOSCALE WRITE MET FILES
#			
######################################################################################################


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
#		MAKE MET FILES PER POINT
#========================================================================
#stationOrder=mf$id[nameVec]

#for(i in 1:length(stationOrder)){
for(i in 1:npoints){

Tair=round((tPoint[,i]-273.15),2) #K to deg
RH=round(rPoint[,i],2)
Wd=round(wdPoint[,i],2)
Ws=round(wsPoint[,i],2)
SW=round(sol[,i],2)
LW=round(lwPoint[,i],2)
Prec=round(pSurf_lapse[,i],5)
Date<-datesSurf_cut
Date<-format(as.POSIXct(Date), format="%d/%m/%Y %H:%M") #GEOtop format date
meteo=cbind(Date,Tair,RH,Wd,Ws,SW,LW,Prec)

if(length(which(is.na(meteo)==TRUE))>0){print(paste('WARNING:', length(which(is.na(meteo)==TRUE)), 'NANs found in meteofile',nameVec[i],sep=' '))}
if(length(which(is.na(meteo)==TRUE))==0){print(paste( length(which(is.na(meteo)==TRUE)), 'NANs found in meteofile',nameVec[i],sep=' '))}

gsimindex=formatC(nameVec[i], width=5,flag='0')
write.table(meteo, paste(spath, '/result/S',gsimindex,'/meteo0001.txt', sep=''), sep=',', row.names=F)
}



#EXTRACT DATA TO POINTS (sw and toa)
#returns 2d matrix with rows=stations in order of boxID (mf)
#boxID=mf$boxID
#swPoint=sw3hr[,boxID]
#toaPoint=toa3hr[,boxID]

