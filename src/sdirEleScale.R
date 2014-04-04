#===========================================================================================================================
#				ELEVATION ADJUSTMENT
#============================================================================================================================

sdirEleScale=function(sdirm,toaPoint,dates, mf){

dims=dim(sdirm)
dim1=dims[1]
dim2=dims[2]


svec=c()
for(i in 1:dim2){
se=solarZ(dates=dates, lat=mf$lat[i], long=mf$lon[i], tz=1)
svec=cbind(svec,se)
}

dbvec=c()
for(i in 1:dim2){
dz=mf$eleDiff[i]
dz=dz/1000
s=toaPoint[,i]
b=sdirm[,i]
zen=svec[,i]

thetaz=(pi/180)*zen #radians
m=1/cos(thetaz)
k= -log(b/s)/m
k[is.na(k)==T]<-0 #correct b=0/s=0 problem

t=exp(1)^(-k*dz*cos(thetaz))
t[t>1]<-1.1 #to constrain to reasonable values
t[t<0.8]<-0.9 #to constrain to reasonable values
db=(1-t)*sdirm[,i]
dbvec=cbind(dbvec,db)
}

sdirEle=sdirm+dbvec #additative correction
return(sdirEle)
}
