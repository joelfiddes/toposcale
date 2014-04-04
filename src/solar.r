#solar functions
#add options eg terrain effects

solarCompute=function(swin,toa, dates,mf,tz){


#===========================================================================================================================
#							RUIZ PARTITION
#============================================================================================================================
dd=dates
swPoint=swin
toaPoint=toa
#dimensions
dim1=length(dd) #dims (dates * mf)
dim2=length(mf$id)

#correction find solar zenith
svec=c()
for(i in 1:dim2){
se=solarZ(dates=dd, lat=mf$lat[i], long=mf$lon[i], tz=tz)
svec=cbind(svec,se)
}



#need to add correction 
kt=as.vector(swPoint)/as.vector(toaPoint)#*se
kt[is.na(kt)==T]<-0 # make sure 0/0 =0
kt[is.infinite(kt)==T]<-0 # make sure 0/0 =0
kt[kt<0]<-0
kt[kt>1]<-0.8 #upper limit of kt

#very slow need to vectorise
#kdvec=c()
#for (i in 1:length(swPoint)){
#y=kt[i]
#kd=0.952 - 1.041 * exp(1) ^ - exp(2.300-4.702*y)
#kdvec=c(kdvec, kd)
#}
y<-kt
kdvec=0.952 - 1.041 * exp(1) ^ - exp(2.300-4.702*y)

sdif= kdvec*as.vector(swPoint)
sdir=as.vector(swPoint)-sdif

#===========================================================================================================================
#				ELEVATION ADJUSTMENT
#============================================================================================================================
sdirm=matrix(sdir,dim1 ,dim2, byrow=F) 

svec=c()
for(i in 1:dim2){
se=solarZ(dates=dd, lat=mf$lat[i], long=mf$lon[i], tz=1)
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
#deltam=mf$eleDiff*svec

#===========================================================================================================================
#				svf ADJUSTMENT -dif
#============================================================================================================================
sdifm=matrix(sdif, dim1,dim2, byrow=F)

difvec=c()
for(i in 1:dim2){
dz=mf$svf[i]
difc=(sdifm[,i]*dz)
difvec=cbind(difvec,difc)
}


#sol=difvec+dbvec+sdirm
sol=swPoint+dbvec
return(sol)
}

