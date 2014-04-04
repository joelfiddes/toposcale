#partition SWin to direct and diffuse beam according Ruiz et al 2010

solarPartition=function(swPoint=swm,toaPoint=toam, out){

dims=dim(swm)
dim1=dims[1]
dim2=dims[2]

#===========================================================================================================================
#							RUIZ PARTITION
#============================================================================================================================
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

sdifm=matrix(sdif, dim1,dim2, byrow=F)
sdirm=matrix(sdir, dim1,dim2, byrow=F)

if(out=='dif'){return(sdifm)}
if(out=='dir'){return(sdirm)}
}

