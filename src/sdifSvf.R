# correct SWin diffuse for svf reduction
 
sdifSvf=function(sdifm, mf){

dims=dim(sdifm)
dim1=dims[1]
dim2=dims[2]

difvec=c()
for(i in 1:dim2){
dz=mf$svf[i]
difc=(sdifm[,i]*dz)
difvec=cbind(difvec,difc)
}

return(difvec)
}
