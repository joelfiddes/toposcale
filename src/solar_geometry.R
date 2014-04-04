solarGeom<-function(mf,dates, sdirm, tz){
require(insol)
#=======================================================================================================================
#	meta data of point/sample, sp
#==========================================================================================================================
dims=dim(sdirm)
sp=dims[2]
solTopoCorM=c()

for(i in 1:sp){
lat=mf$lat[i]
long=mf$lon[i]
slope=mf$slp[i]
aspect=mf$asp[i]
svf=mf$svf[i]

#===========================================================================================================================
#	compute mean horizon elevation - why get negative hor.el?
#==========================================================================================================================
hor.el=(((acos(sqrt(svf))*180)/pi)*2)-slope
hor.el[hor.el<0]<-0

#===========================================================================================================================
#	normal vector - Calculates a unit vector normal to a surface defined by slope inclination and slope orientation.
#==========================================================================================================================
nv = normalvector(slope=slope, aspect=aspect)

#===========================================================================================================================
#	Calculates a unit vector in the direction of the sun from the observer position.
#============================================================================================================================
jd=JD(dates)
sunv=sunvector(jd=jd, latitude=lat, longitude=long, timezone=tz)

#===========================================================================================================================
#Computes the intensity according to the position of the sun (sunv) and dotproduct normal vector to slope.
#============================================================================================================================
dotprod=sunv %*% as.vector(nv) #double check other instances of matrix*vector
dotprod[dotprod<0]<-0 #negative indicates selfshading

#===============================================================================================
#		SUN ELEVATION below hor.el set to 0 - binary mask
#================================================================================================
se= sunEle(dates=dates, lat=lat, long=long, tz=tz)
se[se<hor.el]<-0
se[se>0]<-1


#===============================================================================================
#		derive incident radiation on slope accounting for self shading and cast shadow and solar geometry
#================================================================================================
solTopoCor=se*dotprod*sdirm[,i]

solTopoCorM=cbind(solTopoCorM,solTopoCor)
}
return(solTopoCorM)
}

