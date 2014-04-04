
#===============================================================================================
#		SUN ELEVATION
#================================================================================================
sunEle=function(dates=dates, lat=lat, long=long, tz=tz){
require(insol)
#Computes Julian Day from POSIXct object
jd=JD(dates)
#Calculates a unit vector in the direction of the sun from the observer position.
sunv=sunvector(jd=jd, latitude=lat, longitude=long, timezone=tz)
#Returns a matrix of azimuth and zenith angles of the sun given the unit vectors from the observer to the direction of the sun.
zen=sunpos(sunv)
se=90-zen[,2]
return(se)
}


#===============================================================================================
#		solar Zenith
#================================================================================================
solarZ=function(dates=dates, lat=lat, long=long, tz=tz){
require(insol)
#Computes Julian Day from POSIXct object
jd=JD(dates)
#Calculates a unit vector in the direction of the sun from the observer position.
sunv=sunvector(jd=jd, latitude=lat, longitude=long, timezone=tz)
#Returns a matrix of azimuth and zenith angles of the sun given the unit vectors from the observer to the direction of the sun.
zen=sunpos(sunv)
se=zen[,2]
return(se)
}

#===============================================================================================
#		solar AXIMUTH
#================================================================================================


solarA=function(dates=dates, lat=lat, long=long, tz=tz){
require(insol)
#Computes Julian Day from POSIXct object
jd=JD(dates)
#Calculates a unit vector in the direction of the sun from the observer position.
sunv=sunvector(jd=jd, latitude=lat, longitude=long, timezone=tz)
#Returns a matrix of azimuth and zenith angles of the sun given the unit vectors from the observer to the direction of the sun.
az=sunpos(sunv)
se=az[,1]
return(se)
}




