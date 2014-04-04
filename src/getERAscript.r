######################################################################################################
#	
#			GET ERA DATA
#			
######################################################################################################


workd=datRoot


#get ERA-I data script

#NEW SERVER: http://apps.ecmwf.int/datasets/data/interim_full_daily/
#NEW SERVER SCRIPT: https://software.ecmwf.int/wiki/display/WEBAPI/Accessing+ECMWF+data+servers+in+batch?
#DETAILS for server access: /home/joel/src/ERA_tools/eraBatchAccess.r
#REQUIRES: CDO (grb->nc conversion) https://code.zmaw.de/projects/cdo

# fetches ERA interim data based on parameters set. 
# Converts grib files to netcdf
# workd = folder to both write 'download_script.py to and also download data to
# more info on parameters and other options and ECWMF retrieval system 'MARS' http://www.ecmwf.int/publications/manuals/mars/guide/




#===============================================================================
#				GET BBOX FROM MF
#===============================================================================
#tol=0.1
#n=max(mf$lat+tol)
#s=min(mf$lat-tol)
#e=max(mf$lon+tol)
#w=min(mf$lon-tol)

#===============================================================================
#				GET BBOX FROM SHP
#===============================================================================
tol=0.7 #must be greater than 0.5*box resolution to get correct extent
xtent=extent(dshp2)
n=xtent@ymax-tol
s=xtent@ymin+tol
e=xtent@xmax-tol
w=xtent@xmin+tol
ar= paste(n,w,s,e,sep='/')# region of interest N/W/S/E

#===============================================================================
#				 PARAMETERS SURFACE
#===============================================================================
t='00/12'#00/12 gives 3hr data for sfc retrieval ; 00/06/12/18 gives 6hr data for pl retrieval (3hr not possible) ; 00/12 for accumulated
stp='3/6/9/12'#3/6/9/12 gives 3hr data for sfc ; 0 gives 6hr data for pl retrieval (3hr not possible)
lt='sfc'# sfc=surface or pl=pressure level
typ='fc'#an=analysis or fc=forecast, depends on parameter - check on ERA gui.


#===============================================================================
#				GET DATA SURFACE
#===============================================================================
for( i in 1:length(parNameSurf)){
par= parCodeSurf[i]# parameter code - check on ERA gui.
tar=paste(parNameSurf[i],'.grb', sep='')
getERA(dd=dd, t=t, grd=grd, stp=stp, lt=lt,typ=typ,par=par,ar=ar,tar=tar,plev=plev,workd=workd)
}

#===============================================================================
#				 PARAMETERS PRESSURE LEVEL
#===============================================================================
t='00/06/12/18'#00/12 gives 3hr data for sfc retrieval ; 00/06/12/18 gives 6hr data for pl retrieval (3hr not possible) ; 00/12 for accumulated
stp='0'#3/6/9/12 gives 3hr data for sfc ; 0 gives 6hr data for pl retrieval (3hr not possible)
lt='pl'# sfc=surface or pl=pressure level
typ='an'#an=analysis or fc=forecast, depends on parameter - check on ERA gui.

#===============================================================================
#				GET DATA PRESSURE LEVEL
#===============================================================================
for( i in 1:length(parNamePl)){
par= parCodePl[i]# parameter code - check on ERA gui.
tar=paste(parNamePl[i],'.grb', sep='')
getERA(dd=dd, t=t, grd=grd, stp=stp, lt=lt,typ=typ,par=par,ar=ar,tar=tar,plev=plev,workd=workd)
}

#===============================================================================
#				CLEAN UP GRIB FILES
#===============================================================================
grb=list.files(workd, pattern='.grb')

for(i in 1:length(grb)){
system(paste('rm ',workd,'/',grb[i],sep=''))
}



