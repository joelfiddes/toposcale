######################################################################################################
#	
#			EXPERIMENT SETUP 1
#			
######################################################################################################


#method to concatenate
"&" <- function(...) UseMethod("&") 
"&.default" <- .Primitive("&") 
"&.character" <- function(...) paste(...,sep="") 

#=======================================================================================================
#			DIRECTORIES RELATIVE
#=======================================================================================================
dir.create(epath)
#master=paste(epath,'_master/',sep='')#created manually
datRoot=paste(epath,'/met/',sep='')#needed only when getERA=T
dir.create(datRoot,recursive=T)

tmp=paste(datRoot,'conv/',sep='')
outRootPl=paste(tmp,'pressurelevel/',sep='')
outRootSurf=paste(tmp,'surface/',sep='')
outRootmet=paste(tmp,'all/',sep='')
dir.create(tmp,recursive=T)
dir.create(outRootPl)
dir.create(outRootSurf)
dir.create(outRootmet)

#=======================================================================================================
#			INFILES
#=======================================================================================================
#PRESSURE LEVEL FIELDS (AND DEWT)
infileG=paste(datRoot, 'gpot.nc',sep='')
infileT=paste(datRoot, 'tpl.nc',sep='')
infileRh=paste(datRoot, 'rhpl.nc',sep='')
infileU=paste(datRoot, 'upl.nc',sep='')
infileV=paste(datRoot, 'vpl.nc',sep='')
infileD=paste(datRoot, 'dt.nc',sep='')

#SURFACE FIELDS
lwFile=paste(datRoot, 'strd.nc',sep='')
swFile=paste(datRoot, 'ssrd.nc',sep='')
pFile=paste(datRoot, 'p.nc',sep='')
tFile=paste(datRoot, 't.nc',sep='')
toaFile=paste(datRoot, 'toa.nc',sep='')






