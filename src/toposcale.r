########################################################################################################
#
#			TOPOSCALE
#
########################################################################################################

#===========================================================================
#				POINTS
#===========================================================================
#Get points meta data - loop through box directories
mf=read.table(listpoints,header=T,sep=',')

npoints=length(mf$id)


#find ele diff station/gidbox
eraBoxEle<-getEraEle(dem=eraBoxEleDem, eraFile=tFile) # $masl
gridEle<-rep(eraBoxEle[nbox],length(mf$id))
mf$gridEle<-gridEle
eleDiff=mf$ele-mf$gridEle
mf$eleDiff<-eleDiff



#=======================================================================================================
#			READ FILES
#=======================================================================================================

load(paste(outRootmet,'/gPl',sep=''))
load(paste(outRootmet,'/tairPl',sep=''))
load(paste(outRootmet,'/rhumPl',sep=''))
load(paste(outRootmet,'/uPl',sep=''))
load(paste(outRootmet,'/vPl',sep=''))
#var name =*Pl_cut (e.g. gPl_cut)
load(paste(outRootmet,'/lwSurf',sep=''))
load(paste(outRootmet,'/swSurf',sep=''))
load(paste(outRootmet,'/tSurf',sep=''))
load(paste(outRootmet,'/rhSurf',sep=''))
load(paste(outRootmet,'/pSurf',sep=''))
load(paste(outRootmet,'/toaSurf',sep=''))
load(paste(outRootmet,'/datesSurf',sep=''))
#var name =*Surf_cut (e.g. tSurf_cut)
#========================================================================
#		COMPUTE SCALED FLUXES - T,Rh,Ws,Wd,LW
#========================================================================

#init dataframes
tPoint=c()
rPoint=c()
uPoint=c()
vPoint=c()
lwPoint=c()
nameVec=c()

#for actual point only
#nboxSeq=c(1:eraCells)
	#for (nbox in nboxSeq){

		#get grid coordinates
		x<-coordMap$xlab[nbox] # long cell
		y<-coordMap$ylab[nbox]# lat cell

#get long lat centre point of nbox (for solar calcs)
lat=get.var.ncdf(nc, 'lat')
lon=get.var.ncdf(nc, 'lon')
latn=lat[y]
lonn=lon[x]
mf$lat=rep(latn,length(mf$id))
mf$lon=rep(lonn,length(mf$id))

		#extract PL data by nbox coordinates dims[data,xcoord,ycoord, pressurelevel]
		gpl=gPl_cut[,x,y,]
		tpl=tairPl_cut[,x,y,]
		rpl=rhumPl_cut[,x,y,]
		upl=uPl_cut[,x,y,]
		vpl=vPl_cut[,x,y,]

		#extract surface data by nbox  dims[data,nbox]
		lwSurf=lwSurf_cut[,nbox]
		tSurf=tSurf_cut[,nbox]
		rhSurf=rhSurf_cut[,nbox]
		toaSurf=toaSurf_cut[,nbox]
		swSurf=swSurf_cut[,nbox]
		

		#get grid mean elev
		gridEle<-eraBoxEle[nbox]

		#for actual point only
		#get valid stations in nbox
#		singleGridboxPoints_vec=mf$id[mf$boxID==nbox]
#		if (length(na.omit(singleGridboxPoints_vec))==0){next}
#		stations<-na.omit(singleGridboxPoints_vec)
stations=mf$id

		#get station attributes
		lsp=mf[stations,]
	
			for (i in 1:length(lsp$id)){

			stationEle=lsp$ele[i]
			nameVec=c(nameVec,(lsp$id[i])) #keeps track of order of stations

			#AIR TEMPERATURE
			t_mod<-plevel2point(gdat=gpl,dat=tpl, stationEle=stationEle)
			tPoint=cbind(tPoint, t_mod)
			#RELATIVE HUMIDITY
			r_mod<-plevel2point(gdat=gpl,dat=rpl, stationEle=stationEle)
			rPoint=cbind(rPoint, r_mod)
			#WIND U
			wu<-plevel2point(gdat=gpl,dat=upl, stationEle=stationEle)
			uPoint=cbind(uPoint, wu)
			#WIND V
			wv<-plevel2point(gdat=gpl,dat=vpl, stationEle=stationEle)
			vPoint=cbind(vPoint, wv)
			#LWIN			
			lw_mod<-lwinTscale( tpl=t_mod,rhpl=r_mod,rhGrid=rhSurf,tGrid=tSurf, lwGrid=lwSurf, x1=0.484, x2=8)
			lwPoint=cbind(lwPoint, lw_mod)

			}
		#for actual point only}

#========================================================================
#		CONVERT U/V TO WS/WD
#========================================================================
u=uPoint
v=vPoint
wdPoint=windDir(u,v)
wsPoint=windSpd(u,v)

#========================================================================
#		correct order of grid boxes from flux computation
#========================================================================
#nboxOrd=mf$boxID[nameVec]#for actual point only

#========================================================================
#		COMPUTE SW (no loop needed)
#========================================================================


sw=swSurf_cut[,nbox]
swm=matrix(rep(sw,npoints),ncol=npoints) #make matrix with ncol =points, repeats of each nbox vector
toa=toaSurf_cut[,nbox]
toam=matrix(rep(toa,npoints),ncol=npoints)


dd=as.POSIXct(datesSurf_cut)

sol=solarCompute(swin=swm,toa=toam, dates=dd,mf=mf,tz=tz)

#========================================================================
#		COMPUTE P
#========================================================================
pSurf=pSurf_cut[,nbox] #in order of mf file
pSurfm=matrix(rep(pSurf,npoints),ncol=npoints)

#weights function here
#need to convert monthly correction factor to 3hr one
#weights_stk=stack(subWeights)
#cf=extract(weights_stk, valShp)
#z=t(cbind(cf,cf,cf,cf,cf,cf,cf,cf,cf,cf,cf,cf,cf))#repeat for x (13) years of dataframe
if(climtolP==TRUE){
subw=brick(subWeights)
idgrid=raster(idgrid)
df=data.frame(getValues(idgrid),getValues(subw))
}
#============================================================================================
#			Apply Liston lapse
#=============================================================================================
ed=mf$eleDiff
lapseCor=(1+(pfactor*(ed/1000))/(1-(pfactor*(ed/1000))))
pSurf_lapseT=t(pSurfm)*lapseCor
pSurf_lapse=t(pSurf_lapseT)



