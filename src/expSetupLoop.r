#======================================================================================
#				SET UP SIMULATION DIRECTORIES
#======================================================================================


#simulation path
spath=paste(epath,'/result/B',simindex,sep='') #simulation path
#input locations
#demLoc=paste(spath,'/preds/ele.tif',sep='')
#location of predictors 
#predRoot=paste(spath,'/preds',sep='')
#set tmp directory for raster package
rasterOptions(tmpdir=paste(spath, '/raster_tmp', sep=''))


dir.create(spath, recursive = TRUE)
#dir.create(predRoot)

outpath=paste(spath,'/result/out/',sep='') #simulation output path
dir.create(outpath, recursive=TRUE)
dir.create(paste(spath, '/raster_tmp', sep='')) #tmp folder
#set up folders
#dir.create(paste(spath, '/fuzRst', sep=''))
#dir.create(paste(spath, '/rec', sep=''))

