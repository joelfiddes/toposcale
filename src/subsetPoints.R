#find points in given gridbox box

## shapefile of val points
require(raster)
box<-nbox

## shapefile of points cut by ERA box
dat_cut=points_shp[dshp2[box,],]

if(length(dat_cut$ele)>0){

dat_cut$id<-1:length(dat_cut[,1]) # change id from 1:n
write.table(dat_cut, paste(spath, '/listpoints.txt', sep=''), sep=',', row.names=F)
}else{next}

