batchfile=paste(spath,'/batch_tscale.txt',sep='')
file.create(batchfile)

write(paste('cd ',root,sep=''),file=batchfile,append=T)
write('parallel   R CMD BATCH --no-save --no-restore ::: ./src/TopoAPP/tscale_SW.R ./src/TopoAPP/tscale_Rhum.R ./src/TopoAPP/tscale_windU.R ./src/TopoAPP/tscale_Tair.R ./src/TopoAPP/tscale_windV.R ./src/TopoAPP/tscale_P.R',file=batchfile,append=T)
#lwin is computed in toposcale_writeMet_parallel.r as dependent on T and R
system(paste('chmod 777 ',spath,'/batch_tscale.txt',sep=''))

