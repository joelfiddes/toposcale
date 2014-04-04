# TODO: Add comment
# 
# Author: stgruber
###############################################################################



#==============================================================================
# MISC STUFF
#==============================================================================
#construct method to concatenate strings with ampersand
"&" <- function(...) UseMethod("&") 
"&.default" <- .Primitive("&") 
"&.character" <- function(...) paste(...,sep="") 

#require("gregmisc") - depracated
#does it really need these packages? 
require(gdata)
require(gtools)
require(gmodels)


#==============================================================================
# FIND LINE WITH A SPECIFIC KEYWORD (CASE INSENSITIVE) [NOT FOR EXPORT]
#==============================================================================
gt.par.fline<-function(fs, keyword) { 
	nl<-length(fs)
	comchar<-"!" #character to indicate comments
	splchar<-"=" #sparate variable name and content
	keyword<-toupper(keyword) #make case insensitive
	found<-FALSE
	
	#loop
	for (l in 1:nl) {
		line<-trim(fs[l]) #assign line, but first remove leading/trailing blanks
		if ((substr(line,1,1) != comchar)*(nchar(line) >= 1)) {
			varname<-unlist(strsplit(line,splchar, fixed = TRUE))[1]
			varname<-trim(varname) #remove blanks
			if ((toupper(varname) == keyword)) found<-TRUE
		}
		if (found == TRUE) break #exit loop if keyword was found
	}	
	
	#return result
	if (found == FALSE) {
		return(NA)
	} else {
		return(l)
	}
}


#==============================================================================
# EXTRACT VARIABLE IN A SPECIFIC LINE  [NOT FOR EXPORT]
#==============================================================================
gt.par.rline<-function(fs,ln) { 
	#fs --> file string containing entire parameter file
	#ln --> line number to read
	comchar<-"!" #indicate comments
	splchar<-"=" #sparate variable name and content
	vecchar<-"," #separate vector elements
	
	line<-trim(fs[ln]) #assign line, but first remove leading/trailing blanks
	line<-unlist(strsplit(line,splchar, fixed = TRUE))
	varname<-line[1] #get variable name
	varvalu<-lnie[2] #get variable value
	varvalu<-unlist(strsplit(line,comchar, fixed = TRUE))[1] #delete comment at end of line if one exists	
	varvalu<-unlist(strsplit(line,vecchar, fixed = TRUE))    #separate vector elements
	varvalu<-trim(varvalu) #remove blanks
	
	#return result
	return(varvalu)
}


#==============================================================================
# WRITE VARIABLE INTO A SPECIFIC LINE  [NOT FOR EXPORT]
#==============================================================================
gt.par.wline<-function(fs,ln,vs) { 
	#fs --> file string containing entire parameter file
	#ln --> line number to read
	#vs --> variable character string to insert
	comchar<-"!" #indicate comments
	splchar<-"=" #sparate variable name and content
	vecchar<-"," #separate vector elements
	
	#check is character string is vector or scalar
	if (length(vs) > 1) vs<-paste(vs,sep="",collapse=vecchar)
	line<-trim(fs[ln]) #assign line, but first remove leading/trailing blanks
	varname<-unlist(strsplit(line,splchar,fixed = TRUE))[1] #get variable name	
	line<-varname&splchar&vs
	fs[ln]<-line
	return(fs)
}


#==============================================================================
# WRITE VARIABLE INTO FILESTRING OF PARAMETER FILE  [FOR EXPORT]
#==============================================================================
gt.par.wvar<-function(fs,keyword,vs,type="NUMERIC") { 
	#fs --> file string containing entire parameter file
	#ln --> line number to read
	#vs --> variable to insert
	comchar<-"!" #indicate comments
	splchar<-"=" #sparate variable name and content
	vecchar<-"," #separate vector elements
	
	if (toupper(type) == "STRING") {
		vs<-"\""&vs&"\""
	} else {
		#convert to character
		vs<-as.character(vs)	
	}
	
	#check is character string is vector or scalar
	if (length(vs) > 1) vs<-paste(vs,sep="",collapse=vecchar)
	
	#find line
	ln<-gt.par.fline(fs, keyword)
	
	if (is.na(ln) == FALSE) { #insert into existing line
		fs<-gt.par.wline(fs,ln,vs)
	} else {        #if line does not exist, append at end
		line<-keyword&splchar&vs
		fs<-c(fs,line)
	}
	return(fs)
}


#==============================================================================
# READ EXPERIMENT MASTER PARAMETER FILE  [FOR EXPORT, DOCU]
#==============================================================================
#generate experiment directory and links
gt.exp.rmaster<-function(eroot) {
	#DIRECTORY STRUCTURE	
	#experiment_root/           --> base directory of an experiment
	#experiment_root/_master/   --> simulation template, experiments use common data from here
	#experiment_root/_control/  --> executable, source, batch.txt, documentation
	#experiment_root/000001/    --> path for experiment run 1
	#fs 						--> filestring with modified parameter file to define experiment
	parfilename<-"geotop.inpts" #standard name of parameter file
	fs<-readLines(eroot&"/_master/"&parfilename) 
	return(fs)
}
#==============================================================================



#==============================================================================
# REPLACE ONE LINE IN A SPECIFIC INPTS FILE  [FOR EXPORT]
#==============================================================================
gt.exp.modify<-function(eroot,enumber,keyword,value,type="NUMERIC") {
	#make path
	epath<-eroot&"/"&formatC(enumber,width=6,flag="0")&"/"
	#standard name of parameter file
	parfilename<-"geotop.inpts" 
	#read inpts file
	fs<-readLines(epath&parfilename) 
	#replace one parameter
	fs<-gt.par.wvar(fs,keyword,value,type="NUMERIC")
	#write paramater_file -------------------------------
	comchar<-"!" #indicate comments
	con<-file(epath&"/"&parfilename, "w")  # open an output file connection
	cat(comchar,"MODIFIED PARAMETER FILE",'\n', file = con,sep="")
	cat(fs, file = con,sep='\n')
	close(con)
}#=============================================================================


#==============================================================================
# DELETE FILE IN ONE EXPERIMENT  [FOR EXPORT]
#==============================================================================
gt.exp.file.delete<-function(eroot,enumber,file) {
	#make path
	epath<-eroot&"/"&formatC(enumber,width=6,flag="0")&"/"
	
	#delete file (* and? work)
	unlink(epath&file, recursive = FALSE)
}
#==============================================================================




#==============================================================================
# WRITE EXPERIMENT DIRECTORY AND PARAMETER FILE  [FOR EXPORT]
#==============================================================================
#generate experiment directory and links
gt.exp.write<-function(eroot_loc,eroot_sim,enumber,fs) {
	#DIRECTORY STRUCTURE	
	#experiment_root/           --> base directory of an experiment
	#experiment_root/_master/   --> simulation template, experiments use common data from here
	#experiment_root/_control/  --> executable, source, batch.txt, documentation
	#experiment_root/000001/    --> path for experiment 1
	#fs 						--> filestring with modified parameter file to define experiment
	#eroot_loc                  --> experiment root on local machine (where simulation is prepared)
	#eroot_sim                  --> experiment root on simulation machine (used for batch file writing)
	comchar<-"!" #indicate comments
	parfilename<-"geotop.inpts" #standard name of parameter file
	#make directories ----------------------------------
	enumber<-round(enumber,0)
	epath<-eroot_loc&"/"&formatC(enumber,width=6,flag="0")
	dir.create(epath, showWarnings = TRUE, recursive = FALSE)        # experiment directory
	dir.create(epath&"/out", showWarnings = TRUE, recursive = FALSE) # output
	dir.create(epath&"/out/rec", showWarnings = TRUE, recursive = FALSE) # recovery 
	dir.create(epath&"/in", showWarnings = TRUE, recursive = FALSE) # input 
	#make symbolic links ________________________________
	setwd(epath&"/in")
	system("ln -sf ../../_master/meteo meteo")
	system("ln -sf ../../_master/hor hor") 
	#write paramater_file -------------------------------
	con <- file(epath&"/"&parfilename, "w")  # open an output file connection
	cat(comchar,"SCRIPT-GENERATED EXPERIMENT FILE",'\n', file = con,sep="")
	cat(fs, file = con,sep='\n')
	close(con)
	
	#return path	
	return(epath)
}
#==============================================================================
#same as above but not ctreate all new directories -0 writes to existing epath
gt.exp.write2<-function(eroot_loc,eroot_sim,enumber,fs) {

	comchar<-"!" #indicate comments
	parfilename<-"geotop.inpts" #standard name of parameter file
	enumber<-round(enumber,0)
	epath<-eroot_loc&"/"&formatC(enumber,width=6,flag="0")
	setwd(epath&"/in")

	con <- file(epath&"/"&parfilename, "w")  # open an output file connection
	cat(comchar,"SCRIPT-GENERATED EXPERIMENT FILE",'\n', file = con,sep="")
	cat(fs, file = con,sep='\n')
	close(con)
	
	return(epath)
}

#same as above but not ctreate all new directories -0 writes to existing epath +EVEN SIMPLER
gt.exp.write3<-function(simPath,fs) {

	comchar<-"!" #indicate comments
	parfilename<-"geotop.inpts" #standard name of parameter file
	setwd(simPath)

	con <- file(simPath&"/"&parfilename, "w")  # open an output file connection
	cat(comchar,"SCRIPT-GENERATED EXPERIMENT FILE",'\n', file = con,sep="")
	cat(fs, file = con,sep='\n')
	close(con)
	
	return(simPath)
}

#==============================================================================
# WRITE BATCH/SCHROEDINGER FILES
#==============================================================================
# This script will make a list of experiment directories contained in a
# local simmulation directory (eroot_loc) and write bach files for the
# Schroedinger grid engine.
# Execute on Schroedinger by typing "qsub myscript.sh" (or any other filename)
# You can control the state of your job by "qstat"
#
# VARIABLES - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#      eroot_loc: experiment root directory on local machine 
#                 (e.g., /home/stgruber/exp01)
#      eroot_sim: experiment root directory on 
#                 productive (Schroedinger) file system
#    enumber_vec: vector of experiment numbers (e.g., 1:60)
#           name: short name of job to be displayed in Schroedinger 
#                 SGE statistics (e.g., EXP01)
#        maxtime: maximum time in seconds needed for GEOtop run (see below)
#
# SCHROEDINGER QUEUEING SYSTEM - - - - - - - - - - - - - - - - - - - - - - - -
# ----name----  ---maxtime--  
# very-short.q	        1800  
#      short.q         86400  
#        med.q        172800  
#       long.q        259200  
#==============================================================================
gt.exp.batch<-function(eroot_loc, eroot_sim, enumber_vec, name, maxtime, executable) {
	#settings
	batchfilename  <-"batch_schroedinger.txt"         #file with list of individual simulations
	scriptfilename <-"geotop_schroedinger_submit.sh"  #script for job submission: "qsub myscript.sh" (or any other filename)
	cleanupfilename_in <-"geotop_schroedinger_cleanup_in.sh"  #script to delete all in/ directories and make a .tgz
	cleanupfilename_rec<-"geotop_schroedinger_cleanup_rec.sh" #script to delete all in/ directories and make a .tgz
	
	#---- make Schroedinger SCRIPT file ---------------------------------------
	#Create filestring and insert first line, Bourne shell
	fs<-"#!/bin/sh"	
	
	#set number of simulations and step size
	fs<-c(fs,"#$ -t 1-"&round(length(enumber_vec),0)&":8      # setting range and step size")
	
	#set maximum time in seconds	
	fs<-c(fs,"#$ -N "&as.character(name)&" # name of this Schroedinger run")
	
	#set maximum time in seconds	
	fs<-c(fs,"#$ -l s_cpu="&round(maxtime,0)&" # max time [s] per cpu")
	
	#diverse settings
	fs<-c(fs,"#$ -S /bin/sh     # shell used")
	fs<-c(fs,"#$ -v PATH        # environment variables to be exported to the execution context of the job")
	fs<-c(fs,"#$ -o $JOB_NAME_$JOB_ID_$TASK_ID.out  # create output file per task ($TASK_ID only valid here)")
	fs<-c(fs,"#$ -j y           # error stream of job merged into standard output stream")
	fs<-c(fs,"# this script launches 8 simulations on one node and the startes the next 8")
	
	# set main variables
	fs<-c(fs,"eroot_sch="&eroot_sim)
	
	#entering loop 0 to 7
	fs<-c(fs,"#entering loop 0 to 7")	
	fs<-c(fs,"for fi in 0 1 2 3 4 5 6 7")	
	fs<-c(fs,"do")	
	fs<-c(fs,"  counter=$(( $SGE_TASK_ID + $fi ))   # increase counter, bash arithmetics, here SGE_TASK_ID not as above")		
	fs<-c(fs,"  counter_run=$(( $counter * 2 ))      # line number for run command")
	fs<-c(fs,"  counter_cop=$(( $counter_run - 1 ))  # line number for copy command")
	#use paste to insert quotations into string
	fs<-c(fs,paste("  gtcop=`sed -n ","\"","${counter_cop}p","\""," $eroot_sch/_control/batch_schroedinger.txt` #get line from job list",sep=""))
	fs<-c(fs,paste("  gtjob=`sed -n ","\"","${counter_run}p","\""," $eroot_sch/_control/batch_schroedinger.txt` #get line from job list",sep=""))	
	#copy and run job
	fs<-c(fs,"  $gtcop   # copy input to experiment directory ")
	fs<-c(fs,"  $gtjob & # execute job in background")
	fs<-c(fs,"done")
	
	#wait for jobs to be done
	fs<-c(fs,"wait # wait until all background jobs are finished")
	
	#write file
	con <- file(eroot_loc&"/_control/"&scriptfilename, "w")  # open an output file connection
	cat(fs,file = con,sep='\n')
	close(con)
	
	
	
	#---- make BATCH file ------------------------------------------------
	# Uneven numbers copy the input directory, even numbers run a job.
	exe_sch<-eroot_sim&"/_control/"&executable # full path to executable
	path_in<-eroot_sim&"/_master/in"          # source of input data to copy
	fs<-NULL #initialise empty file string
	#loop over directories
	for (enumber in enumber_vec) {
		epath<-eroot_sim&"/"&formatC(round(enumber_vec[enumber],0),width=6,flag="0")&"/"
		line_cop<-"cp -r "&path_in&" "&epath
		line_run<-exe_sch&" "&epath
		fs<-c(fs,line_cop,line_run)
	}
	#write Schroedinger batch file
	con <- file(eroot_loc&"/_control/"&batchfilename, "w")  # open an output file connection
	cat(fs,file = con,sep='\n')
	close(con)
	
	
	#---- make INPUT CLEANUP SCRIPT file ---------------------------------------
	# Create filestring and insert first line, Bourne shell
	fs<-"#!/bin/sh"	
	#loop over directories
	for (enumber in enumber_vec) {
		epath_in<-eroot_sim&"/"&formatC(round(enumber,0),width=6,flag="0")&"/in"
		fs<-c(fs,"rm -rf "&epath_in)
	}
	#write file
	con <- file(eroot_loc&"/_control/"&cleanupfilename_in, "w")  # open an output file connection
	cat(fs,file = con,sep='\n')
	close(con)
	
	
	#---- make RECOVERY CLEANUP SCRIPT file ---------------------------------------
	# Create filestring and insert first line, Bourne shell
	fs<-"#!/bin/sh"	
	#loop over directories
	for (enumber in enumber_vec) {
		epath_in<-eroot_sim&"/"&formatC(round(enumber,0),width=6,flag="0")&"/rec"
		fs<-c(fs,"rm -rf "&epath_in)
	}
	#write file
	con <- file(eroot_loc&"/_control/"&cleanupfilename_rec, "w")  # open an output file connection
	cat(fs,file = con,sep='\n')
	close(con)
}
#==============================================================================




#==============================================================================
# MAKE HORIZON FILES
#==============================================================================
#Needs to be updated
#make series of standard horizon files - e.g.,  hor0001.txt for 10¡ 
horfile.write<-function(eroot_loc) {
	for (el in seq(from=0,to=90,by=10)) {
		hor<-data.frame(az=c(0,360),el=c(el,el))
		#write file
		outfile<-eroot_loc&"/_master/hor000"&round(el/10+1,0)&".txt"
		write.csv(hor,file=outfile,quote=FALSE,row.names = FALSE)
	}
}
#==============================================================================




#==============================================================================
# SKY VIEW FACTOR (analtic for slope and horizon of constant elevation, only)
#==============================================================================
sky.view<-function(slp,hor.el) {
	sky<-cos((slp+hor.el)/2*pi/180)^2
	return(round(sky,2))
}
#==============================================================================




#==============================================================================
# MAKE POINTS FOR POINT FILES
#==============================================================================
# Changes: 
# 26/03/2012: removed "eroot_loc" as firs parameter [Stephan Gruber]
points.make<-function(hor_seq, ele_seq, slp_seq, asp_seq, lan_seq, 
		soi_seq, dis_seq, fre_seq, swe_seq, cur_seq) {	
	#initialize names
	ID_vec<-NULL
	ele_vec<-NULL
	slp_vec<-NULL
	asp_vec<-NULL
	sky_vec<-NULL
	hor_vec<-NULL #horizon elevation (uniform)	
	lan_vec<-NULL #land cover type number
	soi_vec<-NULL #soil type number
	dis_vec<-NULL #distance to free drainage
	fre_vec<-NULL #depression of free drainage below surface
	swe_vec<-NULL #maximum snow water equivalent
	cur_vec<-NULL #curvature (snow transport) in all directions
	
#make loop and generate data frame
	ID<-1
	for (ele in ele_seq) {
		for (slp in slp_seq) {
			if (slp == 0) {avec<-0:0} else {avec<-asp_seq}
			for (asp in avec) {
				for (hor in hor_seq) {
					sky<-sky.view(slp,hor) 
					for (lan in lan_seq) {					
						for (soi in soi_seq) {
							for (dis in dis_seq) {
								for (fre in fre_seq) {
									for (swe in swe_seq) {
										for (cur in cur_seq) {
											ID_vec<-c( ID_vec,ID)
											ele_vec<-c(ele_vec,ele)
											slp_vec<-c(slp_vec,slp)
											asp_vec<-c(asp_vec,asp)
											sky_vec<-c(sky_vec,sky)
											hor_vec<-c(hor_vec,round(hor/10+1,0)) #make right number		
											lan_vec<-c(lan_vec,lan)
											soi_vec<-c(soi_vec,soi)
											dis_vec<-c(dis_vec,dis)
											fre_vec<-c(fre_vec,fre)
											tmp<-swe*(90-slp)
											if (slp <= 50) {tmp<-1000000}
											swe_vec<-c(swe_vec,tmp)
											cur_vec<-c(cur_vec,cur)		
											ID<-ID+1
										}
									}
								}
							}
						}
					}	
				}	
			}	
		}	
	}	
	topo<-data.frame(ID=ID_vec,ele=ele_vec,slp=slp_vec,asp=asp_vec,sky=sky_vec,
			landcover=lan_vec,soil=soi_vec,dist=dis_vec,free=fre_vec, 
			maxswe=swe_vec,curvNS=cur_vec,curvWE=cur_vec,
			curvNwSe=cur_vec,curvNeSw=cur_vec,hor=hor_vec)
	return(topo)
}
#==============================================================================





#==============================================================================
#FUNCTION TO MAKE THICKNESS AND DEPTH OF LAYERS  [FOR EXPORT]
#============================================================================== 
#  dzmin: minimal z-spacing ("how fine is the grid") 
#   zmax: depth of lowermost node center ("how large is the domain") 
#   base: resolution reduction ("how strong is the grid corsened with depth")
gt.soil.discretize <- function(dzmin,zmax,base) {
	#layer thicknesses	
	dz<-dzmin*base^(0:2005)
	
	#depth of layer lower boundary
	z_bot<-cumsum(dz)
	#depth of layer upper boundary
	z_top<-z_bot-dz
	#depth of layer center (node)
	z<-(z_bot+z_top)/2
	
	#data frame
	discr<-data.frame(dz=dz,z=z,z_bot=z_bot,z_top=z_top)
	
	#restrict to maximum depth
	discr<-subset(discr,z < zmax)
	nz<-length(discr$z)
	
	discr$dz[nz] <- zmax-discr$z_bot[nz-1] 	
	discr$z[nz]  <- discr$z_bot[nz-1] + discr$dz[nz]/2    
	
	#drop auxiliary columns
	discr<-discr[1:nz,1:2] 
	return(discr)
}
#==============================================================================



#==============================================================================
# WRITE SOIL TYPE [STEFANIE]
#==============================================================================
gt.write.soil <- function(fs, soil.type, soil.file){
	#fs<-gt.par.wvar(fs,"!FIXED STUFF", paste("soil_", soil.type, sep =""))	
	for(i in 1:dim(soil.file)[1]){
		fs <- gt.par.wvar(fs, as.character(soil.file[i,"Name"]), soil.file[i, soil.type])
	}
	return(fs)
}
#==============================================================================





#==============================================================================
#FUNCTION TO MAKE SNOW PARAMETERIZATIONS  [FOR EXPORT]
#============================================================================== 
# For new parameterization, GEOtop 1.222 and higher 
#   nbot: number of layers with limited thickness near ground surface 
#   ntop: number of layers with limited thickness at top of snow pack 
# nthick: number of layers in the middle that can have any thickness
gt.snow.discretize <- function(nbot,ntop,nthick) {
	#ThickerSnowLayers
	tl<-(1:nthick)+nbot
	
	#max number of layers
	ml<-sum(nbot,ntop,nthick)
	
	#return data frame
	return(data.frame(MaxSnowLayerNumber=round(ml,0),ThickerSnowLayers=round(tl,0)))
}
#==============================================================================





#==============================================================================
# ACCESS EXPERIMENT RESULTS  [FOR EXPORT]
#==============================================================================
# takes into account and merges existing recovery files
gt.run.read<-function(eroot,enumber,file){
	#make directory
	path<-eroot&"/"&formatC(enumber,width=6,flag="0")&"/"

	#read data
	data<-read.csv(path&file)

	#loop over possible recovery files
	filebase <-substr(file,1,nchar(file)-4)
	extension<-".txt"		
	for (rec in 1:999) {
		suffix<-"_crec"&formatC(rec,width=4,flag="0")
	    recfile<-path&filebase&suffix&extension
		if (file.exists(recfile)) {
			data<-rbind(data,read.csv(recfile))
		} else break
	}
	
	#return
	return(data)
}
#==============================================================================



#==============================================================================
# READ GEOTOP MATRIX INTO DATA FRAME [FOR EXPORT]
#============================================================================== 
#usage to extract comment:
#comment<-gt.rmatrix("/group/geotop/sometestfile.txt",comment=TRUE)
#usage to read data:
#comment<-gt.rmatrix("/group/geotop/sometestfile.txt")
gt.rmatrix<-function(infile,comment=FALSE) {
	comchar<-"!" #character to indicate comments
	GTtime<-"%d/%m/%Y %H:%M" #descrition of data format used by GEOtop
	#count comment lines
	found<-FALSE #begin of data found? (first line without comment)
	fs <-readLines(infile,n=1000)
	for (l in 1:length(fs)) {
		line<-trim(fs[l])
		if (substr(line,1,1) != comchar) break
	}
	l<-l-1
	#decide if comment of data is returned
	if (comment == TRUE) {
		return(fs[1:l]) #get and return comment
	} else {
		data<-read.csv(infile, skip=l, header = TRUE, sep = ",", dec=".")
		#if first column is a date, then convert
		#data format always looks like this:
		#31/01/2004 13:24:00
		#1234567890123456789
		isDate<-TRUE
		data[,1]<-as.character(data[,1])
		if (nchar(data[1,1]) != 16)        isDate<-FALSE
		if (substr(data[1,1], 3, 3) != "/") isDate<-FALSE
		if (substr(data[1,1], 6, 6) != "/") isDate<-FALSE
		if (substr(data[1,1],11,11) != " ") isDate<-FALSE
		if (substr(data[1,1],14,14) != ":") isDate<-FALSE	
		if (isDate == TRUE) data$Date <- as.POSIXct(data[,1], GTtime, tz="")	
		return(data)
	}
}		
#==============================================================================



#==============================================================================
# READ GEOTOP TIME SERIES, SELECT SIMULATION PERIOD and RUN [FOR EXPORT]
#============================================================================== 
gt.rmatrix.speriod<-function(infile, speriod, run){
	data<-gt.rmatrix(infile)
	data<-subset(data, Simulation_Period==speriod & Run == run)
	return(data)
}
#==============================================================================





#==============================================================================
# WRITE DATA FRAME INTO GEOTOP MATRIX [FOR EXPORT]  
#============================================================================== 
gt.wmatrix<-function(data, outfile, comment="") {
	comchar<-"!" #character to indicate comments
	GTtime<-"%d/%m/%Y %H:%M" #descrition of data format used by GEOtop
	data$Date<-format(data$Date,format=GTtime)
	write.table(data, file = outfile, append = FALSE, quote = FALSE, sep = ",",
			eol = "\n", na = "NA", dec = ".", row.names = FALSE,
			col.names = TRUE)
}	
#==============================================================================




#==============================================================================
# DISTRIBUTE DAILY PRECIPITATION RANDOMLY OVER 24 HOURS   [FOR EXPORT]  
#============================================================================== 
# INPUT RESTRICTIONS:
#   Both data frames have a column called "Date" in POSIXct and the column with
#   precipitation is called "Prec"
#
# RATIONALE AND METHOD:
#   When only daily preciptation is available, this needs to be distributed to
#   a higher-resolution (usually hourly) data set of other meteorologicl 
#   observations. In the absence of better models that may account for e.g., 
#   convective clouding, precipitation is distributed randomly to avoid a bias
#   with temperature affecting rain/snow proportions.
#   The variable "hourly_max" limits the hourly precipitation. More than that is
#   distributed to moew than one hour.
#
# ATTENTION:
#   Daily precipitation is limited to hourly_max*24 !!
gt.input.prec.day2hour<-function(df.daily.prec,df.hourly.met, hourly_max) {
	#get begin and end of common period
	beg<-max(min(df.daily.prec$Date),min(df.hourly.met$Date))
	end<-min(max(df.daily.prec$Date),max(df.hourly.met$Date))
	#cut to common dates
	df.daily.prec<-df.daily.prec[df.daily.prec$Date >= beg,]
	df.daily.prec<-df.daily.prec[df.daily.prec$Date <= end,]	
	df.hourly.met<-df.hourly.met[df.hourly.met$Date >= beg,]
	df.hourly.met<-df.hourly.met[df.hourly.met$Date <= end,]	
	
	#select only days with precipitation
	df.daily.prec<-df.daily.prec[df.daily.prec$Prec > 0,]
	df.daily.prec$nhours<-floor(df.daily.prec$Prec/hourly_max)+1
	
	
	#remove exceeding precip
	df.daily.prec$Prec[df.daily.prec$Prec > (hourly_max*24)]=hourly_max*24
	
	tmp.daily<-NULL #dummy name
	#loop over durations
	for (dur in 1:24) {
		#select days with precipitation that is apportioned to n hours
		tmp<-df.daily.prec[df.daily.prec$nhours == dur,1:2]
		#generate random hour
		tmp$Date<-tmp$Date+floor(runif(length(tmp[,2]),0,(25-dur)))*3600
		#partition precip
		tmp$Prec<-tmp$Prec/dur
		tmp$Date<-tmp$Date-3600 #to prepare later adding
		#make new times and add to common data frame
		for (idur in 1:dur) {
			tmp$Date<-tmp$Date+3600 #add one hour
			tmp.daily<-rbind(tmp.daily,tmp)  #add to data frame
		}
	} 
	df.daily.prec$Prec[is.na(df.daily.prec$Prec) == TRUE]<-0
	
	#memorize origial sum
	sum.o<-sum(df.daily.prec$Prec)
	#sort
	df.daily.prec<-tmp.daily[order(tmp.daily$Date),]
	df.daily.prec$Prec[is.na(df.daily.prec$Prec) == TRUE]<-0
	
	#correct to origial sum
	sum.n<-sum(df.daily.prec$Prec)
	df.daily.prec$Prec<-round(df.daily.prec$Prec*(sum.o/sum.n),1)
	
	#insert precipitation into houly
	out<-merge(df.hourly.met, df.daily.prec, by = "Date",all.x = TRUE, incomparables = 0.0)
	out$Prec[is.na(out$Prec) == TRUE]<-0
	
	#out<-out[!duplicated(out$Date),]
	
	#return
	return(out)
}
#==============================================================================





#==============================================================================
# READ MANY GEOTOP TIME SERIES AND COMBINE DATA FRAMES [FOR EXPORT]
#============================================================================== 
# drop decimals and replace "X" from headers like "X10.000000"
gt.header.suffix.add<-function(header,LetterSuffix,LetterStart="X") {
	#extract names to change
	Index<-substr(header,1,1)==LetterStart
	ToChange<-header[Index]
	NChange<-length(ToChange)
	if (NChange > 0) {
		ToChange<-strsplit(ToChange,".",fixed=TRUE)
		ToChange<-unlist(ToChange)[(0:(NChange-1))*2+1]
		substr(ToChange,1,1)<-LetterSuffix
		header[Index]<-ToChange	
	}
	return(header)
}
#
gt.run.combined<-function(eroot,ie,ListResultFiles,ListNamePrefix) {
	#read result file
	nfiles<-length(ListResultFiles)
	AllRaw<-gt.run.read(eroot,ie,ListResultFiles[1])
	#drop non-numeric column
	AllRaw<-subset(AllRaw,select=-Date12.DDMMYYYYhhmm.)
	names(AllRaw)<-gt.header.suffix.add(names(AllRaw),ListNamePrefix[1])
	
	#read files if more than one and merge into one data frame
	if (nfiles > 1) {
		for (ifile in 2:nfiles) {				
			new<-gt.run.read(eroot,ie,ListResultFiles[ifile])
			#drop non-numeric column
			new<-subset(new,select=-Date12.DDMMYYYYhhmm.)
			names(new)<-gt.header.suffix.add(names(new),ListNamePrefix[ifile])
			AllRaw<-merge(AllRaw,new, 
					by=c("Date","Simulation_Period","Run","IDpoint"))
		}		
	}		
	
	#return result
	return(AllRaw)
}
#==============================================================================




#==============================================================================
# MAKE PARAMETER SET FOR SENSITIVITY STUDY
#==============================================================================
#This function creates a parameter set based on the data frame param. 
#Local or global parameter set generation can be chosen. Local means to
#keep all parameters to their default value and to only change one parameter
#at a time. Global means that the full parameter space is explored. If n1, n2...
#are the numbers of values per parameter, then the total number of experiments
#is for local (n1+n2+....*nN) and for global (n1*n2*....*nN).
gt.parameter_set.create<-function(param,method=local) {
	parse<-FALSE #did I understand method?
	
	# make default array line and names
	npar  <-length(param)
	parr  <-data.frame(newcol=get(names(param)[1],param)$default[1])
	names(parr)<-as.character(get(names(param)[1],param)$keyword[1])
	if (npar > 1) {
		for (pn in 2:npar) {
			newcol<-data.frame(newcol=
							get(names(param)[pn],param)$default[1])
			
			#combine array and fix names
			parr<-cbind(parr,newcol)
			names(parr)[pn]<-as.character(get(names(param)[pn],param)$keyword[1])
		}	
	}
	
	#-- LOCAL SENSITIVITY ------------
	if (toupper(method)=="LOCAL") {
		parse<-TRUE
		# loop over parameters and fill in non-default columns
		for (pn in 1:npar) { #loop over variables
			vec<-get(names(param)[pn],param)$values
			vec<-vec[vec != get(names(param)[pn],param)$default[1]]
			for (pv in 1:length(vec)) { #loop over values
				newrow<-parr[1,]
				newrow[pn]<-vec[pv]
				parr<-rbind(parr,newrow)
			}
		}
	} # end of local
	
	#-- GLOBAL SENSITIVITY ------------
	if (toupper(method)=="GLOBAL") {
		parse<-TRUE
		# loop over parameters and fill in non-default columns
		for (pn in 1:npar) { #loop over variables
			vec<-get(names(param)[pn],param)$values
			newrows<-parr
			for (pv in 1:length(vec)) { #loop over values
				newrows[,pn]<-vec[pv]
				parr<-rbind(parr,newrows)
			}
		}
		#drop non-unique
		parr<-unique(parr)
		
	} #end of global
	
	#
	if (parse == FALSE) {
		print("Could not parse method string")
		return(-1)
	}
	
	#write
	return(parr)
}
#==============================================================================





#==============================================================================
# EXTEND PARAMETER SET FOR SENSITIVITY STUDY WITH DEFAULT VALUES
#==============================================================================
gt.parameter_set.extend<-function(parr,param) {
	#establish length
	nrows<-length(parr[,1])
	
	# make default array line and names
	npar<-length(param)
	for (pn in 1:npar) {
		newcol<-data.frame(newcol=
						rep(get(names(param)[pn],param)$default[1],nrows))
		
		#combine array and fix names
		parr<-cbind(parr,newcol)
		names(parr)[length(parr[1,])]<-as.character(get(names(param)[pn],param)$keyword[1])
	}
	
	#write
	return(parr)
}
#==============================================================================





#==============================================================================
# BATCH PROCESS POINT OUTPUT FILES FROM SENSITIVITY STUDIES [FOR EXPORT]
#==============================================================================
# -read parameter file, make parameter data frame
# -loop over experiment directories, based on data frame, possibly restricted range
# -read listpoint file, combine with parameter data frame  
# -read point output files, 
# -loop over years and point number
# -parse to processor function to compute results
# -append to data frame
# -write output data frame in experiment directory
#
# ListResultFiles: c("soilT.txt","soilL.txt")
gt.pointout.batch<-function(eroot, parfile, topofile, exp_range, 
		ListResultFiles,ListNamePrefix, 
		StartDate, function_processing) {
	
	#read parameter file
	par<-read.csv(file.path(eroot,parfile))
	
	#restrict number of experiments to process
	if (exp_range[1] > 0) par<-par[exp_range,]
	
	#data frame to hold all results
	AllResults<-data.frame()
	
	#loop over directories
	for (ie in par$enumber) {
		print(paste("Processing experiment number:",ie))
		
		#read topography file and establish number of points
		topography<-gt.run.read(eroot,ie,topofile)
		topography$ID<-as.integer(topography$ID)
		
		#read and combine result files
		AllRaw<-gt.run.combined(eroot,ie,ListResultFiles,ListNamePrefix)
		
		#establish number of years and add column with year to data frame
		AllRaw$year<-as.POSIXlt(AllRaw$Date)$year+1900	
		
		#year loop
		for (iy in unique(AllRaw$year)) {
			YearRaw<-subset(AllRaw,year==iy) #change to smarter system
			#point loop
			for (ip in topography$ID) {
				#extract data from results
				DataYearPoint<-subset(YearRaw,IDpoint==ip,select=-Date)
				#evaluate supplied analysis function for one topography point and for one year
				ResYearPoint<-eval(call(function_processing,DataYearPoint))
				ResYearPoint$year   <-iy #add year
				ResYearPoint$point  <-ip #add point
				ResYearPoint$enumber<-ie #add point
				#add row to data frame
				AllResults<-rbind(AllResults,ResYearPoint)
			}
		}
	}	
	#merge data frames of one directory with topography information	
	AllResults<-merge(AllResults,topography,by.x="point",by.y="ID")
	#merge data frames of one directory with parameter information
	AllResults<-merge(AllResults,par,by.x="enumber",by.y ="enumber")
	
	#write data frame to master directory
	write.csv(AllResults, file=file.path(eroot,"_master/analysis.txt"), row.names = FALSE)
}


#==============================================================================
# WRITE EXPERIMENT, SPLIT TOPO OVER SEVERAL DIRECTORIES [FOR EXPORT]
#==============================================================================
# -read parameter file, make parameter data frame
# -loop over experiment directories, based on data frame, possibly restricted range
# -read listpo
gt.exp.subset.write<-function(eroot_loc,eroot_sim,enumber,fs,topo,ntopo_opt) {
	#establish stepping through point file
	ntopo     <- ceiling(dim(topo)[1] / ntopo_opt)   #numer of loops required
	topo_last <- dim(topo)[1] - ntopo_opt * (ntopo-1)#number in last loop
	
	#loop though topo list
	for (tn in seq_along(1:ntopo)) { #loop over number of topography setups
		#write experiment
		epath<-gt.exp.write(eroot_loc,eroot_sim,enumber,fs)
		
		#write points
		if(tn != ntopo){
			write.csv(topo[(tn-1)*ntopo_opt+(1:ntopo_opt),], 
					file=file.path(paste(epath, "/in",sep=""),"listpoints.txt"), 
					quote=FALSE,row.names=FALSE)}
		if(tn == ntopo){
			write.csv(topo[(tn-1)*ntopo_opt+(1:topo_last),], 
					file=file.path(paste(epath, "/in",sep=""),"listpoints.txt"), 
					quote=FALSE,row.names=FALSE)
		}
		
		enumber<-enumber+1 #increment experiment number
	}
	return(enumber-1)
}




