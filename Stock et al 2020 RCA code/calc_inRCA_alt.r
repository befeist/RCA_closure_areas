# Brian Stock
# 9/3/15
# Calculate if fishing locations were in RCAs, using Date, Lat, Lon, and Depth
# Calculate inRCA with alternate methods:
#   1. Start only
#   2. End only
#   3. Start AND end
#   4. Avg with 1km buffer
#   5. Avg with 2km buffer
# ---------------------------------------------------------

# Go back to original data to grab start and end positions for each unique haul
setwd("/home/brian/Documents/Bycatch/WCGOP/data") 				# linux
load("wcgop_predicted_cv.RData") #42k tows
X <- read.csv("/home/brian/Documents/Bycatch/WCGOP/data/Stock_OBProcessed_Bycatch_2002_13_2014-12-04.csv")
data <- X[which((X$sector %in% c("Limited Entry Trawl","Catch Shares")) & X$gear=="Bottom Trawl"),]
n.hauls <- dim(dat)[1]
dat$SET_LAT <- dat$SET_LONG <- dat$UP_LAT <- dat$UP_LONG <- NA
for(i in 1:n.hauls){
	cur_haul <- data[which(data$HAUL_ID==dat$HAUL_ID[i]),]
	dat$SET_LAT[i] <- cur_haul[1,"SET_LAT"]
	dat$SET_LONG[i] <- cur_haul[1,"SET_LONG"]
	dat$UP_LAT[i] <- cur_haul[1,"UP_LAT"]
	dat$UP_LONG[i] <- cur_haul[1,"UP_LONG"]
} # takes a couple minutes (laptop)

# Get depths for Set and Up locations
# "get_depths.r" loads the bathymetry data into one raster "all"
source("/home/brian/Dropbox/bycatch/WCGOP/get_depths.r")
# # test 1000 depths
# round(extract(all,cbind(head(dat$SET_LONG,1000),head(dat$SET_LAT,1000)),method="bilinear")/-1.8288,0) 
dat$SET_DEPTH <- round(extract(all,cbind(dat$SET_LONG,dat$SET_LAT),method="bilinear")/-1.8288,0) 
dat$UP_DEPTH <- round(extract(all,cbind(dat$UP_LONG,dat$UP_LAT),method="bilinear")/-1.8288,0)
dat$AVG_DEPTH <- round(extract(all,cbind(dat$LONG,dat$LAT),method="bilinear")/-1.8288,0)

# Get RCA boundaries 2002-2012 (I'm not sure where 2013 came from, but it's the same as 2012)
# These were manually modified from "Historical_trawl_RCA_2002-2012_11.30.12.xlsx"
# If using more recent data, note that current RCA boundaries are slightly different than 2012
library(dplyr)
library(tidyr)
rca <- read.csv("/home/brian/Dropbox/bycatch/WCGOP/RCAs/rca_boundaries.csv",header=TRUE)
years <- sort(as.numeric(levels(as.factor(rca$Year))),decreasing=TRUE)

# Get number of lat bins for each year 
get_n_bins <- function(yr) {a <- rca %>% dplyr::filter(Year==yr) %>% dplyr::select(Lat.low) %>% dim; return(a[1])}
n.bins <- sapply(years,get_n_bins) 		# vector, # lat bins by year

# create lat bin # vector (row names for "rca")
LAT.bins <- NULL
for(yr in 1:length(n.bins)){ LAT.bins <- c(LAT.bins,n.bins[yr]:1) } 	

# each row in rca.new is a month (e.g. Dec 2012) with its low and high depth closures
rca.new <- rca %>% mutate(LAT.bin=LAT.bins) %>% gather(Month,Close,Jan:Dec)
close.lohi <- matrix(as.numeric(unlist(strsplit(rca.new$Close,"-"))), ncol=2, byrow=TRUE)
rca.new <- rca.new %>% mutate(close.low=close.lohi[,1],close.high=close.lohi[,2])

# depth in fathoms (positive)
check_RCA <- function(df,rca,rca.new,depthcol){
	y <- data.frame(date=df$DATE,lat=df$LAT,long=df$LONG,depth=df[depthcol],id=1:dim(df)[1])
	colnames(y)[4] <- "depth"

	# Don't need to check tows in depths > 250fm or year = 2002
	# Now only calculating for ~31k tows instead of 55k
	y$Year <- format(as.Date(y$date),"%Y")
	y$Month <- format(as.Date(y$date),"%b")
	checkRCA <- dplyr::filter(y, depth<250) # only could be in an RCA if depth < 250 fm
	checkRCA <- dplyr::filter(checkRCA,Year!=2002) # no RCA closures in 2002

	y$inRCA = 0 # add "inRCA" covariate (0 if not, 1 if yes), will leave as 0 all rows not in checkRCA
	y$bin = 0
	# this takes about 2 min to run for 33k points
	for(j in 1:nrow(checkRCA)){
		i <- checkRCA$id[j] # row j in checkRCA is row i in y
		breaks <- c(55,rca %>% dplyr::filter(Year==y$Year[i]) %>% dplyr::select(Lat.low) %>% unlist) # get lat/lon cutpoints for the tow's year
		y$bin[i] <- cut(y$lat[i],breaks=breaks,labels=1:(length(breaks)-1)) 	# get lat/lon bin of tow, defined by Lat and breaks above
		low <- rca.new %>% dplyr::filter(Year==y$Year[i],Month==y$Month[i],LAT.bin==y$bin[i]) %>% dplyr::select(close.low) 	# get lower depth of RCA for tow's month/year
		high <- rca.new %>% dplyr::filter(Year==y$Year[i],Month==y$Month[i],LAT.bin==y$bin[i]) %>% dplyr::select(close.high) # get upper depth of RCA for tow's month/year
		if(abs(y$depth[i]) < high & abs(y$depth[i]) > low) y$inRCA[i] = 1 	# inRCA=1 if the tow depth is between lower and upper RCA depth limits
	}
	# # Check inRCA is working
	# pos <- filter(test,inRCA==1) %>% select(Year,Month,Lat,Ngdc_fath,bin)
	# rca.new %>% filter(Year==pos$Year[i],Month==pos$Month[i],LAT.bin==pos$bin[i])
	return(y$inRCA)
}

# # Test on 10 tows
# test1 <- head(dat,10)
# check_RCA(test1,rca,rca.new,"SET_DEPTH")
# check_RCA(test1,rca,rca.new,"AVG_DEPTH")

dat$inRCA_1 <- check_RCA(dat,rca,rca.new,"SET_DEPTH") # 1 min
dat$inRCA_2 <- check_RCA(dat,rca,rca.new,"UP_DEPTH")
dat$inRCA_3 <- dat$inRCA_1 * dat$inRCA_2
dat$inRCA_check <- check_RCA(dat,rca,rca.new,"AVG_DEPTH")

sum(dat$inRCA)/dim(dat)[1]
sum(dat$inRCA_1)/dim(dat)[1]
sum(dat$inRCA_2)/dim(dat)[1]
sum(dat$inRCA_3)/dim(dat)[1]
sum(dat$inRCA_check)/dim(dat)[1]

# Calculate inRCA using buffer = 1km and 2km
# Define lat/long points for buffer, using geosphere::destPoint
library(geosphere)
# r is row of data frame
get_depth_circle <- function(r,buffer,all){
	cir <- geosphere::destPoint(c(r$LONG,r$LAT),1:360,buffer)
	depth <- round(raster::extract(all,cir,method="bilinear")/-1.8288,0)
	return(data.frame(DATE=r$DATE,LONG=cir[,1],LAT=cir[,2],DEPTH=depth))
}
# pts <- get_depth_circle(r=dat[1,],buffer=1000,all=all)
# check_all <- check_RCA(pts,rca,rca.new,"DEPTH")
# inRCA <- prod(check_all)

# Only check the buffer of locations in RCAs
checkRCA <- dplyr::filter(dat,inRCA_check==1) # tows in RCAs
dat$inRCA_4 <- dat$inRCA_5 <- 0
# 4. Buffer = 1000m
for(i in 1:dim(checkRCA)[1]){
	j <- checkRCA$id[i]
	pts <- get_depth_circle(dat[j,],buffer=1000,all)
	check_all <- check_RCA(pts,rca,rca.new,"DEPTH")
	dat$inRCA_4[j] <- prod(check_all)
} # 3:54

# 5. Buffer = 2000m
for(i in 1:dim(checkRCA)[1]){
	j <- checkRCA$id[i]
	pts <- get_depth_circle(dat[j,],buffer=2000,all)
	check_all <- check_RCA(pts,rca,rca.new,"DEPTH")
	dat$inRCA_5[j] <- prod(check_all)
} 

# 6. Set AND Up AND Avg w/in 1km buffer
dat$inRCA_6 <- dat$inRCA_4 * dat$inRCA_3

inRCA_summary <- dat %>% group_by(Year) %>% summarise(count=n(),inRCA_avg=sum(inRCA_check)) %>% mutate(propRCA=inRCA_check/count)
print(inRCA_summary)

# Print summary of alternate RCA definitions
altRCA_summary <- data.frame(
  defRCA=c("Avg depth Blake","Avg depth mine","Set only","Up only","Set AND Up","1km buffer","2km buffer","Set AND Up AND buffer"),
  propRCA=c(sum(dat$inRCA)/dim(dat)[1],
    sum(dat$inRCA_check)/dim(dat)[1],
    sum(dat$inRCA_1)/dim(dat)[1],
    sum(dat$inRCA_2)/dim(dat)[1],
    sum(dat$inRCA_3)/dim(dat)[1],
    sum(dat$inRCA_4)/dim(dat)[1],
    sum(dat$inRCA_5)/dim(dat)[1],
    sum(dat$inRCA_6)/dim(dat)[1]))
altRCA_summary$propRCA <- round(altRCA_summary$propRCA,3)
print(altRCA_summary)

# Calculate mean catch inside vs. outside RCA (binomial and positive)
summary_2 <- dat %>% group_by(inRCA_2) %>% summarise(propDBRK=length(which(DBRK>0))/n(),meanDBRK=mean(DBRK[which(DBRK>0)]), propPHLB=length(which(PHLB>0))/n(),meanPHLB=mean(PHLB[which(PHLB>0)]),propYEYE=length(which(YEYE>0))/n(),meanYEYE=mean(YEYE[which(YEYE>0)]))
summary_3 <- dat %>% group_by(inRCA_3) %>% summarise(propDBRK=length(which(DBRK>0))/n(),meanDBRK=mean(DBRK[which(DBRK>0)]), propPHLB=length(which(PHLB>0))/n(),meanPHLB=mean(PHLB[which(PHLB>0)]),propYEYE=length(which(YEYE>0))/n(),meanYEYE=mean(YEYE[which(YEYE>0)]))
summary_4 <- dat %>% group_by(inRCA_4) %>% summarise(propDBRK=length(which(DBRK>0))/n(),meanDBRK=mean(DBRK[which(DBRK>0)]), propPHLB=length(which(PHLB>0))/n(),meanPHLB=mean(PHLB[which(PHLB>0)]),propYEYE=length(which(YEYE>0))/n(),meanYEYE=mean(YEYE[which(YEYE>0)]))
summary_6 <- dat %>% group_by(inRCA_6) %>% summarise(propDBRK=length(which(DBRK>0))/n(),meanDBRK=mean(DBRK[which(DBRK>0)]), propPHLB=length(which(PHLB>0))/n(),meanPHLB=mean(PHLB[which(PHLB>0)]),propYEYE=length(which(YEYE>0))/n(),meanYEYE=mean(YEYE[which(YEYE>0)]))

cat("Avg (11%):\n", file = "/home/brian/Dropbox/bycatch/WCGOP/results/inRCA_summary.txt")
capture.output(summary_2,file = "/home/brian/Dropbox/bycatch/WCGOP/results/inRCA_summary.txt", append = TRUE)
cat("\nSet AND Up (2.8%):\n", file = "/home/brian/Dropbox/bycatch/WCGOP/results/inRCA_summary.txt",append=TRUE)
capture.output(summary_3,file = "/home/brian/Dropbox/bycatch/WCGOP/results/inRCA_summary.txt", append = TRUE)
cat("\n1km buffer (4.4%):\n", file = "/home/brian/Dropbox/bycatch/WCGOP/results/inRCA_summary.txt", append = TRUE)
capture.output(summary_4,file = "/home/brian/Dropbox/bycatch/WCGOP/results/inRCA_summary.txt", append = TRUE)
cat("\nSet AND Up AND buffer (1.4%):\n", file = "/home/brian/Dropbox/bycatch/WCGOP/results/inRCA_summary.txt", append = TRUE)
capture.output(summary_6,file = "/home/brian/Dropbox/bycatch/WCGOP/results/inRCA_summary.txt", append = TRUE)

# # Figure: number of tows inside RCAs by year
# pdf("/home/brian/Dropbox/bycatch/WCGOP/RCAs/inRCA_byyear.pdf")
# plot(inRCA_summary$Year,inRCA_summary$inRCA,type='o',xlab="Year",ylab="Number of tows in RCAs")
# dev.off()

# # Figure: percent of tows inside RCAs by year
# pdf("/home/brian/Dropbox/bycatch/WCGOP/RCAs/propRCA_byyear.pdf")
# plot(inRCA_summary$Year,inRCA_summary$propRCA,type='o',xlab="Year",ylab="Percent of tows in RCAs")
# dev.off()

save(dat,altRCA_summary,file="wcgop_inRCA_comparison.RData")

