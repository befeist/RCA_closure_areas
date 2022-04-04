# Brian Stock
# 8/18/15
# Calculate if fishing locations were in RCAs, using Date, Lat, Lon, and Depth

setwd("/home/brian/Documents/Bycatch/WCGOP/data")
library(dplyr)
library(tidyr)
# load("wcgop_predicted.RData") # this only has 42k tows, what I ended up fitting
load("wcgop_processed.RData") # this has 55k tows, what I sent/received from Blake

# Note that these boundaries are for gear="bottom trawl" tows, 
# from both "Catch Shares" (post 2011) and "Limited Entry Trawl" (pre 2011) sectors.

# Get RCA boundaries 2002-2012 (I'm not sure where 2013 came from, but it's the same as 2012)
# These were manually modified from "Historical_trawl_RCA_2002-2012_11.30.12.xlsx"
# If using more recent data, note that current RCA boundaries are slightly different than 2012
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

# "blake" is the data: Date, Lat, Long, Ngdc_fath (depth in fathoms)
blake <- read.table("observer_points_with_attributes.txt",header=TRUE,sep=",")

# Don't need to check tows in depths > 250fm or year = 2002
# Now only calculating for ~31k tows instead of 55k
checkRCA <- dplyr::filter(blake,Fath_categ!="250+") # only could be in an RCA if depth < 250 fm
checkRCA <- dplyr::filter(checkRCA,Year!=2002) # no RCA closures in 2002

# test <- checkRCA[1:1000,]
blake$Month <- format(as.Date(blake$Date),"%b")
blake$inRCA = 0 # add "inRCA" covariate (0 if not, 1 if yes), will leave as 0 all rows not in checkRCA
blake$bin = 0
# this takes about 2 min to run for 33k points
for(j in 1:nrow(checkRCA)){
	i <- checkRCA$Master_id[j]
	breaks <- c(55,rca %>% dplyr::filter(Year==blake$Year[i]) %>% dplyr::select(Lat.low) %>% unlist) # get lat/lon cutpoints for the tow's year
	blake$bin[i] <- cut(blake$Lat[i],breaks=breaks,labels=1:(length(breaks)-1)) 	# get lat/lon bin of tow, defined by Lat and breaks above
	low <- rca.new %>% dplyr::filter(Year==blake$Year[i],Month==blake$Month[i],LAT.bin==blake$bin[i]) %>% dplyr::select(close.low) 	# get lower depth of RCA for tow's month/year
	high <- rca.new %>% dplyr::filter(Year==blake$Year[i],Month==blake$Month[i],LAT.bin==blake$bin[i]) %>% dplyr::select(close.high) # get upper depth of RCA for tow's month/year
	if(abs(blake$Ngdc_fath[i]) < high & abs(blake$Ngdc_fath[i]) > low) blake$inRCA[i] = 1 	# inRCA=1 if the tow depth is between lower and upper RCA depth limits
}
# # Check inRCA is working
# pos <- filter(test,inRCA==1) %>% select(Year,Month,Lat,Ngdc_fath,bin)
# rca.new %>% filter(Year==pos$Year[i],Month==pos$Month[i],LAT.bin==pos$bin[i])

# blake$inRCA is the 0/1 RCA covariate
inRCA_summary <- blake %>% group_by(Year) %>% summarise(count=n(),inRCA=sum(inRCA)) %>% mutate(propRCA=inRCA/count)
print(inRCA_summary)

# Figure: number of tows inside RCAs by year
pdf("/home/brian/Dropbox/bycatch/WCGOP/RCAs/inRCA_byyear.pdf")
plot(inRCA_summary$Year,inRCA_summary$inRCA,type='o',xlab="Year",ylab="Number of tows in RCAs")
dev.off()

# Figure: percent of tows inside RCAs by year
pdf("/home/brian/Dropbox/bycatch/WCGOP/RCAs/propRCA_byyear.pdf")
plot(inRCA_summary$Year,inRCA_summary$propRCA,type='o',xlab="Year",ylab="Percent of tows in RCAs")
dev.off()

# get list of HAUL_IDs in RCAs to Jason
inRCA_hauls <- dat %>% dplyr::filter(inRCA==1) %>% dplyr::select(HAUL_ID)
write(as.matrix(inRCA_hauls),file="/home/brian/Dropbox/bycatch/WCGOP/RCAs/inRCA_hauls.txt",sep = "\t",ncolumns=1)
write.table(as.matrix(inRCA_summary),file="/home/brian/Dropbox/bycatch/WCGOP/RCAs/inRCA_summary.txt",sep="\t",quote=FALSE,row.names=FALSE)


