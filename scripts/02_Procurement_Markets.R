##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       2_Procurement_Markets.R produces the costs inputs to the PMGs and also
#                   writes out the buy and sell inputs from the producers and cosumers tables
#Date:              June 30, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 2 Procurement Markets
#-----------------------------------------------------------------------------------
progressStart(pmg,3)

#------------------------------------------------------------------------------------------------------
# Process skims and create inputs and functions for applying models
#------------------------------------------------------------------------------------------------------
progressNextStep("Processing skims")

#Distribution Channel Model: develop correspondences 
distchan_calcats <- data.table(CHOICE=c("0","1","2+","2+"),CHID=1:4) #correspondence between choice model alts and target categories int he distchan model
#add the FAMESCTG category to pairs for comparison with calibration targets
famesctg <- c(rep("A",3),rep("E",4),rep("K",2),rep("F",3),rep("C",7),rep("B",5),rep("J",6),"C",rep("G",3),"D",rep("I",2),"G","J",rep("H",4))
setnames(distchan_cal, c("CATEGORY", "CHOICE", "TARGET"))
#process gcd skims
setkey(mesozone_gcd, Production_zone, Consumption_zone)
mesozone_gcd[,c("Production_lon", "Production_lat", "Consumption_lon", "Consumption_lat"):=NULL]# Drop unneeded fields

#Shipment size model: correspondences, clean up calibration data
ShipSize_calcats <- data.table(CHOICE=1:9,CHID=c(rep(1,5),2,rep(3,3))) #correspondence between choice model alts and target categories in the shipment size model
setkey(ShipSize_cal, Commodity_SCTG, WeightCategory)
ShipSize_cal <- ShipSize_cal[!Commodity_SCTG %in% c(0,99),list(Commodity_SCTG, WeightCategory, TonsPct)]
ShipSize_cal[is.na(TonsPct) | TonsPct < 0.0001,TonsPct:=0.0001]
setnames(ShipSize_cal, c("Commodity_SCTG", "WeightCategory", "TonsPct"), c("CATEGORY", "CHOICE", "TARGET"))

## ---------------------------------------------------------------
## Heither, revised 03-18-2016: code to recalculate weight distributions for low weight (Size 1) & Heavy weight (Size 3)
##                              shipments to better reflect 2007 CFS weight distribution data.
## -- Low Weight
shipSize1 <- ShipSize_cal[CHOICE<=5]	## under 1000 pounds
shipSize1[,x:= ave(shipSize1$TARGET,shipSize1$CATEGORY,FUN=cumsum)] ## calc cumulative sums
shipSize1[,x2:= ave(shipSize1$x,shipSize1$CATEGORY,FUN=max)] ## max value within group
shipSize1[,Target2:= x/x2]  ## updated distribution within commodity-size group
shipSize1 <- shipSize1[CHOICE<5]	## drop last value for distribution calculation
ship1=list()
for(i in unique(shipSize1$CATEGORY)){
	targ <- shipSize1[CATEGORY==i,Target2]
	ship1[[i]] <- targ
}
## -- High Weight
shipSize3 <- ShipSize_cal[CHOICE>=7]	## 10000 pounds or more
shipSize3[,x:= ave(shipSize3$TARGET,shipSize3$CATEGORY,FUN=cumsum)] ## calc cumulative sums
shipSize3[,x2:= ave(shipSize3$x,shipSize3$CATEGORY,FUN=max)] ## max value within group
shipSize3[,Target2:= x/x2]  ## updated distribution within commodity 
shipSize3 <- shipSize3[CHOICE<9]
ship3=list()
for(i in unique(shipSize3$CATEGORY)){
	targ <- shipSize3[CATEGORY==i,Target2]
	ship3[[i]] <- targ
}

## ---------------------------------------------------------------
## Heither, revised 09-25-2015: CMAP pre-processing procedures create time & cost fields for all 54 mode paths for
##                              ALL Domestic and Foreign combinations.  RSG shortcut code is now turned off.

#### -- OBSOLETE: CMAP only skims, time and cost fields for 54 paths
setnames(skims, c("Origin", "Destination"), c("Production_zone", "Consumption_zone"))
#### -- setkey(skims, Production_zone, Consumption_zone)
cskims <- skims[,c("Production_zone", "Consumption_zone",paste0("time", 1:54), paste0("cost", 1:54)),with = FALSE]
#### -- ###TODO need some minimum times and costs for intrazonal trips
#### -- cskims[Production_zone==Consumption_zone,c("time31","time46","cost31","cost46"):=0]
#### -- ###TODO create skims for none-Chicago pairs -- based on zone to zone distances and speeds
#### -- #add a skim for time and cost 31 -- '31 - FTL Direct', 46 -- '46 - LTL Direct'
#### -- #hardcode these values and apply to the non CMAP zones
#### -- timepermile <- 0.0124 #hours
#### -- costpermile <- 0.0895 #cost is in per ton-mile
#### -- ncskims <- mesozone_gcd[Production_zone>150 & Consumption_zone>150]
#### -- ncskims[,time31:=timepermile*GCD]
#### -- ncskims[,time46:=timepermile*GCD]
#### -- ncskims[,cost31:=costpermile*GCD]
#### -- ncskims[,cost46:=costpermile*GCD]
#### -- ncskims[,GCD:=NULL]
#### -- ncskims[,c(paste0("time",c(1:30,32:45,47:54)),paste0("cost",c(1:30,32:45,47:54))):=NA]
#### -- cskims<-rbind(cskims,ncskims,use.names=TRUE)
setkey(cskims, Production_zone, Consumption_zone)
#### -- rm(skims,ncskims)
rm(skims)
##
## Heither, revised 01-29-2016: create a file of ineligible modes for each zone pair (QC review)
mdpaths <- c(1:54)
test <- cbind(melt(cskims[,c("Production_zone","Consumption_zone",paste0("time",mdpaths)),with=F], measure.vars=paste0("time",mdpaths), variable.name="timepath",
	value.name="time"), melt(cskims[,paste0("cost",mdpaths),with=F], measure.vars=paste0("cost",mdpaths), variable.name="costpath", value.name="cost"))
test[,MinPath:=as.numeric(substring(test$timepath, first=5))]
ineligible <- test[is.na(cost),list(Production_zone,Consumption_zone,MinPath)]
ineligible[,InEl:=1]	
setkey(ineligible,Production_zone,Consumption_zone,MinPath)
## ---------------------------------------------------------------

#Prepare SCTG specific input file
setkey(sctg,Commodity_SCTG)
#Assign values for B2,B3,B5,a, and sdQ paramaters in logistics cost equation
sctg[,c("B2","B3","B5"):=c(model$scenvars$LowDiscRate,model$scenvars$MedDiscRate,model$scenvars$MedDiscRate,model$scenvars$MedDiscRate,model$scenvars$HighDiscRate)
     [match(Category,c("Bulk natural resource (BNR)","Animals","Intermediate processed goods (IPG)","Other","Finished goods (FG)"))]]
sctg[,a:=c(model$scenvars$LowMultiplier,model$scenvars$MediumMultiplier,model$scenvars$HighMultiplier)
     [match(Category2,c("Functional","Functional/Innovative","Innovative"))]]
sctg[,sdQ:=c(model$scenvars$LowVariability,model$scenvars$MediumVariability,model$scenvars$HighVariability)
     [match(Category2,c("Functional","Functional/Innovative","Innovative"))]]

#For each of the 54 possible paths, define the B0 (logistics cost equation constant) and the ls (log savings) value
#These vary depending on the commodity and mode; extremely high values of B0 are set to indicate a non-available mode-path
sctg[,paste0("B0",1:54):=10000]
sctg[,paste0("ls",1:54):=1]

#Category='Bulk natural resource (BNR)'
sctg[Category=="Bulk natural resource (BNR)",paste0("B0",1:2) := 0]
sctg[Category=="Bulk natural resource (BNR)",paste0("ls",1:2) := 0.5]
sctg[Category=="Bulk natural resource (BNR)",paste0("B0",3:12) := sctg[Category=="Bulk natural resource (BNR)",paste0("B0",3:12), with = FALSE] * 0.1]
sctg[Category=="Bulk natural resource (BNR)",paste0("B0",13:45) := sctg[Category=="Bulk natural resource (BNR)",paste0("B0",13:45), with = FALSE] * 10.0]
sctg[Category=="Bulk natural resource (BNR)",ls3 := 0.5]

#Category='Animals'
sctg[Category=="Animals",paste0("B0",3:12) := sctg[Category=="Animals",paste0("B0",3:12), with = FALSE] * 0.75]
sctg[Category=="Animals",paste0("B0",c(1:2,13:30)) := sctg[Category=="Animals",paste0("B0",c(1:2,13:30)), with = FALSE] * 10.0]
sctg[Category=="Animals",B031 := B031*0.25]

#Category='Intermediate processed goods (IPG)'
sctg[Category=="Intermediate processed goods (IPG)" | Category=="Other",paste0("B0",1:12) := sctg[Category=="Intermediate processed goods (IPG)" | Category=="Other",paste0("B0",1:12), with = FALSE]* 2.0]
sctg[Category=="Finished goods (FG)",paste0("B0",1:12) := sctg[Category=="Finished goods (FG)",paste0("B0",1:12), with = FALSE] * 10.0]
sctg[Category=="Intermediate processed goods (IPG)" | Category=="Other",paste0("B0",14:30) := sctg[Category=="Intermediate processed goods (IPG)" | Category=="Other",paste0("B0",14:30), with = FALSE] * 0.9]
sctg[Category=="Intermediate processed goods (IPG)" | Category=="Other",paste0("B0",31:46) := sctg[Category=="Intermediate processed goods (IPG)" | Category=="Other",paste0("B0",31:46), with = FALSE] * 0.5]

#Category='Finished goods (FG)'
sctg[Category=="Finished goods (FG)",paste0("B0",31:46) := sctg[Category=="Finished goods (FG)",paste0("B0",31:46), with = FALSE] * 0.9]
sctg[Category=="Finished goods (FG)",paste0("B0",c(32:45,47:50)) := sctg[Category=="Finished goods (FG)",paste0("B0",c(32:45,47:50)), with = FALSE] * 0.25]

## ---------------------------------------------------------------
## Heither, revised 07-24-2015: added model$scenvars variables, corrected B3 portion of function to include j, corrected
##                              final line of equation to implement CS code
#Define the logistics cost function used in the mode path model
calcLogisticsCost <- function(dfspi,s,path){
  
  setnames(dfspi,c("pounds","weight","value","lssbd","time","cost"))
  ## variables from model$scenvars
  B1   <- model$scenvars$B1		#Constant unit per order
  B4   <- model$scenvars$B4		#Storage costs per unit per year
  j    <- model$scenvars$j		#Fraction of shipment that is lost/damaged  
  sdLT <- model$scenvars$sdLT	#Standard deviation in lead time
  LT_OrderTime <- model$scenvars$LT_OrderTime	#Expected lead time for order fulfillment (to be added to in-transit time)  
  #vars from S
  B2 <- s$B2
  B3 <- s$B3
  B5 <- s$B5
  a  <- s$a
  sdQ<- s$sdQ
  B0 <- s[[paste0("B0",path)]]
  ls <- s[[paste0("ls",path)]]
  
  #adjust factors for B0 and ls based on small buyer, 
  if(s$Category=="Bulk natural resource (BNR)" & path %in% 5:12) B0 <- B0 - 0.25*B0*dfspi$lssbd
  if(s$Category=="Finished goods (FG)" & path %in% c(14:30,32:38)) ls <- ls - 0.25*ls*dfspi$lssbd
  if(s$Category=="Finished goods (FG)" & path %in% 39:45) ls <- ls - 0.5*ls*dfspi$lssbd
  
  #Calculate annual transport & logistics cost
  dfspi[,minc:= B0/1000 * runif(length(pounds)) + #changed scale of B0 -- too large for small shipment flows
          B1*pounds/weight + 
          ls*(pounds/2000)*cost + #since cost is per ton
          B2*j*value +
          B3*time/24*j*value/365 + 
          (B4 + B5*value/(pounds/2000))*weight/(2*2000) +
          a*sqrt((LT_OrderTime+time/24)*((sdQ*pounds/2000)^2) + (pounds/2000)^2*(sdLT*LT_OrderTime)^2)]
		  
  return(dfspi$minc)
}
## ---------------------------------------------------------------

## ---------------------------------------------------------------
## Heither, revised 07-24-2015: added model$scenvars variables, corrected mode exclusion logic, updated Mesozone references
## Heither, revised 11-06-2015: revised mode exclusion logic due to new indirect truck modes for non-CMAP US shipments
## Heither, revised 11-24-2015: revised to include recycling check file
## Heither, revised 02-05-2016: revised so correct modepath is reported for Supplier-Buyer pair [keep NAICS/Commodity_SCTG, return as stand-alone data.table]
minLogisticsCostSctgPaths <- function(dfsp,iSCTG,paths){
    s <- sctg[iSCTG] 
    dfsp <- merge(dfsp,cskims[,c("Production_zone","Consumption_zone",paste0("time",paths),paste0("cost",paths)),with=F])
    numrows <- nrow(dfsp)
    dfsp <- cbind(melt(dfsp[,c("Production_zone","Consumption_zone","SellerID","BuyerID","NAICS","Commodity_SCTG","PurchaseAmountTons","weight","ConVal","lssbd",paste0("time",paths)),with=F], measure.vars=paste0("time",paths), variable.name="timepath", value.name="time"), 
                  melt(dfsp[,paste0("cost",paths),with=F], measure.vars=paste0("cost",paths), variable.name="costpath", value.name="cost"))
    dfsp[,path:=as.numeric(unlist(lapply(paths,rep,numrows)))]   ## store numeric path value; as.numeric to stop warning about int-num mismatch between dfsp & pc (Heither, 11-06-2015)
    dfsp[,minc:=unlist(lapply(paths,function(x) calcLogisticsCost(dfsp[path==x,list(PurchaseAmountTons,weight,ConVal,lssbd,time,cost)],s,x)))]
    ## variables from model$scenvars
    CAP1Carload  <- model$scenvars$CAP1Carload    #Capacity of 1 Full Railcar
    CAP1FTL      <- model$scenvars$CAP1FTL		#Capacity of 1 Full Truckload 
    CAP1Airplane <- model$scenvars$CAP1Airplane	#Capacity of 1 Airplane Cargo Hold
  
  dfsp[,avail:=TRUE]
  dfsp[path %in% 1:12 & weight<CAP1Carload,avail:=FALSE] #Eliminate Water and Carload from choice set if Shipment Size < 1 Rail Carload
  dfsp[path %in% c(14,19:26,31) & weight<CAP1FTL,avail:=FALSE] #Eliminate FTL and FTL-IMX combinations from choice set if Shipment Size < 1 FTL
  dfsp[path %in% c(32:38) & weight<CAP1FTL,avail:=FALSE] #Eliminate FTL Linehaul with FTL external drayage choice set if Shipment Size < 1 FTL (Heither, 11-06-2015)
  dfsp[path %in% c(15:18,27:30,39:46) & weight>CAP1FTL,avail:=FALSE] #Eliminate LTL and its permutations from choice set if Shipment Size > FTL (Heither, 11-06-2015)  
  dfsp[path %in% 47:50 & weight>CAP1Airplane,avail:=FALSE] #Eliminate Air from choice set if Shipment Size > Air Cargo Capacity
  dfsp[path %in% 51:52 & weight<(0.75*CAP1FTL),avail:=FALSE] #Eliminate Container-Direct from choice set if Shipment Size < 1 40' Container
  dfsp[path %in% 53:54 & weight<CAP1FTL,avail:=FALSE] #Eliminate International Transload-Direct from choice set if Shipment Size < 1 FTL  
  dfsp <- dfsp[avail==TRUE & !is.na(minc)]		## limit to only viable choices before finding minimum path
  dfsp <- dfsp[dfsp[,.I[which.min(minc)],by=list(SellerID,BuyerID)]$V1,]
  dfsp <- dfsp[,list(SellerID,BuyerID,NAICS,Commodity_SCTG,time,path,minc)]
  
  ###if(file.exists("E:/cmh/mode_check0.txt")){
		###write.table(dfsp, file="E:/cmh/mode_check0.txt", row.names=F, col.names=F, append=TRUE, sep=",")
		###} else {write.table(dfsp, file="E:/cmh/mode_check0.txt", row.names=F, sep=",")}
  numrows2 <- nrow(dfsp)
  
  cc <- model$Current_Commodity
  rec_chk <- model$recycle_check
  x_write=c(cc,iSCTG,numrows,numrows2,length(paths))
  write(x_write, rec_chk, ncol=length(x_write), append=TRUE)
  return(dfsp)
}

## Heither, revised 02-05-2016: revised so correct modepath is reported for Supplier-Buyer pair [return stand-alone data.table]
minLogisticsCost <- function(df,runmode){  
  pass <- 0			### counter for number of passes through function
  for (iSCTG in unique(df$Commodity_SCTG)){
    print(paste("iSCTG: ",iSCTG))
	
	###### Heither, 02-09-2016: Runmode==0 - use for initial cost file development and shipper select best mode --
	if(runmode==0)	{
		#Direct (Limited to US Domestic) and anything within CMAP region (even if flagged as indirect): 4 direct mode-paths in the direct path choiceset: c(3,13,31,46)
		### -- Heither, 10-15-2015: Selection Logic simplified due to enforcing Distribution channel logical consistency above -- ###
		###df[(distchannel==1 |(Production_zone<151 & Consumption_zone<151)) & Commodity_SCTG==iSCTG,c("MinGmnql","MinPath","Attribute2_ShipTime"):=minLogisticsCostSctgPaths(df[(distchannel==1 |(Production_zone<151 & Consumption_zone<151)) & Commodity_SCTG==iSCTG],iSCTG,c(3,13,31,46))]
		### Heither, 02-05-2016: send subset of data to function - Direct (Limited to US Domestic) and anything within CMAP region
		df1 <- df[(distchannel==1 |(Production_zone<151 & Consumption_zone<151)) & Commodity_SCTG==iSCTG]
		df2 <- minLogisticsCostSctgPaths(df1,iSCTG,c(3,13,31,46))		
		if(nrow(df2)>0) {if(pass==0) {
			df_out <- copy(df2)			### make an actual copy, not just a reference to df2
			pass <- pass + 1
			} else {
			df_out <- rbind(df_out,df2)
			}
		}        
		
		#Indirect and International: 50 indirect mode-paths in the path choiceset: c(1:2,4:12,14:30,32:45,47:54)
		###df[distchannel>1 & (Production_zone>150 | Consumption_zone>150) & Commodity_SCTG==iSCTG,c("MinGmnql","MinPath","Attribute2_ShipTime"):=minLogisticsCostSctgPaths(df[distchannel>1 & (Production_zone>150 | Consumption_zone>150) & Commodity_SCTG==iSCTG],iSCTG,c(1:2,4:12,14:30,32:45,47:54))]
		### Heither, 02-05-2016: send subset of data to function - Indirect and International
		df1 <- df[distchannel>1 & (Production_zone>150 | Consumption_zone>150) & Commodity_SCTG==iSCTG]
		df2 <- minLogisticsCostSctgPaths(df1,iSCTG,c(1:2,4:12,14:30,32:45,47:54))	
		if(nrow(df2)>0) {if(pass==0) {
			df_out <- copy(df2)
			pass <- pass + 1
			} else {
			df_out <- rbind(df_out,df2)
			}
		} 
	} else {
		###### Heither, 02-09-2016: Runmode!=0 - use for shipments between domestic zone and domestic port --
		df1 <- df[Commodity_SCTG==iSCTG]
		df2 <- minLogisticsCostSctgPaths(df1,iSCTG,c(4:12,14:30,32:45))
		if(nrow(df2)>0) {if(pass==0) {
			df_out <- copy(df2)
			pass <- pass + 1
			} else {
			df_out <- rbind(df_out,df2)
			}
		}
	}
   }
	##### Heither, 02-05-2016: Following line for debugging only
	#####write.table(df_out, file="E:/cmh/mode_check1.txt", row.names=F, sep=",")
    return(df_out)
}  
## ---------------------------------------------------------------

create_pmg_sample_groups <- function(naics,groups,sprod){
  
  # sort by sizes
  setkey(consc, Size)
  setkey(prodc, Size)
  #add group id and number of groups to consc and prodc; if not splitting producers assign 0
  consc[,numgroups:=groups]
  prodc[,numgroups:=groups]
  consc[,group:=1:groups]
  if(sprod==1){
    prodc[,group:=1:groups]
  } else {
    prodc[,group:=0]
  }
  #Check that for all groups the capacity > demand
  #to much demand overall?
  prodconsratio <- sum(prodc$OutputCapacityTons)/sum(consc$PurchaseAmountTons)
  if(prodconsratio < 1.1){ #TODO need to move this to variables
    #reduce consumption to ratio is >=1.1
    consc[,PurchaseAmountTons:=PurchaseAmountTons/1.1*prodconsratio]
  }
  #to much in just some groups - shuffle consumers to even out
  if(sprod==1){
    prodconsgroup <- merge(prodc[,list(OutputCapacityTons=sum(OutputCapacityTons),Producers=.N),by=group],
                           consc[,list(PurchaseAmountTons=sum(PurchaseAmountTons),Consumers=.N),by=group],by="group")
    prodconsgroup[,prodconsratio:=OutputCapacityTons/PurchaseAmountTons]
    prodconsgroup[,consexcess:=PurchaseAmountTons - OutputCapacityTons]
    
    iter <- 1 #counter to break in case something goes wrong and we get into an endless loop
    
    while (nrow(prodconsgroup[prodconsratio<1]) > 0){
      mingroup <- prodconsgroup[which.min(prodconsratio)]$group
      maxgroup <- prodconsgroup[which.max(prodconsratio)]$group
      maxgroupprod <- prodconsgroup[Producers>1][which.max(prodconsratio)]$group
      reqtomove <- prodconsgroup[mingroup]$consexcess 
      maxsample <- nrow(consc[group==mingroup]) - 1 #leave at least one consumer in the group
      if (maxsample > 0){ #move consumers to other groups
        print(paste("Moving Consumers:",mingroup,"to",maxgroup,reqtomove, maxsample))
        #create a sample frame of the first maxsample records and identify a set that is just over reqtomove
        sampsellers <- sample.int(maxsample)
        constomove <- consc[group==mingroup][sampsellers,list(BuyerID,PurchaseAmountTons)]
        constomove[,PATCum:=cumsum(PurchaseAmountTons)]
        threshold <- sum(constomove$PurchaseAmountTons)-reqtomove
        consc[BuyerID %in% constomove[PATCum>threshold]$BuyerID,group:=maxgroup]
      } else { #no consumers left to move from this group so move some producers to it -- opposite direction
        maxsampleprod <- nrow(prodc[group==maxgroupprod]) - 1 
        print(paste("Moving Producers:", maxgroupprod,"to",mingroup,reqtomove, maxsampleprod))
        #create a sample frame of the first maxsample records and identify a set that is just over reqtomove
        sampbuyers <- sample.int(maxsampleprod)
        prodstomove <- prodc[group==maxgroupprod][sampbuyers,list(SellerID,OutputCapacityTons)]
        prodstomove[,OCTCum:=cumsum(OutputCapacityTons)]
        threshold <- sum(prodstomove$OutputCapacityTons)-reqtomove
        prodc[SellerID %in% prodstomove[OCTCum>threshold]$SellerID,group:=mingroup]
      } 
      prodconsgroup <- merge(prodc[,list(OutputCapacityTons=sum(OutputCapacityTons),Producers=.N),by=group],
                             consc[,list(PurchaseAmountTons=sum(PurchaseAmountTons),Consumers=.N),by=group],by="group")
      prodconsgroup[,prodconsratio:=OutputCapacityTons/PurchaseAmountTons]
      prodconsgroup[,consexcess:=PurchaseAmountTons - OutputCapacityTons]
      
      iter <- iter + 1
      if(iter==100){#break out of the loop. This should never be necessary but here to stop endless loops. Groups with excess consumption requirements will potentially run slowly
        break
      }
    }     
  }
  #for casese where the producers are not being split allow consumers of buy all from one producer
  if(sprod==0) consc[,SingleSourceMaxFraction:=1.0]
  
}

predict_logit <- function(df,mod,cal=NULL,calcats=NULL,iter=1){
  
  #prepare the data items used in the model application and calibration
  alts <- max(mod$CHID)
  ut<-diag(alts)
  ut[upper.tri(ut)] <- 1
  if(is.numeric(df$CATEGORY)) df[,CATEGORY:=paste0("x",CATEGORY)] #numeric causes problems with column names
  cats <- unique(df$CATEGORY)
  mod<-data.table(expand.grid.df(mod,data.frame(CATEGORY=cats)))
  if(is.numeric(cal$CATEGORY)) cal[,CATEGORY:=paste0("x",CATEGORY)]
  
  for (iters in 1:iter){
    #calibration loops
    
    if(iters>1 & !is.null(cal) & !is.null(calcats)){
      
      #After first iternaton compare results with targets and calculate adjustment
      sim <- sapply(cats,function(x) tabulate(simchoice[min(df$Start[df$CATEGORY==x]):max(df$Fin[df$CATEGORY==x])],nbins=alts))
      sim <- sim/colSums(sim)
      
      if(length(unique(calcats$CHOICE))<length(unique(calcats$CHID))) {#the sim choices need to be aggregated to the calibration data
        sim <- cbind(calcats,sim)
        sim <- melt(sim,id.vars=c("CHOICE","CHID"),variable.name="CATEGORY")
        sim <- sim[,list(MODEL=sum(value)),by=list(CHOICE,CATEGORY)]
        sim <- merge(sim,cal,c("CATEGORY","CHOICE"))
        sim[,ascadj:=log(TARGET/MODEL)]
        adj <- merge(sim,calcats,"CHOICE",allow.cartesian=TRUE)[,list(CATEGORY,CHID,ascadj)]
      } 
      if(length(unique(calcats$CHOICE))>length(unique(calcats$CHID))) {#the calibratin data need to be aggregated to the sim choices
        caldat <- merge(cal[CATEGORY %in% cats],calcats,"CHOICE")
        caldat <- caldat[,list(TARGET=sum(TARGET)),by=list(CATEGORY,CHID)]
        sim <- data.table(CHID=1:nrow(sim),sim)
        sim <- melt(sim,id.vars=c("CHID"),variable.name="CATEGORY")
        sim <- merge(sim,caldat,c("CATEGORY","CHID"))
        sim[,ascadj:=log(TARGET/value)]
        adj <- sim[,list(CATEGORY,CHID,ascadj)]
      }
      if(length(unique(calcats$CHOICE))==length(unique(calcats$CHID))) {
        stop("Need to implment calibration for same calcats as choice alts")
        #####TODO add in some code here for the other case:
        #####1. same number of choices as calibration categories (so not aggregation required)
        
      }
      
      mod <- merge(mod,adj,c("CATEGORY","CHID"))
      mod[TYPE=="Constant",COEFF:=COEFF+ascadj]
      mod[,ascadj:=NULL]
    }
    
    #apply the choice model 
    ### -- Heither, 08-06-2015: the following line was modified to use the pmin function to cap values at 600 before using the exponential function to prevent INF values --##	
    ##utils <- lapply(cats, function(y) sapply(1:alts, function(x) exp(rowSums(sweep(df[CATEGORY==y,mod[CHID==x & CATEGORY==y,VAR],with=F],2,mod[CHID==x & CATEGORY==y,COEFF],"*")))))
    utils <- lapply(cats, function(y) sapply(1:alts, function(x) exp(pmin(rowSums(sweep(df[CATEGORY==y,mod[CHID==x & CATEGORY==y,VAR],with=F],2,mod[CHID==x & CATEGORY==y,COEFF],"*")),600))))
	utils <- lapply(1:length(cats),function(x) if(is.null(dim(utils[[x]]))){(utils[[x]]/sum(utils[[x]])) %*% ut} else {(utils[[x]]/rowSums(utils[[x]])) %*% ut})
    utils <- do.call("rbind",utils)
    temprand <- runif(max(df$Fin))
    simchoice <- unlist(lapply(1:nrow(df),function(x) 1L + findInterval(temprand[df$Start[x]:df$Fin[x]],utils[x,])))
  }
  return(simchoice)
}  

create_pmg_inputs <- function(naics,g,sprod){
  starttime <- proc.time()
  
  print(paste("Writing buy and sell files for ",naics,"group",g))
  
  #All consumers for this group write PMG input and create table for merging
  write.csv(consc[group==g,list(InputCommodity,BuyerID,Zone,NAICS,Size,OutputCommodity,PurchaseAmountTons,PrefWeight1_UnitCost,PrefWeight2_ShipTime,SingleSourceMaxFraction)],
            file = file.path(model$outputdir,paste0(naics, "_g", g, ".buy.csv")),row.names = FALSE)
  conscg <- consc[group==g,list(InputCommodity,NAICS,Zone,Buyer.SCTG,BuyerID,Size,ConVal,PurchaseAmountTons)]
  
  #If splitting produers, write out each group and else write all with output capacity reduced
  if(sprod==1){
    write.csv(prodc[group==g,list(OutputCommodity,SellerID,Zone,NAICS,Size,OutputCapacityTons,NonTransportUnitCost)],
              file = file.path(model$outputdir,paste0(naics, "_g", g, ".sell.csv")),row.names = FALSE)
    prodcg <- prodc[group==g,list(OutputCommodity,NAICS,Commodity_SCTG,SellerID,Size,Zone,OutputCapacityTons)]
  } else {
    #reduce capacity based on demand in this group
    consamount <- sum(conscg$PurchaseAmountTons)/sum(consc$PurchaseAmountTons)
    prodc[,OutputCapacityTonsG:= OutputCapacityTons * consamount]
    write.csv(prodc[,list(OutputCommodity,SellerID,Zone,NAICS,Size,OutputCapacityTons=OutputCapacityTonsG,NonTransportUnitCost)],
              file = file.path(model$outputdir,paste0(naics, "_g", g, ".sell.csv")),row.names = FALSE)
    prodcg <- prodc[,list(OutputCommodity,NAICS,Commodity_SCTG,SellerID,Size,Zone,OutputCapacityTons=OutputCapacityTonsG)]
  }
  
  print(paste("Applying distribution, shipment, and mode-path models to",naics,"group",g))
  # Rename ready to merge
  setnames(conscg, c("InputCommodity", "NAICS", "Zone","Size"), c("NAICS", "Buyer.NAICS", "Consumption_zone","Buyer.Size"))
  setnames(prodcg, c("OutputCommodity", "NAICS", "Zone","Size"),c("NAICS", "Seller.NAICS", "Production_zone","Seller.Size"))
  
  #merge together to create all of the pairs of firms for this group and commodity
  pc <- merge(prodcg,conscg,"NAICS", allow.cartesian = TRUE)
  pc <- pc[Production_zone<=273 | Consumption_zone<=273]	### -- Heither, 10-14-2015: potential foreign flows must have one end in U.S. so drop foreign-foreign
  
  print("Organizing data for distribution channel model")
  #Distribution size model
  #buyer/seller attributes
  
  pc[, c("emple49", "emp50t199", "empge200", "mfgind", "trwind", "whind") := 0]
  pc[Buyer.Size <= 49, emple49 := 1]
  pc[Buyer.Size >= 50 & Buyer.Size <= 199, emp50t199 := 1]
  pc[Buyer.Size >= 200, empge200 := 1]
  
  pc[,Seller.NAICS2:=substr(Seller.NAICS,1,2)]
  pc[Seller.NAICS2 %in% 31:33, mfgind := 1]
  pc[Seller.NAICS2 %in% 48:49, trwind := 1]
  pc[Seller.NAICS2 %in% c(42, 48, 49), whind := 1]
  pc[,Buyer.NAICS2:=substr(Buyer.NAICS,1,2)]
  pc[Buyer.NAICS2 %in% 31:33, mfgind := 1]
  pc[Buyer.NAICS2 %in% 48:49, trwind := 1]
  pc[Buyer.NAICS2 %in% c(42, 48, 49), whind := 1]
  
  pc[, CATEGORY := famesctg[Commodity_SCTG]]
  
  setkey(pc, Production_zone, Consumption_zone)
  pc <- merge(pc, mesozone_gcd, c("Production_zone", "Consumption_zone")) # append distances
  setnames(pc, "GCD", "Distance")
  
  print("Applying distribution channel model")
  #Apply choice model of distribution channel and iteratively adjust the ascs
  #The model estimated for mfg products was applied to all other SCTG commodities
  if (nrow(pc[Commodity_SCTG %in% c(1:9)]) > 0) {
    setkeyv(pc, c("CATEGORY", unique(distchan_food$VAR[distchan_food$TYPE == "Variable"]))) #sorted on vars, calibration coefficients, so simulated choice is ordered correctly
    pc_food <- pc[Commodity_SCTG %in% c(1:9),c("CATEGORY", unique(distchan_food$VAR[distchan_food$TYPE == "Variable"])),with=F]
    df <- pc_food[, list(Start = min(.I), Fin = max(.I)), by = eval(c("CATEGORY", unique(distchan_food$VAR[distchan_food$TYPE == "Variable"])))] #unique combinations of model coefficients
    df[, eval(unique(distchan_food$VAR[distchan_food$TYPE == "Constant"])) := 1] #add 1s for constants to each group in df
    print(paste(nrow(df), "unique combinations"))
    pc[Commodity_SCTG %in% c(1:9), distchannel := predict_logit(df, distchan_food, distchan_cal, distchan_calcats, 4)]
  }
  if (nrow(pc[!Commodity_SCTG %in% c(1:9)]) > 0) {
    setkeyv(pc, c("CATEGORY", unique(distchan_mfg$VAR[distchan_mfg$TYPE == "Variable"]))) #sorted on vars so simulated choice is ordered correctly
    pc_mfg <- pc[!Commodity_SCTG %in% c(1:9),c("CATEGORY", unique(distchan_mfg$VAR[distchan_mfg$TYPE == "Variable"])),with=F]
    df <- pc_mfg[, list(Start = min(.I), Fin = max(.I)), by = eval(c("CATEGORY", unique(distchan_mfg$VAR[distchan_mfg$TYPE == "Variable"])))] #unique combinations of model coefficients
    ####do this in the function (seems unecessary here)?
    df[, eval(unique(distchan_mfg$VAR[distchan_mfg$TYPE == "Constant"])) := 1] #add 1s for constants to each group in df
    print(paste(nrow(df), "unique combinations"))
    pc[!Commodity_SCTG %in% c(1:9), distchannel := predict_logit(df, distchan_mfg, distchan_cal, distchan_calcats, 4)]
  }
  rm(df)
  
  ### -- Heither, 10-15-2015: Enforce Distribution channel logical consistency -- ###
  pc[distchannel==1 & Production_zone %in% c(179:180) & !Consumption_zone %in% c(179:180), distchannel := 3]				### -- Hawaii origin - non-Hawaii destination
  pc[distchannel==1 & !Production_zone %in% c(179:180) & Consumption_zone %in% c(179:180), distchannel := 3]				### -- non-Hawaii origin - Hawaii destination
  pc[distchannel==1 & Production_zone<=273 & Consumption_zone>273 & !Consumption_zone %in% c(310,399), distchannel := 3]	### -- U.S. origin - foreign destination (except Canada/Mexico)
  pc[distchannel==1 & Production_zone>273 & !Production_zone %in% c(310,399) & Consumption_zone<=273, distchannel := 3]		### -- foreign origin (except Canada/Mexico) - U.S. destination
  
  print("Organizing data for shipment size model")
  # Shipment size model
  # buyer/seller attributes
  pc[, log_dist := log10(Distance + 1)]
  pc[, c("DISTCHAN", "DISTCHAN_2", "DISTCHAN_3", "SIC1", "SIC2", "SIC3") := 0]
  pc[distchannel == 1, DISTCHAN := 1]
  pc[distchannel == 2, DISTCHAN_2 := 1]
  pc[distchannel == 3, DISTCHAN_3 := 1]
  
  pc[Buyer.NAICS2 %in% as.character(70:89), SIC1 := 1]
  pc[Buyer.NAICS2 %in% c("23","48", "49"), SIC2 := 1]
  pc[Buyer.NAICS2 %in% as.character(c(21,31:45)), SIC3 := 1]
  
  #apply the model
  
  ####TODO calibration of this model needs to be weighted somehow
  ####as the target data is in terms of tonnes, not shipments and so each record should represnet
  ####the potential commodity flow
  ####pass in a vector of weights to sum over?
  
  ####it seems like the distribution channel model is also a problem here -- as each 
  ####record really represents a different number of shipments, But we don't know that at time of application?
  
  print("Applying shipment size model...")
  if (nrow(pc[Commodity_SCTG %in% c(1:9)]) > 0) {
    setkeyv(pc, c("Commodity_SCTG", unique(ShipSize_food$VAR[ShipSize_food$TYPE == "Variable"]))) #sorted on vars, calibration coefficients, so simulated choice is ordered correctly
    pc_food <- pc[Commodity_SCTG %in% c(1:9),c("Commodity_SCTG", unique(ShipSize_food$VAR[ShipSize_food$TYPE == "Variable"])),with=F]
    df <- pc_food[, list(Start = min(.I), Fin = max(.I)), by = eval(c("Commodity_SCTG", unique(ShipSize_food$VAR[ShipSize_food$TYPE == "Variable"])))] #unique combinations of model coefficients
    df[, eval(unique(ShipSize_food$VAR[ShipSize_food$TYPE == "Constant"])):=1] #add 1s for constants to each group in df
    setnames(df, "Commodity_SCTG", "CATEGORY")
    print(paste(nrow(df), "unique combinations"))
    pc[Commodity_SCTG %in% c(1:9), ship_size := predict_logit(df, ShipSize_food, ShipSize_cal, ShipSize_calcats, 4)]
  }
  if (nrow(pc[!Commodity_SCTG %in% c(1:9)]) > 0) {
    setkeyv(pc, c("Commodity_SCTG", unique(ShipSize_mfg$VAR[ShipSize_mfg$TYPE == "Variable"]))) #sorted on vars so simulated choice is ordered correctly
    pc_mfg <- pc[!Commodity_SCTG %in% c(1:9),c("Commodity_SCTG", unique(ShipSize_mfg$VAR[ShipSize_mfg$TYPE == "Variable"])),with=F]
    df <- pc_mfg[, list(Start = min(.I), Fin = max(.I)), by = eval(c("Commodity_SCTG", unique(ShipSize_mfg$VAR[ShipSize_mfg$TYPE == "Variable"])))] #unique combinations of model coefficients
    setnames(df, "Commodity_SCTG", "CATEGORY")
    ####do this in the function (seems unecessary here)?
    df[, eval(unique(ShipSize_mfg$VAR[ShipSize_mfg$TYPE == "Constant"])) := 1] #add 1s for constants to each group in df
    print(paste(nrow(df), "unique combinations"))
    pc[!Commodity_SCTG %in% c(1:9), ship_size := predict_logit(df, ShipSize_mfg, ShipSize_cal, ShipSize_calcats, 4)]
  }
  rm(df)
  
  #Simulate the actual shipment weight
  # The probabilities are based on data from the CFS and texas
  print("Simulating the actual shipment weight")
  pc[, temprand := runif(.N)]
  pc[, weight := 0L]
  #pc[, weight := as.double(weight)]
  pc[ship_size == 2, weight := c(1500L, 2500L, 3500L, 4500L, 5500L, 6500L, 7500L, 8500L, 9500L)[1 + findInterval(temprand, c(0.23, 0.44, 0.59, 0.66, 0.74, 0.79, 0.86, 0.92))]]
  ## ---------------------------------------------------------------
## Heither, revised 03-18-2016: code reflects revised weight distributions for low weight (Size 1) & Heavy weight (Size 3) shipments
  for(i in unique(pc$Commodity_SCTG)){
    pc[ship_size == 1 & Commodity_SCTG == i, weight := c(50L, 75L, 300L, 625L, 875L)[1 + findInterval(temprand, as.numeric(unlist(ship1[i])))]]
    pc[ship_size == 3 & Commodity_SCTG == i, weight := c(30000L, 75000L, 150000L)[1 + findInterval(temprand, as.numeric(unlist(ship3[i])))]]
  } 
### --  for(i in unique(pc$Commodity_SCTG)){
### --     pc[ship_size == 1 & Commodity_SCTG == i, weight := c(50L, 75L, 300L, 625L, 875L)[1 + findInterval(temprand, ShipSize_cal$TARGET[ShipSize_cal$CATEGORY == paste0("x", i) & ShipSize_cal$CHOICE == 1])]]
### --     pc[ship_size == 3 & Commodity_SCTG == i, weight := c(30000L, 75000L, 150000L)[1 + findInterval(temprand, ShipSize_cal$TARGET[ShipSize_cal$CATEGORY == paste0("x", i) & ShipSize_cal$CHOICE == 3])]]
### --   }
  ## ---------------------------------------------------------------
  #check if shipment size is > total for year, if so reduce so all in one shipment
  pc[PurchaseAmountTons < weight, weight := as.integer(round(PurchaseAmountTons))] #remember units are both pounds

  #clean up extra fields
  pc[,c("temprand", "log_dist", "Buyer.NAICS2", "Seller.NAICS2",
        "DISTCHAN", "DISTCHAN_2", "DISTCHAN_3",
        "SIC1", "SIC2", "SIC3",
        "emple49", "emp50t199", "empge200", "mfgind",
        "trwind", "whind", "CATEGORY") := NULL]
  
  #Calculate Costs and Times for Each Mode-Path Alternative
  #Each shipper-receiver pair selects one transport & logistics path for its shipping needs based on annual transport & logistics costs
  ## ---------------------------------------------------------------
  ## Heither, revised 07-22-2015
  print("Applying the mode-path choice model")
  setkey(pc, Production_zone, Consumption_zone)
  pc[, lssbd := ifelse(Seller.Size > 5 & Buyer.Size < 3 & Distance > 300, 1, 0)]
  #Add "MinGmnql","MinPath","Attribute2_ShipTime" to pc using the minLogisticsCost function
  ##Rprof("profile1.out")
  
  ## ---------------------------------------------------------------
  ## Heither, revised 02-05-2016: revised so correct modepath is reported
  df_fin <- minLogisticsCost(pc,0) 
  setnames(df_fin,c("time","path","minc"),c("Attribute2_ShipTime","MinPath","MinGmnql"))
  setkey(df_fin,SellerID,BuyerID,NAICS,Commodity_SCTG)
  setkey(pc,SellerID,BuyerID,NAICS,Commodity_SCTG)  
  pc <- pc[df_fin]
  setkey(pc,Production_zone,Consumption_zone)	### return to original sort order
  
  ## ~~~~~~~~~~~~~~~~~~~
  
  ##Rprof(NULL)
  ##profsum <- summaryRprof("profile1.out")
  pc[, Attribute1_UnitCost := MinGmnql / PurchaseAmountTons]
  pc[, Attribute2_ShipTime := Attribute2_ShipTime / 24] #Convert from hours to days

  ## ---------------------------------------------------------------
  
  #Prepare and write the cost file
  print(paste("Preparing and writing the costs file for",naics))
  
  #save the full table to an rdata file for use after the PMG game
  save(pc, file = file.path(model$outputdir,paste0(naics, "_g", g, ".Rdata")))
  
  # Clean up unneeded variables
  pc[, c("Production_zone", "Consumption_zone", "Commodity_SCTG", "NAICS","Seller.NAICS", "Seller.Size", "Buyer.NAICS",
         "Buyer.SCTG","Buyer.Size","ConVal","PurchaseAmountTons", "OutputCapacityTons",
         "weight", "Distance", "ship_size", "distchannel",
         "lssbd", "MinGmnql", "MinPath") := NULL]
  
  #For possible buyer and supplier pairs, build a file 
  #BuyerID
  #SellerID  
  #Attribute1_UnitCost  
  #Attribute2_ShipTime  
  #Attribute3_K
  #Attribute4_L  
  #Attribute5_M  
  #Tag1_SingleSource
  #Tag2_PortOfOrigin	
  #Tag3_PortOfEntry
  
  #pc[, Tag1_SingleSource := SellerID]
  #pc[, c("Attribute3_K", "Attribute4_L", "Attribute5_M") := 0]
  #pc[, c("Tag2_PortOfOrigin", "Tag3_PortOfEntry") := ""]
  
  #write out the costs file for this group
  write.csv(pc, file = file.path(model$outputdir,paste0(naics, "_g", g, ".costs.csv")), row.names = FALSE)
  
  ## Heither, revised 10-05-2015: File Cleanup if Outputs folder being re-used from previous run
  ## -- Delete NAICS_gX.out file if exists from prior run (existence will prevent PMG from running)
  if(file.exists(file.path(outputdir,paste0(naics,"_g", g, ".out.csv")))){
		file.remove(file.path(outputdir,paste0(naics,"_g", g, ".out.csv")))
  }
  ## -- Delete NAICS_gX.txt file if exists from prior run as well
  if(file.exists(file.path(outputdir,paste0(naics,"_g", g, ".txt")))){
		file.remove(file.path(outputdir,paste0(naics,"_g", g, ".txt")))
  } 
  
  
  rm(pc,prodcg,conscg)
  gc()
  
  print(paste("Complete for", naics, "group", g, "in", (proc.time() - starttime)[3]))
}

#save current workspace for use by seperate R script processes for running NAICS groups
save.image(file = file.path(model$outputdir,"PMG_Workspace.Rdata"))

pmg <- progressEnd(pmg)

