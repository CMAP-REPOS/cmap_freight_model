##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       1_Firmsynthesis.R produces a synthetic population of firms. From those
#                   firms a sample are used for simulation as producers and consumers                   
#Date:              January 28, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 1 Firm Synthesis
#-----------------------------------------------------------------------------------
progressStart(firmsyn,9)

#-----------------------------------------------------------------------------------
#Enumerating firms and merge with correspondenses
#-----------------------------------------------------------------------------------
progressNextStep("Enumerating Firms")

#Aggregate the employment data by zones, NAICS, and firm size category
#1='1-19',2='20-99',3='100-249',4='250-499',5='500-999',6='1,000-2,499',7='2,500-4,999',8='Over 5,000'
cbp <- cbp[!is.na(CBPZONE) & !is.na(FAFZONE) & !is.na(Industry_NAICS6_CBP),
           list(e1=sum(e1),e2=sum(e2),e3=sum(e3),e4=sum(e4),e5=sum(e5),e6=sum(e6),e7=sum(e7),e8=sum(e8)),
           by=list(Industry_NAICS6_CBP,CBPZONE,FAFZONE)] #Remove records with missing zones and NAICS codes
setkey(c_n6_n6io_sctg,Industry_NAICS6_CBP)
cbp <- merge(cbp,c_n6_n6io_sctg[,list(Industry_NAICS6_CBP,Industry_NAICS6_Make,Commodity_SCTG)],by="Industry_NAICS6_CBP") #Merge in the I/O NAICS codes and SCTG codes (Remove a few businesses with unknown naics codes from InfoUSA data)
cbp[,c("n2","n4"):=list(substr(Industry_NAICS6_CBP,1,2),substr(Industry_NAICS6_CBP,1,4))] #add 2 and 4 digit NAICS
cbp <- melt(cbp, measure.vars=paste0("e",1:8),variable.name ="esizecat",value.name="est") #Melt to create separate rows for each firm size category
cbp[,esizecat:=as.integer(esizecat)] #convert esizecat to an integer (1:8)
cbp[,Emp:= c(10L,60L,175L,375L,750L,1750L,3750L,7500L)[esizecat]] # Estimate the number of employees
cbp <- cbp[rep(seq_len(cbp[,.N]),est),] #Enumerates the agent businesses using the est variable.
cbp[,BusID:=.I] #Add an ID

#-----------------------------------------------------------------------------------
# Allocating specific commodity and location for each establishment
#-----------------------------------------------------------------------------------
progressNextStep("Allocating commodities and locations to establishments")

#TODO: are these still appropriate correspodences given the new I/O data?
#This section identifies producers who make 2+ commodities (especially wholesalers) and
#simulates a specific commodity for them based on probability thresholds for multiple commodities
set.seed(151)
cbp[,temprand:=runif(.N)]
#For all the NAICS which may produce more than one SCTG commodity, simulate one SCTG commodity using set probability thresholds
setkey(cbp,Industry_NAICS6_CBP)
cbp[list(211111),Commodity_SCTG:=c(16L,19L)[1+findInterval(temprand,c(0.45))]]
cbp[list(324110),Commodity_SCTG:=c(17L,18L,19L)[1+findInterval(temprand,c(0.25,0.50))]]

setkey(cbp,n4)
cbp["4245",Commodity_SCTG:=c(1L,2L,3L,4L)[1+findInterval(temprand,c(0.25,0.50,0.75))]] #Farm Product Raw Material Merchant Wholesalers
cbp["4244",Commodity_SCTG:=c(5L,6L,7L,9L)[1+findInterval(temprand,c(0.25,0.50,0.75))]] #Grocery and Related Product Wholesalers
cbp["4248",Commodity_SCTG:=8L] #Beer, Wine, and Distilled Alcoholic Beverage Merchant Wholesalers
cbp["4233",Commodity_SCTG:=c(10L,11L,12L,25L,26L)[1+findInterval(temprand,c(0.10,0.20,0.80,0.90))]] #Lumber and Other Construction Materials Merchant Wholesalers
cbp["4235",Commodity_SCTG:=c(13L,14L,31L,32L)[1+findInterval(temprand,c(0.25,0.50,0.75))]] #Metal and Mineral (except Petroleum) Merchant Wholesalers
cbp["4237",Commodity_SCTG:=c(15L,33L)[1+findInterval(temprand,c(0.50))]] #Hardware, and Plumbing and Heating Equipment and Supplies Merchant Wholesalers
cbp["4247",Commodity_SCTG:=c(16L,17L,18L,19L)[1+findInterval(temprand,c(0.25,0.50,0.75))]] #Petroleum and Petroleum Products Merchant Wholesalers
cbp["4246",Commodity_SCTG:=c(20L,21L,22L,23L)[1+findInterval(temprand,c(0.25,0.50,0.75))]] #Chemical and Allied Products Merchant Wholesalers
cbp["4242",Commodity_SCTG:=21L] #Drugs and Druggists Sundries Merchant Wholesalers
cbp["4234",Commodity_SCTG:=24L] #Professional and Commercial Equipment and Supplies Merchant Wholesalers
cbp["4241",Commodity_SCTG:=c(27L,28L,29L)[1+findInterval(temprand,c(0.33,0.67))]] #Paper and Paper Product Merchant Wholesalers 
cbp["4243",Commodity_SCTG:=30L] #Apparel, Piece Goods, and Notions Merchant Wholesalers
cbp["4238",Commodity_SCTG:=34L] #Machinery, Equipment, and Supplies Merchant Wholesalers
cbp["4251",Commodity_SCTG:=c(35L,38L)[1+findInterval(temprand,c(0.50))]] #Wholesale Electronic Markets and Agents and Brokers
cbp["4236",Commodity_SCTG:=c(35L,38L)[1+findInterval(temprand,c(0.50))]] #Electrical and Electronic Goods Merchant Wholesalers
cbp["4231",Commodity_SCTG:=c(36L,37L)[1+findInterval(temprand,c(0.50))]] #Motor Vehicle and Motor Vehicle Parts and Supplies Merchant Wholesalers
cbp["4232",Commodity_SCTG:=39L] #Furniture and Home Furnishing Merchant Wholesalers
cbp["4239",Commodity_SCTG:=40L] #Miscellaneous Durable Goods Merchant Wholesalers
cbp["4249",Commodity_SCTG:=40L] #Miscellaneous Nondurable Goods Merchant Wholesalers
cbp[n2=="42",Industry_NAICS6_Make:=paste0(n4,"00")]

#Assign firms which from CBPZones (counties) to model MESOZONES that are smaller in size -- more like cities
cbpc <- cbp[CBPZONE > 999,list(CBPZONE,BusID,n2,Emp)]
#Assign specific NAICS categories which would be used to locate businesses to tazs
cbpc[n2 %in% c("31","32","33"),n2:="3133"]
cbpc[n2 %in% c("44","45"),n2:="4445"]
cbpc[n2 %in% c("48","49"),n2:="4849"]
cbpc[n2 %in% c("S0"),n2:="92"]

mzemp <- data.table(melt(mzemp, id.vars=c("COUNTY", "MESOZONE")))
mzemp[,n2:=sub("rank","",as.character(variable))]
setnames(mzemp,c("COUNTY","value"),c("CBPZONE","EmpRank"))
cbpc <- merge(cbpc,mzemp,c("CBPZONE","n2"),allow.cartesian=TRUE,all.x=TRUE) #Merge the rankings dataset to the firms database based on county
#Select candidate tazs based on the industry of the firm, firm size, and ranking of that particular industry in a taz
cbpc[,candidate:=0L]
cbpc[Emp>5000 & EmpRank %in% c(9,10),candidate:=1L]
cbpc[Emp>2000 & Emp<=5000 & EmpRank %in% c(7:10),candidate:=1L]
cbpc[Emp>500 & Emp<=2000 & EmpRank %in% c(5:10),candidate:=1L]
cbpc[Emp>100 & Emp<=500 & EmpRank %in% c(4:10),candidate:=1L]
cbpc[Emp>20 & Emp<=100 & EmpRank %in% c(2:10),candidate:=1L]
cbpc[Emp<=20 & EmpRank %in% c(1:10), candidate:=1L]
#small number of businesses that did not get a candiate TAZ - allow those to have some candidates (small error is better than omitting the businesses)
ZeroCand <- cbpc[,sum(candidate),by=BusID][V1==0]
cbpc[BusID %in% ZeroCand$BusID,candidate:=1L]
cbpc <- cbpc[candidate==1,] #remove non-candidate TAZs
cbpc[,u:=runif(.N)] #Generate a random number based on which one of the candidate tazs would be selected
cbpc <- cbpc[cbpc[,.I[which.max(u)],by=BusID]$V1,] #Assign the taz for which the random number generated is the highest among all candidate tazs
#Assign MESOZONES for all firms
cbp[CBPZONE <= 123, MESOZONE:= CBPZONE + 150L]
cbp[CBPZONE %in% 801:808, MESOZONE:= CBPZONE]
setkey(cbp,BusID)
setkey(cbpc,BusID)
cbp[CBPZONE >999,MESOZONE:=cbpc$MESOZONE]
#Cleanup
rm(mzemp,ZeroCand,cbpc)
cbp[,c("Industry_NAICS6_CBP","n2","n4","est","temprand"):=NULL] #Revome extra fields,

#save warehouse list for use in truck touring model
#NAICS 481 air, 482 rail, 483 water, 493 warehouse and storage 
warehouses <- cbp[MESOZONE<150,][substr(Industry_NAICS6_Make,1,3) %in% c(481,482,483,493)]
save(warehouses,file=file.path(model$outputdir,"warehouses.Rdata"))

#-----------------------------------------------------------------------------------
#Create producers/suppliers database
#-----------------------------------------------------------------------------------
progressNextStep("Creating Producers Database")

# Creation of Producers database
# All agents that produce some SCTG commodity become potential producers
# wholesales are dealt with separately below
producers <- cbp[Commodity_SCTG>0 & substr(Industry_NAICS6_Make,1,2) != "42",] 

# Create a table of production values and employment by NAICS code to calculate production value

#TODO change to use melt.data.table here to simplify
setnames(io, names(io),sub("X", "", names(io))) # Strip out the leading 'X' in the column names
io <- data.table(melt(io,id.vars="Industry_NAICS6_MakeUse")) # Melt into long format and turn back into a data.table
setnames(io, c("Industry_NAICS6_MakeUse", "variable", "value"), c("Industry_NAICS6_Make","Industry_NAICS6_Use","ProVal"))
io[,Industry_NAICS6_Use:=as.character(Industry_NAICS6_Use)]
io_sum <- copy(io) #keep for summaries

#Wholesalers: grouped into 42000 in IO tables, but 6 digit NAICS codes in employment data
#Distribute the IO table production and consumption
towhl <- io[substr(Industry_NAICS6_Use,1,2)=="42" & ProVal > 0 & Industry_NAICS6_Make %in% producers$Industry_NAICS6_Make]
sctgwhl <- data.table(SCTG=1:40,
                      NAICS_whl=c(rep("424500",4),rep("424400",3),"424800","424400",rep("423300",3),rep("423500",2),"423700",
                      rep("424700",4),"424600","424200",rep("424600",2),"423400",rep("423300",2),rep("424100",3),
                      "424300",rep("423500",2),"423700","423800","425100",rep("423100",2),"425100","423200","423900"))
#TODO note that "424900" also SCTG=40
sctgwhl <- merge(sctgwhl,unique(c_n6_n6io_sctg[,list(SCTG=Commodity_SCTG,Industry_NAICS6_Make,Proportion)]),by="SCTG")
sctgwhl <- merge(sctgwhl,towhl[,list(Industry_NAICS6_Make,ProVal)],by="Industry_NAICS6_Make")
sctgwhl[,ProVal:=ProVal*Proportion]

fromwhl <- io[substr(Industry_NAICS6_Make,1,2)=="42" & ProVal > 0]
setnames(fromwhl,"ProVal","ProValFromWhl")
fromwhl <- merge(fromwhl[,list(Industry_NAICS6_Use, ProValFromWhl)],
                 io[ProVal > 0],by="Industry_NAICS6_Use",all.x=TRUE)
setnames(fromwhl,"ProVal","ProValUse")
#which of those commodities can actually be sourced from wholesalers?
#TODO improve the naming here to be more explicit (e.g., fromwhl not a good name)
fromwhl <- merge(fromwhl,sctgwhl,by="Industry_NAICS6_Make",allow.cartesian = TRUE)
fromwhl[,ProValUse:=ProValUse*Proportion]
fromwhl[,ProValPctUse:=ProValUse/sum(ProValUse),by=Industry_NAICS6_Use]

#allocate out the ProValFromWhl by weighting both input to wholesalers
#first need to scale production amount to consumption
whlcons<-sum(unique(fromwhl[,list(Industry_NAICS6_Use,ProValFromWhl)])$ProValFromWhl)
whlprod<-sum(unique(fromwhl[,list(Industry_NAICS6_Make,ProVal)])$ProVal)
fromwhl[,ProValFact:=ProVal*whlcons/whlprod]

#matrix of producers to consumers
#row totals are production amounts
#column totals are consumption amounts
#initial cell values are the proportion of consumption that comes from each producers by industry
#Then need to IPF to adjust cell values such that row and column totals are conserved
fromwhl[,CellValue:=ProValFact*ProValPctUse]

for(i in 1:10){
  fromwhl[,ColTotal:=sum(CellValue),by=Industry_NAICS6_Use]
  fromwhl[,ColWeight:=ProValFromWhl/ColTotal]
  fromwhl[,CellValue:=CellValue*ColWeight]
  fromwhl[,RowTotal:=sum(CellValue),by=Industry_NAICS6_Make]
  fromwhl[,RowWeight:=ProValFact/RowTotal]
  fromwhl[,CellValue:=CellValue*RowWeight]
}

fromwhl[,CellValue:=round(CellValue)]
fromwhl <- fromwhl[CellValue>0,list(Industry_NAICS6_Make, Industry_NAICS6_Use,SCTG, NAICS_whl, ProValWhl=CellValue)]
iowhl <- fromwhl[,list(ProValWhl=sum(ProValWhl)),by=list(Industry_NAICS6_Make, Industry_NAICS6_Use)]

#remove wholesale records from io table
io <- io[substr(Industry_NAICS6_Use,1,2) !="42" & substr(Industry_NAICS6_Make,1,2) !="42"]

#replace wholesale records by adding the make-use value back to the io table 
#as if it was direct from producer to consumer and not via whl
#so that the production amounts and consumption amounts will be correct
io <- merge(io,iowhl,by=c("Industry_NAICS6_Make", "Industry_NAICS6_Use"),all.x=TRUE)
io[is.na(ProValWhl),ProValWhl:=0]
io[,ProVal:=ProVal+ProValWhl]
io[,ProValWhl:=NULL]

#add the wholesales with the correct capacities in value and tons 
#to both producer and consumer tables
wholesalers <- cbp[substr(Industry_NAICS6_Make,1,2) == "42",] 
whlval <- fromwhl[,list(ProVal=sum(ProValWhl)),by=NAICS_whl]
setnames(whlval,"NAICS_whl","Industry_NAICS6_Make")
whlval <- merge(whlval,wholesalers[,list(Emp=sum(Emp)), by=Industry_NAICS6_Make],"Industry_NAICS6_Make")
whlval[,ValEmp:=ProVal/Emp] #production value per employee
wholesalers <- merge(whlval[,list(Industry_NAICS6_Make,ValEmp)], wholesalers, "Industry_NAICS6_Make") #merge the value per employee back on to businesses
wholesalers[,ProdVal:=Emp*ValEmp] #calculate production value for each establishment

#issues - need to be marked as a wholesales using the NAICS code
#but need to be tagged with the correct make/use commodity seperate from their NAICS code
#this should be easy for the consumer side, check possible for the producers side

prodval <- merge(io[,list(ProVal=sum(ProVal)),by=Industry_NAICS6_Make],producers[,list(Emp=sum(Emp)), by=Industry_NAICS6_Make],"Industry_NAICS6_Make")
prodval[,ValEmp:=ProVal/Emp] #production value per employee (in Million of Dollars)
producers <- merge(prodval[,list(Industry_NAICS6_Make,ValEmp)], producers, "Industry_NAICS6_Make") #merge the value per employee back on to producers
producers[,ProdVal:=Emp*ValEmp] #calculate production value for each establishment (in Million of Dollars)

# Add foreign producers - one agent per country per commodity
# add in the NAICS_Make code, group, and calculate employment requirements to support that production
setnames(for_prod,"Commodity_NAICS6","Industry_NAICS6_CBP")
for_prod <- merge(for_prod,c_n6_n6io_sctg[,list(Industry_NAICS6_CBP,Industry_NAICS6_Make,Commodity_SCTG)],by="Industry_NAICS6_CBP") #Merge in the I/O NAICS codes and SCTG codes
for_prod <- for_prod[Commodity_SCTG > 0,list(ProdVal=sum(USImpVal)/1000000),by=list(Industry_NAICS6_Make,CBPZONE,FAFZONE,Commodity_SCTG)]
for_prod <- merge(for_prod,prodval[,list(Industry_NAICS6_Make,ValEmp)],by="Industry_NAICS6_Make",all.x=TRUE)
#TODO check on these missing commodities/commodites without value in the IO table -- should there be US production too? should it have value?
for_prod <- for_prod[!is.na(ValEmp) & ValEmp!=0]
#estimate employment and size category
#update ValEmp using foreign producer adjustment
for_prod[,ValEmp:= ValEmp * model$scenvars$foreignprodcostfactor] #same adjustment applied to unitcost, so assumption is that quantity per employee is the same as domestic production
for_prod[,Emp:=round(ProdVal/ValEmp)]
for_prod[,esizecat:=findInterval(Emp,c(0,20,100,250,500,1000,2500,5000))]
#producers' output in POUNDS (units costs converted to pounds)
unitcost[,UnitCost:=UnitCost/2000]
for_prod <- merge(for_prod, unitcost, "Commodity_SCTG")
#update unit cost using foreign producer adjustment
for_prod[,UnitCost:=UnitCost * model$scenvars$foreignprodcostfactor]
for_prod[,ProdCap:=ProdVal*1000000/UnitCost] # ProdVal was in $M
### -------------------------------------------------------------------------------------
## -- Heither, 03-11-2016: enumerate large foreign consumers into multiple firms (threshold reduced)
for_prod[ProdCap<=500000000, est:=1]
for_prod[ProdCap>500000000, est:=ceiling(ProdCap/500000000)]		## number of fims to create
for_prod[est>1,ProdVal:=ProdVal/est]								## update ProdVal for multiple firms
for_prod[est>1,ProdCap:=ProdCap/est]								## update ProdCap for multiple firms
for_prod <- for_prod[rep(seq_len(for_prod[,.N]),est),]				## Enumerates the foreign producers using the est variable.
for_prod[,est:=NULL]
### -------------------------------------------------------------------------------------

# calculate other fields required in producers tables
for_prod[,MESOZONE:= CBPZONE + 150L]
for_prod[,BusID:= max(cbp$BusID) + .I]

# Calculate producers' output in POUNDS (units costs converted to pounds)
producers <- merge(producers, unitcost, "Commodity_SCTG")
producers[,ProdCap:=ProdVal*1000000/UnitCost] # ProdVal was in $M
# Calculate wholesalers output in POUNDS
wholesalers <- merge(wholesalers, unitcost, "Commodity_SCTG")
# factor up unitcost to reflect wholesalers margin
wholesalers[,UnitCost:=UnitCost*model$scenvars$wholesalecostfactor]
wholesalers[,ProdCap:=ProdVal*1000000/UnitCost] # ProdVal was in $M

# combine domestic and foreign producers
producers <- rbind(producers,for_prod,use.names=TRUE)
rm(for_prod)

# Prepare for Writing out a producers file for each NAICS, with each firm represented by:
# SellerID (BusID)  Zone (MESOZONE)	NAICS (NAICS6_Make)	Size (Emp)	OutputCommodity (SCTG_Make)	OutputCapacityTons (ProdCap)	NonTransportUnitCost (UnitCost)
producers[,c("FAFZONE","CBPZONE","esizecat","ProdVal","ValEmp"):=NULL]
setnames(producers,c("BusID","MESOZONE","Industry_NAICS6_Make","Emp","ProdCap","UnitCost"),c("SellerID","Zone","NAICS","Size","OutputCapacityTons","NonTransportUnitCost"))
producers[,OutputCommodity:=NAICS]

#Add in wholesalers to producers
wholesalers[,c("FAFZONE","CBPZONE","esizecat","ProdVal","ValEmp"):=NULL]
setnames(wholesalers,c("BusID","MESOZONE","Industry_NAICS6_Make","Emp","ProdCap","UnitCost"),c("SellerID","Zone","NAICS","Size","OutputCapacityTons","NonTransportUnitCost"))
#simulate the single specific NAICS commodity that the wholesaler deals in to simplify
#(wholesale NAICS are one to many NAICS commodities)
#each wholesale firm is identified with a specific NAICS and SCTG
#need probabilities for the match with NAICS commodity
whlnaics <- fromwhl[,list(ProValWhl=sum(ProValWhl)),by=list(Industry_NAICS6_Make,SCTG,NAICS=NAICS_whl)]
whlnaics[,ProbProValWhl:=ProValWhl/sum(ProValWhl),by=list(SCTG,NAICS)]
setkey(whlnaics,NAICS,SCTG)
whlnaics[,CumProValWhl:=cumsum(ProbProValWhl),by=list(SCTG,NAICS)]
whlnaicscombs <- unique(whlnaics[,list(NAICS,SCTG)])
wholesalers[,temprand:=runif(.N)]
for(i in 1:nrow(whlnaicscombs)){
  whlnaicsi <- whlnaics[NAICS==whlnaicscombs$NAICS[i] & SCTG==whlnaicscombs$SCTG[i]]
  wholesalers[NAICS==whlnaicscombs$NAICS[i] & Commodity_SCTG==whlnaicscombs$SCTG[i],
              OutputCommodity:=whlnaicsi$Industry_NAICS6_Make[1+findInterval(temprand,whlnaicsi$CumProValWhl)]]
}
#TODO - clean up correspondences to avoid no matches here
wholesalers[,temprand:=NULL]
wholesalers <- wholesalers[!is.na(OutputCommodity)]

producers <- rbind(producers,wholesalers)
setkey(producers,OutputCommodity)
#writing out done below once sampling identified

#-----------------------------------------------------------------------------------
#Create consumers database
#-----------------------------------------------------------------------------------
progressNextStep("Creating Consumers Database")

#For each firm generate a list of input commodities that need to be purchased (commodity code, amount)
io <- io[Industry_NAICS6_Make %in% unique(producers$NAICS)] #focus on just producers of transported commodities
setkey(io,Industry_NAICS6_Use,ProVal) #sort on NAICS_USe, ProVal
io[,CumPctProVal:=cumsum(ProVal)/sum(ProVal), by=Industry_NAICS6_Use] #cumulative pct value of the consumption inputs
io <- io[CumPctProVal > 1 - model$scenvars$provalthreshold,] #select suppliers including the first above the threshold value
#Calcuate value per employee required
setnames(cbp,"Industry_NAICS6_Make","Industry_NAICS6_Use")  #In the consumers table Use code is that of the consuming firm
emp<- cbp[,list(Emp=sum(Emp)), by=Industry_NAICS6_Use]
io <- merge(io,emp,"Industry_NAICS6_Use")
rm(emp)
io[,ValEmp:=ProVal/Emp]

#Merge top k% suppliers with establishment list to create a consumers\buyers dataset
consumers <- merge(io[,list(Industry_NAICS6_Use,Industry_NAICS6_Make,ValEmp)], cbp[,list(MESOZONE,Industry_NAICS6_Use,Commodity_SCTG,BusID,Emp)], "Industry_NAICS6_Use", allow.cartesian = TRUE)
setnames(consumers,"Commodity_SCTG","Buyer.SCTG")
consumers <- merge(c_n6_n6io_sctg[!duplicated(Industry_NAICS6_Make),list(Industry_NAICS6_Make, Commodity_SCTG)], consumers,"Industry_NAICS6_Make") #merge in the first matching SCTG code

#Some Naics6-Make industries (NAICS6_Make) make more than one SCTG. 
#Account for this by simulating the SCTG commodity supplied by them based on probability thresholds
mult_n6make <- unique(c_n6_n6io_sctg[Commodity_SCTG>0 & Proportion<1,list(Industry_NAICS6_Make,Commodity_SCTG,Proportion)])
setkey(consumers,Industry_NAICS6_Make)
n6m_samp <- consumers[unique(mult_n6make$Industry_NAICS6_Make)][,.N,by=Industry_NAICS6_Make]

assign_mult_sctg <- function(n6m){
  sample(mult_n6make$Commodity_SCTG[mult_n6make$Industry_NAICS6_Make==n6m],
         n6m_samp$N[n6m_samp$Industry_NAICS6_Make==n6m],
         replace = TRUE,
         prob = mult_n6make$Proportion[mult_n6make$Industry_NAICS6_Make==n6m])
}

for (i in 1:nrow(n6m_samp)){
  consumers[n6m_samp$Industry_NAICS6_Make[i],Commodity_SCTG:=assign_mult_sctg(n6m_samp$Industry_NAICS6_Make[i])]
}

rm(mult_n6make,n6m_samp)

# Calculate the purchase amount and convert to tons needed - this is production value
consumers[,ConVal:=ValEmp*Emp]
# Convert purchase value from $M to POUNDS
consumers[,UnitCost:=unitcost$UnitCost[match(Commodity_SCTG,unitcost$Commodity_SCTG)]]
consumers[,ConVal:=ConVal*1000000] # Value was in $M
consumers[,PurchaseAmountTons:=ConVal / UnitCost] 
consumers[,c("ValEmp","UnitCost"):= NULL] # Remove extra fields

#Add foreign consumers to the domestic consumption
#In this case we know US Export Value by commodity and country
#We need to synthesize the types of firms that are buying it (by industry)
#Use the IO data to indicate the industry types that consume the exported commodities
setnames(for_cons,"Commodity_NAICS6","Industry_NAICS6_CBP")
for_cons <- merge(for_cons,c_n6_n6io_sctg[,list(Industry_NAICS6_CBP,Industry_NAICS6_Make,Commodity_SCTG)],by="Industry_NAICS6_CBP") #Merge in the I/O NAICS codes and SCTG codes
for_cons <- for_cons[Commodity_SCTG > 0,list(ProdVal=sum(USExpVal)/1000000),by=list(Industry_NAICS6_Make,CBPZONE,FAFZONE,Commodity_SCTG)]
io[,PctProVal:=ProVal/sum(ProVal),by=Industry_NAICS6_Make]
setkey(io, Industry_NAICS6_Make)
for_cons <- merge(for_cons,io[,list(Industry_NAICS6_Make,Industry_NAICS6_Use, ProVal,PctProVal)],by="Industry_NAICS6_Make",allow.cartesian = TRUE)
for_cons[,ConVal:=ProdVal*PctProVal]
# Convert purchase value from $M to POUNDS
for_cons[,UnitCost:=unitcost$UnitCost[match(Commodity_SCTG,unitcost$Commodity_SCTG)]]
for_cons[,ConVal:=ConVal*1000000] # Value was in $M
for_cons[,PurchaseAmountTons:=ConVal / UnitCost] 
### -------------------------------------------------------------------------------------
## -- Heither, 03-11-2016: enumerate large foreign consumers into multiple firms (threshold reduced)
for_cons[PurchaseAmountTons<=500000000, est:=1]
for_cons[PurchaseAmountTons>500000000, est:=ceiling(PurchaseAmountTons/500000000)]		## number of fims to create
for_cons[est>1,ProdVal:=ProdVal/est]														## update ProdVal for multiple firms
for_cons[est>1,ConVal:=ConVal/est]															## update ConVal for multiple firms
for_cons[est>1,PurchaseAmountTons:=PurchaseAmountTons/est]									## update PurchaseAmountTons for multiple firms
for_cons <- for_cons[rep(seq_len(for_cons[,.N]),est),]										## Enumerates the foreign producers using the est variable.
for_cons[,est:=NULL]
### -------------------------------------------------------------------------------------

# calculate other fields required in producers tables, clean table, and rbind with consumers
for_cons[,MESOZONE:= CBPZONE + 150L]
for_cons[,BusID:= max(producers$SellerID) + .I] #add foreign consumers on after the foreign producers
for_cons[,Buyer.SCTG:=0L] #don't know buyer industry
for_cons[,Emp:=0L] #don't know employment of buying firm
for_cons[,c("CBPZONE","FAFZONE","ProdVal","ProVal","PctProVal", "UnitCost"):= NULL] # Remove extra fields
consumers <- rbind(consumers,for_cons,use.names=TRUE)

# Add preference weights
setkey(prefweights,Commodity_SCTG)
consumers <- merge(consumers, prefweights[,list(Commodity_SCTG,CostWeight,TimeWeight,SingleSourceMaxFraction)],"Commodity_SCTG")
#consumers[,c("PrefWeight3_AttributeK","PrefWeight4_AttributeL","PrefWeight5_AttributeM","MaxFrac1_SingleSource","MaxFrac2_PortOfOrigin","MaxFrac3_PortOfEntry"):=list(0,0,0,0.8,1.0,1.0)]

# Prepare for writing a file of each NAICS containing the firms that use those goods as inputs (done once sampling figured out below)
#BuyerID (BusID)  Zone (MESOZONE)	NAICS (NAICS6_Use)	Size (Emp)	OutputCommodity (SCTG_Make)	InputCommodity (SCTG_Use)	PurchaseAmountTons	PrefWeight1_UnitCost (CostWeight)	PrefWeight2_ShipTime (TimeWeight)	PrefWeight3_AttributeK	PrefWeight4_AttributeL	PrefWeight5_AttributeM	MaxFrac1_SingleSource	MaxFrac2_PortOfOrigin	MaxFrac3_PortOfEntry
setnames(consumers,c("BusID","MESOZONE","Industry_NAICS6_Make","Industry_NAICS6_Use","Emp","CostWeight","TimeWeight"),c("BuyerID","Zone","InputCommodity","NAICS","Size","PrefWeight1_UnitCost","PrefWeight2_ShipTime"))
consumers[,OutputCommodity:=NAICS]

#Add wholesalers to the consumers
wholesalers[,ConVal:=OutputCapacityTons*NonTransportUnitCost/model$scenvars$wholesalecostfactor]
wholesalers[,NonTransportUnitCost:=NULL]
wholesalers <- merge(wholesalers, prefweights[,list(Commodity_SCTG,CostWeight,TimeWeight,SingleSourceMaxFraction)],"Commodity_SCTG")
setnames(wholesalers,c("SellerID","OutputCommodity","OutputCapacityTons","CostWeight","TimeWeight"),
         c("BuyerID","InputCommodity","PurchaseAmountTons","PrefWeight1_UnitCost","PrefWeight2_ShipTime"))
wholesalers[,Buyer.SCTG:=Commodity_SCTG]
wholesalers[,OutputCommodity:=InputCommodity]
consumers <- rbind(consumers,wholesalers,use.names=TRUE)

setkey(consumers,InputCommodity)

#-----------------------------------------------------------------------------------
# Output Summaries of Consumers and Producers Databases
#-----------------------------------------------------------------------------------
progressNextStep("Producing Consumers and Producers Summaries")

#output summaries -- add this to the output functions to replace old ones
sctgcat <- data.table(Commodity_SCTG = prefweights$Commodity_SCTG, SCTG_Name = prefweights$Commodity_SCTG_desc)
#Sumamrize CBP, Producers and Consumers
firms_sum <- list()

#CBP
firms_sum[["firms"]] <- nrow(cbp)
firms_sum[["employment"]] <- sum(cbp$Emp)
firms_sum[["firmsempbysctg"]] <- merge(sctgcat,cbp[,list(Establishments=.N,Employment=sum(Emp)),by=Commodity_SCTG],"Commodity_SCTG",all.y=T)

#IO
firms_sum[["total_value"]] <- sum(io$ProVal)
firms_sum[["Industry_NAICS_Make"]] <- length(unique(io$Industry_NAICS6_Make))
firms_sum[["Industry_NAICS_Use"]] <- length(unique(io$Industry_NAICS6_Use))

#producers
firms_sum[["producers"]] <- nrow(producers)
firms_sum[["producers_emp"]] <- sum(producers$Size)
firms_sum[["producers_cap"]] <- sum(producers$OutputCapacityTons)
producers_summary <- merge(sctgcat,producers[,list(Producers=.N,Employment=sum(Size),OutputCapacity=sum(OutputCapacityTons)),by=Commodity_SCTG],"Commodity_SCTG",all.y=T)
firms_sum[["producersempbysctg"]] <- producers_summary
producers_domfor <- producers[,list(Producers=.N,Employment=sum(Size),OutputCapacity=sum(OutputCapacityTons)),by=Zone]
producers_domfor[,DomFor:=ifelse(Zone <= 273,"Domestic","Foreign")]
producers_domfor <- producers_domfor[,list(Producers=sum(Producers),Employment=sum(Employment),OutputCapacity=sum(OutputCapacity)),by=DomFor]
firms_sum[["producersdomfor"]] <- producers_domfor

#consumers
firms_sum[["consumers"]] <- length(unique(consumers$BuyerID))
firms_sum[["consumption_pairs"]] <- nrow(consumers)
firms_sum[["threshold"]] <- model$scenvars$provalthreshold
firms_sum[["consumer_inputs"]] <- sum(consumers$PurchaseAmountTons)
consumers_summary <- merge(sctgcat,consumers[,list(Consumers=.N,InputRequirements=sum(PurchaseAmountTons)),by=Commodity_SCTG],"Commodity_SCTG",all.y=T)
firms_sum[["consumersbysctg"]] <- consumers_summary
consumers_domfor <- consumers[,list(Consumers=.N,Employment=sum(Size),ConsumptionValue=sum(ConVal),InputRequirements=sum(PurchaseAmountTons)),by=Zone]
consumers_domfor[,DomFor:=ifelse(Zone <= 273,"Domestic","Foreign")]
consumers_domfor <- consumers_domfor[,list(Consumers=sum(Consumers),ConsumptionValue=sum(ConsumptionValue),InputRequirements=sum(InputRequirements)),by=DomFor]
firms_sum[["consumers_domfor"]] <- consumers_domfor

#matching consumers and suppliers -- by SCTG category
setnames(producers_summary,"Commodity_SCTG","Commodity")
setnames(consumers_summary,"Commodity_SCTG","Commodity")
match_summary <- merge(producers_summary[,list(Commodity,SCTG_Name,Producers,OutputCapacity)],
                                consumers_summary[,list(Commodity,Consumers,InputRequirements)],
                                "Commodity")
setcolorder(match_summary,c("Commodity","SCTG_Name","Producers","Consumers","OutputCapacity","InputRequirements"))
match_summary[,Ratio_OutputInput:=OutputCapacity/InputRequirements]
match_summary[,Possible_Matches:=as.numeric(Producers) * as.numeric(Consumers)]
firms_sum[["matches"]] <- match_summary

#matching consumers and suppliers -- by NAICS codes
producers_summary_naics <- producers[,list(Producers=.N,Employment=sum(Size),OutputCapacity=sum(OutputCapacityTons)),by=OutputCommodity]
firms_sum[["producersempbynaics"]] <- data.frame(producers_summary_naics) #so it prints all rows
consumers_summary_naics <- consumers[,list(Consumers=.N,Employment=sum(Size),InputRequirements=sum(PurchaseAmountTons)),by=InputCommodity]
firms_sum[["consumersbynaics"]] <- data.frame(consumers_summary_naics) #so it prints all rows
setnames(producers_summary_naics,"OutputCommodity","NAICS")
setnames(consumers_summary_naics,"InputCommodity","NAICS")
match_summary_naics <- merge(producers_summary_naics[,list(NAICS,Producers,OutputCapacity)],
                       consumers_summary_naics[,list(NAICS,Consumers,InputRequirements)],
                       "NAICS",all=TRUE)
setcolorder(match_summary_naics,c("NAICS","Producers","Consumers","OutputCapacity","InputRequirements"))
match_summary_naics[,Ratio_OutputInput:=OutputCapacity/InputRequirements]
match_summary_naics[,Possible_Matches:=as.numeric(Producers) * as.numeric(Consumers)]
firms_sum[["matches_naics"]] <- data.frame(match_summary_naics) #so it prints all rows

#raw io data summary for comparison
io_sum_make <- io_sum[,list(ProVal=sum(ProVal)),by=Industry_NAICS6_Make]
firms_sum[["io_sum_make_naics"]] <- data.frame(io_sum_make)
io_sum_make <- merge(io_sum_make,c_n6_n6io_sctg[!duplicated(Industry_NAICS6_Make),list(Industry_NAICS6_Make,Commodity_SCTG)],by="Industry_NAICS6_Make",all.x=TRUE)
io_sum_make <- io_sum_make[Commodity_SCTG>0,list(ProVal=sum(ProVal)),by=Commodity_SCTG]
firms_sum[["io_sum_make_sctg"]] <- data.frame(io_sum_make[order(Commodity_SCTG)])

#output
capture.output(print(firms_sum),file=file.path(model$outputdir,"firm_syn.txt" ))

#------------------------------------------------------------------------------------------------------
# Define sample sizes for procurement markets to be run in the next step
#------------------------------------------------------------------------------------------------------
progressNextStep("Sample sizes for procurement markets")

#Get number of (none NA) matches by NAICS, and check for imbalance in producers and consumers
naics_set <- firms_sum$matches_naics[!is.na(firms_sum$matches_naics[,"Possible_Matches"]),c("NAICS","Producers","Consumers","Possible_Matches")]
naics_set$ConsProd_Ratio <- naics_set$Consumers/naics_set$Producers
naics_set$Split_Prod <- ifelse(naics_set$ConsProd_Ratio < model$scenvars$consprodratiolimit,TRUE,FALSE)

#calculate group sizes for each commodity so that all groups are less than threshold
#Either cut both consumers and producers or just consumers (with all producers in each group)
calcSampleGroups <- function(ncons,nprod,cthresh,sprod,cprl){
  ngroups <- 1L
  nconst <- as.numeric(ncons)
  nprodt <- as.numeric(nprod)
  if(sprod){
    while (nconst * nprodt > cthresh) {
      ngroups <- ngroups + 1L
      nconst <- as.numeric(ceiling(as.numeric(ncons) / ngroups))
      nprodt <- as.numeric(ceiling(as.numeric(nprod) / ngroups))
    }
  } else {
    while ((nconst * nprodt > cthresh) | (nconst/nprodt > cprl)) {
      ngroups <- ngroups + 1L
      nconst <- as.numeric(ceiling(as.numeric(ncons) / ngroups))
    }
  }  
  return(c(nprodt,nconst,nconst * nprodt,nconst/nprodt, ngroups))
}

naics_set[,c("nProducers","nConsumers","nMatches","rev_CPRatio","groups")] <- do.call(rbind,lapply(naics_set$NAICS,function(x) calcSampleGroups(naics_set$Consumers[naics_set$NAICS==x],naics_set$Producers[naics_set$NAICS==x],model$scenvars$combinationthreshold,naics_set$Split_Prod[naics_set$NAICS==x],model$scenvars$consprodratiolimit)))
save(naics_set,file=file.path(model$outputdir,"naics_set.Rdata"))

#----------------------------------------------------------------------------------
# Write out the individual NAICS market producer and consumer files
#----------------------------------------------------------------------------------
progressNextStep("Creating producer and consumers lists")

#key the tables for faster subsetting on naics codes
setkey(consumers,InputCommodity)
setkey(producers,OutputCommodity)

for (naics in naics_set$NAICS) {
  # Construct data.tables for just the current commodity
  consc <- consumers[naics,]
  prodc <- producers[naics,]
  #write the tables to an R data file
  save(consc,prodc, file = file.path(model$outputdir,paste0(naics, ".Rdata")))
}

rm(naics, consc, prodc)

#Clean up workspace prior to saving/ending this step
rm(producers_summary,producers_summary_naics,consumers_summary,consumers_summary_naics,
   match_summary,match_summary_naics,firms_sum,io,io_sum,prefweights,sctgcat,unitcost)
rm(cbp, producers, consumers)

firmsyn <- progressEnd(firmsyn)
