# Converts the trip list to a trip table
tt_build <- function(shipments) {
  
  # Begin progress tracking
  progressStart(action = "Writing...", task = "Trip Tables", dir = SCENARIO_LOG_PATH, subtasks = FALSE)
  
  setkey(shipments,Day_ID,Tour_ID2,Trip_Seq)
  
  ### -- Heither, 05-10-2016: assign trip to CMAP TOD category
  shipments[StartTime>=20 | StartTime< 6, CMAP_TOD:=1]
  shipments[StartTime>= 6 & StartTime< 7, CMAP_TOD:=2]
  shipments[StartTime>= 7 & StartTime< 9, CMAP_TOD:=3]
  shipments[StartTime>= 9 & StartTime<10, CMAP_TOD:=4]
  shipments[StartTime>=10 & StartTime<14, CMAP_TOD:=5]
  shipments[StartTime>=14 & StartTime<16, CMAP_TOD:=6]
  shipments[StartTime>=16 & StartTime<18, CMAP_TOD:=7]
  shipments[StartTime>=18 & StartTime<20, CMAP_TOD:=8]
  
  ## check StartTimes w/in 24 hour period - esp. Direct return-to-warehouse trips
  
  ### -- Create Trip Table Storage Location -- 
  dir.create(paste0(SCENARIO_OUTPUT_PATH,"/trip_tables",sep=""), showWarnings=F)
  
  shipments[,Count:=1]
  alltrip <- shipments[,list(trips=sum(Count)),by=list(o,d,CMAP_TOD,Veh_Type)]
  for(i in 1:max(alltrip$Veh_Type))
  {
  	tempveh <- alltrip[Veh_Type==i,list(o,d,CMAP_TOD,trips)]
  	j <- 1
  	while (j <= max(tempveh$CMAP_TOD))
  	{
  		tod <- tempveh[CMAP_TOD==j,list(o,d,CMAP_TOD,trips)]
  		tod[,CMAP_TOD:=NA]
  		setkey(tod,o,d)
  		if(i==1)
  		{
  			outmtx <- paste0(SCENARIO_OUTPUT_PATH,"/trip_tables/l_tod_",j,".txt",sep="")
  			cat("t matrices\nd matrix=mf15\na matrix=mf15 ltrk 0 light truck trips for TOD", j,"\n", file=outmtx)
  		}
  		else if(i==2)
  		{
  			outmtx <- paste0(SCENARIO_OUTPUT_PATH,"/trip_tables/m_tod_",j,".txt",sep="")
  			cat("t matrices\nd matrix=mf16\na matrix=mf16 mtrk 0 medium truck trips for TOD", j,"\n", file=outmtx)
  		}
  		else
  		{
  			outmtx <- paste0(SCENARIO_OUTPUT_PATH,"/trip_tables/h_tod_",j,".txt",sep="")
  			cat("t matrices\nd matrix=mf17\na matrix=mf17 htrk 0 heavy truck trips for TOD", j,"\n", file=outmtx)
  		}
  		write.table(tod, file=outmtx, append=T, sep=" ", na=":", row.names=F, col.names=F)
  		j <- j+1
  	}
  
  }
  
  #Assigning a text Time of Day category 
  shipments[,TimeOfDay:=c("EM","AM","MD","PM","NT")[1+findInterval(StartTime,c(6,10,15,19))]]
  #Assigning a text Vehicle type
  shipments[,Vehicle:=c("2-axle","3,4 axle","semi/trailer")[Veh_Type]]
  #Aggregate by zone
  trip_table <- shipments[,list(Trips=.N),by=list(ORI_mz=o, DES_mz=d,Vehicle,TimeOfDay)]

  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  return(tt_list = list(shipments = shipments, trip_table = trip_table))
}




