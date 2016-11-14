/* STEP3_VERIFY_COSTS_TIMES.SAS
      Craig Heither, revised 11-10-2015

	This program:
		- verifies all appropriate modes are available between zone pairs (do it here before running the Meso model).
		- creates files to verify that the "best" domestic port logic is reasonable.
*/
*################################################################################################;  

options linesize= 179 pagesize= 65;

libname	p 'SASLIB';

filename out0 "port_detail_review.csv";
filename out1 "port_summary_review.csv";

data a; set p.modepath_costs;

*--------------------------------------------------------------*;
** ### -- Check non-CMAP U.S. intrazonal movements -- ### **;
*--------------------------------------------------------------*;
data a1; set a(where=(151<=Origin<=273 & Destination=Origin));
data b1; set a1;
 ** Options should be: carload direct/indirect [3,4], IMX direct/indirect [13,14], FTL direct [31], LTL direct [46], air [47]: for now allow air **;
 **  Also: new FTL indirect [32] and LTL indirect [39] for non-CMAP shipments **;
  x1= nmiss(time3,time4,time13,time14,time31,time32,time39,time46,time47,cost3,cost4,cost13,cost14,cost31,cost32,cost39,cost46,cost47);
  if x1>0;
    proc print; title "Missing Time/Cost Data for non-CMAP U.S. intrazonal movements";

data b1; set a1;
 ** ONLY Options should be modes listed directly above **;
 array tm{45} time1-time2 time5-time12 time15-time30 time33-time38 time40-time45 time48-time54; 
 array cs{45} cost1-cost2 cost5-cost12 cost15-cost30 cost33-cost38 cost40-cost45 cost48-cost54; 
  time=0; cost=0;
  do i=1 to 45;
     time=max(time,tm[i]); cost=max(cost,cs[i]);
  end;
  if time>0 or cost>0;
    proc print; title "Bad Time/Cost Data for non-CMAP U.S. intrazonal movements";


*--------------------------------------------------------------*;
** ### -- Check Canada to CMAP -- ### **;
*--------------------------------------------------------------*;
data a1; set a(where=(Origin=310 & Destination<=132));
data b1; set a1;
 ** Minimum Options available to all zones should be: water ports [1,2], FTL direct [31], LTL direct [46], air [47-50] **;
  x1= nmiss(time1,time2,time31,time46,time47,time48,time49,time50,cost1,cost2,cost31,cost46,cost47,cost48,cost49,cost50);
  if x1>0;
    proc print; title "Missing Time/Cost Data for Canada to CMAP movements";

data b1; set a1;
 ** No international water [51-54] should be available **;
  time= max(time51,time52,time53,time54);
  cost= max(cost51,cost52,cost53,cost54);
  if time>0 or cost>0;
    proc print; title "Bad Time/Cost (international water) Data for Canada to CMAP movements";

data b1; set a1;
 ** verify at least some rail is available for certain zone pairs **;
 array railtm{28} time3-time30; 
 array railcs{28} cost3-cost30; 
  time=0; cost=0;
  do i=1 to 28;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0 then delete;
  proc print; title "ERROR: No Rail Service found for Canada to CMAP movements";


*--------------------------------------------------------------*;
** ### -- Check Canada to non-CMAP U.S. -- ### **;
*--------------------------------------------------------------*;
data a1; set a(where=(Origin=310 & 151<=Destination<=273));

 *** --- PART 1. Exclude Hawaii --- ***;
data b1; set a1(where=(Destination not in (179,180)));
 ** Minimum Options available to all zones should be: FTL direct [31], LTL direct [46], air [47], international water [51-54] **;
  x1= nmiss(time31,time46,time47,time51,time52,time53,time54,cost31,cost46,cost47,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data for Canada to non-CMAP U.S. movements";

data b1; set a1(where=(Destination not in (179,180)));
 ** No CMAP water [2 only] or air [48-50] should be available **;
  time= max(time2,time48,time49,time50);
  cost= max(cost2,cost48,cost49,cost50);
  if time>0 or cost>0;
    proc print; title "Bad Time/Cost (CMAP air or CMAP water) Data for Canada to non-CMAP U.S. movements";

data b1; set a1(where=(Destination not in (154,179,180)));  *** exclude Alaska & Hawaii ***;
 ** verify at least some rail is available for certain zone pairs ... **;
 array railtm{28} time3-time30; 
 array railcs{28} cost3-cost30; 
  time=0; cost=0;
  do i=1 to 28;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0 then delete;
  proc print; title "ERROR: No Rail Service found for Canada to non-CMAP U.S. movements";

data b1; set a1(where=(Destination=154));  *** to Alaska ONLY ***;
 ** but there should be NO rail service between Canada and Alaska **;
 array railtm{28} time3-time30; 
 array railcs{28} cost3-cost30; 
  time=0; cost=0;
  do i=1 to 28;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0;
  proc print; title "ERROR: There should be NO Rail Service between Canada & Alaska";

   *** --- PART 2. Canada to Hawaii --- ***;
data b1; set a1(where=(Destination in (179,180)));
 ** ONLY air [47] and international water [51-54] should be available **;
  x1= nmiss(time47,time51,time52,time53,time54,cost47,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data (air or international shipping) for Canada to Hawaii movements";

data b1; set a1(where=(Destination in (179,180)));
 ** other modes must NOT be available **;
 array tm{49} time1-time46 time48-time50; 
 array cs{49} cost1-cost46 cost48-cost50; 
  time=0; cost=0;
  do i=1 to 49;
     time=max(time,tm[i]); cost=max(cost,cs[i]);
  end;
  if time>0 or cost>0;
    proc print; title "Bad Time/Cost (unavailable modes) Data for Canada to Hawaii movements";


*--------------------------------------------------------------*;
** ### -- Check Mexico to CMAP -- ### **;
*--------------------------------------------------------------*;
data a1; set a(where=(Origin=399 & Destination<=132));
data b1; set a1;
 ** Minimum Options available to all zones should be: water ports [1,2], FTL direct [31], LTL direct [46], air [47-50] **;
  x1= nmiss(time1,time2,time31,time46,time47,time48,time49,time50,cost1,cost2,cost31,cost46,cost47,cost48,cost49,cost50);
  if x1>0;
    proc print; title "Missing Time/Cost Data for Mexico to CMAP movements";
 
data b1; set a1;
 ** No international water [51-54] should be available **;
  time= max(time51,time52,time53,time54);
  cost= max(cost51,cost52,cost53,cost54);
  if time>0 or cost>0;
    proc print; title "Bad Time/Cost (international water) Data for Mexico to CMAP movements";

data b1; set a1;
 ** verify at least some rail is available for certain zone pairs **;
 array railtm{28} time3-time30; 
 array railcs{28} cost3-cost30; 
  time=0; cost=0;
  do i=1 to 28;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0 then delete;
  proc print; title "ERROR: No Rail Service found for Mexico to CMAP movements";


*--------------------------------------------------------------*;
** ### -- Check Mexico to non-CMAP U.S. -- ### **;
*--------------------------------------------------------------*;
data a1; set a(where=(Origin=399 & 151<=Destination<=273));

 *** --- PART 1. Exclude Hawaii --- ***;
data b1; set a1(where=(Destination not in (179,180)));
 ** Minimum Options available to all zones should be: FTL direct [31], LTL direct [46], air [47], international water [51-54] **;
  x1= nmiss(time31,time46,time47,time51,time52,time53,time54,cost31,cost46,cost47,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data for Mexico to non-CMAP U.S. movements";

data b1; set a1(where=(Destination not in (179,180)));
 ** No CMAP water [2 only] or air [48-50] should be available **;
  time= max(time2,time48,time49,time50);
  cost= max(cost2,cost48,cost49,cost50);
  if time>0 or cost>0;
    proc print; title "Bad Time/Cost (CMAP air or CMAP water) Data for Mexico to non-CMAP U.S. movements";

data b1; set a1(where=(Destination not in (154,179,180)));  *** exclude Alaska & Hawaii ***;
 ** verify at least some rail is available for certain zone pairs ... **;
 array railtm{28} time3-time30; 
 array railcs{28} cost3-cost30; 
  time=0; cost=0;
  do i=1 to 28;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0 then delete;
  proc print; title "ERROR: No Rail Service found for Mexico to non-CMAP U.S. movements";

data b1; set a1(where=(Destination=154));  *** to Alaska ONLY ***;
 ** but there should be NO rail service between Mexico and Alaska **;
 array railtm{28} time3-time30; 
 array railcs{28} cost3-cost30; 
  time=0; cost=0;
  do i=1 to 28;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0;
  proc print; title "ERROR: There should be NO Rail Service between Mexico & Alaska";


*--------------------------------------------------------------*;
** ### -- Check Hawaii to everywhere except Hawaii -- ### **;
*--------------------------------------------------------------*;
data a1; set a(where=(Origin in (179,180) & Destination not in (179,180)));

 *** --- PART 1. CMAP zones --- ***;
data b1; set a1(where=(Destination<=132));
 ** ONLY air [47-50] and international water [51-54] should be available to CMAP zones **;
  x1= nmiss(time47,time48,time49,time50,time51,time52,time53,time54,cost47,cost48,cost49,cost50,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data (air or international shipping) for Hawaii to CMAP movements";

data b1; set a1(where=(Destination<=132));
 ** ONLY air [47-50] and international water [51-54] should be available to CMAP zones **;
 array tm{46} time1-time46; 
 array cs{46} cost1-cost46; 
  time=0; cost=0;
  do i=1 to 46;
     time=max(time,tm[i]); cost=max(cost,cs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0;
  proc print; title "ERROR: Bad Time/Cost for Hawaii to CMAP movements";

 *** --- PART 2. Non-CMAP zones --- ***;
data b1; set a1(where=(Destination>132));
 ** ONLY air [47] and international water [51-54] should be available to non-CMAP zones **;
  x1= nmiss(time47,time51,time52,time53,time54,cost47,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data (air or international shipping) for Hawaii to non-CMAP movements";

data b1; set a1(where=(Destination>132));
 ** ONLY air [47] and international water [51-54] should be available to non-CMAP zones **;
 array tm{49} time1-time46 time48-time50; 
 array cs{49} cost1-cost46 cost48-cost50; 
  time=0; cost=0;
  do i=1 to 49;
     time=max(time,tm[i]); cost=max(cost,cs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0;
  proc print; title "ERROR: Bad Time/Cost for Hawaii to non-CMAP movements";


*--------------------------------------------------------------*;
** ### -- Check Alaska to everywhere except Alaska -- ### **;
*--------------------------------------------------------------*;
data a1; set a(where=(Origin=154 & Destination ne 154));

 *** --- PART 1. CMAP zones --- ***;
data b1; set a1(where=(Destination<=132));
 ** Minimum Options available to all zones should be: FTL direct [31], LTL direct [46], air [47-50], international water [51-54] **;
  x1= nmiss(time31,time46,time47,time48,time49,time50,time51,time52,time53,time54,cost31,cost46,cost47,cost48,cost49,cost50,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data (truck, air or international shipping) for Alaska to CMAP movements";

data b1; set a1(where=(Destination<=132));
 ** No CMAP water [1-2] or rail [3-30] should be available **;
 array railtm{30} time1-time30; 
 array railcs{30} cost1-cost30; 
  time=0; cost=0;
  do i=1 to 30;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0;
    proc print; title "Bad Time/Cost (CMAP water or rail) Data for Alaska to CMAP movements";

 *** --- PART 2. non-CMAP U.S. zones except Hawaii --- ***;
data b1; set a1(where=(132<Destination<=273 & Destination not in (179,180)));
 ** Minimum Options available to all zones should be: FTL direct [31], LTL direct [46], air [47], international water [51-54] **;
  x1= nmiss(time31,time46,time47,time51,time52,time53,time54,cost31,cost46,cost47,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data (truck, air or international shipping) for Alaska to non-CMAP U.S. movements";

data b1; set a1(where=(132<Destination<=273 & Destination not in (179,180)));
 ** No CMAP water [1-2], rail [3-30] or air [48-50] should be available **;
 array railtm{33} time1-time30 time48-time50; 
 array railcs{33} cost1-cost30 cost48-cost50; 
  time=0; cost=0;
  do i=1 to 33;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0;
    proc print; title "Bad Time/Cost (CMAP water, rail or air) Data for Alaska to non-CMAP U.S. movements";


 *** --- PART 3. Foreign zones (except Canada/Mexico) and Hawaii --- ***;
data b1; set a1(where=((Destination>273 or Destination in (179,180)) & Destination not in (310,399)));
 ** ONLY air [47] and international water [51-54] should be available to foreign zones **;
  x1= nmiss(time47,time51,time52,time53,time54,cost47,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data (air or international shipping) for Alaska to Hawaii/Foreign movements";

data b1; set a1(where=((Destination>273 or Destination in (179,180)) & Destination not in (310,399)));
 ** ONLY air [47] and international water [51-54] should be available to foreign zones **;
 array tm{49} time1-time46 time48-time50; 
 array cs{49} cost1-cost46 cost48-cost50; 
  time=0; cost=0;
  do i=1 to 49;
     time=max(time,tm[i]); cost=max(cost,cs[i]);
  end;
  if time>0 or cost>0;
  proc print; title "ERROR: Bad Time/Cost for Alaska to Hawaii/Foreign movements";


*--------------------------------------------------------------*;
** ### -- Check U.S. to Foreign -- ### **;
*--------------------------------------------------------------*;
data a1; set a(where=(Origin<=273 & Origin not in (154,179,180) & Destination>273 & Destination not in (310,399)));

 *** --- PART 1. CMAP zones --- ***;
data b1; set a1(where=(Origin<=132));
 ** ONLY air [47-50] and international water [51-54] should be available to foreign zones **;
  x1= nmiss(time47,time48,time49,time50,time51,time52,time53,time54,cost47,cost48,cost49,cost50,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data (air or international shipping) for CMAP to Foreign movements";

data b1; set a1(where=(Origin<=132));
 ** ONLY air [47-50] and international water [51-54] should be available to foreign zones **;
 array tm{46} time1-time46; 
 array cs{46} cost1-cost46; 
  time=0; cost=0;
  do i=1 to 46;
     time=max(time,tm[i]); cost=max(cost,cs[i]);
  end;
  if time>0 or cost>0;
  proc print; title "ERROR: Bad Time/Cost for CMAP to Foreign movements";
 
   *** --- PART 2. non-CMAP U.S. zones --- ***;
data b1; set a1(where=(Origin>132));
 ** ONLY air [47] and international water [51-54] should be available to foreign zones **;
  x1= nmiss(time47,time51,time52,time53,time54,cost47,cost51,cost52,cost53,cost54); 
  if x1>0;
    proc print; title "Missing Time/Cost Data (air or international shipping) for non-CMAP U.S. to Foreign movements";


data b1; set a1(where=(Origin>132));
 ** ONLY air [47] and international water [51-54] should be available to foreign zones **;
 array tm{49} time1-time46 time48-time50; 
 array cs{49} cost1-cost46 cost48-cost50; 
  time=0; cost=0;
  do i=1 to 49;
     time=max(time,tm[i]); cost=max(cost,cs[i]);
  end;
  if time>0 or cost>0;
  proc print; title "ERROR: Bad Time/Cost for non-CMAP U.S. to Foreign movements";


*----------------------------------------------------------------------------------*;
** ### -- Check non-CMAP U.S. to non-CMAP U.S. (excluding Alaska/Hawaii) -- ### **;
*----------------------------------------------------------------------------------*;
data a1; set a(where=(132<Origin<=273 & Origin not in (154,179,180) & 132<Destination<=273 & Destination not in (154,179,180)));
  if Origin=Destination then delete;                *** -- no intrazonal movements -- ***;

  
data b1; set a1;
 ** AT A MINIMUM: FTL direct/indirect [31,32], LTL direct/indirect [46,39] and non-CMAP air [47] must be available **;
  x1= nmiss(time31,time32,time39,time46,time47,cost31,cost32,cost39,cost46,cost47); 
  if x1>0;
  data b2; set b1(obs=10);
    proc print; title "Missing Time/Cost Data (for MINIMUM ALLOWABLE modes): non-CMAP U.S. to non-CMAP U.S. movements";
	
data b1; set a1;
 ** verify at least some rail and inland waterway is available for certain zone pairs **;
 array railtm{5} time1 time3-time4 time13-time14; 
 array railcs{5} cost1 cost3-cost4 cost13-cost14; 
  time=0; cost=0;
  do i=1 to 5;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
 proc summary nway; var time cost; output out=b max=;
data b; set b;
  if time>0 & cost>0 then delete;
  proc print; title "ERROR: No Rail Service or Inland Water Option found for non-CMAP U.S. to non-CMAP U.S. movements";

data b1; set a1;
 ** the following modes are NOT allowed for non-CMAP U.S. to non-CMAP U.S. movements: **;
 ** CMAP water [2 only], rail w/ stop at logistics node [5-12, 15-30], truck w/ stop at logistics node [33-38,40-45], CMAP air [48-50], international water [51-54] **;
 array railtm{44} time2 time5-time12 time15-time30 time33-time38 time40-time45 time48-time54; 
 array railcs{44} cost2 cost5-cost12 cost15-cost30 cost33-cost38 cost40-cost45 cost48-cost54; 
  time=0; cost=0;
  do i=1 to 44;
     time=max(time,railtm[i]); cost=max(cost,railcs[i]);
  end;
  if time>0 & cost>0;
  data b2; set b1(obs=10);
  proc print; title "ERROR: There is a Prohibited Mode for non-CMAP U.S. to non-CMAP U.S. movements";

  
*--------------------------------------------------------------*;
** ### -- Verify Every Pair has at least One Viable Option -- ### **;
*--------------------------------------------------------------*; 
data a1; set a;  
  array tm{54} time1-time54; 
  array cs{54} cost1-cost54; 
  misstime=0; misscost=0;
  do i=1 to 54;
     if tm[i]=. then misstime=misstime+1; 
     if cs[i]=. then misscost=misscost+1; 	 
  end;
 if misstime=54 or misscost=54;
   proc print; title "Movement has NO Transport Options";

*--------------------------------------------------------------*;
** ### -- Verify Mode Options for DIRECT Shipments -- ### **;
*--------------------------------------------------------------*; 
 ** The logic in the meso model can apply direct shipment options to:
      - intra-CMAP shipments
	  - intrazonal shipments (U.S. states)
	  - shipments between the two Hawaiian zones
	  - shipments between states (but not between Hawaii and any other state)
	  - shipments between the U.S. (except Hawaii) and Canada/Mexico  ;
	  
data a1; set a;  
   *** -- set direct flag for zonal interchanges -- ***;
 if (Origin<=132 & Destination<=132) then direct=1;
 else if (Origin=Destination) then direct=1;
 else if (Origin in (179,180) & Destination in (179,180)) then direct=1;
 if (Origin<=273 & Origin not in (179,180) & Destination<=273 & Destination not in (179,180)) then direct=1;
 if (Origin<=273 & Origin not in (179,180) & Destination in (310,399)) then direct=1;
 if (Origin in (310,399) & Destination<=273 & Destination not in (179,180)) then direct=1;

data b1; set a1(where=(direct=1));
 ** AT A MINIMUM: FTL direct [31] & LTL direct [46] must be available **;
  x1= nmiss(time31,time46,cost31,cost46); 
  if x1>0;
  data b2; set b1(obs=10);
    proc print; title "Missing Time/Cost Data (for MINIMUM ALLOWABLE DIRECT modes)";

*--------------------------------------------------------------*;
** ### -- Verify Mode Options for INDIRECT Shipments -- ### **;
*--------------------------------------------------------------*; 
data a1; set a;

** ### -- Verify intra-CMAP Shipments -- ### **;
 ** NOTE: The Meso model logic actually forces these shipments to use Direct modes **;
 
 
** ### -- Verify shipments between CMAP & Rest of U.S. (except Hawaii)/Canada/Mexico -- ### **;
data b1; set a1(where=(Origin<=132 & (132<Destination<=273 or Destination in (310,399)) & Destination not in (179,180))); 
  ** AT A MINIMUM: FTL indirect [32] & LTL indirect [39] & one air mode [47] must be available **;
  x1= nmiss(time32,time39,time47,cost32,cost39,cost47); 
  if x1>0;
  data b2; set b1(obs=10);
    proc print; title "Missing Time/Cost Data (for MINIMUM ALLOWABLE INDIRECT modes): between CMAP & Rest of U.S. (except Hawaii)/Canada/Mexico";

	
** ### -- Verify shipments between Rest of U.S. (except Hawaii)/Canada/Mexico & CMAP -- ### **;
data b1; set a1(where=(Destination<=132 & (132<Origin<=273 or Origin in (310,399)) & Origin not in (179,180))); 
  ** AT A MINIMUM: FTL indirect [32] & LTL indirect [39] & one air mode [47] must be available **;
  x1= nmiss(time32,time39,time47,cost32,cost39,cost47); 
  if x1>0;
  data b2; set b1(obs=10);
    proc print; title "Missing Time/Cost Data (for MINIMUM ALLOWABLE INDIRECT modes): between Rest of U.S. (except Hawaii)/Canada/Mexico & CMAP";	

	
** ### -- Verify U.S.-Foreign (except Canada/Mexico) shipments -- ### **;	
data b1; set a1(where=((Origin<=273 & Destination>273 & Destination not in (310,399)) or (Origin>273 & Origin not in (310,399) & Destination<=273))); 
  ** AT A MINIMUM: one air mode [47] and international shipping [51-54] must be available **;
  x1= nmiss(time47,time51,time52,time53,time54,cost47,cost51,cost52,cost53,cost54); 
  if x1>0;
  data b2; set b1(obs=10);
    proc print; title "Missing Time/Cost Data (for MINIMUM ALLOWABLE INDIRECT modes): U.S.-Foreign (except Canada/Mexico) shipments";   

   *********** check inland water;
data a1; set a;
data b1; set a1(where=(time1 is not null or cost1 is not null));
  data b2; set b1(where=(Origin<=132 or Destination<=132));
  
  data b3; set b1(where=(Origin>132 & Destination>132));
    
   
*##################################################################################################;
*##################################################################################################;
**** --- Review specific movements to verify which domestic port is used for foreign trade --- ****;
  
*===================================
        KANSAS CITY
*===================================;		
*--------------------------------------------------------------*;
** ### -- to Eastern Asia -- ### **;
*--------------------------------------------------------------*;
 ** Kansas City (189-KS part, 209-MO part)
     China (316), Japan (374), Mongolia (403), Democratic Peoples Republic of Korea [N. Korea] (418), Republic of Korea [S. Korea] (456)   ;

data a1; set a(where=(Origin in (189,209)));	 
data b1; set a1(where=(Destination in (316,374,403,418,456)));
  proc sort; by Origin Destination;
  
 data null; file out0; 
 put "===============================" / "Detail: Kansas City to Eastern Asia" / "Origin,Destination,Location,Port_name,Port_mesozone,GCD,MinShipTime,MinShipCost";   
   data x; set b1; 
      file out0 mod dsd;
	   put Origin Destination Location Port_name Port_mesozone GCD MinShipTime MinShipCost;	
  
 proc summary nway data=b1; class Port_name; id Port_mesozone; var GCD; output out=x N=Frequency; proc sort data=x; by descending Frequency;
     data null; file out1; put "===============================" / "Summary: Kansas City to Eastern Asia";   
   data x(drop=_type_ _freq_); set x; 
      file out1 mod dsd;
	   put Port_name Port_mesozone Frequency;	 

*--------------------------------------------------------------*;
** ### -- to Europe -- ### **;
*--------------------------------------------------------------*;
data b1; set a1(where=(Destination in (275,285,291,292,304,324,328,330,339,344,345,350,353,364,370,372,382,388,412,419,430,431,434,435,446,451,452,458,463,464,469,481,483)));
  proc sort; by Origin Destination;
  
 data null; file out0 mod; 
 put "===============================" / "Detail: Kansas City to Europe" / "Origin,Destination,Location,Port_name,Port_mesozone,GCD,MinShipTime,MinShipCost";   
   data x; set b1; 
      file out0 mod dsd;
	   put Origin Destination Location Port_name Port_mesozone GCD MinShipTime MinShipCost;	
  
   proc summary nway data=b1; class Port_name; id Port_mesozone; var GCD; output out=x N=Frequency; proc sort data=x; by descending Frequency;
     data null; file out1 mod; put / "===============================" / "Summary: Kansas City to Europe";   
   data x(drop=_type_ _freq_); set x; 
      file out1 mod dsd;
	   put Port_name Port_mesozone Frequency;	 
   
*--------------------------------------------------------------*;
** ### -- to Rest of Americas -- ### **;
*--------------------------------------------------------------*;   
data b1; set a1(where=(Destination in (281,283,293,297,300,302,310,312,315,319,323,333,334,336,356,359,360,363,373,399,414,424,426,427,484,487)));
  proc sort; by Origin Destination;
  
 data null; file out0 mod; 
 put "===============================" / "Detail: Kansas City to Rest of Americas" / "Origin,Destination,Location,Port_name,Port_mesozone,GCD,MinShipTime,MinShipCost";   
   data x; set b1; 
      file out0 mod dsd; put Origin Destination Location Port_name Port_mesozone GCD MinShipTime MinShipCost;	  

   proc summary nway data=b1; class Port_name; id Port_mesozone; var GCD; output out=x N=Frequency; proc sort data=x; by descending Frequency;
     data null; file out1 mod; put / "===============================" / "Summary: Kansas City to Rest of Americas";   
   data x(drop=_type_ _freq_); set x; 
      file out1 mod dsd;
	   put Port_name Port_mesozone Frequency;	
   
   
*===================================
        CHICAGO
*===================================;
*--------------------------------------------------------------*;
** ### -- to Eastern Asia -- ### **;
*--------------------------------------------------------------*;
 ** selected Chicago zones from IL & IN portions;

data a1; set a(where=(Origin in (1,20,33,42,49,58,62,71,78,85,87,109,127,128,129)));	 
data b1; set a1(where=(Destination in (316,374,403,418,456)));
  proc sort; by Origin Destination;
  
 data null; file out0 mod; 
 put "===============================" / "Detail: Chicago to Eastern Asia" / "Origin,Destination,Location,Port_name,Port_mesozone,GCD,MinShipTime,MinShipCost";   
   data x; set b1; 
      file out0 mod dsd; put Origin Destination Location Port_name Port_mesozone GCD MinShipTime MinShipCost;	
	  
   proc summary nway data=b1; class Port_name; id Port_mesozone; var GCD; output out=x N=Frequency; proc sort data=x; by descending Frequency;
     data null; file out1 mod; put / "===============================" / "Summary: Chicago to Eastern Asia";   
   data x(drop=_type_ _freq_); set x; 
      file out1 mod dsd;
	   put Port_name Port_mesozone Frequency;	  

*--------------------------------------------------------------*;
** ### -- to Europe -- ### **;
*--------------------------------------------------------------*;
data b1; set a1(where=(Destination in (275,285,291,292,304,324,328,330,339,344,345,350,353,364,370,372,382,388,412,419,430,431,434,435,446,451,452,458,463,464,469,481,483)));
  proc sort; by Origin Destination;
  
 data null; file out0 mod; 
 put "===============================" / "Detail: Chicago to Europe" / "Origin,Destination,Location,Port_name,Port_mesozone,GCD,MinShipTime,MinShipCost";   
   data x; set b1; 
      file out0 mod dsd; put Origin Destination Location Port_name Port_mesozone GCD MinShipTime MinShipCost;	

   proc summary nway data=b1; class Port_name; id Port_mesozone; var GCD; output out=x N=Frequency; proc sort data=x; by descending Frequency;
     data null; file out1 mod; put / "===============================" / "Summary: Chicago to Europe";   
   data x(drop=_type_ _freq_); set x; 
      file out1 mod dsd;
	   put Port_name Port_mesozone Frequency;	  
	      
*--------------------------------------------------------------*;
** ### -- to Rest of Americas -- ### **;
*--------------------------------------------------------------*;   
data b1; set a1(where=(Destination in (281,283,293,297,300,302,310,312,315,319,323,333,334,336,356,359,360,363,373,399,414,424,426,427,484,487)));
  proc sort; by Origin Destination;
  
 data null; file out0 mod; 
 put "===============================" / "Detail: Chicago to Rest of Americas" / "Origin,Destination,Location,Port_name,Port_mesozone,GCD,MinShipTime,MinShipCost";   
   data x; set b1; 
      file out0 mod dsd; put Origin Destination Location Port_name Port_mesozone GCD MinShipTime MinShipCost;   

   proc summary nway data=b1; class Port_name; id Port_mesozone; var GCD; output out=x N=Frequency; proc sort data=x; by descending Frequency;
     data null; file out1 mod; put / "===============================" / "Summary: Chicago to Rest of Americas";   
   data x(drop=_type_ _freq_); set x; 
      file out1 mod dsd;
	   put Port_name Port_mesozone Frequency;

	   
*===================================
        DENVER
*===================================;
*--------------------------------------------------------------*;
** ### -- to Eastern Asia -- ### **;
*--------------------------------------------------------------*;
data a1; set a(where=(Origin=164));	 
data b1; set a1(where=(Destination in (316,374,403,418,456)));
  proc sort; by Origin Destination;
  
 data null; file out0 mod; 
 put "===============================" / "Detail: Denver to Eastern Asia" / "Origin,Destination,Location,Port_name,Port_mesozone,GCD,MinShipTime,MinShipCost";   
   data x; set b1; 
      file out0 mod dsd; put Origin Destination Location Port_name Port_mesozone GCD MinShipTime MinShipCost;  

   proc summary nway data=b1; class Port_name; id Port_mesozone; var GCD; output out=x N=Frequency; proc sort data=x; by descending Frequency;
     data null; file out1 mod; put / "===============================" / "Summary: Denver to Eastern Asia";   
   data x(drop=_type_ _freq_); set x; 
      file out1 mod dsd;
	   put Port_name Port_mesozone Frequency;
   
*--------------------------------------------------------------*;
** ### -- to Europe -- ### **;
*--------------------------------------------------------------*;
data b1; set a1(where=(Destination in (275,285,291,292,304,324,328,330,339,344,345,350,353,364,370,372,382,388,412,419,430,431,434,435,446,451,452,458,463,464,469,481,483)));
  proc sort; by Origin Destination;
  
 data null; file out0 mod; 
 put "===============================" / "Detail: Denver to Europe" / "Origin,Destination,Location,Port_name,Port_mesozone,GCD,MinShipTime,MinShipCost";   
   data x; set b1; 
      file out0 mod dsd; put Origin Destination Location Port_name Port_mesozone GCD MinShipTime MinShipCost;    

   proc summary nway data=b1; class Port_name; id Port_mesozone; var GCD; output out=x N=Frequency; proc sort data=x; by descending Frequency;
     data null; file out1 mod; put / "===============================" / "Summary: Denver to Europe";   
   data x(drop=_type_ _freq_); set x; 
      file out1 mod dsd;
	   put Port_name Port_mesozone Frequency;
   
*--------------------------------------------------------------*;
** ### -- to Rest of Americas -- ### **;
*--------------------------------------------------------------*;   
data b1; set a1(where=(Destination in (281,283,293,297,300,302,310,312,315,319,323,333,334,336,356,359,360,363,373,399,414,424,426,427,484,487)));
  proc sort; by Origin Destination;
  
 data null; file out0 mod; 
 put "===============================" / "Detail: Denver to Rest of Americas" / "Origin,Destination,Location,Port_name,Port_mesozone,GCD,MinShipTime,MinShipCost";   
   data x; set b1; 
      file out0 mod dsd; put Origin Destination Location Port_name Port_mesozone GCD MinShipTime MinShipCost;     

   proc summary nway data=b1; class Port_name; id Port_mesozone; var GCD; output out=x N=Frequency; proc sort data=x; by descending Frequency;
     data null; file out1 mod; put / "===============================" / "Summary: Denver to Rest of Americas";   
   data x(drop=_type_ _freq_); set x; 
      file out1 mod dsd;
	   put Port_name Port_mesozone Frequency;
	   


run;
