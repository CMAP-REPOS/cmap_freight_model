/* STEP4_PREPARE_FAF-CONSISTENT_MODAL_DISTANCES.SAS
      Craig Heither, rev. 04-29-2016

	This program creates a file of zonal interchange distances by mode that are as consistent as possible with the FAF methodology. 
	
	- Truck: domestic skim distance
	- Rail: domestic skim distance (including skim between logistics node and destination, when  appropriate)
	- Inland Water: assume all is domestic
	- International Water: use distance between domestic location and domestic centroid nearest foreign location
	- Air: for international shipments - use distance between domestic location and domestic centroid nearest foreign location 
*/


*###=================================================================================###
    DEFINE INPUT/OUTPUT FILES
*###=================================================================================###;
libname	p 'SASLIB';
%let scen=&sysparm;					                          *** -- Scenario number -- ***; 
%let emdir=..\output_data\&scen.\;	                          *** -- Location of Emme skims -- ***;
%let outdir=outputs\&sysparm.;                                *** -- Location of output files for Meso Freight Model -- ***;
%let maxCMAP=132;                                             *** -- Maximum CMAP mesozone number -- ***;
%let maxUS=273;                                               *** -- Maximum U.S. mesozone number -- ***;
%let minskim=42;					                          *** -- Minimum skim matrix to be read in -- ***;
%let maxskim=49;					                          *** -- Maximum skim matrix to be read in -- ***;

  *##-- Input --##;
filename in1 "&outdir.\data_mesozone_gcd.csv";                *** -- New output file of Great Circle Distances (created in Step1_Create_GCD_file.sas) -- ***;
  *##-- Output --##;
filename out1 "&outdir.\summary_FAF_shipment_miles.csv";      *** -- New output file of FAF-like shipment distances for ton-miles calculations -- ***;


*###=================================================================================###
    READ MODAL DISTANCES USED IN MESO MODEL
*###=================================================================================###;
data test; set P.modepath_costs;

data test; set test;
   array dist{90} cost1-cost54 LineHaul133-LineHaul150 IntDray133-IntDray150; 
   do i=1 to 54;
     MinPath=i;
	 Cost=dist[i];
	 
	 if i<=2 then do;
	    if Origin<=&maxCMAP or Destination<=&maxCMAP then Miles=dist[i+66];                      ***CMAP Inland Water: LineHaul145 or 146;
	    else Miles=Waterway;                                                                     ***Non-CMAP Inland Water: Waterway;
	 end;

     if 3<=i<=30 then do;
	    if i in(3,4,13,14) then Miles=Rdist00;                                                   ***Rail: Direct Carload/IMX (all locations);
		else if 5<=i<=8 then Miles=dist[i+64]+dist[i+82]+ExtDray;                                ***CMAP Rail: LineHaul147-150 + IntDray147-150 + ExtDray;
	    else if 9<=i<=12 then Miles=dist[i+60]+dist[i+78]+ExtDray;                               ***CMAP Rail: LineHaul147-150 + IntDray147-150 + ExtDray;
		else if 15<=i<=18 then Miles=dist[i+54]+dist[i+72]+ExtDray;                              ***CMAP Rail: LineHaul147-150 + IntDray147-150 + ExtDray;
		else if 19<=i<=22 then Miles=dist[i+50]+dist[i+68]+ExtDray;                              ***CMAP Rail: LineHaul147-150 + IntDray147-150 + ExtDray;		
		else if 23<=i<=26 then Miles=dist[i+46]+dist[i+64]+ExtDray;                              ***CMAP Rail: LineHaul147-150 + IntDray147-150 + ExtDray;		
		else if 27<=i<=30 then Miles=dist[i+42]+dist[i+60]+ExtDray;                              ***CMAP Rail: LineHaul147-150 + IntDray147-150 + ExtDray;	

	    if i in(3,4,13,14) then do; Carrier=Carr; lnode=.; end; 	                             ***assign correct rail carrier & logistics node;
		else if i in(5,9,15,19,23,27) then do; Carrier=Carr147;  lnode=147; end;
		else if i in(6,10,16,20,24,28) then do; Carrier=Carr148; lnode=148; end;
		else if i in(7,11,17,21,25,29) then do; Carrier=Carr149; lnode=149; end;
		else if i in(8,12,18,22,26,30) then do; Carrier=Carr150; lnode=150; end;		
	 end;
	 
	 if 31<=i<=46 then do;
	    if i in(31,46) then do;
		   if (Origin=179 & Destination=180) or (Origin=180 & Destination=179) then Miles=GCD;   ***Hawaii Truck: FTL/LTL direct between Hawaiian zones;
           else Miles=Tdist00;                                                                   ***CMAP & non-CMAP Truck: FTL/LTL direct (always available);
        end;		   
	    else if Origin<=&maxCMAP or Destination<=&maxCMAP then do;
		   if 32<=i<=38 then Miles=dist[i+23]+dist[i+41]+ExtDray;                                ***CMAP Truck: FTL indirect (LH b/n LogNode-Dest + IntDray(LH b/n Orig-LogNode) +ExtDray);
		   else Miles=dist[i+16]+dist[i+34]+ExtDray;
	    end;	
        else do;
	       if i in(32,39) then Miles=TDist00;	                                                 ***Non-CMAP Truck: FTL/LTL indirect (same distance as Direct);   
        end;	   
	 end;
	 
	 if 47<=i<=50 then do;
	    if Origin<=&maxCMAP or Destination<=&maxCMAP then Miles=dist[i+16]+dist[i+34]+ExtDray;   ***CMAP Air: LineHaul141-144 + drayage;
	    else Miles=dist[63]+dist[81]+ExtDray;                                                    ***Non-CMAP Air: all stored in LineHaul141 & IntDray141;
	 end;
	 
     if i>50 then Miles=GCD;                                                                     ***International Shipping;
	 
     output;
   end;
    keep Origin Destination MinPath Miles Cost LineHaul133-LineHaul150 waterway ExtDray Tdist00 d GCD Rdist00 Carrier lnode IntDray133-IntDray150;
  
data test; retain Origin Destination MinPath Miles Cost IntDray133-IntDray150 LineHaul133-LineHaul150 waterway; set test(where=(Cost is not null));  
  MesoMiles=round(Miles,0.1);
  rename Origin=Production_zone Destination=Consumption_zone;
    proc sort; by Production_zone Consumption_zone MinPath Carrier;
  

*###=================================================================================###
    READ MATRIX DATA OF DOMESTIC DISTANCES
*###=================================================================================###;
%let current=&minskim;
%macro ReadSkims;

   %do %while (&current le &maxskim);

      *** -- Each set of skim matrices represents a specific rail carrier or mode -- ***;
	  %if &current=42 %then %do;       %let Carr='T';  %let bs=31; %end;
	  %else %if &current=43 %then %do; %let Carr='B';  %let bs=1;  %end;  
	  %else %if &current=44 %then %do; %let Carr='U';  %let bs=5;  %end;
	  %else %if &current=45 %then %do; %let Carr='X';  %let bs=9;  %end; 	  
	  %else %if &current=46 %then %do; %let Carr='N';  %let bs=13; %end; 
	  %else %if &current=47 %then %do; %let Carr='CP'; %let bs=17; %end;  
	  %else %if &current=48 %then %do; %let Carr='CN'; %let bs=21; %end;
	  %else %if &current=49 %then %do; %let Carr='K';  %let bs=25; %end;
	 run;
	 
	 **** Read Domestic Distance Skims ****;
     data mf&current(keep=Production_zone Consumption_zone DmstMiles); infile "&emdir.mf&current..in" missover dlm=' :' firstobs=5;
	    input Production_zone d1 v1 d2 v2 d3 v3;
		  Consumption_zone=d1; DmstMiles=round(v1,0.1); output;
		  Consumption_zone=d2; DmstMiles=round(v2,0.1); output;
		  Consumption_zone=d3; DmstMiles=round(v3,0.1); output;
		  proc sort nodupkey; by Production_zone Consumption_zone;
		run;  
	 data mf&current; set mf&current(where=(Production_zone>0 & Consumption_zone>0)); length Carrier $2; Carrier=&Carr; run;
	
	 **** Read Original Full Distance Skims ****;
     data base&bs(keep=Production_zone Consumption_zone BaseMiles); infile "&emdir.mf&bs..in" missover dlm=' :' firstobs=5;
	    input Production_zone d1 v1 d2 v2 d3 v3;
		  Consumption_zone=d1; BaseMiles=round(v1,0.1); output;
		  Consumption_zone=d2; BaseMiles=round(v2,0.1); output;
		  Consumption_zone=d3; BaseMiles=round(v3,0.1); output;
		  proc sort nodupkey; by Production_zone Consumption_zone;
		run;  
	 data base&bs; set base&bs(where=(Production_zone>0 & Consumption_zone>0)); length Carrier $2; Carrier=&Carr; run;	
		
	 %let current=%eval(&current+1);
   %end;
  run;
%mend ReadSkims;
%ReadSkims
/* end of macro */
run;

data domestic; set mf&minskim-mf&maxskim;
data baserail; set base1 base5 base9 base13 base17 base21 base25; by Production_zone Consumption_zone Carrier;
proc sort data=base31; by Production_zone Consumption_zone;

*###=================================================================================###
    CREATE FINAL FILE
*###=================================================================================###;
*--------------------*;
*** -- Truck -- ***;
*--------------------*;
data t; set test(where=(31<=MinPath<=46)); proc sort; by Production_zone Consumption_zone MinPath;
data t; merge t(in=hit) base31; by Production_zone Consumption_zone; if hit;

   *** -- Separate into Direct and Indirect -- ****;
data truckdir truckindir; set t;
  if (MinPath in (31,46)) or (MinPath in (32,39) & Production_zone>&maxCMAP) then output truckdir;  *** -- these indirect just use skimmed distance -- ***;
  else output truckindir;   

data truck(drop=i); set domestic(where=(Carrier="T"));
  do i=31 to 46; MinPath=i; output; end;
   proc sort; by Production_zone Consumption_zone MinPath;

   *** ===== Direct Truck ===== ****;   
data truckdir; set truckdir(where=(Production_zone<=&maxUS & Production_zone<=Consumption_zone));   
data truckdir; merge truckdir(in=hit) truck; by Production_zone Consumption_zone MinPath; if hit;    
   if abs(BaseMiles-DmstMiles)>0.1 then FafMiles=MesoMiles-(BaseMiles-DmstMiles); else FafMiles=MesoMiles; *** -- use difference between Base and Domestic to calculate final FAF -- ***;
   if Production_zone=Consumption_zone then do; if Production_zone<=&maxUS then FafMiles=MesoMiles; else FafMiles=0; end;
    keep Production_zone Consumption_zone MinPath MesoMiles BaseMiles FafMiles DmstMiles;
	
   *** -- Get Distance between Hawaiian zones -- ***;   
proc import datafile=in1 out=hawaii dbms=csv replace;   
data hawaii(keep=Production_zone Consumption_zone MinPath MesoMiles FafMiles); set hawaii(where=(179<=Production_zone<=180 & 179<=Consumption_zone<=180));
   MesoMiles=GCD; FafMiles=GCD;
   MinPath=32; output;
   MinPath=39; output;
   proc sort; by Production_zone Consumption_zone MinPath;	

data truckdir; merge truckdir(in=hit) hawaii; by Production_zone Consumption_zone MinPath; if hit;  
   

   *** ===== Indirect Truck ===== ****;   
data truckindir(keep=Production_zone Consumption_zone MinPath MesoMiles lnode); set truckindir(where=(Production_zone<=&maxUS & Production_zone<=Consumption_zone));
  if MinPath<=38 then lnode=MinPath+101;
  else lnode=MinPath+94;
   proc sort; by lnode Consumption_zone MinPath;

data truck; set truck; rename Production_zone=lnode;
   proc sort; by lnode Consumption_zone; 
data base31; set base31; rename Production_zone=lnode;
   proc sort; by lnode Consumption_zone;
data truck; merge truck(in=hit) base31; by lnode Consumption_zone; if hit;   
   proc sort; by lnode Consumption_zone MinPath;
   
data truckindir; merge truckindir(in=hit) truck; by lnode Consumption_zone MinPath; if hit;    
   if abs(BaseMiles-DmstMiles)>0.1 then FafMiles=MesoMiles-(BaseMiles-DmstMiles); else FafMiles=MesoMiles; *** -- use difference between Base/Domestic (b/n LogNode-Dest) for FAF -- ***;
   if Production_zone=Consumption_zone then do; if Production_zone<=&maxUS then FafMiles=MesoMiles; else FafMiles=0; end;
   proc sort; by Production_zone Consumption_zone MinPath; 
    

*--------------------*;
*** -- Rail -- ***;
*--------------------*;
data r1; set test(where=(MinPath in (3,4,13,14) & (Production_zone<=Consumption_zone)));                 *** -- rail not using logistics nodes -- ***;
  proc sort; by Production_zone Consumption_zone MinPath Carrier;
  
data r2; set test(where=(5<=MinPath<=12 or 15<=MinPath<=30));      *** -- rail using logistics nodes -- ***;
  proc sort; by lnode Consumption_zone MinPath Carrier; 
  
data rail(drop=i); set domestic(where=(Carrier not in ("T","")));
  do i=3 to 30; MinPath=i; output; end;
   proc sort nodupkey; by Production_zone Consumption_zone Carrier MinPath;
   
data rail; merge rail(in=hit) baserail; by Production_zone Consumption_zone Carrier; if hit;    
   proc sort; by Production_zone Consumption_zone MinPath Carrier; 

   *** ===== Direct Rail ===== ****;     
data r1(keep=Production_zone Consumption_zone MinPath MesoMiles FafMiles BaseMiles DmstMiles Carrier); merge r1(in=hit) rail; by Production_zone Consumption_zone MinPath Carrier; if hit;
   if abs(BaseMiles-DmstMiles)>0.1 then FafMiles=MesoMiles-(BaseMiles-DmstMiles); else FafMiles=MesoMiles; *** -- use difference between Base and Domestic to calculate final FAF -- ***;
   if Production_zone=Consumption_zone then do; if Production_zone<=&maxUS then FafMiles=MesoMiles; else FafMiles=0; end;

   *** ===== Indirect Rail ===== ****;        
data rail(keep=Consumption_zone MinPath Carrier BaseMiles DmstMiles lnode); set rail(where=(Production_zone<=&maxUS & Production_zone<=Consumption_zone));
   if MinPath in (5,9,15,19,23,27) then lnode=147;
   else if MinPath in (6,10,16,20,24,28) then lnode=148;  
   else if MinPath in (7,11,17,21,25,29) then lnode=149;   
   else if MinPath in (8,12,18,22,26,30) then lnode=150; 
    proc sort nodupkey; by lnode Consumption_zone MinPath Carrier;

data r2; merge r2(in=hit) rail; by lnode Consumption_zone MinPath Carrier; if hit; 
   if abs(BaseMiles-DmstMiles)>0.1 then FafMiles=MesoMiles-(BaseMiles-DmstMiles); else FafMiles=MesoMiles; *** -- use difference between Base/Domestic (b/n LogNode-Dest) for FAF -- ***;
   if Production_zone=Consumption_zone then do; if Production_zone<=&maxUS then FafMiles=MesoMiles; else FafMiles=0; end;
    keep Production_zone Consumption_zone MinPath Carrier MesoMiles BaseMiles FafMiles DmstMiles;
    proc sort; by Production_zone Consumption_zone MinPath Carrier; 
   
data r; set r1 r2; proc sort nodupkey; by Production_zone Consumption_zone MinPath;   
  
 
*--------------------*; 
*** -- Inland Water -- ***; 
*--------------------*;
data w(keep=Production_zone Consumption_zone MinPath MesoMiles FafMiles); set test(where=(MinPath<=2));
  FafMiles=MesoMiles;                      *** -- assume all Inland Water miles are domestic -- ***;


*--------------------*;  
*** -- Air -- ***;  
*--------------------*; 
data a; set test(where=(47<=MinPath<=50));
  FafMiles=MesoMiles;
  
data a1; set a(where=(Production_zone>&maxUS or Consumption_zone>&maxUS));    *** -- international flights -- ***;
  Foreign=max(Production_zone,Consumption_zone);
    proc sort; by Foreign;

 *** ==== Determine U.S. Location Nearest Foreign Country (point X), Use GCD Miles between point X and Domestic Origin/Destination === ***
 *** ==== as FafMiles between Domestic Location and Foreign Location.  Can be improved upon later === ***;
proc import datafile=in1 out=grtcirc dbms=csv replace;
data greatcirc(keep=Production_zone Consumption_zone GCD); set grtcirc(where=(Production_zone<=&maxUS & Consumption_zone>&maxUS));
   proc sort; by Consumption_zone GCD;
data greatcirc(drop=GCD); set greatcirc; by Consumption_zone GCD;
  if first.Consumption_zone;
  rename Production_zone=Domestic Consumption_zone=Foreign;   *** -- Domestic is nearest US zone -- ***;
  
data a1; merge a1(in=hit) greatcirc; by Foreign; if hit; 
   Local=min(Production_zone,Consumption_zone);
     proc sort; by Local Domestic;     *** -- use this distance as Domestic distance -- ***;
   
data domstc(rename=(Production_zone=Local Consumption_zone=Domestic GCD=Dmstc_GCD)); set grtcirc(where=(Production_zone<=&maxUS & Consumption_zone<=&maxUS)); 
  keep Production_zone Consumption_zone GCD;
  proc sort; by Local Domestic;

data a1; merge a1(in=hit) domstc; by Local Domestic; if hit;   
    FafMiles=Dmstc_GCD;
	proc sort; by Production_zone Consumption_zone MinPath;
	
data a; merge a(in=hit) a1; by Production_zone Consumption_zone MinPath; if hit;


*--------------------*; 
*** -- International Water -- ***; 
*--------------------*;
data w2(keep=Production_zone Consumption_zone MinPath MesoMiles FafMiles); set test(where=(MinPath>=51));
  if Production_zone<=&maxUS & Consumption_zone<=&maxUS then FafMiles=MesoMiles;   *** -- use distances between domestic ports -- ***;
  
data w3; set w2(where=(Production_zone>&maxUS or Consumption_zone>&maxUS));        *** -- international shipments -- ***;
  Foreign=max(Production_zone,Consumption_zone);
    proc sort; by Foreign;  
	
 *** ==== Use Same Nearest U.S. Location Methodology (and Same File) as for Air === ***;	
data w3; merge w3(in=hit) greatcirc; by Foreign; if hit; 
   Local=min(Production_zone,Consumption_zone);
     proc sort; by Local Domestic;     *** -- use this distance as Domestic distance -- ***;	

data w3; merge w3(in=hit) domstc; by Local Domestic; if hit;   
    FafMiles=Dmstc_GCD;
	proc sort; by Production_zone Consumption_zone MinPath;
	
data w2; merge w2(in=hit) w3; by Production_zone Consumption_zone MinPath; if hit;

	
data all(keep=Production_zone Consumption_zone MinPath MesoMiles FafMiles Domestic Carrier); set truckdir truckindir r w a w2;
  MesoMiles=round(MesoMiles,0.1);
  FafMiles=round(FafMiles,0.1);  
  Domestic=max(Domestic,0);
  output;
  p=Production_zone; Production_zone=Consumption_zone; Consumption_zone=p; output;
    proc sort nodupkey; by Production_zone Consumption_zone MinPath;

data final(drop=Domestic); set all;	
proc export data=final outfile=out1 dbms=csv replace;

data review; set all(where=(MinPath in (1,47,51,52,53,54)));
proc export data=review outfile="&outdir.\QC_FAF_air_water_miles.csv" dbms=csv replace;

  
  
data qc; set all;  
  *** -- Ensure each record has a positive mileage value -- ***;
   if MesoMiles>0 & FafMiles>0 then delete;
     proc print; title "Bad Mileage values"; 

run;
