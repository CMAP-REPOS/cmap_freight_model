/* STEP2_CREATE_MODEPATH_SKIM_FILE.SAS
      Craig Heither, revised 04-15-2016

	This program creates the following files used in the Meso Freight Model:
		- "data_modepath_skims.csv": provides the cost/time attributes for each of the 54 modepath options between each pair of zones.
		- "data_mesozone_skims.csv": provides CMAP mesozone skim times (in hours) including intrazonal travel, time based on drayage speed as a 
									more accurate estimate of regional travel speed than 60 MPH.
		- "data_modepath_ports.csv": provides file with Domestic zone, Foreign zone and Domestic Port mesozone selected.  Used to determine mode
									(truck/rail) used between Domestic zone and port. 
								


	This version includes a host of modal improvements to support the new Meso model requirements:
		- Indirect truck modes (32 [FTL] & 39 [LTL}) are created for shipments between non-CMAP U.S. (except Hawaii)/Canada/Mexico.
		- Inland waterway mode (1) is created for appropriate non-CMAP shipments.
		- Air mode (47) is created for shipments between non-CMAP locations.
		- International shipping mode is created between all continental U.S. mesozones and Alaska/Hawaii/all foreign countries (see more below).
		
	International shipping mode is created as follows:
		- A list of the top 30 continental U.S. ports (by total import/export tonnage from 2007 FAF) is used as the universe of domestic ports.
		- The domestic ports (with Ocean) are attached to each zone pair.  If the domestic port ocean differs from the destination port ocean, extra distance is added
			to account for using the Panama Canal:
				- assign Panama as intermediate destination for GCD (domestic port to Panama)
				- assign Panama as intermediate destination for GCD (Panama to destination port)
				- sum total distance
		- Determine "Best" domestic port to use for each destination:
				- assume it minimizes total generalized cost (cost + time)
				- total time is comprised of:
					- drayage at each end
					- hauling the shipment from the origin to the domestic port
						- if rail is available: this time is the average of rail and truck transport times
						- otherwise just truck time is used
				- total cost is comprised of:
					- cost per mile on the cargo ship (ignores transloading fee)
					- cost per mile of drayage at each end
					- cost per mile of hauling the shipment from the origin to the domestic port
						- if rail is available: this cost is the average of rail and truck transport costs
						- otherwise just truck cost is used
				- final generalized cost is 0.4*shipping time + 0.6*shipping cost
				
				
	See VERIFY_COSTS_TIMES.SAS for modepath options available to zonal interchanges.
	
*/
*################################################################################################;  

options linesize= 179 pagesize= 65;

*###=================================================================================###
    -- DEFINE ALL MACRO VARIABLES, FILENAMES AND PATHS --
*###=================================================================================###;
libname	p 'SASLIB';

%let scen=&sysparm;					                            *** -- Scenario number -- ***; 
%let emdir=..\output_data\&scen.\;	                            *** -- Location of Emme skims -- ***;
%let maxskim=34;  				                           	    *** -- Maximum skim matrix to be read in -- ***;
filename CCdist "&emdir.CCdist.txt";                            *** -- Highway centroid connector length -- ***;

filename dports "inputs\domestic_ports.csv";                    *** -- Top 30 US ports for international shipping (based on 2013 tonnage) -- ***;
filename fports "inputs\foreign_ports.csv";                     *** -- Ocean used for each foreign port for international shipping -- ***;

filename gcd "outputs\&sysparm.\data_mesozone_gcd.csv";         *** -- New output file of Great Circle Distances created by CREATE_GCD_FILE.SAS -- ***;
filename mdpath "outputs\&sysparm.\data_modepath_skims.csv";    *** -- New output file of modepath skim costs and times -- ***;
filename mdmile "outputs\&sysparm.\data_modepath_miles.csv";    *** -- New output file of modepath distances consistent with cost and time calculations -- ***;
filename mzskims "outputs\&sysparm.\data_mesozone_skims.csv";   *** -- New output file of CMAP mesozone skim times (hours) -- ***;
filename ports "outputs\&sysparm.\data_modepath_ports.csv";     *** -- New output file of domestic port used by shipments -- ***;


%let MinHwyNode=1401;                                           *** -- smallest highway network (CMAP or National) node number in MFN -- ***;
%let Panama=424;                                                *** -- Mesozone number for Panama (used for Panama Canal distance calculationn in international shipping) -- ***;
%let ExtDrayDom=50;                                             *** -- Assume fixed drayage at each U.S. zone outside CMAP -- ***;
%let ExtDrayFor=100;                                            *** -- Assume fixed drayage at each foreign destination country -- ***;
%let seed=8768;                                                 *** -- Seed value for random function to choose best port -- ***;

******* ==== Zone and Node Ranges ==== *****;  
  %Let	FIZ	=	1;		* First CMAP zone;
  %Let	LIZ	=	132;	* Last CMAP zone;
 * ---;
  %Let	FLN	=	133;	* First logistics node;
  %Let	LLN	=	150;	* Last logistics node;
 * ---;
  %Let	FEZ	=	151;	* First non-CMAP US zone;
  %Let	LEZ	=	273;	* Last non-CMAP US zone; 
 * ---;
 *	---- Logistics nodes ----;
  %Let	FTT	=	133;	* First truck terminal;
  %Let	LTT	=	139;	* Last truck terminal;
 * ---;
  %Let	FAT	=	141;	* First airport;
  %Let	LAT	=	144;	* Last airport;
 * ---;
  %Let	FWT	=	145;	* First water port;
  %Let	LWT	=	146;	* Last water port;
 * ---;
  %Let	FRT	=	147;	* First rail terminal;
  %Let	LRT	=	150;	* Last rail terminal;
******* ============================== *****;  

******* ====   Handling Charges   ==== *****;  
 ** -- The following costs are in DOLLARS per TON -- ** ;
  %let BulkHandFee=2 ; 	  ** - Handling charge for bulk carload/water goods (PER TON) - initial CS BulkHandFee=1 **;
  %let WDCHandFee=15 ; 	  ** - Handling charge at warehouse/distribution center (PER TON) - **;
  %let IMXHandFee=15 ; 	  ** - Handling charge for IMX lift (PER TON, assuming about $500/lift) - **;
  %let TloadHandFee=10 ;  ** - Handling charge for transloading (at int'l. ports only)(PER TON) - **;
  %let AirHandFee=20 ; 	  ** - Handling charge for air cargo (PER TON) - **;

******* ====   Linehaul Charges   ==== *****;    
 ** -- In general, truckers charge about $1.75/mile for long haul trucking. 
	Based on this, the cost of truck (assuming 53', 60000 lbs) is $1.75/(30 tons*miles) = $0.06/ton-mile. -- **;

 ** -- Air cost: used UPS website to calculate cost of shipping 100 lbs about 4000 miles (Chicago to London). 
	Overnight costs about $1000. About five days costs $700.
	Use average ($850) --> Deduct $100 for pickup/delivery trucking at each end --> $3.75 per ton-mile. -- **;

 ** -- The following costs are in DOLLARS per TON-MILE -- ** ;
 **** Heither 03-08-2016: original model used threshold of 85 tons to allow rail carload or ship modes ;
 ****          Weight never exceeds 75000 lbs in current Meso model so threshold reduced to that amount  ;
 ****          Due to lowered threshold, reduce competitive advantage of carload & water rates accordingly;
  %let WaterRate	=0.005;   ***  rate for International Water;
  %let WaterRate2	=0.03;    ***  rate for Inland Water, initial WaterRate=0.005;
  %let CarloadRate=0.06;      ***  initial CarloadRate=0.03;
  %let IMXRate	=0.04;       
  %let AirRate	=3.75;
 * -- LTL/FTL for 53' truck: -- *;
  %let LTL53rate	=0.08; 
  %let FTL53rate	=0.08;
 * -- LTL/FTL for 40' truck (i.e., hauling a container from the port): -- *;
  %let LTL40rate	=0.10; 
  %let FTL40rate	=0.10;
  
  %let ExpressSurcharge=1.50;  ** -- Surcharge for direct/express trucking (no stops) -- **;
  
******* ====      Speeds      ==== *****;   
  %let WaterMPH=5;
  %let RailMPH=30;
  %let LHTruckMPH=65;  ** -- line haul truck speed: 03-08-2016 try 65mph -- **;
  %let DrayTruckMPH=45;
  %let AirMPH=500;

******* ==== Handling time (in hours) at logistics nodes ==== *****; 
  %let BulkTime=72 ; 
  %let WDCTime=24 ;    
  %let IMXTime=24 ;
  %let TloadTime=12 ;  ** -- Transload time (international shipments only) -- **;
  %let AirTime=1 ;    

*################################################################################################;  

*###=================================================================================###
    -- PROCESS EMME SKIMS TO CREATE TRANSPORT AND LOGISTICS PATHS --
*###=================================================================================###;
%let i=1;
%macro ReadSkims;

   %do %while (&i le &maxskim);
      *** -- Each set of four skim matrices represents a mode-specific metric -- ***;
	  %if (&i=1 | &i=5 | &i= 9 | &i=13 | &i=17 | &i=21 | &i=25) %then %let LOS=tt;		** -- travel time;
      %if (&i=2 | &i=6 | &i=10 | &i=14 | &i=18 | &i=22 | &i=26) %then %let LOS=ivtt;	** -- in-vehicle travel time;
	  %if (&i=3 | &i=7 | &i=11 | &i=15 | &i=19 | &i=23 | &i=27) %then %let LOS=egrt;	** -- egress time; ** NOT USED;
	  %if (&i=4 | &i=8 | &i=12 | &i=16 | &i=20 | &i=24 | &i=28) %then %let LOS=poe;		** -- point of entry; ** NOT USED;
	  run;
      *** -- Each set of skim matrices represents a specific rail carrier or mode -- ***;
	  %if &i>=1 & &i<=4 %then %let OP=B;
	  %if &i>=5 & &i<=8 %then %let OP=U;
	  %if &i>=9 & &i<=12 %then %let OP=X;
	  %if &i>=13 & &i<=16 %then %let OP=N;
	  %if &i>=17 & &i<=20 %then %let OP=cp;
	  %if &i>=21 & &i<=24 %then %let OP=cn;
	  %if &i>=25 & &i<=28 %then %let OP=K;
	  %if &i=31 %then %do;
	    %let OP=T;  ** -- truck;  %let LOS=tt; %end;
	  %if &i=32 %then %do;
	    %let OP=W;  ** -- water;  %let LOS=tt; %end;	
	  %if &i=33 %then %do;
	    %let OP=T;  ** -- truck;  %let LOS=ivtt; %end;
	  %if &i=34 %then %do;
	    %let OP=W;  ** -- water;  %let LOS=ivtt; %end;		
	 run;
   %put &OP &LOS;
      data mf&i(keep=o dest &OP&LOS); infile "&emdir.mf&i..in" missover dlm=' :' firstobs=5;
	    input o d1 v1 d2 v2 d3 v3;
		  dest=d1; &OP&LOS=v1; output;
		  dest=d2; &OP&LOS=v2; output;
		  dest=d3; &OP&LOS=v3; output;
		  proc sort nodupkey; by o dest;
		run;  
		
		data mf&i; set mf&i(where=(o>0 & dest>0)); run;
		
	  %let i=%eval(&i+1);
	  **-- The egress time and poe information for each mode are not actually used so do not read them in --**;
	  %if (&i=3 | &i=7 | &i=11 | &i=15 | &i=19 | &i=23) %then %let i=%eval(&i+2);
	  %if (&i=27) %then %let i=%eval(&i+4);  **-- jump ahead to read truck and water matrices--**;
   %end;
  run;

%mend ReadSkims;
%ReadSkims
/* end of macro */
**------------------------------------------------------------------------** 
%let i=1; %let j=2;
%macro FixIvtt;
   %do %while (&i le 4);  **25;

      *** -- Each set of skim matrices represents a specific rail carrier or mode -- ***;
	  %if &i=1 %then %let OP=B;
	  %if &i=5 %then %let OP=U;
	  %if &i=9 %then %let OP=X;
	  %if &i=13 %then %let OP=N;
	  %if &i=17 %then %let OP=cp;
	  %if &i=21 %then %let OP=cn;
	  %if &i=25 %then %let OP=K;
	  
	  data mf&i; merge mf&i mf&j; by o dest;
	    if &OPivtt=. then output; proc print;
     run;
	 
	 %let i=%eval(&i+4);
   %end;
  run;
%mend FixIvtt;
%FixIvtt
/* end of macro */
**------------------------------------------------------------------------** 
run;

data skims; merge mf1-mf2 mf5-mf6 mf9-mf10 mf13-mf14 mf17-mf18 mf21-mf22 mf25-mf26 mf31-mf34; by o dest;

**------------------------------------------------------------------------** 
DEVELOP SKIMS IN OUTBOUND DIRECTION (then apply to ei/ie alike.
	In doing so, we assume that rail/truck/water/air service links
	and routes are the same in both directions.
	Currently, we CAN apply different LOS by direction, though, which is 
	desirable for computing different rates for backhaul.
**------------------------------------------------------------------------** ;
data skims; set skims;
 array fixmiss{9} Bivtt Uivtt Xivtt Nivtt cpivtt cnivtt Kivtt Tivtt Wivtt;
 array fixmiss2{9} Btt Utt Xtt Ntt cptt cntt Ktt Ttt Wtt;
    do i=1 to 9; fixmiss{i}=max(0,fixmiss{i}); end;
    do i=1 to 9; fixmiss2{i}=max(0,fixmiss2{i}); end;
	
  if max(Bivtt,Uivtt,Xivtt,Nivtt,cpivtt,cnivtt,Kivtt)>0 then RAvail=1; else RAvail=0;
  if Wtt=0 then WAvail=0; else WAvail=1;
  if o<dest then output;   *** -- second direction and intrazonals will be added later -- ***;  
  
  data check; set skims(where=(o=151 & dest=153));  proc print; title "Check indirect rail";
  **------------------------------------------------------------------------** 
      -- Subset skims into different groups --  
  **------------------------------------------------------------------------** ;
******* ==== Other U.S. Zones - from Emme Skims ==== *****;
data outside; set skims(where=(o>=&FEZ and dest>=&FEZ));
  Source='II_IE_Direct';
  if o<dest then output;   *** -- second direction and intrazonals will be added later -- ***;  
  
******* ==== I-I / I-E with no stop at internal logistics node ==== *****;
data II_IE_Direct; set skims(where=(o<=&LIZ and (dest<=&LIZ or dest>=&FEZ)));
  Source='II_IE_Direct';  
  
******* ==== I-E with one stop at an internal logistics node ==== *****;
   *** (Keep TRUCK only - assumption is that only trucks carry dray freight);
data I_LogNodes(keep=o LogNode ILNTtt); set skims(where=(o<=&LIZ and &FLN<=dest<=&LLN));
  LogNode=dest;
  ILNTtt=Ttt; *** -- I-LogNode Ttt;

******* ==== I-E with one stop at an external logistics node ==== *****;
data LogNodes_E(keep=LogNode dest Btt--Kivtt Wtt LNETtt RAvail WAvail); set skims(where=(&FLN<=o<=&LLN and dest>=&FEZ));
  LogNode=o;
  LNETtt=Ttt; *** -- LogNode-E Ttt;
    
******* ==== Join Internal-Drayage-Time data to External-LineHaul-Time data ==== *****;  
   *** (This code enumerates the potential options: all combinations of CMAP origins-logistics nodes-all non-CMAP US/Canada/Mexico destinations);
data IELN1(drop=I_LogNode); set I_LogNodes(rename=(LogNode=I_LogNode));  
  do i=1 to nObs;
	set LogNodes_E nobs=nObs point=i;
	if LogNode=I_LogNode then output;
  end;
  
******* ==== Add II_IE_Direct then pick best rail carrier ==== *****;  
data IELN1(drop=i); set IELN1 II_IE_Direct outside; 
 attrib Carr length=$2;
    **-- select the rail carrier with the lowest IVTT --**;   
  array	rivt[7];
   do i=1 to 7;
	rivt[i]=9999999; 
   end;
   
  if Bivtt>0 then rivt[1]=Bivtt;
  if Uivtt>0 then rivt[2]=Uivtt;
  if Xivtt>0 then rivt[3]=Xivtt;
  if Nivtt>0 then rivt[4]=Nivtt;
  if CPivtt>0 then rivt[5]=CPivtt;
  if CNivtt>0 then rivt[6]=CNivtt;
  if Kivtt>0 then rivt[7]=Kivtt;
  
  array railtt{7} Btt Utt Xtt Ntt CPtt CNtt Ktt; 

  RIVT_Min = 9999999;
  ChosCarr = .; ChRtt=.; ChRivtt=.; 
  if RAvail=1 then do i=1 to 7;
	if rivt[i]<RIVT_Min then do;
		RIVT_Min=rivt[i];
		ChRtt=railtt[i];
		ChRivtt=rivt[i];
		ChosCarr=i; 
	end;
  end;

  if ChosCarr=1 then Carr='B';
  if ChosCarr=2 then Carr='U';
  if ChosCarr=3 then Carr='X';
  if ChosCarr=4 then Carr='N';
  if ChosCarr=5 then Carr='CP';
  if ChosCarr=6 then Carr='CN';
  if ChosCarr=7 then Carr='K';

data ieln2; set ieln1 ;  ***-- Here Ttt=Direct TT between O & Dest. --**;
 keep o LogNode dest ILNTtt LNETtt Wtt Ttt RAvail WAvail ChosCarr Carr ChRtt ChRivtt Source;
  proc sort; by dest;		
	 
  ***-- Centroid connector distances --***;
  ** (Note: DestCCmi is the Centroid Connector length (in miles) at the DESTINATION END) **;
  ** (Heither Note: the MFN now has separate highway and rail centroid connectors - we need just highway for this)**;  
data DestCCmi(drop=node); infile CCdist firstobs=2 missover;
  input dest node DestCCmi;
  if node>=&MinHwyNode;
   proc sort nodupkey; by dest;
 
data IELN2; merge IELN2(in=hit) DestCCmi; by dest; if hit;

  ****
    Split into 2 datasets:
	  1) No logistics stop in CMAP region (applies only to truck and rail operations, not to water/air.
		    I.e., assume water/air travel always involves truck drayage within the CMAP region. Also
			applies to other U.S. zones outside CMAP
	  2) 1 logistics stop in CMAP region: Assume that drayage to the logistics node is always by truck.
  ****;  
data i00(keep=source Tdist00 Rdist00 RAvail00 o dest Carr); set ieln2(where=(source='II_IE_Direct'));
  Tdist00=Ttt;
  Rdist00=ChRtt;
  RAvail00=RAvail;
   proc sort; by o dest;
 
data ieln2; set ieln2;
 attrib od length=$8;
  od=compress(o||"_"||dest);
  proc sort; by od LogNode;
 
data temp; set ieln2(where=(source=''));
  if &FTT<=LogNode<=&LTT then do; LHmiles=LNETtt-DestCCmi; LHcheck=LHmiles/LNETtt; end;
  if &FAT<=LogNode<=&LAT then do; LHmiles=LNETtt-DestCCmi; LHcheck=LHmiles/LNETtt; end;          *** -- GCD FOR AIR DISTANCE WILL BE SWAPPED IN LATER -- ***;
  if &FWT<=LogNode<=&LWT & Wtt>0 then do; LHmiles=Wtt; LHcheck=LHmiles/Wtt; end;        *** --  DO NOT subtract DestCCmi - Heither 10-04-2016 -- ***;
  if &FRT<=LogNode<=&LRT then do; LHmiles=ChRtt; LHcheck=LHmiles/ChRtt; end;			*** -- originally ChRivtt but changed to travel time for consistency with other modes -- ***;						
   proc sort; by od;
 
data check; set temp(where=(0<=LHcheck<=0.25));
  proc print; title "Review LHmiles"; 
 
data i10; set temp; by od;
 length Carr&FLN-Carr&LLN $2;
 array IntDray[&FLN:&LLN] IntDray&FLN-IntDray&LLN; 
 array LineHaul[&FLN:&LLN] LineHaul&FLN-LineHaul&LLN; 
 array RlCarr[&FLN:&LLN] Carr&FLN-Carr&LLN; 

  retain IntDray LineHaul RlCarr;
	if First.OD then do;
		do i=&FLN to &LLN;  
			IntDray{i} = .; 
			LineHaul{i}= .;
			RlCarr{i}= "";
		end; 
	end;
  IntDray{LogNode} = ILNTtt;
  LineHaul{LogNode} = LHMiles;
  RlCarr{LogNode} = Carr;    ** -- rail carrier mode for indirect shipments -- **;
  if dest<=&LEZ then ExtDray=DestCCmi; else ExtDray=&ExtDrayFor;	*** -- ExtDray distance is fixed, use fixed esitmate for foreign  zones -- ***;
  if Last.OD then output;
  keep o dest od IntDray&FLN--LineHaul&LLN ExtDray Carr&FRT-Carr&LRT;
   proc sort; by o dest; 
	  
   ****
    Merge i00 (0 stops internal, 0 stops external)
    with i10 (1 stop internal, 0 stops external).
    Process potential external stops "on the fly".
  ****;
data i(drop=Source); merge i00 i10; by o dest;

  ****
    Convert LOS values into path costs with the following path type names:
     - F=Full Truckoad
     - L=LTL
     - C=Carload
     - I=IMX (Intermodal)
	 
    AIxyzJ = modes on External Dray (x), Line Haul (y), and Internal Dray (z) portions
     - I = 0/1 = 0 for no external handling stop, 1 for 1 external handling stop
     - J = 0 or 133-150 = ID of internal handling stop (0 if no internal handling stop)
     - A: C=cost (in $), T=time.

    ASSUMPTIONS:
     - each 53' truck carries a maximum of 60,000 lbs (30 tons)
     - each 58' rail carload carries a maximum of 65,000 lbs (about 32 tons)
     - each 40' container carries 45,000 lbs (~75% of 53' truck) (about 22 tons)
     - all IMX is single-stack, no double-stack 
     - IMX cars carry 40' containers
  ****;

******* ==== Get Emme Skimmed truck Distance for all Zones - Hold for later ==== *****;  
data emskim(keep=o dest EmDist); set i;
  EmDist=TDist00; output;
  c=o; o=dest; dest=c; output;
   proc sort nodupkey; by o dest;

  
  **------------------------------------------------------------------------** 
      -- Create air skims for all zone pairs --  
  **------------------------------------------------------------------------** ;  
******* ==== Get Linehaul between CMAP airports and every non-CMAP zone ==== *****;  
proc import datafile=gcd out=airgcd dbms=csv replace;
data cmapair; set airgcd(where=(Production_zone in (141,142,143,144) & Consumption_zone>=&FEZ));
 proc transpose out=airdist prefix=LineHaul; by Consumption_zone; id Production_zone; var GCD;
 
  ******* ==== Keep for Later Use will Rail and Inland Water modes for non-CMAP zones ==== *****;   
data railwater_gcd; set airgcd(where=(Production_zone=Consumption_zone & &FEZ<=Production_zone<=&LEZ));
  rename Production_zone=o Consumption_zone=dest GCD=RailWtr; 
  keep Consumption_zone GCD;

  
  ******* ==== GROUP 1: CMAP zones to every non-CMAP zone ==== *******;
** create template **;
data cmapo(rename=(Production_zone=o)); set airgcd(where=(Production_zone<=&LIZ));
   keep Production_zone; run;
   proc sort nodupkey; by o;  

data noncmapd(rename=(Production_zone=dest)); set airgcd(where=(Production_zone>=&FEZ));
   proc sort nodupkey; by dest;   
  
proc sql noprint;
    create table cmapair as
           select cmapo.*,
                  noncmapd.dest 
	       from cmapo, noncmapd;  

		******* ==== Data come from two places: ==== *******;
		******* ==== 1: get internal drayage from CMAP Origin to each airport from i ==== *******;		
data part1(keep=o IntDray141-IntDray144); set i(where=(o<=&LIZ & dest>=&FEZ));
  proc sort nodupkey; by o;   *** Internal drayage is constant based on origin and airport;
    
data cmapair; merge cmapair part1; by o;	
  proc sort; by dest;

		******* ==== 2: LineHaul from each airport to destination zone from GCD file ==== *******;
data part2(rename=(Consumption_zone=dest)); set airdist(where=(Consumption_zone>=&FEZ));
   drop _name_; run;
   proc sort nodupkey; by dest;  

data cmapair; merge cmapair part2; by dest;	
  ExtDray=&ExtDrayDom;   *** Assume fixed drayage at each destination;
  proc sort; by o dest;
  
  ******* ==== GROUP 2: non-CMAP U.S. zones to every non-CMAP zone ==== *******;  
** create template **;
data noncmapo(rename=(Production_zone=o)); set airgcd(where=(Production_zone>=&FEZ));
   keep Production_zone; run;
   proc sort nodupkey; by o;  

data noncmapd(rename=(o=dest)); set noncmapo;
proc sql noprint;
    create table ncmapair as
           select noncmapo.*,
                  noncmapd.* 
	       from noncmapo, noncmapd;    

		******* ==== use GCD data to populate fields ==== *******;	
data part1; set airgcd(where=(&FEZ<=Production_zone<=&LEZ & Consumption_zone>=&FEZ));
   rename Production_zone=o Consumption_zone=dest;
   keep Production_zone Consumption_zone GCD; run;
      proc sort nodupkey; by o dest; 
	  
data intra; set part1(where=(o=dest));	  
    **** -- Use intrazonal distance to represent drayage at appropriate end -- ****;
	data drayo(rename=(GCD=IntDray)); set intra;
	    keep o GCD; run;
	data drayd(rename=(GCD=ExtDray)); set intra;
	    keep dest GCD; run;	  
		
data part1; merge part1 drayo; by o;	
   proc sort; by dest;	
data part1; merge part1 drayd; by dest;	
   if dest>&LEZ then ExtDray=&ExtDrayFor;  *** Assume fixed drayage at each destination country;
   proc sort; by o dest;			

       *** == Verify All Drayage Present == ***;
       data check; set part1;
         if IntDray>0 & ExtDray>0 then delete;
         proc print; title "===================="; title2 "ERROR - Drayage Missing";
          title3 "====================";
  		
data ncmapair(drop=GCD IntDray); merge ncmapair part1(in=hit); by o dest; if hit; 
    **** -- In order to allow air travel between origins & destination outside of CMAP, we will repurpose     -- ****;
    **** -- IntDray141 & LineHaul141 to represent air travel between these zones: between external zone pairs -- ****;	
    **** -- this is general air cargo, not cargo using OHare.                                                 -- ****;	
	IntDray141=IntDray; LineHaul141=GCD; 
	
data allair; set cmapair ncmapair;
  ******* ==== USE EMME DATA FOR EXTDRAY WHEN AVAILABLE ==== *******;
  if o<=&LIZ & (dest<=&LEZ or dest in (310,399)) & (dest not in (179,180)) then ExtDray=.;
  if o<=dest then output;   *** -- second direction will be added later -- ***;    
  proc sort nodupkey; by o dest; 
  
  ******* ==== #### Final Combined Data: EMME AND AIR #### ==== *******;  
data i; update i allair; by o dest; 
    **** -- Airport linehaul now reflects GCD -- ****;	 
	
  
  **------------------------------------------------------------------------** 
      -- Create international water skims for all zone pairs --  
  **------------------------------------------------------------------------** ; 
  ***  Need to create new procedures to handle international shipments realistically  
  Start simple:   ;
  
  ******* ==== Create template for all U.S. mesozones to Alaska/Hawaii/all foreign countries ==== *****;  
data us(rename=(Production_zone=o)); set airgcd(where=(Production_zone<=&LIZ or &FEZ<=Production_zone<=&LEZ));  *** -- US mesozones excluding logistics nodes -- ***;
   keep Production_zone; run;
   proc sort nodupkey; by o; 
data foreign(rename=(Production_zone=dest)); set airgcd(where=(Production_zone>&LEZ or Production_zone in (154,179,180)));  *** -- include Alaska/Hawaii as destinations -- ***;
   keep Production_zone; run;
   proc sort nodupkey; by dest;  
proc sql noprint;
    create table intship as
           select us.*,
                  foreign.* 
	       from us, foreign;    
 		   
  ******* ==== Attach Ocean to Foreign Port ==== *******;		   
proc import datafile=fports out=fport dbms=csv replace;		   
data fport(rename=(Mesozone=dest Port_ocean=Port_d)); set fport;
  drop Region Continent; 
data fport; set fport;
  if Port_d="AP" then do;   *** Create separate observations for countries with access to both oceans ***;
     Port_d="Atlantic"; output;
     Port_d="Pacific"; output; 
  end; 
  else do; output; end;
  
data fport2; set fport(obs=3);
    ** -- add Alaska and two Hawaiian zones as destinations for international shipping -- **;
  if _n_=1 then do; dest=179; Port_d="Pacific"; Location="Honolulu"; end; 	
  if _n_=2 then do; dest=180; Port_d="Pacific"; Location="Hawaii Rem"; end; 
  if _n_=3 then do; dest=154; Port_d="Pacific"; Location="Alaska"; end; 
data fport; set fport fport2; proc sort; by dest;		

proc sql noprint;
    create table intship2 as
           select intship.*,
                  fport.Port_d,Location 
	       from intship, fport   
	       where intship.dest=fport.dest
		   order by o,dest;
		   
  ******* ==== Attach All Domestic Ports to each Zone Pair ==== *******;	
proc import datafile=dports out=dport dbms=csv replace;		  
proc sql noprint;
    create table intship3 as
           select intship2.*,
                  dport.* 
	       from intship2, dport   
		   order by o,dest;
 	   
  ******* ==== PART 1. Attach GCD between Port and Foreign Country ==== *******;
  ******* ==== if they are on different oceans, add extra distance for using Panama Canal ==== *******;
data intship3; set intship3;
    **** -- Use port at Honolulu for shipments between Hawaii-Foreign Countries & Hawaii-Alaska -- ****;	
    **** --  (Shipments between Hawaii & Continental U.S. will use mainland ports - we already know it will use Honolulu at that end but we want to know the mainland port used) -- ****;
   if o in (179,180) & (dest in (154,179,180) or dest>&LEZ) then do; Port_mesozone=179; Port_name="Honolulu, HI"; Ocean="Pacific"; end;
 
    **** -- Use same logic for shipments from Alaska -- ****;	 
   if o=154 & (dest in (154,179,180) or dest>&LEZ) then do; Port_mesozone=154; Port_name="Anchorage, AK"; Ocean="Pacific"; end;      
    proc sort nodupkey; by o dest Port_mesozone;   *** -- remove unnecessary duplicate entries for shipments from Hawaii & Alaska -- ***;
			   
data intship3; set intship3;
   if Port_d=Ocean then Pan_flag=0; else Pan_flag=1;
   Production_zone=Port_mesozone;               *** -- assign port as origin for GCD -- ***; 
   Consumption_zone=dest;                       *** -- assign foreign country as destination for GCD -- ***; 
   if Pan_flag=0 then output;
   else if Pan_flag=1 then do;
     Consumption_zone=&Panama; output;          *** -- assign Panama as intermediate destination for GCD (port to Panama) -- ***; 
     flag2=1; Production_zone=&Panama;          *** -- assign Panama as intermediate destination for GCD (Panama to dest) -- ***; 
	 Consumption_zone=dest; output;
   end;	 
   proc sort; by Production_zone Consumption_zone;
   
data shipgcd(keep=Production_zone Consumption_zone GCD); set airgcd;   
   proc sort; by Production_zone Consumption_zone;
   
data intship3; merge intship3(in=hit) shipgcd; by Production_zone Consumption_zone; if hit;  
  proc sort; by o dest Port_mesozone Pan_flag;
 
  ******* ==== collapse two-part ship distances into a single summed value ==== *******;
proc summary nway data=intship3; var GCD; class o dest Port_mesozone Pan_flag; id Location Port_name; output out=intship4 sum=;  
  proc sort data=intship4; by o dest Port_mesozone GCD;
data intship4; set intship4; by o dest Port_mesozone;
  if first.Port_mesozone;      *** -- eliminate unnecessary second ocean calculation for ports connecting to Canada/Mexico -- ***;  
   
  ******* ==== PART 2. Attach Truck/Rail LH between Origin and Port ==== *******;
  ******* ==== then add Internal & External drayage ==== *******; 
data toport(rename=(dest=Port_mesozone Tdist00=Tdist Rdist00=Rdist)); set i(where=(o<=&LEZ & (dest<=&LEZ or dest in (310,399))));
  keep o dest Tdist00 Rdist00;
  
  ** -- create Linehaul value between Hawaiian zones -- **;
data hawaii(rename=(Production_zone=o Consumption_zone=Port_mesozone)); set airgcd(where=(Production_zone in (179,180) &  Consumption_zone  in (179,180)));
  DrayFix=GCD;
  keep Production_zone Consumption_zone DrayFix; run;
  proc sort nodupkey; by o Port_mesozone;
  
data toport(drop=c); set toport; 
   if o=Port_mesozone then Tdist=max(Tdist,0);      *** -- force a value so the minimum time calculation works -- ***;
   output;
   c=o; o=Port_mesozone; Port_mesozone=c; output;   *** -- ensure both directions between zones are available -- ***; 
   proc sort nodupkey; by o Port_mesozone; 
      
proc sort data=intship4; by o Port_mesozone;  
data intship4(drop=_type_ _freq_); merge intship4(in=hit) toport hawaii; by o Port_mesozone; if hit;
  if Tdist=. & DrayFix>0 then Tdist=DrayFix;        *** -- set Linehaul value between Hawaiian zones -- ***;
  
    ** -- attach internal drayage, add external drayage -- **;
data intdr; set airgcd(where=(Production_zone=Consumption_zone & Production_zone<=&LEZ));
   rename Production_zone=o GCD=InDray;
   keep Production_zone GCD; run;
   
data intship4; merge intship4(in=hit) intdr; by o; if hit;  
   ExDray=&ExtDrayFor;

  ******* ==== PART 3. Determine 'Best' Domestic Port to Use ==== *******;
  ******* ==== for simplicity, start by assuming it is the one that minimizes overall travel time ==== *******;	  
  ******* ==== overall travel time: time on ship (port to dest) plus truck time(o to port + drayage at each end) ==== *******;	  
data intship4(drop=temp); set intship4;	
  if Rdist>0 then haul_toPort=(Tdist/&LHTruckMPH)*0.5 + (Rdist/&RailMPH)*0.5;
  else haul_toPort=Tdist/&LHTruckMPH;
  MinShipTime=GCD/&WaterMPH + haul_toPort + (InDray+ExDray)/&DrayTruckMPH;  
  
    ** -- Cost: assume no transloading -- **; 
  if Rdist>0 then cost_toPort=(Tdist*&FTL53rate)*0.5 + (Rdist*&IMXRate)*0.5;
  else cost_toPort=Tdist*&FTL53rate;	
  MinShipCost=(InDray+ExDray)*&FTL53rate + cost_toPort + GCD*&WaterRate;  *** -- ignore transload handling fee -- ***;	
  MinShipTime=(0.4*MinShipTime) + (0.6*MinShipCost);	
  if MinShipTime=. then delete;
  output;
  temp=o; o=dest; dest=temp; output;
  proc sort; by o dest MinShipTime;
  
data intship4; set intship4; by o dest MinShipTime;	  
  retain ord 0;
   ord+1;
   if first.dest then ord=1;
     
data intship4; set intship4(where=(ord<=5));    *** -- limit to top 5 ports -- ***; 
  proc summary nway; var ord; class o dest; output out=x max=maxord;     
data x(drop=_type_ _freq_); set x; choice=ranuni(&seed);
   
data intship4; merge intship4 x; by o dest; 
  prob=ord*(1/maxord);	  
  if prob>choice;                       *** -- keep viable choices for movements -- ***;
  proc sort; by o dest ord;
  
data intship4(drop=ord prob maxord choice); set intship4; by o dest ord;		
  if first.dest;	
 
  ******* ==== #### Final Combined Data: EMME AND AIR AND INTL WATER #### ==== *******;  
data i; merge i(in=hit) intship4; by o dest; if hit;
  if o>&LEZ & dest>&LEZ then delete;    *** -- ensure no foreign-to-foreign movements -- ***;


  **------------------------------------------------------------------------** 
      -- Create intrazonal skim data for U.S. mesozones --  
  **------------------------------------------------------------------------** ; 
data intdr; set intdr(where=(o<&FLN or o>&LLN));
  dest=o; Tdist00=InDray;
  InDray=InDray/2; ExtDray=InDray;   ***-- assume no intrazonal rail movements within CMAP --***;
  if o>&LIZ then do;
    RAvail00=1; Rdist00=Tdist00*1.25;     ***-- assume all non-CMAP U.S. mesozones have rail access --***;
  end;

  
  **------------------------------------------------------------------------** 
      -- Create inland waterway skim data for non-CMAP U.S. mesozones --  
  **------------------------------------------------------------------------** ; 
data inland(rename=(Wtt=Waterway)); set mf32(where=(o>=&FEZ & dest>=&FEZ));  
  proc sort; by o dest;
  
  
*###=================================================================================###
    -- COMBINE ALL SKIMS AND CREATE TRANSPORT AND LOGISTICS PATHS --
*###=================================================================================###;	 
data i; merge i intdr; by o dest;

data i(drop=EmDist); merge i(in=hit) emskim inland; by o dest; if hit;
  if TDist00=. & EmDist>0 then TDist00=EmDist;

/* *****  
data check; set i(where=(ExtDray/Tdist00>0.75));
   proc print; title "Tdist-ExtDray check"; 
 
data check; set i(where=(ExtDray/Rdist00>0.75));
   proc print; title "Rdist-ExtDray check";   
*** */
  
proc sort data=i; by dest;  
data i; merge i(in=hit) railwater_gcd; by dest; if hit;   
  proc sort; by o dest; 

data i; set i;  
****
Calculate times, costs, etc. for the various transport paths.
Modified procedures are used to calculate Overseas Air skims
and Overseas Water shipments that are (or are not) transloaded.
Note: Cost/time of water-based ocean shipping is not included.
****;

  array IntDray[&FLN:&LLN] IntDray&FLN-IntDray&LLN; 
  array LineHaul[&FLN:&LLN] LineHaul&FLN-LineHaul&LLN;   * Note: External dray distance is fixed. ;
  
******* ==== Air & water time and cost ==== *****;    
  array cA[&FAT:&LAT] cA&FAT-cA&LAT;
  array tA[&FAT:&LAT] tA&FAT-tA&LAT;
  array mlA[&FAT:&LAT] mlA&FAT-mlA&LAT;   *** -- Air mode complete mileage -- ***;
  array cW[&FWT:&LWT] cW&FWT-cW&LWT;
  array tW[&FWT:&LWT] tW&FWT-tW&LWT; 
  array mlW[&FWT:&LWT] mlW&FWT-mlW&LWT;   *** -- Inland Water mode complete mileage -- ***;

   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;   
   ** ======== AIR ======== **;
   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;     
  if o<=&LEZ then do;
    if o<=&LIZ then do;   *** --- CMAP origin: calculate 4 airport options --- ***;
       do i=&FAT to &LAT;
        **-- Cost of shipping by air per ton --**;
         cA[i] = &LTL53rate*(IntDray[i]+ExtDray) + LineHaul[i]*&AirRate + 2*&AirHandFee;
         tA[i] = (IntDray[i]+ExtDray)/&DrayTruckMPH + LineHaul[i]/&AirMPH + 2*&AirTime;   *** --- Heither: original calculation erroneously multiplied by AirHandFee not AirTime --- ***;
		 mlA[i] = (IntDray[i]+ExtDray) + LineHaul[i];
       end;
	end;
    else do;   *** --- non-CMAP U.S. origin: calculate only 1 airport option --- ***;
       do i=&FAT to &FAT;
        **-- Cost of shipping by air per ton --**;
         cA[i] = &LTL53rate*(IntDray[i]+ExtDray) + LineHaul[i]*&AirRate + 2*&AirHandFee;
         tA[i] = (IntDray[i]+ExtDray)/&DrayTruckMPH + LineHaul[i]/&AirMPH + 2*&AirTime;   
		 mlA[i] = (IntDray[i]+ExtDray) + LineHaul[i];
       end;
	end;
  end;   

   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;   
    ** ======== CMAP/INLAND WATER ======== **;
   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;  
  if o<=&LIZ then do;   *** --- CMAP origin: calculate 2 inland water options --- ***;   
     if RailWtr>0 then ExtDray=RailWtr;
     do i=&FWT to &LWT;   **-- Cost of shipping by water per ton --**;
	   cW[i] = &FTL53rate*(IntDray[i]+ExtDray) + LineHaul[i]*&WaterRate2 + 2*&BulkHandFee;
	   tW[i] = (IntDray[i]+ExtDray)/&DrayTruckMPH + LineHaul[i]/&WaterMPH + 2*&BulkTime;
	   mlW[i] = (IntDray[i]+ExtDray) + LineHaul[i];
     end;
  end;	 
  else do;              *** --- non-CMAP U.S. origin: calculate only 1 inland water option --- ***;
     if RailWtr>0 then ExtDray=RailWtr;
     do i=&FWT to &FWT;   
	   if InDray=. then IWDray=&ExtDrayDom; else IWDray=InDray;
	   cW[i] = &FTL53rate*(IWDray+ExtDray) + Waterway*&WaterRate2 + 2*&BulkHandFee;
	   tW[i] = (IWDray+ExtDray)/&DrayTruckMPH + Waterway/&WaterMPH + 2*&BulkTime;
	   mlW[i] = (IWDray+ExtDray) + Waterway;
     end;  
  
  end;

   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;  
   ** ======== TRUCK AND RAIL ======== **;   
   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;  
   *### -- 0 Stops -- ###*;
   **** Apply the direct/express surcharge where appropriate ****;
  tCarload = Rdist00/&RailMPH ; 
  cCarload = &ExpressSurcharge*Rdist00*&CarloadRate ;
  tIMX = Rdist00/&RailMPH ; 
  cIMX = &ExpressSurcharge*Rdist00*&IMXRate ; 
  tFTL = Tdist00/&LHTruckMPH ; 
  cFTL = &ExpressSurcharge*Tdist00*&FTL53rate ;
  tLTL = Tdist00/&LHTruckMPH ; 
  cLTL = &ExpressSurcharge*Tdist00*&LTL53rate ;

   *### -- 1 Stop  (assume stops only happen with mode switching) -- ###*;
    ***-- 1 external stop, 0 internal stops --*** ;
   *### -- Heither, 10-03-2016: Based on how other modal costs are calculated, I do not believe Linehaul should be reduced by ExtDray --*** ### ;
       *  FTL on ext.............Carload from ext loading to int.;
/*   t1fc0 = ExtDray/&DrayTruckMPH + (Rdist00-ExtDray)/&RailMPH + 1*&BulkTime; 
   t1fi0 = ExtDray/&DrayTruckMPH + (Rdist00-ExtDray)/&RailMPH + 1*&BulkTime ; 
   t1Li0 = ExtDray/&DrayTruckMPH + (Rdist00-ExtDray)/&RailMPH + 1*&WDCTime ; 
   t1Lf0 = ExtDray/&DrayTruckMPH + (Tdist00-ExtDray)/&LHTruckMPH + 1*&WDCTime ; 
   t1LL0 = ExtDray/&DrayTruckMPH + (Tdist00-ExtDray)/&LHTruckMPH + 1*&WDCTime ; 
   c1fc0 = ExtDray*&FTL53rate + (Rdist00-ExtDray)*&CarloadRate + 1*&BulkHandFee ; 
   c1fi0 = ExtDray*&FTL53rate + (Rdist00-ExtDray)*&IMXRate + 1*&BulkHandFee ; 
   c1Li0 = ExtDray*&LTL53rate + (Rdist00-ExtDray)*&IMXRate + 1*&WDCHandFee ; 
   c1Lf0 = ExtDray*&LTL53rate + (Tdist00-ExtDray)*&FTL53rate + 1*&WDCHandFee ; 
   c1LL0 = ExtDray*&LTL53rate + (Tdist00-ExtDray)*&LTL53rate + 1*&WDCHandFee ;  */

   t1fc0 = ExtDray/&DrayTruckMPH + Rdist00/&RailMPH + 1*&BulkTime; 		*** -- time: minpath 4 -- ***;
   t1fi0 = ExtDray/&DrayTruckMPH + Rdist00/&RailMPH + 1*&BulkTime ; 	*** -- time: minpath 14 -- ***;
   t1Li0 = ExtDray/&DrayTruckMPH + Rdist00/&RailMPH + 1*&WDCTime ; 
   t1Lf0 = ExtDray/&DrayTruckMPH + Tdist00/&LHTruckMPH + 1*&WDCTime ; 
   t1LL0 = ExtDray/&DrayTruckMPH + Tdist00/&LHTruckMPH + 1*&WDCTime ; 
   c1fc0 = ExtDray*&FTL53rate + Rdist00*&CarloadRate + 1*&BulkHandFee ; *** -- cost: minpath 4 -- ***;
   c1fi0 = ExtDray*&FTL53rate + Rdist00*&IMXRate + 1*&BulkHandFee ; 	*** -- cost: minpath 14 -- ***;
   c1Li0 = ExtDray*&LTL53rate + Rdist00*&IMXRate + 1*&WDCHandFee ; 
   c1Lf0 = ExtDray*&LTL53rate + Tdist00*&FTL53rate + 1*&WDCHandFee ; 
   c1LL0 = ExtDray*&LTL53rate + Tdist00*&LTL53rate + 1*&WDCHandFee ;   

   *### -- 0/1 external stops, 1 internal stop -- ###*;
   array t0cf[&FRT:&LRT] t0cf&FRT-t0cf&LRT;
   array t0if[&FRT:&LRT] t0if&FRT-t0if&LRT;
   array t0iL[&FRT:&LRT] t0iL&FRT-t0iL&LRT;
   array t1fcf[&FRT:&LRT] t1fcf&FRT-t1fcf&LRT;
   array t1fif[&FRT:&LRT] t1fif&FRT-t1fif&LRT;
   array t1LiL[&FRT:&LRT] t1LiL&FRT-t1LiL&LRT;
   array mlR[&FRT:&LRT] mlR&FRT-mlR&LRT;          *** -- Rail mode complete mileage -- ***;
   array t0fL[&FTT:&LTT] t0fL&FTT-t0fL&LTT;
   array t0LL[&FTT:&LTT] t0LL&FTT-t0LL&LTT;
   array t1LfL[&FTT:&LTT] t1LfL&FTT-t1LfL&LTT;
   array c0cf[&FRT:&LRT] c0cf&FRT-c0cf&LRT;
   array c0if[&FRT:&LRT] c0if&FRT-c0if&LRT;
   array c0iL[&FRT:&LRT] c0iL&FRT-c0iL&LRT;
   array c1fcf[&FRT:&LRT] c1fcf&FRT-c1fcf&LRT;
   array c1fif[&FRT:&LRT] c1fif&FRT-c1fif&LRT;
   array c1LiL[&FRT:&LRT] c1LiL&FRT-c1LiL&LRT;
   array c0fL[&FTT:&LTT] c0fL&FTT-c0fL&LTT;
   array c0LL[&FTT:&LTT] c0LL&FTT-c0LL&LTT;
   array c1LfL[&FTT:&LTT] c1LfL&FTT-c1LfL&LTT;    
   array mlT[&FTT:&LTT] mlT&FTT-mlT&LTT;          *** -- Truck mode complete mileage -- ***;   

   do i=&FRT to &LRT;
     t0cf[i]=(ExtDray+LineHaul[i])/&RailMPH + IntDray[i]/&DrayTruckMPH + 1*&BulkTime;           *** -- time: minpath 5-8 -- ***;
     t0if[i]=(ExtDray+LineHaul[i])/&RailMPH + IntDray[i]/&DrayTruckMPH + 1*&WDCTime;            *** -- time: minpath 19-22 -- ***;
     t0iL[i]=(ExtDray+LineHaul[i])/&RailMPH + IntDray[i]/&DrayTruckMPH + 1*&WDCTime;            *** -- time: minpath 15-18 -- ***;
     c0cf[i]=(ExtDray+LineHaul[i])*&CarloadRate + IntDray[i]*&FTL53rate + 1*&BulkHandFee;       *** -- cost: minpath 5-8 -- ***;
     c0if[i]=(ExtDray+LineHaul[i])*&IMXRate + IntDray[i]*&FTL53rate + 1*&WDCHandFee;            *** -- cost: minpath 19-22 -- ***;
     c0iL[i]=(ExtDray+LineHaul[i])*&IMXRate + IntDray[i]*&LTL53rate + 1*&WDCHandFee;            *** -- cost: minpath 15-18 -- ***;
     t1fcf[i]=(LineHaul[i]/&RailMPH) + (ExtDray+IntDray[i])/&DrayTruckMPH + 2*&BulkTime;        *** -- time: minpath 9-12 -- ***;
     t1fif[i]=(LineHaul[i]/&RailMPH) + (ExtDray+IntDray[i])/&DrayTruckMPH + 2*&WDCTime;         *** -- time: minpath 23-26 -- ***;
     t1LiL[i]=(LineHaul[i]/&RailMPH) + (ExtDray+IntDray[i])/&DrayTruckMPH + 2*&WDCTime;         *** -- time: minpath 27-30 -- ***;
     c1fcf[i]=(LineHaul[i]*&CarloadRate) + (ExtDray+IntDray[i])*&FTL53rate + 2*&BulkHandFee;    *** -- cost: minpath 9-12 -- ***;
     c1fif[i]=(LineHaul[i]*&IMXRate) + (ExtDray+IntDray[i])*&FTL53rate + 2*&WDCHandFee;         *** -- cost: minpath 23-26 -- ***;
     c1LiL[i]=(LineHaul[i]*&IMXRate) + (ExtDray+IntDray[i])*&LTL53rate + 2*&WDCHandFee;         *** -- cost: minpath 27-30 -- ***;
	 mlR[i] = LineHaul[i] + ExtDray + IntDray[i];    
	 mlR3 = Rdist00;              *** -- mileage for minpath 3,4,13,14 -- ***;
   end;   
   
   do i=&FTT to &LTT;
     t0FL[i]=LineHaul[i]/&LHTruckMPH + (ExtDray+IntDray[i])/&DrayTruckMPH + 1*&WDCTime;         *** -- time: minpath 32-38 -- ***;
     t0LL[i]=LineHaul[i]/&LHTruckMPH + (ExtDray+IntDray[i])/&DrayTruckMPH + 1*&WDCTime;
     t1LFL[i]=LineHaul[i]/&LHTruckMPH + (ExtDray+IntDray[i])/&DrayTruckMPH + 1*&WDCTime;        *** -- time: minpath 39-45 -- ***;
     c0FL[i]=(ExtDray+LineHaul[i])*&FTL53rate + IntDray[i]*&LTL53rate + 1*&WDCHandFee;          *** -- cost: minpath 32-38 -- ***;
     c0LL[i]=(ExtDray+LineHaul[i])*&LTL53rate + IntDray[i]*&LTL53rate + 1*&WDCHandFee; 
     c1LFL[i]=LineHaul[i]*&FTL53rate + (ExtDray+IntDray[i])*&LTL53rate + 2*&WDCHandFee;         *** -- cost: minpath 39-45 -- ***;
	 mlT[i] = LineHaul[i] + ExtDray + IntDray[i];   
	 mlT31 = Tdist00;              *** -- mileage for minpath 31,46 -- ***;
   end;
   
   *### -- CREATE INDIRECT FTL AND LTL TRUCK COSTS/TIMES FOR U.S. SHIPMENTS NOT INVOLVING CMAP (OTHERWISE TRUCK IS NOT AN OPTION FOR THESE INDIRECT SHIPMENTS) -- ###*;   
   if (&FEZ<=o<=&LEZ or o in (310,399)) & (&FEZ<=dest<=&LEZ or dest in (310,399)) then do;
      if Tdist00=. then d=GCD; else d=TDist00;																*** -- set indirect truck LH distance -- ***;
      c0FL133=(ExtDray*2+d)*&FTL53rate + 1*&WDCHandFee;  													*** -- FTL indirect cost -- ***;
	  t0FL133=d/&LHTruckMPH + (ExtDray*2)/&DrayTruckMPH + 1*&WDCTime; 										*** -- FTL indirect time -- ***;
	  c1LFL133=d*&FTL53rate + (ExtDray*2)*&LTL53rate + 2*&WDCHandFee;										*** -- LTL indirect cost -- ***;
	  t1LFL133=d/&LHTruckMPH + (ExtDray*2)/&DrayTruckMPH + 1*&WDCTime;										*** -- LTL indirect time -- ***;
	  if (o in (179,180) & dest not in (179,180)) or (o not in (179,180) & dest in (179,180)) then do;
	     c0FL133=.; t0FL133=.; c1LFL133=.; t1LFL133=.;														*** -- Do Not Allow for Hawaii to non-Hawaii shipments -- ***;
	  end;
	  mlT31 = ExtDray*2+d;        *** -- mileage for minpath 31,46 -- ***;
	  
   *### -- CREATE DIRECT FTL AND LTL TRUCK COSTS/TIMES SHIPMENTS BETWEEN TWO HAWAIIAN ZONES (OTHERWISE TRUCK IS NOT AN OPTION FOR THESE DIRECT SHIPMENTS) -- ###*;   	  
   if (o=179 & dest=180) or (o=180 & dest=179) then do;	  
      tFTL = GCD/&LHTruckMPH ; 
      cFTL = &ExpressSurcharge*GCD*&FTL53rate ;
      tLTL = GCD/&LHTruckMPH ; 
      cLTL = &ExpressSurcharge*GCD*&LTL53rate ;
	  mlT31 = GCD;                *** -- mileage for minpath 31,46 -- ***;
   end;
  end;
		
		
   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;  
   ** ======== INTERNATIONAL SHIPPING ======== **;   
   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;     
   if o in (154,179,180) or dest in (154,179,180) or dest>&LEZ then do;       *** -- calculate international shipping between U.S. & Alaska/Hawaii/all foreign countries -- ***;
     if (o<=&LIZ & dest in (310,399)) or o=dest then do;                      *** -- ensure no international shipping between CMAP & Canada/Mexico, no intrazonal shipping -- ***;
	    cFTL40dir=.; tFTL40dir=.; cLTL40dir=.; tLTL40dir=.;
		cFTL53tload=.; tFTL53tload=.; cLTL53tload=.; tLTL53tload=.;
	 end;
     else do;
      ** ==== #51 [Intl Water, no transload, 40 ft container direct from port to dest]: use Bulk handling and fees ==== **;
      ** ==== Costlier rate because this path involves no transloading of containerized goods into 53' trailers ==== **;	  
       cFTL40dir= &ExpressSurcharge*(Tdist+InDray+ExDray)*&FTL40rate + GCD*&WaterRate + 2*&BulkHandFee;   ** -- cost -- **;
       tFTL40dir= GCD/&WaterMPH + Tdist/&LHTruckMPH + (InDray+ExDray)/&DrayTruckMPH + 2*&BulkTime;        ** -- time -- ** ;

      ** ==== #52 [Intl Water, no transload, 40 ft direct]: use Bulk handling and fees ==== **;
      ** ==== Costlier rate because this path involves no transloading of containerized goods into 53' trailers ==== **;		  
       cLTL40dir= &ExpressSurcharge*(Tdist+InDray+ExDray)*&LTL40rate + GCD*&WaterRate + 2*&BulkHandFee;   ** -- cost -- **;
       tLTL40dir= GCD/&WaterMPH + Tdist/&LHTruckMPH + (InDray+ExDray)/&DrayTruckMPH + 2*&BulkTime;        ** -- time -- ** ;

      ** ==== #53 [Intl Water, Transload, 53 ft FTL]: use Transload handling and fees ==== **;
      ** ==== No express surcharge because involves transloading of containerized goods into 53' trailers ==== **;	  
       cFTL53tload= (Tdist+InDray+ExDray)*&FTL53rate + GCD*&WaterRate + 2*&TloadHandFee;                  ** -- cost -- **;
       tFTL53tload= GCD/&WaterMPH + Tdist/&LHTruckMPH + (InDray+ExDray)/&DrayTruckMPH + 2*&TloadTime;     ** -- time -- ** ;	   
	   
      ** ==== #54 [Intl Water, Transload, 53 ft LTL]: use Transload handling and fees ==== **;	
      ** ==== No express surcharge because involves transloading of containerized goods into 53' trailers ==== **;		  
       cLTL53tload= (Tdist+InDray+ExDray)*&LTL53rate + GCD*&WaterRate + 2*&TloadHandFee;                  ** -- cost -- **;
       tLTL53tload= GCD/&WaterMPH + Tdist/&LHTruckMPH + (InDray+ExDray)/&DrayTruckMPH + 2*&TloadTime;     ** -- time -- ** ;	

       mlIW = GCD + tdist + (InDray+ExDray);  	   *** -- all four International Water modes have the same mileage calculation -- ***;
     end; 	   
   end;   
   
data i(drop=temp); set i;   
      ** ==== Ensure Reverse Direction Exists for every Possible Zone Pair ==== **;	 	
  if &FLN<=o<=&LLN or &FLN<=dest<=&LLN then delete;    *** -- logistics nodes are not origins or destinations within the model -- ***;
  output;
  temp=o; o=dest; dest=temp; reverse=1; output;
  proc sort; by o dest reverse;
 
data i; set i; by o dest reverse;
  if last.dest;
  
data check; set i(where=(cw145>0));
data check2; set check(obs=20);
  proc print; title "Inland Water review";  

  
   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;  
   ** ========  FINAL TEMPLATE CHECK  ======== **;   
   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;  
data us(rename=(Production_zone=o)); set airgcd(where=(Production_zone<=&LIZ or &FEZ<=Production_zone<=&LEZ));
   keep Production_zone; run;
   proc sort nodupkey; by o;       *** -- All U.S. mesozones -- ***;
   
data us_d(rename=(o=dest)); set us;   
   
data usforgn(rename=(Production_zone=dest)); set airgcd(where=(Production_zone<&FLN or Production_zone>&LLN));
   keep Production_zone; run;
   proc sort nodupkey; by dest;    *** -- All U.S. & Foreign mesozones -- ***;   
   
data forgn(rename=(dest=o)); set usforgn(where=(dest>&LEZ));
   keep dest; run;
   proc sort nodupkey; by o;    
   
proc sql noprint;
    create table fin1 as
           select us.*,
                  usforgn.* 
	       from us, usforgn;      *** -- All U.S. zones (254) * All U.S. & Foreign (472) = 119888 -- ***;
		   
proc sql noprint;
    create table fin2 as
           select forgn.*,
                  us_d.* 
	       from forgn, us_d;      *** -- All Foreign zones (218) * All U.S. (254) = 55372 -- ***;		   
		 
data fin; set fin1 fin2; proc sort nodupkey; by o dest;		 
data fin; merge fin(in=hit1) i(in=hit2); by o dest;  if hit1 & hit2 then delete;
   proc print; title "===================="; title2 "ERROR - Zone Pair Mismatch"; title3 "====================";
   
*###=================================================================================###
    -- ESTABLISH MODEPATH COSTS AND TIMES --
*###=================================================================================###;	
data i(rename=(o=Origin dest=Destination)); set i;   

data P.modepath_costs; set i;   
 label
  Origin = 'Production MesoZone' 
  Destination = 'Attraction MesoZone'
  c0LL133 = 'Cost, LTL with stop at 133'
  c0LL134 = 'Cost, LTL with stop at 134'
  c0LL135 = 'Cost, LTL with stop at 135'
  c0LL136 = 'Cost, LTL with stop at 136'
  c0LL137 = 'Cost, LTL with stop at 137'
  c0LL138 = 'Cost, LTL with stop at 138'
  c0LL139 = 'Cost, LTL with stop at 139'
  c0cf147 = 'Cost, Carload-FTL with stop at 147'
  c0cf148 = 'Cost, Carload-FTL with stop at 148'
  c0cf149 = 'Cost, Carload-FTL with stop at 149'
  c0cf150 = 'Cost, Carload-FTL with stop at 150'
  c0fL133 = 'Cost, FTL-LTL with stop at 133'
  c0fL134 = 'Cost, FTL-LTL with stop at 134'
  c0fL135 = 'Cost, FTL-LTL with stop at 135'
  c0fL136 = 'Cost, FTL-LTL with stop at 136'
  c0fL137 = 'Cost, FTL-LTL with stop at 137'
  c0fL138 = 'Cost, FTL-LTL with stop at 138'
  c0fL139 = 'Cost, FTL-LTL with stop at 139'
  c0iL147 = 'Cost, IMX-LTL with stop at 147'
  c0iL148 = 'Cost, IMX-LTL with stop at 148'
  c0iL149 = 'Cost, IMX-LTL with stop at 149'
  c0iL150 = 'Cost, IMX-LTL with stop at 150'
  c0if147 = 'Cost, IMX-FTL with stop at 147'
  c0if148 = 'Cost, IMX-FTL with stop at 148'
  c0if149 = 'Cost, IMX-FTL with stop at 149'
  c0if150 = 'Cost, IMX-FTL with stop at 150'
  c1LL0 = 'Cost, LTL(ExtDray)-LTL remainder'
  c1Lf0 = 'Cost, LTL(ExtDray)-FTL remainder'
  c1LfL133 = 'Cost, LTL-FTL-LTL with stop at 133'
  c1LfL134 = 'Cost, LTL-FTL-LTL with stop at 134'
  c1LfL135 = 'Cost, LTL-FTL-LTL with stop at 135'
  c1LfL136 = 'Cost, LTL-FTL-LTL with stop at 136'
  c1LfL137 = 'Cost, LTL-FTL-LTL with stop at 137'
  c1LfL138 = 'Cost, LTL-FTL-LTL with stop at 138'
  c1LfL139 = 'Cost, LTL-FTL-LTL with stop at 139'
  c1Li0 = 'Cost, LTL(ExtDray)-IMX remainder'
  c1LiL147 = 'Cost, LTL-IMX-LTL with stop at 147'
  c1LiL148 = 'Cost, LTL-IMX-LTL with stop at 148'
  c1LiL149 = 'Cost, LTL-IMX-LTL with stop at 149'
  c1LiL150 = 'Cost, LTL-IMX-LTL with stop at 150'
  c1fc0 = 'Cost, FTL(ExtDray)-Carload remainder'
  c1fcf147 = 'Cost, FTL-Carload-FTL with stop at 147'
  c1fcf148 = 'Cost, FTL-Carload-FTL with stop at 148'
  c1fcf149 = 'Cost, FTL-Carload-FTL with stop at 149'
  c1fcf150 = 'Cost, FTL-Carload-FTL with stop at 150'
  c1fi0 = 'Cost, FTL(ExtDray)-IMX remainder'
  c1fif147 = 'Cost, FTL-IMX-FTL with stop at 147'
  c1fif148 = 'Cost, FTL-IMX-FTL with stop at 148'
  c1fif149 = 'Cost, FTL-IMX-FTL with stop at 149'
  c1fif150 = 'Cost, FTL-IMX-FTL with stop at 150'
  cA141 = 'Cost, Air using Airport 141'
  cA142 = 'Cost, Air using Airport 142'
  cA143 = 'Cost, Air using Airport 143'
  cA144 = 'Cost, Air using Airport 144'
  cCarload = 'Cost, Carload Direct'
  cFTL = 'Cost, FTL Direct'
  cIMX = 'Cost, IMX Direct'
  cLTL = 'Cost, LTL Direct'
  cW145 = 'Cost, Water using Port 145'
  cW146 = 'Cost, Water using Port 146'
  cFTL40dir= 'Cost, FTL-40 Ft. Container (Direct from Port)'
  cLTL40dir = 'Cost, LTL-40 Ft. Container (Direct from Port)'
  cFTL53tload = 'Cost, FTL-53 with Transload at Port'
  cLTL53tload = 'Cost, LTL-53 with Transload at Port'
  t0LL133 = 'Time, LTL with stop at 133'
  t0LL134 = 'Time, LTL with stop at 134'
  t0LL135 = 'Time, LTL with stop at 135'
  t0LL136 = 'Time, LTL with stop at 136'
  t0LL137 = 'Time, LTL with stop at 137'
  t0LL138 = 'Time, LTL with stop at 138'
  t0LL139 = 'Time, LTL with stop at 139'
  t0cf147 = 'Time, Carload-FTL with stop at 147'
  t0cf148 = 'Time, Carload-FTL with stop at 148'
  t0cf149 = 'Time, Carload-FTL with stop at 149'
  t0cf150 = 'Time, Carload-FTL with stop at 150'
  t0fL133 = 'Time, FTL-LTL with stop at 133'
  t0fL134 = 'Time, FTL-LTL with stop at 134'
  t0fL135 = 'Time, FTL-LTL with stop at 135'
  t0fL136 = 'Time, FTL-LTL with stop at 136'
  t0fL137 = 'Time, FTL-LTL with stop at 137'
  t0fL138 = 'Time, FTL-LTL with stop at 138'
  t0fL139 = 'Time, FTL-LTL with stop at 139'
  t0iL147 = 'Time, IMX-LTL with stop at 147'
  t0iL148 = 'Time, IMX-LTL with stop at 148'
  t0iL149 = 'Time, IMX-LTL with stop at 149'
  t0iL150 = 'Time, IMX-LTL with stop at 150'
  t0if147 = 'Time, IMX-FTL with stop at 147'
  t0if148 = 'Time, IMX-FTL with stop at 148'
  t0if149 = 'Time, IMX-FTL with stop at 149'
  t0if150 = 'Time, IMX-FTL with stop at 150'
  t1LL0 = 'Time, LTL(ExtDray)-LTL remainder'
  t1Lf0 = 'Time, LTL(ExtDray)-FTL remainder'
  t1LfL133 = 'Time, LTL-FTL-LTL with stop at 133'
  t1LfL134 = 'Time, LTL-FTL-LTL with stop at 134'
  t1LfL135 = 'Time, LTL-FTL-LTL with stop at 135'
  t1LfL136 = 'Time, LTL-FTL-LTL with stop at 136'
  t1LfL137 = 'Time, LTL-FTL-LTL with stop at 137'
  t1LfL138 = 'Time, LTL-FTL-LTL with stop at 138'
  t1LfL139 = 'Time, LTL-FTL-LTL with stop at 139'
  t1Li0 = 'Time, LTL(ExtDray)-IMX remainder'
  t1LiL147 = 'Time, LTL-IMX-LTL with stop at 147'
  t1LiL148 = 'Time, LTL-IMX-LTL with stop at 148'
  t1LiL149 = 'Time, LTL-IMX-LTL with stop at 149'
  t1LiL150 = 'Time, LTL-IMX-LTL with stop at 150'
  t1fc0 = 'Time, FTL(ExtDray)-Carload remainder'
  t1fcf147 = 'Time, FTL-Carload-FTL with stop at 147'
  t1fcf148 = 'Time, FTL-Carload-FTL with stop at 148'
  t1fcf149 = 'Time, FTL-Carload-FTL with stop at 149'
  t1fcf150 = 'Time, FTL-Carload-FTL with stop at 150'
  t1fi0 = 'Time, FTL(ExtDray)-IMX remainder'
  t1fif147 = 'Time, FTL-IMX-FTL with stop at 147'
  t1fif148 = 'Time, FTL-IMX-FTL with stop at 148'
  t1fif149 = 'Time, FTL-IMX-FTL with stop at 149'
  t1fif150 = 'Time, FTL-IMX-FTL with stop at 150'
  tA141 = 'Time, Air using Airport 141'
  tA142 = 'Time, Air using Airport 142'
  tA143 = 'Time, Air using Airport 143'
  tA144 = 'Time, Air using Airport 144'
  tCarload = 'Time, Carload Direct'
  tFTL = 'Time, FTL Direct'
  tIMX = 'Time, IMX Direct'
  tLTL = 'Time, LTL Direct'
  tW145 = 'Time, Water using Port 145'
  tW146 = 'Time, Water using Port 146'
  tFTL40dir	= 'Time, FTL-40 Ft. Container (Direct from Port)'
  tLTL40dir = 'Time, LTL-40 Ft. Container (Direct from Port)'
  tFTL53tload = 'Time, FTL-53 with Transload at Port'
  tLTL53tload = 'Time, LTL-53 with Transload at Port'; 

 array	cost[54];
 array	time[54];
 array	mile[54];
  cost[1] = cW145 ;
  cost[2] = cW146 ;
  cost[3] = cCarload ;
  cost[4] = c1fc0 ;
  cost[5] = c0cf147 ;
  cost[6] = c0cf148 ;
  cost[7] = c0cf149 ;
  cost[8] = c0cf150 ;
  cost[9] = c1fcf147 ;
  cost[10] = c1fcf148 ;
  cost[11] = c1fcf149 ;
  cost[12] = c1fcf150 ;
  cost[13] = cIMX ;
  cost[14] = c1fi0 ;
  cost[15] = c0iL147 ;
  cost[16] = c0iL148 ;
  cost[17] = c0iL149 ;
  cost[18] = c0iL150 ;
  cost[19] = c0if147 ;
  cost[20] = c0if148 ;
  cost[21] = c0if149 ;
  cost[22] = c0if150 ;
  cost[23] = c1fif147 ;
  cost[24] = c1fif148 ;
  cost[25] = c1fif149 ;
  cost[26] = c1fif150 ;
  cost[27] = c1LiL147 ;
  cost[28] = c1LiL148 ;
  cost[29] = c1LiL149 ;
  cost[30] = c1LiL150 ;
  cost[31] = cFTL ;
  cost[32] = c0fL133 ;
  cost[33] = c0fL134 ;
  cost[34] = c0fL135 ;
  cost[35] = c0fL136 ;
  cost[36] = c0fL137 ;
  cost[37] = c0fL138 ;
  cost[38] = c0fL139 ;
  cost[39] = c1LfL133 ;
  cost[40] = c1LfL134 ;
  cost[41] = c1LfL135 ;
  cost[42] = c1LfL136 ;
  cost[43] = c1LfL137 ;
  cost[44] = c1LfL138 ;
  cost[45] = c1LfL139 ;
  cost[46] = cLTL ;
  cost[47] = cA141 ;
  cost[48] = cA142 ;
  cost[49] = cA143 ;
  cost[50] = cA144 ;
  cost[51] = cFTL40dir ;
  cost[52] = cLTL40dir ;
  cost[53] = cFTL53tload ;
  cost[54] = cLTL53tload ;

  time[1] = tW145 ;
  time[2] = tW146 ;
  time[3] = tCarload ;
  time[4] = t1fc0 ;
  time[5] = t0cf147 ;
  time[6] = t0cf148 ;
  time[7] = t0cf149 ;
  time[8] = t0cf150 ;
  time[9] = t1fcf147 ;
  time[10] = t1fcf148 ;
  time[11] = t1fcf149 ;
  time[12] = t1fcf150 ;
  time[13] = tIMX ;
  time[14] = t1fi0 ;
  time[15] = t0iL147 ;
  time[16] = t0iL148 ;
  time[17] = t0iL149 ;
  time[18] = t0iL150 ;
  time[19] = t0if147 ;
  time[20] = t0if148 ;
  time[21] = t0if149 ;
  time[22] = t0if150 ;
  time[23] = t1fif147 ;
  time[24] = t1fif148 ;
  time[25] = t1fif149 ;
  time[26] = t1fif150 ;
  time[27] = t1LiL147 ;
  time[28] = t1LiL148 ;
  time[29] = t1LiL149 ;
  time[30] = t1LiL150 ;
  time[31] = tFTL ;
  time[32] = t0fL133 ;
  time[33] = t0fL134 ;
  time[34] = t0fL135 ;
  time[35] = t0fL136 ;
  time[36] = t0fL137 ;
  time[37] = t0fL138 ;
  time[38] = t0fL139 ;
  time[39] = t1LfL133 ;
  time[40] = t1LfL134 ;
  time[41] = t1LfL135 ;
  time[42] = t1LfL136 ;
  time[43] = t1LfL137 ;
  time[44] = t1LfL138 ;
  time[45] = t1LfL139 ;
  time[46] = tLTL ;
  time[47] = tA141 ;
  time[48] = tA142 ;
  time[49] = tA143 ;
  time[50] = tA144 ;
  time[51] = tFTL40dir ;
  time[52] = tLTL40dir ;
  time[53] = tFTL53tload ;
  time[54] = tLTL53tload ;
   
  mile[1] = mlW145;
  mile[2] = mlW146;
  mile[3] = mlR3;
  mile[4] = mlR3;
  mile[5] = mlR147;
  mile[6] = mlR148;
  mile[7] = mlR149;
  mile[8] = mlR150;
  mile[9] = mlR147;
  mile[10] = mlR148;
  mile[11] = mlR149;
  mile[12] = mlR150;
  mile[13] = mlR3;
  mile[14] = mlR3;
  mile[15] = mlR147;
  mile[16] = mlR148;
  mile[17] = mlR149;
  mile[18] = mlR150;
  mile[19] = mlR147;
  mile[20] = mlR148;
  mile[21] = mlR149;
  mile[22] = mlR150;
  mile[23] = mlR147;
  mile[24] = mlR148;
  mile[25] = mlR149;
  mile[26] = mlR150;
  mile[27] = mlR147;
  mile[28] = mlR148;
  mile[29] = mlR149;
  mile[30] = mlR150;
  mile[31] = mlT31;
  mile[32] = mlT133;
  mile[33] = mlT134;
  mile[34] = mlT135;
  mile[35] = mlT136;
  mile[36] = mlT137;
  mile[37] = mlT138;
  mile[38] = mlT139;
  mile[39] = mlT133;
  mile[40] = mlT134;
  mile[41] = mlT135;
  mile[42] = mlT136;
  mile[43] = mlT137;
  mile[44] = mlT138;
  mile[45] = mlT139;
  mile[46] = mlT31;
  mile[47] = mlA141;
  mile[48] = mlA142;
  mile[49] = mlA143;
  mile[50] = mlA144;
  mile[51] = mlIW;
  mile[52] = mlIW;
  mile[53] = mlIW;
  mile[54] = mlIW;
  
   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;  
   ** ========  Check average costs and times by mode:  ======== **;   
   *   -- W	Water	1 to 2 --                                         ;
   *   -- C	Carload	3 to 12 --                                        ;   
   *   -- I	IMX		13 to 30 --                                       ;
   *   -- F	FTL		31 to 45 --                                       ;	
   *   -- L	LTL		46 --                                             ; 
   *   -- A	Air		47 to 50 --                                       ;  
   *   -- Y	No transload, direct from Port in 40' Container: 	51-52 -- ; 
   *   -- Z	Transload at Port to 53' Truck: 					53-54 -- ;    
   * -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --;   
  AvgCostW 	=	mean(cost[1],cost[2]);
  AvgTimeW 	=	mean(time[1],time[2]);
  AvgCostC 	=	mean(cost[3],cost[4],cost[5],cost[6],cost[7],cost[8],cost[9],cost[10],cost[11],cost[12]);
  AvgTimeC 	=	mean(time[3],time[4],time[5],time[6],time[7],time[8],time[9],time[10],time[11],time[12]);
  AvgCostI	=	mean(cost[12],cost[13],cost[14],cost[15],cost[16],cost[17],cost[18],cost[19],cost[20],cost[21],cost[22],cost[23],cost[24],cost[25],cost[26],cost[27],cost[28],cost[29],cost[30]);
  AvgTimeI	=	mean(time[12],time[13],time[14],time[15],time[16],time[17],time[18],time[19],time[20],time[21],time[22],time[23],time[24],time[25],time[26],time[27],time[28],time[29],time[30]);
  AvgCostF	=	mean(cost[31],cost[32],cost[33],cost[34],cost[35],cost[36],cost[37],cost[38],cost[39],cost[40],cost[41],cost[42],cost[43],cost[44],cost[45]);
  AvgTimeF	=	mean(time[31],time[32],time[33],time[34],time[35],time[36],time[37],time[38],time[39],time[40],time[41],time[42],time[43],time[44],time[45]);
  AvgCostL	=	mean(cost[46]);
  AvgTimeL	=	mean(time[46]);
  AvgCostA	=	mean(cost[47],cost[48],cost[49],cost[50]);
  AvgTimeA	=	mean(time[47],time[48],time[49],time[50]);
  AvgCost40d	=	mean(cost[51],cost[52]);
  AvgTime40d 	=	mean(time[51],time[52]);
  AvgCost53t 	=	mean(cost[53],cost[54]);
  AvgTime53t 	=	mean(time[53],time[54]);  
 
    drop mlW145-mlW146 mlR147-mlR150 mlR3 mlT133-mlT139 mlT31 mlA141-mlA144 mlIW;
 
data i(keep=Origin Destination time1-time54 cost1-cost54); set P.modepath_costs;  *** -- only export fields needed by Meso model -- ***;
proc export outfile=mdpath dbms=csv replace;
 

*###=================================================================================###
    -- CREATE A FILE OF MODAL DISTANCES CONSISTENT WITH TIME/COST CALCULATIONS --
*###=================================================================================###; 
data a(keep=Origin Destination MinPath Miles); set P.modepath_costs;
 array	mile[54];
  do i=1 to 54;
    MinPath=i; Miles=round(mile[i],0.1);
	output;
  end;	
   proc sort nodupkey; by Origin Destination MinPath;
   
data a; set a(where=(Miles>0));   
proc export data=a outfile=mdmile dbms=csv replace;
 
 **** ---- Prepare to output LOS parameters (for report) ---- ****;
data P.Parameters_LOS; input Parameter:$16. Value;
cards;
BulkHandFee 0
WDCHandFee 0
IMXHandFee 0
TloadHandFee 0
AirHandFee 0
WaterRate 0
WaterRate2 0
CarloadRate 0
IMXRate 0
AirRate 0
LTL53rate 0
FTL53rate 0
LTL40rate 0
FTL40rate 0
WaterMPH 0
RailMPH 0
LHTruckMPH 0
DrayTruckMPH 0
AirMPH 0
ExpressSurcharge 0
BulkTime 0
WDCTime 0
IMXTime 0
TloadTime 0
AirTime 0
;run;

data P.Parameters_LOS; set P.Parameters_LOS;
  if Parameter='BulkHandFee' 		then Value=&BulkHandFee;
  if Parameter='WDCHandFee' 		then Value=&WDCHandFee;
  if Parameter='IMXHandFee' 		then Value=&IMXHandFee;
  if Parameter='TloadHandFee' 	then Value=&TloadHandFee;
  if Parameter='AirHandFee' 		then Value=&AirHandFee;
  if Parameter='WaterRate'	 	then Value=&WaterRate;
  if Parameter='WaterRate2'	 	then Value=&WaterRate2;
  if Parameter='CarloadRate' 		then Value=&CarloadRate;
  if Parameter='IMXRate' 			then Value=&IMXRate;
  if Parameter='AirRate' 			then Value=&AirRate;
  if Parameter='LTL53rate' 		then Value=&LTL53rate;
  if Parameter='FTL53rate' 		then Value=&FTL53rate;
  if Parameter='LTL40rate' 		then Value=&LTL40rate;
  if Parameter='FTL40rate' 		then Value=&FTL40rate;
  if Parameter='WaterMPH' 		then Value=&WaterMPH;
  if Parameter='RailMPH' 			then Value=&RailMPH;
  if Parameter='LHTruckMPH' 		then Value=&LHTruckMPH;
  if Parameter='DrayTruckMPH' 	then Value=&DrayTruckMPH;
  if Parameter='AirMPH' 			then Value=&AirMPH;
  if Parameter='ExpressSurcharge' then Value=&ExpressSurcharge;
  if Parameter='BulkTime' 		then Value=&BulkTime;
  if Parameter='WDCTime' 			then Value=&WDCTime;
  if Parameter='IMXTime' 			then Value=&IMXTime;
  if Parameter='TloadTime' 		then Value=&TloadTime;
  if Parameter='AirTime' 			then Value=&AirTime;

data P.Parameters_LOS; set P.Parameters_LOS;
 attrib desc length = $60 ;
  if Parameter='BulkHandFee' 		then desc='Handling charge for bulk goods (per ton)';
  if Parameter='WDCHandFee' 		then desc='Warehouse/DC handling charge (per ton)';
  if Parameter='IMXHandFee' 		then desc='Intermodal lift charge (per ton; assumes $500/lift)';
  if Parameter='TloadHandFee' 	then desc='Transload charge (per ton; at intl. ports only)';
  if Parameter='AirHandFee' 		then desc='Air cargo handling charge (per ton)';
  if Parameter='WaterRate'	 	then desc='Line-haul charge, international water (per ton-mile)';
  if Parameter='WaterRate2'	 	then desc='Line-haul charge, inland water (per ton-mile)';
  if Parameter='CarloadRate' 		then desc='Line-haul charge, carload (per ton-mile)';
  if Parameter='IMXRate' 			then desc='Ling-haul charge, intermodal (per ton-mile)';
  if Parameter='AirRate' 			then desc='Line-haul charge, air (per ton-mile)';
  if Parameter='LTL53rate' 		then desc='Line-haul charge, 53 ft. LTL (per ton-mile)';
  if Parameter='FTL53rate' 		then desc='Line-haul charge, 53 ft. FTL (per ton-mile)';
  if Parameter='LTL40rate' 		then desc='Line-haul charge, 40 ft. LTL (per ton-mile)';
  if Parameter='FTL40rate' 		then desc='Line-haul charge, 40 ft. FTL (per ton-mile)';
  if Parameter='WaterMPH' 		then desc='Water speed (mph)';
  if Parameter='RailMPH' 			then desc='Rail speed (mph)';
  if Parameter='LHTruckMPH' 		then desc='Line-haul truck speed (mph)';
  if Parameter='DrayTruckMPH' 	then desc='Drayage truck speed (mph)';
  if Parameter='AirMPH' 			then desc='Air speed (mph)';
  if Parameter='ExpressSurcharge' then desc='Surcharge for direct/express transport (factor)';
  if Parameter='BulkTime' 		then desc='Handling time at bulk handling facilities (hours)';
  if Parameter='WDCTime' 			then desc='Handling time at warehouse/DCs (hours)';
  if Parameter='IMXTime' 			then desc='Handling time at intermodal yards (hours)';
  if Parameter='TloadTime' 		then desc='Handling time at transload facilities (hours)';
  if Parameter='AirTime' 			then desc='Handling time at air terminals (hours)';
run;

*################################################################################################;  

*###=================================================================================###
    -- CREATE MESOZONE-TO-MESOZONE TRUCK SKIMS FOR STOP SEQUENCING IN TRUCK TOURS --
*###=================================================================================###;
**** ---- This is equivalent to distance, as the skims assume a speed of 60 MPH ---- ****;
data a(keep=Origin Destination dist); infile "&emdir.mf31.in" missover dlm=' :';
  input @1 flag $1. @;
    select;
      when (flag in ('a','c','d','t')) delete;
      otherwise input Origin d1 t1 d2 t2 d3 t3;
    end;
    Destination=d1; dist=t1; output;
    Destination=d2; dist=t2; output;
    Destination=d3; dist=t3; output;
data a; set a(where=(&FIZ<=Origin<=&LIZ & &FIZ<=Destination<=&LIZ & dist>0));

**** ---- Add intrazonal distance ---- ****;
data intrazn; set airgcd(where=(Production_zone=Consumption_zone & Production_zone<=&LIZ));
   rename Production_zone=Origin Consumption_zone=Destination GCD=dist;
   keep Production_zone Consumption_zone GCD; run;

data P.mesozone_skims; set a intrazn;
  Time=round(dist/&DrayTruckMPH,0.001);    *** -- time in hours, use drayage speed as more accurate estimate of regional travel speed -- ***;
  proc sort nodupkey; by Origin Destination;

data a(drop=dist); set P.mesozone_skims;
proc export outfile=mzskims dbms=csv replace;

*###=================================================================================###
    -- CREATE PORTS FILE FOR MESO MODEL --
*###=================================================================================###;
data port(keep=Origin Destination Port_mesozone Port_name); set P.modepath_costs(where=(Port_mesozone is not null));
  proc sort nodupkey; by Origin Destination;

data port(rename=(Origin=Production_zone Destination=Consumption_zone)); set port;
proc export outfile=ports dbms=csv replace;


run;

