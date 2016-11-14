/* STEP5_CREATE_ZONAL_TRUCK_TOUR_FILES.SAS
      Craig Heither, rev. 05-06-2016

	This program creates the CMAP zonal files used in the truck touring model. 

	Input files needed (located in ..\SAS\inputs\<conformity>\):
	  - hwydist_pk - AM Peak skimmed miles
	  - hwytime_pk - AM Peak skimmed minutes	  
	  - hwytime_op - Midday skimmed minutes
	    * [above three files can be generated using ..\Database\macros\zn09_skim_data.mac]
	  - zcentroid_sqmi.txt - zonal area
	  - zcentroid_xcoord.txt - zonal X coordinate	  
	  - zcentroid_ycoord.txt - zonal Y coordinate	  	  
	    * [above three files can be obtained from  <conformity>\Database\data\ folder]
	  - subzn_emp.csv - Conformity subzone total employment	
	    * [above file exported from tg100 SAS dataset located in <conformity>\Database\tg\sas\data\ folder]
	  - sz_zn09_meso.csv - subzone-zone-mesozone correspondence file			
	  
	  For each zone pair (including intrazonal) need:
	   - peak skimmed time & distance
	   - midday skimmed time
	  Zonal needs:
	  - subzone employment values
	  - zone centroid coordinates (in miles) for cluster analyis	  
*/

*###=================================================================================###
    DEFINE INPUT/OUTPUT FILES
*###=================================================================================###;
%let conformity=c16q3;                                        *** -- Conformity analysis used for network data -- ***;
%let sasin=inputs\;	                                          *** -- Location of SAS input files -- ***;
%let outdir=outputs\&sysparm.;                                *** -- Location of output files for Meso Freight Model -- ***;


  *##-- Inputs --##;
filename hwydist "&sasin.&conformity.\hwydist_pk";            *** -- AM Peak skimmed miles -- ***;
filename hwytmpk "&sasin.&conformity.\hwytime_pk";            *** -- AM Peak skimmed minutes -- ***;
filename hwytmop "&sasin.&conformity.\hwytime_op";            *** -- Midday skimmed minutes -- ***;
filename znsqmi "&sasin.&conformity.\zcentroid_sqmi.txt";     *** -- zonal area -- ***;
filename znx "&sasin.&conformity.\zcentroid_xcoord.txt";      *** -- zone x-coordinate -- ***;
filename zny "&sasin.&conformity.\zcentroid_ycoord.txt";      *** -- zone y-coordinate -- ***;
filename emp "&sasin.&conformity.\subzn_emp.csv";             *** -- subzone total employment -- ***;
filename corresp "&sasin.&conformity.\sz_zn09_meso.csv";      *** -- subzone-zone-mesozone correspondence file -- ***;

  *##-- Output --##;
filename out1 "&outdir.\cmap_data_zone_skims.csv";            *** -- New output file of Zonal skims -- ***;
filename out2 "&outdir.\cmap_data_zone_centroids.csv";        *** -- New output file of CMAP zone centroid coordinates -- ***;
filename out3 "&outdir.\cmap_data_zone_employment.csv";       *** -- New output file of CMAP zonal employment -- ***;

*###=================================================================================###
    READ NETWORK SKIM FILES
*###=================================================================================###;
  ** -- AM Miles -- **;
data miles(keep=o dest Miles); infile hwydist missover dlm=' :';
  input @1 flag $1. @; 
    select(flag);
     when('c','t','a','d') delete;   
     otherwise input o d1 v1 d2 v2 d3 v3 d4 v4;
	end; 
   dest=d1; Miles=v1; output;
   dest=d2; Miles=v2; output;
   dest=d3; Miles=v3; output;
   dest=d4; Miles=v4; output;
    proc sort; by o dest;
data miles; set miles(where=(o>0 & dest>0));	
		  
  ** -- AM Time -- **;
data timepk(keep=o dest Peak); infile hwytmpk missover dlm=' :' firstobs=6;
  input o d1 v1 d2 v2 d3 v3 d4 v4;
   dest=d1; Peak=v1; output;
   dest=d2; Peak=v2; output;
   dest=d3; Peak=v3; output;
   dest=d4; Peak=v4; output;
    proc sort; by o dest;
data timepk; set timepk(where=(o>0 & dest>0));
		  		  
  ** -- Midday Time -- **;
data timeop(keep=o dest OffPeak); infile hwytmop missover dlm=' :' firstobs=6;
  input o d1 v1 d2 v2 d3 v3 d4 v4;
   dest=d1; OffPeak=v1; output;
   dest=d2; OffPeak=v2; output;
   dest=d3; OffPeak=v3; output;
   dest=d4; OffPeak=v4; output;
    proc sort; by o dest;
data timeop; set timeop(where=(o>0 & dest>0));		  		  

  ** -- Merged -- **;
data zone; merge timepk timeop miles; by o dest;

*###=================================================================================###
    PROVIDE A DISTANCE & TIME FOR INTRAZONAL PAIRS
*###=================================================================================###;  	
  ** -- Create Intrazonal -- **;
data intra; set zone;
  dest=o;
  proc sort nodupkey; by o dest;  
  
data dist(keep=o sqmi); infile znsqmi missover dlm=' :';  
  input @1 flag $1. @; 
    select(flag);
     when('c','t','a','d') delete;   
     otherwise input @1 o junk $ sqmi;  
	end;

data intra(drop=sqmi); merge intra dist(in=hit); by o; if hit;	
  ***-- For simplicity, assume each zone is a square and the average trip distance -- ***;
  ***-- equals one-half of the length of each side: thus, sqrt(area)/2 -- ***;   
  Miles=round(sqrt(sqmi)/2,0.01);  
  Peak=Miles/3;   *** -- assume 20 MPH -- ***;
  OffPeak=Peak;
	
data zone(rename=(o=Origin dest=Destination)); set zone intra;
 ***-- Convert travel time to hours -- ***;
  Peak=max(round(Peak/60,0.01),0.01);
  OffPeak=max(round(OffPeak/60,0.01),0.01); 
   proc sort; by Origin Destination;	
	
proc export outfile=out1 dbms=csv replace;	

proc sort data=zone; by Origin Peak;
data check(keep=Zone Peak); set zone; by Origin Peak;
  if last.Origin;
  Zone=max(Origin,Destination);  ** assuming POEs are problems;
  Peak=Peak*60;
   proc sort; by Zone descending Peak;
data check; set check; by Zone descending Peak;
  if first.zone;
   proc sort; by descending Peak;
data check; set check(obs=10);
 proc print; title "Zone Pairs with High Travel Times";

*###=================================================================================###
    PROVIDE ZONE CENTROID COORDINATES FOR CLUSTERING
*###=================================================================================###;  	
data x(keep=stop_zone x_coord); infile znx missover dlm=' :';  
  input @1 flag $1. @; 
    select(flag);
     when('c','t','a','d') delete;   
     otherwise input @1 stop_zone junk $ x_coord;  
	end;
  proc sort; by stop_zone;
  
data y(keep=stop_zone y_coord); infile zny missover dlm=' :';  
  input @1 flag $1. @; 
    select(flag);
     when('c','t','a','d') delete;   
     otherwise input @1 junk $ stop_zone y_coord;  
	end;
  proc sort; by stop_zone;  

data coord; merge x y; by stop_zone;
   x_coord=round(x_coord/5280,0.001);  ** -- convert from State Plane feet to Miles -- **;
   y_coord=round(y_coord/5280,0.001);
proc export outfile=out2 dbms=csv replace;

*###=================================================================================###
    PROVIDE ZONAL EMPLOYMENT FOR FIRM LOCATIONS
*###=================================================================================###;  	
proc import datafile=emp out=szemp dbms=csv replace; proc sort; by subzone09;
proc import datafile=corresp out=corresp dbms=csv replace; proc sort; by subzone09;

data szemp(rename=(zone09=Zone mesozone=Mesozone)); merge szemp corresp; by subzone09;
  proc summary nway; class Zone; var i18; id Mesozone; output out=z sum=totalemp;

data z(drop=_type_ _freq_); set z;  
proc export outfile=out3 dbms=csv replace;

run;
