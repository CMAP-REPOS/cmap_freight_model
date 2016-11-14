/* STEP1_CREATE_GCD_FILE.SAS
      Craig Heither, rev. 09-24-2015

	This program creates the file "data_mesozone_gcd.csv" containing Great Circle Distances between all pairs of Mesozones.  The output file replaces the RSG version in the Meso model to address these issues: 
	  - The file RSG delivered measures GCD in Kilometers rather than Miles.
	  - The new file includes logistics nodes so that it can be used to develop the modepath costs.
	  - The new file will reflect the locations of US mesozones in the MFN (except Hawaii).
	  - The new file includes GCD values for intrazonal pairs (US mesozones only).
	
	This program also creates the file "data_mesozone_centroids.csv" to ensure the data used in the Meso model is consistent with the MFN data.

*/

options noxwait;
*###=================================================================================###
    DEFINE INPUT/OUTPUT FILES
*###=================================================================================###;
%let sasin=inputs\;	                                          *** -- Location of SAS input files -- ***;
%let outdir=outputs\&sysparm.;                                *** -- Location of output files for Meso Freight Model -- ***;
%let max=273;                                                 *** -- Maximum U.S. mesozone number -- ***;

  *##-- Inputs --##;
filename rsgdist "&sasin.data_mesozone_gcd.csv";              *** -- Original RSG file of Great Circle Distances -- ***;
filename cmap "&sasin.mesozone_latlon.csv";                   *** -- File of mesozone locations from Master Freight Network -- ***;
filename sqmi "&sasin.Mesozone_sqmi.csv";                     *** -- File of U.S. mesozone area (sq miles), does not include logistics nodes -- ***; 
filename cent "..\input_data\base_ntwk.txt";                  *** -- Emme network batchin file -- ***;

  *##-- Output --##;
filename out1 "&outdir.\data_mesozone_gcd.csv";               *** -- New output file of Great Circle Distances -- ***;
filename out2 "&outdir.\data_mesozone_centroids.csv";         *** -- New output file of CMAP Mesozone centroid coordinates -- ***;

data _null_; command="if not exist &outdir (mkdir &outdir)"; call system(command); run;

*###=================================================================================###
    READ ORIGINAL FILE
*###=================================================================================###;
proc import datafile=rsgdist out=meso dbms=csv replace;
data meso(keep=Production_zone Production_lon Production_lat); set meso;
  proc sort nodupkey; by Production_zone;


*###=================================================================================###
    READ CMAP FILE
*###=================================================================================###;
proc import datafile=cmap out=cmapmeso dbms=csv replace;
data cmapmeso; set cmapmeso;
  ***-- Convert coordinates from decimal degrees to radians for consistency with RSG file -- ***;
  ***-- Conversion: decimal degrees * pi / 180 --***;
  Production_lon=Production_lon*constant('pi')/180;
  Production_lat=Production_lat*constant('pi')/180;
  proc sort nodupkey; by Production_zone;

*###=================================================================================###
    MERGE FILES, OVERWRITE RSG DATA WITH CMAP
*###=================================================================================###;  
data meso; merge meso cmapmeso; by Production_zone;


*###=================================================================================###
    CREATE ALL POTENTIAL MESOZONE COMBINATIONS
*###=================================================================================###;  
data orig; set meso;
data dest(rename=(Production_zone=Consumption_zone Production_lon=Consumption_lon Production_lat=Consumption_lat)); set meso;

proc sql noprint;
    create table allmeso as
           select orig.*,
                  dest.* 
	       from orig, dest;
		   
data allmeso(drop=delta_lon delta_lat a c); set allmeso;
  ***-- Calculate Great Circle Distance using Haversine formula -- ***;
  ***-- see http://www.movable-type.co.uk/scripts/latlong.html for discussion/documentation --***;
  ***-- or http://andrew.hedges.name/experiments/haversine/ --***;
  delta_lon=Consumption_lon - Production_lon;
  delta_lat=Consumption_lat - Production_lat;
  a=sin(delta_lat/2)**2 + cos(Production_lat)*cos(Consumption_lat)*sin(delta_lon/2)**2;
  c=2*arsin(min(1,sqrt(a)));
  GCD=c*3961;  **-- 3961 is radius of Earth in miles, about 39 degrees from equator (Washington DC);  

data allmeso(drop=a b c); set allmeso;
  ***-- Ensure Both Directions are included -- ***;
  output;
  if Production_zone ne Consumption_zone then do;
    a=Production_zone; b=Production_lon; c=Production_lat;
	Production_zone=Consumption_zone; Production_lon=Consumption_lon; Production_lat=Consumption_lat;
	Consumption_zone=a; Consumption_lon=b; Consumption_lat=c;
	output;
  end;
  proc sort nodupkey; by Consumption_zone Production_zone;
	

*###=================================================================================###
    PROVIDE A DISTANCE FOR INTRAZONAL PAIRS (U.S. MESOZONES ONLY)
*###=================================================================================###;  	
proc import datafile=sqmi out=sqmi dbms=csv replace;  

  ***-- For simplicity, assume each mesozone is a square and the average trip distance -- ***;
  ***-- equals one-half of the length of each side: thus, sqrt(area)/2 -- ***;   
data sqmi(drop=mesozone sqmi); set sqmi;
  dist=sqrt(sqmi)/2;  
  Production_zone=mesozone;
  Consumption_zone=mesozone;
   proc sort; by Consumption_zone Production_zone;

   
data allmeso(drop=dist); merge allmeso sqmi; by Consumption_zone Production_zone;
   if Consumption_zone=Production_zone then GCD=max(GCD,dist);
   if Production_zone=182 or Consumption_zone=182 then delete;   *** original Entire CMAP mesozone does not exist in meso model;
proc export outfile=out1 dbms=csv replace;


*###=================================================================================###
    CREATE NEW CMAP MESOZONE CENTROID COORDINATE FILE
*###=================================================================================###;  	
data centrd; infile cent missover obs=400;
   input @1 flag $2. @; 
    select(flag);
     when('a ','a*') input stop_zone x_coord y_coord;   
     otherwise delete;
    end;
data centrd(drop=flag); set centrd(where=(stop_zone<=&max));	
   x_coord=round(x_coord/5280,0.001);  ** -- convert from State Plane feet to Miles -- **;
   y_coord=round(y_coord/5280,0.001);
  proc sort nodupkey; by stop_zone;
proc export outfile=out2 dbms=csv replace;
	
run;
