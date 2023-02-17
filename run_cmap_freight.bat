:: CMAP Freight Model Batch File

:: Variable definitions
:: Scenario and Year
set scenarioname="base"
set scenarioyear=2015

:: Reference scenario and year to use for scenario comparison
:: Note: this scenario must have already been run
set reference="base"
set referenceyear=2015

:: Steps of the model to run (TRUE or FALSE, use upper case)
set runfirmsyn="TRUE"
set runscm="FALSE"
set runfttm="FALSE"
set runttexp="FALSE"
set rundashboard="FALSE"

:: For reference scenario, if this is the base scenario change to validation
if [%scenarioname%]==["base"] set reference="Validation"
if [%scenarioname%]==["base"] set referenceyear=2015

:: Run Freight Model For Selected Scenario and Components
Rscript run_cmap_freight.R %scenarioname% %scenarioyear% %runfirmsyn% %runscm% %runfttm% %runttexp% %rundashboard% %reference% %referenceyear% >run_cmap_freight_log.txt 2>&1

:: Check for errors, exit and return error code if error
if %errorlevel% neq 0 exit /B %errorlevel%

:: Add pause to keep command window open at end of run
pause

