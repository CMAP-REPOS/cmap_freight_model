#' Start the freight model
#'
#' Creates a model object to contain model and scenario structure and variable information.
#' Creates an outputs folder if required (i.e. the scenario has not been run).
#' Reads in the scenario variables. Loads packages required by the model.
#' Creates the lists objects for each of the model components ready for use during the model run.
#' @param basedir Path to the root directory of the model (character string); defaults to the current working directory returned by getwd()
#' @param scenarioname Name of the scenario, which should be identical to the directory name in the file system (character string); defaults to "base"
#' @param scenvarfile Name of the file containing scenario variables, defaults to "scenario_variables.R"
#' @param inputdir Name of the directory containing inputs within the scenario folder, defaults to "inputs"
#' @param outputdir Name of the directory containing outputs within the scenario folder, defaults to "outputs"
#' @param scriptsdir Name of the directory in the model containing model scripts, defaults to "scripts"
#' @param packages Character vector of package names, for packages used in the course of running the model
#' @param steps Character vector of model component names in the order that they need to be run
#' @param steptitles Character vector of model component titles for use in progress bars, print statements, in the order that they need to be run
#' @param stepscripts Character vector of model component script filenames so that the components can be called, in the order that they need to be run   
#' @keywords Management
#' @export

startModel <- function(basedir=getwd(), scenarioname="base", scenvarfile="scenario_variables.R", 
                       inputdir="inputs",outputdir="outputs", scriptsdir="scripts",
                       packages=NULL, steps=NULL, steptitles=NULL, stepscripts=NULL ){
  
  #Create model list
  model <- list(basedir=basedir,
                scendir=file.path(basedir,"scenarios",scenario),
                scenvars=list(),
                inputdir=file.path(basedir,"scenarios",scenario,inputdir),
                outputdir=file.path(basedir,"scenarios",scenario,outputdir),
                scriptsdir=file.path(basedir,scriptsdir),
                packages=packages,
                steps=steps,
                steptitles=steptitles,
                stepscripts=file.path(basedir,scriptsdir,stepscripts))
  
  #Change to Base directory
  setwd(model$basedir)
  
  #Scenario directory: create outputs folder, read variables 
  model$scenvars <- within(model$scenvars, source(file.path(model$scendir,scenvarfile),local=TRUE))
  if(!file.exists(model$outputdir)) dir.create(model$outputdir)
  
  #Load packages and save sessionInfo()
  if(length(model$packages) >= 1) lapply(model$packages,loadPackage)
  model$sessionInfo <- sessionInfo()
  
  #create a set of lists for the steps in the global environment
  lapply(1:length(model$steps),
         function (x) assign(model$steps[x],
                            list(step=model$steps[x],steptitle=model$steptitles[x],inputs=list(),inputtables=list(),summary=list(),outputs=list(),validation=list()),
                            envir = .GlobalEnv))
  
  return(model)
  
}
