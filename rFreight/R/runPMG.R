#' Builds the system call to the PMG application and runs the application
#'
#' Builds the systems call including the command line options to run the PMG
#' application for a particular NAICs market and group sample from within the
#' full set of buyers and sellers in that NAICS market.
#' 
#' @param naics_io_code BEA io code for the commodity to be run, i.e., that matches with the filenaming used for the buy/sell/costs files (character string).
#' @param groupnum is the sample group numbers for the group to be run, i.e., that matches with the numbering used for the buy/sell/costs files (integer).
#' @param writelog TRUE/FALSE to indicate whether to capture standard output from the PMG application is a text file.
#' @param invisible TRUE/FALSE to indicate whether to show the command window or not.
#' @param wait TRUE/FALSE to indicate whether to R should wait for the PMG application to finish, or (if false) should run the PMG application asynchronously.
#' @param pmgexe Path to the pmg executable, defaults to "./PMG/pmg.exe"
#' @param inipath Path to the ini file, defaults to "./PMG/PMG.ini"
#' @param inpath Path to the PMG inputs folder, defaults to "./outputs"
#' @param outpath Path to the PMG outputs folder, defaults to "./outputs"
#' @param logpath Path to the log file folder, defaults to "./outputs"
#' @keywords PMG
#' @export
#' @examples
#' \dontrun{
#' runPMG(naics,g,writelog=FALSE,wait=TRUE)
#' }

runPMG <- function(naics_io_code,groupnum=NA,writelog=FALSE,invisible=TRUE,wait=FALSE,
                   pmgexe="./PMG/pmg.exe",inipath="./PMG/pmg.ini",inpath="./outputs",outpath="./outputs",
                   logpath="./outputs"){
  
  #location of PMG executable: in the PMG folder, called PMG.exe
  pmgexe <- gsub("/","\\",pmgexe,fixed=TRUE)
  
  #command line options
  # 1.  Specify ini file path:
  #     -i C:\path\to\file\pmg.ini 
  inipath <- gsub("/","\\",inipath,fixed=TRUE)
  
  # 2. specify data input and output file name prefixes 
  # -p naics_io_code
  ioprefix <- naics_io_code
  if(!is.na(groupnum)) ioprefix <- paste0(naics_io_code,"_g",groupnum)
  
  # 3. specify data directory path for input files files 
  # location of naics_io_code.buy.csv, naics_io_code.sell.csv and  naics_io_code.costs.csv
  # -d C:\path\to\inputs
  inpath <- gsub("/","\\",inpath,fixed=TRUE)
  
  # 4. specify directory path for output file
  # locations of naics_io_code.out.csv
  # -o C:\path\to\outputs
  outpath <- gsub("/","\\",outpath,fixed=TRUE)
  #
  # divert stout to log for this run
  logcall <- ""
  logpath <- gsub("/","\\",logpath,fixed=TRUE)
  if(writelog) logcall <- paste0(logpath,"\\",naics_io_code,"_g",groupnum,".txt")
  
  # 
  #build system call:
  system2(pmgexe,
          args = paste("-i",inipath,"-p",ioprefix,"-d",inpath,"-o",outpath), 
          stdout = logcall,  
          invisible = invisible, 
          wait = wait)
  
}
