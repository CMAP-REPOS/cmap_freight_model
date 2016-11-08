#' Simulates the application of a logit model, with calibration
#'
#' This is a generic function that is capable for simulating a logit choice
#' inputs required are a data.table and model specification data.frame
#' the output is a vector of the simulated choices
#' @param df data.table of observations
#' @param mod a table of coefficients (df and mod must contain the same set of variables)
#' @param cal a table of calibration data
#' @param calcats a correspondence table between the choice categories and the calibration categories
#' @param iter the number of iterations of the application to run; 1 simply applies the model, >1 adjusts constants in the model to match calibration targets 
#' @keywords Application
#' @export
#' @examples
#' \dontrun{
#' predict_logit(df,mod) #applies the model without calibration
#' }

predict_logit <- function(df,mod,cal=NULL,calcats=NULL,iter=1){
  
  #prepare the data items used in the model application and calibration
  alts <- max(mod$CHID)
  ut<-diag(alts)
  ut[upper.tri(ut)] <- 1
  if(is.numeric(df$CATEGORY)) df[,CATEGORY:=paste0("x",CATEGORY)] #numeric causes problems with column names
  cats <- unique(df$CATEGORY)
  mod<-data.table(expand.grid.df(mod,data.frame(CATEGORY=cats)))
  if(is.numeric(cal$CATEGORY)) cal[,CATEGORY:=paste0("x",CATEGORY)]
  
  for (iters in 1:iter){
    #calibration loops
    
    if(iters>1 & !is.null(cal) & !is.null(calcats)){
      
      #After first iternaton compare results with targets and calculate adjustment
      sim <- sapply(cats,function(x) tabulate(simchoice[min(df$Start[df$CATEGORY==x]):max(df$Fin[df$CATEGORY==x])],nbins=alts))
      sim <- sim/colSums(sim)
      
      if(length(unique(calcats$CHOICE))<length(unique(calcats$CHID))) {#the sim choices need to be aggregated to the calibration data
        sim <- cbind(calcats,sim)
        sim <- melt(sim,id.vars=c("CHOICE","CHID"),variable.name="CATEGORY")
        sim <- sim[,list(MODEL=sum(value)),by=list(CHOICE,CATEGORY)]
        sim <- merge(sim,cal,c("CATEGORY","CHOICE"))
        sim[,ascadj:=log(TARGET/MODEL)]
        adj <- merge(sim,calcats,"CHOICE",allow.cartesian=TRUE)[,list(CATEGORY,CHID,ascadj)]
      } 
      if(length(unique(calcats$CHOICE))>length(unique(calcats$CHID))) {#the calibratin data need to be aggregated to the sim choices
        caldat <- merge(cal[CATEGORY %in% cats],calcats,"CHOICE")
        caldat <- caldat[,list(TARGET=sum(TARGET)),by=list(CATEGORY,CHID)]
        sim <- data.table(CHID=1:nrow(sim),sim)
        sim <- melt(sim,id.vars=c("CHID"),variable.name="CATEGORY")
        sim <- merge(sim,caldat,c("CATEGORY","CHID"))
        sim[,ascadj:=log(TARGET/value)]
        adj <- sim[,list(CATEGORY,CHID,ascadj)]
      }
      if(length(unique(calcats$CHOICE))==length(unique(calcats$CHID))) {
        stop("Need to implment calibration for same calcats as choice alts")
        #####TODO add in some code here for the other case:
        #####1. same number of choices as calibration categories (so not aggregation required)
        
      }
      
      mod <- merge(mod,adj,c("CATEGORY","CHID"))
      mod[TYPE=="Constant",COEFF:=COEFF+ascadj]
      mod[,ascadj:=NULL]
    }
    
    #apply the choice model  
    utils <- lapply(cats, function(y) sapply(1:alts, function(x) exp(rowSums(sweep(df[CATEGORY==y,mod[CHID==x & CATEGORY==y,VAR],with=F],2,mod[CHID==x & CATEGORY==y,COEFF],"*")))))
    utils <- lapply(1:length(cats),function(x) (utils[[x]]/rowSums(utils[[x]])) %*% ut)
    utils <- do.call("rbind",utils)
    temprand <- runif(max(df$Fin))
    simchoice <- unlist(lapply(1:nrow(df),function(x) 1L + findInterval(temprand[df$Start[x]:df$Fin[x]],utils[x,])))
  }
  return(simchoice)
}  
