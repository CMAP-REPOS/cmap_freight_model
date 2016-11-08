#' Skimned Costs and Travel Times for Mode and Path Combinations
#'
#' This file contains the skims for origins and destinations with available path costs and times. The skims are based on 54 paths for different modal and route alternatives defined in the mesoscale model. These alternatives included direct modes (such as truck – Full truck load, truck – less than truck load, rail etc), indirect mode (such as rail-truck, water-truck etc), and also intermodal facilities (such as airports, truck terminals, rail terminals, and ports). This table has times and costs associated with all the 54 mode-path choices between all buyer-seller pairs.   
#'  
#' This table is used in the mode and path choice model to define travel times and cost for each of the 54 path alternatives for travel to, from and within the CMAP region. 
#' @format A dataframe with 241 variables
#' \describe{
#'   \item{Origin}{Origin ID of the zone pair}
#'   \item{Destination}{Destination ID of the zone pair}
#'   \item{...}{(series of fields not used in the model)}
#'   \item{Cost 1 : Cost 54}{Travel Cost per unit weight for each of 54 path alternatives}
#'   \item{Time 1 : Time 54}{Travel Time for each of 54 path alternatives}
#' }
#' @source The sources of this table are the networks, cost assumptions, and path-building assumptions as defined in the mesoscale model documentation.    
"data_modepath_skims"
