##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       0_Create_Pairs_Summaries.R makes some tabulations of the pairs table
#Date:              June 30, 2015
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2015 RSG, Inc. - All rights reserved.
##############################################################################################

library(data.table)
library(bit64)
library(reshape2)

basedir <- "E:/cmh/Meso_Freight_PMG_Base_Test_Setup"
scenario <- "base"

#large file so take some time to load into memory
load(file.path(basedir,"scenarios",scenario,"outputs","pairs.Rdata"))

pairs[,tons:=Last.Iteration.Quantity/2000]

#some simple stats
pairs_stats <- list()
pairs_stats$scenario <- scenario
pairs_stats$location <- file.path(basedir,"scenarios",scenario)
pairs_stats$number_rows <- nrow(pairs)
pairs_stats$number_unique_sellers <- length(unique(pairs$SellerID))
pairs_stats$number_unique_buyers <- length(unique(pairs$BuyerID))
pairs_stats$total_trade_quantity <- sum(pairs$tons)

#summary by naics
trade_naics <- pairs[,list(NumTrades=.N,Tons=sum(tons)),by=NAICS]
pairs_stats$trade_by_naics <- data.frame(trade_naics) #data frame so all prints in capture.output

#summary by sctg
trade_sctg <- pairs[,list(NumTrades=.N,Tons=sum(tons)),by=Commodity_SCTG][order(Commodity_SCTG)]
pairs_stats$trade_by_sctg <- data.frame(trade_sctg) #data frame so all prints in capture.output

#summary by MinPath
modes <- pairs[,list(NumTrades=.N,Tons=sum(tons)),by=MinPath][order(MinPath)]
pairs_stats$trade_by_mode <- data.frame(modes) #data frame so all prints in capture.output

#pick a sample trade and look at selling and buying
pairs_stats$trade_sample_trader <- sample(pairs$SellerID,1)
trade_sample_seller <- pairs[SellerID==pairs_stats$trade_sample_trader]
trade_sample_seller[,Prop_PurchAmount:=Last.Iteration.Quantity/PurchaseAmountTons]
pairs_stats$trade_sample_num_sales <- nrow(trade_sample_seller)
pairs_stats$trade_sample_output_cap <- trade_sample_seller[1]$OutputCapacityTons
pairs_stats$trade_sample_total_sales <- sum(trade_sample_seller$Last.Iteration.Quantity)
pairs_stats$trade_sample_seller <- head(data.frame(trade_sample_seller[,list(BuyerID, Consumption_zone, PurchaseAmountTons, Last.Iteration.Quantity, Prop_PurchAmount)]),100)

trade_sample_buyer <- pairs[BuyerID==pairs_stats$trade_sample_trader]
trade_sample_buyer[,Prop_PurchAmount:=Last.Iteration.Quantity/OutputCapacityTons]
pairs_stats$trade_sample_num_purchases <- nrow(trade_sample_buyer)
pairs_stats$trade_sample_num_inputs <- length(unique(trade_sample_buyer$NAICS))
pairs_stats$trade_sample_purchase_summary <- data.frame(trade_sample_buyer[,list(NumPurchase=.N,PurchaseRequired=min(PurchaseAmountTons),ActualPurchases=sum.integer64(Last.Iteration.Quantity)),by=NAICS][order(NAICS)])
pairs_stats$trade_sample_buyer <- data.frame(trade_sample_buyer[,list(SellerID, Production_zone, OutputCapacityTons, Last.Iteration.Quantity,Prop_PurchAmount)])

capture.output(pairs_stats,file=file.path(basedir,"scenarios",scenario,"outputs","pairs_stats.txt"))