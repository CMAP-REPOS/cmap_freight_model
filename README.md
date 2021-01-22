# cmap_freight_model
This repository contains working code for the development of CMAP’s tour-based and supply chain freight model, which represents the full supply chain for specific commodities shipped from supplier to customer.  It includes an agent-based computational economics extension to the model, which models commodity markets as evolving decentralized systems of autonomous agents.  This extension allows for the flexibility of testing various scenario planning options including macroeconomic changes, changes in transport and logistics costs or changes in business operating strategies.  The cornerstone of this extension is the Procurement Market Game (PMG) module, which uses game theory in matching buyers and sellers within commodity markets and allows them to develop preferential trading partners over successive iterations of the market interactions.

After annual commodity flows between individual firms are developed in the national supply chain model, a regional tour-based daily truck model simulates the tours of pickups and deliveries made by individual trucks operating in the CMAP region.

# modelDevelopment branch
Working version of the code that:
- update includes updates to transition the code to working in R Version 4.0 and with rFreight version 0.1-27
- incorporates updates to a consistent code structure for all model components
- transitions the PMG to open R code format as opposed to a compiled C++ application
- adds a visualization dashboard to view scenario results from the supply chain model
- adds a calibration framework to support calibration of the supply chain model (and other components)