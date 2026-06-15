# BestBaseWorstModel

## Overview

This project is intended to predict the price distribution of ISO-New England (ISONE) average monthly Day-Ahead Locational Marginal Pricing (DA-LMP). There are two primary components:

1\. A Shiny App for uploading daily ICE futures reports and examining the resulting DA-LMP settlement distribution for each contract

2\. A stastistical model that functions as the engine of the project, combining linear regression, a Gamma GLM, and Monte Carlo simulation to produce predictive price distributions

The app is designed for users who do not have to interact with the model directly, but its components and statistical underpinnings are described in the full methodology write-up lined below:

LINK
