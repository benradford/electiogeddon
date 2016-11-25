# Electiogeddon
Benjamin J. Radford  
November 24, 2016  
[http://www.benradford.com/electiogeddon/](http://www.benradford.com/electiogeddon/)  

---------

![2016 Electiogeddon U.S. Map](https://raw.githubusercontent.com/benradford/electiogeddon/images/header_map.png)

## Description:

This project includes code and data necessary to analyze the 2016 U.S. presidential election results by county. Particular attention is paid to identifying discrepencies in vote based on the voting technology utilized per county (electronic versus paper or optical ballot). Further analyses of these data are encouraged.

## Contents:

* __electiogeddon.r__: Code to analyze election results and produce visualizations.
* __data/census_cleaned__: Demographic data on race and sex from the [U.S. Census Bureau](https://www.census.gov/popest/data/counties/asrh/2015/index.html).
* __data/Education.csv__: Education data from the [U.S. Department of Agriculture](https://www.ers.usda.gov/data-products/county-level-data-sets/download-data.aspx).
* __data/PopulationEstimates.csv__: Population data from the [U.S. Department of Agriculture](https://www.ers.usda.gov/data-products/county-level-data-sets/download-data.aspx).
* __data/Unemployment.csv__: Unemployment data from the [U.S. Department of Agriculture](https://www.ers.usda.gov/data-products/county-level-data-sets/download-data.aspx).
* __data/election_results.csv__: 2016 U.S. presidential election results from [tonmcg](https://github.com/tonmcg/County_Level_Election_Results_12-16).
* __data/verifier-search.csv__: 2016 U.S. presidential election voting technology from [VerifiedVoting.org](https://www.verifiedvoting.org).
* __cb_2015_us_county_200k__: County-level (adm02) shapefile for U.S.A. from the [U.S. Census Bureau](https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html).

## Model choices:

The dependent variable is the _proportion of vote_ received by the GOP candidate, Donald Trump, per county. Predictors include _electronic voting machines (indicator)_, _population (logged)_, _percent unemployment_, _percent college degree_, and _precent white_. For simplicity the dependent variable is conceptualized as a continuous-valued response and no link function is used. Therefore, these are purely linear models and predicted values are not bound by 0 and 1 as proportions would be.

For the national models, linear mixed effects models are selected. Random intercepts are included per state. For that state-level models, standard linear regression models are selected.

For data merging issues, Alaska is omitted. For ease of visualization, Hawaii is also omitted.

A full write-up can be found at [http://www.benradford.com/electiogeddon](http://www.benradford.com/electiogeddon).