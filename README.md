# Stat172_final_project
Repository for stat172 final project
---
title:
authors: "Lynette Ndibalekera and Haley Harves"
date: "12/7/2025"
output: "html_document"
---

## Introduction
This repository contains the code and data required for predicting NFL draft outcomes using NFL combine data that was downloaded from Opendatabay.
Our goal of this project is to see how well we can predict draft outcomes using only NFL Combine data and analyze the important variable predictors.

### Project Structure
```
├── Stat172_final_project
├── raw
│       └── combine.csv
│       └── draft.csv          
├── src
│           └── allPlayers.R
│           └── cleaning.R
│           └── defense.R
│           └── OandDLine.R
│           └── offense.R
│           └── QBs.R
│           └── ridgeandlasso.R
│           └── smallerPositions.R
│           └── visualizations.R
├── Output
│       └── 3_cone_vs_40-yard.pdf
|       └── age_and_40-yard.pdf
|       └── age_and_weight.pdf
|       └── age_histogram.pdf
|       └── age_vs_weight.pdf
|       └── agility_vs_speed.pdf
|       └── all_players_final_forest_importance_plot.pdf
|       └── All_players_final_forest_plot.pdf
|       └── all_players_final_forest_rocCurve.pdf
|       └── boxplots_of_weight_by_region.pdf
|       └── defense_ctree.pdf
|       └── defense_final_forest_rocCurve.pdf
|       └── defense_importance_plot.pdf
|       └── defense_tuned_ctree.pdf
|       └── defense_tuned_tree_rocCurve.pdf
|       └── lasso.pdf
|       └── final_forest_rocCurve_smallerPositions.pdf
|       └── forest_comparison.pdf
|       └── heatmap_top3picks_plot.pdf
|       └── lasso.pdf
|       └── mtry_tuning_plot_defense.pdf
|       └── mtry_tuning_plot.pdf
|       └── OandDLine_ctree.pdf
|       └── OandDLine_final_forest_rocCurve.pdf
|       └── OandDLine_mtry.pdf
|       └── OandDLine_rocCurve.pdf
|       └── OandDLine_tunedtree.pdf
|       └── offense_ctree.pdf
|       └── offense_final_forest_rocCurve.pdf
|       └── offense_prunedtree.pdf
|       └── offense_randomforest.pdf
|       └── offense_rf_results.pdf
|       └── offense_rocCurve.pdf
|       └── QBs_ctree.pdf
|       └── QBs_rocCurve.pdf
|       └── random_forest_smallerPositions.pdf
|       └── ridge.pdf
|       └── scatter_plot_for_weight_vs_40-yard.pdf
|       └── smallerPositions_ctree.pdf
|       └── smallerPositions_tunedtree.pdf
|       └── tunedtree_rocCurve.pdf
|       └── var_importance_plot.pdf
│       └── final_forest.ALL.rds
│       └── final_forest_QBs.rds
│       └── final_forest_smallerPositions.rds
└── README.md
```

###  Data 
The files containing the raw data are located in the 'Stat172_final_project/raw' folder

## Cleaning
We merged the combine data and the draft data on player ID keeping all combine players. We also filtered the data to keep only data from 2000 and beyond. Allocated different states to the regions where they belong and imputed missing values. 

### 1. Code
- All scripts with the code are located in the 'Stat_172_final_project/src' folder.
  Requirements
To install the required packages, run the following code in R:

```r
install.packages(c("tidyverse", "rpart", "rpart.plot", "pROC", "dplyr", "randomForest",
                    "tidymodels", "glmnet", "usmap"))
```
### 2. Visualizations
- All visualizations were saved as PDF files and a located in the 'Stat172_final_project/output.

## Requirements
-  Libraries and packages : 
  - tidyverse
  - rpart
  - rpart.plot
  - pROC
  - dplyr
  - randomForest
  - tidymodels
  - glmnet
  - usmap

### Models

Random Forests :


Ridge Regression:


Lasso Regression: 

------------------------------------------------------------------------
## Authors

-   Haley Harves - Data Analyst
-   Lynette Ndibalekera - Data Analyst




























