# main results
# 

# path_to_export_dt <-  here()
# 
# method
# 
# table 1
# 
# table cost main text
# # cost information can be found in T1Dprediction_trialnet/cost
# 
# table cost details in supplementary
# 
# figure performances
# 
# main load 
source("~/Desktop/T1Dprediction_trialnet/R/main_load.R")
path_figures <-"~/Desktop/T1Dprediction_trialnet/article/figures/" 
path_to_export_dt <-  paste0(here(),"/analysis_28_06")
# figures performances by group of variables

source("~/Desktop/T1Dprediction_trialnet/R/article/model_performances.R")

# figure performances by cost

source("~/Desktop/T1Dprediction_trialnet/R/article/model_cost.R")

# figures performances by times
# 
source("~/Desktop/T1Dprediction_trialnet/R/article/times_performances.R")

# GRS figures to show how including GRS increase performances
# 
source("~/Desktop/T1Dprediction_trialnet/R/article/GRS_impact.R")
