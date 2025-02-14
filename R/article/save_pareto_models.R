save_model <- function(outcome  =  "T1D.Indicator",
                                 time = "time_since_first_screening",
                                 formula,
                                 trainData,
                                 testData,
                                 bonus_variables = NULL,
                                 model_2_fit,
                                 path_to_export_dt, affix){
  
  response <-  paste0("Surv(",time,",", outcome,")")
  formula_v <- paste(response,formula, sep = " ~ ")
  n1 <- Sys.time()


  models <- create_models(model_2_fit = model_2_fit, formula_v = formula_v, arg_l = list(x = TRUE, data = trainData))
  for(i in 1:length(models)){ #remove patients information
    models[[i]]$x <- NULL 
    models[[i]]$y <- NULL 
    models[[i]]$linear.predictors <- NULL 
    models[[i]]$residuals <- NULL 
  }
  save(models, file=paste0(path_to_export_dt,affix,"pareto_models",".Rdata"))
}


# load parameters for stage 0
parameters <- parameters_by_stage(stage = "stage0")

dt <- data_demographics_ogtt %>% 
  select(any_of(c(parameters$variables,parameters$time,parameters$outcome,"Draw.Date.x","AB_group", c("DPTRS","DPTRS60","M120","CPH","LR")))) %>% 
  drop_na()
# select 1AB only

dt <- dt %>% filter(AB_group %in% c("GAD","IAA","IA2A"))
trainData <-
  dt[dt$Draw.Date.x < "2012-02-20", ] %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,c("DPTRS","DPTRS60","M120","CPH","LR")))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
testData <-
  dt[dt$Draw.Date.x >= "2012-02-20", ]  %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,c("DPTRS","DPTRS60","M120","CPH","LR")))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
path_to_export_dt <- paste0(here(),"/pareto_front_models/")

pareto_formula_clean <- res_pareto$formula
pareto_formula_unclean <- unclean_formula(formula =pareto_formula_clean, tex = F)

save_model(outcome  =  parameters$outcome,
           time = parameters$time,
           formula = pareto_formula_unclean,
           trainData = trainData,
           testData = testData,
           model_2_fit = "coxph",
           path_to_export_dt = path_to_export_dt,
           affix = "Cox_Stage0"
)    

# load parameters for stage 1
parameters <- parameters_by_stage(stage = "stage1")


dt <- data_demographics_ogtt %>% 
  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"Draw.Date.x","AB_group","ADA_dysglycemia", "DPTRS","DPTRS60","M120","CPH","LR"))) %>% 
  drop_na()

# select mAB normoglycemia stage 1

dt <- dt %>% filter(ADA_dysglycemia == "Euglycemia" & !(AB_group %in% c("GAD","IAA","IA2A")))
trainData <-
  dt[dt$Draw.Date.x < "2012-02-20", ] %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,  "DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
testData <-
  dt[dt$Draw.Date.x >= "2012-02-20", ]  %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)

save_model(outcome  =  parameters$outcome,
           time = parameters$time,
           formula = pareto_formula_unclean,
           trainData = trainData,
           testData = testData,
           model_2_fit = "coxph",
           path_to_export_dt = path_to_export_dt,
           affix = "Cox_Stage1"
)    

# load parameters for stage 2
parameters <- parameters_by_stage(stage = "stage2")
dt <- data_demographics_ogtt %>% 
  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"Draw.Date.x","AB_group","ADA_dysglycemia","DPTRS","DPTRS60","M120","CPH","LR"))) %>% 
  drop_na()
# select mAB normoglycemia stage 2

dt <- dt %>% filter(ADA_dysglycemia == "Dysglycemia" & !(AB_group %in% c("GAD","IAA","IA2A")))
trainData <-
  dt[dt$Draw.Date.x < "2012-02-20", ] %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
testData <-
  dt[dt$Draw.Date.x >= "2012-02-20", ]  %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome, "DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)

save_model(outcome  =  parameters$outcome,
           time = parameters$time,
           formula = pareto_formula_unclean,
           trainData = trainData,
           testData = testData,
           model_2_fit = "coxph",
           path_to_export_dt = path_to_export_dt,
           affix = "Cox_Stage2"
)    

# Load required libraries
library(survival)
library(broom)
library(openxlsx)


# Create a new workbook
wb <- createWorkbook()
namesheet <- c("Single autoantibody","stage 1","stage 2")
model_list <- list()
for(j in 1:3){
  load(paste0(here(),"/pareto_front_models/Cox_Stage",j-1,"pareto_models.Rdata"))
  for (i in seq_along(pareto_formula_clean)) {
    model <- models[[i]]
    coef_table <- tidy(model)
    model_name <- pareto_formula_clean[i]
    model_list[[model_name]] <- coef_table
  }
  
addWorksheet(wb, namesheet[j])
# Initialize the starting row 
current_row <- 1 
# Write each table below the other 
for (model_name in names(model_list)) { 
# Write the model name 
writeData(wb, sheet = j, x = data.frame(Model = model_name), startRow = current_row, colNames = FALSE) 
current_row <- current_row + 1 
# Write the coefficients table 
writeData(wb, sheet = j, x = model_list[[model_name]], startRow = current_row) 
current_row <- current_row + nrow(model_list[[model_name]]) + 2 
# Move down a few rows for spacing
} 
}
# Save the workbook
saveWorkbook(wb, file = paste0(here(),"/article/Diabetolegia/model_coefficients.xlsx"), overwrite = TRUE) 
