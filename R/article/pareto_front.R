# pareto front
source("~/Desktop/T1Dprediction_trialnet/R/train and test models/parameters_by_stage.R", echo=TRUE)
library("rPref") 
library("gtools")

# heat map
# 
# load all of the data

# bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR")
bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR",
                     "DPTRS + GRS2","DPTRS60 + GRS2","M120 + GRS2","CPH + GRS2","LR + GRS2",
                     "DPTRS + Hba1c","DPTRS60 + Hba1c",
                     "DPTRS + GRS2 + Hba1c","DPTRS60 + GRS2 + Hba1c")
horizon <- 3


# cost and performances

model <- "Cox"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,  bonus_variables =  bonus_variables)
gg_stage0 <-  plot_cost_for_article(formula_v, cost_l, affix = paste0(model,"_Stage0"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 2")

cost_stage0 <- gg_stage0$data %>% rename(AUC_stage0 = AUC, Brier_stage0 = Brier_score) %>% select(formula, cost,AUC_stage0, Brier_stage0 )

gg_stage1 <-  plot_cost_for_article(formula_v, cost_l, affix = paste0(model,"_Stage1"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 2")
cost_stage1 <- gg_stage1$data %>% rename(AUC_stage1 = AUC, Brier_stage1 = Brier_score) %>% select(formula, AUC_stage1, Brier_stage1 )

gg_stage2 <- plot_cost_for_article(formula_v, cost_l, affix = paste0(model,"_Stage2"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 2")


cost_stage2 <- gg_stage2$data %>% rename(AUC_stage2 = AUC, Brier_stage2 = Brier_score) %>% select(formula, AUC_stage2, Brier_stage2)


cost <- cost_stage0 %>% left_join(cost_stage1, by = c("formula" = "formula")) %>% left_join(cost_stage2,by = c("formula" = "formula"))
# time

gg_stage0 <- plot_time_article(formula_v, data_time,affix = paste0(model,"_Stage0"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Single AB", horizon = 3)

time_formula <- gg_stage0$data %>% select(formula, time)


df <- cost %>% left_join(time_formula) %>% 
  mutate(AUC = round(mean(c(AUC_stage0,AUC_stage1,AUC_stage2)),3))
df <- df %>% dplyr::arrange(AUC) %>% select(formula, AUC_stage0, Brier_stage0, AUC_stage1, Brier_stage1, AUC_stage2, Brier_stage2, cost, time) 

# percentize function to find the rank, 
# to account the fact that high Brier score, time and cost are bad we invers them
df_i <- df %>% mutate_at(vars(contains("Brier")), ~ 1 - .x ) %>% mutate(cost = - cost, time = - time )
df_percentize <- heatmaply::percentize(as.matrix(df_i[,-1]))
df_percentize <- heatmaply::normalize(df_percentize) # to ensure that it is between 0 and 1
df_percentize <- cbind(df[,1], df_percentize)

df_melt_percentize <- melt(df_percentize, id = "formula", variable.name = "scale", value.name = "Rank")



df_melt <- melt(df, id = "formula", variable.name = "scale", value.name = "value")

# combine the two
# 
df_combined <- df_melt %>% left_join(df_melt_percentize)


df_pareto <- df %>% group_by(formula) %>%  
  mutate(AUC = round(mean(c(AUC_stage0,AUC_stage1,AUC_stage2)),3), Brier = round(mean(c(Brier_stage0,Brier_stage1,Brier_stage2)),3)) %>% ungroup()      


# df_pareto <- df %>% group_by(formula) %>%  
#   mutate(AUC = mean(AUC_stage0,AUC_stage1,AUC_stage2), Brier = mean(Brier_stage0,Brier_stage1,Brier_stage2)) %>% ungroup()      
res_pareto <- psel(df_pareto, low(cost) * low(time) * high(AUC) * low(Brier), top_level = 1)
res_pareto <- res_pareto %>%  distinct(cost,time,AUC,Brier,.keep_all = TRUE)

formula_to_plot <- res_pareto$formula
length(formula_to_plot)
df_melt_subset <- df_combined %>% filter(formula %in% formula_to_plot) %>% mutate(formula = clean_formula(formula, tex = TRUE))


df_melt_subset <- df_melt_subset %>% mutate(formula = factor(formula, levels = unique(df_melt_subset$formula))) %>% 
  mutate(scale  = clean_label(scale))

library(latex2exp)
gg <- ggplot(df_melt_subset, aes(scale, formula, fill=Rank)) + 
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size=2.5) +
  theme_bw(base_size=10) +
  theme(axis.text.x = element_text(angle = 0), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.margin = unit(c(3, 1, 0, 0), "mm")) +
  scale_fill_gradient(low="white", high="lightblue",name = "Rank",limits = c(0,1), breaks = c(0,1),labels = c("Worst model", "Best model") )  +
  scale_y_discrete(labels=TeX(levels(df_melt_subset$formula))) +
  scale_x_discrete(labels=TeX(unique(df_melt_subset$scale))) 
gg
ggsave(filename = paste0(path_figures,"pareto_front_table.JPEG"),plot = gg, dpi = 500, width= 9, height = 9,  bg = "white")


df <- df %>% group_by(formula) %>%  
  mutate(AUC = mean(c(AUC_stage0,AUC_stage1,AUC_stage2)), Brier = mean(c(Brier_stage0,Brier_stage1,Brier_stage2))) %>% ungroup()      
histogram(df$AUC)
histogram(df$Brier)
histogram(df$cost)
histogram(df$time)
res_pareto <- res_pareto %>% mutate(performance_power = case_when(
  AUC > 0.7 & Brier < 0.165 ~ "good",
  AUC > 0.65 & Brier < 0.17 ~ "mild",
  AUC < 0.65 & Brier > 0.17 ~ "poor",
  TRUE ~ "unclassified"
)) %>% 
  mutate(performance_cost = case_when(
    cost > 200 ~ "poor",
    cost > 100 ~ "mild",
    cost <= 100 ~ "good",
    TRUE ~ "unclassified"
  )) %>% 
  mutate(performance_time = case_when(
    time >= 120 ~ "poor",
    time > 60 ~ "mild",
    TRUE ~ "good"
  )) 
  
table(res_pareto$performance_power, res_pareto$performance_cost, res_pareto$performance_time)

df %>% mutate(performance_power = case_when(
  AUC > 0.7 & Brier < 0.165 ~ "good",
  AUC > 0.65 & Brier < 0.17 ~ "mild",
  AUC < 0.65 & Brier > 0.17 ~ "poor",
  TRUE ~ "unclassified"
)) %>% 
  mutate(performance_cost = case_when(
    cost > 200 ~ "poor",
    cost > 100 ~ "mild",
    cost <= 100 ~ "good",
    TRUE ~ "unclassified"
  )) %>% 
  mutate(performance_time = case_when(
    time >= 120 ~ "poor",
    time > 60 ~ "mild",
    TRUE ~ "good"
  )) %>% 
  filter(performance_power == "good" & performance_cost == "good" & performance_time == "good") %>% View()
