# bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR")
bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR",
                     "DPTRS + GRS2","DPTRS60 + GRS2","M120 + GRS2","CPH + GRS2","LR + GRS2",
                     "DPTRS + Hba1c","DPTRS60 + Hba1c",
                     "DPTRS + GRS2 + Hba1c","DPTRS60 + GRS2 + Hba1c")
#### at 3 years horizon -----------

model <- "Cox"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,  bonus_variables =  bonus_variables)
gg_stage0 <- plot_summary1(formula_v,affix = paste0(model,"_Stage0"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Single AB", horizon = 3)

gg_stage0 <- gg_stage0 + geom_text_repel(data = gg_stage0$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         min.segment.length = 0,
                                         seed = 42) +
  xlim(c(0.45,1)) +
  theme(legend.position="none")

gg_stage1 <- plot_summary1(formula_v,affix = paste0(model,"_Stage1"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 1", horizon = 3)
gg_stage1 <- gg_stage1 + geom_text_repel(data = gg_stage1$data %>% 
                                              mutate(label = ifelse(group_variable == 'Classic models',
                                                                    formula, "")),
                                            aes(label = label), 
                                            box.padding = 0.5,
                                            show.legend = FALSE, #this removes the 'a' from the legend
                                            max.overlaps = Inf,
                                            seed = 42) +
  xlim(c(0.45,1)) +
  theme(legend.position="none")

gg_stage2 <- plot_summary1(formula_v,affix = paste0(model,"_Stage2"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 2", horizon = 3)

# capture legend before removing it from the plot
legend <- get_legend(gg_stage2 +   theme(legend.position="top") +  guides(colour=guide_legend(ncol=3,nrow=2,byrow=TRUE)) ) 


gg_stage2 <- gg_stage2 + geom_text_repel(data = gg_stage2$data %>% 
                    mutate(label = ifelse(group_variable == 'Classic models',
                                          formula, "")),
                  aes(label = label), 
                  box.padding = 0.5,
                  show.legend = FALSE, #this removes the 'a' from the legend
                  max.overlaps = Inf,
                  seed = 42) +
  xlim(c(0.45,1)) +
  theme(legend.position="none")

gg <- cowplot::plot_grid(gg_stage0, gg_stage1, gg_stage2, labels = c('A', 'B', 'C'), nrow = 3, label_size = 12)

gg_leg <- plot_grid(gg,legend, nrow = 2, rel_heights = c(0.9, .1))

ggsave(filename = paste0(path_figures,"performances_3_years_horizon.JPEG"),plot = gg_leg, dpi = 300, width= 6, height = 9,  bg = "white")



##### at 5 years horizon 
model <- "Cox"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,  bonus_variables =  bonus_variables)
gg_stage0 <- plot_summary1(formula_v,affix = paste0(model,"_Stage0"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Single AB", horizon = 5) +
  theme(legend.position="none") 

gg_stage0 <- gg_stage0 + geom_text_repel(data = gg_stage0$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42) +
  theme(legend.position="none")

gg_stage1 <- plot_summary1(formula_v,affix = paste0(model,"_Stage1"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 1", horizon = 5)
gg_stage1 <- gg_stage1 + geom_text_repel(data = gg_stage1$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42) +
  theme(legend.position="none")

gg_stage2 <- plot_summary1(formula_v,affix = paste0(model,"_Stage2"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 2", horizon = 5)

# capture legend before removing it from the plot
legend <- get_legend(gg_stage2 +   theme(legend.position="top") +  guides(colour=guide_legend(ncol=3,nrow=2,byrow=TRUE)) ) 


gg_stage2 <- gg_stage2 + geom_text_repel(data = gg_stage2$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42) +
  theme(legend.position="none")

gg <- cowplot::plot_grid(gg_stage0, gg_stage1, gg_stage2, labels = c('A', 'B', 'C'), nrow = 3, label_size = 12)
gg_leg <- plot_grid(gg,legend, nrow = 2, rel_heights = c(0.9, .1))

ggsave(filename = paste0(path_figures,"performances_5_years_horizon.JPEG"),plot = gg_leg, dpi = 300, width= 6, height = 9, bg = "white")


