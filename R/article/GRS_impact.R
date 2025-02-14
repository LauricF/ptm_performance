variable_of_interest <- "GRS2"
#### at 3 years horizon -----------
bonus_variables <- c("Index60","DPTRS","DPTRS60","M120","CPH","LR",
                     "DPTRS + GRS2","DPTRS60 + GRS2","M120 + GRS2","CPH + GRS2","LR + GRS2")
# bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR","DPTRS + GRS2","DPTRS60 + GRS2","M120 + GRS2","CPH + GRS2","LR + GRS2")
model <- "Cox"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,  bonus_variables =  bonus_variables)
formula_v <- unique(formula_v)
length(formula_v)
gg_stage0 <- find_close_formula_article(formula_v,affix = paste0(model,"_Stage0"),path_to_export_dt =  path_to_export_dt, variable_of_interest = variable_of_interest, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Single AB", horizon = 5)

gg_stage0 <- gg_stage0 + geom_text_repel(data = gg_stage0$data %>% 
                                            mutate(label = ifelse((formula %in% bonus_variables),
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 1,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 45, 
                                         force = 1, 
                                         colour = "black",
                                         min.segment.length = 0,
                                         direction = "x", size = 3) +
  coord_cartesian(xlim = c(0.5, 0.9), ylim = c(0.11,0.18)) +
  geom_line(data = gg_stage0$data %>% 
              filter(formula %in% bonus_variables),aes(group = group_variable),alpha = 1,linewidth = 0.5, colour = "purple") +
  theme(legend.position="none")


gg_stage1 <- find_close_formula_article(formula_v,affix = paste0(model,"_Stage1"),path_to_export_dt =  path_to_export_dt, variable_of_interest = variable_of_interest, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 1", horizon = 3)
gg_stage1 <- gg_stage1 + geom_text_repel(data = gg_stage1$data %>% 
                                            mutate(label = ifelse(`Classic models` == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42, colour = "black",min.segment.length = 0, size = 3) +
  geom_line(data = gg_stage1$data %>% 
              filter(formula %in% bonus_variables),aes(group = group_variable),alpha = 1,linewidth = 0.5, colour = "purple") +
  theme(legend.position="none")

gg_stage2 <- find_close_formula_article(formula_v,affix = paste0(model,"_Stage2"),path_to_export_dt =  path_to_export_dt, variable_of_interest = variable_of_interest, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 2", horizon = 3)

# capture legend before removing it from the plot
legend <- get_legend(gg_stage2 +   theme(legend.position="top")) 


gg_stage2 <- gg_stage2 + geom_text_repel(data = gg_stage2$data %>% 
                                            mutate(label = ifelse(`Classic models` == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42, colour = "black",min.segment.length = 0, size = 3) +
  geom_line(data = gg_stage2$data %>% 
              filter(formula %in% bonus_variables),aes(group = group_variable),alpha = 1,linewidth = 0.5, colour = "purple") +
  theme(legend.position="none")

gg <- cowplot::plot_grid(gg_stage0, gg_stage1, gg_stage2, labels = c('A', 'B', 'C'), nrow = 3, label_size = 12)

gg_leg <- plot_grid(gg,legend, nrow = 2, rel_heights = c(0.9, .1))

ggsave(filename = paste0(path_figures,"with_or_without_",variable_of_interest,"_3_years_horizon.JPEG"),plot = gg_leg, dpi = 300, width= 6, height = 9,  bg = "white")


#### at 7 years horizon -----------
horizon <- 7
model <- "Cox"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,  bonus_variables =  bonus_variables)
formula_v <- unique(formula_v)
gg_stage0 <- find_close_formula_article(formula_v,affix = paste0(model,"_Stage0"),path_to_export_dt =  path_to_export_dt, variable_of_interest = variable_of_interest, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Single AB", horizon = horizon)

gg_stage0 <- gg_stage0 + geom_text_repel(data = gg_stage0$data %>% 
                                            mutate(label = ifelse(`Classic models` == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42, colour = "black",min.segment.length = 0, size = 3) +
  geom_line(data = gg_stage0$data %>% 
              filter(formula %in% bonus_variables),aes(group = group_variable),alpha = 1,linewidth = 0.5, colour = "purple") +
  theme(legend.position="none")

gg_stage1 <- find_close_formula_article(formula_v,affix = paste0(model,"_Stage1"),path_to_export_dt =  path_to_export_dt, variable_of_interest = variable_of_interest, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 1", horizon = horizon)
gg_stage1 <- gg_stage1 + geom_text_repel(data = gg_stage1$data %>% 
                                            mutate(label = ifelse(`Classic models` == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42, colour = "black",min.segment.length = 0, size = 3) +
  geom_line(data = gg_stage1$data %>% 
              filter(formula %in% bonus_variables),aes(group = group_variable),alpha = 1,linewidth = 0.5, colour = "purple") +
  theme(legend.position="none")

gg_stage2 <- find_close_formula_article(formula_v,affix = paste0(model,"_Stage2"),path_to_export_dt =  path_to_export_dt, variable_of_interest = variable_of_interest, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "Stage 2", horizon = horizon)

# capture legend before removing it from the plot
legend <- get_legend(gg_stage2 +   theme(legend.position="top") +  guides(colour=guide_legend(ncol=3,nrow=2,byrow=TRUE)) ) 


gg_stage2 <- gg_stage2 + geom_text_repel(data = gg_stage2$data %>% 
                                            mutate(label = ifelse(`Classic models` == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42, colour = "black",min.segment.length = 0, size = 3) +
  geom_line(data = gg_stage2$data %>% 
              filter(formula %in% bonus_variables),aes(group = group_variable),alpha = 1,linewidth = 0.5, colour = "purple") +
  theme(legend.position="none")

gg <- cowplot::plot_grid(gg_stage0, gg_stage1, gg_stage2, labels = c('A', 'B', 'C'), nrow = 3, label_size = 12)

gg_leg <- plot_grid(gg,legend, nrow = 2, rel_heights = c(0.9, .1))

ggsave(filename = paste0(path_figures,"with_or_without_",variable_of_interest,"_",horizon,"_years_horizon.JPEG"),plot = gg_leg, dpi = 300, width= 6, height = 9,  bg = "white")
