bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR")
# bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR",
                     # "DPTRS + GRS2","DPTRS60 + GRS2","M120 + GRS2","CPH + GRS2","LR + GRS2",
                     # "DPTRS + Hba1c","DPTRS60 + Hba1c",
                     # "DPTRS + GRS2 + Hba1c","DPTRS60 + GRS2 + Hba1c")
#### at 3 years horizon -----------

model <- "Cox"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,  bonus_variables =  bonus_variables)

gg_stage0 <- plot_time_article(formula_v, data_time,affix = paste0(model,"_Stage0"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "", horizon = 3)

gg_stage0 <- gg_stage0 + geom_text_repel(data = gg_stage0$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         min.segment.length	= 0,
                                       #  colour = "black",
                                         
                                         seed = 42) +
  theme(legend.position="none") +
  # theme(axis.title.x = element_text( hjust = 0.05),
  #       axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.3)))

gg_stage1 <- plot_time_article(formula_v, data_time, affix = paste0(model,"_Stage1"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "", horizon = 3)

gg_stage1 <- gg_stage1 + geom_text_repel(data = gg_stage1$data %>% 
                                              mutate(label = ifelse(group_variable == 'Classic models',
                                                                    formula, "")),
                                            aes(label = label), 
                                            box.padding = 0.5,
                                            show.legend = FALSE, #this removes the 'a' from the legend
                                            max.overlaps = Inf,
                                       #  colour = "black",
                                            seed = 42) +
  theme(legend.position="none") +
    # theme(axis.title.x = element_text( hjust = 0.05),
    #     axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))

gg_stage2 <- plot_time_article(formula_v, data_time, affix = paste0(model,"_Stage2"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "", horizon = 3)

# capture legend before removing it from the plot
legend <- get_legend(gg_stage2 ) 


gg_stage2 <- gg_stage2 + geom_text_repel(data = gg_stage2$data %>% 
                    mutate(label = ifelse(group_variable == 'Classic models',
                                          formula, "")),
                  aes(label = label), 
                  box.padding = 0.5,
                  show.legend = FALSE, #this removes the 'a' from the legend
                  max.overlaps = Inf,
                  min.segment.length= 0,
                #  colour = "black",
                  seed = 42) +
  theme(legend.position="none") +
  # theme(axis.title.x = element_text( hjust = 0.05),
  #       axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))

gg <- cowplot::plot_grid(gg_stage0, gg_stage1, gg_stage2, labels = c('a', 'b', 'c'), nrow = 3, label_size = 12)

gg_leg <- plot_grid(gg,legend, ncol = 2, rel_widths  = c(1, .1))

ggsave(filename = paste0(path_figures,"times_3_years_horizon.pdf"),plot = gg_leg, dpi = 600, width= 9.5, height = 9,  bg = "white")


# at 5 years horizon
model <- "Cox"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,  bonus_variables =  bonus_variables)
gg_stage0 <- plot_time_article(formula_v, data_time, affix = paste0(model,"_Stage0"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")",title = "", horizon = 5) +
  theme(legend.position="none")

gg_stage0 <- gg_stage0 + geom_text_repel(data = gg_stage0$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                       #  colour = "black",
                                         seed = 42) +
  theme(legend.position="none") +
  # theme(axis.title.x = element_text( hjust = 0.05),
  #       axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.2)))

gg_stage1 <- plot_time_article(formula_v, data_time, affix = paste0(model,"_Stage1"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "", horizon = 5)
gg_stage1 <- gg_stage1 + geom_text_repel(data = gg_stage1$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                       #  colour = "black",
                                         seed = 42) +
  theme(legend.position="none") +
  # theme(axis.title.x = element_text( hjust = 0.05),
  #       axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.2)))

gg_stage2 <- plot_time_article(formula_v, data_time, affix = paste0(model,"_Stage2"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "", horizon = 5)

# capture legend before removing it from the plot
legend <- get_legend(gg_stage2) 


gg_stage2 <- gg_stage2 + geom_text_repel(data = gg_stage2$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 2.,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         force_pull = 0.,
                                       #  colour = "black",
                                         seed = 43) +
  theme(legend.position="none") +
  # theme(axis.title.x = element_text( hjust = 0.05),
  #       axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.3)))

gg <- cowplot::plot_grid(gg_stage0, gg_stage1, gg_stage2, labels = c('a', 'b', 'c'), nrow = 3, label_size = 12)
gg_leg <- plot_grid(gg,legend, ncol = 2, rel_widths  = c(1, .1))

ggsave(filename = paste0(path_figures,"times_5_years_horizon.pdf"),plot = gg_leg, dpi = 600, width= 9.5, height = 9, bg = "white")

ggsave(filename = paste0(path_figures,"times_5_years_horizon.JPEG"),plot = gg_leg, dpi = 600, width= 9.5, height = 9, bg = "white")

