bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR")
#### at 3 years horizon -----------
x_min <- 0.5
x_max <- 1

model <- "Cox"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,  bonus_variables =  bonus_variables)
gg_stage0 <- plot_summary1(formula_v,affix = paste0(model,"_Stage0"),group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", path_to_export_dt =  path_to_export_dt,  title = "", horizon = 3)

gg_stage0 <- gg_stage0 + geom_text_repel(data = gg_stage0$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42) +
  theme(legend.position="none") +
  theme(axis.title.x = element_text( hjust = 0.05),
        axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))

gg_stage1 <- plot_summary1(formula_v,affix = paste0(model,"_Stage1"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "", horizon = 3)
gg_stage1 <- gg_stage1 + geom_text_repel(data = gg_stage1$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42) +
  theme(legend.position="none") +
  theme(legend.position="none") +
  # theme(axis.title.x = element_text( hjust = 0.05),
  #       axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))

gg_stage2 <-  plot_summary1(formula_v,affix = paste0(model,"_Stage2"),group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", title = "", horizon = 3)

# capture legend before removing it from the plot
legend <- get_legend(gg_stage2 +   theme(legend.position="top") +  guides(colour=guide_legend(ncol=2,nrow=3,byrow=TRUE, override.aes = list(size = 3,alpha = 1))) ) 


gg_stage2 <- gg_stage2 + geom_text_repel(data = gg_stage2$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42) +
  theme(legend.position="none") +
  # theme(axis.title.x = element_text( hjust = 0.05),
  #       axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))

gg <- cowplot::plot_grid(gg_stage0, gg_stage1, gg_stage2, labels = c('a', 'b', 'c'), nrow = 3, label_size = 12)

gg_leg <- plot_grid(gg,legend, nrow = 2, rel_heights = c(0.9, .1))

ggsave(filename = paste0(path_figures,"performances_3_years_horizon.pdf"),plot = gg_leg, dpi = 600, width= 8, height = 9,  bg = "white")


# at 5 years horizon
model <- "Cox"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,  bonus_variables =  bonus_variables)
gg_stage0 <-  plot_summary1(formula_v,affix = paste0(model,"_Stage0"),group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", path_to_export_dt =  path_to_export_dt,  title = "", horizon = 5)

gg_stage0 <- gg_stage0 + geom_text_repel(data = gg_stage0$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42) +
  theme(legend.position="none") +
  theme(axis.title.x = element_text( hjust = 0.05),
        axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)))

gg_stage1 <-  plot_summary1(formula_v,affix = paste0(model,"_Stage1"),group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", path_to_export_dt =  path_to_export_dt,  title = "", horizon = 5)
gg_stage1 <- gg_stage1 + geom_text_repel(data = gg_stage1$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42) +
  theme(legend.position="none") +
  # theme(axis.title.x = element_text( hjust = 0.05),
  #       axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.1)))
gg_stage2 <-  plot_summary1(formula_v,affix = paste0(model,"_Stage2"),group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", path_to_export_dt =  path_to_export_dt,  title = "", horizon = 5)

# capture legend before removing it from the plot
legend <-  get_legend(gg_stage2 +   theme(legend.position="top") +  guides(colour=guide_legend(ncol=2,nrow=3,byrow=TRUE, override.aes = list(size = 3,alpha = 1))) ) 


gg_stage2 <- gg_stage2 + geom_text_repel(data = gg_stage2$data %>% 
                                           mutate(label = ifelse(group_variable == 'Classic models',
                                                                 formula, "")),
                                         aes(label = label), 
                                         box.padding = 0.5,
                                         show.legend = FALSE, #this removes the 'a' from the legend
                                         max.overlaps = Inf,
                                         seed = 42) +
  theme(legend.position="none") +
  # theme(axis.title.x = element_text( hjust = 0.05),
  #       axis.title.y = element_text( hjust = 0.05)) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.3)))

gg <- cowplot::plot_grid(gg_stage0, gg_stage1, gg_stage2, labels = c('a', 'b', 'c'), nrow = 3, label_size = 12)
gg_leg <- plot_grid(gg,legend, nrow = 2, rel_heights = c(0.9, .1))

ggsave(filename = paste0(path_figures,"performances_5_years_horizon.pdf"),plot = gg_leg, dpi = 600, width= 8, height = 9, bg = "white")
ggsave(filename = paste0(path_figures,"performances_5_years_horizon.JPEG"),plot = gg_leg, dpi = 600, width= 8, height = 9, bg = "white")
