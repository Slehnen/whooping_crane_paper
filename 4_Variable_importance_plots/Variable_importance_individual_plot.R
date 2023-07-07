library(ggpubr)
library(ggthemes)
library(dplyr)
library(forcats)

ggplot_imp <- function(dataframe, model_parts) {
  metric_name <- attr(model_parts[[1]], "loss_name")
  metric_lab <- paste(
    "One  minus AUC loss after permutations")
  
  full_vip <- bind_rows(dataframe) %>%
    filter(variable != "_baseline_")
  
  perm_vals <- full_vip %>% 
    filter(variable == "_full_model_") %>% 
    group_by(label) %>% 
    summarise(dropout_loss = mean(dropout_loss))
  
  p <- full_vip %>%
    filter(variable != "_full_model_") %>% 
    mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    ggplot(aes(dropout_loss, variable)) 
  if(length(dataframe) > 1) {
    p <- p + 
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(alpha = 0.2)
  } else {
    p <- p + 
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(fill = "#91CBD765", alpha = 0.4)
    
  }
  p +
    theme(legend.position = "none") +
    theme_few(15)+
    labs(x = metric_lab, 
         y = NULL,  fill = NULL,  color = NULL)
}


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
model_parts_ensemble_pop2 <- readRDS("pop_var_dat_ind.RDS")
model_parts_ensemble_pop <- readRDS("model_parts_ensemble_pop_ind.RDS")

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
model_parts_ensemble_day2 <- readRDS("Day_var_dat_group_ind.RDS")
model_parts_ensemble_day <- readRDS("model_parts_ensemble_day_group_ind.RDS")
ggplot_imp(model_parts_ensemble_day2, model_parts_ensemble_day)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
model_parts_ensemble_HR2 <- readRDS("HR_var_dat_ind.RDS")
model_parts_ensemble_HR <- readRDS("model_parts_ensemble_HR_ind.RDS")

model_parts_ensemble_roost2 <- readRDS("Roost_var_dat_ind.RDS")
model_parts_ensemble_roost <- readRDS("model_parts_ensemble_roost_ind.RDS")
ggplot_imp(model_parts_ensemble_roost2, model_parts_ensemble_roost)


ggarrange(ggplot_imp(model_parts_ensemble_pop2, model_parts_ensemble_pop),
          ggplot_imp(model_parts_ensemble_HR2, model_parts_ensemble_HR),
          ggplot_imp(model_parts_ensemble_day2, model_parts_ensemble_day),
          ggplot_imp(model_parts_ensemble_roost2, model_parts_ensemble_roost), 
          ncol = 2, nrow = 2,
          labels = c("a)", "b)", "c)", "d)"))

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Figures/Figures_for_pub")
jpeg(filename = "Individual_variable_importance.jpeg", width = 3500, height = 3600,
     pointsize = 12, quality = 100, bg = "white", res = 300)
ggarrange(ggplot_imp(model_parts_ensemble_pop2, model_parts_ensemble_pop),
          ggplot_imp(model_parts_ensemble_HR2, model_parts_ensemble_HR),
          ggplot_imp(model_parts_ensemble_day2, model_parts_ensemble_day),
          ggplot_imp(model_parts_ensemble_roost2, model_parts_ensemble_roost), 
          ncol = 2, nrow = 2,
          labels = c("a)", "b)", "c)", "d)"))
dev.off()

