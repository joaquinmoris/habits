library(tidyverse)
library(patchwork)
library(afex)
library(papaja)

load_participant_D <- function(filename){
  group_id <- str_match(string = filename, pattern = 'G(1|3)_([0-9]+)')
  read.table(filename,stringsAsFactors = F, header = T) %>%
    as_tibble() %>%
    mutate(group = group_id[1,2], id = group_id[1,3]) %>%
    pivot_longer(names_to = "parameter", cols = c("a", "v"), values_to = "value") %>%
    mutate(condition = as.character(dataset)) %>%
    select(group, id, condition, parameter, value, penalty, fit)
}


plot_list <- function (results, cur_parameter){
  return (results %>%
            mutate(group = if_else(group == "1", "Short training", "Long training")) %>%
            group_by(group, condition, parameter) %>%
            filter (parameter == cur_parameter) %>%
            ggplot(aes(y = value, x = condition)) +
            geom_boxplot(outlier.shape = NA) +
            geom_jitter(height = 0, alpha = 0.2, width = 0.3) +
            ggtitle(paste("Parameter:",cur_parameter)) +
            xlab ("") +
            scale_x_discrete(labels = c(expression(Dev-O^{"10"}), expression(Dev-O^{"100"})))+
            facet_grid(.~group) +
            theme_minimal())
}

custom_paired_t.test <- function(cur_test, results){
  t.test(x = results %>%
      filter(group == cur_test$cur_group, parameter == cur_test$cur_parameter, condition == "10") %>%
      pull(value),
    y = results %>%
      filter(group == cur_test$cur_group, parameter == cur_test$cur_parameter, condition == "100") %>%
      pull(value),
    paired = TRUE)
}