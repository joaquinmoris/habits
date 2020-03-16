library(patchwork)
library(afex)
library(papaja)
source('./scripts/functions/fast_dm_functions.R')

# Load the results for all of the participants
results <- map_df(list.files('output/exp2b/fast_dm/logs/', '(1|3)_[0-9]', full.names = TRUE),
                  load_participant_D)

# Plot of the two parameters
list_to_patch <- map(c("a", "v"), plot_list, results = results)
list_to_patch[[1]] / list_to_patch[[2]] + xlab("Devalued outcome")

# Omnibus mixed anova
main_omnibus <- afex::aov_ez("id", "value",
                             results %>%
                               mutate(id = paste0(group,"_",id)),
                             between = "group", within = c("condition","parameter"))

omnibus_G1 <- afex::aov_ez("id", "value",
                             results %>%
                               mutate(id = paste0(group,"_",id)) %>%
                               filter(group == "1"), within = c("condition","parameter"))

omnibus_G3 <- afex::aov_ez("id", "value",
                           results %>%
                             mutate(id = paste0(group,"_",id)) %>%
                             filter(group == "1"), within = c("condition","parameter"))

for (cur_test in list(list(cur_group = "1", cur_parameter = "a"),
                      list(cur_group = "1", cur_parameter = "v"),
                      list(cur_group = "3", cur_parameter = "a"),
                      list(cur_group = "3", cur_parameter = "v"))){

  print(paste("Group:", cur_test$cur_group,"Parameter:", cur_test$cur_parameter))
  t.test(x = results %>%
           filter(group == cur_test$cur_group, parameter == cur_test$cur_parameter, condition == "10") %>%
           pull(value),
         y = results %>%
           filter(group == cur_test$cur_group, parameter == cur_test$cur_parameter, condition == "100") %>%
           pull(value),
         paired = TRUE)
}
