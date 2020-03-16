# Data book in https://osf.io/s8mey/
# Raw data in https://osf.io/9x3tm/

# Load packages
source('./scripts/functions/load_data_functions.R')

# Directories
input_dir <- 'input/exp2b/'
output_dir <- 'output/exp2b/fast_dm/'

# List of parameters of each condition
conditions <- list(
  list(selected_block = 1, selected_cues = c(1,2), name = "_D100",
       responses = c(1, 0, 2), do_append = F),
  list(selected_block = 2, selected_cues = c(3,4), name = "_D010",
       responses = c(1, 0, 2), do_append = F),
  list(selected_block = 1, selected_cues = c(3,4), name = "_ND",
       responses = c(0, 1, 2), do_append = F),
  list(selected_block = 2, selected_cues = c(1,2), name = "_ND",
       responses = c(0, 1, 2),do_append = T)
)

# Prepare the use of furrr
plan(multiprocess)

# Extract the trials of each participant and save them for fast-dm
system.time(
  future_walk(list.files(input_dir, 'DATA', full.names = TRUE),
       process_datafile,
       output_dir = output_dir,
       condition_list = conditions)
)

# Fit the model for the non devalued trials
system('./output/exp2b/fast_dm/fit_nondevalued.bat')

# Read the t0 data estimated from the non devalued trials
t0_data <- read.table('output/exp2b/fast_dm/logs/ND.log',
                      stringsAsFactors = F,
                      header = T) %>%
  as_tibble()

# Create the ctl file for each participant
future_walk(1:nrow(t0_data), participant_ctl, df = t0_data)

# Fit the model for the devalued trials
system('.output/exp2b/fast_dm/fit_devalued.bat')

# Join together all the 
system.time(
behav_data <- future_map_dfr(list.files(input_dir, 'DATA', full.names = TRUE),
              join_participants,
              output_dir = output_dir,
              condition_list = conditions)
)

behav_data %>%
  filter(response !=2) %>%
  group_by(set, group, participant) %>%
  count() %>%
  group_by(set) %>%
  summarise(min = min(n),
            max = max(n),
            median = median(n),
            mean = round(mean(n), 2),
            sd = round(sd(n), 2)) %>%
  write_csv('output/exp2b/valid_trials.csv')

behav_data %>%
  filter(response !=2) %>%
  group_by(set, group, id) %>%
  count() %>%
  ggplot(aes(y = n, x = set, color = group)) +
  geom_jitter(height = 0) +
  geom_text(aes(label = id))
