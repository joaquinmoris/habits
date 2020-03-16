library(R.matlab)
library(tidyverse)
library(furrr)

#############
# Functions to process the data
#############

process_datafile <- function(filename, output_dir, condition_list){
  # Load and then process and write the files
  print(filename)
  cur_data <- load_datafile(filename)
  walk(condition_list,
       write_files,
       cur_data = cur_data, filename = filename, output_dir = output_dir)
  
}


load_datafile <- function(filename){
  # Load the data from MATLAB, extract the relevant information and convert to a tibble
  cur_data <- readMat(filename)
  #cur_data <- cur_data[["DATA"]]
  if (length(cur_data[["DATA"]]) == 4){
    cur_data <- cur_data[["DATA"]][[4]]
  } else {
    cur_data <- cur_data[["DATA"]][[8]]
  }
  return(
    tibble(block = cur_data[,3], cue = cur_data[,4],
           response = cur_data[,5], rt = cur_data[,7])
  )
}


write_files <- function(cur_condition, cur_data, filename, output_dir) {
  # Use the information stored in the condition list to choose the block
  # and type of cues that will be kept, and recode the responses
  # Then save the file
  cur_data %>%
    filter(block  == cur_condition$selected_block,
           cue %in% cur_condition$selected_cues) %>%
    mutate(response = case_when(response == 0 ~ cur_condition$responses[1],
                                response == 1 ~ cur_condition$responses[2],
                                TRUE ~ cur_condition$responses[3])) %>%
    filter(response != 2) %>%
    select(response, rt) %>%
    write_delim(paste0(output_dir,
                       generate_group_id(filename),
                       cur_condition$name,
                       '.dat'),
                col_names = FALSE, append = cur_condition$do_append)
}


generate_group_id <- function(filename) {
  # Generate a string with the code of the experimental group
  # (G1 for 1 session of training, G3 for 3 sessions of training)
  # followed by an underscore and the participant's id (NNN)
  
  digits <- str_extract_all(string =
                              str_extract(filename, 'DATA[_0-9]+.mat'),
                            pattern = '[0-9]+')[[1]]
  id <- sprintf('%.3d', as.integer(digits[3]))
  
  if (digits[1] == 1 & digits[2] == 1){
    return(paste0("G1_", id))}
  else{
    if ((digits[1] == 3 & digits[2] == 2)){
      return(paste0("G3_", id))}
    else {stop("Incorrect data or data filename (session or group)")}
  }
}

# Crear el ctl de los ND y calcular el t0 de esos ensayos para todos los participantes
ND_ctl <- function(path){
  
  write_lines(paste0('method ml
precision 3
set st0 0
set zr 0.5
set d 0
set szr 0
set sv 0
set p 0
format RESPONSE TIME
load .\\data\\*_ND.dat
log .\\logs\\ND.log'), paste0(path, 'ND.ctl'))
}


# Crear el ctl de los D y fijando el t0 a partir de un valor
D_ctl <- function(participant, t0){
  
  write_lines(paste0('method ml
precision 3
set t0 ', t0,
                     '\nset st0 0
set zr 0.5
set d 0
set szr 0
set sv 0
set p 0
format RESPONSE TIME
load .\\data\\',participant,'_D*.dat
log .\\logs\\',participant,'.log'), paste0('./output/exp2b/fast_dm/', participant, '.ctl'))
}


# For a given index of the dataframe extract the participant's id and t0 ang
# generate their ctl
participant_ctl <- function(index, df){
  participant <- df$dataset[index]
  t0 <- df$t0[index]
  D_ctl(participant = participant, t0 = t0)
}


process_participant <- function(cur_condition, cur_data, filename, output_dir) {
  # Use the information stored in the condition list to choose the block
  # and type of cues that will be kept, and recode the responses
  # Then save the file
  cur_data %>%
    filter(block  == cur_condition$selected_block,
           cue %in% cur_condition$selected_cues) %>%
    mutate(response = case_when(response == 0 ~ cur_condition$responses[1],
                                response == 1 ~ cur_condition$responses[2],
                                TRUE ~ cur_condition$responses[3])) %>%
    mutate(group = substring(generate_group_id(filename), 2, 2),
           participant = substring(generate_group_id(filename), 4),
           set = substring(cur_condition$name, 2)) %>%
    select(group, participant, set, block, cue, response, rt)
}

join_participants <- function(filename, output_dir, condition_list){
  # Load and then process and write the files
  print(filename)
  cur_data <- load_datafile(filename)
  map_df(condition_list,
       process_participant,
       cur_data = cur_data, filename = filename, output_dir = output_dir)
  
}