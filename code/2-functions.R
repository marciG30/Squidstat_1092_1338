## processing FUNCTIONS for SquidStat.
## do not change this code for day-to-day work. 
## make changes only if you want to alter the workflow.

## 2021-03-05 Kaizad F. Patel

# These functions will pull all chrono/cv files together,
# clean and process the data,
# and plot graphs.

# PART 0. LOAD PACKAGES AND SET UP THE PLOTS -----------------------------
library(tidyverse)
theme_set(theme_bw())

#
# PART 1. CHRONO DATA -----------------------------------------------------
process_chrono_files = function(CHRONO_DATA_PATH){
  
  # this function will pull all .csv files from the input folder,
  # and compile them into a single file
  import_chrono_files = function(CHRONO_DATA_PATH){
    chrono_filePaths <- list.files(path = CHRONO_DATA_PATH, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
    
    chrono_data <- 
      do.call(bind_rows, lapply(chrono_filePaths, function(path) {
        # the files are comma-delimited, but in a weird encoding format, 
        # so read.csv needs an encoding argument 
        df <- read.csv(path, sep = ",", fileEncoding = "latin1", fill = TRUE)
        
        # then, add a new column `source` to denote the file name
        df[["source"]] <- rep(path, nrow(df))
        
        df
      }))
  }
  chrono_files = import_chrono_files(CHRONO_DATA_PATH)
  
  # this function will clean the file (rename, filter unnecessary data)
  clean_chrono_files = function(chrono_files){
    temp = 
      chrono_files %>% 
      # rename and subset the columns needed
      rename(step_number = `Step.number`,
             elapsed_time_s = `Elapsed.Time..s.`,
             current_mA = `Current..mA.`) %>% 
      dplyr::select(step_number, elapsed_time_s, current_mA, source) %>% 
      mutate(instrument = str_extract(source, "prime[0-9]{4}"),
             channel = str_extract(source, "Chan_[0-9]"),
             date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>% 
      mutate(elapsed_time_s = as.numeric(elapsed_time_s),
             current_mA = as.numeric(current_mA),
             date = lubridate::ymd(date)) %>% 
      dplyr::select(-source) %>%
      # if a run spanned multiple days, we need to add the maximum time of the first day to the time of the second day
      # to do this, we assign a group number to each date
      # and then add the max per group to the next group
      mutate(newdate = date != c(NA, head(date, -1))) %>% 
      drop_na() %>% 
      mutate(date_group = cumsum(newdate)) %>% 
      dplyr::select(-newdate)
    
    max_time = 
      temp %>% 
      group_by(channel, instrument, date_group) %>% 
      dplyr::summarise(max = max(elapsed_time_s)) %>% 
      mutate(date_group = date_group+1) %>% 
      rename(max_prev = max)
  
    temp %>% 
      left_join(max_time) %>% 
      replace(is.na(.), 0) %>% 
      mutate(elapsed_time_s = elapsed_time_s + max_prev) %>% 
      # convert elapsed_time_s into hour
      # rearrange by date and time
      mutate(elapsed_time_hr = elapsed_time_s/3600) %>% 
      dplyr::select(-max_prev) %>% 
      group_by(instrument, channel) %>% 
      arrange(date, elapsed_time_s)
    
  }
  chrono_processed = clean_chrono_files(chrono_files)
  chrono_processed
}
plot_chrono_graph = function(chrono_processed){    
  chrono_processed %>% 
    mutate(group = if_else((channel %in% c("Chan_1", "Chan_2") & instrument == "prime1092") | 
                             (channel == "Chan_4" & instrument == "prime1338"),
                           "no membrane", "membrane")) %>% 
    ggplot(aes(x = elapsed_time_hr, y = current_mA))+
    #geom_point()+
    geom_path(aes(color = group))+
    labs(x = "elapsed time (hours)",
         y = "current (mA)")+
    facet_grid(instrument~channel)
}

#
# PART 2. cyclic voltammetry ------------------------------------------------------
process_cv_files = function(CV_DATA_PATH){
  import_cv_files = function(CV_DATA_PATH){
    # this function will pull all .csv files from the input folder,
    # and compile them into a single file
    
    cv_filePaths <- list.files(path = CV_DATA_PATH, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
    cv_data <- 
      do.call(bind_rows, lapply(cv_filePaths, function(path) {
        # the files are comma-delimited, but in a weird encoding format, so read.csv will not work. 
        # first, import using read.table
        df <- read.csv(path, sep = ",", fileEncoding = "latin1", fill = TRUE)
        
        # then, add a new column `source` to denote the file name
        df[["source"]] <- rep(path, nrow(df))
        df
      }))
  }
  cv_files = import_cv_files(CV_DATA_PATH)
  
  clean_cv_files = function(cv_files){
    
    
    # finally, clean the file
    # rename and subset the columns needed
    temp = 
      cv_files %>% 
      rename(step_number = `Step.number`,
             elapsed_time_s = `Elapsed.Time..s.`,
             current_mA = `Current..mA.`,
             working_electrode_V = `Working.Electrode..V.`) %>% 
      dplyr::select(step_number, elapsed_time_s, current_mA, working_electrode_V, source) %>% 
      mutate(instrument = str_extract(source, "prime[0-9]{4}"),
             channel = str_extract(source, "Chan_[0-9]"),
             date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>% 
      mutate(elapsed_time_s = as.numeric(elapsed_time_s),
             current_mA = as.numeric(current_mA),
             working_electrode_V = as.numeric(working_electrode_V),
             date = lubridate::ymd(date)) %>% 
      dplyr::select(-source) %>% 
      #filter(current_mA >= -0.3) %>% 
      group_by(instrument, channel) %>% 
      arrange(date, elapsed_time_s) %>% 
      # filter(elapsed_time_s > 120) %>% 
    # if a run spanned multiple days, we need to add the maximum time of the first day to the time of the second day
    # to do this, we assign a group number to each date
    # and then add the max per group to the next group
    mutate(newdate = date != c(NA, head(date, -1))) %>% 
      drop_na() %>% 
      mutate(date_group = cumsum(newdate)) %>% 
      dplyr::select(-newdate)
    
    max_time = 
      temp %>% 
      group_by(channel, instrument, date_group) %>% 
      dplyr::summarise(max = max(elapsed_time_s)) %>% 
      mutate(date_group = date_group+1) %>% 
      rename(max_prev = max)
    
    temp %>% 
      left_join(max_time) %>% 
      replace(is.na(.), 0) %>% 
      mutate(elapsed_time_s = elapsed_time_s + max_prev) %>% 
      dplyr::select(-max_prev) %>% 
      group_by(instrument, channel) %>% 
      arrange(date, elapsed_time_s)
  }
  cv_processed = clean_cv_files(cv_files)
  cv_processed
}
plot_cv_graph = function(cv_processed){
  cv_processed %>% 
  mutate(group = if_else((channel %in% c("Chan_1", "Chan_2") & instrument == "prime1092") | 
                             (channel == "Chan_4" & instrument == "prime1338"),
                           "no membrane", "membrane")) %>% 
  ggplot(aes(x = working_electrode_V, y = current_mA))+
  #geom_point()+
  geom_path()+
  # if you need to color by membrane/no-membrane, use the line below  
  # geom_path(aes(color = group))+
    labs(x = "working electrode (V)",
       y = "current (mA)")+
  xlim(-0.9,0.9)+
  facet_wrap(~instrument+channel)
  #facet_grid(instrument~channel)
}
