## USE THIS SCRIPT TO PROCESS SQUIDSTAT OUTPUT FILES
## SET THE CHRONO AND CV FILE PATHS, THEN RUN THE REST OF THE SCRIPT

## This script will pull all .csv files from the target folders and then clean and process the files 

## 2021-02-05 Kaizad F. Patel, Marci Garcia

######################
######################

# PART 0. SET INPUT FILE PATHS --------------------------------------------
CHRONO_PATH = "data/chrono/Chrono_2021_02_25"
CV_PATH = "data/cv"


# PART 0b. LOAD PACKAGES AND SET UP THE PLOTS -----------------------------
library(tidyverse)
theme_set(theme_bw())

#
# PART 1. CHRONO DATA -----------------------------------------------------
## import files -------------------------------------------------------------
chrono_filePaths <- list.files(path = CHRONO_PATH, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
chrono_data <- 
  do.call(bind_rows, lapply(chrono_filePaths, function(path) {
    # the files are comma-delimited, but in a weird encoding format, so read.csv will not work. 
    # first, import using read.table
    df <- read.table(path, sep = ",", fill = TRUE)
    
    # next, we need to move the column headers up from the first row
    df <- 
      df %>% 
      rownames_to_column() %>%
      `colnames<-`(.[1,]) %>%
      .[-1,] %>%
      `rownames<-`(NULL) %>% as.data.frame(.) 
    
    # then, add a new column `source` to denote the file name
    df[["source"]] <- rep(path, nrow(df))
    
    # finally, clean the file
    # rename and subset the columns needed
    df2 = 
      df %>% 
      rename(step_number = `Step number`,
             elapsed_time_s = `Elapsed Time (s)`,
             current_mA = `Current (mA)`) %>% 
      dplyr::select(step_number, elapsed_time_s, current_mA, source) %>% 
      mutate(instrument = str_extract(source, "Prime[0-9]{4}"),
             channel = str_extract(source, "ch[0-9]"),
             date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>% 
      mutate(elapsed_time_s = as.numeric(elapsed_time_s),
             current_mA = as.numeric(current_mA),
             date = lubridate::ymd(date)) %>% 
      dplyr::select(-source)
    
    df2
  }
  )
  )


# process files -----------------------------------------------------------
## use `names()` to get column names

chrono_data_processed = 
  chrono_data %>% 
  # convert elapsed_time_s into hour
  # rearrange by date and time
  mutate(elapsed_time_hr = elapsed_time_s/3600) %>% 
  group_by(instrument, channel) %>% 
  arrange(date, elapsed_time_s)

#
# plot the graph ----------------------------------------------------------

chrono_data_processed %>% 
  ggplot(aes(x = elapsed_time_hr, y = current_mA))+
  #geom_point()+
  geom_path()+
  labs(x = "elapsed time (hours)",
       y = "current (mA)")+
  facet_grid(instrument~channel)


#

# -------------------------------------------------------------------------
# PART 2. cyclic voltammetry ------------------------------------------------------
## import files -------------------------------------------------------------
cv_filePaths <- list.files(path = CV_PATH, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
cv_data <- 
  do.call(bind_rows, lapply(cv_filePaths, function(path) {
    # the files are comma-delimited, but in a weird encoding format, so read.csv will not work. 
    # first, import using read.table
    df <- read.csv(path, sep = ",", fileEncoding = "latin1", fill = TRUE)
    
    # then, add a new column `source` to denote the file name
    df[["source"]] <- rep(path, nrow(df))
    
    # finally, clean the file
    # rename and subset the columns needed
    df2 = 
      df %>% 
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
      dplyr::select(-source)
    
    df2
    
  }
  )
  )

#
# clean and process the data ----------------------------------------------

cv_data_processed = 
  cv_data %>% 
  filter(current_mA >= -0.3) %>% 
  group_by(instrument, channel) %>% 
  arrange(date, elapsed_time_s)

#
# plot the graph ----------------------------------------------------------

cv_data_processed %>% 
  ggplot(aes(x = working_electrode_V, y = current_mA))+
  #geom_point()+
  geom_path()+
  labs(x = "working electrode (V)",
       y = "current (mA)")+
  xlim(-0.9,0.9)+
  #facet_wrap(~instrument+channel)
  facet_grid(instrument~channel)
