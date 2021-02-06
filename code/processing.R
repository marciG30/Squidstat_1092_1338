
# load packages
library(tidyverse)

# PART 1. CHRONO DATA -----------------------------------------------------
## import files -------------------------------------------------------------
PATH = "data/chrono"
filePaths <- list.files(path = PATH,pattern = "*.csv", recursive = TRUE, full.names = TRUE)

chrono_data = sapply(list.files(path = PATH, pattern = "*.csv", 
                                recursive = TRUE, full.names = TRUE),
                     read.csv,header=TRUE, simplify = FALSE) %>% bind_rows() 



data = read.csv("Chrono_Geo_0.2V_constant_potent_Prime1092_ch1_(2021-01-28_12_07_37)/1_Chrono_Geo_0.2V_constant_potent-Constant_Potential_20210128_120741_v2.csv")



data_cv = read.delim2("data/1_CV_GEO_RT_REF-Cyclic Voltammetry 20210205 134439.txt")



# convert elapsed time s to hr
# current vs time


## use `names()` to get column names


# clean and process the data ----------------------------------------------

data_processed = 
  data %>% 
  mutate(elapsed_time_hr = `Elapsed.Time..s.`/3600) %>% 
  rename(current_mA = `Current..mA.`)


# plot the graph ----------------------------------------------------------
data_processed %>% 
  ggplot(aes(x = elapsed_time_hr, y = current_mA))+
  geom_point()+
  geom_path()



# PART 2. cyclic voltammetry ------------------------------------------------------

data = read.csv("data/cv/CV_GEO_RT_REF Prime1092 ch1 (2021-02-05 13_44_04)/1_CV_GEO_RT_REF-Cyclic Voltammetry 20210205 134439_v2.csv")


# clean and process the data ----------------------------------------------

data_processed = 
  data %>% 
  rename(current_mA = `Current..mA.`,
         working_electrode_V = `Working.Electrode..V.`)


# plot the graph ----------------------------------------------------------
data_processed %>% 
  ggplot(aes(x = working_electrode_V, y = current_mA))+
  #geom_point()+
  geom_path()



