library(tidyverse)
library(lubridate)


ActiveData2=read.csv("thermeter202101291227/OtherMeter202101291227.csv",header= TRUE)

ActiveData2_subset = 
  ActiveData2 %>% 
  filter(Channel == c("Channel1", "Channel2","Channel3", "Channel4")) %>% 
  mutate(Timestamp = as.POSIXct(Timestamp), 
         newcol = Inv_V +1/Voltage) %>% 
  group_by(Channel) %>% 
  mutate(elapsed_hours = difftime(Timestamp, min(Timestamp), units = c("hours")),
         elapsed_hours = as.double(elapsed_hours))

ActiveData2_subset %>% 
  ggplot((aes(x = elapsed_hours, y = Inv_V, color = Channel))) +
  geom_point() +
  ylim(2,6) + 
  facet_wrap(~Channel,scales="free")

ggsave("graph.png")
