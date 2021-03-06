library(tidyverse)
library(lubridate)


#repeat{
  
  #enter code here 
ActiveData2=read.csv("thermeter202101291227/OtherMeter202101291227.csv",header= TRUE)
ActiveData2$Timestamp <- as.POSIXct(ActiveData2$Timestamp)
#ActiveData3 <- gather(data = ActiveData2,key= Channel, value = voltage, Channel1:Channel24,factor_key = TRUE)
ActiveData2 <- ActiveData2 %>%  mutate(Inv_V +1/Voltage) 
ActiveData2_subset<- ActiveData2 %>% filter(Channel == c("Channel1", "Channel2","Channel3", "Channel4"))
write.csv(x=ActiveData2_subset,file="thermeter202101291227/OtherMeter202101291227_output.csv", row.names = FALSE)
  
#sys.sleep(60) #wait 60 seconds 
#}


ActiveData2_subset %>% ggplot((aes(x=Timestamp, y=Inv_V, color=Channel)))+geom_point()+ylim(0,5) +facet_wrap(~Channel,scales="free")  
  
#nrows this is to set the number of rows 
#ActiveData2 %>% ggplot((aes(x=Timestamp, y=Inv_V, color=Channel)))+geom_point()+ylim(0,15)
#ActiveData2 %>% ggplot((aes(x=Timestamp, y=Inv_V, color=Channel)))+geom_point()+facut_wrap(~Channel,nrow=5,ncol=5,scales="free")








