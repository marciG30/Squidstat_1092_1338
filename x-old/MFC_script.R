mfc_data=read.csv("Geo_chrono_cV_12_14_20_kp.csv")
library(ggplot2)

ggplot(mfc_data, aes(x=New_time_hr,y=Current_mA))+
  #geom_point will make points, aes means assign channel colors 
  geom_point(size=0.5,aes(color=as.character(channel)))+
  #labs will assing labels 
  labs(x="Time(Hr)",y="Current(mA)",title = "Geobacter Growth",color="channel")+
  theme_bw()+
  #theme will allow to modify format of graph 
  theme(plot.title=element_text(hjust = 0.5))+ # center the title 
  facet_wrap(~channel) # will create differ pannels 

ggsave("12_14_2020.tiff", height=2.5, width=7.5)# will save the last plot made. #tiff is standard file type for saving images 
