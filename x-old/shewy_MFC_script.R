mfc_data=read.csv("chan1_shewy_12_14_20.csv")
library(ggplot2)

ggplot(mfc_data, aes(x=New_time,y=Current_mA))+
  #geom_point will make points, aes means assign channel colors 
  geom_point(size=0.5,aes(color=as.character(Channel)))+
  #labs will assing labels 
  labs(x="Time(Hr)",y="Current(mA)",title = "Shewanella Growth",color="channel")+
  theme_bw()+
  #theme will allow to modify format of graph 
  theme(plot.title=element_text(hjust = .5))+ # center the title 
  # facet_wrap(~channel) # will create differ pannels 
  
  ggsave("shewanella_12_14_20.tiff", height=6, width=7.5)# will save the last plot made. #tiff is standard file type for saving images 

