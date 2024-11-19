library(readxl)
library(bigleaf)
library(devtools)
library(tidyverse)
library(lubridate)
library(dplyr)
library(quantreg) # for quantile regression
library(deming)
library(lmodel2)
library(zoo)
library(ggplot2)


dirs <- c("project" = "D:\\Week9_20241115/",
          "data" = "D:\\Week9_20241115/data/",
          "processed" = "D:\\Week9_20241115/processed/",
          "results" = "D:\\Week9_20241115/results/",
          "test" = "D:\\Week9_20241115/test/")

## defined const
g  = 9.81 # acceleration due to gravity (m s-2)
vK = 0.4  # von Karman constant

FileName = paste0(dirs[["data"]], "SurfaceData_Cabauw_05-10-May-2008.txt")
SFCData  = read.table(FileName, na.strings = "NaN", header = TRUE) %>% mutate (Ta_K =Ta002+273.15,
                                                                               L = -1/vK*(ust005^3)/wT005*(Ta_K/g),
                                                                               Mechanic_mixing =abs(5/L),
                                                                               Hour = floor(time),
                                                                               Minute = 60*(time-Hour))
SFCData <-  SFCData %>% mutate(Date_time = make_datetime(year, month, day, Hour,Minute))

#######
#L vs hour
p1  <-  ggplot()+
  geom_point(data = SFCData,aes(x=time,y=L),color="red",size =3, alpha =0.5)+
  ylim(-150, 200)+
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 19, 24))+
  geom_hline(yintercept=0, col = "black",linewidth=1,linetype = "dashed")+
  geom_vline(xintercept=4, col = "black",linewidth=0.7,linetype = "dashed")+
  geom_vline(xintercept=19, col = "black",linewidth=0.7, linetype = "dashed")+
  annotate("text", x = 4, y = 190, label = "Sunrise", color = "blue", angle = 90, vjust = -0.5, size = 5) +
  annotate("text", x = 19, y = 190, label = "Sunset", color = "blue", angle = 90, vjust = -0.5,size = 5)+
  ggtitle("Assignment 1 A")+
  labs(x = "Hour", y= "L")+
  theme(
    panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
    panel.grid.major.y = element_line(color = rgb(0.5, 0.5, 0.5, alpha = 0.5),linetype = "dashed",linewidth =  0.2),  # Set horizontal major grid lines to grey
    panel.grid.minor = element_blank(),
    # plot.title = element_text(size = 20, face =  "bold",margin = margin(10, 0, 10, 0)),# figure title
    plot.title.position = "plot",
    plot.caption.position = 'plot',
    # axis.title.y = element_blank(),
    # plot.caption= element_text(size=20),
    panel.background = element_rect(colour = "chartreuse4", size=1.1, fill=NA),# add a box to the panel
    axis.ticks=element_line(size=0.4), # change ticks width
    axis.ticks.length=unit(-0.1, "cm"),# change ticks length
    plot.margin = margin(10, 15, 0, 15),
    plot.title = element_text(size = 25, face = "bold"),
    axis.title = element_text(size = 20, color = "black",face = "bold"),# x,and y axis title
    axis.text = element_text( size = 20,face = "bold"),
    axis.text.x = element_text(vjust =0.5, size=20,face = "bold"),
    axis.text.y = element_text(vjust =0.5, size=20,face = "bold"),
    # strip.text = element_text(size = 10, face = "bold"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    legend.title=element_text(size=5,face = "bold"),
    # legend.title = element_blank(),
    legend.text = element_text(size=5),
    legend.position='bottom',
    legend.box.margin = margin(0, 0, 0, 0),  # Adjust margin around the legend box
    legend.spacing.x = unit(0.1, 'cm'),  # Adjust spacing between legend items horizontally
    legend.spacing.y = unit(0.1, 'cm')
    # legend.box.just = "center" #change legend key height
  ) + 
  guides(
    color = guide_colorbar(  # Adjust the legend for the 'color' aesthetic
      keyheight = unit(0.4, 'cm'),  # Change legend key height for 'Variable Values'
      keywidth = unit(2, 'cm'), # Change legend key width for 'Variable Values'
      title.position = "top",  # Move the legend title to the top
      title.hjust = 0.5  # Center the legend title over the color bar
    ),
    fill = guide_legend(  # Use guide_legend for discrete fill colors
      keyheight = unit(0.4, 'cm'),  # Change the height of the legend key for 'Farbe'
      keywidth = unit(0.4, 'cm'),   # Change the width of the legend key for 'Farbe'
      title.position = "top",  # Position the title above the keys
      title.hjust = 0.5  # Center the legend title horizontally
    )
  )
  
  
p1

#######
#heat flux vs hour
p2  <-  ggplot()+
  geom_point(data = SFCData,aes(x=time,y=wT005),color="red", size =3, alpha =0.5)+
  # ylim(-150, 200)+
  geom_hline(yintercept=0, col = "black",linewidth=1)+
  geom_vline(xintercept=4, col = "blue",linewidth=1)+
  geom_vline(xintercept=19, col = "blue",linewidth=1)

p2


#######
#L vs hour
p3  <-  ggplot()+
  geom_point(data = SFCData,aes(x=U010, y=Mechanic_mixing),color="red", size =3, alpha =0.5)+
  geom_hline(yintercept=0.2, col = "black",linewidth=0.7,linetype = "dashed")+
  geom_vline(xintercept=2.5, col = "black",linewidth=0.7,linetype = "dashed")+
  geom_vline(xintercept=4.3, col = "black",linewidth=0.7,linetype = "dashed")+
  annotate("text", x = 0, y = 0.2, label = "0.2", color = "blue", angle = 0, vjust = -0.5, size = 10) +
  annotate("text", x = 2.3, y = 0, label = "2.5", color = "blue", angle = 0, vjust = -0.5, size = 10) +
  annotate("text", x = 4.1, y = 0, label = "4.3", color = "blue", angle = 0, vjust = -0.5, size = 10) +
  scale_y_log10()+
  ggtitle("Assignment 1 C")+
  labs(x = expression(bar(u)[10]~"(m"~"s"^-1~")"), y= "|z/L|")+
  theme(
    panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
    panel.grid.major.y = element_line(color = rgb(0.5, 0.5, 0.5, alpha = 0.5),linetype = "dashed",linewidth =  0.2),  # Set horizontal major grid lines to grey
    panel.grid.minor = element_blank(),
    # plot.title = element_text(size = 20, face =  "bold",margin = margin(10, 0, 10, 0)),# figure title
    plot.title.position = "plot",
    plot.caption.position = 'plot',
    # axis.title.y = element_blank(),
    # plot.caption= element_text(size=20),
    panel.background = element_rect(colour = "chartreuse4", size=1.1, fill=NA),# add a box to the panel
    axis.ticks=element_line(size=0.4), # change ticks width
    axis.ticks.length=unit(-0.1, "cm"),# change ticks length
    plot.margin = margin(10, 15, 0, 15),
    plot.title = element_text(size = 25, face = "bold"),
    axis.title = element_text(size = 20, color = "black",face = "bold"),# x,and y axis title
    axis.text = element_text( size = 20,face = "bold"),
    axis.text.x = element_text(vjust =0.5, size=20,face = "bold"),
    axis.text.y = element_text(vjust =0.5, size=20,face = "bold"),
    # strip.text = element_text(size = 10, face = "bold"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    legend.title=element_text(size=5,face = "bold"),
    # legend.title = element_blank(),
    legend.text = element_text(size=5),
    legend.position='bottom',
    legend.box.margin = margin(0, 0, 0, 0),  # Adjust margin around the legend box
    legend.spacing.x = unit(0.1, 'cm'),  # Adjust spacing between legend items horizontally
    legend.spacing.y = unit(0.1, 'cm')
    # legend.box.just = "center" #change legend key height
  ) + 
  guides(
    color = guide_colorbar(  # Adjust the legend for the 'color' aesthetic
      keyheight = unit(0.4, 'cm'),  # Change legend key height for 'Variable Values'
      keywidth = unit(2, 'cm'), # Change legend key width for 'Variable Values'
      title.position = "top",  # Move the legend title to the top
      title.hjust = 0.5  # Center the legend title over the color bar
    ),
    fill = guide_legend(  # Use guide_legend for discrete fill colors
      keyheight = unit(0.4, 'cm'),  # Change the height of the legend key for 'Farbe'
      keywidth = unit(0.4, 'cm'),   # Change the width of the legend key for 'Farbe'
      title.position = "top",  # Position the title above the keys
      title.hjust = 0.5  # Center the legend title horizontally
    )
  )

p3




z2  = 2
z5  = 5
z10 = 10

