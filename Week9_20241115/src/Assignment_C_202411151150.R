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

FileName = paste0(dirs[["data"]], "Cabauw_TimeSeries_09May2008_05000530_rotated.txt")
SFCData  = read.table(FileName, na.strings = "NaN", header = TRUE) %>% mutate(Date_time = make_datetime(yyyy, mm, dd, HH,MIN,SEC))
SFCData <-  SFCData %>% mutate(x_index =1: length(SFCData[[1]]))

aver_w <- mean(SFCData[["w"]])
st_w <- sd(SFCData[["w"]])
aver_v <- mean(SFCData[["v"]])
st_v <- sd(SFCData[["v"]])

aver_T <- mean(SFCData[["T"]])
st_T <- sd(SFCData[["T"]])
aver_u <- mean(SFCData[["u"]])
st_u <- sd(SFCData[["u"]])

# sd is the root square of the variance
I_u=st_u/aver_u
I_w=st_w/aver_u
I_v=st_v/aver_u

# we probably still need a density of air
TKE <- 0.5* (st_w^2 + st_u^2 +st_v^2)



u_sta <- ((cov(SFCData[["w"]], SFCData[["u"]], method="pearson"))^2 + (cov(SFCData[["w"]], SFCData[["v"]], method="pearson"))^2)^0.25
HF <- cov(SFCData[["w"]], SFCData[["T"]], method="pearson") # heat flux
L <- -1/0.4*(u_sta^3)/HF*(aver_T/9.81) #  Obukhov length
st_w/u_sta  #evaluate 𝜎w/𝑢∗. 1.3 
st_u/u_sta  #evaluate 𝜎u/𝑢∗ 2.5 
st_v/u_sta  #evaluate 𝜎v/𝑢∗ 1.9 
3/L # z/L
 
#######

#u
p1  <-  ggplot()+
  geom_point(data = SFCData,aes(x=x_index,y=u),color="red", size =3, alpha =0.5)+
  geom_hline(yintercept=mean(SFCData[["u"]]), col = "black",linewidth=1)+ 
  ggtitle("Assignment 3 A")+
  labs(x = "time step", y= expression(u~"(m"~"s"^-1~")"))+
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
    axis.title = element_text(size = 30, color = "black",face = "bold"),# x,and y axis title
    axis.text = element_text( size = 30,face = "bold"),
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

p2  <-  ggplot()+
  geom_point(data = SFCData,aes(x=x_index,y=w),color="red",size =3, alpha =0.5)+
  geom_hline(yintercept=mean(SFCData[["w"]]), col = "black",linewidth=1)+
  ggtitle("Assignment 3 A")+
  labs(x = "time step", y= expression(w~"(m"~"s"^-1~")"))+
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
    axis.title = element_text(size = 30, color = "black",face = "bold"),# x,and y axis title
    axis.text = element_text( size = 30,face = "bold"),
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
p2

p3  <-  ggplot()+
  geom_point(data = SFCData,aes(x=x_index,y=v),color="red",size =3, alpha =0.5)+
  geom_hline(yintercept=mean(SFCData[["v"]]), col = "black",linewidth=1)+
  ggtitle("Assignment 3 A")+
  labs(x = "time step", y= expression(v~"(m"~"s"^-1~")"))+
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
    axis.title = element_text(size = 30, color = "black",face = "bold"),# x,and y axis title
    axis.text = element_text( size = 30,face = "bold"),
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

p4  <-  ggplot()+
  geom_point(data = SFCData,aes(x=x_index,y=SFCData[[10]]),color="red",size =3, alpha =0.5)+
  geom_hline(yintercept=mean(SFCData[["T"]]), col = "black",linewidth=1)+
  ggtitle("Assignment 3 A")+
  labs(x = "time step", y= expression(T~"(K)"))+
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
    axis.title = element_text(size = 30, color = "black",face = "bold"),# x,and y axis title
    axis.text = element_text( size = 30,face = "bold"),
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
p4










z2  = 2
z5  = 5
z10 = 10

