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
                                                                               Mechanic_m_1 = 10/L,
                                                                               X_1 = (1-16*Mechanic_m_1)^(1/4),
                                                                               Hour = floor(time),
                                                                               Minute = 60*(time-Hour))
SFCData <-  SFCData %>% mutate(Date_time = make_datetime(year, month, day, Hour,Minute))
SFCData <-  SFCData %>% mutate(est_usta = U010*0.5/(log(10/0.02)))
SFCData <-  SFCData %>% mutate(model_usta = U010*0.0802-0.0691)
SFCData <-  SFCData %>% mutate(tem = ifelse(Mechanic_m_1>0, -5*Mechanic_m_1, log(((1+X_1)/2)^2) + log(((1+X_1^2)/2)) - 2*atan(X_1) + 3.14/2))
SFCData <-  SFCData %>% mutate(est_usta_1 = U010*0.5/(log(10/0.02)-tem))

#######

lm_fit <- lm(ust005 ~U010 , data=SFCData)
summary(lm_fit)
#u* vs u10
p1  <-  ggplot()+
  geom_point(data = SFCData,aes(x=U010,y=ust005),color="red",size =3, alpha =0.5)+
  # geom_hline(yintercept=0, col = "black",linewidth=1)+
  geom_smooth(data = SFCData,aes(x=U010,y=ust005),method = "lm", se = TRUE)+
  ggtitle("Assignment 2 A")+
  labs(x = expression(bar(u)[10]~"(m"~"s"^-1~")"), y= expression(u["*5"]~"(m"~"s"^-1~")"))+
  annotate("text", x = 2.3, y = 0.45, label = "y=0.0802x-0.0691", color = "black", angle = 0, vjust = -0.5, size = 10) +
  annotate("text", x = 2, y = 0.42, label = "R2=0.87", color = "black", angle = 0, vjust = -0.5, size = 10) +
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



lm_fit_1 <- lm(model_usta ~est_usta , data=SFCData)
summary(lm_fit_1)
p2  <-  ggplot()+
  geom_point(data = SFCData,aes(x=est_usta,y=model_usta),color="red", alpha = 0.5, size = 3 )+
  # geom_hline(yintercept=0, col = "black",linewidth=1)+
  geom_abline(slope = 1,
              intercept = 0,
              color="#136ad5",
              linetype = "dashed",
              linewidth=1)+
  geom_smooth(data = SFCData,aes(x=est_usta,y=model_usta), color = "#000000", method = "lm", se = TRUE)+
  ggtitle("Assignment 2 A (unit:m/s)")+
  coord_fixed(ratio = 1)+
  ylim(0, 0.6)+
  xlim(0, 0.6)+
  # coord_cartesian(expand = FALSE,clip = 'off')+
  labs(x = expression(u["*10"]~"(quick and dirty evaluation)"), y= expression(u["*5"]~"(model)"))+
  annotate("text", x = 0.15, y = 0.55, label = "y=0.997x-0.069", color = "black", angle = 0, vjust = -0.5, size = 10) +
  annotate("text", x = 0.1, y = 0.515, label = "R2=0.97", color = "black", angle = 0, vjust = -0.5, size = 10) +
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
p2

#######


lm_fit_1 <- lm(ust005 ~est_usta_1 , data=SFCData)
summary(lm_fit_1)
p3  <-  ggplot()+
  geom_point(data = SFCData,aes(x=est_usta_1,y=ust005),color="red")+
  # geom_hline(yintercept=0, col = "black",linewidth=1)+
  geom_smooth(data = SFCData,aes(x=est_usta_1,y=ust005), color = "#000000", method = "lm", se = TRUE)+
  geom_abline(slope = 1,
              intercept = 0,
              color="#136ad5",
              linetype = "dashed",
              linewidth=1)+
  ggtitle("Assignment 2 B (unit:m/s)")+
  coord_fixed(ratio = 1)+
  ylim(0, 0.6)+
  xlim(0, 0.6)+
  # coord_cartesian(expand = FALSE,clip = 'off')+
  labs(x = expression(u["*10"]~"(diabatic wind profile)"), y= expression(u["*5"]~"(measured)"))+
  annotate("text", x = 0.15, y = 0.55, label = "y=0.604x-0.067", color = "black", angle = 0, vjust = -0.5, size = 10) +
  annotate("text", x = 0.1, y = 0.515, label = "R2=0.92", color = "black", angle = 0, vjust = -0.5, size = 10) +
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

#######
#L vs hour





z2  = 2
z5  = 5
z10 = 10

