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

FileName = paste0(dirs[["data"]], "Cabauw_SpecDens_09May2008_05000530.txt")
SFCData  = read.table(FileName, na.strings = "NaN", header = TRUE) 

u_sta <- 0.2088 # it is from assignment 3
aver_u <- 2.707 # it is from assignment 3

SFCData <- SFCData %>% mutate (dim_Su = freq*Su/((u_sta^2)*1^(2/3)),
                               dim_Sv = freq*Sv/((u_sta^2)*1^(2/3)),
                               dim_Sw = freq*Sw/((u_sta^2)*1^(2/3)),
                               n = (freq*3/aver_u),
                               n_Su   = 0.3*n^(-2/3),
                               n_Sv   = 0.4*n^(-2/3),
                               n_Sw   = 0.4*n^(-2/3),
                               n_Su_emp = 79*n/ (1+263*n^5/3),
                               n_Sv_emp = 13*n/ (1+32*n^5/3),
                               n_Sw_emp = 3.5*n/ (1+8.6*n^5/3)
                               
                               )

#  the trapezoidal rule to evaluate numerically the integrals: 
#  x1 is frequency, y1 is the spectral density 
trapezoidal <- function(x1,y1) {
  integral = 0
 for (i in 2: length(x1)) {
    tem <- 0.5*(x1[i]-x1[i-1])*(y1[i]+y1[i-1])
    integral <- integral +tem
 }
  return (integral)
}

integral_Su <- trapezoidal(x1 = SFCData[["freq"]], y1 = SFCData[["Su"]])
integral_Sv <- trapezoidal(x1 = SFCData[["freq"]], y1 = SFCData[["Sv"]])
integral_Sw <- trapezoidal(x1 = SFCData[["freq"]], y1 = SFCData[["Sw"]])


#Su
p1  <-  ggplot()+
  geom_point(data = SFCData,aes(x=n_Su,y=dim_Su),color="red")+
  # geom_point(data = SFCData,aes(x=n_Su_emp,y=dim_Su),color="black")+
  scale_x_log10()+
  scale_y_log10()
  # geom_hline(yintercept=mean(SFCData[["u"]]), col = "black",linewidth=1)
p1

p2  <-  ggplot()+
  geom_point(data = SFCData,aes(x=n_Sv,y=dim_Sv),color="red")+
  # geom_point(data = SFCData,aes(x=n_Sv_emp,y=dim_Su),color="black")+
  scale_x_log10()+
  scale_y_log10()
p2

p3  <-  ggplot()+
  geom_point(data = SFCData,aes(x=n_Sw,y=dim_Sw),color="red")+
  # geom_point(data = SFCData,aes(x=n_Sw_emp,y=dim_Su),color="black")+
  scale_x_log10()+
  scale_y_log10()
p3


p4 <- ggplot()+
  geom_point(data = SFCData,aes(x=n,y=Sw/Su),color="red", size =3, alpha =0.5)+
  geom_point(data = SFCData,aes(x=n,y=Sv/Su),color="black",size =3, alpha =0.5)+
  geom_hline(yintercept=4/3, col = "blue",linewidth=1.5)+
  geom_hline(yintercept=4/3*0.7, col = "blue",linewidth=1,linetype ="dashed")+
  geom_hline(yintercept=4/3*1.3, col = "blue",linewidth=1, linetype ="dashed")+
  geom_vline(xintercept=0.3, col = "blue",linewidth=1, linetype ="dashed")+
  annotate("text", x = 0.1, y = 4/3, label = "4/3", color = "blue", angle = 0, vjust = -0.5, size = 10) +
  annotate("text", x = 0.55, y = 0, label = "n=0.3", color = "blue", angle = 0, vjust = -0.5, size = 10) +
  ggtitle("Assignment 5 A")+
  labs(x = expression("n=fz/u"), y= expression("Sw,v/Su"))+
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
    plot.title = element_text(size = 35, face = "bold"),
    axis.title = element_text(size = 30, color = "black",face = "bold"),# x,and y axis title
    axis.text = element_text( size = 30,face = "bold"),
    axis.text.x = element_text(vjust =0.5, size=30,face = "bold"),
    axis.text.y = element_text(vjust =0.5, size=30,face = "bold"),
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






# well only inlcude n_Su > 5 (in a log scale) in IS, let choose n_Su=10
# n=(10/0.3)^(-3/2)
# f_cri <- n*aver_u/3
fs <- SFCData[["freq"]][194] * SFCData[["Su"]][194] 
temp_1 <- fs/0.55/(2*3.14*SFCData[["freq"]][194]/aver_u)^(-2/3)
diss <- temp_1^(3/2)

KTimeScale <- (1.53e-5/diss)^0.5
fk = 1/ KTimeScale






