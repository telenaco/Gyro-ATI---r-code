library(gridExtra)
library(grid)
library(tidyverse)
library(ggplot2)

ati <- read.csv("2021-09-22_19-57-16_atiData.csv")
gyro <- read.csv("2021-09-22_19-57-16_gyroData.csv")

############################################################################
#             REMOVE VALUES OUT OF RANGE
############################################################################

# ati data
{
    # running average to filter/smooth the data from the ati 
    ati$Fx_f <- stats::filter(ati$Fx, rep(1/40,40), sides=2)
    ati$Fy_f <- stats::filter(ati$Fy, rep(1/40,40), sides=2)
    ati$Fz_f <- stats::filter(ati$Fz, rep(1/40,40), sides=2)
    ati$Tx_f <- stats::filter(ati$Tx, rep(1/40,40), sides=2)
    ati$Ty_f <- stats::filter(ati$Ty, rep(1/40,40), sides=2)
    ati$Tz_f <- stats::filter(ati$Tz, rep(1/40,40), sides=2)
    
    ati <- subset(ati,select = -c(X))
    
    # Start with ati data
    loopCount = as.numeric(row.names(ati))
    
    {
      start_time <- Sys.time()
    # convert time from HH:MM:SS.000 to milliseconds
      for (i in loopCount){
        s = ati$Time[i]
        
        hours = as.numeric(str_sub(s, 2, 3))
        minutes = as.numeric(str_sub(s, 5, 6))
        seconds = as.numeric(str_sub(s, 8, 9))
        millis = as.numeric(str_sub(s, 11, 13))
        
        ati$timeElapsed[i] = (((hours*3600)+(minutes*60)+seconds)*1000)+millis
      }
      
    # flag last reading for each ms
      for (i in loopCount){ 
        if(ati$timeElapsed[i] != ati$timeElapsed[i+1])
        {
          ati$newTime[i] = FALSE
        } 
        else if (ati$timeElapsed[i] == ati$timeElapsed[i+1])
        {
          ati$newTime[i] = TRUE
        }
      }
      end_time <- Sys.time()
      print(end_time - start_time)
    }
      
    # keep one reading for ms   
    ati <- subset (ati,ati$newTime == FALSE)
    row.names(ati) <- NULL
}

# gyro data
{
    # keep second data cluster
    gyro <- gyro[c(90000:130000),]
    
    # reset the index order
    row.names(gyro) <- NULL
    
    # real time calculation needs to be scale by 0.5
    gyro$x1 <- gyro$x1/2
    gyro$y1 <- gyro$y1/2
    gyro$z1 <- gyro$z1/2
    gyro$x2 <- gyro$x2/2
    gyro$y2 <- gyro$y2/2
    gyro$z2 <- gyro$z2/2
    
    loopCount = as.numeric(row.names(gyro))
    
    
    for (i in loopCount){
      gyro$degPitch[i]<- gyro$radPitch[i] * (180/pi) 
      gyro$degYaw[i] <- gyro$radYaw[i] * (180/pi)
    }
}

# reset timers

ati <- ati[c(3050:83639),]

for (i in 1:nrow(gyro)){gyro$millisElapsed[i] <- (gyro$millis[i+1]- gyro$millis[i])}
for (i in 2:nrow(gyro)){gyro$millisElapsed[i] <- (gyro$millisElapsed[i] + gyro$millisElapsed[i-1])}
gyro$millisElapsed[1] = 0

for (i in 1:nrow(ati)){ ati$millisElapsed[i] <- (ati$timeElapsed[i+1]- ati$timeElapsed[i])}
for (i in 2:nrow(ati)){ati$millisElapsed[i] <- (ati$millisElapsed[i] + ati$millisElapsed[i-1])}
ati$millisElapsed[1] = 0

ati <- na.omit(ati)
gyro <- na.omit(gyro)

ggplot() +
  geom_line(data=ati, aes( x= millisElapsed ,y=Tx_f), color='blue', lwd =1.1)+
  geom_line(data=gyro, aes( x= millisElapsed ,y=x1), color='red', lwd =0.1)

gyro <- subset(gyro, gyro$millisElapsed < 78050)
ati <- subset(ati, ati$millisElapsed < 78050)


write.csv(gyro,"2021-09-22_19-57-16_gyroData_clean.csv")
write.csv(ati,"2021-09-22_19-57-16_atiData_clean.csv")

########################################################################################
#           THERE A 6 READINGS ON THE DATAFRAME SEPARATE THEM
########################################################################################

gyro <- read.csv("2021-09-22_19-57-16_gyroData_clean.csv")
ati <- read.csv("2021-09-22_19-57-16_atiData_clean.csv")

# ati data
{
    ati$Fx_f <- stats::filter(ati$Fx, rep(1/20,20), sides=2)
    ati$Fy_f <- stats::filter(ati$Fy, rep(1/20,20), sides=2)
    ati$Fz_f <- stats::filter(ati$Fz, rep(1/20,20), sides=2)
    ati$Tx_f <- stats::filter(ati$Tx, rep(1/20,20), sides=2)
    ati$Ty_f <- stats::filter(ati$Ty, rep(1/20,20), sides=2)
    ati$Tz_f <- stats::filter(ati$Tz, rep(1/20,20), sides=2)
    
    ati1 <- ati[c(4700:16000),]
    ggplot() +
      geom_line(data=ati1, aes( x=as.numeric(row.names(ati1)) ,y=Tx), color='red', lwd = 1.2)+
      geom_line(data=ati1, aes( x=as.numeric(row.names(ati1)) ,y=Tx_f), color='yellow', lwd = 0.1)
    
    ati2 <- ati[c(17500:28500),]
    ggplot() +
      geom_line(data=ati2, aes( x=as.numeric(row.names(ati2)) ,y=Tx_f), color='red')
    
    ati3 <- ati[c(32000:43500),]
    ggplot() +
      geom_line(data=ati3, aes( x=as.numeric(row.names(ati3)) ,y=Tx_f), color='red')
    
    ati4 <- ati[c(44500:55000),]
    ggplot() +
      geom_line(data=ati4, aes( x=as.numeric(row.names(ati4)) ,y=Tx_f), color='red')
    
    ati5 <- ati[c(56400:66500),]
    ggplot() +
      geom_line(data=ati5, aes( x=as.numeric(row.names(ati5)) ,y=Tx_f), color='red')
    
    ati6 <- ati[c(67900:78000),]
    ggplot() +
      geom_line(data=ati6, aes( x=as.numeric(row.names(ati6)) ,y=Tx_f), color='red')
    
    row.names(ati1) <- NULL
    row.names(ati2) <- NULL
    row.names(ati3) <- NULL
    row.names(ati4) <- NULL
    row.names(ati5) <- NULL
    row.names(ati6) <- NULL
    
    for (i in 1:nrow(ati1)){ ati1$timeElapsed[i] <- (ati1$timeElapsed[i+1]- ati1$timeElapsed[i])}
    for (i in 2:nrow(ati1)){ati1$timeElapsed[i] <- (ati1$timeElapsed[i] + ati1$timeElapsed[i-1])}
    ati1$timeElapsed[1] = 0
    
    for (i in 1:nrow(ati2)){ ati2$timeElapsed[i] <- (ati2$timeElapsed[i+1]- ati2$timeElapsed[i])}
    for (i in 2:nrow(ati2)){ati2$timeElapsed[i] <- (ati2$timeElapsed[i] + ati2$timeElapsed[i-1])}
    ati2$timeElapsed[1] = 0
    
    for (i in 1:nrow(ati3)){ ati3$timeElapsed[i] <- (ati3$timeElapsed[i+1]- ati3$timeElapsed[i])}
    for (i in 2:nrow(ati3)){ati3$timeElapsed[i] <- (ati3$timeElapsed[i] + ati3$timeElapsed[i-1])}
    ati3$timeElapsed[1] = 0
    
    for (i in 1:nrow(ati4)){ ati4$timeElapsed[i] <- (ati4$timeElapsed[i+1]- ati4$timeElapsed[i])}
    for (i in 2:nrow(ati4)){ati4$timeElapsed[i] <- (ati4$timeElapsed[i] + ati4$timeElapsed[i-1])}
    ati4$timeElapsed[1] = 0
    
    for (i in 1:nrow(ati5)){ ati5$timeElapsed[i] <- (ati5$timeElapsed[i+1]- ati5$timeElapsed[i])}
    for (i in 2:nrow(ati5)){ati5$timeElapsed[i] <- (ati5$timeElapsed[i] + ati5$timeElapsed[i-1])}
    ati5$timeElapsed[1] = 0
    
    for (i in 1:nrow(ati6)){ ati6$timeElapsed[i] <- (ati6$timeElapsed[i+1]- ati6$timeElapsed[i])}
    for (i in 2:nrow(ati6)){ati6$timeElapsed[i] <- (ati6$timeElapsed[i] + ati6$timeElapsed[i-1])}
    ati6$timeElapsed[1] = 0
}

# disk of the model was off by x2 the weight of the disk, adjust it here
{
  gyro$x1 <- gyro$x1/2
  gyro$y1 <- gyro$y1/2
  gyro$z1 <- gyro$z1/2
  gyro$x2 <- gyro$x2/2
  gyro$y2 <- gyro$y2/2
  gyro$z2 <- gyro$z2/2
}  

# gyro data
{
    ggplot() +
      geom_line(data=gyro, aes( x= as.numeric(row.names(gyro)) ,y=x1), color='red')
    
    
    gyro1 <- gyro[c(1000:6500),]
    ggplot() +
      geom_line(data=gyro1, aes( x=as.numeric(row.names(gyro1)) ,y=x1), color='red')
    
    gyro2 <- gyro[c(7500:12600),]
    ggplot() +
      geom_line(data=gyro2, aes( x=as.numeric(row.names(gyro2)) ,y=x1), color='red')
    
    gyro3 <- gyro[c(15000:20500),]
    ggplot() +
      geom_line(data=gyro3, aes( x=as.numeric(row.names(gyro3)) ,y=x1), color='red')
    
    gyro4 <- gyro[c(21300:26100),]
    ggplot() +
      geom_line(data=gyro4, aes( x=as.numeric(row.names(gyro4)) ,y=x1), color='red')
    
    gyro5 <- gyro[c(27150:32100),]
    ggplot() +
      geom_line(data=gyro5, aes( x=as.numeric(row.names(gyro5)) ,y=x1), color='red')
    
    gyro6 <- gyro[c(33180:38250),]
    ggplot() +
      geom_line(data=gyro6, aes( x=as.numeric(row.names(gyro6)) ,y=x1), color='red')
    
    row.names(gyro1) <- NULL
    row.names(gyro2) <- NULL
    row.names(gyro3) <- NULL
    row.names(gyro4) <- NULL
    row.names(gyro5) <- NULL
    row.names(gyro6) <- NULL
    
    
    for (i in 1:nrow(gyro1)){ gyro1$millis[i] <- (gyro1$millis[i+1]- gyro1$millis[i])}
    for (i in 2:nrow(gyro1)){gyro1$millis[i] <- (gyro1$millis[i] + gyro1$millis[i-1])}
    gyro1$millis[1] = 0
    
    for (i in 1:nrow(gyro2)){ gyro2$millis[i] <- (gyro2$millis[i+1]- gyro2$millis[i])}
    for (i in 2:nrow(gyro2)){gyro2$millis[i] <- (gyro2$millis[i] + gyro2$millis[i-1])}
    gyro2$millis[1] = 0
    
    for (i in 1:nrow(gyro3)){ gyro3$millis[i] <- (gyro3$millis[i+1]- gyro3$millis[i])}
    for (i in 2:nrow(gyro3)){gyro3$millis[i] <- (gyro3$millis[i] + gyro3$millis[i-1])}
    gyro3$millis[1] = 0
    
    for (i in 1:nrow(gyro4)){ gyro4$millis[i] <- (gyro4$millis[i+1]- gyro4$millis[i])}
    for (i in 2:nrow(gyro4)){gyro4$millis[i] <- (gyro4$millis[i] + gyro4$millis[i-1])}
    gyro4$millis[1] = 0
    
    for (i in 1:nrow(gyro5)){ gyro5$millis[i] <- (gyro5$millis[i+1]- gyro5$millis[i])}
    for (i in 2:nrow(gyro5)){gyro5$millis[i] <- (gyro5$millis[i] + gyro5$millis[i-1])}
    gyro5$millis[1] = 0
    
    for (i in 1:nrow(gyro6)){ gyro6$millis[i] <- (gyro6$millis[i+1]- gyro6$millis[i])}
    for (i in 2:nrow(gyro6)){gyro6$millis[i] <- (gyro6$millis[i] + gyro6$millis[i-1])}
    gyro6$millis[1] = 0
}

###############################################################################
#           EXTRACT FIRST 720 DEGREES
###############################################################################

# ati data
{
  subAti1 <- ati1[c(0:2700),]
  subAti1 <- na.omit(subAti1)
  ggplot() +
    geom_line(data=subGyro1, aes( x= millis ,y=y1), color='red', lwd = 2)+
    geom_line(data=subGyro1, aes( x= millis ,y=y2), color='yellow', lwd =0.5)
  
  subAti2 <- ati2[c(50:3000),]
  row.names(subAti2) <- NULL
  subAti2 <- na.omit(subAti2)
  ggplot() +
    geom_line(data=subAti2, aes( x= as.numeric(row.names(subAti2)) ,y=Tx), color='blue', lwd =0.1)+
    geom_line(data=subGyro2, aes( x= millis ,y=x1), color='red', lwd =2.2)
  
  subAti3 <- ati3[c(0:2700),]
  subAti3 <- na.omit(subAti3)
  ggplot() +
    geom_line(data=subAti3, aes( x= timeElapsed ,y=Tx_f), color='blue', lwd =0.1)+
    geom_line(data=subGyro3, aes( x= millis ,y=x1), color='red', lwd =2.2)
  
  subAti4 <- ati4[c(0:2500),]
  subAti4 <- na.omit(subAti4)
  ggplot() +
    geom_line(data=subAti4, aes( x= timeElapsed ,y=Tx_f), color='blue', lwd =0.1)+
    geom_line(data=subGyro4, aes( x= millis ,y=x1), color='red', lwd =2.2)
  
  subAti5 <- ati5[c(0:2250),]
  subAti5 <- na.omit(subAti5)
  ggplot() +
    geom_line(data=subAti5, aes( x= timeElapsed ,y=Tx_f), color='blue', lwd =0.1)+
    geom_line(data=subGyro5, aes( x= millis ,y=x1), color='red', lwd =2.2)
  
  subAti6 <- ati6[c(100:2500),]
  row.names(subAti6) <- NULL
  subAti6 <- na.omit(subAti6)
  ggplot() +
    geom_line(data=subAti6, aes( x= as.numeric(row.names(subAti6)) ,y=Tx_f), color='blue', lwd =1)+
    geom_line(data=subGyro6, aes( x= millis ,y=x1), color='red', lwd =1)
}  

# gyro data
{
    subGyro1 <- subset(gyro1, gyro1$degPitch < 720)
    subGyro1 <- na.omit(subGyro1)
    #subGyro1 <- subset(gyro1, gyro1$degYaw < 720)
    
    subGyro2 <- subset(gyro2, gyro2$degPitch < 720)
    #subGyro2 <- subset(gyro2, gyro2$degYaw < 720)
    
    for (i in 1:nrow(subGyro2)){ subGyro2$millis[i] <- (subGyro2$millis[i+1]- subGyro2$millis[i])}
    for (i in 2:nrow(subGyro2)){subGyro2$millis[i] <- (subGyro2$millis[i] + subGyro2$millis[i-1])}
    subGyro2$millis[1] = 0
    subGyro2 <- na.omit(subGyro2)
    
    subGyro3 <- subset(gyro3, gyro3$degPitch < 720)
    #subGyro3 <- subset(gyro3, gyro3$degYaw < 720)
    subGyro3 <- na.omit(subGyro3)
    
    subGyro4 <- subset(gyro4, gyro4$degPitch < 720)
    #subGyro4 <- subset(gyro4, gyro4$degYaw < 720)
    
    for (i in 1:nrow(subGyro4)){ subGyro4$millis[i] <- (subGyro4$millis[i+1]- subGyro4$millis[i])}
    for (i in 2:nrow(subGyro4)){subGyro4$millis[i] <- (subGyro4$millis[i] + subGyro4$millis[i-1])}
    subGyro4$millis[1] = 0
    subGyro4 <- na.omit(subGyro4)
    
    #subGyro5 <- subset(gyro5, gyro5$degPitch < 720)
    subGyro5 <- subset(gyro5, gyro5$degYaw < 720)
    subGyro5 <- na.omit(subGyro5)
    subGyro5$degPitch[1] = 0
    subGyro5$degYaw[1] = 0
    
    #subGyro6 <- subset(gyro6, gyro6$degPitch < 720)
    subGyro6 <- subset(gyro6, gyro6$degYaw < 720)
    
    for (i in 1:nrow(subGyro6)){ subGyro6$millis[i] <- (subGyro6$millis[i+1]- subGyro6$millis[i])}
    for (i in 2:nrow(subGyro6)){subGyro6$millis[i] <- (subGyro6$millis[i] + subGyro6$millis[i-1])}
    subGyro6$millis[1] = 0
    subGyro6 <- na.omit(subGyro6)
}

###############################################################################
#           PLOT DATA FOR BOTH GRAPHS 
###############################################################################


# torque graphs
{
    p0 <- ggplot() +
      geom_line(data=subAti1, aes( x=timeElapsed ,y=Tx_f), color='red')+
      geom_line(data=subGyro1, aes( x=millis ,y=x1), color='black', linetype = "dashed")+
      ggtitle("X Torque")+
      theme(axis.title.x=element_blank(),
            plot.title = element_text(hjust = 0.5))+
      labs(y= "Torque(Nm)")
    
    p1 <- ggplot() +
      geom_line(data=subAti1, aes( x=timeElapsed ,y=Ty_f), color='green')+
      geom_line(data=subGyro1, aes( x=millis ,y=y1), color='black', linetype = "dashed")+
      ggtitle("Y Torque")+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5))+
      labs(x = "Time(ms)")
    
    p2 <- ggplot() +
      geom_line(data=subAti1, aes( x=timeElapsed ,y=Tz_f), color='blue', )+
      geom_line(data=subGyro1, aes( x=millis ,y=z1), color='black',linetype = "dashed")+
      ggtitle("Z Torque")+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5))+
      labs(x = "Time(ms)")
    
    p3 <- ggplot() +
      geom_line(data=subAti3, aes( x=timeElapsed ,y=Tx_f), color='red')+
      geom_line(data=subGyro3, aes( x=millis ,y=x1), color='black',linetype = "dashed")+
      theme(axis.title.x=element_blank())+
      labs(y= "Torque(Nm)")
    
    p4 <- ggplot() +
      geom_line(data=subAti3, aes( x=timeElapsed ,y=Ty_f), color='green')+
      geom_line(data=subGyro3, aes( x=millis ,y=y1), color='black', linetype = "dashed")+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank())

    p5 <- ggplot() +
      geom_line(data=subAti3, aes( x=timeElapsed ,y=Tz_f), color='blue')+
      geom_line(data=subGyro3, aes( x=millis ,y=z1), color='black', linetype = "dashed")+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank())
    
    p6 <- ggplot() +
      geom_line(data=subAti5, aes( x=as.numeric(row.names(subAti5)) ,y=Tx_f), color='red')+
      geom_line(data=subGyro5, aes( x=millis ,y=x1), color='black', linetype = "dashed")+
      labs(x = "Time(ms)", y= "Torque(Nm)")
    
    p7 <- ggplot() +
      geom_line(data=subAti5, aes( x=as.numeric(row.names(subAti5)) ,y=Ty_f), color='green')+
      geom_line(data=subGyro5, aes( x=millis ,y=y1), color='black', linetype = "dashed")+
      theme(axis.title.y=element_blank())+
      labs(x = "Time(ms)")
    
    p8 <- ggplot() +
      geom_line(data=subAti5, aes( x=as.numeric(row.names(subAti5)) ,y=Tz_f), color='blue')+
      geom_line(data=subGyro5, aes( x=millis ,y=z1), color='black', linetype = "dashed")+
      theme(axis.title.y=element_blank())+
      labs(x = "Time(ms)")
}

# vel graphs
{
    vel1 <- ggplot() +
      geom_line(data=subGyro1, aes( x=millis ,y=degYaw), color='blue')+
      geom_line(data=subGyro1, aes( x=millis ,y=degPitch), color='green')+
      ggtitle("Yaw & Pitch rotation")+
      theme(axis.title.x=element_blank(),
            plot.title = element_text(hjust = 0.5))+
      labs( y= "Position (deg)")+
      scale_y_continuous(breaks=seq(0,720,90))
    
    vel2 <-ggplot() +
      geom_line(data=subGyro3, aes( x=millis ,y=degYaw), color='blue')+
      geom_line(data=subGyro3, aes( x=millis ,y=degPitch), color='green')+
      labs( y= "Position (deg)")+
      scale_y_continuous(breaks=seq(0,720,90))+
      theme(axis.title.x=element_blank())
      
    
    vel3 <-ggplot() +
      geom_line(data=subGyro5, aes( x=millis ,y=degYaw), color='blue')+
      geom_line(data=subGyro5, aes( x=millis ,y=degPitch), color='green')+
      labs(x = "Time(ms)", y= "Position (deg)")+
      scale_y_continuous(breaks=seq(0,720,90))
}

emp1 <- ggplot()+ 
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)
        )

# create legends 
plotLeg <- ggplot() +
  geom_line(data=subAti1, aes( x=timeElapsed ,y=Tx_f, colour = 'Ati X'))+
  theme(legend.position = "bottom")+
  scale_colour_manual(name = "Torque", 
                      values = c("ATI X" = "red",
                                 "ATI Y" = "green",
                                 "ATI Z" = "blue",
                                 "Real Time Calculation" = "black"))

plotLegVel <- ggplot() +
  geom_line(data=subGyro1, aes( x=millis ,y=degYaw, colour = 'Yaw'))+
  theme(legend.position = "bottom")+
  scale_colour_manual(name = "Rotation", 
                      values = c("Yaw" = "blue",
                                 "Pitch" = "green"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

plotLeg <- get_legend(plotLeg)
plotLegVel <- get_legend(plotLegVel)

# Draw plots with shared legend
grid.arrange(
  arrangeGrob(emp1,vel1,p0 + ylim(-0.28, 0.28),p1 + ylim(-0.28, 0.28),p2 + ylim(-0.28, 0.28), ncol = 5),
  arrangeGrob(emp1,vel2,p3 + ylim(-0.28, 0.28),p4 + ylim(-0.28, 0.28),p5 + ylim(-0.28, 0.28), ncol = 5),
  arrangeGrob(emp1,vel3,p6 + ylim(-0.28, 0.28),p7 + ylim(-0.28, 0.28),p8 + ylim(-0.28, 0.28), ncol = 5),
  arrangeGrob(emp1, plotLegVel, plotLeg, ncol = 3, widths = c(1,1,3)),
  nrow = 4, 
  heights = c(10, 10, 10, 2))
  




