install.packages("reshape2")
require (ggplot2)
require (reshape2)
###Ajuste hombres
#ENSA 2000
x1 <-c(1,2,3,4,5,6,7); y1 <-c(0.1041, 0.1547,0.1650,0.1693,0.1717,0.1324,0.0974)
fit1a  <- lm(y1~x1); summary(fit1a)
fit2a<- lm(y1~poly(x1,2,raw=TRUE)); summary(fit2a)
fit3a <- lm(y1~poly(x1,3,raw=TRUE)); summary(fit3a)
fit4a <- lm(y1~poly(x1,4,raw=TRUE)); summary(fit4a)
bb<- data.frame (x1,y1)
ggplot(bb, aes(x1)) + 
  geom_line(aes(y = predict(fit1a), col = "Polinimio grado 1"))+
  geom_line(aes(y = predict(fit2a), col = "Polinimio grado 2")) + 
  geom_line(aes(y = predict(fit3a), col = "Polinomio grado 3"))+
  geom_line(aes(y = predict(fit4a), col = "Polinomio grado 4"))+
  labs(title = "Prevalencia por autorreporte de diabetes en México...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))

#ENSANUT 2006
x2 <-c(1,2,3,4,5,6,7); y2 <-c(0.1116,0.1456,0.1631,0.1741,0.1563,0.1212,0.1035)
bb2<-data.frame(x2,y2)
fit1b  <- lm(y2~x2); summary(fit1b)
fit2b<- lm(y2~poly(x2,2,raw=TRUE)); summary(fit2b)
fit3b <- lm(y2~poly(x2,3,raw=TRUE)); summary(fit3b)
fit4b <- lm(y2~poly(x2,4,raw=TRUE)); summary(fit4b)
ggplot(bb2,aes(x2))+
  geom_line(aes(y=predict(fit1b)),col="blue")+
  geom_line(aes(y=predict(fit2b)),col="red")+
  geom_line(aes(y=predict(fit3b)),col="green")+
  geom_line(aes(y=predict(fit4b)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))

#ENSANUT 2012
x3 <-c(1,2,3,4,5,6,7); y3 <-c(0.1807,0.2026,0.2154,0.2741,0.2356,0.1853,0.1889)
fit1c  <- lm(y3~x3); summary(fit1c)
fit2c <- lm(y3~poly(x3,2,raw=TRUE)); summary(fit2c)
fit3c <- lm(y3~poly(x3,3,raw=TRUE)); summary(fit3c)
fit4c <- lm(y3~poly(x3,4,raw=TRUE)); summary(fit4c)
bb3<-data.frame(x3,y3)
ggplot(bb3,aes(x3))+
  geom_line(aes(y=predict(fit1c)),col="blue")+
  geom_line(aes(y=predict(fit2c)),col="red")+
  geom_line(aes(y=predict(fit3c)),col="green")+
  geom_line(aes(y=predict(fit4c)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#ENSANUT MC 2016
x4 <-c(1,2,3,4,5,6,7); y4 <-c(0.1800,0.1738,0.2773,0.2771,0.2175,0.1511,0.1252)
fit1d  <- lm(y4~x4); summary(fit1d)
fit2d <- lm(y4~poly(x4,2,raw=TRUE)); summary(fit2d)
fit3d <- lm(y4~poly(x4,3,raw=TRUE)); summary(fit3d)
fit4d <- lm(y4~poly(x4,4,raw=TRUE)); summary(fit4d)
bb4<-data.frame(x4,y4)
ggplot(bb4,aes(x4))+
  geom_line(aes(y=predict(fit1d)),col="blue")+
  geom_line(aes(y=predict(fit2d)),col="red")+
  geom_line(aes(y=predict(fit3d)),col="green")+
  geom_line(aes(y=predict(fit4d)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
###Ajuste mujeres
x5 <-c(1,2,3,4,5,6,7); y5 <-c(0.1397,0.1810,0.2033,0.1957,0.2223,0.1951,0.1024)
fit1e  <- lm(y5~x5); summary(fit1e)
fit2e <- lm(y5~poly(x5,2,raw=TRUE)); summary(fit2e)
fit3e <- lm(y5~poly(x5,3,raw=TRUE)); summary(fit3e)
fit4e <- lm(y5~poly(x5,4,raw=TRUE)); summary(fit4e)
bb5<-data.frame(x5,y5)
ggplot(bb5,aes(x5))+
  geom_line(aes(y=predict(fit1e)),col="blue")+
  geom_line(aes(y=predict(fit2e)),col="red")+
  geom_line(aes(y=predict(fit3e)),col="green")+
  geom_line(aes(y=predict(fit4e)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#
x6 <-c(1,2,3,4,5,6,7); y6 <-c(0.1311,0.1583,0.1861,0.2483,0.1962,0.1468,0.1705)
fit1f  <- lm(y6~x6); summary(fit1f)
fit2f <- lm(y6~poly(x6,2,raw=TRUE)); summary(fit2f)
fit3f <- lm(y6~poly(x6,3,raw=TRUE)); summary(fit3f)
fit4f <- lm(y6~poly(x6,4,raw=TRUE)); summary(fit4f)
bb6<-data.frame(x6,y6)
ggplot(bb6,aes(x6))+
  geom_line(aes(y=predict(fit1f)),col="blue")+
  geom_line(aes(y=predict(fit2f)),col="red")+
  geom_line(aes(y=predict(fit3f)),col="green")+
  geom_line(aes(y=predict(fit4f)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#
x7 <-c(1,2,3,4,5,6,7); y7 <-c(0.1565,0.2378,0.2311,0.3057,0.2985,0.2411,0.2079)
fit1g  <- lm(y7~x7); summary(fit1g)
fit2g <- lm(y7~poly(x7,2,raw=TRUE)); summary(fit2g)
fit3g <- lm(y7~poly(x7,3,raw=TRUE)); summary(fit3g)
fit4g <- lm(y7~poly(x7,4,raw=TRUE)); summary(fit4g)
bb7<-data.frame(x7,y7)
ggplot(bb7,aes(x7))+
  geom_line(aes(y=predict(fit1g)),col="blue")+
  geom_line(aes(y=predict(fit2g)),col="red")+
  geom_line(aes(y=predict(fit3g)),col="green")+
  geom_line(aes(y=predict(fit4g)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#
x8 <-c(1,2,3,4,5,6,7); y8 <-c(0.1295,0.2218,0.3626,0.2722,0.3227,0.2504,0.2180)
fit1h  <- lm(y8~x8); summary(fit1h)
fit2h <- lm(y8~poly(x8,2,raw=TRUE)); summary(fit2h)
fit3h <- lm(y8~poly(x8,3,raw=TRUE)); summary(fit3h)
fit4h <- lm(y8~poly(x8,4,raw=TRUE)); summary(fit4h)
bb8<-data.frame(x8,y8)
ggplot(bb8,aes(x8))+
  geom_line(aes(y=predict(fit1h)),col="blue")+
  geom_line(aes(y=predict(fit2h)),col="red")+
  geom_line(aes(y=predict(fit3h)),col="green")+
  geom_line(aes(y=predict(fit4h)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))


##########################################################################################
###Ajuste prevalencias ENASEM
###Ajuste hombres
#ENASEM 2001
x1b <-c(1,2,3,4,5,6,7); y1b <-c(0.2294,0.2122,0.1662,0.1486,0.1010,0.0758,0.0669)
fit1i  <- lm(y1~x1); summary(fit1i)
fit2i <- lm(y1b~poly(x1b,2,raw=TRUE)); summary(fit2i)
fit3i <- lm(y1b~poly(x1b,3,raw=TRUE)); summary(fit3i)
fit4i <- lm(y1b~poly(x1b,4,raw=TRUE)); summary(fit4i)
bb1b<-data.frame(x1b,y1b)
ggplot(bb1b,aes(x1b))+
  geom_line(aes(y=predict(fit1i)),col="blue")+
  geom_line(aes(y=predict(fit2i)),col="red")+
  geom_line(aes(y=predict(fit3i)),col="green")+
  geom_line(aes(y=predict(fit4i)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#ENASEM 2003
x2b <-c(1,2,3,4,5,6,7); y2b <-c(0.1190,0.2198,0.1981,0.1569,0.1330,0.0812,0.0921)
fit1j  <- lm(y2b~x2b); summary(fit1j)
fit2j <- lm(y2b~poly(x2b,2,raw=TRUE)); summary(fit2j)
fit3j <- lm(y2b~poly(x2b,3,raw=TRUE)); summary(fit3j)
fit4j <- lm(y2b~poly(x2b,4,raw=TRUE)); summary(fit4j)
bb2b<-data.frame(x2b,y2b)
ggplot(bb2b,aes(x2b))+
  geom_line(aes(y=predict(fit1j)),col="blue")+
  geom_line(aes(y=predict(fit2j)),col="red")+
  geom_line(aes(y=predict(fit3j)),col="green")+
  geom_line(aes(y=predict(fit4j)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#ENASEM 2012
x3b <-c(1,2,3,4,5,6,7); y3b <-c(0.1905,0.2436,0.1822,0.1421,0.0946,0.0660,0.0810)
fit1k  <- lm(y3b~x3b); summary(fit1k)
fit2k <- lm(y3b~poly(x3b,2,raw=TRUE)); summary(fit2k)
fit3k <- lm(y3b~poly(x3b,3,raw=TRUE)); summary(fit3k)
fit4k <- lm(y3b~poly(x3b,4,raw=TRUE)); summary(fit4k)
bb3b<-data.frame(x3b,y3b)
ggplot(bb3b,aes(x3b))+
  geom_line(aes(y=predict(fit1k)),col="blue")+
  geom_line(aes(y=predict(fit2k)),col="red")+
  geom_line(aes(y=predict(fit3k)),col="green")+
  geom_line(aes(y=predict(fit4k)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#ENASEM 2015
x4b <-c(1,2,3,4,5,6,7); y4b <-c(0.1288,0.2386,0.1685,0.1675,0.1233,0.0792,0.0942)
fit1l  <- lm(y4b~x4b); summary(fit1l)
fit2l <- lm(y4b~poly(x4b,2,raw=TRUE)); summary(fit2l)
fit3l <- lm(y4b~poly(x4b,3,raw=TRUE)); summary(fit3l)
fit4l <- lm(y4b~poly(x4b,4,raw=TRUE)); summary(fit4l)
bb4b<-data.frame(x4b,y4b)
ggplot(bb4b,aes(x4b))+
  geom_line(aes(y=predict(fit1l)),col="blue")+
  geom_line(aes(y=predict(fit2l)),col="red")+
  geom_line(aes(y=predict(fit3l)),col="green")+
  geom_line(aes(y=predict(fit4l)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
###Ajuste mujeres
#ENASEM 2001
x5b <-c(1,2,3,4,5,6,7); y5b <-c(0.2513,0.2172,0.1755,0.1287,0.0867,0.0612,0.0794)
fit1m  <- lm(y5b~x5b); summary(fit1m)
fit2m <- lm(y5b~poly(x5b,2,raw=TRUE)); summary(fit2m)
fit3m <- lm(y5b~poly(x5b,3,raw=TRUE)); summary(fit3m)
fit4m <- lm(y5b~poly(x5b,4,raw=TRUE)); summary(fit4m)
bb5b<-data.frame(x5b,y5b)
ggplot(bb5b,aes(x5b))+
  geom_line(aes(y=predict(fit1m)),col="blue")+
  geom_line(aes(y=predict(fit2m)),col="red")+
  geom_line(aes(y=predict(fit3m)),col="green")+
  geom_line(aes(y=predict(fit4m)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#ENASEM 2003
x6b <-c(1,2,3,4,5,6,7); y6b <-c(0.1397,0.2371,0.1956,0.1478,0.1156,0.0712,0.0929)
fit1n  <- lm(y6b~x6b); summary(fit1n)
fit2n <- lm(y6b~poly(x6b,2,raw=TRUE)); summary(fit2n)
fit3n <- lm(y6b~poly(x6b,3,raw=TRUE)); summary(fit3n)
fit4n <- lm(y6b~poly(x6b,4,raw=TRUE)); summary(fit4n)
bb6b<-data.frame(x6b,y6b)
ggplot(bb6b,aes(x6b))+
  geom_line(aes(y=predict(fit1n)),col="blue")+
  geom_line(aes(y=predict(fit2n)),col="red")+
  geom_line(aes(y=predict(fit3n)),col="green")+
  geom_line(aes(y=predict(fit4n)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#ENASEM 2012
x7b <-c(1,2,3,4,5,6,7); y7b <-c(0.2342,0.2377,0.1718,0.1260,0.0890,0.0656,0.0757)
fit1o  <- lm(y7b~x7b); summary(fit1o)
fit2o <- lm(y7b~poly(x7b,2,raw=TRUE)); summary(fit2o)
fit3o <- lm(y7b~poly(x7b,3,raw=TRUE)); summary(fit3o)
fit4o <- lm(y7b~poly(x7b,4,raw=TRUE)); summary(fit4o)
bb7b<-data.frame(x7b,y7b)
ggplot(bb7b,aes(x7b))+
  geom_line(aes(y=predict(fit1o)),col="blue")+
  geom_line(aes(y=predict(fit2o)),col="red")+
  geom_line(aes(y=predict(fit3o)),col="green")+
  geom_line(aes(y=predict(fit4o)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
#ENASEM 2016
x8b <-c(1,2,3,4,5,6,7); y8b <-c(0.1488,0.2452,0.1890,0.1405,0.1116,0.0752,0.0897)
fit1p  <- lm(y8b~x8b); summary(fit1p)
fit2p <- lm(y8b~poly(x8b,2,raw=TRUE)); summary(fit2p)
fit3p <- lm(y8b~poly(x8b,3,raw=TRUE)); summary(fit3p)
fit4p <- lm(y8b~poly(x8b,4,raw=TRUE)); summary(fit4p)
bb8b<-data.frame(x8b,y8b)
ggplot(bb8b,aes(x8b))+
  geom_line(aes(y=predict(fit1p)),col="blue")+
  geom_line(aes(y=predict(fit2p)),col="red")+
  geom_line(aes(y=predict(fit3p)),col="green")+
  geom_line(aes(y=predict(fit4p)),col="purple")+
  labs(title = "titulo...", caption = "Elaboracion propia...")+
  xlab("Grupos quinquenales")+ylab("Prevalencia")+theme_bw()+theme(plot.caption = element_text(hjust = -.02))
