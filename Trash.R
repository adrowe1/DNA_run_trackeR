ggdet <- 0
ggfil <- 0
plotdet <- ggdet
plotfil <- ""
class(plotfil)
color <- c('blue', 'red', 'green', 'yellow')
diadetxy <- ggplot() + theme_bw() + coord_equal(ratio=1)+ coord_cartesian(xlim=c(1000,8000), ylim = c(500,3000))
diadetfx <- ggplot(data = RawDataMat, aes(x=RawDataMat$f))
for(i in 1:col)
{
  ggdet <- geom_point(data = DetDataMat, aes_string(x=paste("x",i, sep = ""), y = paste("y",i, sep = "")), color = color[i])
  ggfil <- geom_point(data = FilDataMat, aes_string(x=paste("x",i, sep = ""), y = paste("y",i, sep = "")), color = color[i], alpha=0.3)
  diadetxy <- diadetxy + ggdet + ggfil
  
  gfdet <- geom_line(data = DetDataMat, aes_string(y= paste("x",i, sep = "")), color = color[i]) 
  gffil <- geom_point(data = FilDataMat, aes_string(y= paste("x",i, sep = "")), color = color[i], alpha=0.3) 
  diadetfx <- diadetfx + gfdet + gffil
}

ggplotly(diadetxy)
ggplotly(diadetfx)

bb <- 0
bb <- geom_point(data = DetDataMat, aes(x = x1, y = y1, color = 'Raw1'), color = 'blue')
geom_point(data = DetDataMat, aes(x = x2, y = y2, color = 'Raw1'), color = 'red')+ 
  geom_point(data = FilDataMat, aes(x = x1, y = y1, color = 'Fil1', alpha=0.3), color = 'blue')+
  geom_point(data = FilDataMat, aes(x = x2, y = y2, color = 'Fil2', alpha=0.3), color = 'red')

geom_point(aes(x=detx3, y=dety3, color='det3'), color='green')+
  geom_point(aes(x=felx1, y=fely1), color='gray' , alpha=0.3) + 
  geom_point(aes(x=felx2, y=fely2), color='gray' , alpha=0.3) +
  geom_point(aes(x=felx3, y=fely3), color='gray' , alpha=0.3) +
  theme_bw() + coord_equal(ratio=1)+ coord_cartesian(xlim=c(1000,8000), ylim = c(500,3000))
ggplotly(dia1)

dia2 <- detect%>%
  ggplot(aes(FrameNa)) + geom_line(aes(y=detx1, color='det1'), color='blue')+
  geom_line(aes(y=detx2, color='det2'), color='red')+
  geom_line(aes(y=detx3, color='det3'), color='green')+  
  geom_point(aes(y=felx1, color='fel1'), color='blue', alpha=0.3)+
  geom_point(aes(y=felx2, color='fel2'), color='red', alpha=0.3)+ 
  geom_point(aes(y=felx3, color='fel3'), color='green', alpha=0.3)

ggplotly(dia2)



# plots can be tweaked and become more practical 

# signal distribution plots can be modified such that it shows the process of filteration and detection 

filter2%>%
  ggplot(aes(x=felx, y=fely, colour=felf)) + geom_point(alpha=0.5) + theme_bw() + coord_equal(ratio=1)+ coord_cartesian(ylim = c(1500,2000))

detect%>%
  ggplot(aes(x=detx, y=dety, colour=detf)) + geom_point(alpha=0.5) + theme_bw() + coord_equal(ratio=1)+ coord_cartesian(ylim = c(1500,2000))


# detected trajectories:  
# showing the detected trajectory among the whole data can be interesting 
# a zoomable plot can make it easy to find the scanning reactions 
selection%>%
  ggplot(aes(x=rawf, y=rawx)) + geom_point(alpha=1)

filter2%>%
  ggplot(aes(x=felf, y=felx)) + geom_point(alpha=1)
#  coord_cartesian(xlim = c(16000,16250))
# we need to be able to see the changes of y in parallel to x in terms of frame 



coord_cartesian(xlim = c(20250,20300))      ## for zooming 

# i need to do the frame number extraction 


#geom_point(aes( y=felx, alpha=0.5))
#geom_point(aes( y=rawx, alpha=0.1))

rm(list = ls())



test <- c(1,2,3,5,6)
if (test>3){
  print("es")
}




# creation of a matrix from rawdata, this matrix can be used for visualization of the raw data parallel to the filtered and detected data 
row = max(SourceData$frame) # max number of frame 
SignalPerFrame <- 0
for(i in 1:row) # this loop is for extractoin of number of signals in one frame which later will be the row numbers  
{  
  SignalPerFrame[i] <- length(SourceData$frame[SourceData$frame==i])
}


col = max(SignalPerFrame) # max number of signal per frame  
RawDataMatx <- matrix(0,row,col)
RawDataMaty <- matrix(0,row,col)
RawDataMatf <- matrix(0,row,1)
k = 0
p = 0

for(i in 1:row) 
{
  RawDataMatf[i,1] <- i
  
  l <- length(SourceData$frame[SourceData$frame==i])
  if (l==0)
  {
    RawDataMatx[i,] <- 0
    RawDataMaty[i,] <- 0
    k <- k+1
  }
  else 
  {
    for(j in 1:l)
    {
      RawDataMatx[i,j] <- SourceData$`x [nm]`[i-k+p+j-1]
      RawDataMaty[i,j] <- SourceData$`y [nm]`[i-k+p+j-1]
      
    }
    p <- p+l-1
  }
}

RawDataMat <- data.frame(RawDataMatx, RawDataMaty)
#names(RawDataMat)[1] <- paste("f")
for (i in 1:col)
{
  names(RawDataMat)[i]<-paste("x",i, sep = "")
  names(RawDataMat)[col+i]<-paste("y",i, sep = "")
}
RawDataMat[RawDataMat==0] <- NA


# this is for the extraction of the raw data from the data base,  

# rawi <- SourceData$id
# rawf <- SourceData$frame
# rawx <- SourceData$`x [nm]`
# rawy <- SourceData$`y [nm]`
# 
# RawData <- data.frame(rawi, rawf, rawx, rawy) # this dataframe is used for filtering and the creation of rearrangement matrix 
# rm(rawf, rawi, rawx, rawy) 






attach(filter1)
# second filtering and rearrangement the data based on frame numbers and matrix construction  
SignalPerFrame <- 0
for(i in 1:row) # this loop is for extractoin of number of signals in one frame which later will be the row numbers  
{  
  SignalPerFrame[i] <- length(frame[frame==i])
}

#row = max(SourceData$frame) # max number of frame, i want to keep it as the max frame number of raw data  
col = max(SignalPerFrame) # max number of signal per frame  
FilDataMatx <- matrix(0,row,col)
FilDataMaty <- matrix(0,row,col)
k=0   # i have to introduce the initial amounts as 0 otherwise i'll see errors 
p=0


for(i in 1:(row)) 
{
  l <- length(frame[frame==i])
  if (l==0)
  {
    FilDataMatx[i,1] <- 0
    FilDataMaty[i,1] <- 0
    k <- k+1
  }
  else 
  {
    for(j in 1:l)
    {
      FilDataMatx[i,j] <- `x [nm]`[i-k+p+j-1]
      FilDataMaty[i,j] <- `y [nm]`[i-k+p+j-1]
      
    }
    p <- p+l-1
  }
}

FilDataMat <- data.frame(FilDataMatx, FilDataMaty)

for (i in 1:col)
{
  names(FilDataMat)[i]<-paste("x",i, sep = "")
  names(FilDataMat)[col+i]<-paste("y",i, sep = "")
}
FilDataMat[FilDataMat==0] <- NA









plotItAll <- function(DetDataMat, FilDataMat){
  longD <- makeLongForm(DetDataMat)
  longF <- makeLongForm(FilDataMat)
  
  longD %>% ggplot(aes(x=t, y=x, colour=z)) + geom_line() + geom_point(data=dplyr::anti_join(longF, longD, by=c("x", "y")))
}




































plotItAll(DetDataMat, FilDataMat)



