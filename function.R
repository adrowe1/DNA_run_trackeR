
MakeItMatrix <- function(RawData,InputData,DataMat)
{
# creation of a matrix from rawdata, this matrix can be used for visualization of the raw data parallel to the filtered and detected data 
row = max(RawData$frame) # max number of frame 
SignalPerFrame <- 0
for(i in 1:row) # this loop is for extractoin of number of signals in one frame which later will be the row numbers  
{  
  SignalPerFrame[i] <- length(InputData$frame[InputData$frame==i])
}


col = max(SignalPerFrame) # max number of signal per frame  
DataMatx <- matrix(0,row,col)
DataMaty <- matrix(0,row,col)
DataMatf <- matrix(0,row,1)
k = 0
p = 0

for(i in 1:row) 
{
  DataMatf[i,1] <- i
  
  l <- length(InputData$frame[InputData$frame==i])
  if (l==0)
  {
    DataMatx[i,] <- 0
    DataMaty[i,] <- 0
    k <- k+1
  }
  else 
  {
    for(j in 1:l)
    {
      DataMatx[i,j] <- InputData$`x [nm]`[i-k+p+j-1]
      DataMaty[i,j] <- InputData$`y [nm]`[i-k+p+j-1]
      
    }
    p <- p+l-1
  }
}
DataMat <- data.frame(DataMatx, DataMaty)
for (i in 1:col)
  {
    names(DataMat)[i]<-paste("x",i, sep = "")
    names(DataMat)[col+i]<-paste("y",i, sep = "")
  }
return(DataMat)
}
###########################################


MakeZeroNA <- function(InputDataSet)
{
  InputDataSet[InputDataSet==0] <- NA
  return(InputDataSet)
}

###########################################

FindTrajectory <- function(FilteredDataSet, dxmax, dxmin, dymax)
{
  
  
  row <- nrow(FilteredDataSet)
  col <- ncol(FilteredDataSet)/2
  # detection loop 
  for(i in 1:(row-1)) 
  {
    for(j in 1:(col))
    {
      if (FilteredDataSet[i,j]!=0)
      {
        deltax<- abs(FilteredDataSet[i,j]-FilteredDataSet[i+1,1:col])   
        deltay<- abs(FilteredDataSet[i,(j+col)]-FilteredDataSet[i+1,(col+1):(2*col)])   
        min <- which.min(deltax)
        if(deltax[min]<dxmax & deltax[min]>dxmin & deltay[min]<dymax)
        {
          if (abs(FilteredDataSet[i+1,min]-FilteredDataSet[i,j]) == min(abs(FilteredDataSet[i,1:col]-FilteredDataSet[i+1,min])))
          {
            ax <- FilteredDataSet[i+1,j]
            FilteredDataSet[i+1,j] <- FilteredDataSet[i+1,min]  
            FilteredDataSet[i+1,min] <- ax
            ay <- FilteredDataSet[i+1,(j+col)]
            FilteredDataSet[i+1,(j+col)] <- FilteredDataSet[i+1,(min+col)]  
            FilteredDataSet[i+1,(min+col)] <- ay
          } 
          else 
          {
            print("it didn't move")
            print(i)
            print(j)
          }
        } 
        else if (i!=1)
        {
          if (FilteredDataSet[i-1,j]!=0)
          {
            if (abs(FilteredDataSet[i,j]-FilteredDataSet[i-1,j])<dxmax & abs(FilteredDataSet[i,j]-FilteredDataSet[i-1,j])> dxmin & abs(FilteredDataSet[i,(j+col)]-FilteredDataSet[i-1,(j+col)])<dymax)
            { 
              FilteredDataSet[i+1,j] <- 0
              FilteredDataSet[i+1,(j+col)] <- 0
              
              print("last point detected")
              print(i)
              print(j)
            }
            else 
            {
              FilteredDataSet[i,j] <- 0
              FilteredDataSet[i,(j+col)] <- 0
            }
            
            
          }
          
          else 
          {
            FilteredDataSet[i,j] <- 0
            FilteredDataSet[i,(j+col)] <- 0
          }
        }
        else 
        {
          FilteredDataSet[i,j] <- 0
          FilteredDataSet[i,(j+col)] <- 0
        }
      }
      else 
      {  
        FilteredDataSet[i,j] <- 0
        FilteredDataSet[i,(j+col)] <- 0
      }
    }
  }
  return(FilteredDataSet)
  
}
  
 ######################################################################################



MakeLongForm <- function(InputMatrix){
  LongForm <- data_frame(t=rep(1:(nrow(InputMatrix)),(ncol(InputMatrix)/2)),
                        x=InputMatrix[,1:(ncol(InputMatrix)/2)] %>% unlist() %>% as.vector(), 
                        y=InputMatrix[,(ncol(InputMatrix)/2+1):ncol(InputMatrix)] %>% unlist() %>% as.vector(), 
                        z=rep(LETTERS[1:(ncol(InputMatrix)/2)], each=nrow(InputMatrix)))
  return (LongForm)
  
  }


#######################################################################################

PlotFX1 <- function(InputLongForm)
{
  ReadyPlot <- InputLongForm %>% ggplot(aes(x=t, y=x, colour=z)) + geom_line()
}

#######################################################################################

PlotFX2 <- function(InputLongForm1,InputLongForm2)
{
  ReadyPlot <- InputLongForm1 %>% ggplot(aes(x=t, y=x, colour=z)) + geom_line()+  
    geom_point(data=dplyr::semi_join(InputLongForm2, InputLongForm1, by=c("x","y")), alpha=0.5)
  #geom_point(data=dplyr::anti_join(InputRaw, InputFil, by=c("x","y")), alpha=0.3)
}

#######################################################################################

PlotFX3 <- function(InputLongForm1,InputLongForm2, InputLongForm3)
{
    ReadyPlot <- InputLongForm1 %>% ggplot(aes(x=t, y=x, colour=z)) + geom_line()+  
    geom_point(data=dplyr::semi_join(InputLongForm2, InputLongForm1, by=c("x","y")), alpha=0.5)+
    geom_point(data=dplyr::anti_join(InputLongForm3, InputLongForm2, by=c("x","y")), color='gray', alpha=0.3)
}

#######################################################################################
PlotXY1 <- function(InputLongForm1)
{
  ReadyPlot <- InputLongForm1 %>% ggplot(aes(x=x, y=y, colour=z)) + geom_point()+  
  theme_bw() +  coord_cartesian(xlim=c(1000,8000), ylim = c(500,3000))+ coord_equal(ratio=0.1)

}

#######################################################################################
PlotXY2 <- function(InputLongForm1,InputLongForm2)
{
  ReadyPlot <- InputLongForm1 %>% ggplot(aes(x=x, y=y, colour=z)) + geom_point()+
  geom_point(data=dplyr::semi_join(InputLongForm2, InputLongForm1, by=c("x","y")), alpha=0.5)+
  theme_bw() +  coord_cartesian(xlim=c(1000,8000), ylim = c(500,3000))+ coord_equal(ratio=0.1)
}

#######################################################################################
PlotXY3 <- function(InputLongForm1,InputLongForm2, InputLongForm3)
{
  ReadyPlot <- InputLongForm1 %>% ggplot(aes(x=x, y=y, colour=z)) + geom_point()+
    geom_point(data=dplyr::anti_join(InputLongForm2, InputLongForm1, by=c("x","y")), alpha=0.5)+
    geom_point(data=dplyr::anti_join(InputLongForm3, InputLongForm2, by=c("x","y")), color='gray',alpha=0.3)+
    theme_bw() +  coord_cartesian(xlim=c(1000,8000), ylim = c(500,3000))+ coord_equal(ratio=0.1)
}

#######################################################################################


ExtractTrajectory <- function(DetectedLongForm)
{
  k=1
  n=1
  m=1
  f=0
  t=0
  x=0
  y=0
  z=0
  StartId <- 0
  EndId <- 0
  StartFrame <- 0
  EndFrame <- 0
  for(i in 1:(nrow(DetectedLongForm)))
  {
    if (!is.na(DetectedLongForm$x[i])) 
    {
      f[k] <- k
      t[k] <- DetectedLongForm$t[i]
      x[k] <- DetectedLongForm$x[i]
      y[k] <- DetectedLongForm$y[i]
      z[k] <- DetectedLongForm$z[i]
      
      
      k <- k+1
    }
    else if (i!=nrow(DetectedLongForm) & is.na(DetectedLongForm$x[i])) 
    {
      if (!is.na(DetectedLongForm$x[i+1]))
      {
        f[k] <- k
        t[k] <- DetectedLongForm$t[i]
        x[k] <- DetectedLongForm$x[i]
        y[k] <- DetectedLongForm$y[i]
        z[k] <- DetectedLongForm$z[i]
        
        StartFrame[m] <- DetectedLongForm$t[i]
        StartId[m] <- k
        m <- m+1
        k <- k+1
      }
      if (i!=1) 
      {
        if (!is.na(DetectedLongForm$x[i-1])) 
        {
          f[k] <- k
          t[k] <- DetectedLongForm$t[i]
          x[k] <- DetectedLongForm$x[i]
          y[k] <- DetectedLongForm$y[i]
          z[k] <- DetectedLongForm$z[i]
          
          EndFrame[n] <- DetectedLongForm$t[i]
          EndId[n] <- k
          n <- n+1
          if (is.na(StartFrame[1]))
          {
            StartFrame[1] <- 1   
            m <- 2
          }
          k <- k+1
        }
      }
      else 
      {
        print("do nothing") 
      }
    }
  }

  
  EndFrame[EndFrame==1] <- max(DetectedLongForm$t)
  FrameCut <- data.frame(StartFrame, EndFrame)
  ExtractDet <- data.frame(f,t,x,y,z)
  
  
  
  k=1
  ToMacroStart <- ""
  ToMacroEnd <- ""
  TrajNumber <- ""
  Traj=0
  Extract=NA
  
  for (i in 1:nrow(FrameCut))
  {
    if(FrameCut$EndFrame[i]-FrameCut$StartFrame[i] > 5)
    { 
      Traj <- subset(ExtractDet, f >= StartId[i] & f <= EndId[i])
      s <- as.factor(rep(k, nrow(Traj)))
      Traj <- cbind(Traj,(s))
      Extract <- rbind(Extract,Traj)
      
      TrajNumber <- paste(TrajNumber,",", k )
      ToMacroStart <- paste(ToMacroStart,",",StartFrame[i], sep = "")
      ToMacroEnd <- paste(ToMacroEnd,",",EndFrame[i], sep = "")
      k <- k+1
    }
  }
  print(TrajNumber)
  print(ToMacroStart)
  print(ToMacroEnd)
  
  return(list(data.frame(Extract),data.frame(FrameCut)))
  
}

#############################################################################


FilterIt <- function(RawData, xmin, xmax, ymin, ymax, intenmax, intenmin)
{
  filter<- SourceData%>% filter(`x [nm]`> xmin, `x [nm]`<xmax,`y [nm]` > ymin, `y [nm]` < ymax, `intensity [photon]`> intenmin, `intensity [photon]` < intenmax)
}

##############################################################################3

PlotFX1TrajOnSeq <- function(InputLongForm)
{
  ReadyPlot <- InputLongForm %>% ggplot(aes(x=seq(1,nrow(InputLongForm),1), y=x, colour=X.s.)) + geom_line()
}

##############################################################################3
PlotFX1TrajOnFra <- function(InputLongForm)
{
  ReadyPlot <- InputLongForm %>% ggplot(aes(x=t, y=x, colour=X.s.)) + geom_line()
}

##################################################################################

PlotFX1SingleTraj <- function(InputLongForm, TrajNumber)
{
  fil <- InputLongForm %>%filter(X.s.==TrajNumber)
  a <- max(fil$t)
  b <- min(fil$t)
  ReadyPlot <- fil%>% ggplot(aes(x=t, y=x, colour=X.s.)) + 
  geom_line()+ coord_cartesian(xlim=c(b,a))
}



























