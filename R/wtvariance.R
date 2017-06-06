#'Weighted variance background subtraction algorithm
#'
#'This function calculates the weighted variance of previous three frames. The buffer maintained
#'here can contain only three frames.
#'
#' @param b 3-D matrix containing grayscaled video frames.
#' @param nf previous number of frames to be considered to build background.
#' @param thresh threshold required to obtain foreground images.Its value can be set to 180-230 or more approximately.
#' @return A 3-D matrix of frames containing foreground obtained after background subtraction
#'  is applied(binary images).
#' @export
#'
wtvar<- function(b,nf,thresh)
{
  bgmodel<-base::array(0,dim=c(dim(b)[1],dim(b)[2],nf))
  wmean<-base::array(0,dim=c(dim(b)[1],dim(b)[2]))
  for(i in 1:nf)
  {
    bgmodel[,,i]=b[,,i]
  }
  for(i in 2:dim(b)[3])
  {
    #print(b)
    if(i<=(nf+1))
    {
      if(i==2)
      {
        b[,,i-1]= base::floor(base::abs(b[,,i]-bgmodel[,,i-1]))
      }
      if(i==3)
      {
        wmean=(bgmodel[,,1]*0.5)+(bgmodel[,,2]*0.5)
        bgtemp<-base::array(0,dim=c(dim(b)[1],dim(b)[2]))
        for(k in 1:2)
        {
          bgtemp=bgtemp+((bgmodel[,,k]-wmean)^2)
        }
        b[,,i-1]= base::floor(base::abs(b[,,i]-(bgtemp/2)))
      }
      if(i==4)
      {
        wmean=(bgmodel[,,1]*0.2)+(bgmodel[,,2]*0.3)+(bgmodel[,,3]*0.5)
        bgtemp<-base::array(0,dim=c(dim(b)[1],dim(b)[2]))
        for(k in 1:nf)
        {
          bgtemp=bgtemp+((bgmodel[,,k]-wmean)^2)
        }
        b[,,i-1]=base::floor(base::abs(b[,,i]-(bgtemp/2)))
      }
    }
    else
    {
      j= (i-1)%%nf
      if(j!=0)
      {

        bgmodel[,,j]=b[,,i-1]
        temp=j
      }
      else
      {
        bgmodel[,,nf]=b[,,i-1]
        temp=nf
      }


      if(temp==1)
      {
        wmean=(bgmodel[,,1]*0.5)+(bgmodel[,,3]*0.3)+(bgmodel[,,2]*0.2)
      }
      if(temp==2)
      {
        wmean=(bgmodel[,,2]*0.5)+(bgmodel[,,1]*0.3)+(bgmodel[,,3]*0.2)
      }
      if(temp==3)
      {
        wmean=(bgmodel[,,3]*0.5)+(bgmodel[,,2]*0.3)+(bgmodel[,,1]*0.2)
      }

    bgtemp<-base::array(0,dim=c(dim(b)[1],dim(b)[2]))
    for(k in 1:nf)
    {
      bgtemp=bgtemp+((bgmodel[,,k]-wmean)^2)
    }
    b[,,i-1]= base::floor(base::abs(b[,,i]-(bgtemp/2)))
    }
    b[,,i-1]<- imager::threshold(b[,,i-1],thr=thresh,approx=TRUE)
    graphics::plot(imager::as.cimg(b[,,i-1]),main=i)
  }

  return (b)
}


