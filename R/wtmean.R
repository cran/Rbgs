#'Weighted Mean background subtraction algorithm
#'
#'This function calculates the weighted mean of previous three frames. The buffer maintained
#'here can contain only three frames.
#'
#' @param b 3-D matrix containing grayscaled video frames.
#' @param nf It is size of buffer that contains previous frames and its value can be set to three only.
#' @param thresh threshold required to obtain foreground images.Its value can be around 10 - 30.
#' @return A 3-D matrix of frames containing foreground obtained after background subtraction
#'  is applied(binary images).
#' @export

wtmean<- function(b,nf,thresh)
{

  bgmodel<- base::array(0,dim=c(dim(b)[1],dim(b)[2],nf))
  for(i in 1:nf)
  {
    bgmodel[,,i]=b[,,i]
  }
  for(i in 2:dim(b)[3])
  {

    if(i<=(nf+1))
    {
     if(i==2)
       b[,,i-1]= base::floor(base::abs(b[,,i]-bgmodel[,,i-1]))
     if(i==3)
       b[,,i-1]= base::floor(base::abs(b[,,i]-((bgmodel[,,1]*0.5)+(bgmodel[,,2]*0.5))))
     if(i==4)
       b[,,i-1]= base::floor(base::abs(b[,,i]-((bgmodel[,,1]*0.1)+(bgmodel[,,2]*0.2)+(bgmodel[,,3]*0.7))))
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
        b[,,i-1]= base::floor(base::abs(b[,,i]-((bgmodel[,,1]*0.7)+(bgmodel[,,3]*0.2)+(bgmodel[,,2]*0.1))))
      if(temp==2)
        b[,,i-1]= base::floor(base::abs(b[,,i]-((bgmodel[,,2]*0.7)+(bgmodel[,,1]*0.2)+(bgmodel[,,3]*0.1))))
      if(temp==3)
        b[,,i-1]= base::floor(base::abs(b[,,i]-((bgmodel[,,3]*0.7)+(bgmodel[,,2]*0.2)+(bgmodel[,,1]*0.1))))
    }
    b[,,i-1]=imager::threshold(b[,,i-1],thr=thresh,approx=TRUE)

    graphics::plot(imager::as.cimg(b[,,i-1]), main=i)
  }
  return(b)
}




