#'Variance based background subtraction algorithm
#'
#'This function calculates the variance of previous nf number of frames and obtains the foreground by
#'subtracting the variance obtained from the current frame.
#'
#' @param b 3-D matrix containing grayscaled video frames.
#' @param thresh threshold required to obtain foreground images.Its value can be around 220.
#' @param nf number of frames to be considered to construct background model. Its value has to be greater
#' greater than 1. Smaller value gives better results.
#' @return A 3-D matrix of frames containing foreground obtained after background subtraction
#'  is applied(binary images).
#' @export

varbgs<- function(b,nf, thresh)
{
  bgmodel<- base::array(0,dim=c(dim(b)[1],dim(b)[2],nf))
  for(i in 1:nf)
  {
    bgmodel[,,i]=b[,,i]
  }
  b[,,1]= base::abs( base::floor(bgmodel[,,2]-bgmodel[,,1]))
  for(i in 3:dim(b)[3])
  {

    if(i<=(nf+1))
    {
      b[,,i-1]= base::floor(base::abs(b[,,i]- base::apply(bgmodel[,,1:i-1],c(1,2),FUN="var",na.rm=FALSE)))
    }
    else
    {
      j= (i-1)%%nf
      if(j!=0)
      {

        bgmodel[,,j]=b[,,i-1]
      }
      else
      {
        bgmodel[,,nf]=b[,,i-1]
      }

      b[,,i-1]= base::floor(base::abs(b[,,i]- base::apply(bgmodel,c(1,2),FUN="var",na.rm=FALSE)))
    }
    b[,,i-1]=imager::threshold(b[,,i-1],thr=thresh,approx=TRUE)
    graphics::plot(imager::as.cimg(b[,,i-1]), main=i)

  }

  return(b)
}


