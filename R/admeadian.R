#'Adaptive meadian background subtraction algorithm
#'
#'This function performs background subtraction on input grayscale frames using adaptive median
#'background subtraction algorithm.The algorithm depends upon the number of previous frames taken
#'into consideration.
#'
#' @param b 3-D matrix containing grayscaled video frames.
#' @param nf Number of previous frames to be taken into consideration.
#' @param thresh threshold required to obtain foreground images. its value can be around 10 to 30 depending
#' upon the accuracy of the result.
#' @return A 3-D matrix of frames containing foreground obtained after background subtraction
#'  is applied(binary images).
#' @examples
#'  \donttest{
#' videoURL <- system.file("extdata","daria_skip.avi",package = "Rbgs")
#' frames <- readvideoframe(videoURL,1,15)
#' bground<-admedian(frames,3,25) }
#'
#' @export
admedian<- function(b,nf,thresh)
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
      b[,,i-1]=base::floor(base::abs(b[,,i]-base::apply(bgmodel[,,1:i-1],c(1,2),FUN="median",na.rm=FALSE)))
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
      b[,,i-1]=base::floor(base::abs(b[,,i] - base::apply(bgmodel,c(1,2),FUN="median",na.rm=FALSE)))
    }

    b[,,i-1]<- imager::threshold(b[,,i-1],thr=thresh,approx=TRUE)

    graphics::plot(imager::as.cimg(b[,,i-1]),main=i)
  }
  return (b)
}
