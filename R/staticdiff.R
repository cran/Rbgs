#'Static frame difference background subtraction algorithm.
#'
#'This function implements a static background subtraction method in which background model is
#'set to first frame. This static background model is then subtracted from all subsequent frames
#'to obtain the forground images.
#'
#' @param b 3-D matrix containing grayscaled video frames.
#' @param thresh threshold required to obtain foreground images. Its value can lie between 30-40
#' or more depending upon the accuracy required.
#' @return A 3-D matrix of frames containing foreground obtained after background subtraction is
#' applied(binary images).
#' @examples
#'  \donttest{
#' videoURL <- system.file("extdata","daria_skip.avi",package = "Rbgs")
#' frames <- readvideoframe(videoURL,1,9)
#' bground<-staticdiff(frames,35) }
#' @export
#'
staticdiff<- function(b,thresh)
{
  flag=0;

  bgmodel<- base::array(0,dim=c(dim(b)[1],dim(b)[2]))

  if(flag==0)
  {
    bgmodel=b[,,1]
    flag=1;
  }

  for(i in 1:dim(b)[3])
  {
    b[,,i]= base::abs(b[,,i]-bgmodel)
    b[,,i]=imager::threshold(b[,,i],thr=thresh,approx=TRUE)
    graphics::plot(imager::as.cimg(b[,,i]),main=i)
  }

 return (b)
  }
