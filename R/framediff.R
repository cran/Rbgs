#'Frame difference background subtraction algorithm.
#'
#'This functions performs background subtraction on input grayscale frames using dynamic frame difference
#'background subtraction algorithm.The algorithm subtracts the previous frame from the current frame.
#'hence background model is continuously updated with the previous frame.
#'
#' @param b 3-D matrix containing grayscaled video frames.
#' @param thresh threshold required to obtain foreground images. Its value can be around 10-30 or more depending
#' upon the accuracy.
#' @return A 3-D matrix of frames containing foreground obtained after background subtraction
#'  is applied(binary images).
#'  @examples
#' ##Save the URL of the video file into R session and then load videoframes
#' videoURL <- system.file("extdata","jog.mp4",package = "Rbgs")
#' frames <- readvideoframe(videoURL,90,110)
#' foreground <- framediff(frames,20)
#'
#' @export
framediff<- function(b,thresh)
{

  bgmodel<- base::array(0,dim=c(dim(b)[1],dim(b)[2]))
  for(i in 1:dim(b)[3])
  {
    if(i>1)
    {
      b[,,i-1]= base::abs(b[,,i]-b[,,i-1])
      b[,,i-1]=imager::threshold(b[,,i-1],thr=thresh, approx=TRUE)
      graphics::plot(imager::as.cimg(b[,,i-1]), main=i)
    }
  }
  return(b)
}
