#'Mean background subtraction algorithm
#'
#'This function calculates the mean of all previous frames and obtains the #'foreground by subtracting the mean from the current frame. All n-1 frames are #'taken into consideration at nth iteration.
#'
#' @param b 3-D matrix containing grayscaled video frames.
#' @param thresh threshold required to obtain foreground images.Its value can lie
#' between 30 40 or more depending upon accuracy of results.
#' @return A 3-D matrix of frames containing foreground obtained after background #'subtraction
#'  is applied(binary images).
#' @export
meanbgs<- function(b,thresh)
{
  bgmodel<- base::array(0,dim=c(dim(b)[1],dim(b)[2]))
  bgmodel<-b[,,1]
  for(i in 2:dim(b)[3])
  {
    b[,,i-1] <- base::abs(floor(b[,,i] - (bgmodel/(i-1))))
    bgmodel <- bgmodel+b[,,i]
    b[,,i-1]<- imager::threshold(b[,,i-1],thr=thresh,approx=TRUE)
    graphics::plot(imager::as.cimg(b[,,i-1]),main=i)
  }
  return (b)
}
