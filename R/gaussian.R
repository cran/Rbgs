#'Gaussian background subtraction algorithm
#'
#'This functions creates a gaussian background model using the previous grayscale frames.
#'gaussian parameters (i.e mean and variance) are updated after every new frame is encountered.
#
#'
#' @param b 3-D matrix containing grayscaled video frames.
#' @param thresh threshold required to obtain foreground images. Its value can be set around 3.5 - 4.5
#' depending upon the accuracy.
#' @return A 3-D matrix of frames containing foreground obtained after background subtraction
#'  is applied(binary images).
#' @export
#'
guassianbgs<-function(b,thresh)
{

  mask= base::array(1,c(3,3))
  variance<-init(b[,,1])
  mean<-b[,,1]
  distance<- base::array(0,c(dim(b)[1],dim(b)[2]))
  alpha=0.01
  for(i in 2:dim(b)[3])
  {
    b[,,i-1]= base::abs((b[,,i]-mean)/ base::ceiling(base::sqrt(variance)))

    distance= base::abs((b[,,i]-mean)^2)
    mean=(alpha*b[,,i])+((1-alpha)*mean)

    variance= (distance*alpha)+((1-alpha)*variance)
    b[,,i-1]=imager::threshold(b[,,i-1],thr=thresh,approx=TRUE)
    graphics::plot(imager::as.cimg(b[,,i-1]), main=i)
  }

  return(b)


}


init<-function(invar)
{
  #initialization
  width=dim(invar)[2]
  height=dim(invar)[1]
  invar1<- base::array(0,c(height,width))
  x=invar[,1]
  invar= base::cbind(x,invar)
  x=invar[,width+1]
  invar= base::cbind(invar,x)
  x=invar[1,]
  invar= base::rbind(x,invar)
  x=invar[height+1,]
  invar= base::rbind(invar,x)

  for(i in 2:(height+1))
  {
    for(j in 2:(width+1))
    {
      imin=i-1
      imax=i+1
      jmin=j-1
      jmax=j+1
      temp= base::array(invar[imin:imax,jmin:jmax],c(1,9))
      invar1[imin,jmin]=stats::var(c(temp))


    }

  }
  return(invar1)
}
