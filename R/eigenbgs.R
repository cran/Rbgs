#'Eigen background subtraction algorithm
#'
#'This function uses previous frames to construct a background model in eigen space. This eigen background
#'model is then subtracted from current frame to obtain the foreground images.
#'
#' @param b 3-D matrix containing grayscaled video frames.
#' @param nf Number of previous frames to be considered. Its value has to be greater than 1.
#' @param thresh threshold required to obtain foreground images.value of thresh can be set to 10 - 15 approximately.
#' @return A 3-D matrix of frames containing foreground obtained after background subtraction
#'  is applied(binary images).
#' @export
eigenbgs<-function(b,nf,thresh)
{
  dimx<-(dim(b)[1]*dim(b)[2])
  tempx<- base::array(0,c(dimx,nf))
  for(i in 1:nf)
  {
    tempx[,i]=b[,,i]
  }
   b[,,1]=b[,,2]-b[,,1]
   b[,,1]=imager::threshold(b[,,1],thr=thresh,approx=TRUE)
  for(i in 3:dim(b)[3])
  {
    if(i<=nf+1)
    {
      tempmeans<-base::array(0,c(dimx,1))

      tempmeans= base::rowMeans(tempx[,1:i-1])
      normx<- base::array(0,c(dimx,i-1))

      for(k in 1:(i-1))
      {
        normx[,k]=tempx[,k]-tempmeans
      }

    }
    if(i>(nf+1))
    {
      j= magrittr::mod((i-1),nf)
      if(j!=0)
      {
        tempx[,j]=b[,,i-1]
      }
      else
      {
        tempx[,nf]= b[,,i-1]
      }
    tempmeans<-base::array(0,c(dimx,1))
    tempmeans= base::rowMeans(tempx)
    normx<- base::array(0,c(dimx,nf))
    k=1
    for(k in 1:nf)
    {
      normx[,k]=tempx[,k]-tempmeans
    }
    }
    svdx=base::svd(normx)
    U=svdx$u
    dorigx=dim(b)[1]*dim(b)[2]
    origx<- base::array(b[,,i],c(dorigx,1))
    p= base::t(U) %*% (origx-tempmeans)
    approxx=(U %*% p) + tempmeans
    distance= imager::threshold(base::abs(origx-approxx),thr=thresh, approx=TRUE)
    b[,,i-1] = base::array(distance,c(dim(b)[1],dim(b)[2]))

    graphics::plot(imager::as.cimg(b[,,i-1]),main=i)


  }
  return (b)
  }









