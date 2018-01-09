#'Read the specified frames of a video.
#'
#' This function takes as input URL of the video. It reads only those frames of the video that
#' have been specified by the user. It requires two more parameters 'start' and 'end', that defines the
#' range of the videoframes to be retrieved.
#'
#' @param videoURL Path to the input video file
#' @param start It indicates the first frame you wish to read.IT should be in range zero to frame count
#' and smaller than end parameter.
#' @param end It indicates the last frame you wish to read. It should be greater than start and should
#' be in range 0 to frame count.
#' @return A matrix of the grayscale frames.
#' @examples
#' ##Save the URL of the video file into R session and then load the required videoframes
#' videoURL <- system.file("extdata","jog.mp4",package = "Rbgs")
#' frames <- readvideoframe(videoURL,213,233)
#' @export
readvideoframe <- function(videoURL,start,end)
{
  jarpath <- get_path()
  flag_xug <- (base::file.exists(jarpath)) & (base::file.size(jarpath) ==40318152)
  if((flag_xug == FALSE) & (!(interactive())))
     {
      warning("Xuggle 5.4 is not installed. You can install by calling inst_xug()")
     return(NULL)
      }

  if (flag_xug == FALSE)
  {
    inst_xug()
  }
  else
  {
    rJava::.jpackage(name='Rbgs',jars="*",morePaths=jarpath)
  }
  rJava::.jinit()
  ob1=rJava::.jnew("Videoread")
  ob2=rJava::.jcall(ob1,"[[[I","framesetread",videoURL,base::as.integer(start),base::as.integer(end))
  nframes=length(ob2)-1
  frames= base::array(0,c(100,100,nframes))
  i <-1
  for (i in 1:nframes)
  {
    frames[,,i]= base::as.matrix(base::sapply(ob2[[i]],rJava::.jevalArray))
    graphics::plot(imager::as.cimg(frames[,,i]),main=i)
  }

  return(frames)
    }

