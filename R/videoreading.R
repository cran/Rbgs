#'Read a Video
#'
#' This function takes as input URL of the video. It can return maximum hundred frames of the video
#' in which each frame is resized to 100 X 100 pixel image.
#' If the video contains less than hundred frames. then it will return all the frames otherwise it
#' will automatically return first hundred frames. Frames returned are grayscale frames.
#'
#' @param videoURL Path to the input video file
#' @return A matrix of the grayscale frames.
#' @examples
#' ##Save the URL of the video file into R session and then load videoframes
#' videoURL <- system.file("extdata","daria_skip.avi",package = "Rbgs")
#' frames <- readvideo(videoURL)
#' @export
readvideo <- function(videoURL)
{
  jpaths <- get_path()
  flag_xug <- (base::file.exists(jpaths)) & (base::file.size(jpaths) ==40318152)
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
    rJava::.jpackage(name='Rbgs',jars="*",morePaths=jpaths)
  }
rJava::.jinit()

ob1=rJava::.jnew("Videoread")
ob2=rJava::.jcall(ob1,"[[[I","defaultread",videoURL)
nframes=length(ob2)-1
frames= base::array(0,c(100,100,nframes))
i <-1
for (i in 1:nframes)
{
  frames[,,i]=base::as.matrix( base::sapply(ob2[[i]],rJava::.jevalArray))
  graphics::plot(imager::as.cimg(frames[,,i]),main=i)
}

return(frames)

}
