#'Total number of frames in a video.
#'
#' This function takes as input URL of the video. This function returns total number of frames contained in
#' the video file.
#'
#' @param videoURL Path to the input video file
#' @return count total no of frames in a video.
#' @examples
#' ##Save the URL of the video file into R session and then load videoframes
#' videoURL <- system.file("extdata","daria_skip.avi",package = "Rbgs")
#' no_of_frames <- totalframe(videoURL)
#' @export
totalframe <- function(videoURL)
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

  countx=rJava::.jnew("Videoread")
  count=rJava::.jcall(countx,"I","framecount",videoURL)


  return(count)

}
