#'Total number of frames in a video.
#'
#' This function takes as input URL of the video. This function returns total number of frames contained in
#' the video file.
#'
#' @param videoURL Path to the input video file
#' @return count total no of frames in a video.
#' @export
totalframe <- function(videoURL)
{

  rJava::.jinit()

  countx=rJava::.jnew("Videoread")
  count=rJava::.jcall(countx,"I","framecount",videoURL)


  return(count)

}
