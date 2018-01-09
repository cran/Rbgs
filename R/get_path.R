get_path <- function()
{
  temp = .libPaths()
  path=file.path(temp[1],"xuggle-xuggler-5.4.jar")
  return(path)
}
