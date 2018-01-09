#'This function helps to add the Xuggle-5.4 to classpath.
#'
#' This function asks the user whether to download and add xuggle-5.4 to classpath. Based upon the
#' input provided by the user, specific action is taken
#'
#' @export
inst_xug <- function()
{
  if(interactive())
  {
      jarpaths <- get_path()
    answer<-tcltk::tk_messageBox("okcancel", "Xuggle-5.4 must be installed for this package. Do you want to install xuggle?")
    if (answer=='ok')
    {
      utils::download.file(url="http://www.dcm4che.org/maven2/xuggle/xuggle-xuggler/5.4/xuggle-xuggler-5.4.jar",dest=jarpaths,method="libcurl",mode="wb")
      rJava::.jpackage(name='Rbgs',jars="*",morePaths=jarpaths)
      if(base::file.exists(jarpaths) & base::file.size(jarpaths) < 40318152)
      {
        base::message("xuggle-5.4 could not be downloaded completely. You need to install it by calling inst_xug() ")
      }
    }
    else
    {
      base::message("Package Rbgs won't work without xuggle-5.4. You need to install it by calling inst_xug().")
    }
  rJava::.jpackage(name='Rbgs',jars="*",morePaths=jarpaths)
}
}


