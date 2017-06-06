.onLoad <- function(libname,pkgname)
  {
  paths=paste(.libPaths()[1],"/xuggle-xuggler-5.4.jar",sep="")
  if(!(base::file.exists(paths)) | (base::file.exists(paths) & base::file.size(paths) != 40318152))
  {
    a=utils::download.file(url="http://www.dcm4che.org/maven2/xuggle/xuggle-xuggler/5.4/xuggle-xuggler-5.4.jar",dest=paths,method="libcurl",mode="wb")
     }


  rJava::.jpackage(name = 'Rbgs', lib.loc=.libPaths(),jars = "*")

  rJava::.jpackage(name='Rbgs',jars="*",morePaths=paths)

  }





