.onLoad <- function(libname,pkgname)
{
  jarfiles=system.file("java", package='Rbgs')
  rJava::.jpackage(name = 'Rbgs', lib.loc=jarfiles,jars = "*")
}










