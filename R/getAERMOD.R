getAERMOD <- function() {
 
  tf <- tempfile("aermod", fileext = ".zip")
  download.file("http://www.epa.gov/ttn/scram/models/aermod/aermod_exe.zip", tf)
  
  unzip(tf)
  
}