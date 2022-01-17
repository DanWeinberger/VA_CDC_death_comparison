runIfExpired <- function(storeName, f, update=F) {
  basepath <- "Data/"
  mostRecent <- mostRecentTimestamp(storeName, basepath=basepath)
  f <- rlang::as_function(f)
  
  runAndArchive <- function() {
    data <- f()
    storeRDS(data, storeName, basepath)
    data
  }
  
  if (is.na(mostRecent)) 
    return(runAndArchive())
  
  if (update==F)
    return(retrieveRDS(storeName, basepath))
  
  runAndArchive()
}
