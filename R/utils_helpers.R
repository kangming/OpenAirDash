#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
getTile<-function(ImProxy,target){
  targetInfo <- ifelse(is.null(target),"",paste0('-',target))
  main <- paste(paste0(ImProxy$name,"(",ImProxy$mname,")",targetInfo),paste0(ImProxy$sdate,'至',ImProxy$edate),sep="\n")
  return (main)
}
