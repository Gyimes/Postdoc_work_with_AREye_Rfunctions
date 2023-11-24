#This function removes columns with only NAs

#### NA columns dropper ####
na_col_dropper <- function(dat){
  allcols <- colnames(dat)
  for (i in 1:length(allcols)) {
    if (dim((tidyr::drop_na(data.frame(dat[[allcols[i]]]))))[1] == 0) {
      dat[[allcols[i]]] <- NULL
    }
  }
  return(dat)
}
