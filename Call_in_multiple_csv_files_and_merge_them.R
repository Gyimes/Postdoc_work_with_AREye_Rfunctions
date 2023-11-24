#### Data caller ####
# It is frequently the case that an experiemental device or a periodic report comes in as multiple csv files (or other data matrices). This function is to call in all such files in a short move.
# This function can be actualised to txt or other formats as well by replacing the "read.csv" command to another appropriate one, and changing the the gsub() command's patter to the appropriate extension.

datacaller <- function(directory, save_name = FALSE){
  direct <- dir(directory) # Make a list of filenames in the specified folder
  out <- list() #Create and empty list
  for (dfil in direct) {
    out[[(length(out)+1)]] <- read.csv(file = paste0(directory,dfil), header = T) #Call in each file into the list
    if(save_name == TRUE) {
      out[[length(out)]]$File.Name <- as.character(gsub(".csv", "", dfil))
    }
  }
  return(do.call(rbind, out)) # Bind the rows of the frames in each list position creating one big dataframe
}
