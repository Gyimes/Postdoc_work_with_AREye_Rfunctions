#This function helps polishing the data by making sure that the "subject" column is reflecting the condition difference. For example, if subject is always recorded as 1:10, this would help making it 1:10 for ctrl, and 101:110 for experimental.
#This is useful for later filtering, subsetting, checking and plotting the data

datapolisher_groups <- function(dat, groupcol, subjectcol, groupdiffby){
  levelcheck <- dplyr::distinct(data.frame(dat[[groupcol]]))[["dat..groupcol.."]]
  if (length(levelcheck)-1 == length(groupdiffby)) {
    for (i in 2:length(levelcheck)) {
      dat[[subjectcol]] <- dplyr::if_else(dat[[groupcol]] == levelcheck[i], as.numeric(dat[[subjectcol]])+groupdiffby[i-1], as.numeric(dat[[subjectcol]]))
    }
    return(dat)
  }
  else {
    if (length(levelcheck)-1 > length(groupdiffby)) {
      print(paste0(length(levelcheck)-1-length(groupdiffby), " more groupdiff values needed"))
    }
    else{
      print(paste0(length(groupdiffby)-length(levelcheck)+1, " less groupdiff values needed"))
    }
  }
}
