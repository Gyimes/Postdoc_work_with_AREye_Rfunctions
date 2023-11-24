#This function removes participants from a dataset

#### Participant killer ####
participantkiller <- function(dat, participantcol, list_of_people_to_exclude){
  subjs <- dplyr::distinct(data.frame(dat[[participantcol]]))[,1]
  subjs2 <- dplyr::filter(data.frame(subjs), !subjs %in% list_of_people_to_exclude)
  dat <- dplyr::filter(dat, get(participantcol) %in% subjs2[,1])
  return(dat)
}
