#The device measured eye movement with euler angles and head movements in cm. I have created heatmaps where the values represented how frequently the particiapnt looked at a certain point.

#### Frames and heatmaps ####
framemaker_withplot <- function(data, from, to, by, subject, cond, xaxscol, yaxscol, maxlim = 10, limby = 2){
  library(ggplot2)
  d <- dplyr::filter(data, Participant_ID %in% subject & <Cond_column> == cond)
  if (dim(d)[1] > 0) {
    dframe <- data.frame(rep(seq(from = from, to = to, by = by), each = length(seq(from = from, to = to, by = by))))
    colnames(dframe) <- "Xaxis"
    dframe$Yaxis <- seq(from = from, to = to, by = by)
    dframe$Val <- 0
    for (linerunner in 1:dim(d)[1]) {
      a <- (length(c(-70:d[[xaxscol]][linerunner]))-1)*(length(seq(from = from, to = to, by = by)))+length(c(-70:d[[yaxscol]][linerunner])) #These values correspond to the data I was working with, it may need some tweaking if the zero-zero-zero is established differently
      dframe$Val[a] <- dframe$Val[a]+1
    }
    Res <- list()
    Res[[1]] <- dframe
    names(Res)[1] <- "Frame"
    
    p <- ggplot(data = dframe, aes(x = Yaxis, y = -Xaxis, fill = Val))+
      geom_tile()+
      coord_fixed()+
      scale_fill_viridis_c(option = "B", direction = -1, limits=c(0, maxlim), breaks=seq(0,maxlim,by=limby))+
      ylab("Veritcal [°]")+
      xlab("Horizontal [°]")+
      ggtitle(paste0("Participant ", subject, " ", dplyr::if_else(cond == 0, "CTRL", "EXPERIMENTAL")))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    Res[[2]] <- p
    names(Res)[2] <- "Plot"
    return(Res)
  }
  else{
    Res <- list()
    Res[[1]] <- NA
    names(Res)[1] <- "Frame"
    Res[[2]] <- NA
    names(Res)[2] <- "Plot"
    return(Res)
  }
}
framemaker_withplot_head <- function(data, xfrom, xto, yfrom, yto, by, subject, cond, xaxscol, yaxscol, maxlim = 10, limby = 2){
  library(ggplot2)
  d <- dplyr::filter(data, Participant_ID %in% subject & <Cond_column> == cond)
  if (dim(d)[1] > 0) {
    dframe <- data.frame(rep(seq(from = xfrom, to = xto, by = by), each = length(seq(from = yfrom, to = yto, by = by))))
    colnames(dframe) <- "Xaxis"
    dframe$Yaxis <- seq(from = yfrom, to = yto, by = by)
    dframe$Val <- 0
    for (linerunner in 1:dim(d)[1]) {
      a <- (length(c(xfrom:d[[xaxscol]][linerunner]))-1)*(length(seq(from = yfrom, to = yto, by = by)))+length(c(yfrom:d[[yaxscol]][linerunner]))
      dframe$Val[a] <- dframe$Val[a]+1
    }
    Res <- list()
    Res[[1]] <- dframe
    names(Res)[1] <- "Frame"
    
    p <- ggplot(data = dframe, aes(x = Xaxis, y = Yaxis, fill = Val))+
      geom_tile()+
      coord_fixed()+
      scale_fill_viridis_c(option = "B", direction = -1, limits=c(0, maxlim), breaks=seq(0,maxlim,by=limby))+
      ylab("Veritcal [cm]")+
      xlab("Horizontal [cm]")+
      ggtitle(paste0("Participant ", subject, " ", dplyr::if_else(cond == 0, "CTRL", "EXPERIMENTAL"))))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    Res[[2]] <- p
    names(Res)[2] <- "Plot"
    return(Res)
  }
  else{
    Res <- list()
    Res[[1]] <- NA
    names(Res)[1] <- "Frame"
    Res[[2]] <- NA
    names(Res)[2] <- "Plot"
    return(Res)
  }
}
framemaker_withplot_head_ybl_corr <- function(data, xfrom, xto, yfrom, yto, by, subject, cond, xaxscol, yaxscol, maxlim = 10, limby = 2){
  library(ggplot2)
  d <- dplyr::filter(data, Participant_ID %in% subject & <Cond_column> == cond)
  bl <- mean(d[[yaxscol]])
  d[[yaxscol]] <- d[[yaxscol]]-bl
  if (dim(d)[1] > 0) {
    dframe <- data.frame(rep(seq(from = xfrom, to = xto, by = by), each = length(seq(from = yfrom, to = yto, by = by))))
    colnames(dframe) <- "Xaxis"
    dframe$Yaxis <- seq(from = yfrom, to = yto, by = by)
    dframe$Val <- 0
    for (linerunner in 1:dim(d)[1]) {
      a <- (length(c(xfrom:d[[xaxscol]][linerunner]))-1)*(length(seq(from = yfrom, to = yto, by = by)))+length(c(yfrom:d[[yaxscol]][linerunner]))
      dframe$Val[a] <- dframe$Val[a]+1
    }
    Res <- list()
    Res[[1]] <- dframe
    names(Res)[1] <- "Frame"
    
    p <- ggplot(data = dframe, aes(x = Xaxis, y = Yaxis, fill = Val))+
      geom_tile()+
      coord_fixed()+
      scale_fill_viridis_c(option = "B", direction = -1, limits=c(0, maxlim), breaks=seq(0,maxlim,by=limby))+
      ylab("Veritcal [cm]")+
      xlab("Horizontal [cm]")+
      ggtitle(paste0("Participant ", subject, " ", dplyr::if_else(cond == 0, "CTRL", "EXPERIMENTAL"))))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    Res[[2]] <- p
    names(Res)[2] <- "Plot"
    return(Res)
  }
  else{
    Res <- list()
    Res[[1]] <- NA
    names(Res)[1] <- "Frame"
    Res[[2]] <- NA
    names(Res)[2] <- "Plot"
    return(Res)
  }
}
framemaker_withplot_head_ybl_corr2 <- function(data, xfrom, xto, yfrom, yto, by, subject, cond, xaxscol, yaxscol, maxlim = 10, limby = 2){
  library(ggplot2)
  d <- dplyr::filter(data, Participant_ID %in% subject & <Cond_column> == cond)
  bl <- mean(d[[yaxscol]])
  d[[yaxscol]] <- d[[yaxscol]]-bl
  if (dim(d)[1] > 0) {
    dframe <- data.frame(rep(seq(from = xfrom, to = xto, by = by), each = length(seq(from = yfrom, to = yto, by = by))))
    colnames(dframe) <- "Xaxis"
    dframe$Yaxis <- seq(from = yfrom, to = yto, by = by)
    dframe$Val <- 0
    for (linerunner in 1:dim(d)[1]) {
      a <- (length(c(xfrom:d[[xaxscol]][linerunner]))-1)*(length(seq(from = yfrom, to = yto, by = by)))+length(c(yfrom:d[[yaxscol]][linerunner]))
      dframe$Val[a] <- dframe$Val[a]+1
    }
    Res <- list()
    Res[[1]] <- dframe
    names(Res)[1] <- "Frame"
    
    p <- ggplot(data = dframe, aes(x = Xaxis, y = Yaxis, fill = Val))+
      geom_tile()+
      coord_fixed()+
      scale_fill_viridis_c(option = "B", direction = -1, limits=c(0, maxlim), breaks=seq(0,maxlim,by=limby))+
      ylab("Veritcal [cm]")+
      xlab("Horizontal [cm]")+
      ylim(-100,100)+
      xlim(-100,100)+
      ggtitle(paste0("Participant ", subject, " ", dplyr::if_else(cond == 0, "CTRL", "EXPERIMENTAL"))))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    Res[[2]] <- p
    names(Res)[2] <- "Plot"
    return(Res)
  }
  else{
    Res <- list()
    Res[[1]] <- NA
    names(Res)[1] <- "Frame"
    Res[[2]] <- NA
    names(Res)[2] <- "Plot"
    return(Res)
  }
}
