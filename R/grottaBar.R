#' grottaBar
#'
#' Automates the production of a Grotta Bar in ggplot
#'
#' @param x a 2-dimensional table, returned by the table() function
#' @param groupName a character string giving the name of the group varialble
#' @param scoreName a character string giving the n
#' @param width a number adjusting the width of the lines between bars
#'
#' @references aosmith's answer to the following question: https://stackoverflow.com/questions/51213169/is-there-an-efficient-way-to-draw-lines-between-different-elements-in-a-stacked
grottaBar <- function(x,groupName,scoreName,width=0.9, returnData = FALSE, printNumbers = TRUE,
                      textsize=5, linesize=0.5){

  x <- as.data.frame(x)

  freqName <- setdiff(colnames(x),c(groupName,scoreName))
  x <- data.frame(group=x[,groupName],score=x[,scoreName],n=x[,freqName])

  x$p <- x$n

  x[x$group == x$group[1],"p"] <- x[x$group == x$group[1],"p"]/sum(x[x$group == x$group[1],"p"])
  x[x$group != x$group[1],"p"] <- x[x$group != x$group[1],"p"]/sum(x[x$group != x$group[1],"p"])


  x <- do.call("rbind",by(x,x$group,function(df){
    df$p_prev <- cumsum(df$p)-df$p
    df
  }))

  rownames(x) <- NULL

  groupLevels <- levels(x$group)
  x$group <- as.numeric(x$group)

  # Get dataset for lines
  y <- {}
  for(i in 1:(length(groupLevels)-1)){
    this_y <- x[which(x$group %in% i:(i+1)),]

    this_y$score <- as.numeric(this_y$score)

    this_y <- rbind(this_y,
                    data.frame(group=c(i,i+1),
                               score=c(0,0),
                               n = c(0,0),
                               p=c(0,0),
                               p_prev=c(0,0)
                               )
                    )

    this_y$line_id <- paste(this_y$score,i)

    this_y$group <- this_y$group - (-1)^(this_y$group==i) * width/2

    y <- rbind(y,this_y)
  }


 ggp <- ggplot2::ggplot(x)+
    ggplot2::geom_rect(color="black", size=linesize,
                       ggplot2::aes(xmin=group-width/2,xmax=group+width/2,
                           ymin=p_prev,ymax=p_prev+p,fill=score))+
    ggplot2::geom_line(data=y, size=linesize,
                       ggplot2::aes(x=group,y=p+p_prev,group=line_id))+
    ggplot2::geom_text(data=x[which(x$n>0),], size=textsize,
                       ggplot2::aes(x=group,y=p_prev+0.5*p,
                            label=sprintf("%d",n,100*p)))+
    scale_x_continuous(breaks = 1:length(groupLevels), labels=groupLevels)+
    scale_y_continuous(labels=scales::percent_format())+
    ggplot2::coord_flip()+
    labs(fill=scoreName)+
    theme_bw()+
    theme(axis.title = element_blank(),legend.position = "top")+
    scale_fill_brewer(palette="RdYlGn",direction = -1)+
    guides(fill=guide_legend(nrow = 1))

  if(returnData)
  {
    out <- list(plot=ggp,rectData=x, lineData=y)
    return(out)
  }
  else
  {
    return(ggp)
  }

}


