#' grottaBar
#'
#' Automates the production of a Grotta Bar in ggplot
#'
#' @param x a 2- or 3- dimensional table, returned by the table() function
#' @param groupName a character string giving the name of the group varialble
#' @param scoreName a character string giving outcome (mRS) labels
#' @param strataNamea character string giving the strata variable name
#' @param width a number adjusting the width of the lines between bars
#'
#' @examples
#'
#' df <- alteplase
#' df$mRS <- df$mRS -1
#' x <- table(mRS=df$mRS,
#'            Group=df$treat,
#'            Time=df$time)
#'
#' grottaBar(x,groupName="Group",
#'           scoreName = "mRS",
#'           strataName="Time",
#'           colourScheme ="colour"
#'  )
#'
#'x <- table(mRS=df$mRS,
#'           Group=df$treat)
#'grottaBar(x,groupName="Group",
#'           scoreName = "mRS",
#'          colourScheme ="colour"
#' )
#'
#'
#' @references aosmith's answer to the following question: https://stackoverflow.com/questions/51213169/is-there-an-efficient-way-to-draw-lines-between-different-elements-in-a-stacked
grottaBar <- function(x,groupName,scoreName,strataName = NULL,
                      colourScheme="colour",
                      width=0.9,
                      returnData = FALSE, printNumbers = TRUE,
                      textSize=15, numberSize=5,
                      lineSize=0.5){

  x <- as.data.frame(x)

  freqName <- setdiff(colnames(x),c(groupName,scoreName,strataName))
  if(length(freqName)>1){
    stop("Too many free columns. Is your data stratified?")
  }

  x <- data.frame(strata=x[,strataName],group=x[,groupName],score=x[,scoreName],n=x[,freqName])

  # Dummy code strata if it doesn't exist
  if(!("strata" %in% colnames(x))) x <- cbind(strata="",x)

  # Get proportions. This has to be done by strata.
  x <- by(x,x$strata,function(x){
    x$p <- x$n
    x[x$group == x$group[1],"p"] <- x[x$group == x$group[1],"p"]/sum(x[x$group == x$group[1],"p"])
    x[x$group != x$group[1],"p"] <- x[x$group != x$group[1],"p"]/sum(x[x$group != x$group[1],"p"])

    x <- do.call("rbind",by(x,x$group,function(df){
      df$p_prev <- cumsum(df$p)-df$p
      df
    }))

    x
  })
  x <- do.call("rbind",x)
  rownames(x) <- NULL

  groupLevels <- levels(x$group)
  x$group <- as.numeric(x$group)

  # Get dataset for lines. This should also be done by strata
  y <- by(x,x$strata,function(x){
    y <- {}
    for(i in 1:(length(groupLevels)-1)){
      this_y <- x[which(x$group %in% i:(i+1)),]

      this_y$score <- as.numeric(this_y$score)

      this_y <- rbind(this_y,
                      data.frame(strata=unique(x$strata),
                                 group=c(i,i+1),
                                 score=c(0,0),
                                 n = c(0,0),
                                 p=c(0,0),
                                 p_prev=c(0,0)
                      )
      )

      this_y$line_id <- paste(unique(x$strata),this_y$score,i)

      this_y$group <- this_y$group - (-1)^(this_y$group==i) * width/2

      y <- rbind(y,this_y)
    }
    y
  })
  y <- do.call("rbind",y)


  ggp <- ggplot2::ggplot(x)+
    ggplot2::geom_rect(color="black", size=lineSize,
                       ggplot2::aes(xmin=group-width/2,xmax=group+width/2,
                                    ymin=p_prev,ymax=p_prev+p,fill=score))+
    ggplot2::geom_line(data=y, size=lineSize,
                       ggplot2::aes(x=group,y=p+p_prev,group=line_id))


  if(printNumbers){
    ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0),], size=numberSize,
                                   ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                label=sprintf("%d",n,100*p)))
  }



  if(colourScheme=="colour"){
    ggp <- ggp + scale_fill_brewer(palette="RdYlGn",direction = -1)
  } else if (colourScheme=="grayscale"){
    ggp <- ggp + scale_fill_manual(
      values=c("#ffffff",
               "#e3e3e3",
               "#c9c9c9",
               "#bababa",
               "#b0b0b0",
               "#949494",
               "#7d7d7d"
      )
    )
  }else{
    stop("colourScheme not recognised")
  }


  if(!is.null(strataName)){
    ggp <- ggp+facet_wrap(~strata, ncol = 2, dir = "v")
  }

  ggp <- ggp +
    scale_x_continuous(breaks = 1:length(groupLevels), labels=groupLevels)+
    scale_y_continuous(labels=scales::percent_format(),expand = expansion(add=0.01))+
    ggplot2::coord_flip(clip="off")+
    labs(fill="mRS")+
    guides(fill=guide_legend(nrow = 1))+
    theme_bw()+
    theme(axis.title = element_blank(),legend.position = "top",
          strip.background = element_rect(fill="white"),
          panel.grid.major =element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size=textSize),
          plot.margin = margin(1, 1, 1, 1, "cm")
    )

  ggp

  if(returnData)
  {
    out <- list(plot=ggp, rectData=x, lineData=y)
    return(out)
  }
  else
  {
    return(ggp)
  }

}


