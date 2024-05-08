#' grottaBar
#'
#' Automates the production of a Grotta Bar using \code{ggplot()}
#'
#' @usage
#' grottaBar(x,groupName,scoreName,strataName = NULL,
#'           colorScheme="lowGreen",
#'           printNumbers = "count",
#'           nCol = 1, dir = "v",
#'           width = 0.9,
#'           textSize = 15, numberSize = 5,
#'           textFace = "plain",
#'           textColor = "black", textCut = 0,
#'           lineSize = 0.5,
#'           returnData = FALSE,
#'           ...
#' )
#'
#' @param x a 2- or 3- dimensional table, returned by the table() function
#' @param groupName a character string giving the name of the group varialble
#' @param scoreName a character string giving outcome (mRS) labels
#' @param strataName a character string giving the strata variable name
#' @param colorScheme a character string indicating the colors that should be used by the plot
#' @param width a number adjusting the width of the lines between bars
#' @param printNumbers a character string indicating if numbers should be printed for each category.
#' @param nCol an integer indicating the number of columns to use for displaying stratified results. Has no effect if no stratification is used.
#' @param dir a character indicating if stratified results should be laid out vertically (\code{"v"}) or horizontally \code{"h"}.
#' @param textSize a number indicating the size of text labels
#' @param numberSize a number indicating the size of printed numbers
#' @param textFace a character string indicating font face of printed numbers. Can be "plain", "bold", "italic" or "bold.italic".
#' @param textColor vector of two colors for text labels
#' @param textCut Controls when the color of the text changes. The first \code{textCut} categories will use the first color
#' @param lineSize a number indicating the thickness of lines in the plot
#' @param lineColor vector color for lines in the plot
#' @param drawLine boolean indicating if connecting lines should be drawn or not
#' @param returnData a boolean indicating if the data used to create the plot should be returned. For expert users only.
#' @param ... additional arguments. Ignored except for \code{colourScheme} and \code{textColour} which will override their counterpart arguments.
#'
#' @details
#' This tool produces a "Grotta" bar chart based on a table of count data.
#' A Grotta bar chart is a common data visualisation tool in stroke research, and is in essence a horizontally stacked proportional bar
#' chart showing the distribution of ordinal outcome data (typically the modified Rankin Scale) across groups, with lines drawn connecting
#' categories across groups.
#'
#' The tool provides three default options for \code{colorScheme}:
#' \itemize{
#'     \item{\code{"lowGreen"}}{ A "traffic light" gradient from green to red, where low scores are colored green}
#'     \item{\code{"lowRed"}}{ A "traffic light" gradient from red to green, where low scores are colored red}
#'     \item{\code{"grayscale"}}{ A grayscale gradient for producing a black and white plot}
#' }
#'
#' In addition to these, setting \code{colorScheme="custom"} allows for a
#' user-specified color scheme by using the ggplot2 family of \code{scale_fill_} functions.
#'
#' The options for \code{printNumbers} are:
#' \itemize{
#'     \item{\code{"count"}}{ The raw counts in the table.}
#'     \item{\code{"proportion"}}{ The within-group proportion, rounded to 2 decimal places.}
#'     \item{\code{"percentage"}}{ The within-group percentage, rounded to 2 decimal places.}
#'     \item{\code{"count.percentage"}}{ The raw count with percentage in parentheses.}
#'     \item{\code{"none"}}{ Do not print any numbers.}
#' }
#'
#' These options may be abbreviated. \code{"p"} is not a valid abbreviation as it matches to multiple options.
#' The minimal abbreviation for \code{"count.percentage"} is \code{"c.p"}
#'
#' @returns A ggplot object, or a list containing a ggplot object and the data used to generate it.
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
#'           colorScheme ="lowGreen"
#'  )
#'
#'   grottaBar(x,groupName="Time",
#'           scoreName = "mRS",
#'           strataName="Group",
#'           colorScheme ="grayscale"
#'  )
#'
#'x <- table(mRS=df$mRS,
#'           Group=df$treat)
#'
#'    grottaBar(x,groupName="Group",
#'              scoreName = "mRS",
#'              colorScheme ="custom"
#'    ) + ggplot2::scale_fill_brewer(palette = "Spectral", direction=-1)
#'
#'   grottaBar(x,groupName="Group",
#'           scoreName = "mRS",
#'           colorScheme ="custom",
#'           textFace = "italic",
#'           printNumbers = "count.percentage"
#'  ) + viridis::scale_fill_viridis(discrete = TRUE,direction = -1)
#'
#'
#' grottaBar(
#'           x,
#'           groupName = "Group",
#'           scoreName = "mRS",
#'           colorScheme = "custom",
#'           textFace = "italic",
#'           printNumbers = "count.percentage"
#' ) + viridis::scale_fill_viridis(discrete = TRUE, direction = -1)
#'
#'
#' grottaBar(x,groupName="Group",
#'            scoreName = "mRS",
#'            colorScheme ="custom",
#'            textFace = "italic",
#'            textColor = c("black","white"),
#'            lineColor = "white",
#'            textCut = 5,
#'            printNumbers = "count.percentage"
#' ) + viridis::scale_fill_viridis(discrete = TRUE,direction = -1)
#'
#'
grottaBar <- function(x,
                      groupName,
                      scoreName,
                      strataName = NULL,
                      colorScheme = "lowGreen",
                      printNumbers = "count",
                      nCol = 1,
                      dir = "v",
                      width = 0.9,
                      textSize = 15,
                      numberSize = 5,
                      textFace = "plain",
                      textColor = "black",
                      textCut = 0,
                      lineSize = 0.5,
                      lineColor = "black",
                      drawLines = TRUE,
                      returnData = FALSE,
                      ...
){

  # Allow British English spelling of "color"
  # exists mainly for backwards compatibility
  # from before arguments were homogenised
  # to american spelling

  args <- list(...)

  if(!is.null(args$colourScheme)){
    colorScheme <- args$colourScheme
  }

  if(!is.null(args$textColour)){
    textColor <- args$textColour
  }


  # This code draws heavily from aosmith's answer to the following question:
  # https://stackoverflow.com/questions/51213169/is-there-an-efficient-way-to-draw-lines-between-different-elements-in-a-stacked

  # See also this discussion on making ggplot2 play nicely with R CMD CHECK
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

  group <- p_prev <- p <- score <- line_id <- n <- NULL


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

    for(i in unique(x$group)) x[x$group == i,"p"] <- x[x$group == i,"p"]/sum(x[x$group == i,"p"])

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
    ggplot2::geom_rect(color=lineColor,
                       alpha = ifelse(colorScheme=="grayscale",0.5,1),
                       linewidth=lineSize,
                       ggplot2::aes(xmin=group-width/2,xmax=group+width/2,
                                    ymin=p_prev,ymax=p_prev+p,fill=score))
  if (drawLines){
  ggp <- ggp +
    ggplot2::geom_line(data=y, color=lineColor,linewidth=lineSize,
                       ggplot2::aes(x=group,y=p+p_prev,group=line_id))
    }


  if(grepl("^c[o]*[u]*[n]*[t]*$",printNumbers)){

    if(is.integer(x$n)){
      ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0),], size=numberSize,
                                     fontface = textFace,
                                     ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                  color = as.numeric(score) > textCut,
                                                  label=sprintf("%d",n)))
    } else {

      #Get maximum required decimal places
      maxDecimal <- max(nchar(x$n-floor(x$n))-2)

      ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0),], size=numberSize,
                                     fontface = textFace,
                                     ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                  color = as.numeric(score) > textCut,
                                                  label=sprintf(sprintf("%%0.%df",maxDecimal),n)))
    }

  } else if (grepl("^pr[o]*[p]*[o]*[r]*[t]*[i]*[o]*[n]*$",printNumbers)){

    ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0),], size=numberSize,
                                   fontface = textFace,
                                   ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                color = as.numeric(score) > textCut,
                                                label=sprintf("%0.2f",p)))

  } else if (grepl("^pe[r]*[c]*[e]*[n]*[t]*[a]*[g]*[e]*$",printNumbers)){

    ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0),], size=numberSize,
                                   fontface = textFace,
                                   ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                color = as.numeric(score) > textCut,
                                                label=sprintf("%2.2f",100*p)))

  } else if (grepl("^c[o]*[u]*[n]*[t]*.p[e]*[r]*[c]*[e]*[n]*[t]*[a]*[g]*[e]*",printNumbers)) {

    if(is.integer(x$n)){
      ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0),], size=numberSize,
                                     fontface = textFace,
                                     ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                  color = as.numeric(score) > textCut,
                                                  label=sprintf("%d\n(%2.1f%s)",n,100*p,"%")))
    } else {
      stop("count.percentage works only with integers")
    }




  } else if (grepl("^n[o]*[n]*[e]*$",printNumbers)){

    # Do nothing if we were told not to print any numbers

  } else {
    stop("Unrecognised option for printNumbers")
  }

  if(colorScheme=="lowGreen"){

    ggp <- ggp + ggplot2::scale_fill_brewer(palette="RdYlGn",direction = -1)

  } else if(colorScheme=="lowRed"){

    ggp <- ggp + ggplot2::scale_fill_brewer(palette="RdYlGn",direction = 1)

  } else if  (colorScheme=="grayscale"){

    ggp <- ggp + ggplot2::scale_fill_brewer(palette="Greys")

  } else if ( colorScheme =="custom"){

    # Do nothing, assume the user will handle this later.

  } else {

    stop("colorScheme not recognised")

  }


  if(!is.null(strataName)){
    ggp <- ggp+ggplot2::facet_wrap(~strata, ncol = nCol, dir = dir)
  }

  ggp <- ggp +
    ggplot2::scale_x_continuous(breaks = 1:length(groupLevels), labels=groupLevels)+
    ggplot2::scale_y_continuous(labels=scales::percent_format(),expand = ggplot2::expansion(add=0.01))+
    ggplot2::coord_flip(clip="off")+
    ggplot2::labs(fill=scoreName)+
    ggplot2::guides(fill=ggplot2::guide_legend(nrow = 1))+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   legend.position = "top",
                   strip.background = ggplot2::element_rect(fill="white"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   text = ggplot2::element_text(size=textSize),
                   plot.margin = ggplot2::margin(1, 1, 1, 1, "cm")
    )+
    ggplot2::scale_color_manual(guide = "none", values = textColor)

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


