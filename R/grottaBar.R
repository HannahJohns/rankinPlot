#' grottaBar
#'
#' Automates the production of a Grotta Bar using \code{ggplot()}
#'
#' @usage
#' grottaBar(x,groupName,scoreName,strataName = NULL,
#'           colorScheme="whiteBlue",
#'           colorScheme.reverse = FALSE,
#'           printNumbers = "count",
#'           nCol = 1, dir = "v",
#'           width = 0.9,
#'           textSize = 15, numberSize = 5,
#'           textFace = "plain",
#'           textColor = NULL,
#'           lineSize = 0.5,
#'           lineColor = "black",
#'           drawLines = TRUE,
#'           returnData = FALSE,
#'           ...
#' )
#'
#' @param x a 2- or 3- dimensional table, returned by the table() function
#' @param groupName a character string giving the name of the group variable
#' @param scoreName a character string giving outcome labels
#' @param strataName a character string giving the strata variable name
#' @param colorScheme a character string indicating the colors that should be used by the plot, or a discrete fill scale returned by ggplot2.
#' @param colorScheme.reverse A logical  indicating if the colour scheme should be reversed.
#' @param width a number adjusting the width of the lines between bars
#' @param printNumbers a character string indicating if numbers should be printed for each category.
#' @param nCol an integer indicating the number of columns to use for displaying stratified results. Has no effect if no stratification is used.
#' @param dir a character indicating if stratified results should be laid out vertically (\code{"v"}) or horizontally \code{"h"}.
#' @param textSize a number indicating the size of text labels
#' @param numberSize a number indicating the size of printed numbers
#' @param textFace a character string indicating font face of printed numbers. Can be "plain", "bold", "italic" or "bold.italic".
#' @param textColor vector of colors for text labels
#' @param lineSize a number indicating the thickness of lines in the plot
#' @param lineColor vector color for lines in the plot
#' @param drawLines boolean indicating if connecting lines should be drawn or not
#' @param returnData a boolean indicating if the data used to create the plot should be returned. For expert users only.
#' @param ... additional arguments. Ignored except for \code{colourScheme} and \code{textColour} which will override their counterpart arguments.
#'
#' @details
#' This tool produces a "Grotta" bar chart based on a table of count data.
#' A Grotta bar chart is a common data visualisation tool in stroke research, and is in essence a horizontally stacked proportional bar
#' chart showing the distribution of ordinal outcome data (typically the modified Rankin Scale) across groups, with lines drawn connecting
#' categories across groups.
#'
#' The tool provides the following options for \code{colorScheme}:
#' \describe{
#'     \item{\code{"whiteBlueGradient"}}{ A gradient from white to blue, where low scores are white}
#'     \item{\code{"lowGreen"}}{ A "traffic light" gradient from green to red, where low scores are colored green}
#'     \item{\code{"lowRed"}}{ A "traffic light" gradient from red to green, where low scores are colored red}
#'     \item{\code{"grayscale"}}{ A grayscale gradient for producing a black and white plot}
#'     \item{\code{"none"}}{No scale is supplied and default ggplot2 fill colours are used}
#' }
#'
#' In addition, setting colourScheme to a ggplot2 discrete scale (e.g. \code{ggplot2::scale_fill_brewer()} allows for a
#' user-specified color scheme using the ggplot2 family of \code{scale_fill_} functions.
#'
#' The options for \code{printNumbers} are:
#' \describe{
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
#' @references
#' National Institute of Neurological Disorders and Stroke rt-PA Stroke Study Group. "Tissue plasminogen activator for acute ischemic stroke." New England Journal of Medicine 333.24 (1995): 1581-1588.
#'
#' @examples
#'
#'
#'df <- alteplase
#'
#'x <- table(mRS=df$mRS,
#'           Group=df$treat,
#'           Time=df$time)
#'
#'grottaBar(x,groupName="Group",
#'          scoreName = "mRS",
#'          strataName="Time"
#')
#'
#'
#'grottaBar(x,groupName="Time",
#'          scoreName = "mRS",
#'          strataName="Group",
#'          textColor = c(rep("black",4),rep("white",3))
#')
#'
#'
#'x <- table(mRS=df$mRS,
#'           Group=df$treat)
#'
#'
#'grottaBar(x,groupName="Group",
#'          scoreName = "mRS",
#'          colorScheme = FALSE
#')
#'
#'grottaBar(x,groupName="Group",
#'          scoreName = "mRS",
#'          colorScheme = ggplot2::scale_fill_brewer(palette = "Spectral", direction=-1)
#')
#'
#'
#'grottaBar(x,groupName="Group",
#'          scoreName = "mRS",
#'          colorScheme = FALSE
#')+ ggplot2::scale_fill_brewer(palette = "Spectral", direction=-1)
#'
#'
#'
#'grottaBar(x,groupName="Group",
#'          scoreName = "mRS",
#'          printNumbers = "count.percentage",
#'          colorScheme = "Grayscale"
#')
grottaBar <- function(x,
                      groupName,
                      scoreName,
                      strataName = NULL,
                      colorScheme = "whiteBlue",
                      colorScheme.reverse = FALSE,
                      printNumbers = "count",
                      nCol = 1,
                      dir = "v",
                      width = 0.9,
                      textSize = 15,
                      numberSize = 5,
                      textFace = "plain",
                      textColor = NULL,
                      lineSize = 0.5,
                      lineColor = "black",
                      drawLines = TRUE,
                      returnData = FALSE,
                      ...
){

  args <- list(...)

  # Allow British English spelling of "color"
  # exists mainly for backwards compatibility
  # from before arguments were homogenised
  # to american spelling

  if(!is.null(args$colourScheme)){
    colorScheme <- args$colourScheme
  }

  if(!is.null(args$textColour)){
    textColor <- args$textColour
  }

  # Parse alternative inputs
  if(!("ScaleDiscrete" %in% class(colorScheme))){
    if(is.null(colorScheme)) colorScheme <- "none"
    if(is.na(colorScheme)) colorScheme <- "none"
    if(is.logical(colorScheme)){
      if(!colorScheme){
        colorScheme <- "none"
      }
    }
  }


  if(is.null(printNumbers)) printNumbers <- "none"
  if(is.na(printNumbers)) printNumbers <- "none"
  if(is.logical(printNumbers)){
    if(!printNumbers){
      printNumbers <- "none"
    }
  }


  if(length(colorScheme) == 1 & is.character(colorScheme)){

    if(colorScheme == "custom"){
      warning("colorScheme = \"custom\" is depreciated. Please use colorScheme = \"none\", NA, NULL or FALSE instead.")
      colorScheme <- "none"
    }

    if(colorScheme == "lowGreen"){
      warning("colorScheme = \"lowGreen\" is depreciated. Please use colorScheme = \"RedYellowGreen\" and colorScheme.reverse=TRUE instead.")
      colorScheme <- "RedYellowGreen"
      colorScheme.reverse <- TRUE
    }

    if(colorScheme == "lowRed"){
      warning("colorScheme = \"lowGreen\" is depreciated. Please use colorScheme = \"RedYellowGreen\" and colorScheme.reverse=FALSE instead.")
      colorScheme <- "RedYellowGreen"
      colorScheme.reverse <- FALSE
    }

    if(colorScheme == "grayscale"){
      warning("colorScheme = \"grayscale\" is depreciated. Please use colorScheme=\"Grayscale\" instead.")
      colorScheme <- "Grayscale"
    }
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

  strataLevels <- levels(x$strata)
  scoreLevels <- levels(x$score)
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



  # Depreciated options go here.
  # It can't go earlier because we need some of the above preprocessing
  # to map to the new version of the results

  if(!is.null(args$textCut)){

    warning("Using `textCut` is depreciated. Please provide a character vector to the `textColor` argument instead.")

    new_textColor <- rep(textColor[1],length(scoreLevels))
    if(length(textColor)>1){
      new_textColor[(1:length(scoreLevels)) > args$textCut] <- textColor[2]
      new_textColor[(1:length(scoreLevels)) <= args$textCut] <- textColor[1]
    }

    textColor <- new_textColor

  }

  if(!is.null(textColor)){
    if(length(textColor) == 1){
      textColor <- rep(textColor,length(scoreLevels))
    } else if(length(textColor) != length(scoreLevels) ){
      stop("textColor should be of length 1 or length equal to the number of values the score can take on.")
    }
  }



  ggp <- ggplot2::ggplot(x)+
    ggplot2::geom_rect(color=lineColor,
                       linewidth=lineSize,
                       ggplot2::aes(xmin=group-width/2,xmax=group+width/2,
                                    ymin=p_prev,ymax=p_prev+p,fill=score))
  if (drawLines){
  ggp <- ggp +
    ggplot2::geom_line(data=y, color=lineColor,linewidth=lineSize,
                       ggplot2::aes(x=group,y=p+p_prev,group=line_id))
  }





  # Colour schemes for the bars
  if("ScaleDiscrete" %in% class(colorScheme)){

    ggp <- ggp + colorScheme
    fill_colours <- NULL

  } else {


    if(length(colorScheme)>1 | !("character" %in% class(colorScheme))){
      stop("colorScheme must be either a single character string or a ScaleDiscrete object.")
    }

    if ( !(colorScheme  %in% c("custom","none"))){
      if(colorScheme == "whiteBlue"){

        fill_colours <- grDevices::colorRampPalette(c("#FFFFFF","#055882"))(length(scoreLevels))

      } else if(colorScheme=="RedYellowGreen"){

        if(length(scoreLevels) <= 11){
          fill_colours <- RColorBrewer::brewer.pal(length(scoreLevels),"RdYlGn")
        } else {
          # Extend out the colour space if there's not enough in the pallette
          fill_colours <- RColorBrewer::brewer.pal(11,"RdYlGn")

          fill_colours <- unique(c(
            grDevices::colorRampPalette(c(fill_colours[1],fill_colours[6]))(floor(length(scoreLevels)/2)+1),
            grDevices::colorRampPalette(c(fill_colours[6],fill_colours[11]))(ceiling(length(scoreLevels)/2))
          )
          )
        }

      } else if (colorScheme %in% c("Grayscale","Greyscale")){

        if(length(scoreLevels) <= 9){
          fill_colours <- RColorBrewer::brewer.pal(length(scoreLevels),"Greys")
        } else {
          # Extend out the colour space if there's not enough in the pallette
          fill_colours <- grDevices::colorRampPalette(c("#FFFFFF","#000000"))(length(scoreLevels))
        }

      } else {
        stop("colorScheme not recognised")
      }

      if(colorScheme.reverse){
        fill_colours <- rev(fill_colours)
      }

      names(fill_colours) <- scoreLevels
      ggp <- ggp + ggplot2::scale_fill_manual(values = fill_colours)

    } else {
      # This is needed so that textColor checks don't break later
      fill_colours <- NULL
    }

  }


  if(is.null(textColor) & ("character" %in% class(fill_colours)) & !("ScaleDiscrete" %in% class(fill_colours))){

    textColor <- sapply(1:length(fill_colours),function(i){

      this_rgb <- c(grDevices::col2rgb(fill_colours[i]))

      color <- NA
      # If this colour is closer to white, return black. Otherwise, return white
      if(sum((this_rgb-c(255,255,255))^2) <= sum(this_rgb^2)){
        color <- "black"
      } else {
        color <- "white"
      }
      return(color)
    })

  } else if(is.null(textColor) & ( ("ScaleDiscrete" %in% class(fill_colours)) | is.null(fill_colours))) {
    textColor <- "black"
  }

  if(length(textColor) == 1 ) textColor <- rep(textColor,length(scoreLevels))
  names(textColor) <- scoreLevels

  for(this_color in unique(textColor)){



    if(grepl("^c[o]*[u]*[n]*[t]*$",printNumbers)){

      if(is.integer(x$n)){
        ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0 &
                                                    x$score %in% names(textColor[textColor==this_color])
                                                    ),], size=numberSize,
                                       fontface = textFace,
                                       color = this_color,
                                       ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                    label=sprintf("%d",n)))
      } else {

        #Get maximum required decimal places
        maxDecimal <- max(nchar(x$n-floor(x$n))-2)

        ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0 &
                                                    x$score %in% names(textColor[textColor==this_color])),], size=numberSize,
                                       fontface = textFace,
                                       color = this_color,
                                       ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                    label=sprintf(sprintf("%%0.%df",maxDecimal),n)))
      }

    } else if (grepl("^pr[o]*[p]*[o]*[r]*[t]*[i]*[o]*[n]*$",printNumbers)){

      ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0 &
                                                  x$score %in% names(textColor[textColor==this_color])),], size=numberSize,
                                     fontface = textFace,
                                     color = this_color,
                                     ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                  color = this_color,
                                                  label=sprintf("%0.2f",p)))

    } else if (grepl("^pe[r]*[c]*[e]*[n]*[t]*[a]*[g]*[e]*$",printNumbers)){

      ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0 &
                                                  x$score %in% names(textColor[textColor==this_color])),], size=numberSize,
                                     fontface = textFace,
                                     color = this_color,
                                     ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                  label=sprintf("%2.2f",100*p)))

    } else if (grepl("^c[o]*[u]*[n]*[t]*.p[e]*[r]*[c]*[e]*[n]*[t]*[a]*[g]*[e]*",printNumbers)) {

      if(is.integer(x$n)){
        ggp <- ggp+ ggplot2::geom_text(data=x[which(x$n>0 &
                                                    x$score %in% names(textColor[textColor==this_color])),], size=numberSize,
                                       fontface = textFace,
                                       color = this_color,
                                       ggplot2::aes(x=group,y=p_prev+0.5*p,
                                                    label=sprintf("%d\n(%2.1f%s)",n,100*p,"%")))
      } else {
        stop("count.percentage works only with integers")
      }


    } else if (grepl("^n[o]*[n]*[e]*$",printNumbers)){

      # Do nothing if we were told not to print any numbers

    } else {
      stop("Unrecognised option for printNumbers")
    }
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
    )


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


