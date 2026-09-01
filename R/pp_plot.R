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
pp_plot <- function(x,
                    groupName,
                    scoreName,
                    strataName = NULL,
                    panel = T,

                    drawPolygon = panel,
                    drawContour = F,
                    drawBars = T,
                    drawCI = T,

                    colorScheme = "lowGreen",
                    printNumbers = "count",
                    nCol = 1,
                    dir = "v",
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

  args <- list(...)

  if(!is.null(args$colourScheme)){
    colorScheme <- args$colourScheme
  }

  if(!is.null(args$textColour)){
    textColor <- args$textColour
  }

  x <- as.data.frame(x)

  freqName <- setdiff(colnames(x),c(groupName,scoreName,strataName))
  if(length(freqName)>1){
    stop("Too many free columns. Is your data stratified?")
  }

  x <- data.frame(strata=x[,strataName],group=x[,groupName],score=x[,scoreName],n=x[,freqName])

  # Dummy code strata if it doesn't exist
  if(!("strata" %in% colnames(x))){
    x <- cbind(strata="",x)
    x$strata <- factor(x$strata)
  }


  strataLevels <- levels(x$strata)
  scoreLevels <- levels(x$score)

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

  if(length(groupLevels)>2) stop("pp_plot does not support more than two groups")

  x$group <- as.numeric(x$group)

  # We need the data in wide format
  x <- do.call("rbind",by(x,paste(x$strata,x$score), function(x){
    data.frame(strata=unique(x$strata), score = unique(x$score),
               n_1 = x$n[x$group==1],
               p_1 = x$p[x$group==1],
               p_prev_1 = x$p_prev[x$group==1],
               n_2 = x$n[x$group==2],
               p_2 = x$p[x$group==2],
               p_prev_2 = x$p_prev[x$group==2]
               )
  }))
  x$strata <- factor(x$strata,strataLevels)

  rownames(x) <- NULL

  # Get cumulative probability for treatment group assuming
  # that proportional odds model is true.
  oddsCurve <- function(x,r) r*x/((r-1)*x + 1)
  oddsCurve_x <- function(x,r) r/((r-1)*x+1)^2 # First derivative with respect to x


  x_strata <- x[x$strata==x$strata[1],]
  results_by_strata <- by(x,x$strata,function(x_strata){

    p0 <- x_strata$p_1
    p1 <- x_strata$p_2

    posx <- cbind(score_1 = x_strata$score, xmin=cumsum(p0)-p0,xmax=cumsum(p0))
    posy <- cbind(score_2 = x_strata$score, ymin=cumsum(p1)-p1,ymax=cumsum(p1))

    posx <- lapply(1:nrow(posx),function(i){posx[i,]})
    posy <- lapply(1:nrow(posy),function(i){posy[i,]})


    posGrid <- expand.grid(posx,posy)

    as.data.frame(cbind(
      do.call("rbind",posGrid[[1]]),
      do.call("rbind",posGrid[[2]])
    )) -> posGrid

    posGrid$score_1 <- factor(posGrid$score_1, labels = scoreLevels)
    posGrid$score_2 <- factor(posGrid$score_2, labels = scoreLevels)




    # If we want to add contour lines, set this up here
    # TODO: we should offer this as an option for the user to specify.
    r <- unique(sort(outer(c(1.5,2,2.5,5,10,20,50,100,1000),c(-1,1),FUN = "^")))

    contour_df <- do.call("rbind",
                          lapply(r, function(odds){
                            out <- data.frame(r=odds,qc=seq(0,1,length.out=501))
                            out$qt <- out$r*out$qc/((out$r-1)*out$qc + 1)
                            out
                          })
    )

    # Get label positions based on maximal distance along contour line from
    # the center
    do.call("rbind",by(contour_df,contour_df$r, function(data){
      data$dev <- ifelse(data$r==1,
                         (data$qc-0.5)^2+(data$qt-0.5)^2,
                         -(data$qc-data$qt)^2
      )
      data[which(data$dev == min(data$dev)),]
    })) -> contour_df_label

    # If we want to draw the connective lines, it's done here
    xlabel <- data.frame(labels=scoreLevels, xpos = unique(apply(posGrid[,c("xmin","xmax")],1,mean)))
    ylabel <- data.frame(labels=scoreLevels, ypos = unique(apply(posGrid[,c("ymin","ymax")],1,mean)))

    splitprop <- outer(p0,p1)
    splitProp <- sum(splitprop[lower.tri(splitprop)])/(sum(splitprop[lower.tri(splitprop)])+sum(splitprop[upper.tri(splitprop)]))

    tieGrid <- posGrid[which(posGrid$score_1==posGrid$score_2),]

    splitTieDf <- do.call("rbind",lapply(1:nrow(tieGrid),function(i){

      dx <- tieGrid[i,"xmax"]-tieGrid[i,"xmin"]
      dy <- tieGrid[i,"ymax"]-tieGrid[i,"ymin"]


      splitprop <- outer(p0,p1)

      # what proportion of the split ties should be considered wins vs losses
      # splitProp <- sum(splitprop[lower.tri(splitprop)])/(sum(splitprop[lower.tri(splitprop)])+sum(splitprop[upper.tri(splitprop)]))

      splitProp <- 0.5

      # Split ties diagonally
      if(splitProp >= 0.5){

        tmp <- rbind(
          c(tieGrid[i,"xmin"],tieGrid[i,"ymax"]-dy*sqrt(2*(1-splitProp))),
          c(tieGrid[i,"xmin"]+dx*sqrt(2*(1-splitProp)),tieGrid[i,"ymax"])
        )

      } else {

        tmp <- rbind(
          c(tieGrid[i,"xmax"]-dx*sqrt(2*(splitProp)),tieGrid[i,"ymin"]),
          c(tieGrid[i,"xmax"],tieGrid[i,"ymin"]+dy*sqrt(2*(splitProp)))
        )
      }

      # Split ties horizontally

      # tmp <- rbind(
      #   c(tieGrid[i,"xmin"],tieGrid[i,"ymin"]+splitProp*(tieGrid[i,"ymax"]-tieGrid[i,"ymin"])),
      #   c(tieGrid[i,"xmax"],tieGrid[i,"ymin"]+splitProp*(tieGrid[i,"ymax"]-tieGrid[i,"ymin"]))
      # )

      colnames(tmp) <- c("x","y")

      #0.2 0.3 are arbitrary and used to force the sequence order later when we're using geom_segment
      cbind(score=as.numeric(tieGrid[i,"score_1"])+c(0.2,0.3),tmp)

    }))

    posGrid <- do.call("rbind",lapply(1:nrow(tieGrid),function(i){
      #0.1 0.4 are arbitrary and used to force the sequence order later when we're using geom_segment
      cbind(score=as.numeric(tieGrid[i,"score_1"])+c(0.1,0.4),
            rbind(
              c(x=tieGrid[i,"xmin"],y=tieGrid[i,"ymin"]),
              c(x=tieGrid[i,"xmax"],y=tieGrid[i,"ymax"])
            )
      )
    }))

    allPoints <- cbind(as.data.frame(rbind(posGrid,splitTieDf)))
    allPoints <- allPoints[order(allPoints$score,allPoints$x,allPoints$y),]




    # Points and 95% CI here
    dichot_odds <- t(sapply(1:(length(scoreLevels)-1),function(i){

      this_xtab <- rbind(
        c(sum(x_strata$n_1[as.numeric(x_strata$score)>i]),
          sum(x_strata$n_1[as.numeric(x_strata$score)<=i])
        ),
        c(sum(x_strata$n_2[as.numeric(x_strata$score)>i]),
          sum(x_strata$n_2[as.numeric(x_strata$score)<=i])
        )
      )

      out <- c(fisher.test(this_xtab)$estimate, fisher.test(this_xtab)$conf.int)

      names(out) <- c("or","lower","upper")
      out

    }))
    tieGrid <- cbind(tieGrid,rbind(dichot_odds,c(rep(NA,3))))




    do.call("rbind",lapply(1:(nrow(tieGrid)-1),function(i){

      x_mid <- tieGrid[i,"xmax"]
      y_mid <- oddsCurve(x_mid,tieGrid[i,"or"])

      # For each point, find intersection between lower/upper odds curves
      # and a straight line that's normal to the curve at that point

      # We can just rootfind this

      x_lower <- uniroot(function(x_lower){

        m <- -1/oddsCurve_x(x_mid,tieGrid[i,"or"])

        normalLineVal <- oddsCurve(x_mid,tieGrid[i,"or"]) + m*(x_lower-x_mid)

        CurveVal <- oddsCurve(x_lower,tieGrid[i,"lower"])

        CurveVal - normalLineVal

      },interval = c(0,1))$root

      y_lower <- oddsCurve(x_lower,tieGrid[i,"lower"])

      x_upper <- uniroot(function(x_upper){

        m <- -1/oddsCurve_x(x_mid,tieGrid[i,"or"])

        normalLineVal <- oddsCurve(x_mid,tieGrid[i,"or"]) + m*(x_upper-x_mid)

        CurveVal <- oddsCurve(x_upper,tieGrid[i,"upper"])

        CurveVal - normalLineVal

      },interval = c(1e-8,1-1e-8))$root

      y_upper <- oddsCurve(x_upper,tieGrid[i,"upper"])

      c(i=i,
        odds_lower=unname(tieGrid[i,"lower"]),
        x_lower=unname(x_lower),
        y_lower=unname(y_lower),
        odds_mid=unname(tieGrid[i,"or"]),
        x_mid=unname(x_mid),
        y_mid=unname(y_mid),
        odds_upper=unname(tieGrid[i,"upper"]),
        x_upper=unname(x_upper),
        y_upper=unname(y_upper)
      )

    }) ) -> odds

    odds <- as.data.frame(odds)

    # Experimental feature - plot the odds contour lines along each
    # dichotomous point.

    # Disabled by default as it needs work.

    # contour_df_odds <- do.call("rbind",
    #                            lapply(odds$odds_mid, function(odds){
    #                              out <- data.frame(r=odds,qc=seq(0,1,length.out=501))
    #                              out$qt <- out$r*out$qc/((out$r-1)*out$qc + 1)
    #                              out
    #                            })
    # )


    # Win/loss polygons


    winShape <- rbind(
      data.frame(x=tieGrid$xmax,y=tieGrid$ymax),
      data.frame(x=tieGrid$xmax,y=tieGrid$ymin)
    )
    winShape <- winShape[order(winShape$x,winShape$y),]

    winShape <- rbind(
      winShape,
      c(x=1,y=0)
    )

    lossShape <- rbind(
      data.frame(x=tieGrid$xmin,y=tieGrid$ymax),
      data.frame(x=tieGrid$xmin,y=tieGrid$ymin)
    )
    lossShape <- lossShape[order(lossShape$x,lossShape$y),]
    lossShape <- rbind(
      lossShape,
      c(x=0,y=1)
    )


    # Add strata information to all data frames

    tieGrid$strata <- unique(x_strata$strata)
    winShape$strata <- unique(x_strata$strata)
    lossShape$strata <- unique(x_strata$strata)
    contour_df$strata <- unique(x_strata$strata)
    contour_df_label$strata <- unique(x_strata$strata)
    allPoints$strata <- unique(x_strata$strata)
    # contour_df_odds$strata <- unique(x_strata$strata)
    odds$strata <- unique(x_strata$strata)


    return(list(
      tieGrid = tieGrid,
      winShape = winShape,
      lossShape = lossShape,
      contour_df = contour_df,
      contour_df_label = contour_df_label,

      allPoints = allPoints,
      # contour_df_odds = contour_df_odds,
      odds = odds
    ))

  })

  tieGrid <- do.call("rbind",lapply(results_by_strata, function(tmp){tmp$tieGrid}))
  winShape  <- do.call("rbind",lapply(results_by_strata, function(tmp){tmp$winShape}))
  lossShape <- do.call("rbind",lapply(results_by_strata, function(tmp){tmp$lossShape}))
  contour_df <- do.call("rbind",lapply(results_by_strata, function(tmp){tmp$contour_df}))
  contour_df_label <- do.call("rbind",lapply(results_by_strata, function(tmp){tmp$contour_df_label}))
  allPoints <- do.call("rbind",lapply(results_by_strata, function(tmp){tmp$allPoints}))
  # contour_df_odds <- do.call("rbind",lapply(results_by_strata, function(tmp){tmp$contour_df_odds}))
  odds <- do.call("rbind",lapply(results_by_strata, function(tmp){tmp$odds}))



  library(ggplot2)

  out <- ggplot()

  # Draw polygons
  if(drawPolygon){
    out <- out +
      geom_rect(data=tieGrid,
                fill="#e8e156",
                alpha=0.4,
                color="#999999",
                aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                    group=paste(score_1,score_2)
                )
      )+
      geom_polygon(data=winShape,color="#999999",fill="#71f594", alpha = 0.4,aes(x=x,y=y))+
      geom_polygon(data=lossShape,color="#999999",fill="#f97194", alpha = 0.4,aes(x=x,y=y))
  }

  out <- out + annotate("segment",x=0,y=0,xend=1,yend=1, color="dark red",size=1, linetype="dashed")

  # Draw contour lines

  if(drawContour){

    out <- out+
      geom_line(data=contour_df,
                color="dark gray", linetype="dashed",
                aes(x=qc,y=qt,group=paste(r)))+
      geom_text(data=contour_df_label,
                label.size = NA,
                label.padding = unit(0, "lines"),
                hjust=0,vjust=1,size=3.5,
                aes(x=qc,y=qt,label=sprintf("%0.2f",r)),
                position=position_dodge()
      )


    #
    # out +
    #   geom_path(data=contour_df_odds,
    #             aes(x=qc,y=qt,group=r), color="dark gray", linetype="dashed"
    #   )

  }

  # Draw bars
  if(drawBars){

    if(panel | length(unique(x$strata))==1){

      out <- out +
        geom_rect(data=tieGrid,
                  ymin=-0.1,ymax=0,color="black",
                  aes(xmin=xmin,xmax=xmax,fill=factor(score_1)))+
        geom_rect(data=tieGrid,
                  xmin=-0.1,xmax=0,color="black",
                  aes(ymin=ymin,ymax=ymax,fill=factor(score_2))) +
        geom_text(data=tieGrid, aes(x=(xmin+xmax)/2,y=-0.05,label=sprintf("%0.2f",xmax-xmin)))+
        geom_text(data=tieGrid, aes(y=(ymin+ymax)/2,x=-0.05,label=sprintf("%0.2f",ymax-ymin)))

    } else {


      barWidth  <- 0.1

      tieGrid$barMin <- -barWidth * as.numeric(tieGrid$strata)
      tieGrid$barMax <- tieGrid$barMin+barWidth


      out <- out +
        geom_rect(data=tieGrid,
                  color="black",
                  aes(ymin=barMin,ymax=barMax,
                      xmin=xmin,xmax=xmax,fill=factor(score_1)))+
        geom_rect(data=tieGrid,
                  color="black",
                  aes(xmin=barMin,xmax=barMax,
                      ymin=ymin,ymax=ymax,fill=factor(score_2)))+
        geom_text(data=tieGrid, aes(x=(xmin+xmax)/2,y=(barMin + barMax)/2,label=sprintf("%0.2f",xmax-xmin)))+
        geom_text(data=tieGrid, aes(y=(ymin+ymax)/2,x=(barMin + barMax)/2,label=sprintf("%0.2f",ymax-ymin)))


      out <- out + geom_text(data =  unique(tieGrid[,c("strata","barMin","barMax")]),
                             aes(label = strata,
                                 color = strata,
                                 x=(barMin + barMax)/2 ,
                                 y=(barMin + barMax)/2)
                             )


    }

    out <- out + scale_fill_brewer(palette="RdYlGn",direction=-1)

  }


  # Draw points

  if(panel | length(unique(x$strata))==1){

    out <- out +
      geom_path(data=allPoints,
                aes(x=x,y=y))+
      geom_point(data=odds,
                 aes(x=x_mid,y=y_mid))

    # Draw confidence intervals around the points
    if(drawCI)
      out <- out+
      geom_segment(data=odds,
                   aes(x=x_lower,y=y_lower,xend=x_upper,yend=y_upper, group=i))

  } else {

    out <- out +
      geom_path(data=allPoints,
                aes(x=x,y=y,color=strata))+
      geom_point(data=odds,
                 aes(x=x_mid,y=y_mid,color=strata))

    # Draw confidence intervals around the points
    if(drawCI)
      out <- out+
        geom_segment(data=odds,
                     aes(x=x_lower,y=y_lower,xend=x_upper,yend=y_upper, group=i,color=strata))

    out <- out + scale_color_brewer(palette="Set1")
  }

  out <- out +
    labs(x="Control mRS distribution",y="Treatment mRS distribution",fill="mRS") +
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      aspect.ratio = 1
    )

  if(panel & length(unique(x$strata)) > 1){
    out <- out + facet_wrap(~strata)
  }

  out

}
