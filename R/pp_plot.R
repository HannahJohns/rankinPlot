#' pp_plot
#'
#' Creates probability-probability plots for visualizing all-to-all comparisons
#' of ranked data across two groups
#'
#' @usage
#'
#' pp_plot(x,
#'         groupName,
#'         scoreName,
#'         strataName = NULL,
#'         reverse.scores = F,
#'         panel = TRUE,
#'         panel.nCol = NULL,
#'         panel.dir = "h",
#'         polygon = panel,
#'         polygon.alpha = 0.4,
#'         polygon.color = "#999999",
#'         polygon.win.fill = "#71f594",
#'         polygon.tie.fill = "#e8e156",
#'         polygon.loss.fill= "#f97194",
#'         contour = FALSE,
#'         contour.color = "#555555",
#'         contour.line.color = contour.color,
#'         contour.label.color = contour.color,
#'         confint = TRUE,
#'         confint.angle = "fixed",
#'         confint.level = 0.95,
#'         bar = TRUE,
#'         bar.colorScheme = "whiteBlueGradient",
#'         bar.colorScheme.reverse = FALSE,
#'         bar.width = 0.1,
#'         bar.lineColor = "black",
#'         bar.linewidth =  0.5,
#'         bar.text = "count",
#'         bar.text.size = 5,
#'         bar.text.color = NULL,
#'         bar.text.face = "plain",
#'         neutral.color = "#222222",
#'         neutral.linetype = "dashed",
#'         neutral.linewidth = 0.5,
#'         line.color = NULL,
#'         line.linetype = "solid",
#'         strata.text.size = bar.text.size,
#'         ...
#'         )
#'
#' @param x a 2- or 3- dimensional table, returned by the table() function
#' @param groupName a character string giving the name of the group variable
#' @param scoreName a character string giving outcome labels
#' @param strataName a character string giving the strata variable name
#'                     reverse.scores = F,
#' @param panel a logical indicating if strata should be separated across panels. If true, returns a faceted plot. If false, all strata are condensed into a single panel.
#' @param panel.nCol an integer indicating the number of columns to use for displaying stratified results. Has no effect if no stratification is used or panel is false.
#' @param panel.dir a character indicating if stratified results should be laid out vertically (\code{"v"}) or horizontally \code{"h"}. Has no effect if no stratification is used or panel is false.
#' @param polygon A logical indicating if polygons should be drawn to show the proportion of pairs that are wins, losses or ties. Cannot be used if there are strata and panel is false.
#' @param polygon.alpha A numeric value for the transparency (alpha) for the polygons.
#' @param polygon.color A character string giving the border color for the polygons.
#' @param polygon.win.fill A character string giving the fill color for the polygons indicating a region of wins.
#' @param polygon.tie.fill A character string giving the fill colour for the polygons indicating a region of tied pairs.
#' @param polygon.loss.fill A character string giving the fill colour for the polygons indicating a region of losses.
#' @param contour A logical indicating if contours should be drawn indicating where the probability-probability plot should sit if the proportional odds assumption is met.
#' @param contour.color A character string giving the color of the contours
#' @param contour.line.color A character string giving the color of the contour lines
#' @param contour.label.color A character string giving the color of the contour text
#' @param confint A logical indicating if confidence intervals representing should be drawn around each point. See details.
#' @param confint.angle A character string indicating the direction to draw the angle. See details.
#' @param confint.level A numeric value indicating the level of confidence for the confidence interval.
#' @param bar A logical indicating if bars should be drawn to indicate the distribution of the outcome in each group and strata.
#' @param bar.colorScheme A character string indicating the colour scheme to use for the bars. See details.
#' @param bar.colorScheme.reverse A logical  indicating if the colour scheme should be reversed.
#' @param bar.width A numeric value indicating the width of the bars
#' @param bar.lineColor A character string indicating the colour of the bar borders.
#' @param bar.linewidth A numeric value  indicating the width of the bar borders.
#' @param bar.text a character string indicating if numbers should be printed for each category.
#' @param bar.text.size a number indicating the size of text labels
#' @param bar.text.color A vector of colors for text labels
#' @param bar.text.face A character string indicating font face of printed numbers. Can be "plain", "bold", "italic" or "bold.italic".
#' @param neutral.color A character string indicating the color of the neutral line
#' @param neutral.linetype A character string indicating the line type of the neutral line
#' @param neutral.linewidth A numeric value  indicating the width of the neutral line
#' @param line.color A character string indicating the colour to draw the probability-probability line with, or a discrete color scale returned by ggplot2 to have this vary by strata.
#' @param line.linetype A character string indicating the linetype to draw the probability-probability line with, or a discrete linetype scale returned by ggplot2 to have this vary by strata.
#' @param strata.text.size A number indicating the size of the text to draw the strata label. Only relevant if data is stratified and panel is false.
#' @param ... Any other arguments. Ignored, but used to catch e.g. British spelling of "color".
#'
#' @details
#'
#' Confidence intervals are estimated using Fisher's exact test and correspond to an odds ratio. They are visually represented as segments on the figure, where the start and end
#' points correspond to cumulative probabilities that match the upper/lower bound of the odds ratio. Two options are given for the direction of these lines. "fixed" indicates that
#' all confidence intervals should be drawn at 45 degrees, while "proportional.odds" indicates that they should be drawn perpendicular to the proportional odds contour line.
#'
#' The tool provides the following options for \code{bar.colorScheme}:
#' \itemize{
#'     \item{\code{"whiteBlue"}}{ A gradient from white to blue, where low scores are white}
#'     \item{\code{"RedYellowGreen"}}{ A "traffic light" gradient from green to red, where low scores are colored red}
#'     \item{\code{"Grayscale"}}{Grayscale coloring where low scores are colored light and high scores are colored dark}
#'     \item{\code{"none", FALSE, NULL or NA}}{No scale is supplied and default ggplot2 fill colours are used}
#' }
#'
#' In addition, setting colourScheme to a ggplot2 discrete scale (e.g. \code{ggplot2::scale_fill_brewer()} allows for a
#' user-specified color scheme using the ggplot2 family of \code{scale_fill_} functions.
#'
#' The options for \code{bar.text} are:
#' \itemize{
#'     \item{\code{"count"}}{ The raw counts in the table.}
#'     \item{\code{"proportion"}}{ The within-group proportion, rounded to 2 decimal places.}
#'     \item{\code{"percentage"}}{ The within-group percentage, rounded to 2 decimal places.}
#'     \item{\code{"count.percentage"}}{ The raw count with percentage in parentheses.}
#'     \item{\code{"none", FALSE, NULL or NA}}{ Do not print any numbers.}
#' }
#'
#' These options may be abbreviated. \code{"p"} is not a valid abbreviation as it matches to multiple options.
#' The minimal abbreviation for \code{"count.percentage"} is \code{"c.p"}
#'
#'
#' @returns A ggplot object containing the plot.
#'
#' @references
#' Johns, Hannah, et al. "Practical guidance for Win Statistics and Tournament Methods for multifaceted outcomes in stroke research: Review and recommendations." International Journal of Stroke (2026) DOI: https://doi.org/10.1177/17474930261475853
#'
#' @examples
#'
#'df <- alteplase
#'
#'x <- table(mRS=df$mRS,
#'           Group=df$treat,
#'           Time=df$time)
#'
#'pp_plot(x,
#'        groupName =  "Group",
#'        scoreName = "mRS",
#'        strataName = "Time",
#'        panel = TRUE,
#'        confint = TRUE
#')
#'
#'
#'pp_plot(x,
#'        groupName =  "Group",
#'        scoreName = "mRS",
#'        strataName = "Time",
#'        panel = FALSE,
#'        confint = FALSE
#')
#'
#'
#'df <- remapcap
#'df <- df[which(df$group %in% c("uc","shock")),]
#'df$group <-droplevels(df$group)
#'
#'x <- table(Score=df$score,
#'           Group=df$group
#')
#'
#'
#'
#'pp_plot(x,groupName="Group",
#'        scoreName = "Score",
#'        reverse.scores = TRUE,
#'        bar.text = FALSE,
#'        confint.angle = "fixed",
#'        bar.colorScheme = "RedYellowGreen",
#'        bar.colorScheme.reverse = TRUE
#')
#'
#'# Visually inspect proportional odds assumption
#'pp_plot(x,groupName="Group",
#'        scoreName = "Score",
#'        reverse.scores = TRUE,
#'        bar.text = FALSE,
#'        confint.angle = "proportional.odds",
#'        bar.colorScheme = "RedYellowGreen",
#'        bar.colorScheme.reverse = TRUE,
#'        contour = TRUE,
#'        polygon = FALSE,
#'        neutral.color = "darkred", neutral.linewidth =1
#')
#'
#'
#'
#'# Transform the data for comparison of two arms against a common control arm
#'
#'df_shock <- remapcap[which(remapcap$group %in% c("uc","shock")),]
#'df_fixed <- remapcap[which(remapcap$group %in% c("uc","fixed")),]
#'
#'df_shock$group2 <- ifelse(df_shock$group=="uc","Usual Care","Intervention")
#'df_fixed$group2 <- ifelse(df_fixed$group=="uc","Usual Care","Intervention")
#'df_shock$intervention <- "Shock"
#'df_fixed$intervention <- "Fixed"
#'
#'df <- rbind(df_shock,df_fixed)
#'df$group2 <- factor(df$group2,levels=c("Usual Care","Intervention"))
#'
#'
#'x <- table(Score=df$score,
#'           Group=df$group2,
#'           Intervention=df$intervention
#')
#'
#'
#'
#'pp_plot(x,groupName="Group",
#'        scoreName = "Score",
#'        strataName = "Intervention",
#'        reverse.scores = TRUE,
#'        bar.text = FALSE,
#'        confint= FALSE,
#'        panel=FALSE,
#'        bar.colorScheme = "RedYellowGreen",
#'        bar.colorScheme.reverse = TRUE
#')
pp_plot <- function(x,
                    groupName,
                    scoreName,
                    strataName = NULL,

                    reverse.scores = FALSE,

                    panel = TRUE,
                    panel.nCol = NULL,
                    panel.dir = "h",

                    polygon = panel,
                    polygon.alpha = 0.4,
                    polygon.color = "#999999",
                    polygon.win.fill = "#71f594",
                    polygon.tie.fill = "#e8e156",
                    polygon.loss.fill= "#f97194",

                    contour = FALSE,
                    contour.color = "#555555",
                    contour.line.color = contour.color,
                    contour.label.color = contour.color,

                    confint = TRUE,
                    confint.angle = "fixed",
                    confint.level = 0.95,

                    bar = TRUE,
                    bar.colorScheme = "whiteBlue",
                    bar.colorScheme.reverse = F,
                    bar.width = 0.1,
                    bar.lineColor = "black",
                    bar.linewidth =  0.5,

                    bar.text = "count",
                    bar.text.size = 5,
                    bar.text.color = NULL,
                    bar.text.face = "plain",

                    neutral.color = "#222222",
                    neutral.linetype = "dashed",
                    neutral.linewidth = 0.5,

                    line.color = NULL,
                    line.linetype = "solid",
                    strata.text.size = bar.text.size,
                    ...
){

  # Used in the main PP plot and strata labels
  aes_wrapper <- function(...){ggplot2::aes(...)}

  # Allow British English spelling of "color"
  args <- list(...)

  if(!is.null(args$polygon.colour)){
    polygon.color <- args$polygon.colour
  }

  if(!is.null(args$contour.color)){
    contour.color <- args$contour.colour
  }

  if(!is.null(args$contour.line.colour)){
    contour.line.color <- args$contour.line.colour
  }

  if(!is.null(args$contour.label.colour)){
    contour.label.color <- args$contour.label.colour
  }

  if(!is.null(args$bar.colourScheme)){
    bar.colorScheme <- args$bar.colourScheme
  }
  if(!is.null(args$bar.colourScheme.reverse)){
    bar.colorScheme.reverse <- args$bar.colourScheme.reverse
  }

  if(!is.null(args$bar.lineColour)){
    bar.lineColor <- args$bar.lineColour
  }

  if(!is.null(args$bar.text.color)){
    bar.text.color <- args$bar.text.color
  }

  if(!is.null(args$neutral.colour)){
    neutral.color <- args$neutral.colour
  }

  if(!is.null(args$line.colour)){
    line.color <- args$line.colour
  }



  # Parse alternative inputs for "none"
  if(!("ScaleDiscrete" %in% class(bar.colorScheme))){
    if(is.null(bar.colorScheme)) bar.colorScheme <- "none"
    if(is.na(bar.colorScheme)) bar.colorScheme <- "none"
    if(is.logical(bar.colorScheme)){
      if(!bar.colorScheme){
        bar.colorScheme <- "none"
      }
    }
  }

  if(is.null(bar.text)) bar.text <- "none"
  if(is.na(bar.text)) bar.text <- "none"
  if(is.logical(bar.text)){
    if(!bar.text){
      bar.text <- "none"
    }
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

  # Get default options

  if(is.null(line.color)){
    if(length(strataLevels)==1 | panel ){
      line.color <- "black"
    } else {
      line.color <- ggplot2::scale_color_brewer(palette="Dark2")
    }
  }

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
    data.frame(strata=unique(x$strata),
               score = unique(x$score),
               n_1 = x$n[x$group==1],
               p_1 = x$p[x$group==1],
               p_prev_1 = x$p_prev[x$group==1],
               n_2 = x$n[x$group==2],
               p_2 = x$p[x$group==2],
               p_prev_2 = x$p_prev[x$group==2]
               )
  }))
  x$strata <- factor(x$strata,strataLevels)

  # The above busts the ordering of factors because it's looping over
  # a character concatenation of strata and score. This causes problems later,
  # because the code below assumes that x is ordered according to factors.
  # Correcting the order after the fact is the simplest fix.

  if(reverse.scores){
    x$score <- factor(x$score,levels=rev(levels(x$score)))
    scoreLevels <- rev(scoreLevels)
  }

  x <- x[order(x$score),]

  rownames(x) <- NULL

  # Get cumulative probability for treatment group assuming
  # that proportional odds model is true.
  oddsCurve <- function(x,r) r*x/((r-1)*x + 1)
  oddsCurve_x <- function(x,r) r/((r-1)*x+1)^2 # First derivative with respect to x

  # x_strata <- x[x$strata==x$strata[1],]
  results_by_strata <- by(x,x$strata,function(x_strata){

    p0 <- x_strata$p_1
    p1 <- x_strata$p_2

    posx <- cbind(score_1 = x_strata$score, xcount=x_strata$n_1, xmin=cumsum(p0)-p0,xmax=cumsum(p0))
    posy <- cbind(score_2 = x_strata$score, ycount=x_strata$n_2, ymin=cumsum(p1)-p1,ymax=cumsum(p1))

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

    # splitprop <- outer(p0,p1)
    # splitProp <- sum(splitprop[lower.tri(splitprop)])/(sum(splitprop[lower.tri(splitprop)])+sum(splitprop[upper.tri(splitprop)]))
    splitProp <- 0.5

    tieGrid <- posGrid[which(posGrid$score_1==posGrid$score_2),]

    splitTieDf <- do.call("rbind",lapply(1:nrow(tieGrid),function(i){

      dx <- tieGrid[i,"xmax"]-tieGrid[i,"xmin"]
      dy <- tieGrid[i,"ymax"]-tieGrid[i,"ymin"]


      # splitprop <- outer(p0,p1)

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

      out <- c(stats::fisher.test(this_xtab)$estimate, stats::fisher.test(this_xtab, conf.level = confint.level)$conf.int)

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

      x_lower <- stats::uniroot(function(x_lower){

        if(confint.angle == "proportional.odds"){
          m <- -1/oddsCurve_x(x_mid,tieGrid[i,"or"])
        } else if(confint.angle == "fixed"){
          m <- -1
        } else {
          stop("confint.angle should be proportional.odds or fixed")
        }

        normalLineVal <- oddsCurve(x_mid,tieGrid[i,"or"]) + m*(x_lower-x_mid)

        CurveVal <- oddsCurve(x_lower,tieGrid[i,"lower"])

        CurveVal - normalLineVal

      },interval = c(0,1))$root

      y_lower <- oddsCurve(x_lower,tieGrid[i,"lower"])

      x_upper <- uniroot(function(x_upper){

        if(confint.angle == "proportional.odds"){
          m <- -1/oddsCurve_x(x_mid,tieGrid[i,"or"])
        } else if(confint.angle == "fixed"){
          m <- -1
        } else {
          stop("confint.angle should be proportional.odds or fixed")
        }

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

  out <- ggplot2::ggplot()

  # Draw polygons
  if(polygon){
    out <- out +
      ggplot2::geom_rect(data=tieGrid,
                fill=polygon.tie.fill,
                alpha=polygon.alpha,
                color=polygon.color,
                ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                    group=paste(score_1,score_2)
                )
      )+
      ggplot2::geom_polygon(data=winShape,color=polygon.color,fill=polygon.win.fill, alpha = polygon.alpha, ggplot2::aes(x=x,y=y))+
      ggplot2::geom_polygon(data=lossShape,color=polygon.color,fill=polygon.loss.fill, alpha = polygon.alpha, ggplot2::aes(x=x,y=y))
  }

  out <- out + ggplot2::annotate("segment",x=0,y=0,xend=1,yend=1,
                                 color=neutral.color,
                                 linewidth=neutral.linewidth,
                                 linetype=neutral.linetype)

  # Draw contour lines

  if(contour){

    out <- out+
      ggplot2::geom_line(data=contour_df,
                color=contour.line.color, linetype="dashed",
                ggplot2::aes(x=qc,y=qt,group=paste(r)))+
      ggplot2::geom_label(data=contour_df_label,
                color=contour.label.color,
                label.size = NA,
                label.padding = ggplot2::unit(0, "lines"),
                hjust=0,vjust=1,size=3.5,
                ggplot2::aes(x=qc,y=qt,label=sprintf("%0.2f",r))
      )

    # out +
    #   geom_path(data=contour_df_odds,
    #             aes(x=qc,y=qt,group=r), color="dark gray", linetype="dashed"
    #   )

  }

  # Draw bars
  if(bar){

    if(panel | length(unique(x$strata))==1){

      tieGrid$barMin <- -bar.width
      tieGrid$barMax <- 0

    } else {

      tieGrid$barMin <- -bar.width * as.numeric(tieGrid$strata)
      tieGrid$barMax <- tieGrid$barMin+bar.width

    }


    out <- out +
      ggplot2::geom_rect(data=tieGrid,
                         color=bar.lineColor,
                         linewidth = bar.linewidth,
                         ggplot2::aes(ymin=barMin,ymax=barMax,
                                      xmin=xmin,xmax=xmax,fill=factor(score_1)))+
      ggplot2::geom_rect(data=tieGrid,
                         color=bar.lineColor,
                         linewidth = bar.linewidth,
                         ggplot2::aes(xmin=barMin,xmax=barMax,
                                      ymin=ymin,ymax=ymax,fill=factor(score_2)))


    # Colour schemes for the bars
    if("ScaleDiscrete" %in% class(bar.colorScheme)){

      out <- out + bar.colorScheme
      fill_colours <- NULL

    } else {

      if(length(bar.colorScheme)>1 | !("character" %in% class(bar.colorScheme))){
        stop("bar.colorScheme must be either a single character string or a ScaleDiscrete object.")
      }

      if ( !(bar.colorScheme  %in% c("custom","none"))){

        if(bar.colorScheme == "whiteBlue"){

          fill_colours <- grDevices::colorRampPalette(c("#FFFFFF","#055882"))(length(scoreLevels))

        } else if(bar.colorScheme=="RedYellowGreen"){

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

        } else if (bar.colorScheme %in% c("Grayscale","Greyscale")){

          if(length(scoreLevels) <= 9){
            fill_colours <- RColorBrewer::brewer.pal(length(scoreLevels),"Greys")
          } else {
            # Extend out the colour space if there's not enough in the pallette
            fill_colours <- grDevices::colorRampPalette(c("#FFFFFF","#000000"))(length(scoreLevels))
          }

        } else {
          stop("bar.colorScheme not recognised")
        }

        if(bar.colorScheme.reverse){
          fill_colours <- rev(fill_colours)
        }

        names(fill_colours) <- scoreLevels
        out <- out + ggplot2::scale_fill_manual(values = fill_colours)

      } else {
        fill_colours <- NULL
      }
    }


    if(is.null(bar.text.color) & ("character" %in% class(fill_colours)) & !("ScaleDiscrete" %in% class(fill_colours))){

      bar.text.color <- sapply(1:length(fill_colours),function(i){

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

    } else if(is.null(bar.text.color) & ( ("ScaleDiscrete" %in% class(fill_colours)) | is.null(fill_colours)) ) {
      bar.text.color <- "black"
    }

    if(length(bar.text.color) == 1 ) bar.text.color <- rep(bar.text.color,length(scoreLevels))
    names(bar.text.color) <- scoreLevels

    # Because colour aesthetic is reserved for strata in this plot,
    # we need to draw different grobs for different text colours.
    # This is a flagrant violation of the one variable per aesthetic mapping
    # principle that ggplot2 is built on but it works.

    for(this_color in unique(bar.text.color)){

      this_tieGrid=tieGrid[which(tieGrid$score_1 %in% scoreLevels[bar.text.color == this_color]),]


      if(grepl("^c[o]*[u]*[n]*[t]*$",bar.text)){

        out <- out +
          ggplot2::geom_text(data=this_tieGrid,
                             ggplot2::aes(x=(xmin+xmax)/2,
                                          y=(barMin + barMax)/2,
                                          label=sprintf("%d",xcount)
                             ),
                             size = bar.text.size,
                             color = this_color,
                             fontface = bar.text.face
          )+
          ggplot2::geom_text(data=this_tieGrid,
                             ggplot2::aes(y=(ymin+ymax)/2,
                                          x=(barMin + barMax)/2,
                                          label=sprintf("%d",ycount)
                             ),
                             size = bar.text.size,
                             color = this_color,
                             fontface = bar.text.face
          )

      } else if (grepl("^pr[o]*[p]*[o]*[r]*[t]*[i]*[o]*[n]*$",bar.text)){


        out <- out +
          ggplot2::geom_text(data=this_tieGrid,
                             ggplot2::aes(x=(xmin+xmax)/2,
                                          y=(barMin + barMax)/2,
                                          label=sprintf("%0.2f",xmax-xmin)
                             ),
                             size = bar.text.size,
                             color = this_color,
                             fontface = bar.text.face
          )+
          ggplot2::geom_text(data=this_tieGrid,
                             ggplot2::aes(y=(ymin+ymax)/2,
                                          x=(barMin + barMax)/2,
                                          label=sprintf("%0.2f",ymax-ymin)
                             ),
                             size = bar.text.size,
                             color = this_color,
                             fontface = bar.text.face
          )


      } else if (grepl("^pe[r]*[c]*[e]*[n]*[t]*[a]*[g]*[e]*$",bar.text)){


        out <- out +
          ggplot2::geom_text(data=this_tieGrid,
                             ggplot2::aes(x=(xmin+xmax)/2,
                                          y=(barMin + barMax)/2,
                                          label=sprintf("%2.2f%%",100*(xmax-xmin))
                             ),
                             size = bar.text.size,
                             color = this_color,
                             fontface = bar.text.face
          )+
          ggplot2::geom_text(data=this_tieGrid,
                             ggplot2::aes(y=(ymin+ymax)/2,
                                          x=(barMin + barMax)/2,
                                          label=sprintf("%2.2f%%",100*(ymax-ymin))
                             ),
                             size = bar.text.size,
                             color = this_color,
                             fontface = bar.text.face
          )

      } else if (grepl("^c[o]*[u]*[n]*[t]*.p[e]*[r]*[c]*[e]*[n]*[t]*[a]*[g]*[e]*",bar.text)) {

        out <- out +
          ggplot2::geom_text(data=this_tieGrid,
                             ggplot2::aes(x=(xmin+xmax)/2,
                                          y=(barMin + barMax)/2,
                                          label=sprintf("%d\n(%2.2f%%)",xcount,100*(xmax-xmin))
                             ),
                             size = bar.text.size,
                             color = this_color,
                             fontface = bar.text.face
          )+
          ggplot2::geom_text(data=this_tieGrid,
                             ggplot2::aes(y=(ymin+ymax)/2,
                                          x=(barMin + barMax)/2,
                                          label=sprintf("%d\n(%2.2f%%)",ycount,100*(ymax-ymin))
                             ),
                             size = bar.text.size,
                             color = this_color,
                             fontface = bar.text.face
          )

      } else if (grepl("^n[o]*[n]*[e]*$",bar.text)){

        # Do nothing if we were told not to print any numbers

      } else {
        stop("Unrecognised option for printNumbers")
      }

    }



    # Add strata labels for bars if needed.
    # Only do it if we're not panelling (where it's shown in the facet label)
    # AND there's more than one stratum.

    if(!panel & length(unique(x$strata))>1){

      segment_horiz_aes <- list(
        x=rlang::sym("position"),
        y=rlang::sym("position"),
        yend=rlang::sym("position")
      )
      segment_horiz_constant <- list(xend=0)

      segment_vert_aes <- list(
        x=rlang::sym("position"),
        y=rlang::sym("position"),
        xend=rlang::sym("position")
      )
      segment_vert_constant <- list(yend=0)

      strata_label_aes  <- list(
        label = rlang::sym("strata"),
        x=rlang::sym("position"),
        y=rlang::sym("position")
      )
      strata_label_constant <- list(size=strata.text.size,
                                    fill="white"
                                    )

      # Did we specify a type for the line?
      # Is it an aesthetic mapping?
      if(length(line.linetype) == 1){

        segment_horiz_constant <- c(segment_horiz_constant, linetype = line.linetype)
        segment_vert_constant <- c(segment_vert_constant, linetype = line.linetype)
        strata_label_constant <- c(strata_label_constant, linetype = line.linetype)

      } else if("ScaleDiscrete" %in% class(line.linetype)){

        segment_horiz_aes <- c(segment_horiz_aes, linetype = rlang::sym("strata"))
        segment_vert_aes <- c(segment_vert_aes, linetype = rlang::sym("strata"))
        strata_label_aes <- c(strata_label_aes, linetype = rlang::sym("strata"))

      }


      # Do the same with color
      # Did we specify a type for the line?
      # Is it an aesthetic mapping?



      if(length(line.color) == 1){

        segment_horiz_constant <- c(segment_horiz_constant, color = line.color)
        segment_vert_constant <- c(segment_vert_constant, color = line.color)
        strata_label_constant <- c(strata_label_constant, color = line.color)

      } else if("ScaleDiscrete" %in% class(line.color)){

        segment_horiz_aes <- c(segment_horiz_aes, color = rlang::sym("strata"))
        segment_vert_aes <- c(segment_vert_aes, color = rlang::sym("strata"))
        strata_label_aes <- c(strata_label_aes, color = rlang::sym("strata"))

      }

      segment_horiz_aes <- do.call(aes_wrapper,segment_horiz_aes)
      segment_vert_aes <- do.call(aes_wrapper,segment_vert_aes)
      strata_label_aes <- do.call(aes_wrapper,strata_label_aes)


      # Feeding through the position without naming it was causing problems
      strataLabel_data <- unique(tieGrid[,c("strata","barMin","barMax")])
      strataLabel_data$position <- (strataLabel_data$barMin + strataLabel_data$barMax)/2

      out <- out + do.call(function(...){ggplot2::geom_segment(data= strataLabel_data,
                                                               segment_horiz_aes,
                                                               ...,
                                                               show.legend = FALSE
                                                               )},
                           segment_horiz_constant
                           )+
                          do.call(function(...){ggplot2::geom_segment(data= strataLabel_data,
                                                                      segment_vert_aes,
                                                                      ...,
                                                                      show.legend = FALSE
                          )},
                          segment_vert_constant
                          ) +
                          do.call(function(...){ggplot2::geom_label(data= strataLabel_data,
                                                               strata_label_aes,
                                                               ...,
                                                               show.legend = FALSE
                          )},
                          strata_label_constant
                          )


    } # End if we're adding stratum labels


  } # End if bars are shown


  ## Main PP Plot ##################################################

  # Draw points

  # This whole section is stupid, but it prevents combinatorial
  # explosion from what inputs are provided so we're doing it anyway.

  # Construct the aesthetic mapping algorithmically
  # so we can inject the options we need

  path_aes <- list(x=rlang::sym("x"),
                   y=rlang::sym("y"))
  path_constant <- list()


  point_aes <- list(x=rlang::sym("x_mid"),
                   y=rlang::sym("y_mid"))
  point_constant <- list()

  confint_aes <- list(x=rlang::sym("x_lower"),
                      y=rlang::sym("y_lower"),
                      xend=rlang::sym("x_upper"),
                      yend=rlang::sym("y_upper"),
                      group=rlang::sym("i"))
  confint_constant <- list()

  # Did we specify a type for the line?
  # Is it an aesthetic mapping?
  if(length(line.linetype) == 1){

    path_constant <- c(path_constant,linetype=line.linetype)
    # point_constant <- c(point_constant,linetype=line.linetype)
    # confint_constant <- c(confint_constant,linetype=line.linetype)

  } else if("ScaleDiscrete" %in% class(line.linetype)){

    path_aes <- c(path_aes,linetype=rlang::sym("strata"))
    # point_aes <- c(point_aes,linetype=rlang::sym("strata"))
    # confint_aes <- c(confint_aes,linetype=rlang::sym("strata"))
  }


  # Do the same with color
  # Did we specify a type for the line?
  # Is it an aesthetic mapping?
  if(length(line.color) == 1){

    path_constant <- c(path_constant,color=line.color)
    point_constant <- c(point_constant,color=line.color)
    confint_constant <- c(confint_constant,color=line.color)

  } else if("ScaleDiscrete" %in% class(line.color)){

    path_aes <- c(path_aes,color=rlang::sym("strata"))
    point_aes <- c(point_aes,color=rlang::sym("strata"))
    confint_aes <- c(confint_aes,color=rlang::sym("strata"))
  }

  # This is even harder to read than I thought it would be.

  # Convert the lists of aesthetic mappings into aes() mappings.
  # so they're in the correct format for ggplot2
  path_aes <- do.call(aes_wrapper,path_aes)
  point_aes <- do.call(aes_wrapper,point_aes)
  confint_aes <- do.call(aes_wrapper,confint_aes)

  # Then we need to repeat this trick for each geom_object with each constant
  # aesthetic value
  # browser()

  out <- out + do.call(function(...){ggplot2::geom_path(data=allPoints,path_aes,...)},
                  path_constant
              ) +
              do.call(function(...){ggplot2::geom_point(data=odds,point_aes,...)},
                      point_constant
              )

  # Draw confidence intervals around the points
  if(confint){
    out <- out+ do.call(function(...){ggplot2::geom_segment(data=odds,confint_aes,...)},
                  confint_constant
      )
  }


  # Add scales if they were instructed.
  if("ScaleDiscrete" %in% class(line.color)){
    out <- out + line.color
  }


  if("ScaleDiscrete" %in% class(line.linetype)){
    out <- out + line.linetype
  }

  labs_list <- list(x=groupLevels[1],
                    y=groupLevels[2],
                    fill=scoreName)

  if("ScaleDiscrete" %in% class(line.linetype)){
    labs_list <- c(labs_list, linetype = strataName)
  }

  if("ScaleDiscrete" %in% class(line.color)){
    labs_list <- c(labs_list, color = strataName)
  }


  out <- out +
    do.call(function(...){ggplot2::labs(...)},labs_list)+
    ggplot2::theme_bw()+
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill="white"),
      aspect.ratio = 1
    )

  if(panel & length(unique(x$strata)) > 1){
    out <- out + ggplot2::facet_wrap(~strata, ncol = panel.nCol, dir = panel.dir)
  }

  out
}
