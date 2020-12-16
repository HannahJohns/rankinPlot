
grottaBar <- function(x,groupName,scoreName,width=0.05){

  x <- as.data.frame(x)

  freqName <- setdiff(colnames(x),c(groupName,scoreName))
  x <- data.frame(group=x[,groupName],score=x[,scoreName],n=x[,freqName])

  x


  x$colwidth = ifelse(x$group == x$group[1],
                      as.numeric(x$group) + (0.5-width),
                      as.numeric(x$group) - (0.5-width)
  )


  x[x$group == x$group[1],"n"] <- x[x$group == x$group[1],"n"]/sum(x[x$group == x$group[1],"n"])
  x[x$group != x$group[1],"n"] <- x[x$group != x$group[1],"n"]/sum(x[x$group != x$group[1],"n"])



  ggplot2::ggplot(x,ggplot2::aes(x=group,y=n,fill=score))+
    ggplot2::geom_bar(color="black",position = "stack",stat="identity")+
    ggplot2::geom_line(ggplot2::aes(x=colwidth),position = ggplot2::position_stack())+
    ggplot2::annotate(geom = "segment", y = 0, yend = 0, x = 1.5-width, xend = 1.5+width)

}


