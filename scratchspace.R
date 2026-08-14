library(rankinPlot)


rm(list=ls())
x <- table(group=alteplase$treat,mrs = alteplase$mRS, time = alteplase$time)

pp_plot(x,
        groupName =  "group",
        scoreName = "mrs",
        strataName = "time"
)

grottaBar(x,
        groupName =  "group",
        scoreName = "mrs",
        strataName = "time"
)




library(tidyverse)


# Using the same distribution as before

# p0 <- ((1:6)/sum(1:6))

N <- 700

p0 <- 1:6
p0 <- p0/sum(p0)
p0 <- p0^(1/3)
p0 <- p0/sum(p0)

rbind(c(1, 0.1, 0,   0,    0,   0),
      c(0, 0.9, 0.3, 0,    0,   0),
      c(0, 0,   0.7, 0.4,  0,   0),
      c(0, 0,   0,   0.55, 0.4, 0),
      c(0, 0,   0,   0.15, 0.3, 0),
      c(0, 0,   0,   0,    0.3, 1)
) -> M

p1 <- round(M %*% p0,3)
p0 <- round(p0,3)


p1 <- c(p1)
p0 <- c(p0)

p1 <- p1/sum(p1)
p0 <- p0/sum(p0)




##### Panel 1: Contour plot ##################################################

r <- unique(sort(outer(c(1,1.5,2,2.5,5,10,20,50,100,1000),c(-1,1),FUN = "^")))

contour_df <- do.call("rbind",
                      lapply(r, function(odds){
                        out <- data.frame(r=odds,qc=seq(0,1,length.out=501))
                        out$qt <- out$r*out$qc/((out$r-1)*out$qc + 1)
                        out
                      })
)

contour_df %>%
  mutate(dev = ifelse(r==1,
                      (qc-0.5)^2+(qt-0.5)^2,
                      -(qc-qt)^2)) %>%
  group_by(r) %>%
  filter(dev==min(dev)) -> contour_df_label

contour_df %>%
  ggplot(aes(x=qc,y=qt,group=factor(r)))+
  geom_line()+
  geom_label(data=contour_df_label,aes(label=sprintf("%0.2f",r)),label.size = NA,size=3.5)+
  theme_bw()+
  labs(x="Probability of good outcome in control group",
       y="Probability of good outcome in treatment group"
  )+
  theme(aspect.ratio = 1)

#### Panel 2: 1 + Boxes showing mRS distribution ##############################

posx <- cbind(rankin0 = 0:5, xmin=cumsum(p0)-p0,xmax=cumsum(p0))
posy <- cbind(rankin1 = 0:5, ymin=cumsum(p1)-p1,ymax=cumsum(p1))

posx <- lapply(1:nrow(posx),function(i){posx[i,]})
posy <- lapply(1:nrow(posy),function(i){posy[i,]})

posGrid <- expand.grid(posx,posy)

as.data.frame(cbind(
  do.call("rbind",posGrid[[1]]),
  do.call("rbind",posGrid[[2]])
)) -> posGrid



xlabel <- data.frame(labels=0:5, xpos = unique(apply(posGrid[,c("xmin","xmax")],1,mean)))
ylabel <- data.frame(labels=0:5, ypos = unique(apply(posGrid[,c("ymin","ymax")],1,mean)))


splitprop <- outer(p0,p1)
splitProp <- sum(splitprop[lower.tri(splitprop)])/(sum(splitprop[lower.tri(splitprop)])+sum(splitprop[upper.tri(splitprop)]))

tieGrid <- posGrid[which(posGrid$rankin0==posGrid$rankin1),]


splitTieDf <- do.call("rbind",lapply(1:nrow(tieGrid),function(i){

  dx <- tieGrid[i,"xmax"]-tieGrid[i,"xmin"]
  dy <- tieGrid[i,"ymax"]-tieGrid[i,"ymin"]


  splitprop <- outer(p0,p1)
  splitProp <- sum(splitprop[lower.tri(splitprop)])/(sum(splitprop[lower.tri(splitprop)])+sum(splitprop[upper.tri(splitprop)]))

  #   Split ties diagonally
  #   if(splitProp >= 0.5){
  #
  #     tmp <- rbind(
  #       c(tieGrid[i,"xmin"],tieGrid[i,"ymax"]-dy*sqrt(2*(1-splitProp))),
  #       c(tieGrid[i,"xmin"]+dx*sqrt(2*(1-splitProp)),tieGrid[i,"ymax"])
  #     )
  #
  #   } else {
  #
  #     tmp <- rbind(
  #       c(tieGrid[i,"xmax"]-dx*sqrt(2*(splitProp)),tieGrid[i,"ymin"]),
  #       c(tieGrid[i,"xmax"],tieGrid[i,"ymin"]+dy*sqrt(2*(splitProp)))
  #     )
  #   }

  # Split ties horizontally

  tmp <- rbind(
    c(tieGrid[i,"xmin"],tieGrid[i,"ymin"]+splitProp*(tieGrid[i,"ymax"]-tieGrid[i,"ymin"])),
    c(tieGrid[i,"xmax"],tieGrid[i,"ymin"]+splitProp*(tieGrid[i,"ymax"]-tieGrid[i,"ymin"]))
  )

  colnames(tmp) <- c("x","y")
  cbind(mRS=tieGrid[i,"rankin0"]+c(0.2,0.3),tmp)

}))


posGrid <- do.call("rbind",lapply(1:nrow(tieGrid),function(i){
  cbind(mRS=tieGrid[i,"rankin0"]+c(0.1,0.4),
        rbind(
          c(x=tieGrid[i,"xmin"],y=tieGrid[i,"ymin"]),
          c(x=tieGrid[i,"xmax"],y=tieGrid[i,"ymax"])
        )
  )
}))

allPoints <- cbind(as.data.frame(rbind(posGrid,splitTieDf)))

allPoints <- allPoints[order(allPoints$mRS),]


ggplot()+
  # geom_polygon(data=allPoints_area,
  #              fill="blue",alpha=0.2,
  #           aes(x=x,y=y))+
  geom_rect(data=tieGrid,
            fill="gray",
            alpha=0.1,
            color="#999999",
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                group=paste(rankin0,rankin1)
            )
  )+
  geom_line(data=contour_df,
            color="dark gray", linetype="dashed",
            aes(x=qc,y=qt,group=paste(r)))+
  geom_abline(slope=1,intercept=0, color="black",size=1)+
  geom_label(data=contour_df_label,
             label.size = NA,
             label.padding = unit(0, "lines"),
             hjust=0,vjust=1,size=3.5,
             aes(x=qc,y=qt,label=sprintf("%0.2f",r)),
             position=position_dodge()
  )+
  # geom_point(data=tieGrid,
  #            aes(x=xmax,y=ymax))+
  geom_rect(data=tieGrid,
            ymin=-0.1,ymax=0,color="black",
            aes(xmin=xmin,xmax=xmax,fill=factor(rankin0)))+
  geom_rect(data=tieGrid,
            xmin=-0.1,xmax=0,color="black",
            aes(ymin=ymin,ymax=ymax,fill=factor(rankin0)))+
  # geom_segment(data=tieGrid,linetype="dashed",color="grey",
  #              x=0,
  #              aes(y=ymax,xend=xmax,yend=ymax))+
  # geom_segment(data=tieGrid,linetype="dashed",color="grey",
  #              y=0,
  #              aes(x=xmax,xend=xmax,yend=ymax))+
  geom_text(data=tieGrid, aes(x=(xmin+xmax)/2,y=-0.05,label=sprintf("%0.2f",xmax-xmin)))+
  geom_text(data=tieGrid, aes(y=(ymin+ymax)/2,x=-0.05,label=sprintf("%0.2f",ymax-ymin)))+
  scale_fill_brewer(palette="RdYlGn",direction=-1)+
  labs(x="Control mRS distribution",y="Treatment mRS distribution",fill="mRS")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1
  )



# Panel 3: 1 + dots and confidence intervals ###################################

# Show contour lines for actual odds values


oddsCurve <- function(x,r) r*x/((r-1)*x + 1)
oddsCurve_x <- function(x,r) r/((r-1)*x+1)^2 # First derivative with respect to x


# Make up some confidence intervals
tieGrid$estimate <- tieGrid$ymax*(1-tieGrid$xmax)/(tieGrid$xmax*(1-tieGrid$ymax))

# Get halfwidth, etc. based on wald normal approximation
tieGrid$halfWidth <- with(tieGrid,1.96*sqrt(1/N)*sqrt(1/xmax+1/ymax+1/(1-xmax)+1/(1-ymax)))
tieGrid$lower <- exp(log(tieGrid$estimate) - tieGrid$halfWidth)
tieGrid$upper <- exp(log(tieGrid$estimate) + tieGrid$halfWidth)

do.call("rbind",lapply(1:(nrow(tieGrid)-1),function(i){

  print(i)

  x_mid <- tieGrid[i,"xmax"]
  y_mid <- oddsCurve(x_mid,tieGrid[i,"estimate"])

  # For each point, find intersection between lower/upper odds curves
  # and a straight line that's normal to the curve at that point

  # We can just rootfind this

  x_lower <- uniroot(function(x_lower){

    m <- -1/oddsCurve_x(x_mid,tieGrid[i,"estimate"])

    normalLineVal <- oddsCurve(x_mid,tieGrid[i,"estimate"]) + m*(x_lower-x_mid)

    CurveVal <- oddsCurve(x_lower,tieGrid[i,"lower"])

    CurveVal - normalLineVal

  },interval = c(0,1))$root

  y_lower <- oddsCurve(x_lower,tieGrid[i,"lower"])

  x_upper <- uniroot(function(x_upper){

    m <- -1/oddsCurve_x(x_mid,tieGrid[i,"estimate"])

    normalLineVal <- oddsCurve(x_mid,tieGrid[i,"estimate"]) + m*(x_upper-x_mid)

    CurveVal <- oddsCurve(x_upper,tieGrid[i,"upper"])

    CurveVal - normalLineVal

  },interval = c(1e-8,1-1e-8))$root

  y_upper <- oddsCurve(x_upper,tieGrid[i,"upper"])

  c(i=i,
    odds_lower=unname(tieGrid[i,"lower"]),
    x_lower=unname(x_lower),
    y_lower=unname(y_lower),
    odds_mid=unname(tieGrid[i,"estimate"]),
    x_mid=unname(x_mid),
    y_mid=unname(y_mid),
    odds_upper=unname(tieGrid[i,"upper"]),
    x_upper=unname(x_upper),
    y_upper=unname(y_upper)
  )

}) ) -> odds

odds <- as.data.frame(odds)


contour_df_odds <- do.call("rbind",
                           lapply(odds$odds_mid, function(odds){
                             out <- data.frame(r=odds,qc=seq(0,1,length.out=501))
                             out$qt <- out$r*out$qc/((out$r-1)*out$qc + 1)
                             out
                           })
)


ggplot()+
  # geom_line(data=propOdds,
  #           color="blue",size=1,linetype="dashed",
  #           aes(x=x,y=y)
  # )+
  geom_path(data=contour_df_odds,
            aes(x=qc,y=qt,group=r), color="dark gray", size=0, linetype="dashed"
  )+
  geom_segment(data=odds,
               aes(x=x_lower,y=y_lower,xend=x_upper,yend=y_upper, group=i))+
  geom_point(data=odds,
             aes(x=x_mid,y=y_mid))+
  geom_abline(slope=1,intercept=0)+
  geom_label(data=odds,aes(x=x_lower+0.005,y=y_lower,
                           label=sprintf("mRS%s OR %0.2f (%0.2f-%0.2f)",ifelse(i==1,0,sprintf("0-%d",i-1)),
                                         odds_mid, odds_lower, odds_upper)),
             hjust=0, size=3)+
  labs(x="Probability of good outcome in control group",y="Probability of good outcome in treatment group")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        aspect.ratio = 1
  )

# Panel 4: Final Plot ##########################################################

rbind(
  tieGrid %>%
    mutate(weight=xmax-xmin, group=1) %>%
    select(rankin=rankin0,group,weight=weight),
  tieGrid %>%
    mutate(weight=ymax-ymin, group=0) %>%
    select(rankin=rankin0,group,weight=weight)
) %>% mutate(weight=weight*N,
             rankin=factor(rankin)) -> propodds_df

MASS::polr(rankin~group,weights = propodds_df$weight,data=propodds_df) %>% coefficients() %>% exp() %>% unname() -> commonOdds

commonOdds_df <- data.frame(qc = seq(0,1,length.out=201))
commonOdds_df$qt <- oddsCurve(commonOdds_df$qc,commonOdds)

ggplot()+
  # geom_polygon(data=allPoints_area,
  #              fill="blue",alpha=0.2,
  #           aes(x=x,y=y))+
  geom_rect(data=tieGrid,
            fill="gray",
            alpha=0.1,
            color="#999999",linetype="dashed",
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                group=paste(rankin0,rankin1)
            )
  )+
  geom_abline(slope=1,intercept=0, color="black",size=0)+
  geom_segment(data=odds,
               aes(x=x_lower,y=y_lower,xend=x_upper,yend=y_upper, group=i))+
  geom_point(data=odds,
             aes(x=x_mid,y=y_mid))+
  geom_rect(data=tieGrid,
            ymin=-0.1,ymax=0,color="black",
            aes(xmin=xmin,xmax=xmax,fill=factor(rankin0)))+
  geom_rect(data=tieGrid,
            xmin=-0.1,xmax=0,color="black",
            aes(ymin=ymin,ymax=ymax,fill=factor(rankin0)))+
  geom_line(data=commonOdds_df,aes(x=qc,y=qt),color="blue")+
  geom_text(data=tieGrid, aes(x=(xmin+xmax)/2,y=-0.05,label=sprintf("%0.2f",xmax-xmin)))+
  geom_text(data=tieGrid, aes(y=(ymin+ymax)/2,x=-0.05,label=sprintf("%0.2f",ymax-ymin)))+
  geom_label(data=odds,aes(x=x_lower+0.005,y=y_lower,
                           label=sprintf("mRS%s OR %0.2f (%0.2f-%0.2f)",ifelse(i==1,0,sprintf("0-%d",i-1)),
                                         odds_mid, odds_lower, odds_upper)),
             hjust=0, size=3)+
  scale_fill_brewer(palette="RdYlGn",direction=-1)+
  labs(x="Control mRS distribution",y="Treatment mRS distribution",fill="mRS")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1
  )


# Panel 5: Show CI for proportional odds model #################################


MASS::polr(rankin~group,weights = propodds_df$weight,data=propodds_df) %>%
  (function(x){
    out <- c(coef(x),confint(x))
    names(out) <- c("estimate","lower","upper")
    out
  }) %>% exp() -> commonOdds

commonOdds_df <- data.frame(qc = seq(0,1,length.out=201))
commonOdds_df$estimate <- oddsCurve(commonOdds_df$qc,commonOdds["estimate"])
commonOdds_df$lower <- oddsCurve(commonOdds_df$qc,commonOdds["lower"])
commonOdds_df$upper <- oddsCurve(commonOdds_df$qc,commonOdds["upper"])
# commonOdds_df %>% gather("estimate","qt",estimate,lower,upper) -> commonOdds_df

ggplot()+
  # geom_polygon(data=allPoints_area,
  #              fill="blue",alpha=0.2,
  #           aes(x=x,y=y))+
  geom_ribbon(data=commonOdds_df,aes(x=qc,ymin=lower,ymax=upper),fill="#addcff")+
  geom_line(data=commonOdds_df,aes(x=qc,y=estimate),color="blue")+
  geom_rect(data=tieGrid,
            fill="gray",
            alpha=0.1,
            color="#999999",linetype="dashed",
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                group=paste(rankin0,rankin1)
            )
  )+
  geom_abline(slope=1,intercept=0, color="black",size=0)+
  geom_segment(data=odds,
               aes(x=x_lower,y=y_lower,xend=x_upper,yend=y_upper, group=i))+
  geom_point(data=odds,
             aes(x=x_mid,y=y_mid))+
  geom_rect(data=tieGrid,
            ymin=-0.1,ymax=0,color="black",
            aes(xmin=xmin,xmax=xmax,fill=factor(rankin0)))+
  geom_rect(data=tieGrid,
            xmin=-0.1,xmax=0,color="black",
            aes(ymin=ymin,ymax=ymax,fill=factor(rankin0)))+
  geom_text(data=tieGrid, aes(x=(xmin+xmax)/2,y=-0.05,label=sprintf("%0.2f",xmax-xmin)))+
  geom_text(data=tieGrid, aes(y=(ymin+ymax)/2,x=-0.05,label=sprintf("%0.2f",ymax-ymin)))+
  scale_fill_brewer(palette="RdYlGn",direction=-1)+
  labs(x="Control mRS distribution",y="Treatment mRS distribution",fill="mRS")+
  geom_label(data=odds,aes(x=x_lower+0.005,y=y_lower,
                           label=sprintf("mRS%s OR %0.2f (%0.2f-%0.2f)",ifelse(i==1,0,sprintf("0-%d",i-1)),
                                         odds_mid, odds_lower, odds_upper)),
             hjust=0, size=3)+
  geom_abline(linetype="dashed")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1
  )

# Panel 6: Shade area corresponding to win proportion ##########################

allPoints <- allPoints[order(allPoints$mRS),]
allPoints_area <- rbind(allPoints,
                        c(mRS=10,x=1,y=0))



ggplot()+
  geom_polygon(data=allPoints_area,
               fill="blue",alpha=0.2,
               aes(x=x,y=y))+
  geom_path(data=allPoints,aes(x=x,y=y))+
  # geom_ribbon(data=commonOdds_df,aes(x=qc,ymin=lower,ymax=upper),fill="dark blue",alpha=0.2)+
  geom_line(data=commonOdds_df,aes(x=qc,y=estimate),color="blue")+
  geom_rect(data=tieGrid,
            fill="gray",
            alpha=0.1,
            color="#999999",linetype="dashed",
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                group=paste(rankin0,rankin1)
            )
  )+
  geom_abline(slope=1,intercept=0, color="black",size=0)+
  geom_segment(data=odds,
               aes(x=x_lower,y=y_lower,xend=x_upper,yend=y_upper, group=i))+
  geom_point(data=odds,
             aes(x=x_mid,y=y_mid))+
  geom_rect(data=tieGrid,
            ymin=-0.1,ymax=0,color="black",
            aes(xmin=xmin,xmax=xmax,fill=factor(rankin0)))+
  geom_rect(data=tieGrid,
            xmin=-0.1,xmax=0,color="black",
            aes(ymin=ymin,ymax=ymax,fill=factor(rankin0)))+
  geom_text(data=tieGrid, aes(x=(xmin+xmax)/2,y=-0.05,label=sprintf("%0.2f",xmax-xmin)))+
  geom_text(data=tieGrid, aes(y=(ymin+ymax)/2,x=-0.05,label=sprintf("%0.2f",ymax-ymin)))+
  geom_label(data=odds,aes(x=x_lower+0.005,y=y_lower,
                           label=sprintf("mRS%s OR %0.2f (%0.2f-%0.2f)",ifelse(i==1,0,sprintf("0-%d",i-1)),
                                         odds_mid, odds_lower, odds_upper)),
             hjust=0, size=3)+
  scale_fill_brewer(palette="RdYlGn",direction=-1)+
  labs(x="Control mRS distribution",y="Treatment mRS distribution",fill="mRS")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1
  )


