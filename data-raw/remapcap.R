
# Angus, Derek C., et al. "Effect of hydrocortisone on mortality and organ support in patients with severe COVID-19: the REMAP-CAP COVID-19 corticosteroid domain randomized clinical trial." Jama 324.13 (2020): 1317-1329.

# Points extracted from main figure of paper
df <- data.frame(
  shock.x = c(-0.899082569,
              0.130275229,
              1.119266055,
              3.117431193,
              4.126605505,
              2.00733945,
              5.11559633,
              6.104587156,
              7.133944954,
              8.102752294,
              9.111926606,
              10.12110092,
              11.13027523,
              12.09908257,
              13.08807339,
              14.09724771,
              15.1266055,
              16.11559633,
              17.12477064,
              18.15412844,
              19.10275229,
              20.11192661,
              21
  ),
  shock.y = c(0.260932945,
              0.501457726,
              0.508746356,
              0.529154519,
              0.551020408,
              0.551020408,
              0.572886297,
              0.587463557,
              0.609329446,
              0.616618076,
              0.631195335,
              0.644314869,
              0.688046647,
              0.71574344,
              0.750728863,
              0.781341108,
              0.807580175,
              0.842565598,
              0.887755102,
              0.912536443,
              0.948979592,
              0.985422741,
              1
  ),

  fixed.x = c(-0.979816514,
              0.029357798,
              0.998165138,
              1.987155963,
              3.016513761,
              4.04587156,
              5.014678899,
              6.023853211,
              7.012844037,
              8.022018349,
              9.011009174,
              10,
              11.0293578,
              11.99816514,
              13.00733945,
              14.01651376,
              15.00550459,
              16.0146789,
              16.99357798,
              18.00275229,
              19.00183486,
              20.01100917,
              20.97981651
  ),
  fixed.y = c(0.295918367,
              0.508746356,
              0.523323615,
              0.524781341,
              0.533527697,
              0.54664723,
              0.55393586,
              0.562682216,
              0.583090379,
              0.604956268,
              0.620991254,
              0.642857143,
              0.648688047,
              0.657434402,
              0.686588921,
              0.721574344,
              0.759475219,
              0.781341108,
              0.868804665,
              0.919825073,
              0.963556851,
              1,
              1
  ),


  uc.x = c(-1.100917431,
           -0.071559633,
           0.917431193,
           1.906422018,
           2.895412844,
           3.904587156,
           4.893577982,
           5.882568807,
           6.911926606,
           7.900917431,
           8.910091743,
           9.919266055,
           10.92844037,
           11.89724771,
           12.90642202,
           13.87522936,
           14.88440367,
           15.93394495,
           16.90275229,
           17.95229358,
           18.90091743,
           19.88990826,
           20.95963303
  ),
  uc.y = c(0.327988338,
           0.533527697,
           0.55393586,
           0.564139942,
           0.602040816,
           0.61516035,
           0.634110787,
           0.673469388,
           0.683673469,
           0.692419825,
           0.714285714,
           0.721574344,
           0.774052478,
           0.861516035,
           0.871720117,
           0.881924198,
           0.890670554,
           0.921282799,
           0.940233236,
           0.941690962,
           0.981049563,
           1,
           1
  )
)



df$uc.x <- round(df$uc.x)
df$shock.x <- round(df$shock.x)
df$fixed.x <- round(df$fixed.x)

targetdf <- df
targetdf$uc.y <- diff(c(0,targetdf$uc.y))
targetdf$shock.y <- diff(c(0,targetdf$shock.y))
targetdf$fixed.y <- diff(c(0,targetdf$fixed.y))

targetdf$uc.y <- round(targetdf$uc.y * 101)
targetdf$shock.y <- round(targetdf$shock.y * 141)
targetdf$fixed.y <- round(targetdf$fixed.y * 137)

target <- as.table(t(targetdf[,c("fixed.y","shock.y","uc.y")]))

rownames(target) <- c("fixed","shock","uc")
colnames(target) <- -1:21


# Convert from organ-free support days to survival

# Death rates are assumed exponential
death_uc <- 1/uniroot(function(x){df$uc.y[1] - pexp(21,rate=x)},lower = 1e-5,upper=1)$root
death_shock <- 1/uniroot(function(x){df$shock.y[1] - pexp(21,rate=x)},lower = 1e-5,upper=1)$root
death_fixed <- 1/uniroot(function(x){df$fixed.y[1] - pexp(21,rate=x)},lower = 1e-5,upper=1)$root



# Extract organ support free days

tmpdf <- df

tmpdf$shock.y <- tmpdf$shock.y - tmpdf$shock.y[1]
tmpdf$fixed.y <- tmpdf$fixed.y - tmpdf$fixed.y[1]
tmpdf$uc.y <- tmpdf$uc.y - tmpdf$uc.y[1]

tmpdf <- tmpdf[-1,]

tmpdf$shock.y <- tmpdf$shock.y/tmpdf$shock.y[nrow(tmpdf)]
tmpdf$fixed.y <- tmpdf$fixed.y/tmpdf$fixed.y[nrow(tmpdf)]
tmpdf$uc.y <- tmpdf$uc.y/tmpdf$uc.y[nrow(tmpdf)]

# Reverse this to get ICU LoS

tmpdf$shock.x <- 21 - tmpdf$shock.x
tmpdf$fixed.x <- 21 - tmpdf$fixed.x
tmpdf$uc.x <- 21 - tmpdf$uc.x

tmpdf$shock.y <- 1 - tmpdf$shock.y
tmpdf$fixed.y <- 1 - tmpdf$fixed.y
tmpdf$uc.y <- 1 - tmpdf$uc.y

tmpdf

# Assume events follow a weibull distribution
uc_pars <- optim(par = c(1,10), function(x){sum((tmpdf$uc.y - pweibull(tmpdf$uc.x,scale = x[1],shape = x[2]))^2)})$par
shock_pars <- optim(par = c(1,10), function(x){sum((tmpdf$shock.y - pweibull(tmpdf$shock.x,scale = x[1],shape = x[2]))^2)})$par
fixed_pars <- optim(par = c(1,10), function(x){sum((tmpdf$fixed.y - pweibull(tmpdf$fixed.x,scale = x[1],shape = x[2]))^2)})$par



# Generate data randomly, pick the one that closest fits the original distribution

set.seed(6843)

lapply(1:10000,function(i){

  gdf <- rbind(
    data.frame(group="uc",
               deathTime = rweibull(101,scale=death_uc,shape=1),
               dischargeTime = rweibull(101,scale=uc_pars[1]  ,shape=uc_pars[2])
    ),
    data.frame(group="shock",
               deathTime = rweibull(141,scale=death_shock,shape=1),
               dischargeTime = rweibull(141,scale=shock_pars[1]  ,shape=shock_pars[2])
    ),
    data.frame(group="fixed",
               deathTime = rweibull(137,scale=death_fixed,shape=1),
               dischargeTime = rweibull(137,scale=fixed_pars[1]  ,shape=fixed_pars[2])
    )
  )


  gdf$deathStatus <- ifelse(gdf$deathTime>21,0,1)
  gdf$dischargeStatus <- ifelse(gdf$dischargeTime>21,0,1)
  gdf$dischargeStatus <- ifelse(gdf$dischargeTime>gdf$deathTime, 0 ,gdf$dischargeStatus)

  gdf$dischargeTime <- ifelse(gdf$dischargeTime>gdf$deathTime,gdf$deathTime,gdf$dischargeTime)
  gdf$dischargeTime <- ifelse(gdf$dischargeTime>21,21,gdf$dischargeTime)
  gdf$deathTime <- ifelse(gdf$deathTime>21,21,gdf$deathTime)


  gdf$score <- ifelse(gdf$deathStatus==1,-1,21-floor(gdf$dischargeTime))
  gdf$score <- factor(gdf$score,levels=-1:21)

  gdf
}) -> draws

score <- sapply(draws,function(x){sum((table(x$group,x$score)-target)^2)})
min(score)

remapcap <- draws[[which(score==min(score))]]

remapcap$group <- factor(remapcap$group,levels = c("uc","shock","fixed"))

usethis::use_data(remapcap, overwrite = TRUE)
