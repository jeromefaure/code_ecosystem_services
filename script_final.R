#SCRIPT PAPER COMBINING INCENTIVES WITH COLLECTIVE ACTION TO PROVIDE POLLINATION AND A BUNDLE OF ECOSYSTEM SERVICES IN FARMLAND
# FAURE ET AL.
install.packages('ggplot2','fmsb','scales','gridExtra','cowplot','readxl','reshape2','ggpubr','xlsx','gtable','grid','nloptr')

library(ggplot2)
library(fmsb)
library(scales)
library(gridExtra)
library(cowplot)
library(readxl)
library(reshape2)
library(ggpubr)

library(xlsx)


library(gtable)
library(grid)
library(nloptr)

##### RADARCHART FUNCTION -----
radarchart2 <- function (df, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6, 
                         plwd = 1, pdensity = NULL, pangle = 45, pfcol = NA, cglty = 3, 
                         cglwd = 1, cglcol = "navy", axislabcol = "blue", vlabcol = "black", title = "", 
                         maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL, 
                         vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL, 
                         palcex = NULL, ...) 
{
  if (!is.data.frame(df)) {
    cat("The data must be given as dataframe.\n")
    return()
  }
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE, 
       axes = FALSE, xlab = "", ylab = "", main = title, asp = 1, 
       ...)
  theta <- seq(90, 450, length = n + 1) * pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  for (i in 0:seg) {
    polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + 
                                                             CGap), lty = cglty, lwd = cglwd, border = cglcol)
    if (axistype == 1 | axistype == 3) 
      CAXISLABELS <- paste(i/seg * 100, "(%)")
    if (axistype == 4 | axistype == 5) 
      CAXISLABELS <- sprintf("%3.2f", i/seg)
    if (!is.null(caxislabels) & (i < length(caxislabels))) 
      CAXISLABELS <- caxislabels[i + 1]
    if (axistype == 1 | axistype == 3 | axistype == 4 | 
        axistype == 5) {
      if (is.null(calcex)) 
        text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
             col = axislabcol)
      else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
                col = axislabcol, cex = calcex)
    }
  }
  if (centerzero) {
    arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty, 
           length = 0, col = cglcol)
  }
  else {
    arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy * 
             1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
  }
  PAXISLABELS <- df[1, 1:n]
  if (!is.null(paxislabels)) 
    PAXISLABELS <- paxislabels
  if (axistype == 2 | axistype == 3 | axistype == 5) {
    if (is.null(palcex)) 
      text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
    else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol, 
              cex = palcex)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels)) 
    VLABELS <- vlabels
  if (is.null(vlcex)) 
    text(xx * 1.2, yy * 1.2, VLABELS, col = vlabcol)
  else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex, col = vlabcol)
  series <- length(df[[1]])
  SX <- series - 2
  if (length(pty) < SX) {
    ptys <- rep(pty, SX)
  }
  else {
    ptys <- pty
  }
  if (length(pcol) < SX) {
    pcols <- rep(pcol, SX)
  }
  else {
    pcols <- pcol
  }
  if (length(plty) < SX) {
    pltys <- rep(plty, SX)
  }
  else {
    pltys <- plty
  }
  if (length(plwd) < SX) {
    plwds <- rep(plwd, SX)
  }
  else {
    plwds <- plwd
  }
  if (length(pdensity) < SX) {
    pdensities <- rep(pdensity, SX)
  }
  else {
    pdensities <- pdensity
  }
  if (length(pangle) < SX) {
    pangles <- rep(pangle, SX)
  }
  else {
    pangles <- pangle
  }
  if (length(pfcol) < SX) {
    pfcols <- rep(pfcol, SX)
  }
  else {
    pfcols <- pfcol
  }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1, 
                                                         ] - df[2, ]) * seg/(seg + CGap)
    if (sum(!is.na(df[i, ])) < 3) {
      cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, 
                  df[i, ]))
    }
    else {
      for (j in 1:n) {
        if (is.na(df[i, j])) {
          if (na.itp) {
            left <- ifelse(j > 1, j - 1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left > 1, left - 1, n)
            }
            right <- ifelse(j < n, j + 1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right < n, right + 1, 
                              1)
            }
            xxleft <- xx[left] * CGap/(seg + CGap) + 
              xx[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            yyleft <- yy[left] * CGap/(seg + CGap) + 
              yy[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            xxright <- xx[right] * CGap/(seg + CGap) + 
              xx[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + 
                                                                                            CGap)
            yyright <- yy[right] * CGap/(seg + CGap) + 
              yy[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + 
                                                                                            CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft
              yytmp <- yyleft
              xxleft <- xxright
              yyleft <- yyright
              xxright <- xxtmp
              yyright <- yytmp
            }
            xxs[j] <- xx[j] * (yyleft * xxright - yyright * 
                                 xxleft)/(yy[j] * (xxright - xxleft) - 
                                            xx[j] * (yyright - yyleft))
            yys[j] <- (yy[j]/xx[j]) * xxs[j]
          }
          else {
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                 j]) * seg/(seg + CGap)
          yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                 j]) * seg/(seg + CGap)
        }
      }
      if (is.null(pdensities)) {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], col = pfcols[i - 
                                                                                                      2])
      }
      else {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], density = pdensities[i - 
                                                                                                              2], angle = pangles[i - 2], col = pfcols[i - 
                                                                                                                                                         2])
      }
      points(xx * scale, yy * scale, pch = ptys[i - 2], 
             col = pcols[i - 2])
    }
  }
}







# LANDSCAPE CALIBRATION ----
nfarmers = 6 ; mbeekeeper = 1 ; farmer_acreage = 95

tot_area = nfarmers * farmer_acreage


##################################################### ECOLOGICAL FUNCTIONS#####################################################

# x1 = OSR area / x2 = pesticide use variable / x3 = beehive number / x4 = wheat area / x5 = grassland area

#carrying capacities bees
k_honeybees = 18900 #k_honeybees = phi (survival rate = 0.63) * dH (density per hive =30000); see Allier (2012)
k_wildbees = 5000 #see Montoya 2019 for carrying capacity and model of population (negative linear)/ 
# and see Kleczkowski et al. (2017) for further references (notably Osborne et al. 2008)
max_beehives = 5/7 #max beehive for 1 ha / Bova (2012)

damage = 0.5 ; delta = 0.5 # from the data set, Chambers et al. (2019), Wintermanter et al. (2020)
damage_funct = function(x1,x2) {1 - delta * x1*x2^damage} 


#FUNCTION BEES
#Bees abundance per ha
# (1) absolute population
Bees = function(x1,x2,x3,x5){
  x3 * k_honeybees + x5 * k_wildbees *damage_funct(x1,x2)} #the wildbees suffer a negative impact of pesticides, see Woodcock et al. (2016)

# (2) reduced population / assumption : Bees(0,0,max_beehives) being the maximum of the function
Bees_reduced = function(x1,x2,x3,x5){
  Bees(x1,x2,x3,x5)/Bees(0,0,max_beehives,1)
}



##################################################### ECONOMIC FUNCTIONS#####################################################



#AGRICULTURAL PRODUCTION ----

#OSR PRODUCTION

mean_yield_OSR = 3.1 #Average OSR yield observed in ZAPVS 2013-2016 (from the data set)

#(1) Partial yield pollinators
alpha1 = 1.7 # maximal OSR yield derived from pollination, see Perrot et al. (2018) with their results from the data set
beta1 = 0.3 #Half-saturation constant of bees (if 0.3, 30% in bee abundance allows to reach the half-maximum of maximal partial yield)

f1_1 = function(x1,x2,x3,x5){
  alpha1 *  (Bees_reduced(x1,x2,x3,x5)/(Bees_reduced(x1,x2,x3,x5) + beta1))} # Functional form = Montoya et al. (2019)

#(2) Partial yield agrochemicals
alpha2 = 1 # maximal OSR yield aderived from pesticides application, see Fernandez-Cornejo et al. (1998)
beta2 = 0.3 #Half-saturation constant of pesticides, see Gaba et al. (2016)

f1_2 = function(x2){
  alpha2 *  (x2/(x2+beta2))
}

#(3) Partial yield independent from both agrochemicals and bees

f1_3 = mean_yield_OSR * 0.5

#OSR PRODUCTION TOTAL
F_OSR = function(x1,x2,x3,x5){
  x1 * (f1_1(x1,x2,x3,x5) + f1_2(x2) + f1_3)
}

rho_OSR = function(x1,x2,x3,x5){
  f1_1(x1,x2,x3,x5) + f1_2(x2) + f1_3
}


### NON-POLL DEP PRODUCTIONS 

elas = 0.55 #elasticity of agricultural production ; Ory (2020)


#WHEAT GROSS MARGIN

f_W = 550 #gross margin wheat ; source : Cerfrance (2018)

F_W = function(x4){x4^elas*f_W}

#GRASSLANDS GROSS MARGIN

f_G =400 #gross margin grasslands ; source : PEREL (2015)

F_G = function(x5){x5^elas*f_G}


#MAIZE PRODUCTION

#f_M = 360 #gross margin hay

#F_M = function(x6){x6^elas*f_M}


#PROFIT FUNCTION - FARMERS
p_OSR = 350  #OSR output price | Visionet (2020) (taking average price since 2010)
om_OSR_others = 550 #marginal cost of land and others for OSR  Guillermet (2015)
om_OSR_pest = 150 #marginal cost of pesticides for OSR / from gross margin data in the data set && Guillermet (2015)

#Profit function for farmer
pi1 = function(x1,x2,x3,x4,x5){  p_OSR * F_OSR(x1,x2,x3,x5) + F_W(x4) + F_G(x5)  - x1*om_OSR_others - x1*x2*om_OSR_pest} 

#Profit function with tax
pi1_tax = function(x1,x2,x3,x4,x5){  p_OSR * F_OSR(x1,x2,x3,x5) + F_W(x4) + F_G(x5)   - x1*om_OSR_others - x1*x2*om_OSR_pest - x2 * t1 * x1}

pi1_PT = function(x1,x2,x3,x4,x5,tax){  p_OSR * F_OSR(x1,x2,x3,x5) + F_W(x4) + F_G(x5)   - x1*om_OSR_others - x1*x2*om_OSR_pest - x2 * tax * x1}


#HONEY PRODUCTION ----

#cobb-douglas honey production
K = 20 ; gamma_1 = 0.3
Y = function(x1) K * x1^gamma_1


gamma = 0.4 #beehives elasticity/  see Vaziritabar, S. ;  Oshidari, S. ;  Aghamirkarimi, A. 2014 elasticity at 0.35 for capital ////
#0.36 to 0.4 in Siebert

#Honey production function / see Siebert (1980)
F2 = function(x1,x2,x3){
  y = x3^gamma * (Y(x1)  * damage_funct(x1,x2) )
  return(y) }


p2 = 7 #honey price
om3 = 100 #marginal cost of beehives

#beekeeper's profit function
pi2 = function(x1,x2,x3){ 
  y = p2 * F2(x1,x2,x3) - om3*x3 
  return(y)}

pi2_s1 = function(x1,x2,x3){ 
  y = p2 * F2(x1,x2,x3) - om3*x3 + s1_lim*x3
  return(y)}

pi2_s2 = function(x1,x2,x3){ 
  y = p2 * F2(x1,x2,x3) - om3*x3  + s2_lim*F2(x1,x2,x3)
  return(y)}


pi2_HS = function(x1,x2,x3,S){ 
  y = p2 * F2(x1,x2,x3) - om3*x3 + S*x3
  return(y)}

pi2_PS = function(x1,x2,x3,S){ 
  y = p2 * F2(x1,x2,x3) - om3*x3  + S*F2(x1,x2,x3)
  return(y)}





#BEST RESPONSE FUNCTIONS ----
x3_max = function(x1,x2){ (om3 / (gamma * Y(x1) * damage_funct(x1,x2) * p2))^(1/(gamma - 1))}
x3_max_HS = function(x1,x2) {( (om3 - s1_lim)  / (gamma * Y(x1) * damage_funct(x1,x2) * p2))^(1/(gamma - 1))}
x3_max_PS = function(x1,x2) { ( om3   / (gamma * Y(x1) * damage_funct(x1,x2) * (p2 + s2_lim)))^(1/(gamma - 1))}



########################################## INDICATORS (ECOSYSTEM SERVICES AND ECONOMIC)  ----

#CALIBRATION ES INDICATORS

toxicity_relative_index = 3.7/13 


#ECOSYSTEM SERVICES INDICATORS
es_1_OSR =  6940000 #calorific power of OSR

es_1_G = 3417000 #calorific power of grasslands
Y_grass = 5 #yield of grasslands (t/ha)

es_1_W = 4350000  #calorific power of wheat
Y_wheat = 7 #yield of wheat (t/ha)

es_1_honey = 3270 #calorific power of honey
z = 0.25 #slope of the log of species richness

#function of ES magnitude
ecosystem_services = function(x1,x2,x3,x4,x5){
  y1 = F_OSR(x1,x2,x3,x5) * es_1_OSR + Y_wheat * es_1_W + Y_grass * es_1_G + F2(x1,x2,x3)* es_1_honey #food provision
  y2 = x5^z #species abundance (z calibration  Crawley and Harral 2001)
  y3 = Bees_reduced(x1,x2,x3,x5) #pollination (pollinators abundance)
  y4 = 1 - x1 * x2 - toxicity_relative_index* x4  #water quality - toxicity pesticides
  y4bis = 1 - x1 - x4 #water quality - nutrients
  y5 = x5 * k_wildbees *damage_funct(x1,x2) #wild pollinators
  return(c(y1,y2,y3,y4,y4bis,y5))}



########################################## PARETO OPTIMUM ----

Welfare_opt = function(x){return(- pi1(x[1],x[2],x[3],x[4],x[5]) - pi2(x[1],x[2],x[3]))}
Welfare = function(x){return(pi1(x[1],x[2],x[3],x[4],x[5]) + pi2(x[1],x[2],x[3]))}

constr_land_use = function(x){return(x[1] + x[4] + x[5] - 1)}

opts = list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-5,
              "maxeval"= 100000,
              "print_level" = 0 )

x0 = c(0.25,0.1,0.1,0.25,0.5)

solution = nloptr(x0=x0,
                  eval_f = Welfare_opt,
                  lb = rep(0,5),
                  ub = c(1,1,max_beehives,1,1),
                  eval_g_eq = constr_land_use,
                  opts = opts)

solution_pareto = round(solution$solution,3)



Welfare_pareto = -solution$objective 



########################################## STACKELERG OPTIMA ----

########################################## INPUT-OUTPUT FUNCTIONS OF THE SCENARIOS 

#SUBSIDY 1 : HS
Subs1 = function(S) {
  x3_max_HS = function(x1,x2){( (om3 - S) / (gamma * Y(x1) * damage_funct(x1,x2) * p2))^(1/(gamma - 1)) }
  f = function(x){y = - pi1(x[1],x[2],x3_max_HS(x[1],x[2]),x[3],x[4])
  return(y)}
  
  
  x0 = c(0.333333,0.5,0.333333,0.333333)
  ui = rbind(c(1,0,0,0), 
             c(-1,0,0,0),
             c(0,1,0,0),
             c(0,-1,0,0),
             c(0,0,1,0),
             c(0,0,-1,0),
             c(0,0,0,1),
             c(0,0,0,-1),
             c(-1,0,-1,-1),
             c(1,0,1,1))
  ci = c(0,-1,0,-1,0,-1,0,-1,-1.0001,0.9999)
  #solution = optim(x0, f, control=list(fnscale= -1)  )
  solution = constrOptim(x0, f, grad = NULL, ui = ui, ci = ci)
  
  area_OSR = solution$par[1]
  chem = solution$par[2]
  area_W = solution$par[3]
  area_G = solution$par[4]
  hives = x3_max_HS(area_OSR,chem)
  
  return(c(area_OSR,chem,hives,area_W,area_G))}



#SUBSIDY 2 : PS
Subs2 = function(S) {
  x3_max_PS = function(x1,x2){  ( om3 / (gamma * (Y(x1) * damage_funct(x1,x2)) * ( p2 + S ) ))^(1/(gamma - 1))  }
  
  f = function(x){y = - pi1(x[1],x[2],x3_max_PS(x[1],x[2]),x[3],x[4])
  return(y)}
  
  x0 = c(0.333333,0.5,0.333333,0.333333)
  ui = rbind(c(1,0,0,0), 
             c(-1,0,0,0),
             c(0,1,0,0),
             c(0,-1,0,0),
             c(0,0,1,0),
             c(0,0,-1,0),
             c(0,0,0,1),
             c(0,0,0,-1),
             c(-1,0,-1,-1),
             c(1,0,1,1))
  ci = c(0,-1,0,-1,0,-1,0,-1,-1.0001,0.9999)
  #solution = optim(x0, f, control=list(fnscale= -1)  )
  solution = constrOptim(x0, f, grad = NULL, ui = ui, ci = ci)
  
  area_OSR = solution$par[1]
  chem = solution$par[2]
  area_W = solution$par[3]
  area_G = solution$par[4]
  hives = x3_max_PS(area_OSR,chem)
  
  return(c(area_OSR,chem,hives,area_W,area_G))}




#TAXATION : AT
Tax = function(S) {
  f = function(x){ y =  - pi1(x[1],x[2],x3_max(x[1],x[2]),x[3],x[4]) + S * x[2]*x[1]
  return(y)}
  
  x0 = c(0.333333,0.5,0.333333,0.333333)
  ui = rbind(c(1,0,0,0), 
             c(-1,0,0,0),
             c(0,1,0,0),
             c(0,-1,0,0),
             c(0,0,1,0),
             c(0,0,-1,0),
             c(0,0,0,1),
             c(0,0,0,-1),
             c(-1,0,-1,-1),
             c(1,0,1,1))
  ci = c(0,-1,0,-1,0,-1,0,-1,-1.0001,0.9999)
  #solution = optim(x0, f, control=list(fnscale= -1)  )
  solution = constrOptim(x0, f, grad = NULL, ui = ui, ci = ci)
  
  area_OSR = solution$par[1]
  chem = solution$par[2]
  area_W = solution$par[3]
  area_G = solution$par[4]
  hives = x3_max(area_OSR,chem)
  
  return(c(area_OSR,chem,hives,area_W,area_G))}



#TAXATION MYOPIC : AT
Tax_myopic = function(S) {
  f = function(x){ y =  - pi1(x[1],x[2],hives[1],x[3],x[4]) + S * x[2]*x[1]
  return(y)}
  
  x0 = c(0.333333,0.5,0.333333,0.333333)
  ui = rbind(c(1,0,0,0), 
             c(-1,0,0,0),
             c(0,1,0,0),
             c(0,-1,0,0),
             c(0,0,1,0),
             c(0,0,-1,0),
             c(0,0,0,1),
             c(0,0,0,-1),
             c(-1,0,-1,-1),
             c(1,0,1,1))
  ci = c(0,-1,0,-1,0,-1,0,-1,-1.0001,0.9999)
  #solution = optim(x0, f, control=list(fnscale= -1)  )
  solution = constrOptim(x0, f, grad = NULL, ui = ui, ci = ci)
  
  area_OSR = solution$par[1]
  chem = solution$par[2]
  area_W = solution$par[3]
  area_G = solution$par[4]
  hives = x3_max(area_OSR,chem)
  
  return(c(area_OSR,chem,hives,area_W,area_G))}



#################### PUBLIC INTERVENTION LEVEL COMPUTATION ----

#LEVEL OF TAX
t1 = 50

#PUBLIC SPENDING
Budget = 5000  #spending for one beekeeper ; Ministere de l'agriculture (2020) FranceAgriMer


Pub_spend = function(x1 ,x2 ,x3 ,S1 ,S2 ,T1 ){(- T1 * x2 * x1  + S1 * x3  + S2 * F2(x1,x2,x3) )*tot_area}

#LEVEL OF HS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs1(s)[1],Subs1(s)[2],Subs1(s)[3],s,0,0) < Budget){s = s + 1 }
s1_lim = s ; rm(s)

#LEVEL OF PS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs2(s)[1],Subs2(s)[2],Subs2(s)[3],0,s,0) < Budget){s = s + 0.1 }
s2_lim = s ; rm(s)



########################################## STACKELBERG WITH COMMUNICATION ----
area_OSR = round(c(Subs1(0)[1],Subs1(s1_lim)[1],Subs2(s2_lim)[1],Tax(t1)[1]),4)
area_W = round(c(Subs1(0)[4],Subs1(s1_lim)[4],Subs2(s2_lim)[4],Tax(t1)[4]),4)
area_G = round(c(Subs1(0)[5],Subs1(s1_lim)[5],Subs2(s2_lim)[5],Tax(t1)[5]),4)

chem = round(c(Subs1(0)[2],Subs1(s1_lim)[2],Subs2(s2_lim)[2],Tax(t1)[2]),4)

hives = round(c(Subs1(0)[3],Subs1(s1_lim)[3],Subs2(s2_lim)[3],Tax(t1)[3]),4)


########################################## STACKELBERG WITHOUT COMMUNICATION ----
area_OSR_myopic = round(c(Subs1(0)[1],Subs1(0)[1],Subs2(0)[1],Tax_myopic(t1)[1]),4)
area_W_myopic = round(c(Subs1(0)[4],Subs1(0)[4],Subs2(0)[4],Tax_myopic(t1)[4]),4)
area_G_myopic = round(c(Subs1(0)[5],Subs1(0)[5],Subs2(0)[5],Tax_myopic(t1)[5]),4)

chem_myopic = round(c(Subs1(0)[2],Subs1(0)[2],Subs2(0)[2],Tax_myopic(t1)[2]),4)

hives_myopic = round(c(Subs1(0)[3],
                        x3_max_HS(Subs1(0)[1],Subs1(0)[2]),
                        x3_max_PS(Subs1(0)[1],Subs1(0)[2]),
                        x3_max(area_OSR_myopic[4],chem_myopic [4])),4)



Welfare(c(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]))/Welfare_pareto
Welfare(c(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2]))/Welfare_pareto
Welfare(c(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3]))/Welfare_pareto
Welfare(c(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4]))/Welfare_pareto

Welfare(c(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1],area_W_myopic[1],area_G_myopic[1]))/Welfare_pareto
Welfare(c(area_OSR_myopic[2],chem_myopic[2],hives_myopic[2],area_W_myopic[2],area_G_myopic[2]))/Welfare_pareto
Welfare(c(area_OSR_myopic[3],chem_myopic[3],hives_myopic[3],area_W_myopic[3],area_G_myopic[3]))/Welfare_pareto
Welfare(c(area_OSR_myopic[4],chem_myopic[4],hives_myopic[4],area_W_myopic[4],area_G_myopic[4]))/Welfare_pareto



################################################## SENSITIVITY ANALYSIS ON INCENTIVE LEVELS ---- 
n_simu = 4
 vector_S1 = c(10,20,30,40) ; vector_S2 = c(0.5,1,1.5,2) ; vector_T = c(40,45,55,60)
sensitivity_policies_data = data.frame(area_OSR = rep(NA,4), chem = NA, hives = NA, area_W = NA, area_G = NA, incentive_level = c(vector_S1,vector_S2,vector_T), scenario = c(rep('S1',n_simu),rep('S2',n_simu),rep('T',n_simu)))

# computing the decision
for (i in 1:3){
  for (k in 1:n_simu){ if (i == 1){
    sensitivity_policies_data$area_OSR[k] = round(Subs1(sensitivity_policies_data$incentive_level[k])[1],4)
    sensitivity_policies_data$chem[k] = round(Subs1(sensitivity_policies_data$incentive_level[k])[2],4)
    sensitivity_policies_data$hives[k] = round(Subs1(sensitivity_policies_data$incentive_level[k])[3],4)
    sensitivity_policies_data$area_W[k] = round(Subs1(sensitivity_policies_data$incentive_level[k])[4],4)
    sensitivity_policies_data$area_G[k] = round(Subs1(sensitivity_policies_data$incentive_level[k])[5],4)
  } else if (i == 2){ 
    sensitivity_policies_data$area_OSR[k + (i - 1)*n_simu] = round(Subs2(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[1],4)
    sensitivity_policies_data$chem[k + (i - 1)*n_simu] = round(Subs2(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[2],4)
    sensitivity_policies_data$hives[k + (i - 1)*n_simu] = round(Subs2(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[3],4)
    sensitivity_policies_data$area_W[k + (i - 1)*n_simu] = round(Subs2(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[4],4)
    sensitivity_policies_data$area_G[k + (i - 1)*n_simu] = round(Subs2(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[5],4)
  } else {
    sensitivity_policies_data$area_OSR[k + (i - 1)*n_simu] = round(Tax(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[1],4)
    sensitivity_policies_data$chem[k + (i - 1)*n_simu] = round(Tax(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[2],4)
    sensitivity_policies_data$hives[k + (i - 1)*n_simu] = round(Tax(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[3],4)
    sensitivity_policies_data$area_W[k + (i - 1)*n_simu] = round(Tax(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[4],4)
    sensitivity_policies_data$area_G[k + (i - 1)*n_simu] = round(Tax(sensitivity_policies_data$incentive_level[k + (i - 1)*n_simu])[5],4)  
    } }
}

sensitivity_policies_data$total_wealth = NA;  sensitivity_policies_data$agriculture_wealth = NA; sensitivity_policies_data$beekeeping_wealth = NA

for (i in 1:nrow(sensitivity_policies_data)){
sensitivity_policies_data$agriculture_wealth[i] = if (sensitivity_policies_data$scenario[i] == 'S1'){ tot_area*(pi1(sensitivity_policies_data$area_OSR[i],sensitivity_policies_data$chem[i],sensitivity_policies_data$hives[i],sensitivity_policies_data$area_W[i],sensitivity_policies_data$area_G[i]) - pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]))
                                                 } else if (sensitivity_policies_data$scenario[i] == 'S2') { tot_area*(pi1(sensitivity_policies_data$area_OSR[i],sensitivity_policies_data$chem[i],sensitivity_policies_data$hives[i],sensitivity_policies_data$area_W[i],sensitivity_policies_data$area_G[i]) - pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]))
                                                 } else { tot_area*(pi1_PT(sensitivity_policies_data$area_OSR[i],sensitivity_policies_data$chem[i],sensitivity_policies_data$hives[i],sensitivity_policies_data$area_W[i],sensitivity_policies_data$area_G[i],sensitivity_policies_data$incentive_level[i]) - pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1])) }
  
sensitivity_policies_data$beekeeping_wealth[i] = if (sensitivity_policies_data$scenario[i] == 'S1'){ tot_area*(pi2_HS(sensitivity_policies_data$area_OSR[i],sensitivity_policies_data$chem[i],sensitivity_policies_data$hives[i], S = sensitivity_policies_data$incentive_level[i]) - pi2(area_OSR[1],chem[1],hives[1]))
} else if (sensitivity_policies_data$scenario[i] == 'S2') { tot_area*(pi2_PS(sensitivity_policies_data$area_OSR[i],sensitivity_policies_data$chem[i],sensitivity_policies_data$hives[i], S = sensitivity_policies_data$incentive_level[i]) - pi2(area_OSR[1],chem[1],hives[1]))
} else {tot_area*(pi2(sensitivity_policies_data$area_OSR[i],sensitivity_policies_data$chem[i],sensitivity_policies_data$hives[i]) - pi2(area_OSR[1],chem[1],hives[1])) }

sensitivity_policies_data$total_wealth[i] = sensitivity_policies_data$agriculture_wealth[i] + sensitivity_policies_data$beekeeping_wealth[i]
}



for (i in 1:n_simu){ # Adding public spendings for HS
  sensitivity_policies_data$total_wealth[i] = sensitivity_policies_data$total_wealth[i] - tot_area*sensitivity_policies_data$incentive_level[i]*sensitivity_policies_data$hives[i] }

for (i in (n_simu+1):(2*n_simu)){ # Adding public spendings for PS
  sensitivity_policies_data$total_wealth[i] = sensitivity_policies_data$total_wealth[i] - tot_area*sensitivity_policies_data$incentive_level[i]*F2(sensitivity_policies_data$area_OSR[i],sensitivity_policies_data$chem[i],sensitivity_policies_data$hives[i]) }

for (i in (2*n_simu+1):(3*n_simu)){ # Adding public revenues for PT
  sensitivity_policies_data$total_wealth[i] = sensitivity_policies_data$total_wealth[i] + tot_area*sensitivity_policies_data$incentive_level[i]* sensitivity_policies_data$area_OSR[i]* sensitivity_policies_data$chem[i]}




View(sensitivity_policies_data)
View(sensitivity_policies_data[,c(8,9,10)])

data_sensi_clean = data.frame(C1 = rep(NA,nrow(sensitivity_policies_data)), C2 = NA, C3 = NA)

for (i in 1:nrow(data_sensi_clean)){
  for (j in 1:3){
    data_sensi_clean[i,j] = paste(as.character(round(sensitivity_policies_data[,c(8,9,10)][i,j])),'€')
  }
}

#View(data_sensi_clean)


######################### FIGURE 2 PLOT OF GAINS AND LOSSES ----

colors_pigou = c('grey80','black')


W_HS = data.frame(values = c((pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2]) - pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]))*tot_area, #effect on farmer | HS
                             (pi2_s1(area_OSR[2],chem[2],hives[2]) - pi2(area_OSR[1],chem[1],hives[1]))*tot_area, #effect on beekeeper with subs | HS
                             - (hives[2]*s1_lim)*tot_area,
                             (pi1(area_OSR_myopic[2],chem_myopic[2],hives_myopic[2],area_W_myopic[2],area_G_myopic[2]) - pi1(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1],area_W_myopic[1],area_G_myopic[1]))*tot_area, #effect on farmer and myopic | HS
                             (pi2_s1(area_OSR_myopic[2],chem_myopic[2],hives_myopic[2]) - pi2(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1]))*tot_area, #effect on beekeeper with subs and myopic | HS
                             - (hives_myopic[2]*s1_lim)*tot_area))

W_HS_bis = data.frame(values = c(W_HS$values[4],W_HS$values[1]-W_HS$values[4],
                                 W_HS$values[5],W_HS$values[2]-W_HS$values[5],
                                 W_HS$values[6],W_HS$values[3]-W_HS$values[6]),
                      agent = c('1','1','2','2','3','3'), 
                      status = c('myopic','non-myopic','myopic','non-myopic','myopic','myopic'),
                      color = c('1','2','1','2','1','1'))


plot_a =ggplot(data=W_HS_bis, aes(x=agent, y=values, fill = forcats::fct_rev(status), color = forcats::fct_rev(color)))  + theme_bw() +
  geom_bar(stat="identity", width = 0.5) + 
  coord_flip(ylim = c(1.1*min(c(W_HS$values,W_PS$values,W_AT$values)),1.1*max(c(W_HS$values,W_PS$values,W_AT$values)))) +
  scale_fill_manual(values = colors_pigou, 
                    labels = c('With','Without')) +
  scale_color_manual(values = colors_pigou) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits=c('3', '1', '2'), 
                   labels = c('Policy maker', 'Agriculture\n(6 farmers)     ', 'Beekeeping\n(one beekeeper)')) +
  labs(title = '(a) Hive subsidy (HS)', x = '', y = expression(paste(Delta,'Wealth','(Euros)')), fill = 'Communication', color = 'Scenarios', linetype = 'Economic functions') + 
  theme(legend.position = 'right', axis.title.x = element_text(size = 11, vjust = 0), axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        strip.text.x = element_blank(),
        #axis.text.y = element_text(color = 'black', angle = 40, size = 9),
        plot.title = element_text(size = 11, face = 'italic', hjust = 0)) + guides(color = F)



W_PS = data.frame(values = c((pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3]) - pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]))*tot_area, #effect on farmer | HS
                             (pi2_s2(area_OSR[3],chem[3],hives[3]) - pi2(area_OSR[1],chem[1],hives[1]))*tot_area, #effect on beekeeper with subs | HS
                             - (F2(area_OSR[3],chem[3],hives[3])*s2_lim)*tot_area,
                             (pi1(area_OSR_myopic[3],chem_myopic[3],hives_myopic[3],area_W_myopic[3],area_G_myopic[3]) - pi1(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1],area_W_myopic[1],area_G_myopic[1]))*tot_area, #effect on farmer and myopic | HS
                             (pi2_s2(area_OSR_myopic[3],chem_myopic[3],hives_myopic[3]) - pi2(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1]))*tot_area, #effect on beekeeper with subs and myopic | HS
                             - (F2(area_OSR_myopic[3],chem_myopic[3],hives_myopic[3])*s2_lim)*tot_area))

W_PS_bis = data.frame(values = c(W_PS$values[4],W_PS$values[1]-W_PS$values[4],
                                 W_PS$values[5],W_PS$values[2]-W_PS$values[5],
                                 W_PS$values[6],W_PS$values[3]-W_PS$values[6]),
                      agent = c('1','1','2','2','3','3'), 
                      status = c('myopic','non-myopic','myopic','non-myopic','myopic','myopic'),
                      color = c('1','2','1','2','1','1'))


plot_b = ggplot(data=W_PS_bis, aes(x=agent, y=values, fill = forcats::fct_rev(status), color = forcats::fct_rev(color)))  + theme_bw() +
  geom_bar(stat="identity", width = 0.5) + 
  coord_flip(ylim = c(1.1*min(c(W_HS$values,W_PS$values,W_AT$values)),1.1*max(c(W_HS$values,W_PS$values,W_AT$values)))) +
  scale_fill_manual(values = colors_pigou, 
                    labels = c('Non-myopic','Myopic')) +
  scale_color_manual(values = colors_pigou) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits=c('3', '1', '2'), 
                   labels = c('Policy maker', 'Agriculture\n(6 farmers)     ', 'Beekeeping\n(one beekeeper)')) +
  labs(title = '(b) Price subsidy (PS)', x = '', y = expression(paste(Delta,'Wealth','(Euros)')), fill = 'Behavior', color = 'Scenarios', linetype = 'Economic functions') + 
  theme(legend.position = 'right', axis.title.x = element_text(size = 11, vjust = 0), axis.text.x = element_text(size = 8),
        #axis.text.y = element_text(size = 10, angle = 45), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 11, face = 'italic', hjust = 0))



W_AT = data.frame(values = c((pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4]) - pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]))*tot_area, 
                             (pi2(area_OSR[4],chem[4],hives[4]) - pi2(area_OSR[1],chem[1],hives[1]))*tot_area, 
                             + (area_OSR[4]*chem[4]*t1)*tot_area,
                             (pi1_tax(area_OSR_myopic[4],chem_myopic[4],hives_myopic[4],area_W_myopic[4],area_G_myopic[4]) - pi1(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1],area_W_myopic[1],area_G_myopic[1]))*tot_area, 
                             (pi2(area_OSR_myopic[4],chem_myopic[4],hives_myopic[4]) - pi2(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1]))*tot_area, 
                             + (area_OSR_myopic[4]*chem_myopic[4]*t1)*tot_area),
                  status = c(rep('nonmyopic',3),rep('myopic',3)))

W_AT_bis = data.frame(values = c(W_AT$values[4],W_AT$values[1]-W_AT$values[4],
                                 W_AT$values[2],W_AT$values[5]-W_AT$values[2],
                                 W_AT$values[6],W_AT$values[3]-W_AT$values[6]),
                      agent = c('1','1','2','2','3','3'), 
                      status = c('myopic','non-myopic','myopic','non-myopic','myopic','non-myopic'),
                      color = c('1','2','1','1','1','2'))


plot_c = ggplot(data=W_AT_bis, aes(x=agent, y=values, fill = forcats::fct_rev(status), color = forcats::fct_rev(color)))  + theme_bw() +
  geom_bar(stat="identity", width = 0.5) + 
  coord_flip(ylim = c(1.1*min(c(W_HS$values,W_PS$values,W_AT$values)),1.1*max(c(W_HS$values,W_PS$values,W_AT$values)))) +
  scale_fill_manual(values = colors_pigou, 
                    labels = c('Non-myopic','Myopic')) +
  scale_color_manual(values = colors_pigou) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits=c('3', '1', '2'), 
                   labels = c('Policy maker', 'Agriculture\n(6 farmers)     ', 'Beekeeping\n(one beekeeper)')) +
  labs(title = '(c) Pesticide tax (PT)',  x = '', y = expression(paste(Delta,'Wealth','(Euros)')), fill = 'Behavior', color = 'Scenarios', linetype = 'Economic functions') + 
  theme(legend.position = 'right', axis.title.x = element_text(size = 11, vjust = 0), axis.text.x = element_text(size = 8),
        #axis.text.y = element_text(size = 10, angle = 45), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 11, face = 'italic', hjust = 0))


ggarrange(plot_a, plot_b, plot_c, align = 'h', legend = 'bottom', ncol = 3, common.legend = T) 

################################################## FIGURE (3a) RADAR CHART WITH COM ----

## DEFINITION MATRIX MYOPIC


Provision = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[1:4]
Biodiversity = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[5:8]
Poll = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[9:12]
Water_Pest = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[13:16]
Water_Nutr = ecosystem_services(aarea_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[17:20]
Wild = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[21:24]

I1 = ((Provision - Provision[1])/abs(Provision[1]))*100
I2 = ((Poll - Poll[1])/abs(Poll[1]))*100
I3 = ((Biodiversity - Biodiversity[1])/abs(Biodiversity[1]))*100
I4 = ((Wild - Wild[1])/abs(Wild[1]))*100
I5 = ((Water_Pest - Water_Pest[1])/abs(Water_Pest[1]))*100
I6 = ((Water_Nutr - Water_Nutr[1])/abs(Water_Nutr[1]))*100


Priv_Wealth = c(pi1(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1],area_W_myopic[1],area_G_myopic[1]) + pi2(area_OSR[1],chem[1],hives[1]),
                pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2]) + pi2_s1(area_OSR[2],chem[2],hives[2]),
                pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3]) + pi2_s2(area_OSR[3],chem[3],hives[3]),
                pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4]) + pi2(area_OSR[4],chem[4],hives[4]))

Tot_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1])*tot_area + pi2(area_OSR[1],chem[1],hives[1])*tot_area - Pub_spend(area_OSR[1],chem[1],hives[1],0,0,0),
               pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2])*tot_area + pi2_s1(area_OSR[2],chem[2],hives[2])*tot_area - Pub_spend(area_OSR[2],chem[2],hives[2],s1_lim,0,0),
               pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3])*tot_area + pi2_s2(area_OSR[3],chem[3],hives[3])*tot_area - Pub_spend(area_OSR[3],chem[3],hives[3],0,s2_lim,0),
               pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4])*tot_area + pi2(area_OSR[4],chem[4],hives[4])*tot_area - Pub_spend(area_OSR[4],chem[4],hives[4],0,0,t1))

Pareto_ratio = c(Welfare(c(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1],area_W_myopic[1],area_G_myopic[1]))/Welfare_pareto,
                 Welfare(c(area_OSR_myopic[2],chem_myopic[2],hives_myopic[2],area_W_myopic[2],area_G_myopic[2]))/Welfare_pareto,
                 Welfare(c(area_OSR_myopic[3],chem_myopic[3],hives_myopic[3],area_W_myopic[3],area_G_myopic[3]))/Welfare_pareto,
                 Welfare(c(area_OSR_myopic[4],chem_myopic[4],hives_myopic[4],area_W_myopic[4],area_G_myopic[4]))/Welfare_pareto)

I7 = ((Priv_Wealth - Priv_Wealth[1])/abs(Priv_Wealth[1]))*100
I8 = ((Tot_Wealth - Tot_Wealth[1])/abs(Tot_Wealth[1]))*100
I9 = ((Pareto_ratio - Pareto_ratio[1])/abs(Pareto_ratio[1]))*100


I_MATRIX_t1_myopic = data.frame(cbind(I1,I2,I5,I6,I4,I3,I9))

###


Provision = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[1:4]
Biodiversity = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[5:8]
Poll = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[9:12]
Water_Pest = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[13:16]
Water_Nutr = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[17:20]
Wild = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[21:24]

I1 = ((Provision - Provision[1])/abs(Provision[1]))*100
I2 = ((Poll - Poll[1])/abs(Poll[1]))*100
I3 = ((Biodiversity - Biodiversity[1])/abs(Biodiversity[1]))*100
I4 = ((Wild - Wild[1])/abs(Wild[1]))*100
I5 = ((Water_Pest - Water_Pest[1])/abs(Water_Pest[1]))*100
I6 = ((Water_Nutr - Water_Nutr[1])/abs(Water_Nutr[1]))*100


Priv_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]) + pi2(area_OSR[1],chem[1],hives[1]),
                pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2]) + pi2_s1(area_OSR[2],chem[2],hives[2]),
                pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3]) + pi2_s2(area_OSR[3],chem[3],hives[3]),
                pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4]) + pi2(area_OSR[4],chem[4],hives[4]))

Tot_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1])*tot_area + pi2(area_OSR[1],chem[1],hives[1])*tot_area - Pub_spend(area_OSR[1],chem[1],hives[1],0,0,0),
               pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2])*tot_area + pi2_s1(area_OSR[2],chem[2],hives[2])*tot_area - Pub_spend(area_OSR[2],chem[2],hives[2],s1_lim,0,0),
               pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3])*tot_area + pi2_s2(area_OSR[3],chem[3],hives[3])*tot_area - Pub_spend(area_OSR[3],chem[3],hives[3],0,s2_lim,0),
               pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4])*tot_area + pi2(area_OSR[4],chem[4],hives[4])*tot_area - Pub_spend(area_OSR[4],chem[4],hives[4],0,0,t1))

Pareto_ratio = c(Welfare(c(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]))/Welfare_pareto,
                 Welfare(c(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2]))/Welfare_pareto,
                 Welfare(c(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3]))/Welfare_pareto,
                 Welfare(c(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4]))/Welfare_pareto)


I7 = ((Priv_Wealth - Priv_Wealth[1])/abs(Priv_Wealth[1]))*100
I8 = ((Tot_Wealth - Tot_Wealth[1])/abs(Tot_Wealth[1]))*100
I9 = ((Pareto_ratio - Pareto_ratio[1])/abs(Pareto_ratio[1]))*100




I_MATRIX_t1 = data.frame(cbind(I1,I2,I5,I6,I4,I3,I9))
colnames(I_MATRIX_t1) = c('Food/Feed provision','Pollination',
                          '\nWater          \nquality (pesticides)         ',
                          '\nWater          \nquality (nutrients)         ',
                          'Wild bee\nabundance','                 Plant species\n                richness','       Economic\n       efficiency')
I_MATRIX_t_SA = data.frame(cbind(I2,I1,I5,I6,I4,I3,I7,I8))


colors_vect = c("red","chartreuse2","darkgreen","darkblue")
colors_vect_indic = c(rep("darkorange",4),rep("darkolivegreen3",2),rep("darkslategray3",1))

max_sup = NA ; min_inf = NA #ATTENTION À AVOIR BIEN DÉFINI LA MATRICE I_MATRIX_t1_myopic
j = 1:4
for (i in 1:7){max_sup[i] = floor(max(I_MATRIX_t1[,i],I_MATRIX_t1_myopic[,i])/5 + 1)*5} ;

min_inf = floor(min(I_MATRIX_t1,I_MATRIX_t1_myopic[,i])/5 )*5

data =  rbind(max_sup,min_inf,I_MATRIX_t1[j,])


data_modified = rbind(rep(1,7),rep(-0.5,7),rep(NA,7),rep(NA,7),rep(NA,7),rep(NA,7))
for (i in 3:6){
  for (j in 1:7){
    data_modified[i,j] = ifelse(data[i,j] > 0, data[i,j]/max_sup[j], (0.5*data[i,j])/abs(min_inf))}}


dev.off() ; 
radarchart2(data.frame(data_modified), #title = TeX('Bundle of ecosystem services'), 
            vlabels = '',
            vlabcol = colors_vect_indic,
            #c('Water quality','Wildbees','Plant species \n richness','Food provision','Pollination','Private wealth','Total wealth'), 
            #seg = 10 + abs(min_inf/10),
            cglty = 1, cglwd=0.1,
            pcol = c(colors_vect[1],colors_vect[2],colors_vect[3],colors_vect[4]), 
            vlcex =  1, plwd = c(1.5,rep(2,4)), plty=c(3,1,1,1), pty = c(32,rep(21,4)), 
            paxislabels = paste('+',c(max_sup),'%'),
            caxislabels = c(paste(min_inf,'%'),NA,NA,NA,NA,NA),
            axistype=5, axislabcol = 'grey60' , calcex = 1.15, palcex = 1.15, title = '')

rm(data,data_modified)



################################################## FIGURE 3(b) RADAR CHART WITHOUT COM ----



Provision = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[1:4]
Biodiversity = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[5:8]
Poll = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[9:12]
Water_Pest = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[13:16]
Water_Nutr = ecosystem_services(aarea_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[17:20]
Wild = ecosystem_services(area_OSR_myopic,chem_myopic,hives_myopic,area_W_myopic,area_G_myopic)[21:24]

I1 = ((Provision - Provision[1])/abs(Provision[1]))*100
I2 = ((Poll - Poll[1])/abs(Poll[1]))*100
I3 = ((Biodiversity - Biodiversity[1])/abs(Biodiversity[1]))*100
I4 = ((Wild - Wild[1])/abs(Wild[1]))*100
I5 = ((Water_Pest - Water_Pest[1])/abs(Water_Pest[1]))*100
I6 = ((Water_Nutr - Water_Nutr[1])/abs(Water_Nutr[1]))*100


Priv_Wealth = c(pi1(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1],area_W_myopic[1],area_G_myopic[1]) + pi2(area_OSR[1],chem[1],hives[1]),
                pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2]) + pi2_s1(area_OSR[2],chem[2],hives[2]),
                pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3]) + pi2_s2(area_OSR[3],chem[3],hives[3]),
                pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4]) + pi2(area_OSR[4],chem[4],hives[4]))

Tot_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1])*tot_area + pi2(area_OSR[1],chem[1],hives[1])*tot_area - Pub_spend(area_OSR[1],chem[1],hives[1],0,0,0),
               pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2])*tot_area + pi2_s1(area_OSR[2],chem[2],hives[2])*tot_area - Pub_spend(area_OSR[2],chem[2],hives[2],s1_lim,0,0),
               pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3])*tot_area + pi2_s2(area_OSR[3],chem[3],hives[3])*tot_area - Pub_spend(area_OSR[3],chem[3],hives[3],0,s2_lim,0),
               pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4])*tot_area + pi2(area_OSR[4],chem[4],hives[4])*tot_area - Pub_spend(area_OSR[4],chem[4],hives[4],0,0,t1))

Pareto_ratio = c(Welfare(c(area_OSR_myopic[1],chem_myopic[1],hives_myopic[1],area_W_myopic[1],area_G_myopic[1]))/Welfare_pareto,
  Welfare(c(area_OSR_myopic[2],chem_myopic[2],hives_myopic[2],area_W_myopic[2],area_G_myopic[2]))/Welfare_pareto,
  Welfare(c(area_OSR_myopic[3],chem_myopic[3],hives_myopic[3],area_W_myopic[3],area_G_myopic[3]))/Welfare_pareto,
  Welfare(c(area_OSR_myopic[4],chem_myopic[4],hives_myopic[4],area_W_myopic[4],area_G_myopic[4]))/Welfare_pareto)

I7 = ((Priv_Wealth - Priv_Wealth[1])/abs(Priv_Wealth[1]))*100
I8 = ((Tot_Wealth - Tot_Wealth[1])/abs(Tot_Wealth[1]))*100
I9 = ((Pareto_ratio - Pareto_ratio[1])/abs(Pareto_ratio[1]))*100


I_MATRIX_t1_myopic = data.frame(cbind(I1,I2,I5,I6,I4,I3,I9))
colnames(I_MATRIX_t1_myopic) = c('Food/Feed provision','Pollination',
                                 '\nWater          \nquality (pesticides)         ',
                                 '\nWater          \nquality (nutrients)         ',
                                 'Wild bee\nabundance','                 Plant species\n                richness','       Economic\n       efficiency')
I_MATRIX_t_SA = data.frame(cbind(I2,I1,I5,I6,I4,I3,I9))
colors_vect = c("red","chartreuse2","darkgreen","darkblue")
colors_vect_indic = c(rep("darkorange",4),rep("darkolivegreen3",2),rep("darkslategray3",1))

j = 1:4

data =  rbind(max_sup,min_inf,I_MATRIX_t1_myopic[j,])


data_modified = rbind(rep(1,7),rep(-0.5,7),rep(NA,7),rep(NA,7),rep(NA,7),rep(NA,7))
for (i in 3:6){
  for (j in 1:7){
    data_modified[i,j] = ifelse(data[i,j] > 0, data[i,j]/max_sup[j], (0.5*data[i,j])/abs(min_inf))}}


dev.off() ; 
radarchart2(data.frame(data_modified), #title = TeX('Bundle of ecosystem services'), 
            vlabels = '',
            vlabcol = colors_vect_indic,
            #c('Water quality','Wildbees','Plant species \n richness','Food provision','Pollination','Private wealth','Total wealth'), 
            #seg = 10 + abs(min_inf/10),
            cglty = 1, cglwd=0.1,
            pcol = c(colors_vect[1],colors_vect[2],colors_vect[3],colors_vect[4]), 
            vlcex =  1, plwd = c(1.5,rep(2,4)), plty=c(3,1,1,1), pty = c(32,rep(21,4)), 
            paxislabels = paste('+',c(max_sup),'%'),
            caxislabels = c(paste(min_inf,'%'),NA,NA,NA,NA,NA),
            axistype=5, axislabcol = 'grey60' , calcex = 1.15, palcex = 1.15, title = '')


legend(x=0.95 ,y=-0.2, legend = c('BAU','HS','PS','PT'), title = 'Scenarios', y.intersp = 0.6,
       bty = "n", pch=1 , col=c(colors_vect[1:4]) ,  cex=0.9, pt.cex=1)

legend(x=0.95 ,y=0, legend = c('Ecosystem\nservice','Ecological','Economic'), title = 'Indicators           ', y.intersp = 0.2 ,x.intersp = 0.7,
       bty = "n", pch=16 , col= unique(colors_vect_indic),  cex=0.9, pt.cex=1)

rm(data,data_modified)


# ORIGINAL SCENARIO OUTPUTS
area_OSR = round(c(Subs1(0)[1],Subs1(s1_lim)[1],Subs2(s2_lim)[1],Tax(t1)[1]),4)
chem = round(c(Subs1(0)[2],Subs1(s1_lim)[2],Subs2(s2_lim)[2],Tax(t1)[2]),4)
hives = round(c(Subs1(0)[3],Subs1(s1_lim)[3],Subs2(s2_lim)[3],Tax(t1)[3]),4)

area_W = round(c(Subs1(0)[4],Subs1(s1_lim)[4],Subs2(s2_lim)[4],Tax(t1)[4]),4)
area_G = round(c(Subs1(0)[5],Subs1(s1_lim)[5],Subs2(s2_lim)[5],Tax(t1)[5]),4)




################################################## FIGURE 4 - LAND USE ----


M_plot = data.frame(area = c(area_G[1],area_OSR[1],area_W[1],
                    area_G[2],area_OSR[2],area_W[2],
                    area_G[3],area_OSR[3],area_W[3],
                    area_G[4],area_OSR[4],area_W[4]), 
                    crop = rep(c('Grasslands','OSR','Wheat'),4),
           scenario = c(rep('BAU',3),rep('HS',3),rep('PS',3),rep('AT',3)))

M_subplot = M_plot[M_plot$scenario == 'BAU',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_1 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(a) Business as usual (BAU)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('goldenrod2','yellow','chartreuse2')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'bold', size = 11))
  
M_subplot = M_plot[M_plot$scenario == 'HS',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_2 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(b) Hive subsidy (HS)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('goldenrod2','yellow','chartreuse2')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'bold', size = 11))

M_subplot = M_plot[M_plot$scenario == 'PS',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_3 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(c) Price subsidy (PS)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('goldenrod2','yellow','chartreuse2')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'bold', size = 11))

M_subplot = M_plot[M_plot$scenario == 'AT',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_4 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(d) Pesticide tax (PT)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('goldenrod2','yellow','chartreuse2')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'bold', size = 11))

annotate_figure(ggarrange(plot_1,plot_2,plot_3,plot_4,
          align = 'hv', legend = 'right', nrow = 2, ncol = 2, common.legend = TRUE), top = '')






####################################################### FIGURE 5 CAUSAL CHAIN / CASCADE #########################

#MARGINAL PRODUCTIVITY OF FARMER
prod_marg_farmer_x1 = function(x1,x2,x3,x5) {x1*((alpha1*delta*k_wildbees*x2^damage*x5*(k_honeybees*x3+k_wildbees*(1-delta*x1*x2^damage)*x5))/((k_wildbees+k_honeybees*max_beehives)^2*(beta1+(k_honeybees*x3+k_wildbees*(1-delta*x1*x2^damage)*x5)/(k_honeybees*max_beehives+k_wildbees))^2)-
                                                      (alpha1*delta*k_wildbees*x2^damage*x5)/((k_honeybees*max_beehives+k_wildbees)*((k_honeybees*x3+k_wildbees*(1-delta*x1*x2^damage)*x5)/(k_honeybees*max_beehives+k_wildbees)+beta1)))+
    (alpha1*(k_honeybees*x3+k_wildbees*(1-delta*x1*x2^damage)*x5))/((k_honeybees*max_beehives+k_wildbees)*((k_honeybees*x3+k_wildbees*(1-delta*x1*x2^damage)*x5)/(k_honeybees*max_beehives+k_wildbees)+beta1))+(alpha2*x2)/(x2+beta2)+f1_3}
prod_marg_farmer_x2 = function(x1,x2,x3,x5) {x1*(-(alpha1*damage*delta*k_wildbees*x1*x2^(damage-1)*x5)/((k_honeybees*max_beehives+k_wildbees)*((k_honeybees*x3+k_wildbees*(1-delta*x1*x2^damage)*x5)/(k_honeybees*max_beehives+k_wildbees)+beta1))+
                                                (alpha1*damage*delta*k_wildbees*x1*x2^(damage-1)*x5*(k_honeybees*x3+k_wildbees*(1-delta*x1*x2^damage)*x5))/((k_wildbees+k_honeybees*max_beehives)^2*(beta1+(k_honeybees*x3+k_wildbees*(1-delta*x1*x2^damage)*x5)/(k_honeybees*max_beehives+k_wildbees))^2)+alpha2/(x2+beta2)-(alpha2*x2)/(beta2+x2)^2)}
#MARGINAL PRODUCTIVITY OF BEEKEEPER
prod_marg_beekeeper_x3 = function(x1,x2,x3) {x1^gamma_1*(1-delta*x1*x2^damage)*x3^(gamma-1)*K*gamma}


#PLOTS
M = data.frame(values = c(p2, p2 + (F2(area_OSR[1],chem[1],x3_max_HS(area_OSR[1],chem[1]))^((1-gamma)/gamma)*s1_lim)/((Y(area_OSR[1])*damage_funct(area_OSR[1],chem[1]))^(1/gamma)*gamma), p2 + s2_lim,
                          c(hives[1],x3_max_HS(area_OSR[1],chem[1]),x3_max_PS(area_OSR[1],chem[1]))))

M = data.frame(diff_values = c((M$values[2:3] - M$values[1])/M$values[1],
                               (M$values[5:6] - M$values[4])/M$values[4]),
               scenario = c('HS','PS'),
               variable = c(rep('rentability',2),rep('hives',2)),plot = 'a')

M$scenario = factor(M$scenario,levels = c('HS','PS'))
M$variable = factor(M$variable,levels = c('rentability','hives'))


M_a = M

F4_a = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.5) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-40,80)) +
  scale_x_discrete(name = '', labels = c('Honey \nmarginal revenue','Beehive number')) +
  
  scale_fill_manual(values = colors_vect[2:3], name = '') + labs(subtitle = '(a) (beekeeper)') +
  theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 9)) 
F4_a


M = data.frame(values = c(Bees(area_OSR[1],chem[1],hives[1],area_G[1]),
                          Bees(area_OSR[1],chem[1],x3_max_HS(area_OSR[1],chem[1]),area_G[1]),
                          Bees(area_OSR[1],chem[1],x3_max_PS(area_OSR[1],chem[1]),area_G[1])))

M = data.frame(diff_values = c((M$values[2:3] - M$values[1])/M$values[1]), 
               scenario = c('HS','PS'),
               variable = rep('pollination',2),plot = 'b')

M$scenario = factor(M$scenario,levels = c('HS','PS'))
M$variable = factor(M$variable,levels = c('pollination'))

M_b = M

F4_b = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.25) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-40,80)) +
  scale_x_discrete(name = '', labels = c('Pollination level')) +
  
  scale_fill_manual(values = colors_vect[2:3], name = '') + labs(subtitle = '(b)') +
  theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
F4_b



M = data.frame(values = c(prod_marg_farmer_x1(area_OSR[1],chem[1],hives[1],area_G[1]),
                          prod_marg_farmer_x1(area_OSR[1],chem[1],x3_max_HS(area_OSR[1],chem[1]),area_G[1]),
                          prod_marg_farmer_x1(area_OSR[1],chem[1],x3_max_PS(area_OSR[1],chem[1]),area_G[1]),
                          area_OSR[1:3],
                          prod_marg_farmer_x2(area_OSR[1],chem[1],hives[1],area_G[1]),
                          prod_marg_farmer_x2(area_OSR[1],chem[1],x3_max_HS(area_OSR[1],chem[1]),area_G[1]),
                          prod_marg_farmer_x2(area_OSR[1],chem[1],x3_max_PS(area_OSR[1],chem[1]),area_G[1]),
                          chem[1:3]))

M = data.frame(diff_values = c((M$values[2:3] - M$values[1])/M$values[1],
                               (M$values[5:6] - M$values[4])/M$values[4],
                               (M$values[8:9] - M$values[7])/M$values[7],
                               (M$values[11:12] - M$values[10])/M$values[10]),
               scenario = c('HS','PS'),
               variable = c(rep('MP_x1',2),rep('area',2),rep('MP_x2',2),rep('chem',2)),plot = 'c')

M$scenario = factor(M$scenario,levels = c('HS','PS'))
M$variable = factor(M$variable,levels = c('MP_x1','area','MP_x2','chem'))

M_c = M

F4_c = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.5) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-20,80)) +
  scale_x_discrete(name = '', labels = c('OSR \nmarginal product','OSR proportion','Agrochemical \nmarginal product','Agrochemical \nrate')) +
  
  scale_fill_manual(values = colors_vect[2:3], name = '') + labs(subtitle = '(c) (farmer)') +
  theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
F4_c




M = data.frame(values = c(p_OSR,p_OSR - t1* (chem[1]/(f1_1(area_OSR[1],chem[1],hives[1],area_G[1])+ f1_2(chem[1]) + f1_3)),area_OSR[1],area_OSR[4],chem[1],chem[4]))

M = data.frame(diff_values = c((M$values[2] - M$values[1])/M$values[1], 
                               (M$values[4] - M$values[3])/M$values[3],
                               (M$values[6] - M$values[5])/M$values[5]),
               scenario = c('AT'),
               variable = c('MR','area','chem'),plot = 'd')

M$scenario = factor(M$scenario,levels = c('AT'))
M$variable = factor(M$variable,levels = c('MR','area','chem'))

M_d = M

F4_d = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.25) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-40,10)) +
  scale_x_discrete(name = '', labels = c('OSR \nmarginal revenue','OSR area','Agrochemical \nrate')) +
  
  scale_fill_manual(values = colors_vect[4], name = '') + labs(subtitle = '(d) (farmer)') +
  theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
F4_d


M = data.frame(values = c(damage_funct(area_OSR[1],chem[1]),
                          damage_funct(area_OSR[4],chem[4]),
                          Bees(area_OSR[1],chem[1],hives[1],area_G[1]),
                          Bees(area_OSR[4],chem[4],hives[1],area_G[4])))

M = data.frame(diff_values = c(-(M$values[2] - M$values[1])/M$values[1],
                               (M$values[4] - M$values[3])/M$values[3]), 
               scenario = c('AT'),
               variable = c('damage','pollination'),plot = 'e')



M_e = M

F4_e = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.125) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-40,10)) +
  scale_x_discrete(name = '', labels = c('Pesticide damage rate','Pollination')) +
  
  scale_fill_manual(values = colors_vect[4], name = '') + labs(subtitle = '(e)') +
  theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
F4_e



M = data.frame(values = c(prod_marg_beekeeper_x3(area_OSR[1],chem[1],hives[1]),prod_marg_beekeeper_x3(area_OSR[4],chem[4],hives[1]),
                          hives[1],hives[4]))

M = data.frame(diff_values = c((M$values[2] - M$values[1])/M$values[1], 
                               (M$values[4] - M$values[3])/M$values[3]),
               scenario = c('AT'),
               variable = c('MP_x3','hives'),plot = 'f')

M$scenario = factor(M$scenario,levels = c('AT'))
M$variable = factor(M$variable,levels = c('MP_x3','hives'))

M_f = M

F4_f = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.25) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c()) +
  scale_x_discrete(name = '', labels = c('Beehive \nmarginal product','Beehive number')) +
  
  scale_fill_manual(values = colors_vect[4], name = '') + labs(subtitle = '(f) (beekeeper)') +
  theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
F4_f


###########################
M_F4_r1 = rbind(M_a,M_b,M_c)

levels(M_F4_r1$plot) = c('(a) (beekeeper)','(b)','(c) (farmer)')
levels(M_F4_r1$variable) = c('Honey \nmarginal revenue','Beehive number',
                             'Pollination level','OSR \nmarginal product',
                             'OSR area','Pesticide\nmarginal product','Pesticide\nuse')

P1 = ggplot(data= M_F4_r1 , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.5) ) + 
  facet_grid(~plot, scales = 'free',  space = 'free') +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  geom_ribbon(aes(ymin = 0, ymax = 0, fill = 'Z_AT')) +
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c()) +
  scale_x_discrete(name = '') +
  
  scale_fill_manual(values = colors_vect[2:4], name = 'Scenarios', labels = c('HS','PS','PT')) + 
  theme(legend.position = 'right', plot.title = element_text(size = 11), axis.text.x = element_text(size = 8, angle = 45, face = 'bold'), 
        axis.text.y = element_text(size = 8, face = 'bold'),
        strip.text.x = element_text(size=10, hjust = 0))
F4_r1 = P1 +theme(panel.spacing = unit(4, "lines"))

M_F4_r2 = rbind(M_d,M_e,M_f)
levels(M_F4_r2$plot) = c('(d) (farmer)','(e)','(f) (beekeeper)')
levels(M_F4_r2$variable) = c('OSR \nmarginal revenue','OSR area','Pesticide\nuse',
                             'Pesticide\ndamage rate','Pollination level',
                             'Beehive \nmarginal product','Beehive number')

P2 = ggplot(data= M_F4_r2 , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.225) ) + 
  facet_grid(~plot, scales = 'free_x', space = 'free') +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c()) +
  scale_x_discrete(name = '') +
  
  scale_fill_manual(values = colors_vect[4], name = '') + 
  theme(legend.position = 'right', plot.title = element_text(size = 11), axis.text.x = element_text(size = 8, angle = 45, face = 'bold'), 
        axis.text.y = element_text(size = 8, face = 'bold'),
        strip.text.x = element_text(size=10, hjust = 0))
F4_r2 = P2 +theme(panel.spacing = unit(4, "lines"))


ggarrange(F4_r1, F4_r2,
                          ncol=1, nrow=2, common.legend = TRUE, legend= 'right')
                


###

# SENSITIVITY ANALYSIS OSR PRICE ----
p_OSR = 350 ; 

Pub_spend = function(x1 ,x2 ,x3 ,S1 ,S2 ,T1 ){(- T1 * x2 * x1  + S1 * x3  + S2 * F2(x1,x2,x3) )*tot_area}


#LEVEL OF HS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs1(s)[1],Subs1(s)[2],Subs1(s)[3],s,0,0) < Budget){s = s + 1 }
s1_lim = s ; rm(s)

#LEVEL OF PS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs2(s)[1],Subs2(s)[2],Subs2(s)[3],0,s,0) < Budget){s = s + 0.1 }
s2_lim = s ; rm(s)

area_OSR = round(c(Subs1(0)[1],Subs1(s1_lim)[1],Subs2(s2_lim)[1],Tax(t1)[1]),4)
chem = round(c(Subs1(0)[2],Subs1(s1_lim)[2],Subs2(s2_lim)[2],Tax(t1)[2]),4)
hives = round(c(Subs1(0)[3],Subs1(s1_lim)[3],Subs2(s2_lim)[3],Tax(t1)[3]),4)
area_W = round(c(Subs1(0)[4],Subs1(s1_lim)[4],Subs2(s2_lim)[4],Tax(t1)[4]),4)
area_G = round(c(Subs1(0)[5],Subs1(s1_lim)[5],Subs2(s2_lim)[5],Tax(t1)[5]),4)




Provision = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[1:4]
Biodiversity = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[5:8]
Poll = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[9:12]
Water_Pest = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[13:16]
Water_Nutr = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[17:20]
Wild = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[21:24]

I1 = ((Provision - Provision[1])/abs(Provision[1]))*100
I2 = ((Poll - Poll[1])/abs(Poll[1]))*100
I3 = ((Biodiversity - Biodiversity[1])/abs(Biodiversity[1]))*100
I4 = ((Wild - Wild[1])/abs(Wild[1]))*100
I5 = ((Water_Pest - Water_Pest[1])/abs(Water_Pest[1]))*100
I6 = ((Water_Nutr - Water_Nutr[1])/abs(Water_Nutr[1]))*100


Priv_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]) + pi2(area_OSR[1],chem[1],hives[1]),
                pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2]) + pi2_s1(area_OSR[2],chem[2],hives[2]),
                pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3]) + pi2_s2(area_OSR[3],chem[3],hives[3]),
                pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4]) + pi2(area_OSR[4],chem[4],hives[4]))

Tot_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1])*tot_area + pi2(area_OSR[1],chem[1],hives[1])*tot_area - Pub_spend(area_OSR[1],chem[1],hives[1],0,0,0),
               pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2])*tot_area + pi2_s1(area_OSR[2],chem[2],hives[2])*tot_area - Pub_spend(area_OSR[2],chem[2],hives[2],s1_lim,0,0),
               pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3])*tot_area + pi2_s2(area_OSR[3],chem[3],hives[3])*tot_area - Pub_spend(area_OSR[3],chem[3],hives[3],0,s2_lim,0),
               pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4])*tot_area + pi2(area_OSR[4],chem[4],hives[4])*tot_area - Pub_spend(area_OSR[4],chem[4],hives[4],0,0,t1))

I7 = ((Priv_Wealth - Priv_Wealth[1])/abs(Priv_Wealth[1]))*100
I8 = ((Tot_Wealth - Tot_Wealth[1])/abs(Tot_Wealth[1]))*100



I_MATRIX_t1 = data.frame(cbind(I1,I2,I5,I6,I4,I3,I8,I7))
colnames(I_MATRIX_t1) = c('Food/Feed provision','Pollination',
                          '\nWater          \nquality (pesticides)         ',
                          '\nWater          \nquality (nutrients)         ',
                          'Wild bee\nabundance','Plant species\nrichness','\n          Total\n          Wealth','Stakeholders\'\nWealth')


##########################################################################################
p_OSR =  400 


Pub_spend = function(x1 ,x2 ,x3 ,S1 ,S2 ,T1 ){(- T1 * x2 * x1  + S1 * x3  + S2 * F2(x1,x2,x3) )*tot_area}


#LEVEL OF HS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs1(s)[1],Subs1(s)[2],Subs1(s)[3],s,0,0) < Budget){s = s + 1 }
s1_lim = s ; rm(s)

#LEVEL OF PS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs2(s)[1],Subs2(s)[2],Subs2(s)[3],0,s,0) < Budget){s = s + 0.1 }
s2_lim = s ; rm(s)

area_OSR = round(c(Subs1(0)[1],Subs1(s1_lim)[1],Subs2(s2_lim)[1],Tax(t1)[1]),4)
chem = round(c(Subs1(0)[2],Subs1(s1_lim)[2],Subs2(s2_lim)[2],Tax(t1)[2]),4)
hives = round(c(Subs1(0)[3],Subs1(s1_lim)[3],Subs2(s2_lim)[3],Tax(t1)[3]),4)
area_W = round(c(Subs1(0)[4],Subs1(s1_lim)[4],Subs2(s2_lim)[4],Tax(t1)[4]),4)
area_G = round(c(Subs1(0)[5],Subs1(s1_lim)[5],Subs2(s2_lim)[5],Tax(t1)[5]),4)


M_plot = data.frame(area = c(area_G[1],area_OSR[1],area_W[1],
                             area_G[2],area_OSR[2],area_W[2],
                             area_G[3],area_OSR[3],area_W[3],
                             area_G[4],area_OSR[4],area_W[4]), 
                    crop = rep(c('Grasslands','OSR','Wheat'),4),
                    scenario = c(rep('BAU',3),rep('HS',3),rep('PS',3),rep('AT',3)))

M_subplot = M_plot[M_plot$scenario == 'BAU',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_1 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(a) Business as usual (BAU)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('gold','yellow','chartreuse3')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'italic', size = 11))

M_subplot = M_plot[M_plot$scenario == 'HS',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_2 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(b) Hive subsidy (HS)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('gold','yellow','chartreuse3')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'italic', size = 11))

M_subplot = M_plot[M_plot$scenario == 'PS',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_3 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(c) Price subsidy (PS)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('gold','yellow','chartreuse3')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'italic', size = 11))

M_subplot = M_plot[M_plot$scenario == 'AT',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_4 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(d) Pesticide tax (PT)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('gold','yellow','chartreuse3')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'italic', size = 11))

annotate_figure(ggarrange(plot_1,plot_2,plot_3,plot_4,
                          align = 'hv', legend = 'right', nrow = 2, ncol = 2, common.legend = TRUE), top = 'Land uses in four scenarios')




Provision = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[1:4]
Biodiversity = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[5:8]
Poll = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[9:12]
Water_Pest = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[13:16]
Water_Nutr = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[17:20]
Wild = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[21:24]

I1 = ((Provision - Provision[1])/abs(Provision[1]))*100
I2 = ((Poll - Poll[1])/abs(Poll[1]))*100
I3 = ((Biodiversity - Biodiversity[1])/abs(Biodiversity[1]))*100
I4 = ((Wild - Wild[1])/abs(Wild[1]))*100
I5 = ((Water_Pest - Water_Pest[1])/abs(Water_Pest[1]))*100
I6 = ((Water_Nutr - Water_Nutr[1])/abs(Water_Nutr[1]))*100


Priv_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]) + pi2(area_OSR[1],chem[1],hives[1]),
                pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2]) + pi2_s1(area_OSR[2],chem[2],hives[2]),
                pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3]) + pi2_s2(area_OSR[3],chem[3],hives[3]),
                pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4]) + pi2(area_OSR[4],chem[4],hives[4]))

Tot_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1])*tot_area + pi2(area_OSR[1],chem[1],hives[1])*tot_area - Pub_spend(area_OSR[1],chem[1],hives[1],0,0,0),
               pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2])*tot_area + pi2_s1(area_OSR[2],chem[2],hives[2])*tot_area - Pub_spend(area_OSR[2],chem[2],hives[2],s1_lim,0,0),
               pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3])*tot_area + pi2_s2(area_OSR[3],chem[3],hives[3])*tot_area - Pub_spend(area_OSR[3],chem[3],hives[3],0,s2_lim,0),
               pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4])*tot_area + pi2(area_OSR[4],chem[4],hives[4])*tot_area - Pub_spend(area_OSR[4],chem[4],hives[4],0,0,t1))

I7 = ((Priv_Wealth - Priv_Wealth[1])/abs(Priv_Wealth[1]))*100
I8 = ((Tot_Wealth - Tot_Wealth[1])/abs(Tot_Wealth[1]))*100



I_MATRIX_tinf = data.frame(cbind(I2,I1,I5,I6,I4,I3,I7,I8))
colnames(I_MATRIX_tinf) = c('Pollination','Food/Feed provision',
                          '\nWater          \nquality (pesticides)         ',
                          '\nWater          \nquality (nutrients)         ',
                          'Wild bee\nabundance','Plant species\nrichness','Stakeholders\'\nWealth','\n          Total\n          Wealth')


round(I_MATRIX_tinf,1) #Delta indicators compared to BAU
round((I_MATRIX_tinf - I_MATRIX_t1)/I_MATRIX_t1,2)*100 #Delta p_OSR
INT = round((I_MATRIX_tinf - I_MATRIX_t1)/I_MATRIX_t1,2)*100 #Delta 

OUTPUT = data.frame(Poll = NULL,Food = NULL, WQ_pest = NULL, WQ_nutr = NULL, Wild_bee= NULL, Plant = NULL, Private_wealth = NULL, Total_wealth = NULL)
for (i in 1:3){
  for (j in 1:8){
    OUTPUT[i,j] = paste(round(I_MATRIX_tinf[i+1,j],1),'(',INT[i+1,j],')') }}
write.xlsx(OUTPUT, 
           "/home/jerome/Documents/Docs ordi pro/Thèse GREThA 2018.2021/Paper sustainability JAE/#6 Special issue ES/Manuscript/Sensitivity analysis/price_inf.xlsx")
p_OSR = 350


##########################################################################################
p_OSR =  400 


Pub_spend = function(x1 ,x2 ,x3 ,S1 ,S2 ,T1 ){(- T1 * x2 * x1  + S1 * x3  + S2 * F2(x1,x2,x3) )*tot_area}


#LEVEL OF HS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs1(s)[1],Subs1(s)[2],Subs1(s)[3],s,0,0) < Budget){s = s + 1 }
s1_lim = s ; rm(s)

#LEVEL OF PS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs2(s)[1],Subs2(s)[2],Subs2(s)[3],0,s,0) < Budget){s = s + 0.1 }
s2_lim = s ; rm(s)

area_OSR = round(c(Subs1(0)[1],Subs1(s1_lim)[1],Subs2(s2_lim)[1],Tax(t1)[1]),4)
chem = round(c(Subs1(0)[2],Subs1(s1_lim)[2],Subs2(s2_lim)[2],Tax(t1)[2]),4)
hives = round(c(Subs1(0)[3],Subs1(s1_lim)[3],Subs2(s2_lim)[3],Tax(t1)[3]),4)
area_W = round(c(Subs1(0)[4],Subs1(s1_lim)[4],Subs2(s2_lim)[4],Tax(t1)[4]),4)
area_G = round(c(Subs1(0)[5],Subs1(s1_lim)[5],Subs2(s2_lim)[5],Tax(t1)[5]),4)


area_OSR = round(c(Subs1(0)[1],Subs1(s1_lim)[1],Subs2(s2_lim)[1],Tax(t1)[1]),4)
chem = round(c(Subs1(0)[2],Subs1(s1_lim)[2],Subs2(s2_lim)[2],Tax(t1)[2]),4)
hives = round(c(Subs1(0)[3],Subs1(s1_lim)[3],Subs2(s2_lim)[3],Tax(t1)[3]),4)
area_W = round(c(Subs1(0)[4],Subs1(s1_lim)[4],Subs2(s2_lim)[4],Tax(t1)[4]),4)
area_G = round(c(Subs1(0)[5],Subs1(s1_lim)[5],Subs2(s2_lim)[5],Tax(t1)[5]),4)


M_plot = data.frame(area = c(area_G[1],area_OSR[1],area_W[1],
                             area_G[2],area_OSR[2],area_W[2],
                             area_G[3],area_OSR[3],area_W[3],
                             area_G[4],area_OSR[4],area_W[4]), 
                    crop = rep(c('Grasslands','OSR','Wheat'),4),
                    scenario = c(rep('BAU',3),rep('HS',3),rep('PS',3),rep('AT',3)))

M_subplot = M_plot[M_plot$scenario == 'BAU',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_1 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(a) Business as usual (BAU)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('gold','yellow','chartreuse3')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'italic', size = 11))

M_subplot = M_plot[M_plot$scenario == 'HS',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_2 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(b) Hive subsidy (HS)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('gold','yellow','chartreuse3')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'italic', size = 11))

M_subplot = M_plot[M_plot$scenario == 'PS',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_3 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(c) Price subsidy (PS)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('gold','yellow','chartreuse3')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'italic', size = 11))

M_subplot = M_plot[M_plot$scenario == 'AT',]
M_subplot$crop = factor(M_subplot$crop, levels = c('Wheat','OSR','Grasslands'))
M_subplot$pos = cumsum(M_subplot$area) - 0.5*M_subplot$area 

plot_4 = ggplot(M_subplot, aes(x='', y=area, fill=crop))+  
  geom_bar(width = 1, stat = "identity", color = "white") +  theme_void() +
  labs(x = '', y = 'Land use (%)', fill = 'Crop', title = '(d) Pesticide tax (PT)') +
  geom_text(aes(y = pos, label = paste(round(area*100,1),'%')), color = c('black')) + 
  scale_fill_manual(values = c('gold','yellow','chartreuse3')) +
  coord_polar("y", start=0)  + theme(plot.title = element_text(face = 'italic', size = 11))

annotate_figure(ggarrange(plot_1,plot_2,plot_3,plot_4,
                          align = 'hv', legend = 'right', nrow = 2, ncol = 2, common.legend = TRUE), top = 'Land uses in four scenarios')




Provision = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[1:4]
Biodiversity = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[5:8]
Poll = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[9:12]
Water_Pest = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[13:16]
Water_Nutr = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[17:20]
Wild = ecosystem_services(area_OSR,chem,hives,area_W,area_G)[21:24]

I1 = ((Provision - Provision[1])/abs(Provision[1]))*100
I2 = ((Poll - Poll[1])/abs(Poll[1]))*100
I3 = ((Biodiversity - Biodiversity[1])/abs(Biodiversity[1]))*100
I4 = ((Wild - Wild[1])/abs(Wild[1]))*100
I5 = ((Water_Pest - Water_Pest[1])/abs(Water_Pest[1]))*100
I6 = ((Water_Nutr - Water_Nutr[1])/abs(Water_Nutr[1]))*100


Priv_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1]) + pi2(area_OSR[1],chem[1],hives[1]),
                pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2]) + pi2_s1(area_OSR[2],chem[2],hives[2]),
                pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3]) + pi2_s2(area_OSR[3],chem[3],hives[3]),
                pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4]) + pi2(area_OSR[4],chem[4],hives[4]))

Tot_Wealth = c(pi1(area_OSR[1],chem[1],hives[1],area_W[1],area_G[1])*tot_area + pi2(area_OSR[1],chem[1],hives[1])*tot_area - Pub_spend(area_OSR[1],chem[1],hives[1],0,0,0),
               pi1(area_OSR[2],chem[2],hives[2],area_W[2],area_G[2])*tot_area + pi2_s1(area_OSR[2],chem[2],hives[2])*tot_area - Pub_spend(area_OSR[2],chem[2],hives[2],s1_lim,0,0),
               pi1(area_OSR[3],chem[3],hives[3],area_W[3],area_G[3])*tot_area + pi2_s2(area_OSR[3],chem[3],hives[3])*tot_area - Pub_spend(area_OSR[3],chem[3],hives[3],0,s2_lim,0),
               pi1_tax(area_OSR[4],chem[4],hives[4],area_W[4],area_G[4])*tot_area + pi2(area_OSR[4],chem[4],hives[4])*tot_area - Pub_spend(area_OSR[4],chem[4],hives[4],0,0,t1))

I7 = ((Priv_Wealth - Priv_Wealth[1])/abs(Priv_Wealth[1]))*100
I8 = ((Tot_Wealth - Tot_Wealth[1])/abs(Tot_Wealth[1]))*100



I_MATRIX_tsup = data.frame(cbind(I2,I1,I5,I6,I4,I3,I7,I8))
colnames(I_MATRIX_tsup) = c('Pollination','Food/Feed provision',
                            '\nWater          \nquality (pesticides)         ',
                            '\nWater          \nquality (nutrients)         ',
                            'Wild bee\nabundance','Plant species\nrichness','Stakeholders\'\nWealth','\n          Total\n          Wealth')


round(I_MATRIX_tsup,1) #Delta indicators compared to BAU
round((I_MATRIX_tsup - I_MATRIX_t1)/I_MATRIX_t1,2)*100 #Delta p_OSR
INT = round((I_MATRIX_tsup - I_MATRIX_t1)/I_MATRIX_t1,2)*100 #Delta 

OUTPUT = data.frame(Poll = NULL,Food = NULL, WQ_pest = NULL, WQ_nutr = NULL, Wild_bee= NULL, Plant = NULL, Private_wealth = NULL, Total_wealth = NULL)
for (i in 1:3){
  for (j in 1:8){
    OUTPUT[i,j] = paste(round(I_MATRIX_tsup[i+1,j],1),'(',INT[i+1,j],')') }}
write.xlsx(OUTPUT, 
           "/home/jerome/Documents/Docs ordi pro/Thèse GREThA 2018.2021/Paper sustainability JAE/#6 Special issue ES/Manuscript/Sensitivity analysis/price_sup.xlsx")
p_OSR = 350




