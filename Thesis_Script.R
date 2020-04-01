# Austin Scheetz
# Yale University
# Thesis Script

setwd("C:/Users/austi/Documents/Yale/Thesis/Data")

#######   Libraries and functions   #######  
# __________________________________________ #

library(tidyverse)
library(mice)
library(PerformanceAnalytics)
library(sm)
library(BAMMtools)
library(Deriv)
library(rgenoud)
library(tcltk)
library(gridExtra)
library(grid)
library(rowr)
library(rootSolve)

select <- dplyr::select

source("http://www.reuningscherer.net/STAT660/R/CSQPlot.r.txt")

CES <- function(C,G,a,p) (a*C^p+(1-a)*G^p)^(1/p)
rev.CES <- function(C,a,p,U) ((U^p-a*C^p)/(1-a))^(1/p)

#######   Code before Python treatment   #######
# ____________________________________________ #

# Load in datasets, downloaded from Survey Solutions server
data1 <- read.table("data1", sep = "\t", header = TRUE, as.is = TRUE)
data2 <- read.table("data2", sep = "\t", header = TRUE, as.is = TRUE)
data3 <- read.table("data3", sep = "\t", header = TRUE, as.is = TRUE)
data4 <- read.table("data4", sep = "\t", header = TRUE, as.is = TRUE)
data5 <- read.table("data5", sep = "\t", header = TRUE, as.is = TRUE)

# Adding variables for risk game order
data1$risk_order <- 1
data2$risk_order <- 1
data3$risk_order <- 0
data4$risk_order <- 0
data5$risk_order <- 0

# Recording dataset number
data1$dataset <- 1
data2$dataset <- 2
data3$dataset <- 3
data4$dataset <- 4
data5$dataset <- 5

# Combining datasets
pastoralist <- rbind.fill(data1, data2, data3, data4, data5)

# Export combined data to work with in Python
write.csv(pastoralist, "pastoralist.csv")

#######   Remaining data cleaning   #######
# _______________________________________ #

## Load in data from Python
pastoralist <- read.csv("cleaned_job.csv", as.is = TRUE, header = TRUE)

## Cleaning
pastoralist$education[pastoralist$education == "o"] <- 0
pastoralist$education[pastoralist$education == "primary school"] <- 8
pastoralist$education[pastoralist$education == "primary school 3"] <- 3
pastoralist$education[pastoralist$education == "primary school 4"] <- 4
pastoralist$education[pastoralist$education == "primary school 5"] <- 5
pastoralist$education[pastoralist$education == "primary school 6"] <- 6
pastoralist$education[pastoralist$education == "primary school 7"] <- 7
pastoralist$education[pastoralist$education == "primary school 8"] <- 8
pastoralist$education[pastoralist$education == "primary 8"] <- 8
pastoralist$education[pastoralist$education == "primary  8"] <- 8
pastoralist$education[pastoralist$education == "secondary school"] <- 12
pastoralist$education[pastoralist$education == "secondary 3"] <- 11
pastoralist$education[pastoralist$education == "secondary  3"] <- 11
pastoralist$education[pastoralist$education == "secondary 9"] <- 9
pastoralist$education[pastoralist$education == "secondary school 1"] <- 9
pastoralist$education[pastoralist$education == "secondary school 2"] <- 10
pastoralist$education[pastoralist$education == "secondary school 2yrs"] <- 10
pastoralist$education[pastoralist$education == "secondary school 3"] <- 11
pastoralist$education[pastoralist$education == "secondary school 12"] <- 12
pastoralist$education[pastoralist$education == "secondary school  12"] <- 12
pastoralist$education[pastoralist$education == "college"] <- 14
pastoralist$education[pastoralist$education == "university"] <- 16
pastoralist$education[pastoralist$education == "university 4"] <- 16
pastoralist$education <- as.numeric(pastoralist$education)

pastoralist$num_camels[pastoralist$camels == 0] <- 0

pastoralist$tv[pastoralist$electric == 0] <- 0

pastoralist$num_daughter[pastoralist$children == 0] <- 0
pastoralist$num_son[pastoralist$children == 0] <- 0
pastoralist$school_daughters[pastoralist$num_daughter == 0] <- 0
pastoralist$cu_daughters[pastoralist$num_daughter == 0] <- 0
pastoralist$school_sons[pastoralist$num_son == 0] <- 0
pastoralist$cu_sons[pastoralist$num_son == 0] <- 0

pastoralist$cattle_purchase[pastoralist$cattle_purchase == 9999] <- 0

pastoralist$heifer_sim[pastoralist$heifer_sim == 9999] <- NA
pastoralist$heifer_goat[pastoralist$heifer_goat == 9999] <- NA
pastoralist$heifer_cash[pastoralist$heifer_cash == 9999] <- NA
pastoralist$heifer_maize[pastoralist$heifer_maize == 9999] <- NA
pastoralist$heifer_smartphone[pastoralist$heifer_smartphone == 9999] <- NA
pastoralist$heifer_school_daughter[pastoralist$heifer_school_daughter == 9999] <- NA
pastoralist$heifer_mpala[pastoralist$heifer_mpala == 9999] <- NA
pastoralist$heifer_school_son[pastoralist$heifer_school_son == 9999] <- NA
pastoralist$heifer_steer[pastoralist$heifer_steer == 9999] <- NA
pastoralist$heifer_electricity[pastoralist$heifer_electricity == 9999] <-NA
pastoralist$buy_price[pastoralist$buy_price == 9999] <- NA

pastoralist$num_heifers[pastoralist$num_heifers < 1] <- 0

pastoralist$num_animals_formal[pastoralist$num_animals_formal == "o"] <- 0
pastoralist$num_animals_formal[pastoralist$num_animals_formal == "oljogi"] <- NA
pastoralist$num_animals_formal[pastoralist$num_animals_formal == "6o"] <- 60
pastoralist$num_animals_formal <- as.numeric(pastoralist$num_animals_formal)

pastoralist$num_animals_informal[pastoralist$num_animals_informal == "o"] <- 0
pastoralist$num_animals_informal <- as.numeric(pastoralist$num_animals_informal)

pastoralist$ranch_750ksh_cattle[pastoralist$ranch_750ksh_cattle == 150 & pastoralist$ranch_500ksh_ranch == 1] <- 15

pastoralist$ranch_500ksh_ranch[pastoralist$Respondent_ID == "15-65-47-73"] <- 40

# Reordering cattle on ranch vs total for cases where they were recorded backwards
pastoralist[pastoralist$ranch_free_ranch > pastoralist$ranch_free_cattle, c("ranch_free_ranch", "ranch_free_cattle")] <- pastoralist[pastoralist$ranch_free_ranch > pastoralist$ranch_free_cattle, c("ranch_free_cattle", "ranch_free_ranch")]
pastoralist[pastoralist$ranch_250ksh_ranch > pastoralist$ranch_250ksh_cattle, c("ranch_250ksh_ranch", "ranch_250ksh_cattle")] <- pastoralist[pastoralist$ranch_250ksh_ranch > pastoralist$ranch_250ksh_cattle, c("ranch_250ksh_cattle", "ranch_250ksh_ranch")]
pastoralist[pastoralist$ranch_500ksh_ranch > pastoralist$ranch_500ksh_cattle, c("ranch_500ksh_ranch", "ranch_500ksh_cattle")] <- pastoralist[pastoralist$ranch_500ksh_ranch > pastoralist$ranch_500ksh_cattle, c("ranch_500ksh_cattle", "ranch_500ksh_ranch")]
pastoralist[pastoralist$ranch_750ksh_ranch > pastoralist$ranch_750ksh_cattle, c("ranch_750ksh_ranch", "ranch_750ksh_cattle")] <- pastoralist[pastoralist$ranch_750ksh_ranch > pastoralist$ranch_750ksh_cattle, c("ranch_750ksh_cattle", "ranch_750ksh_ranch")]
pastoralist[pastoralist$ranch_1000ksh_ranch > pastoralist$ranch_1000ksh_cattle, c("ranch_1000ksh_ranch", "ranch_1000ksh_cattle")] <- pastoralist[pastoralist$ranch_1000ksh_ranch > pastoralist$ranch_1000ksh_cattle, c("ranch_1000ksh_cattle", "ranch_1000ksh_ranch")]

# Cleaning livestock demand responses
pastoralist$cgr_250  <- pastoralist$ranch_250ksh_cattle/pastoralist$ranch_250ksh_goat
pastoralist$ranch_250ksh_cattle <- ifelse(pastoralist$cgr_250 > 7, pastoralist$ranch_250ksh_goat, pastoralist$ranch_250ksh_cattle)
pastoralist$ranch_250ksh_goat   <- ifelse(pastoralist$cgr_250 > 7, 150, pastoralist$ranch_250ksh_goat)

for (i in 1:nrow(pastoralist)){
  if(pastoralist$cgr_250[i] > 1){
    if(pastoralist$ranch_250ksh_goat[i] == pastoralist$ranch_250ksh_cattle[i]){
      c <- pastoralist$ranch_250ksh_cattle[i]
      g <- pastoralist$ranch_250ksh_goat[i]
      pastoralist$ranch_250ksh_cattle <- g
      pastoralist$ranch_250ksh_goat   <- c
    }
  }
}

pastoralist$cgr_250 <- NULL

#######   Creating variables   #######
# __________________________________ #

pastoralist$herd  <- pastoralist$num_heifers + pastoralist$num_steers
pastoralist$herd  <- ifelse(pastoralist$cattle_ranch > pastoralist$herd, pastoralist$cattle_ranch, pastoralist$herd)
pastoralist$ranch <- ifelse(pastoralist$cattle_ranch == 0, 0, 1)

#######   CES preparation   #######
# _______________________________ #

###   Prepping dataset for regressions
xast <- pastoralist[, c("ranch_free_cattle", "ranch_250ksh_cattle", "ranch_500ksh_cattle", "ranch_750ksh_cattle", "ranch_1000ksh_cattle","ranch_free_goat", "ranch_250ksh_goat", "ranch_500ksh_goat", "ranch_750ksh_goat", "ranch_1000ksh_goat", "num_goats","herd","Respondent_ID")]

# Data cleaning that will allow code to run smoothly later
for (i in 1:nrow(xast)){
  for (j in 1:4){
    xast[i,j]   <- ifelse(xast[i,j] < xast[i,j+1], xast[i,j+1], xast[i,j]) 
    xast[i,j+5] <- ifelse(xast[i,j+5] < xast[i,j+6], xast[i,j+6], xast[i,j+5]) 
  }
}

## Total cattle
# First, getting total cattle curve info and adding to the corresponding row in the original data
for (i in 1:nrow(xast)){
  # Create a data frame containing demand curve data for each person
  df <- data.frame(price = seq(from = 0, to = 1000, by = 250), quan = c(xast[i,1], xast[i,2], xast[i,3], xast[i,4], xast[i,5]))
  for(j in 1:4){
    if(is.na(df$quan[j]) == FALSE){
      if(df$quan[j] == 0){
        df$quan[j+1:(nrow(df)-j)] <- NA # Set any redundant 0's to NA
      }
    }
  }
  
  # This concludes the data prep portion of this loop
  
  if(length(unique(df$quan)) > 1){
    
    reggie <- lm(df$price ~ df$quan + I(df$quan^2))
    
    xast$intercept.c[i]  <- reggie$coefficients[1]
    xast$slope.c[i]      <- reggie$coefficients[2]
    xast$slope2.c[i]     <- reggie$coefficients[3]
    
  } else{
    if(length(unique(df$quan)) == 1){
      xast$intercept.c[i]  <- NA
      xast$slope.c[i]      <- Inf
      xast$slope2.c[i]     <- NA
    } 
  }
}
# Now, creating a regression equation and adding to the df
for (i in 1:nrow(xast)){
  xast$eqn.c[i] <- paste(xast$intercept.c[i]," + ",xast$slope.c[i]," * q + ",xast$slope2.c[i]," * q^2",sep="")
  
  if(is.na(xast$slope2.c[i]) == TRUE){
    xast$eqn.c[i] <- paste(xast$intercept.c[i], " + ", xast$slope.c[i]," * q", sep = "")
  }
  
  if(is.infinite(xast$slope.c[i]) == TRUE){
    xast$eqn.c[i] <- paste("Inelastic at quan = ", xast$ranch_free_cattle[i], sep = "")
  }
  
  if(is.na(xast$slope.c[i]) == TRUE){
    xast$eqn.c[i] <- 0
  }
}
# Now deriving to get slope
for (i in 1:nrow(xast)){
  if(is.finite(xast$slope.c[i]) == TRUE){
    xast$derivative.c[i] <- Deriv(xast$eqn.c[i], "q")
  } else xast$derivative.c[i] <- NA
}

## Goats
# First, getting goat curve info and adding to the corresponding row in the original data
for (i in 1:nrow(xast)){
  # Create a data frame containing demand curve data for each person
  df <- data.frame(price = seq(from = 0, to = 1000, by = 250), quan = c(xast[i,6], xast[i,7], xast[i,8], xast[i,9], xast[i,10]))
  for(j in 1:4){
    if(is.na(df$quan[j]) == FALSE){
      if(df$quan[j] == 0){
        df$quan[j+1:(nrow(df)-j)] <- NA # Set any redundant 0's to NA
      }
    }
  }
  
  # This concludes the data prep portion of this loop
  
  if(length(unique(df$quan)) > 1){
    
    reggie <- lm(df$price ~ df$quan + I(df$quan^2))
    
    xast$intercept.g[i]  <- reggie$coefficients[1]
    xast$slope.g[i]      <- reggie$coefficients[2]
    xast$slope2.g[i]     <- reggie$coefficients[3]
    
  } else{
    if(length(unique(df$quan)) == 1){
      xast$intercept.g[i]  <- NA
      xast$slope.g[i]      <- Inf
      xast$slope2.g[i]     <- NA
    } 
  }
}
# Now, creating a regression equation and adding to the df
for (i in 1:nrow(xast)){
  xast$eqn.g[i] <- paste(xast$intercept.g[i]," + ",xast$slope.g[i]," * q + ",xast$slope2.g[i]," * q^2",sep="")
  
  if(is.na(xast$slope2.g[i]) == TRUE){
    xast$eqn.g[i] <- paste(xast$intercept.g[i], " + ", xast$slope.g[i]," * q", sep = "")
  }
  
  if(is.infinite(xast$slope.g[i]) == TRUE){
    xast$eqn.g[i] <- paste("Inelastic at quan = ", xast$ranch_free_goat[i], sep = "")
  }
  
  if(is.na(xast$slope.g[i]) == TRUE){
    xast$eqn.g[i] <- 0
  }
}
# Now deriving to get slope
for (i in 1:nrow(xast)){
  if(is.finite(xast$slope.g[i]) == TRUE){
    xast$derivative.g[i] <- Deriv(xast$eqn.g[i], "q")
  } else xast$derivative.g[i] <- NA
}

rm(df,reggie,i,j)

# Adding this information to main dataset
xast.merge <- data.frame(Respondent_ID = xast$Respondent_ID, 
                         int.c = xast$intercept.c,
                         b1.c  = xast$slope.c,
                         b2.c  = xast$slope2.c,
                         int.g = xast$intercept.g,
                         b1.g  = xast$slope.g,
                         b2.g  = xast$slope2.g)

pastoralist <- merge(pastoralist, xast.merge, by = "Respondent_ID", all = TRUE)

### Calculating MRTS for each respondent
df.mrts <- xast[,c("ranch_free_cattle", "ranch_250ksh_cattle", "ranch_500ksh_cattle", "ranch_750ksh_cattle", "ranch_1000ksh_cattle","ranch_free_goat", "ranch_250ksh_goat", "ranch_500ksh_goat", "ranch_750ksh_goat", "ranch_1000ksh_goat", "Respondent_ID", "derivative.c", "derivative.g")]

for (i in 1:nrow(df.mrts)){
  if(is.na(df.mrts$derivative.c[i]) == FALSE & is.na(df.mrts$derivative.g[i]) == FALSE){
    for (j in 1:5){
      # Plug cattle quantity into its derivative and record slope in new object
      eq <- parse(text = df.mrts$derivative.c[i])
      df <- data.frame(q = df.mrts[i,j])
      cattleslope <- eval(eq, envir = df)
      
      # Do the same for goats
      geq <- parse(text = df.mrts$derivative.g[i])
      gdf <- data.frame(q = df.mrts[i,j+5])
      goatslope <- eval(geq, envir = gdf)
      
      # Divide cattle slope by goat slope and append to original dataset
      if(j == 1){
        df.mrts$mrts0[i]    <- cattleslope/goatslope
      }else if(j == 2){
        df.mrts$mrts250[i]  <- cattleslope/goatslope
      }else if(j == 3){
        df.mrts$mrts500[i]  <- cattleslope/goatslope
      }else if(j == 4){
        df.mrts$mrts750[i]  <- cattleslope/goatslope
      }else{
        df.mrts$mrts1000[i] <- cattleslope/goatslope
      }
    }
  }else{
    df.mrts$mrts0[i]    <- NA
    df.mrts$mrts250[i]  <- NA
    df.mrts$mrts500[i]  <- NA
    df.mrts$mrts750[i]  <- NA
    df.mrts$mrts1000[i] <- NA
    # Each person ends up with 5 MRTS values, unless they are inelastic in cattle or goats
  }
}
rm(gdf)

### Fixing duplicate MRTS's so that regression runs later
df.mrts$n.unique <- apply(df.mrts[,c(14:18)], 1, function(x) length(unique(na.omit(x))))
for(i in 1:nrow(df.mrts)){
  if(df.mrts$n.unique[i] < 2){
    
    df.mrts$mrts0[i]    <- NA
    df.mrts$mrts250[i]  <- NA
    df.mrts$mrts500[i]  <- NA
    df.mrts$mrts750[i]  <- NA
    df.mrts$mrts1000[i] <- NA
  }
}

### Creating long data frame that is suitable for regression
df.reg <- data.frame(price = seq(0,1000,250), 
                     cattle = c(df.mrts$ranch_free_cattle[1], 
                                df.mrts$ranch_250ksh_cattle[1], 
                                df.mrts$ranch_500ksh_cattle[1], 
                                df.mrts$ranch_750ksh_cattle[1], 
                                df.mrts$ranch_1000ksh_cattle[1]), 
                     goat = c(df.mrts$ranch_free_goat[1], 
                              df.mrts$ranch_250ksh_goat[1], 
                              df.mrts$ranch_500ksh_goat[1], 
                              df.mrts$ranch_750ksh_goat[1], 
                              df.mrts$ranch_1000ksh_goat[1]), 
                     mrts = c(df.mrts$mrts0[1], 
                              df.mrts$mrts250[1], 
                              df.mrts$mrts500[1], 
                              df.mrts$mrts750[1], 
                              df.mrts$mrts1000[1]),
                     Respondent_ID = rep(df.mrts$Respondent_ID[1], 5))

for(i in 2:nrow(df.mrts)){
  df <- data.frame(price = seq(0,1000,250), 
                   cattle = c(df.mrts$ranch_free_cattle[i], 
                              df.mrts$ranch_250ksh_cattle[i], 
                              df.mrts$ranch_500ksh_cattle[i], 
                              df.mrts$ranch_750ksh_cattle[i], 
                              df.mrts$ranch_1000ksh_cattle[i]), 
                   goat = c(df.mrts$ranch_free_goat[i], 
                            df.mrts$ranch_250ksh_goat[i], 
                            df.mrts$ranch_500ksh_goat[i], 
                            df.mrts$ranch_750ksh_goat[i], 
                            df.mrts$ranch_1000ksh_goat[i]), 
                   mrts = c(df.mrts$mrts0[i], 
                            df.mrts$mrts250[i], 
                            df.mrts$mrts500[i], 
                            df.mrts$mrts750[i], 
                            df.mrts$mrts1000[i]),
                   Respondent_ID = rep(df.mrts$Respondent_ID[i], 5))
  df.reg <- rbind(df.reg, df)
  rm(df)
}


#######   Computing CES parameters and income   ########
# ____________________________________________________ #

# Computing the variables for regression specification
df.reg$CoG  <- ifelse(df.reg$goat != 0, df.reg$cattle/df.reg$goat, NA)
df.reg$CoG[is.finite(df.reg$CoG) == FALSE] <- NA
df.reg$CoG[df.reg$CoG == 0] <- 0.01

df.reg$mrts[df.reg$mrts < 0]  <- NA
df.reg$mrts[df.reg$mrts == 0] <- 1

# Running the regression (with individual fixed effects)
ces.model <- lm(log(df.reg$mrts) ~ log(df.reg$CoG) + df.reg$Respondent_ID + log(df.reg$CoG):df.reg$Respondent_ID)

# Extracting regression results and placing in dataframe
a.est  <- coef(ces.model)[1]
b.est  <- coef(ces.model)[2]
gammas <- coef(ces.model)[3:69]
kappas <- coef(ces.model)[70:136]

coeffs <- data.frame(Respondent_ID = unique(ces.model[["model"]][["df.reg$Respondent_ID"]])[-1], 
                     gamma = gammas, 
                     kappa = kappas)

tmp <- data.frame(Respondent_ID = unique(ces.model[["model"]][["df.reg$Respondent_ID"]])[1],
                  gamma = 0, kappa = 0)

coeffs <- rbind(coeffs, tmp)

# Using regression estimates to calculate CES parameters
A <- exp(a.est)/(1+exp(a.est))
coeffs$B <- (A/(1-A))*exp(coeffs$gamma)
coeffs$alpha <- coeffs$B/(coeffs$B + 1)
summary(coeffs$alpha)
coeffs$B <- NULL

coeffs$rho   <- ifelse(is.na(coeffs$kappa) == FALSE, coeffs$kappa + b.est + 1, b.est + 1)
summary(coeffs$rho)

# Appending coefficients to original dataset
pastoralist <- merge(pastoralist, coeffs, by = "Respondent_ID", all = TRUE)

# Using average parameters to fill in the gaps
pastoralist$alpha <- ifelse(is.na(pastoralist$alpha) == TRUE, mean(coeffs$alpha), pastoralist$alpha)

pastoralist$rho <- ifelse(is.na(pastoralist$rho) == TRUE, mean(coeffs$rho), pastoralist$rho)

# Computing CES income using these parameters
pastoralist <- pastoralist %>% mutate(CES.income = ((alpha*herd^rho)+((1-alpha)*num_goats^rho))^(1/rho))
summary(pastoralist$CES.income)

rm(coeffs, tmp, df.mrts, df.reg, xast)

#######   Creating market demand curve   #######
# _________________________________________ #

# Creating demand curve
past <- pastoralist[, c("ranch_free_ranch", "ranch_250ksh_ranch", "ranch_500ksh_ranch", "ranch_750ksh_ranch", "ranch_1000ksh_ranch")]

df.start  <-  data.frame(price = seq(from = 0, to = 1000, by = 250), quan = c(past[1,1], past[1,2], past[1,3], past[1,4], past[1,5]))
df.overall <- df.start

for (i in 2:nrow(past)){
  df <- data.frame(price = seq(from = 0, to = 1000, by = 250), quan = c(past[i,1], past[i,2], past[i,3], past[i,4], past[i,5]))
  df.overall <- rbind(df.overall,df)
}

df.agg <- data.frame(price = seq(0, 1000, by = 250), 
                     quan = c(sum(df.overall$quan[df.overall$price == 0]), 
                              sum(df.overall$quan[df.overall$price == 250]), 
                              sum(df.overall$quan[df.overall$price == 500]),
                              sum(df.overall$quan[df.overall$price == 750]), 
                              sum(df.overall$quan[df.overall$price == 1000])))

demand.curve <- lm(df.agg$price ~ df.agg$quan)
summary(demand.curve)

demand.eq <- expression(demand.curve[["coefficients"]][1] + 
                          demand.curve[["coefficients"]][2]*q)

## Computing market clearing price
obser <- sum(pastoralist$cattle_ranch, na.rm = TRUE)
mkt.price <- eval(demand.eq, envir = data.frame(q = obser))

## Computing aggregate cash value of surplus transfer
area <- obser*(mkt.price-450)

rm(df.start, df.overall, df, past)

#######   Simulating the peace payment   #######
# ____________________________________________ #

# Using implicit function theorem to calculate slope of the IC at the observed (C,G) point for each respondent
pastoralist <- pastoralist %>% mutate(slope.CES = -(alpha*herd^(rho-1))/((1-alpha)*num_goats^(rho-1)))
pastoralist$slope.CES[is.infinite(pastoralist$slope.CES) == TRUE] <- NA

# Getting y intercept of the current budget constraint ("goat intercept") which will stay constant
pastoralist <- pastoralist %>% mutate(G1 = num_goats - slope.CES*herd)

# Getting x intercept of the current budget constraint ("cattle intercept") which will change
pastoralist <- pastoralist %>% mutate(C1 = -G1/slope.CES)
pastoralist$C1[is.infinite(pastoralist$C1) == TRUE] <- NA

# Getting each respondent's share of the surplus transfer
pastoralist <- pastoralist %>% mutate(cash.share = cattle_ranch/obser)
pastoralist <- pastoralist %>% mutate(permit.share = (cash.share*area)/450)

# Getting the slope of the new budget line
pastoralist <- pastoralist %>% mutate(b = ifelse(C1-permit.share <= 0, NA, G1/(C1-permit.share)) )

# Getting new number of goats that lines up with this new budget constraint
pastoralist <- pastoralist %>% mutate(tau = (b*(1-alpha)/alpha)^(1/(rho-1)) )
pastoralist <- pastoralist %>% mutate(new.goats = G1/(1+b*tau))

# Plugging in to budget constraint to get new number of cattle
pastoralist <- pastoralist %>% mutate(new.cattle = (G1-new.goats)/b)

# Extracting the people using a ranch
users <- pastoralist[pastoralist$ranch == 1,]

# Creating classes for expansion behavior
users$cgr.now <- users$herd/users$num_goats
users$cgr.before <- users$new.cattle/users$new.goats
users$pp.behavior.ces <- ifelse(users$cgr.now < users$cgr.before, "Expand goats", 
                                      ifelse(users$cgr.now > users$cgr.before, "Expand cattle",
                                             "Scale up"))

users$TLU <- users$herd + 0.2*users$num_goats
users$new.TLU <- users$new.cattle + 0.2*users$new.goats

# Investigating the change
summary(users$herd-users$new.cattle)
summary(users$num_goats-users$new.goats)

summary(users$cgr.now - users$cgr.before)
summary(users$cgr.now)
summary(users$cgr.before)

table(users$pp.behavior.ces)

#######   Simulating market absence   #######
# _________________________________________ #

# Getting choke price and computing consumer surplus
choke  <- dc.coeff[1]
consur <- ((choke-mkt.price)*obser/2)
pastoralist <- pastoralist %>% mutate(CS.share = cash.share*consur)
pastoralist <- pastoralist %>% mutate(CS.ps = (CS.share/450) + permit.share)

# Getting the slope of the new budget line
pastoralist <- pastoralist %>% mutate(b.CS = ifelse(C1-CS.ps <= 0, NA, G1/(C1-CS.ps)) )

# Getting new number of goats that lines up with this new budget constraint
pastoralist <- pastoralist %>% mutate(tau.CS = (b.CS*(1-alpha)/alpha)^(1/(rho-1)) )
pastoralist <- pastoralist %>% mutate(new.goats.CS = G1/(1+b.CS*tau.CS))

# Plugging in to budget constraint to get new number of cattle
pastoralist <- pastoralist %>% mutate(new.cattle.CS = (G1-new.goats.CS)/b.CS)

# Extracting the people using a ranch
users <- pastoralist[pastoralist$ranch == 1,]

# Creating classes for expansion behavior
users$cgr.now.CS <- users$new.cattle/users$new.goats
users$cgr.before.CS <- users$new.cattle.CS/users$new.goats.CS
users$CS.behavior.ces <- ifelse(users$cgr.now.CS < users$cgr.before.CS, "Expand goats", 
                                ifelse(users$cgr.now.CS > users$cgr.before.CS, "Expand cattle",
                                       "Scale up"))

users$new.TLU.CS <- users$new.cattle.CS + 0.2*users$new.goats.CS

# Investigating the change
summary(users$new.cattle-users$new.cattle.CS)
summary(users$new.goats-users$new.goats.CS)

summary(users$cgr.now.CS - users$cgr.before.CS)
summary(users$cgr.now.CS)
summary(users$cgr.before.CS)

table(users$CS.behavior.ces)

#######   Computing number of animals on group ranch   ########
# ___________________________________________________________ #

ngombe <- pastoralist %>% select(Respondent_ID, new.cattle.CS, new.goats.CS, 
                                 new.cattle, new.goats, herd, num_goats, cattle_ranch)

zast <- pastoralist[, c("ranch_free_ranch", "ranch_250ksh_ranch", "ranch_500ksh_ranch", "ranch_750ksh_ranch", "ranch_1000ksh_ranch","Respondent_ID")]

# Data cleaning that will allow code to run smoothly later
for (i in 1:nrow(zast)){
  for (j in 1:4){
    zast[i,j]   <- ifelse(zast[i,j] < zast[i,j+1], zast[i,j+1], zast[i,j])
  }
}

# Extracting individual demand at market clearing price
for (i in 1:nrow(zast)){
  # Create a data frame containing demand curve data for each person
  df <- data.frame(price = seq(from = 0, to = 1000, by = 250), quan = c(zast[i,1], zast[i,2], zast[i,3], zast[i,4], zast[i,5]))
  for(j in 1:4){
    if(is.na(df$quan[j]) == FALSE){
      if(df$quan[j] == 0){
        df$quan[j+1:(nrow(df)-j)] <- NA # Set any redundant 0's to NA
      }
    }
  }
  
  # This concludes the data prep portion of this loop
  
  if(length(unique(df$quan)) > 1){
    
    reggie <- lm(df$price ~ df$quan)
    
    inter <- reggie$coefficients[1]
    slope <- reggie$coefficients[2]
    
    zast$mkt.c.demand[i] <- ifelse((mkt.price - inter)/slope > 0, (mkt.price - inter)/slope, 0)
    
  } else{
    if(length(unique(df$quan)) == 1){
      zast$mkt.c.demand[i] <- NA
    } 
  }
}
rm(reggie, inter, slope, df)

# Appending to dataset
zast   <- zast %>% select(Respondent_ID, mkt.c.demand)
ngombe <- merge(ngombe, zast, by = "Respondent_ID", all = TRUE)

ngombe$on.commons.permits  <- ngombe$new.cattle - ngombe$mkt.c.demand
ngombe$on.commons.transfer <- ngombe$herd - ngombe$cattle_ranch

# Extracting people using the ranches now
ngombe.users <- ngombe[ngombe$cattle_ranch > 0,]

summary(ngombe.users$on.commons.permits)
summary(ngombe.users$on.commons.transfer)

summary(ngombe.users$on.commons.permits  - ngombe.users$new.cattle.CS)
summary(ngombe.users$on.commons.transfer - ngombe.users$on.commons.permits)
summary(ngombe.users$on.commons.transfer - ngombe.users$new.cattle.CS)

ngombe.users$on.commons.nothing.TLU  <- ngombe.users$new.cattle.CS + 0.2*ngombe.users$new.goats.CS
ngombe.users$on.commons.permits.TLU  <- ngombe.users$on.commons.permits + 0.2*ngombe.users$new.goats
ngombe.users$on.commons.transfer.TLU <- ngombe.users$on.commons.transfer + 0.2*ngombe.users$num_goats

summary(ngombe.users$on.commons.nothing.TLU)
summary(ngombe.users$on.commons.permits.TLU)
summary(ngombe.users$on.commons.transfer.TLU)

#######   Non-users' response to free grass   #######
# _________________________________________________ #

# Create new dataset
nonusers <- pastoralist[pastoralist$ranch == 0,]

# Get amount of leftover space for each person
multp <- length(nonusers$herd)/length(users$herd)

space.tran.total <- median(ngombe.users$on.commons.permits.TLU - ngombe.users$on.commons.transfer.TLU)
space.perm.total <- median(ngombe.users$on.commons.nothing.TLU - ngombe.users$on.commons.permits.TLU)

space.tran <- space.tran.total/multp
space.perm <- space.perm.total/multp

# Shift out the budget constraints under each market condition
nonusers <- nonusers %>% mutate(C1.tran = C1 - space.tran)
nonusers <- nonusers %>% mutate(G1.tran = G1 - 5*space.tran)

nonusers <- nonusers %>% mutate(C1.perm = C1.tran - space.perm)
nonusers <- nonusers %>% mutate(G1.perm = G1.tran - 5*space.perm)

### Simulate the new bundle under each condition

## First, with transfer

# Getting the slope of the new budget line
nonusers <- nonusers %>% mutate(b.tran = ifelse(C1.tran <= 0, NA, G1.tran/C1.tran))

# Getting tau for this market condition
nonusers <- nonusers %>% mutate(tau.space.tran = (b.tran*(1-alpha)/alpha)^(1/(rho-1)) )

# Getting new number of goats that lines up with new budget constraint
nonusers <- nonusers %>% mutate(goats.space.tran = G1.tran/(1+b.tran*tau.space.tran))

# Plugging in to budget constraint to get new number of cattle
nonusers <- nonusers %>% mutate(cattle.space.tran = (G1.tran-goats.space.tran)/b.tran)

## Now, market-clearing

# Getting the slope of the new budget line
nonusers <- nonusers %>% mutate(b.perm = ifelse(C1.perm <= 0, NA, G1.perm/C1.perm))

# Getting tau for this market condition
nonusers <- nonusers %>% mutate(tau.space.perm = (b.perm*(1-alpha)/alpha)^(1/(rho-1)) )

# Getting new number of goats that lines up with new budget constraint
nonusers <- nonusers %>% mutate(goats.space.perm = G1.perm/(1+b.perm*tau.space.perm))

# Plugging in to budget constraint to get new number of cattle
nonusers <- nonusers %>% mutate(cattle.space.perm = (G1.perm-goats.space.perm)/b.perm)

### Getting the effect of these income boosts
nonusers$space.tran.TLU <- nonusers$cattle.space.tran + 0.2*nonusers$goats.space.tran
nonusers$space.perm.TLU <- nonusers$cattle.space.perm + 0.2*nonusers$goats.space.perm
nonusers$observed.TLU   <- nonusers$herd + 0.2*nonusers$num_goats

summary(nonusers$observed.TLU)
summary(nonusers$space.tran.TLU)
summary(nonusers$space.perm.TLU)

summary(nonusers$herd/nonusers$num_goats)
summary(nonusers$cattle.space.tran/nonusers$goats.space.tran)
summary(nonusers$cattle.space.perm/nonusers$goats.space.perm)

### How do nonusers affect the space left by users?

# See how they change their herds
nonusers$goat.change.mkt   <- nonusers$goats.space.tran - nonusers$goats.space.perm
nonusers$cattle.change.mkt <- nonusers$cattle.space.tran - nonusers$cattle.space.perm

nonusers$goat.change.transfer  <- nonusers$num_goats - nonusers$goats.space.tran
nonusers$cattle.change.transfer <- nonusers$herd - nonusers$cattle.space.tran

# Take the median
goat.c.mkt        <- median(nonusers$goat.change.mkt, na.rm = TRUE)
goat.c.transfer   <- median(nonusers$goat.change.transfer, na.rm = TRUE)
cattle.c.mkt      <- median(nonusers$cattle.change.mkt, na.rm = TRUE)
cattle.c.transfer <- median(nonusers$cattle.change.transfer, na.rm = TRUE)

cattle.c.mkt.users      <- median(ngombe.users$on.commons.permits  - ngombe.users$new.cattle.CS, na.rm = TRUE)
cattle.c.transfer.users <- median(ngombe.users$on.commons.transfer - ngombe.users$on.commons.permits, na.rm = TRUE)
goat.c.mkt.users        <- median(ngombe.users$new.goats - ngombe.users$new.goats.CS, na.rm = TRUE)
goat.c.transfer.users   <- median(ngombe.users$num_goats - ngombe.users$new.goats, na.rm = TRUE)

# Find the TLU of these changes and add in the users
TLU.mkt <- (cattle.c.mkt.users) + 0.2*(goat.c.mkt.users) + 4.5*(cattle.c.mkt + 0.2*goat.c.mkt)
TLU.mkt
1-(-TLU.mkt/space.perm.total)
TLU.transfer <- (cattle.c.transfer.users) + 0.2*(goat.c.transfer.users) + 4.5*(cattle.c.transfer + 0.2*goat.c.transfer)
TLU.transfer
1-(-TLU.transfer/space.tran.total)
1-((-TLU.mkt - TLU.transfer)/(space.perm.total + space.tran.total))

#######   Prepping for total market graph   #######
# _______________________________________________ #

## Adding total cattle
cast <- pastoralist[,c("ranch_free_cattle","ranch_250ksh_cattle","ranch_500ksh_cattle","ranch_750ksh_cattle","ranch_1000ksh_cattle")]

cf.start  <-  data.frame(price = seq(from = 0, to = 1000, by = 250), quan = c(cast[1,1], cast[1,2], cast[1,3], cast[1,4], cast[1,5]))
cf.overall <- cf.start

for (i in 2:nrow(cast)){
  df <- data.frame(price = seq(from = 0, to = 1000, by = 250), quan = c(cast[i,1], cast[i,2], cast[i,3], cast[i,4], cast[i,5]))
  cf.overall <- rbind(cf.overall,df)
}

cf.agg <- data.frame(price = seq(0, 1000, by = 250), quan = c(sum(cf.overall$quan[cf.overall$price == 0]), sum(cf.overall$quan[cf.overall$price == 250]), sum(cf.overall$quan[cf.overall$price == 500]), sum(cf.overall$quan[cf.overall$price == 750]), sum(cf.overall$quan[cf.overall$price == 1000])))

df.agg <- cbind(df.agg, cf.agg$quan)
df.agg$quanc <- df.agg$`cf.agg$quan`
df.agg <- df.agg[,-3]

rm(cf.agg)

## Adding total goats
gast <- pastoralist[,c("ranch_free_goat", "ranch_250ksh_goat", "ranch_500ksh_goat", "ranch_750ksh_goat", "ranch_1000ksh_goat")]

gf.start  <-  data.frame(price = seq(from = 0, to = 1000, by = 250), 
                         quan = c(gast[1,1], gast[1,2], gast[1,3], gast[1,4], gast[1,5]))
gf.overall <- gf.start

for (i in 2:nrow(gast)){
  gf <- data.frame(price = seq(from = 0, to = 1000, by = 250), 
                   quan = c(gast[i,1], gast[i,2], gast[i,3], gast[i,4], gast[i,5]))
  gf.overall <- rbind(gf.overall,gf)
}

gf.agg <- data.frame(price = seq(0, 1000, by = 250), 
                     quan = c(sum(gf.overall$quan[gf.overall$price == 0]), 
                              sum(gf.overall$quan[gf.overall$price == 250]), 
                              sum(gf.overall$quan[gf.overall$price == 500]), 
                              sum(gf.overall$quan[gf.overall$price == 750]), 
                              sum(gf.overall$quan[gf.overall$price == 1000])))

gf.agg$quang  <- gf.agg$quan

df.agg <- cbind(df.agg,gf.agg$quang)
df.agg$quang <- df.agg$`gf.agg$quang`
df.agg <- df.agg[,-4]

#######   Graphs   #######
# ______________________ #

## Payment for peace

dc.coeff <- demand.curve$coefficients
demand <- function(x){dc.coeff[1] + dc.coeff[2]*x}
int <- demand(sum(pastoralist$cattle_ranch))
inter <- function(p){(p-dc.coeff[1])/dc.coeff[2]}
other <- inter(450)

gg1 <- data.frame(x = obser, price = c(450,int))

cols <- c("Surplus transfer" = "grey75", 
          "Observed price x quantity" = "black",
          "Market-clearing price x quantity" = "black",
          "Original supply curve" = NA,
          "Price ceiling supply curve" = NA,
          "Market demand curve" = NA)

shapes <- c("Surplus transfer" = NA, 
            "Observed price x quantity" = 16,
            "Market-clearing price x quantity" = 17,
            "Original supply curve" = NA,
            "Price ceiling supply curve" = NA,
            "Market demand curve" = NA)

lts    <- c("Original supply curve" = "dashed",
            "Price ceiling supply curve" = "dotted",
            "Market demand curve" = "solid",
            "Surplus transfer" = NA, 
            "Observed price x quantity" = NA,
            "Market-clearing price x quantity" = NA)

labs   <- c("Observed price x quantity", "Market-clearing price x quantity")
linelabs <- c("Original supply curve", "Price ceiling supply curve", "Market demand curve")

ggplot(data = gg1, aes(quan, price)) + 
  geom_abline(aes(linetype = "Market demand curve", slope = dc.coeff[2], intercept = dc.coeff[1]), 
              col = "black") + 
  geom_rect(aes(fill = "Surplus transfer", xmin = 0, xmax = 979, ymin = 450, ymax = int), 
            col = "black") + 
  geom_point(aes(x = obser, y = 450, col = "Observed price x quantity", 
                 shape = "Observed price x quantity"), size = 3.5) +
  geom_point(aes(x = obser, y = int, col = "Market-clearing price x quantity", 
                 shape = "Market-clearing price x quantity"), size = 3.5) +
  geom_abline(aes(linetype = "Original supply curve", slope = 0.1, intercept = 665), 
              col = "black") + 
  geom_abline(aes(linetype = "Price ceiling supply curve", slope = 0.1, intercept = 355), 
              col = "black") + 
  xlab("# of livestock") +
  ylab("Price for grazing permits (ksh/head/month)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  coord_cartesian(xlim = c(0,12500), ylim = c(0,900)) + 
  scale_fill_manual(values = cols, name = "Area", breaks = c("Surplus transfer")) +
  scale_color_manual(values = cols, name = NULL, guide = "none") +
  scale_shape_manual(values = shapes, name = "Points", breaks = labs) +
  scale_linetype_manual(values = lts, name = "Lines", breaks = linelabs) +
  theme(axis.line = element_line(color = "black"), 
        legend.position = c(0.8,0.65),
        legend.text = element_text(size=10),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(10,15,10,10))

rm(gg1, cols, shapes, labs)

## Representative IC shift for first movers
graphing <- pastoralist[pastoralist$ranch == 1,]
ex <- graphing[graphing$Respondent_ID == "80-77-18-29",]
ex <- ex %>% select(herd, num_goats, CES.income, alpha, rho, new.goats, new.cattle, slope.CES, b, C1, G1, permit.share)
ex$new.inc <- CES(C = ex$new.cattle, G = ex$new.goats, a = ex$alpha, p = ex$rho)

graph.data <- data.frame(goats = seq(0,100, by=2), cattle = seq(0,50))

ggplot(data = graph.data, aes(x = cattle, y = goats)) +
  stat_function(fun = function(x)((ex$CES.income^ex$rho-ex$alpha*x^ex$rho)/
                                    (1-ex$alpha))^(1/ex$rho), linetype = "twodash") +
  stat_function(fun = function(x)(((ex$new.inc)^ex$rho-ex$alpha*x^ex$rho)/
                                    (1-ex$alpha))^(1/ex$rho)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_segment(aes(x = 0, xend = ex$C1, y = ex$G1, yend = 0), linetype = "twodash") +
  geom_segment(aes(x = 0, xend = ex$C1-ex$permit.share, y = ex$G1, yend = 0)) +
  geom_abline(aes(slope = ex$new.goats/ex$new.cattle, intercept = 0), linetype = "dotted") + 
  coord_cartesian(xlim = c(0,ex$C1+10), ylim = c(0,ex$G1+25), clip = "off") +
  theme(axis.line = element_line(color = "black"), 
           panel.background = element_rect(fill = "transparent", color = NA),
           plot.background = element_rect(fill = "transparent", color = NA),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           axis.title.x = element_text(hjust = 1),
           axis.title.y = element_text(hjust = 1)) + 
  annotation_custom(textGrob("C1"), xmin = 18.7, xmax = 18.7, ymin = -1.75, ymax = -1.75) +
  annotation_custom(textGrob("C1 + permits"), xmin = 33, xmax = 33, ymin = -1.75, ymax = -1.75) +
  annotation_custom(textGrob("G1"), xmin = -1, xmax = -1, ymin = 67, ymax = 67)

## Representative IC shift for second movers
ex <- nonusers[4,]
ex <- ex %>% select(herd, num_goats, CES.income, alpha, rho, goats.space.tran, cattle.space.tran, slope.CES, b, C1, G1)
ex$new.inc <- CES(C = ex$cattle.space.tran, G = ex$goats.space.tran, a = ex$alpha, p = ex$rho)

graph.data <- data.frame(goats = seq(0,100, by=2), cattle = seq(0,50))


ggplot(data = graph.data, aes(x = cattle, y = goats)) +
  stat_function(fun = function(x)((ex$CES.income^ex$rho-ex$alpha*x^ex$rho)/
                                    (1-ex$alpha))^(1/ex$rho), linetype = "twodash") +
  stat_function(fun = function(x)(((ex$new.inc)^ex$rho-ex$alpha*x^ex$rho)/(1-ex$alpha))^(1/ex$rho)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_segment(aes(x = 0, xend = ex$C1, y = ex$G1, yend = 0), linetype = "twodash") +
  geom_segment(aes(x = 0, xend = ex$C1-space.tran, y = ex$G1-5*space.tran, yend = 0)) +
  coord_cartesian(xlim = c(0,ex$C1+3), ylim = c(0,ex$G1+10), clip = "off") +
  theme(axis.line = element_line(color = "black"), 
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(hjust = 1),
        axis.title.y = element_text(hjust = 1)) + 
  annotation_custom(textGrob("C1"), xmin = ex$C1-space.tran, xmax = ex$C1-space.tran, ymin = -1, ymax = -1) +
  annotation_custom(textGrob("C2"), xmin = ex$C1, xmax = ex$C1, ymin = -1, ymax = -1) +
  annotation_custom(textGrob("G1"), xmin = -0.3, xmax = -0.3, ymin = ex$G1-5*space.tran, ymax = ex$G1-5*space.tran) +
  annotation_custom(textGrob("G2"), xmin = -0.3, xmax = -0.3, ymin = ex$G1, ymax = ex$G1)
