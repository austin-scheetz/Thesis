# Austin Scheetz
# Yale University
# Thesis Script

setwd("C:/Users/austi/Documents/Yale/Thesis/AJAE")
pastoralist <- read.csv("pastoralist.csv", as.is = TRUE, header = TRUE)

#######   Libraries and functions   #######  
# __________________________________________ #

library(tidyverse)
library(Deriv)
library(rgenoud)
library(gridExtra)
library(grid)

select <- dplyr::select

CES <- function(C,G,a,p) (a*C^p+(1-a)*G^p)^(1/p)
rev.CES <- function(C,a,p,U) ((U^p-a*C^p)/(1-a))^(1/p)

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

#######   Simulating market absence   #######
# _________________________________________ #

# Using implicit function theorem to calculate slope of the IC at the observed (C,G) point for each respondent
pastoralist <- pastoralist %>% mutate(slope.CES = -(alpha*herd^(rho-1))/((1-alpha)*num_goats^(rho-1)))
pastoralist$slope.CES[is.infinite(pastoralist$slope.CES) == TRUE] <- NA

# Getting y intercept of the current budget constraint ("goat intercept") which will stay constant
pastoralist <- pastoralist %>% mutate(G1 = num_goats - slope.CES*herd)

# Getting x intercept of the current budget constraint ("cattle intercept") which will change
pastoralist <- pastoralist %>% mutate(C1 = -G1/slope.CES)
pastoralist$C1[is.infinite(pastoralist$C1) == TRUE] <- NA

# Getting each respondent's share of the consumer surplus
pastoralist <- pastoralist %>% mutate(permit.share = cattle_ranch/obser)

# Getting choke price and computing consumer surplus
choke  <- demand.curve[["coefficients"]][1]
consur <- ((choke-mkt.price)*obser/2) + area
pastoralist <- pastoralist %>% mutate(CS.share = permit.share*consur)
pastoralist <- pastoralist %>% mutate(CS.ps = (CS.share/450))

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
users$cgr.before.CS <- users$new.cattle.CS/users$new.goats.CS

users$new.TLU.CS <- users$new.cattle.CS + 0.2*users$new.goats.CS

# Investigating the change
summary(users$cgr.before.CS)
summary(users$new.TLU.CS)
summary(users$new.cattle.CS)
summary(users$new.goats.CS)


#######   Ranch-users' bundles with permits in place   ########
# ___________________________________________________________ #

ngombe <- pastoralist %>% select(Respondent_ID, new.cattle.CS, new.goats.CS, herd, num_goats, 
                                 cattle_ranch)

ngombe$on.commons.permits <- ngombe$herd - ngombe$cattle_ranch

# Extracting people using the ranches now
ngombe.users <- ngombe[ngombe$cattle_ranch > 0,]

summary(ngombe.users$herd)
summary(ngombe.users$num_goats)
summary(ngombe.users$on.commons.permits)

ngombe.users$on.commons.nopermits.TLU  <- ngombe.users$new.cattle.CS + 0.2*ngombe.users$new.goats.CS
ngombe.users$on.commons.permits.TLU <- ngombe.users$on.commons.permits + 0.2*ngombe.users$num_goats
ngombe.users$TLU <- ngombe.users$herd + 0.2*ngombe.users$num_goats

summary(ngombe.users$on.commons.nopermits.TLU)
summary(ngombe.users$on.commons.permits.TLU)
summary(ngombe.users$TLU)

#######   Non-users' response to free grass   #######
# _________________________________________________ #

# Create new dataset
nonusers <- pastoralist[pastoralist$ranch == 0,]

# Get amount of leftover space for each person
multp <- length(nonusers$herd)/length(users$herd)

space.perm.total <- median(ngombe.users$on.commons.nopermits.TLU - ngombe.users$on.commons.permits.TLU)

space.perm <- space.perm.total/multp

# Shift out the budget constraints
nonusers <- nonusers %>% mutate(C1.perm = C1 - space.perm)
nonusers <- nonusers %>% mutate(G1.perm = G1 - 5*space.perm)

### Simulate the new bundle under no permits

# Getting the slope of the new budget line
nonusers <- nonusers %>% mutate(b.perm = ifelse(C1.perm <= 0, NA, G1.perm/C1.perm))

# Getting tau for this market condition
nonusers <- nonusers %>% mutate(tau.space.perm = (b.perm*(1-alpha)/alpha)^(1/(rho-1)) )

# Getting new number of goats that lines up with new budget constraint
nonusers <- nonusers %>% mutate(goats.space.perm = G1.perm/(1+b.perm*tau.space.perm))

# Plugging in to budget constraint to get new number of cattle
nonusers <- nonusers %>% mutate(cattle.space.perm = (G1.perm-goats.space.perm)/b.perm)

### Getting the effect of these income boosts
nonusers$space.perm.TLU <- nonusers$cattle.space.perm + 0.2*nonusers$goats.space.perm
nonusers$TLU   <- nonusers$herd + 0.2*nonusers$num_goats

summary(nonusers$herd)
summary(nonusers$num_goats)

summary(nonusers$cattle.space.perm)
summary(nonusers$goats.space.perm)

summary(nonusers$TLU)
summary(nonusers$space.perm.TLU)

summary(nonusers$herd/nonusers$num_goats)
summary(nonusers$cattle.space.perm/nonusers$goats.space.perm)

#######   How do nonusers affect the space left by users?   #######

# See how nonusers change their herds
nonusers$goat.change.perm  <- nonusers$num_goats - nonusers$goats.space.perm
nonusers$cattle.change.perm <- nonusers$herd - nonusers$cattle.space.perm

# Take the median
goat.c.perm   <- median(nonusers$goat.change.perm, na.rm = TRUE)
cattle.c.perm <- median(nonusers$cattle.change.perm, na.rm = TRUE)


# Find the TLU of these changes and add in the users
c.perm.users <- median(ngombe.users$on.commons.permits.TLU - ngombe.users$on.commons.nopermits.TLU, na.rm = TRUE)
c.perm.users

TLU.change.nonusers <- multp*(cattle.c.perm + 0.2*goat.c.perm)
TLU.change.nonusers

TLU.perm <- c.perm.users + TLU.change.nonusers
TLU.perm

TLU.change.nonusers/(-c.perm.users)

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

ggplot(data = gg1, aes(x, price)) + 
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
  xlab("# of cattle") +
  ylab("Price for grazing permits (ksh/head/month)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  coord_cartesian(xlim = c(0,6000), ylim = c(0,900)) + 
  scale_fill_manual(values = cols, name = "Area", breaks = c("Surplus transfer")) +
  scale_color_manual(values = cols, name = NULL, guide = "none") +
  scale_shape_manual(values = shapes, name = "Points", breaks = labs) +
  scale_linetype_manual(values = lts, name = "Lines", breaks = linelabs) +
  theme(axis.line = element_line(color = "black"), 
        legend.position = "right",
        legend.text = element_text(size=10),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(10,15,10,10))

rm(gg1, cols, shapes, labs)

## Representative IC shift for first movers
graphing <- pastoralist[pastoralist$ranch == 1,]
ex <- graphing[graphing$Respondent_ID == "80-77-18-29",]
ex <- ex %>% select(herd, num_goats, CES.income, alpha, rho, new.goats.CS, new.cattle.CS, slope.CES, b, C1, G1, permit.share, CS.ps)
ex$new.inc <- CES(C = ex$new.cattle.CS, G = ex$new.goats.CS, a = ex$alpha, p = ex$rho)

graph.data <- data.frame(shoats = seq(0,100, by=2), cattle = seq(0,50))

ggplot(data = graph.data, aes(x = cattle, y = shoats)) +
  stat_function(fun = function(x)((ex$CES.income^ex$rho-ex$alpha*x^ex$rho)/
                                    (1-ex$alpha))^(1/ex$rho), linetype = "twodash") +
  stat_function(fun = function(x)(((ex$new.inc)^ex$rho-ex$alpha*x^ex$rho)/
                                    (1-ex$alpha))^(1/ex$rho)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_segment(aes(x = 0, xend = ex$C1, y = ex$G1, yend = 0), linetype = "twodash") +
  geom_segment(aes(x = 0, xend = ex$C1-ex$CS.ps, y = ex$G1, yend = 0)) +
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
  annotation_custom(textGrob("C1"), xmin = 17, xmax = 17, ymin = -1.75, ymax = -1.75) +
  annotation_custom(textGrob("C1 + permits"), xmin = 33, xmax = 33, ymin = -1.75, ymax = -1.75) +
  annotation_custom(textGrob("G1"), xmin = -1, xmax = -1, ymin = 67, ymax = 67)

## Representative IC shift for second movers
ex <- nonusers[80,]
ex <- ex %>% select(herd, num_goats, CES.income, alpha, rho, goats.space.perm, cattle.space.perm, slope.CES, b.CS, C1, G1)
ex$new.inc <- CES(C = ex$cattle.space.perm, G = ex$goats.space.perm, a = ex$alpha, p = ex$rho)

graph.data <- data.frame(shoats = seq(0,100, by=2), cattle = seq(0,50))


ggplot(data = graph.data, aes(x = cattle, y = shoats)) +
  stat_function(fun = function(x)((ex$CES.income^ex$rho-ex$alpha*x^ex$rho)/
                                    (1-ex$alpha))^(1/ex$rho), linetype = "twodash") +
  stat_function(fun = function(x)(((ex$new.inc)^ex$rho-ex$alpha*x^ex$rho)/(1-ex$alpha))^(1/ex$rho)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_segment(aes(x = 0, xend = ex$C1, y = ex$G1, yend = 0), linetype = "twodash") +
  geom_segment(aes(x = 0, xend = ex$C1-space.perm, y = ex$G1-5*space.perm, yend = 0)) +
  coord_cartesian(xlim = c(0,ex$C1+1.5), ylim = c(0,ex$G1+5), clip = "off") +
  theme(axis.line = element_line(color = "black"), 
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(hjust = 1),
        axis.title.y = element_text(hjust = 1)) + 
  annotation_custom(textGrob("C1"), xmin = ex$C1-space.perm, xmax = ex$C1-space.perm, ymin = -.35, ymax = -.35) +
  annotation_custom(textGrob("C2"), xmin = ex$C1, xmax = ex$C1, ymin = -.35, ymax = -.35) +
  annotation_custom(textGrob("G1"), xmin = -0.1, xmax = -0.1, ymin = ex$G1-5*space.perm, ymax = ex$G1-5*space.perm) +
  annotation_custom(textGrob("G2"), xmin = -0.1, xmax = -0.1, ymin = ex$G1, ymax = ex$G1)
