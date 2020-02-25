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
  } else{xast$derivative.c[i] <- NA}
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
  } else{xast$derivative.g[i] <- NA}
}

rm(df,reggie,i,j)

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
for(i in 1:nrow(df.mrts)){
  if(df.mrts$mrts0[i] == df.mrts$mrts250[i] & 
     df.mrts$mrts250[i] == df.mrts$mrts500[i] & 
     df.mrts$mrts500[i] == df.mrts$mrts750[i] &
     df.mrts$mrts750[i] == df.mrts$mrts1000[i]){
    
    df.mrts$mrts0[i] <- NA
    df.mrts$mrts250[i] <- NA
    df.mrts$mrts500[i] <- NA
    df.mrts$mrts750[i] <- NA
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
gammas <- coef(ces.model)[3:73]
kappas <- coef(ces.model)[74:144]

coeffs <- data.frame(Respondent_ID = unique(ces.model[["model"]][["df.reg$Respondent_ID"]])[-1], 
                     gamma = gammas, 
                     kappa = kappas)

tmp <- data.frame(Respondent_ID = unique(ces.model[["model"]][["df.reg$Respondent_ID"]])[1],
                  gamma = 0, kappa = 0)

coeffs <- rbind(coeffs, tmp)

# Using regression estimates to calculate CES parameters

# A <- exp(a.est)/(1+exp(a.est))
# coeffs$alpha <- A + coeffs$gamma
# summary(coeffs$alpha)
coeffs$alpha <- exp(a.est + coeffs$gamma)/(1+exp(a.est + coeffs$gamma))
summary(coeffs$alpha)

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

#######   Prepping for simulation of peace payment   #######
# ________________________________________________________ #

# Putting dataset into long format
rantot <- pastoralist[, c("ranch_free_cattle", "ranch_250ksh_cattle", "ranch_500ksh_cattle", "ranch_750ksh_cattle", "ranch_1000ksh_cattle","ranch_free_goat", "ranch_250ksh_goat", "ranch_500ksh_goat", "ranch_750ksh_goat", "ranch_1000ksh_goat", "ranch_free_ranch", "ranch_250ksh_ranch", "ranch_500ksh_ranch", "ranch_750ksh_ranch", "ranch_1000ksh_ranch", "num_goats","herd","Respondent_ID")]

for (i in 1:nrow(rantot)){
  for (j in 1:4){
    rantot[i,j]    <- ifelse(rantot[i,j] < rantot[i,j+1], rantot[i,j+1], rantot[i,j]) 
    rantot[i,j+5]  <- ifelse(rantot[i,j+5] < rantot[i,j+6], rantot[i,j+6], rantot[i,j+5]) 
    rantot[i,j+10] <- ifelse(rantot[i,j+10] < rantot[i,j+11], rantot[i,j+11], rantot[i,j+10]) 
  }
}

df.rantot <- data.frame(price = seq(0,1000,250), 
                        cattle = c(rantot$ranch_free_cattle[1], 
                                   rantot$ranch_250ksh_cattle[1], 
                                   rantot$ranch_500ksh_cattle[1], 
                                   rantot$ranch_750ksh_cattle[1], 
                                   rantot$ranch_1000ksh_cattle[1]), 
                        ranch = c(rantot$ranch_free_ranch[1], 
                                  rantot$ranch_250ksh_ranch[1], 
                                  rantot$ranch_500ksh_ranch[1], 
                                  rantot$ranch_750ksh_ranch[1], 
                                  rantot$ranch_1000ksh_ranch[1]), 
                        goat = c(rantot$ranch_free_goat[1], 
                                 rantot$ranch_250ksh_goat[1], 
                                 rantot$ranch_500ksh_goat[1], 
                                 rantot$ranch_750ksh_goat[1], 
                                 rantot$ranch_1000ksh_goat[1]), 
                        Respondent_ID = rep(rantot$Respondent_ID[1], 5))

for (i in 2:nrow(rantot)){
  df <- data.frame(price = seq(0,1000,250), 
                   cattle = c(rantot$ranch_free_cattle[i], 
                              rantot$ranch_250ksh_cattle[i], 
                              rantot$ranch_500ksh_cattle[i], 
                              rantot$ranch_750ksh_cattle[i], 
                              rantot$ranch_1000ksh_cattle[i]), 
                   ranch = c(rantot$ranch_free_ranch[i], 
                             rantot$ranch_250ksh_ranch[i], 
                             rantot$ranch_500ksh_ranch[i], 
                             rantot$ranch_750ksh_ranch[i], 
                             rantot$ranch_1000ksh_ranch[i]),
                   goat = c(rantot$ranch_free_goat[i], 
                            rantot$ranch_250ksh_goat[i], 
                            rantot$ranch_500ksh_goat[i], 
                            rantot$ranch_750ksh_goat[i], 
                            rantot$ranch_1000ksh_goat[i]), 
                   Respondent_ID = rep(rantot$Respondent_ID[i], 5))
  df.rantot <- rbind(df.rantot, df)
  rm(df)
}

# How much would people increase herd sizes for each additional cow put on a ranch?
cattle.model <- lm(df.rantot$cattle ~ df.rantot$ranch*df.rantot$Respondent_ID)
goat.model   <- lm(df.rantot$goat ~ df.rantot$ranch*df.rantot$Respondent_ID)

# Storing this information and putting into dataframe
vec      <- unique(cattle.model[["model"]][["df.rantot$Respondent_ID"]])[-1]
othervec <- coef(cattle.model)[303:602]
thirdvec <- coef(goat.model)[303:602]
tmp      <- data.frame(Respondent_ID = vec, change.c = othervec, change.g = thirdvec)
tmp2     <- data.frame(Respondent_ID = unique(cattle.model[["model"]][["df.rantot$Respondent_ID"]])[1],
                       change.c = coef(cattle.model)[2], change.g = coef(goat.model)[2])
tmp3     <- rbind(tmp, tmp2)

pastoralist <- merge(pastoralist, tmp3, by = "Respondent_ID", all = TRUE)
rm(tmp, tmp2, tmp3, df.rantot, rantot, vec, othervec, thirdvec)

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

demand.curve <- lm(df.agg$price ~ df.agg$quan + I(df.agg$quan^2))
summary(demand.curve)

demand.eq <- expression(demand.curve[["coefficients"]][1] + 
             demand.curve[["coefficients"]][2]*q + 
             demand.curve[["coefficients"]][3]*q^2)

## Computing market clearing price
quan <- sum(pastoralist$cattle_ranch, na.rm = TRUE)
mkt.price <- eval(demand.eq, envir = data.frame(q = quan))

## Computing value of surplus transfer
area <- quan*(mkt.price-450)
area 
surp.trans <- unname((area/54)/450) # Restates this in terms of monthly permits per current ranch-user
surp.trans

rm(df.start, df.overall, df, past)

#######   Simulating the peace payment   #######
# ____________________________________________ #

# Calculating slope of the IC at the observed (C,G) point for each respondent
deriv.CES <- function(I,C,a,p){(a*C^(p-1)*((a*C^p-I^p)/(a-1))^(1/p-1))/(a-1)}
pastoralist$slope.CES <- deriv.CES(I = pastoralist$CES.income, C = pastoralist$herd, a = pastoralist$alpha, p = pastoralist$rho)

# Now I simulate the peace payment through a change in CES income
pastoralist <- pastoralist %>% mutate(newc = ifelse(herd-surp.trans*change.c < 0, 0, herd-surp.trans*change.c))
pastoralist <- pastoralist %>% mutate(newg = ifelse(num_goats-surp.trans*change.g < 0, 0, num_goats-surp.trans*change.g))

pastoralist <- pastoralist %>% mutate(newInc.low.CES = ((alpha*(newc)^rho)+((1-alpha)*(newg)^rho))^(1/rho))
pastoralist$newInc.low.CES[pastoralist$newInc.low.CES < 0] <- 0

summary(pastoralist$CES.income)
summary(pastoralist$newInc.low.CES)

# ================= #

summary(pastoralist$herd/pastoralist$num_goats - pastoralist$newc/pastoralist$newg)
summary(pastoralist$herd/pastoralist$num_goats)
summary(pastoralist$newc/pastoralist$newg)

summary(users$herd/users$num_goats - users$newc/users$newg)
summary(users$herd/users$num_goats)
summary(users$newc/users$newg)


# ================= #

# Find the number of cattle on the curve for this income level, maintaining the same slope as before
CES.optim <- function(C,I,a,p,x){(a*C^(p-1)*((a*C^p-I^p)/(a-1))^(1/p-1))/(a-1) - x}
pastoralist$newCattle.low.CES <- NA

# Use uniroot to solve numerically
for (i in 1:nrow(pastoralist)){
  pastoralist$newCattle.low.CES[i] <- ifelse(is.null(uniroot.all(CES.optim, c(0,1000),
                                                                 I = pastoralist$newInc.low.CES[i], 
                                                                 a = pastoralist$alpha[i], 
                                                                 p = pastoralist$rho[i], 
                                                                 x = pastoralist$slope.CES[i])) == FALSE, 
                                             uniroot.all(CES.optim, c(0,1000), 
                                                         I = pastoralist$newInc.low.CES[i], 
                                                         a = pastoralist$alpha[i], 
                                                         p = pastoralist$rho[i], 
                                                         x = pastoralist$slope.CES[i]), NA)
  print(paste(i))
}

summary(pastoralist$newCattle.low.CES)

# Plug in the cattle number into the IC equation with new income to get the corresponding goat number
newG.CES <- function(C,I,a,p){(((I^p)-a*(C^p))/(1-a))^(1/p)}

pastoralist$newGoats.low.CES <- newG.CES(C = pastoralist$newCattle.low.CES, 
                                         I = pastoralist$newInc.low.CES, 
                                         a = pastoralist$alpha,
                                         p = pastoralist$rho)

summary(pastoralist$newGoats.low.CES)

# Investigate the change compared to before
summary(pastoralist$herd/pastoralist$num_goats - pastoralist$newCattle.low.CES/pastoralist$newGoats.low.CES)
summary(pastoralist$herd/pastoralist$num_goats)
summary(pastoralist$newCattle.low.CES/pastoralist$newGoats.low.CES)

summary(pastoralist$herd - pastoralist$newCattle.low.CES)
summary(pastoralist$num_goats - pastoralist$newGoats.low.CES)

# Creating classes for goat expansion behavior
pastoralist$pp.behavior.ces <- ifelse(pastoralist$herd/pastoralist$num_goats - pastoralist$newCattle.low.CES/pastoralist$newGoats.low.CES < 0, "Expand goats", 
                                      ifelse(pastoralist$herd/pastoralist$num_goats - pastoralist$newCattle.low.CES/pastoralist$newGoats.low.CES > 0, "Expand cattle", "Scale up"))

table(pastoralist$pp.behavior.ces)

# Extracting the people using a ranch
users <- pastoralist[pastoralist$ranch == 1,]

summary(users$herd-users$newCattle.low.CES)
summary(users$num_goats-users$newGoats.low.CES)

summary(users$herd/users$num_goats - users$newCattle.low.CES/users$newGoats.low.CES)
summary(users$herd/users$num_goats)
summary(users$newCattle.low.CES/users$newGoats.low.CES)

table(users$pp.behavior.ces)

#######   Simulating market absence   #######
# _________________________________________ #

### First, getting the choke price for everyone
zast <- pastoralist[, c("ranch_free_ranch", "ranch_250ksh_ranch", "ranch_500ksh_ranch", "ranch_750ksh_ranch", "ranch_1000ksh_ranch","Respondent_ID")]

# Data cleaning that will allow code to run smoothly later
for (i in 1:nrow(zast)){
  for (j in 1:4){
    zast[i,j]   <- ifelse(zast[i,j] < zast[i,j+1], zast[i,j+1], zast[i,j])
  }
}

## Total cattle
# First, getting total cattle curve info and adding to the corresponding row in the original data
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
    
    reggie <- lm(df$price ~ df$quan + I(df$quan^2))
    
    zast$intercept.c[i]  <- reggie$coefficients[1]
    zast$slope.c[i]      <- reggie$coefficients[2]
    zast$slope2.c[i]     <- reggie$coefficients[3]
    
  } else{
    if(length(unique(df$quan)) == 1){
      zast$intercept.c[i]  <- NA
      zast$slope.c[i]      <- Inf
      zast$slope2.c[i]     <- NA
    } 
  }
}
# Now, creating a regression equation and adding to the df
for (i in 1:nrow(zast)){
  zast$eqn.d[i] <- paste(zast$intercept.c[i]," + ",zast$slope.c[i]," * q + ",zast$slope2.c[i]," * q^2",sep="")
  
  if(is.na(zast$slope2.c[i]) == TRUE){
    zast$eqn.d[i] <- paste(zast$intercept.c[i], " + ", zast$slope.c[i]," * q", sep = "")
  }
  
  if(is.infinite(zast$slope.c[i]) == TRUE){
    zast$eqn.d[i] <- paste("Inelastic at quan = ", zast$ranch_free_cattle[i], sep = "")
  }
  
  if(is.na(zast$slope.c[i]) == TRUE){
    zast$eqn.d[i] <- 0
  }
}

# Adding to main df
tobemerged <- zast %>% select(Respondent_ID, eqn.d)
pastoralist <- merge(pastoralist, tobemerged, by = "Respondent_ID", all = TRUE)

### Now, plugging in this choke price to the cattle and goat curves