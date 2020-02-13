# Austin Scheetz
# Yale University
# Thesis Script

setwd("C:/Users/austi/Documents/Yale/Thesis/Data")

#######   Libraries and source codes   #######  
# __________________________________________ #

library(tidyverse)
library(mice)
library(PerformanceAnalytics)
library(corrplot)
library(sm)
library(micEconCES)
library(MatchIt)
library(BAMMtools)
library(Matching)
library(rgenoud)
library(Deriv)
library(tcltk)
library(gridExtra)
library(rowr)
library(boot)
library(rootSolve)

select <- dplyr::select

source("http://www.reuningscherer.net/STAT660/R/CSQPlot.r.txt")

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

pastoralist$herd <- pastoralist$num_heifers + pastoralist$num_steers
pastoralist$herd <- ifelse(pastoralist$cattle_ranch > pastoralist$herd, pastoralist$cattle_ranch, pastoralist$herd)

pastoralist$tlu <- pastoralist$num_steers + pastoralist$num_heifers + pastoralist$num_goats*0.2 + pastoralist$num_camels

# Imputing missing education observations
educind.data <- pastoralist[,c(2:33,88)]
educind.data <- educind.data[,-21]
imputed <- mice(data = educind.data, method = "norm.predict", m = 5)
for (i in 1:5){
  varname <- paste("completed", i, sep = "")
  varname1 <- assign(varname, mice::complete(imputed, i))
  print(paste("Any NA values in completed", i, "?",sep = ""))
  print(anyNA(varname1))
  print(summary(varname1$education))
}
df <- data.frame(cbind(completed1$education,completed2$education,completed3$education,completed4$education,completed5$education))
pastoralist$educ.pred <- rowMeans(df)
pastoralist$education[is.na(pastoralist$education) == TRUE] <- pastoralist$educ.pred[is.na(pastoralist$education) == TRUE]
pastoralist$educ.ind <- pastoralist$school_sons + pastoralist$school_daughters + pastoralist$cu_sons + pastoralist$cu_daughters + pastoralist$educ.pred

rm(imputed)
rm(completed1)
rm(completed2)
rm(completed3)
rm(completed4)
rm(completed5)
rm(varname1)
rm(df)
rm(educind.data)
rm(i)
rm(varname)

# Ranch dummy
pastoralist$ranch <- ifelse(pastoralist$cattle_ranch == 0, 0, 1)

#######   PCA wealth index   #######
# ________________________________ #

# Extracting variables to include
d2 <- pastoralist[,c(4:29)]
d2 <- d2 %>% select(-num_steers, -num_heifers, -num_goats, -num_steers_2y, -num_heifers_2y, -num_goats_2y, -job)
d2 <- as.data.frame(scale(d2))

# Checking for multivariate normality
CSQPlot(d2,label="Demographic Data")

# Running PCA
pc.nl <- princomp(d2, cor = TRUE)
print(summary(pc.nl), digits = 2, loadings = pc.nl$loadings, cutoff = 0)

# Adding results to dataframe
pastoralist <- pastoralist %>% mutate(wealth.pc.nl = education*(pc.nl[["loadings"]][1]) + 
                                                      family_size*(pc.nl[["loadings"]][2]) + 
                                                      children*(pc.nl[["loadings"]][3]) + 
                                                      num_son*(pc.nl[["loadings"]][4]) + 
                                                      school_sons*(pc.nl[["loadings"]][5]) + 
                                                      cu_sons*(pc.nl[["loadings"]][6]) + 
                                                      num_daughter*(pc.nl[["loadings"]][7]) + 
                                                      school_daughters*(pc.nl[["loadings"]][8]) + 
                                                      cu_daughters*(pc.nl[["loadings"]][9]) + 
                                                      num_cattle_herders*(pc.nl[["loadings"]][10]) +
                                                      num_goat_herders*(pc.nl[["loadings"]][11]) + 
                                                      employed*(pc.nl[["loadings"]][12]) + 
                                                      electric*(pc.nl[["loadings"]][13]) + 
                                                      tv*(pc.nl[["loadings"]][14]) + 
                                                      stove*(pc.nl[["loadings"]][15]) + 
                                                      rooms*(pc.nl[["loadings"]][16]) + 
                                                      motorbike*(pc.nl[["loadings"]][17]) + 
                                                      car*(pc.nl[["loadings"]][18]) + 
                                                      phones*(pc.nl[["loadings"]][19]))

rm(d2)
rm(pc.nl)

#######   Creating and analyzing rigidity classes   #######
# _______________________________________________________ #

## Prep 

# Classifying "inelasticity" (rigidity) for goats and cattle
pastoralist$goat.i   <- ifelse(pastoralist$ranch_free_goat == pastoralist$ranch_250ksh_goat
                               & pastoralist$ranch_250ksh_goat == pastoralist$ranch_500ksh_goat 
                               & pastoralist$ranch_500ksh_goat == pastoralist$ranch_750ksh_goat 
                               & pastoralist$ranch_750ksh_goat == pastoralist$ranch_1000ksh_goat,1,0)

pastoralist$cattle.i <- ifelse(pastoralist$ranch_free_cattle == pastoralist$ranch_250ksh_cattle 
                               & pastoralist$ranch_250ksh_cattle == pastoralist$ranch_500ksh_cattle 
                               & pastoralist$ranch_500ksh_cattle == pastoralist$ranch_750ksh_cattle 
                               & pastoralist$ranch_750ksh_cattle == pastoralist$ranch_1000ksh_cattle,1,0)

# Putting these into a clearer categorical form
pastoralist$class <- ifelse(pastoralist$cattle.i == 0, 
                            ifelse(pastoralist$goat.i == 0, "CEGE", "CEGI"), 
                            ifelse(pastoralist$goat.i == 0, "CIGE", "CIGI"))
    # This step renames the categorical variable because it appears differently in different code
pastoralist$classfixed <- pastoralist$class
pastoralist$classfixed[pastoralist$classfixed == "CIGI"] <- "CR,GR"
pastoralist$classfixed[pastoralist$classfixed == "CEGI"] <- "CF,GR"
pastoralist$classfixed[pastoralist$classfixed == "CIGE"] <- "CR,GF"
pastoralist$classfixed[pastoralist$classfixed == "CEGE"] <- "CF,GF"

# Creating a binary for super-rigid or not
pastoralist$II <- ifelse(pastoralist$class == "CIGI", 1, 0)

# Create barplot visualizing ranch use by rigidty class
bp <- table(pastoralist$ranch, pastoralist$classfixed)
barplot(bp, beside = TRUE, col = c(1,2), main = "Ranch access by rigidity class", ylab = "Number of respondents", xlab = "Rigidity class")
legend("topleft", c("Currently off ranch", "Currently on ranch"), fill = c(1,2))

table(pastoralist$classfixed)

## Analysis 

# Predicting contract acceptance by class
prob.accept <- glm(ranch ~ classfixed + herd + num_goats + wealth.pc.nl, data = pastoralist, family = binomial(link = "logit"))
summary(prob.accept)
exp(coef(prob.accept))
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
  df$quan[df$quan == 0] <- 0.01 # Recoding so that the 1/x operation works
  
  # This concludes the data prep portion of this loop
  
  if(length(unique(df$quan)) > 1){
    
    reggie  <- lm(df$price ~ df$quan + I(1/df$quan))
    reggie2 <- lm(df$price ~ df$quan)
    
    xast$intercept.c[i]   <- reggie$coefficients[1]
    xast$slope.c[i]       <- reggie$coefficients[2]
    xast$slope.inv.c[i]   <- reggie$coefficients[3]
    xast$intercept2.c[i]  <- reggie2$coefficients[1]
    xast$slope2.c[i]      <- reggie2$coefficients[2]
    
  } else{
    if(length(unique(df$quan)) == 1){
      xast$intercept.c[i]   <- NA
      xast$slope.c[i]       <- Inf
      xast$slope.inv.c[i]   <- NA
      xast$intercept2.c[i]  <- NA
      xast$slope2.c[i]      <- Inf
    } 
  }
}
# Now, creating a regression equation and adding to the df
for (i in 1:nrow(xast)){
  xast$eqn.c[i] <- paste(xast$intercept.c[i], " + ", xast$slope.c[i]," * q + ", xast$slope.inv.c[i], " * q^-1", sep = "")
  
  if(is.na(xast$slope.inv.c[i]) == TRUE){
    xast$eqn.c[i] <- paste(xast$intercept2.c[i], " + ", xast$slope2.c[i]," * q", sep = "")
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
  if(is.infinite(xast$slope.c[i]) == FALSE){
    xast$derivative.c[i] <- Deriv(xast$eqn.c[i], "q") 
  } else{xast$derivative.c[i] <- NA}
}
rm(df,reggie,reggie2,i,j)

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
  df$quan[df$quan == 0] <- 0.01 # Recoding so that the 1/x operation works
  
  # This concludes the data prep portion of this loop
  
  if(length(unique(df$quan)) > 1){
    
    reggie  <- lm(df$price ~ df$quan + I(1/df$quan))
    reggie2 <- lm(df$price ~ df$quan)
    
    xast$intercept.g[i]   <- reggie$coefficients[1]
    xast$slope.g[i]       <- reggie$coefficients[2]
    xast$slope.inv.g[i]   <- reggie$coefficients[3]
    xast$intercept2.g[i]  <- reggie2$coefficients[1]
    xast$slope2.g[i]      <- reggie2$coefficients[2]
    
  } else{
    if(length(unique(df$quan)) == 1){
      xast$intercept.g[i]   <- NA
      xast$slope.g[i]       <- Inf
      xast$slope.inv.g[i]   <- NA
      xast$intercept2.g[i]  <- NA
      xast$slope2.g[i]      <- Inf
    } 
  }
}
# Now, creating a regression equation and adding to the df
for (i in 1:nrow(xast)){
  xast$eqn.g[i] <- paste(xast$intercept.g[i], " + ", xast$slope.g[i]," * q + ", xast$slope.inv.g[i], " * q^-1", sep = "")
  
  if(is.na(xast$slope.inv.g[i]) == TRUE){
    xast$eqn.g[i] <- paste(xast$intercept2.g[i], " + ", xast$slope2.g[i]," * q", sep = "")
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
  if(is.infinite(xast$slope.g[i]) == FALSE){
    xast$derivative.g[i] <- Deriv(xast$eqn.g[i], "q") 
  } else{xast$derivative.g[i] <- NA}
}
rm(df,reggie,reggie2,i,j)

df.mrts <- xast[,c("ranch_free_cattle", "ranch_250ksh_cattle", "ranch_500ksh_cattle", "ranch_750ksh_cattle", "ranch_1000ksh_cattle","ranch_free_goat", "ranch_250ksh_goat", "ranch_500ksh_goat", "ranch_750ksh_goat", "ranch_1000ksh_goat", "Respondent_ID", "derivative.c", "derivative.g")]

### Calculating MRTS at each price
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

length(df.mrts$mrts0[is.na(df.mrts$mrts0) == FALSE])

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

df.reg$mrts2 <- ifelse(df.reg$mrts == 0, 1, ifelse(df.reg$mrts == 1, 1.01, df.reg$mrts))
df.reg$mrts2[is.finite(df.reg$mrts2) == FALSE | df.reg$mrts2 < 0] <- NA

# Running the regression (with individual fixed effects)
ces.model <- lm(log(df.reg$mrts2) ~ log(df.reg$CoG) + df.reg$Respondent_ID + log(df.reg$CoG):df.reg$Respondent_ID)

a.est  <- coef(ces.model)[1]
b.est  <- coef(ces.model)[2]
gammas <- coef(ces.model)[3:71]
kappas <- coef(ces.model)[72:140]

coeffs <- data.frame(Respondent_ID = unique(ces.model[["model"]][["df.reg$Respondent_ID"]])[-1], 
                     gamma = gammas, 
                     kappa = kappas)

tmp <- data.frame(Respondent_ID = unique(ces.model[["model"]][["df.reg$Respondent_ID"]])[1],
                  gamma = 0, kappa = 0)

coeffs <- rbind(coeffs, tmp)

A <- exp(a.est)/(1+exp(a.est))
coeffs$alpha <- A + coeffs$gamma
summary(coeffs$alpha)

coeffs$rho   <- coeffs$kappa + b.est + 1
summary(coeffs$rho)

# Appending coefficients to original dataset
pastoralist <- merge(pastoralist, coeffs, by = "Respondent_ID", all = TRUE)

# Using average parameters to fill in the gaps
pastoralist$alpha <- ifelse(is.na(pastoralist$alpha) == TRUE, mean(coeffs$alpha, na.rm=TRUE), pastoralist$alpha)

pastoralist$rho <- ifelse(is.na(pastoralist$rho) == TRUE, mean(coeffs$rho, na.rm=TRUE), pastoralist$rho)

# Computing CES income using these parameters
pastoralist <- pastoralist %>% mutate(CES.income = ((alpha*herd^rho)+((1-alpha)*num_goats^rho))^(1/rho))
summary(pastoralist$CES.income)

rm(coeffs, tmp)

#######   Prepping for simulation of peace payment   #######
# ________________________________________________________ #

# Putting dataset into long format
rantot <- pastoralist[, c("ranch_free_cattle", "ranch_250ksh_cattle", "ranch_500ksh_cattle", "ranch_750ksh_cattle", "ranch_1000ksh_cattle","ranch_free_goat", "ranch_250ksh_goat", "ranch_500ksh_goat", "ranch_750ksh_goat", "ranch_1000ksh_goat", "ranch_free_ranch", "ranch_250ksh_ranch", "ranch_500ksh_ranch", "ranch_750ksh_ranch", "ranch_1000ksh_ranch", "num_goats","herd","Respondent_ID")]

for (i in 1:nrow(rantot)){
  for (j in 1:4){
    rantot[i,j]   <- ifelse(rantot[i,j] < rantot[i,j+1], rantot[i,j+1], rantot[i,j]) 
    rantot[i,j+5] <- ifelse(rantot[i,j+5] < rantot[i,j+6], rantot[i,j+6], rantot[i,j+5]) 
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
cattle.model <- lm(df.rantot$cattle ~ df.rantot$ranch*df.rantot$Respondent_ID + df.rantot$goat*df.rantot$Respondent_ID)

goat.model   <- lm(df.rantot$goat ~ df.rantot$ranch*df.rantot$Respondent_ID + df.rantot$cattle*df.rantot$Respondent_ID)

# Storing this information and putting into dataframe
vec      <- unique(cattle.model[["model"]][["df.rantot$Respondent_ID"]])[-1]
othervec <- coef(cattle.model)[304:603]
thirdvec <- coef(goat.model)[304:603]
tmp      <- data.frame(Respondent_ID = vec, change.c = othervec, change.g = thirdvec)
tmp2     <- data.frame(Respondent_ID = unique(cattle.model[["model"]][["df.rantot$Respondent_ID"]])[1],
                       change.c = coef(cattle.model)[2], change.g = coef(goat.model)[2])
tmp      <- rbind(tmp, tmp2)

pastoralist <- merge(pastoralist, tmp, by = "Respondent_ID", all = TRUE)
rm(tmp, tmp2)

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

demand.curve <- lm(df.agg$price ~ df.agg$quan + I(1/df.agg$quan))
summary(demand.curve)

## Computing value of surplus transfer
area <- 979*(677.3218-450)
area # 222,548 ksh/month paid to MY SAMPLE by all ranchers collectively
222548/301 # 739 ksh/month paid to each pastoralist every month by all ranchers collectively
222548/54  # 4121 ksh/month paid to each pastoralist who uses a ranch


#######   Simulating the peace payment   #######
# ____________________________________________ #

# Calculating slope of the IC at the observed (C,G) point for each respondent
deriv.CES <- function(I,C,a,p){(a*C^(p-1)*((a*C^p-I^p)/(a-1))^(1/p-1))/(a-1)}
pastoralist$slope.CES <- deriv.CES(I = pastoralist$CES.income, C = pastoralist$herd, a = pastoralist$alpha, p = pastoralist$rho)

# Now I simulate the peace payment through a change in Cobb-Douglas income
pastoralist <- pastoralist %>% mutate(newInc.low.CES = ((alpha*(herd-9*change.c)^rho)+((1-alpha)*(num_goats-9*change.g)^rho))^(1/rho))

summary(pastoralist$CES.income)
summary(pastoralist$newInc.low.CES)

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

table(users$pp.behavior.ces)

#######   Robustness checks   #######
# _________________________________ #

# Removing Tangi Nyeusi and Endana
pastoralist.subset <- pastoralist[pastoralist$village != "Tangi Nyeusi" & pastoralist$village != "Endana",]
noaccess.removed <- glm(ranch ~ class + herd + num_goats + wealth.pc.nl, data = pastoralist.subset, family=binomial(link="logit"))
summary(noaccess.removed)
exp(coef(noaccess.removed))

# Examining wealth by rigidity class
prob.II <- glm(pastoralist$II ~ herd + num_goats + wealth.pc.nl + gender + age + children + education + family_size + employed + risk_choice, data = pastoralist, family = binomial(link = "logit"))
summary(prob.II)
exp(coef(prob.II))

# Investigating occupation
prob.accept.job <- glm(ranch ~ job*class, data = pastoralist, family=binomial(link="logit"))
summary(prob.accept.job)

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
dc.coeff
demand <- function(x){dc.coeff[1] + dc.coeff[2]*x + dc.coeff[3]/x}
int <- demand(sum(pastoralist$cattle_ranch))
int
demand(3342)

x  <- seq(978,3343,by=1)
y  <- demand(x)
gg1 <- data.frame(x = x, y = y)

cols <- c("Surplus transfer"="grey30", "Value of grass and own cattle"="grey75","black"="black", "Observed price x quantity"="red")

g <- ggplot(data = df.agg, aes(quan, price)) + 
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x + I(1/x), col = "black") + 
  geom_rect(stat = "identity", aes(fill = "Surplus transfer", xmin = 0, xmax = 979, ymin = 450, ymax = int, colour = "black")) + 
  geom_ribbon(data = gg1, mapping = aes(x = x, ymin = 450, ymax = ifelse(x > 985, y, 450), fill = "Value of grass and own cattle", colour = "black"), inherit.aes = FALSE) + 
  geom_point(aes(x = 979, y = 450, col = "Observed price x quantity"), size = 3.5) +
  geom_point(aes(x = 979, y = int), size = 3.5, col = "black") + 
  geom_point(aes(x = 3342, y = 450), size = 3.5, col = "black") + 
  ggtitle("Market demand curve for grazing permits") +
  xlab("# of livestock") +
  ylab("Price for contracts (ksh/head/month)")
g + coord_cartesian(xlim = c(0,10000), ylim = c(0,750)) + 
  scale_fill_manual(name = "", values = cols) +
  scale_colour_manual(name="Error Bars", values=cols, guide = "none")+
  theme(axis.line = element_line(color = "black"), 
        legend.position = c(0.75,0.75),
        legend.text = element_text(size=10),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

## Total market

cols1 <- c("Total cattle"="blue","Ranch cattle"="red","Total goats"="black","Current market permit price"="darkgreen")

ggplot(data = df.agg, aes(y = price)) + 
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x + I(1/x), aes(x = quan, colour = "Ranch cattle")) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x + I(1/x^2), aes(x = quang, colour = "Total goats")) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x + I(1/x^20), aes(x = quanc, colour = "Total cattle")) +
  ggtitle("Market for cattle grazing permits and its effect on livestock herds") + 
  xlab("Number of livestock") + ylab("Price of ranch access for cattle (ksh/head/month)") + 
  geom_hline(aes(yintercept = 450, colour = "Current market permit price")) + 
  geom_point(aes(x = 8886, y = 450), size = 3.5, col = "blue") + 
  geom_point(aes(x = 30756, y = 450), size = 3.5, col = "black") + 
  geom_point(aes(x = 979, y = 450), size = 3.5, col = "red") +
  scale_fill_manual(name = "", values = cols1) +
  scale_colour_manual(name="", values=cols1)+
  theme(axis.line = element_line(color = "black"), 
        legend.position = "top",
        legend.text = element_text(size=10),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))