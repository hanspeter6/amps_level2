# load packages
library(lavaan)
library(dplyr)
library(emmeans)
library(foreach)
library(doParallel)
library(dummies)
library(stringr)
library(gmodels)
library(formula.tools)
library(tidyr)
library(ggplot2)
library(gridExtra)

# read in the pooled set (ie completely unstandardised data)
set_min <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/set_min_simple_print.rds")

# # standardise media vehicles:
# set_min_st <- cbind.data.frame(set_min[,1:which(names(set_min) == "all")], scale(set_min[,(which(names(set_min) == "all") + 1):ncol(set_min)]))

# draw single (for now) bootstrap version of only 2000 cases
set_boot <- set_min[sample(nrow(set_min), replace = TRUE, size = 20000),] # for smaller sampling, size = 2000

# adding dummies to prepare frame for SEM:
mod_boot <- dummy.data.frame( data = set_boot[,-c(1,2,5,10:13,15:21)], names = c("year", "age", "sex", "edu", "hh.inc", "race", "lsm"), sep = "." )

# # for practice select small set: ( need to read in the full set after done on R itself...)

# define the SEM model
model_sem <- '

# latent variable definitions:
print5 =~ Business.Day + Mail.n.Guardian + The.Sunday.Independent + Sunday.Times + You + Car + Cosmopolitan + Getaway + Topcar + X5FM
afrikaans =~ Rapport + Huisgenoot + Sarie
african =~ Drum + Bona + Metro.FM + Soccer.Laduma + Kickoff
social =~ DSTV + int.social + int.radio + int.search
freeTV =~ e.tv + SABC.1 + SABC.2 + SABC.3
intNews =~ int.print + int.news

# regression models
print5 ~ year.2008 + year.2010 + year.2012 + year.2014 + age.2 + age.3 + age.4 + sex.2 + edu.2 + edu.3 + hh.inc.2 + hh.inc.3 + hh.inc.4 + race.2 + race.3 + race.4 + lsm.2 + lsm.3 + lsm.4 + lsm.5 + year.2008:age.2 + year.2008:age.3 + year.2008:age.4 + year.2008:sex.2 + year.2008:edu.2 + year.2008:edu.3 + year.2008:hh.inc.2 + year.2008:hh.inc.3 + year.2008:hh.inc.4 + year.2008:race.2 + year.2008:race.3 + year.2008:race.4 + year.2008:lsm.2 + year.2008:lsm.3 + year.2008:lsm.4 + year.2008:lsm.5 + year.2010:age.2 + year.2010:age.3 + year.2010:age.4 + year.2010:sex.2 + year.2010:edu.2 + year.2010:edu.3 + year.2010:hh.inc.2 + year.2010:hh.inc.3 + year.2010:hh.inc.4 + year.2010:race.2 + year.2010:race.3 + year.2010:race.4 + year.2010:lsm.2 + year.2010:lsm.3 + year.2010:lsm.4 + year.2010:lsm.5 + year.2012:age.2 + year.2012:age.3 + year.2012:age.4 + year.2012:sex.2 + year.2012:edu.2 + year.2012:edu.3 + year.2012:hh.inc.2 + year.2012:hh.inc.3 + year.2012:hh.inc.4 + year.2012:race.2 + year.2012:race.3 + year.2012:race.4 + year.2012:lsm.2 + year.2012:lsm.3 + year.2012:lsm.4 + year.2012:lsm.5 + year.2014:age.2 + year.2014:age.3 + year.2014:age.4 + year.2014:sex.2 + year.2014:edu.2 + year.2014:edu.3 + year.2014:hh.inc.2 + year.2014:hh.inc.3 + year.2014:hh.inc.4 + year.2014:race.2 + year.2014:race.3 + year.2014:race.4 + year.2014:lsm.2 + year.2014:lsm.3 + year.2014:lsm.4 + year.2014:lsm.5
african ~  year.2008 + year.2010 + year.2012 + year.2014 + age.2 + age.3 + age.4 + sex.2 + edu.2 + edu.3 + hh.inc.2 + hh.inc.3 + hh.inc.4 + race.2 + race.3 + race.4 + lsm.2 + lsm.3 + lsm.4 + lsm.5 + year.2008:age.2 + year.2008:age.3 + year.2008:age.4 + year.2008:sex.2 + year.2008:edu.2 + year.2008:edu.3 + year.2008:hh.inc.2 + year.2008:hh.inc.3 + year.2008:hh.inc.4 + year.2008:race.2 + year.2008:race.3 + year.2008:race.4 + year.2008:lsm.2 + year.2008:lsm.3 + year.2008:lsm.4 + year.2008:lsm.5 + year.2010:age.2 + year.2010:age.3 + year.2010:age.4 + year.2010:sex.2 + year.2010:edu.2 + year.2010:edu.3 + year.2010:hh.inc.2 + year.2010:hh.inc.3 + year.2010:hh.inc.4 + year.2010:race.2 + year.2010:race.3 + year.2010:race.4 + year.2010:lsm.2 + year.2010:lsm.3 + year.2010:lsm.4 + year.2010:lsm.5 + year.2012:age.2 + year.2012:age.3 + year.2012:age.4 + year.2012:sex.2 + year.2012:edu.2 + year.2012:edu.3 + year.2012:hh.inc.2 + year.2012:hh.inc.3 + year.2012:hh.inc.4 + year.2012:race.2 + year.2012:race.3 + year.2012:race.4 + year.2012:lsm.2 + year.2012:lsm.3 + year.2012:lsm.4 + year.2012:lsm.5 + year.2014:age.2 + year.2014:age.3 + year.2014:age.4 + year.2014:sex.2 + year.2014:edu.2 + year.2014:edu.3 + year.2014:hh.inc.2 + year.2014:hh.inc.3 + year.2014:hh.inc.4 + year.2014:race.2 + year.2014:race.3 + year.2014:race.4 + year.2014:lsm.2 + year.2014:lsm.3 + year.2014:lsm.4 + year.2014:lsm.5
afrikaans ~  year.2008 + year.2010 + year.2012 + year.2014 + age.2 + age.3 + age.4 + sex.2 + edu.2 + edu.3 + hh.inc.2 + hh.inc.3 + hh.inc.4 + race.2 + race.3 + race.4 + lsm.2 + lsm.3 + lsm.4 + lsm.5 + year.2008:age.2 + year.2008:age.3 + year.2008:age.4 + year.2008:sex.2 + year.2008:edu.2 + year.2008:edu.3 + year.2008:hh.inc.2 + year.2008:hh.inc.3 + year.2008:hh.inc.4 + year.2008:race.2 + year.2008:race.3 + year.2008:race.4 + year.2008:lsm.2 + year.2008:lsm.3 + year.2008:lsm.4 + year.2008:lsm.5 + year.2010:age.2 + year.2010:age.3 + year.2010:age.4 + year.2010:sex.2 + year.2010:edu.2 + year.2010:edu.3 + year.2010:hh.inc.2 + year.2010:hh.inc.3 + year.2010:hh.inc.4 + year.2010:race.2 + year.2010:race.3 + year.2010:race.4 + year.2010:lsm.2 + year.2010:lsm.3 + year.2010:lsm.4 + year.2010:lsm.5 + year.2012:age.2 + year.2012:age.3 + year.2012:age.4 + year.2012:sex.2 + year.2012:edu.2 + year.2012:edu.3 + year.2012:hh.inc.2 + year.2012:hh.inc.3 + year.2012:hh.inc.4 + year.2012:race.2 + year.2012:race.3 + year.2012:race.4 + year.2012:lsm.2 + year.2012:lsm.3 + year.2012:lsm.4 + year.2012:lsm.5 + year.2014:age.2 + year.2014:age.3 + year.2014:age.4 + year.2014:sex.2 + year.2014:edu.2 + year.2014:edu.3 + year.2014:hh.inc.2 + year.2014:hh.inc.3 + year.2014:hh.inc.4 + year.2014:race.2 + year.2014:race.3 + year.2014:race.4 + year.2014:lsm.2 + year.2014:lsm.3 + year.2014:lsm.4 + year.2014:lsm.5
intNews ~  year.2008 + year.2010 + year.2012 + year.2014 + age.2 + age.3 + age.4 + sex.2 + edu.2 + edu.3 + hh.inc.2 + hh.inc.3 + hh.inc.4 + race.2 + race.3 + race.4 + lsm.2 + lsm.3 + lsm.4 + lsm.5 + year.2008:age.2 + year.2008:age.3 + year.2008:age.4 + year.2008:sex.2 + year.2008:edu.2 + year.2008:edu.3 + year.2008:hh.inc.2 + year.2008:hh.inc.3 + year.2008:hh.inc.4 + year.2008:race.2 + year.2008:race.3 + year.2008:race.4 + year.2008:lsm.2 + year.2008:lsm.3 + year.2008:lsm.4 + year.2008:lsm.5 + year.2010:age.2 + year.2010:age.3 + year.2010:age.4 + year.2010:sex.2 + year.2010:edu.2 + year.2010:edu.3 + year.2010:hh.inc.2 + year.2010:hh.inc.3 + year.2010:hh.inc.4 + year.2010:race.2 + year.2010:race.3 + year.2010:race.4 + year.2010:lsm.2 + year.2010:lsm.3 + year.2010:lsm.4 + year.2010:lsm.5 + year.2012:age.2 + year.2012:age.3 + year.2012:age.4 + year.2012:sex.2 + year.2012:edu.2 + year.2012:edu.3 + year.2012:hh.inc.2 + year.2012:hh.inc.3 + year.2012:hh.inc.4 + year.2012:race.2 + year.2012:race.3 + year.2012:race.4 + year.2012:lsm.2 + year.2012:lsm.3 + year.2012:lsm.4 + year.2012:lsm.5 + year.2014:age.2 + year.2014:age.3 + year.2014:age.4 + year.2014:sex.2 + year.2014:edu.2 + year.2014:edu.3 + year.2014:hh.inc.2 + year.2014:hh.inc.3 + year.2014:hh.inc.4 + year.2014:race.2 + year.2014:race.3 + year.2014:race.4 + year.2014:lsm.2 + year.2014:lsm.3 + year.2014:lsm.4 + year.2014:lsm.5
freeTV ~  year.2008 + year.2010 + year.2012 + year.2014 + age.2 + age.3 + age.4 + sex.2 + edu.2 + edu.3 + hh.inc.2 + hh.inc.3 + hh.inc.4 + race.2 + race.3 + race.4 + lsm.2 + lsm.3 + lsm.4 + lsm.5 + year.2008:age.2 + year.2008:age.3 + year.2008:age.4 + year.2008:sex.2 + year.2008:edu.2 + year.2008:edu.3 + year.2008:hh.inc.2 + year.2008:hh.inc.3 + year.2008:hh.inc.4 + year.2008:race.2 + year.2008:race.3 + year.2008:race.4 + year.2008:lsm.2 + year.2008:lsm.3 + year.2008:lsm.4 + year.2008:lsm.5 + year.2010:age.2 + year.2010:age.3 + year.2010:age.4 + year.2010:sex.2 + year.2010:edu.2 + year.2010:edu.3 + year.2010:hh.inc.2 + year.2010:hh.inc.3 + year.2010:hh.inc.4 + year.2010:race.2 + year.2010:race.3 + year.2010:race.4 + year.2010:lsm.2 + year.2010:lsm.3 + year.2010:lsm.4 + year.2010:lsm.5 + year.2012:age.2 + year.2012:age.3 + year.2012:age.4 + year.2012:sex.2 + year.2012:edu.2 + year.2012:edu.3 + year.2012:hh.inc.2 + year.2012:hh.inc.3 + year.2012:hh.inc.4 + year.2012:race.2 + year.2012:race.3 + year.2012:race.4 + year.2012:lsm.2 + year.2012:lsm.3 + year.2012:lsm.4 + year.2012:lsm.5 + year.2014:age.2 + year.2014:age.3 + year.2014:age.4 + year.2014:sex.2 + year.2014:edu.2 + year.2014:edu.3 + year.2014:hh.inc.2 + year.2014:hh.inc.3 + year.2014:hh.inc.4 + year.2014:race.2 + year.2014:race.3 + year.2014:race.4 + year.2014:lsm.2 + year.2014:lsm.3 + year.2014:lsm.4 + year.2014:lsm.5
social ~  year.2008 + year.2010 + year.2012 + year.2014 + age.2 + age.3 + age.4 + sex.2 + edu.2 + edu.3 + hh.inc.2 + hh.inc.3 + hh.inc.4 + race.2 + race.3 + race.4 + lsm.2 + lsm.3 + lsm.4 + lsm.5 + year.2008:age.2 + year.2008:age.3 + year.2008:age.4 + year.2008:sex.2 + year.2008:edu.2 + year.2008:edu.3 + year.2008:hh.inc.2 + year.2008:hh.inc.3 + year.2008:hh.inc.4 + year.2008:race.2 + year.2008:race.3 + year.2008:race.4 + year.2008:lsm.2 + year.2008:lsm.3 + year.2008:lsm.4 + year.2008:lsm.5 + year.2010:age.2 + year.2010:age.3 + year.2010:age.4 + year.2010:sex.2 + year.2010:edu.2 + year.2010:edu.3 + year.2010:hh.inc.2 + year.2010:hh.inc.3 + year.2010:hh.inc.4 + year.2010:race.2 + year.2010:race.3 + year.2010:race.4 + year.2010:lsm.2 + year.2010:lsm.3 + year.2010:lsm.4 + year.2010:lsm.5 + year.2012:age.2 + year.2012:age.3 + year.2012:age.4 + year.2012:sex.2 + year.2012:edu.2 + year.2012:edu.3 + year.2012:hh.inc.2 + year.2012:hh.inc.3 + year.2012:hh.inc.4 + year.2012:race.2 + year.2012:race.3 + year.2012:race.4 + year.2012:lsm.2 + year.2012:lsm.3 + year.2012:lsm.4 + year.2012:lsm.5 + year.2014:age.2 + year.2014:age.3 + year.2014:age.4 + year.2014:sex.2 + year.2014:edu.2 + year.2014:edu.3 + year.2014:hh.inc.2 + year.2014:hh.inc.3 + year.2014:hh.inc.4 + year.2014:race.2 + year.2014:race.3 + year.2014:race.4 + year.2014:lsm.2 + year.2014:lsm.3 + year.2014:lsm.4 + year.2014:lsm.5

'
# # run single version of the sem model:
fit_sem <- lavaan::sem(model = model_sem, data = mod_boot, std.lv = TRUE, meanstructure = TRUE)

# OR

# # # read in what was done earlier
# fit_sem <- readRDS("/Users/hans-peterbakker/fit_sem.rds")

# create a database of proportions by category levels, ie also by year:
proportions <- data.frame()
for(i in c("age","sex", "edu", "hh.inc", "race", "lsm")) {
  
  temp1 <- as.data.frame(prop.table(xtabs(as.formula(paste0(as.character(~ year ) , "+ ", i)), data = set_boot), margin = 1))
  
  proportions <- rbind.data.frame(proportions, temp1 %>%
                                    mutate(category = (paste0(i, ".", temp1[,2]))) %>%
                                    dplyr::select(year, category, proportion = Freq))
}

# extract coefficients from the SEM object
coefs_sem <- coef(fit_sem)

# generate the yhat (kind of model matrix) object
yhat <- lavPredict(fit_sem, type = "yhat")

# create vector of model matrix colnames (ie the predictors in the various regression equations of the SEM )
names_vector <- colnames(yhat[,which(colnames(yhat) == "year.2008"): ncol(yhat)])

# create a "left-hand" dataframe for category levels by year
lh_df <- expand.grid(year = c("2002","2008", "2010","2012","2014"), category = c("age.1", "age.2", "age.3", "age.4", "sex.1", "sex.2", "edu.1", "edu.2", "edu.3", "hh.inc.1", "hh.inc.2", "hh.inc.3", "hh.inc.4", "race.1", "race.2", "race.3", "race.4", "lsm.1", "lsm.2", "lsm.3", "lsm.4", "lsm.5"))

# create an empty "right-hand" dataframe for the target frame
rh_df <- matrix(0,ncol = length(names_vector), nrow = nrow(lh_df))
colnames(rh_df) <- names_vector
rh_df <- data.frame(rh_df)

# combine them into two single frames to be populated with either equal or proportional weights 
weights_equal <- cbind(lh_df, rh_df)
weights_proportional <- cbind(lh_df, rh_df)

## populate the two weights dataframes:
# weights_equal
for(i in 1:nrow(weights_equal)) {
  for(j in 3:22) { # only the non interaction terms and excluding the first two columns (year, category)
    
    if(names(weights_equal)[j] == paste0("year.",as.character(weights_equal[i,1])) | names(weights_equal)[j] == as.character(weights_equal[i,2])) { # 1 for given level and year
      weights_equal[i,j] <- 1
    }
    
    if(!str_sub(names(weights_equal)[j], start = 1, end = (str_locate(names(weights_equal)[j], "\\.")[1]) -1) %in% str_split(as.character(weights_equal[i,2]), "\\.", simplify = TRUE) & 
       str_sub(names(weights_equal)[j], start = 1, end = (str_locate(names(weights_equal)[j], "\\.")[1]) -1) != "year") { # to generate equal weights for other category levels
      
      if(str_sub(names(weights_equal)[j], start = 1, end = (str_locate(names(weights_equal)[j], "\\.")[1]) -1) == "sex") {
        weights_equal[i,j] <- 1/2
      }
      if(str_sub(names(weights_equal)[j], start = 1, end = (str_locate(names(weights_equal)[j], "\\.")[1]) -1) == "edu") {
        weights_equal[i,j] <- 1/3
      }
      if(str_sub(names(weights_equal)[j], start = 1, end = (str_locate(names(weights_equal)[j], "\\.")[1]) -1) %in% c("age", "race", "hh")) {
        weights_equal[i,j] <- 1/4
      }
      if(str_sub(names(weights_equal)[j], start = 1, end = (str_locate(names(weights_equal)[j], "\\.")[1]) -1) == "lsm") {
        weights_equal[i,j] <- 1/5
      }
      
    }
    
  }
}

# populating the remainder of the dataframe to be used in matrix multiplication (multiplying relevant columns of non interaction terms)
weights_equal[,23:38] <- weights_equal[,3]*weights_equal[,c(7:22)]
weights_equal[,39:54] <- weights_equal[,4]*weights_equal[,c(7:22)]
weights_equal[,55:70] <- weights_equal[,5]*weights_equal[,c(7:22)]
weights_equal[,71:86] <- weights_equal[,6]*weights_equal[,c(7:22)]

# weights_proportional
for(i in 1:nrow(weights_proportional)) {
  for(j in 3:22) { # only the non interaction terms and excluding the first two columns (year, category)
    
    if(names(weights_proportional)[j] == paste0("year.",as.character(weights_proportional[i,1])) | names(weights_proportional)[j] == as.character(weights_proportional[i,2])) { # 1 for given level and year
      weights_proportional[i,j] <- 1
    }
    
    if(!str_sub(names(weights_proportional)[j], start = 1, end = (str_locate(names(weights_proportional)[j], "\\.")[1]) -1) %in% str_split(as.character(weights_proportional[i,2]), "\\.", simplify = TRUE) & 
       str_sub(names(weights_proportional)[j], start = 1, end = (str_locate(names(weights_proportional)[j], "\\.")[1]) -1) != "year") { # to generate equal weights for other category levels
      
      weights_proportional[i,j] <- proportions[which(proportions$category == names(weights_proportional)[j] & proportions$year == weights_proportional$year[i]),3 ]
      
    }
    
  }
}

# populating the remainder of the dataframe to be used in matrix multiplication (multiplying relevant columns of non interaction terms)
weights_proportional[,23:38] <- weights_proportional[,3]*weights_proportional[,c(7:22)]
weights_proportional[,39:54] <- weights_proportional[,4]*weights_proportional[,c(7:22)]
weights_proportional[,55:70] <- weights_proportional[,5]*weights_proportional[,c(7:22)]
weights_proportional[,71:86] <- weights_proportional[,6]*weights_proportional[,c(7:22)]

## to generate marginal means want to multiply the regression coefficients with these two matrices
# creating a matrix of regression coefficients:

coefs_matrix <- cbind(print5 = coefs_sem[which(names(coefs_sem) == "print5~year.2008"):which(names(coefs_sem) == "print5~year.2014:lsm.5")], # need to check names(myCoefs) to id range per factor outcome
                      african = coefs_sem[which(names(coefs_sem) == "african~year.2008"):which(names(coefs_sem) == "african~year.2014:lsm.5")],
                      afrikaans = coefs_sem[which(names(coefs_sem) == "afrikaans~year.2008"):which(names(coefs_sem) == "afrikaans~year.2014:lsm.5")],
                      intNews = coefs_sem[which(names(coefs_sem) == "intNews~year.2008"):which(names(coefs_sem) == "intNews~year.2014:lsm.5")],
                      freeTV = coefs_sem[which(names(coefs_sem) == "freeTV~year.2008"):which(names(coefs_sem) == "freeTV~year.2014:lsm.5")],
                      social = coefs_sem[which(names(coefs_sem) == "social~year.2008"):which(names(coefs_sem) == "social~year.2014:lsm.5")])
rownames(coefs_matrix) <- NULL

## matrix multiplication and add back year/category in dataframe:
# emmeans_equal
emmeans_equal <- as.matrix(weights_equal[,-c(1:2)]) %*% coefs_matrix
emmeans_equal  <- cbind.data.frame(weights_equal[,c(1:2)], emmeans_equal)
# change order of year to be similar to others
emmeans_equal <- emmeans_equal %>%
  arrange(year)

# emmeans_proportional
emmeans_proportional <- as.matrix(weights_proportional[,-c(1:2)]) %*% coefs_matrix
emmeans_proportional  <- cbind.data.frame(weights_proportional[,c(1:2)], emmeans_proportional)
# change order of year to be similar to others
emmeans_proportional <- emmeans_proportional %>%
  arrange(year)

# if I want to label category level properly for visuals (later)
levels(emmeans_equal$category)
levels(emmeans_equal$category) <- c("15-24","25-44", "45-54","55+",
                                    "male", "female",
                                    "<matric", "matric",">matric",
                                    "<R2500","R2500-R6999","R7000-R11999",">=R12000",
                                    "black", "coloured", "indian", "white",
                                    "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
levels(emmeans_proportional$category)
levels(emmeans_proportional$category) <- c("15-24","25-44", "45-54","55+",
                                           "male", "female",
                                           "<matric", "matric",">matric",
                                           "<R2500","R2500-R6999","R7000-R11999",">=R12000",
                                           "black", "coloured", "indian", "white",
                                           "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")

# want to turn wide factors into narrow
emmeans_equal_long <- gather(emmeans_equal, factor, mean, -c(1,2))
emmeans_proportional_long <- gather(emmeans_proportional, factor, mean, -c(1,2))

saveRDS(emmeans_equal_long, "emmeans_equal_long.rds")
saveRDS(emmeans_proportional_long, "emmeans_proportional_long.rds")


# trying some plots

vector_row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2 <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")

# function for plotting fitted models
plot_fitted <- function(data1,data2, factor) { # factor: one of...
  
  if(factor == "print5") {
    a <- "mean"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "print5"
  }
  if(factor == "afrikaans") {
    a <- "mean"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "afrikaans"
  }
  if(factor == "african") {
    a <- "mean"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "african"
  }
  if(factor == "social") {
    a <- "mean"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "social"
  }
  if(factor == "freeTV") {
    a <- "mean"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "freeTV"
  }
  if(factor == "intNews") {
    a <- "mean"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "intNews"
  }
  
  #plot
  ggplot(data = data1, aes_string("year", a, group = "category")) +
    geom_line(color = "blue", size = 0.2) +

    geom_line(data = data2, aes_string("year", a, group = "category"), color = "red", size = 0.2) +
    
    geom_hline(yintercept = mean(lavPredict(fit_sem)[,factor]), size = 0.1) +
    

    facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
    # geom_errorbar(aes_string(ymax = c, ymin = d), size = 0.3, width = 0.4, alpha = 0.5) +
    labs(y = e)
    # coord_cartesian(ylim=c(-0.5, 0.5)) + 
    # scale_y_continuous(breaks=seq(-0.5, 0.5, 0.2))
  
}

## print5
print5_up <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1 & emmeans_equal_long$factor == "print5"),],
                               data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1 & emmeans_proportional_long$factor == "print5"),],
                                factor = "print5")
print5_down <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2 & emmeans_equal_long$factor == "print5"),],
                               data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2 & emmeans_proportional_long$factor == "print5"),],
                               factor = "print5")
jpeg("print5_emm.jpeg", quality = 100)
grid.arrange(print5_up, print5_down, nrow = 2, top = "print5")
dev.off()

## afrikaans
afrikaans_up <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1 & emmeans_equal_long$factor == "afrikaans"),],
                         data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1 & emmeans_proportional_long$factor == "afrikaans"),],
                         factor = "afrikaans")
afrikaans_down <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2 & emmeans_equal_long$factor == "afrikaans"),],
                           data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2 & emmeans_proportional_long$factor == "afrikaans"),],
                           factor = "afrikaans")
jpeg("afrikaans_emm.jpeg", quality = 100)
grid.arrange(afrikaans_up, afrikaans_down, nrow = 2, top = "afrikaans")
dev.off()

## african
african_up <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1 & emmeans_equal_long$factor == "african"),],
                         data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1 & emmeans_proportional_long$factor == "african"),],
                         factor = "african")
african_down <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2 & emmeans_equal_long$factor == "african"),],
                           data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2 & emmeans_proportional_long$factor == "african"),],
                           factor = "african")
jpeg("african_emm.jpeg", quality = 100)
grid.arrange(african_up, african_down, nrow = 2, top = "african")
dev.off()

## social
social_up <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1 & emmeans_equal_long$factor == "social"),],
                         data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1 & emmeans_proportional_long$factor == "social"),],
                         factor = "social")
social_down <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2 & emmeans_equal_long$factor == "social"),],
                           data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2 & emmeans_proportional_long$factor == "social"),],
                           factor = "social")
jpeg("social_emm.jpeg", quality = 100)
grid.arrange(social_up, social_down, nrow = 2, top = "social")
dev.off()

## freeTV
freeTV_up <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1 & emmeans_equal_long$factor == "freeTV"),],
                         data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1 & emmeans_proportional_long$factor == "freeTV"),],
                         factor = "freeTV")
freeTV_down <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2 & emmeans_equal_long$factor == "freeTV"),],
                           data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2 & emmeans_proportional_long$factor == "freeTV"),],
                           factor = "freeTV")
jpeg("freeTV_emm.jpeg", quality = 100)
grid.arrange(freeTV_up, freeTV_down, nrow = 2, top = "freeTV")
dev.off()

## intNews
intNews_up <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1 & emmeans_equal_long$factor == "intNews"),],
                         data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1 & emmeans_proportional_long$factor == "intNews"),],
                         factor = "intNews")
intNews_down <- plot_fitted(data1 = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2 & emmeans_equal_long$factor == "intNews"),],
                           data2 = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2 & emmeans_proportional_long$factor == "intNews"),],
                           factor = "intNews")
jpeg("intNews_emm.jpeg", quality = 100)
grid.arrange(intNews_up, intNews_down, nrow = 2, top = "intNews")
dev.off()
