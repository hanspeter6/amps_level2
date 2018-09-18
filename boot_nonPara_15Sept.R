library(lavaan)
library(dplyr)
library(emmeans)
library(dummies)
library(stringr)
library(gmodels)
library(formula.tools)
library(tidyr)

# read in the pooled set (ie completely unstandardised data)
set_min <- readRDS("set_min_simple_print.rds")
# bootstrapping in parallel

out <- list()
for(k in 1:2) {
  
  # draw single bootstrap of all cases
  set_boot <- set_min[sample(nrow(set_min), replace = TRUE, size = 500),]
  
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
  
  # run the sem model:
  fit_sem <- lavaan::sem(model = model_sem, data = mod_boot, std.lv = TRUE, meanstructure = TRUE)
  
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
  
  ## matrix multiplication, add back year/category in dataframe, turn into long format:
  
  # equal
  emmeans_equal <- as.matrix(weights_equal[,-c(1:2)]) %*% coefs_matrix
  emmeans_equal  <- cbind.data.frame(weights_equal[,c(1:2)], emmeans_equal)
  emmeans_equal_long <- gather(emmeans_equal, factor, equal.mean, -c(1,2))
  
  #proportional
  emmeans_proportional <- as.matrix(weights_proportional[,-c(1:2)]) %*% coefs_matrix
  emmeans_proportional  <- cbind.data.frame(weights_proportional[,c(1:2)], emmeans_proportional)
  emmeans_proportional_long <- gather(emmeans_proportional, factor, prop.mean, -c(1,2))
  
  # join them into single data frame
  emmeans <- merge(emmeans_proportional_long,emmeans_equal_long) %>%
    select(factor, category, year, everything()) %>%
    arrange(factor, category, year)
  
  # label category level properly for visuals (later)
  levels(emmeans$category) <- c("15-24","25-44", "45-54","55+",
                                "male", "female",
                                "<matric", "matric",">matric",
                                "<R2500","R2500-R6999","R7000-R11999",">=R12000",
                                "black", "coloured", "indian", "white",
                                "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
  out[[k]] <- emmeans
  
  rm( list = ls()[-which(ls() %in% c("set_min", "out"))] )
  
  gc()
  
}

saveRDS(out, "out_20.rds")
