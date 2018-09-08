# assumes that I have a sem object created by lavaan

# packages
library(lavaan)
library(dplyr)
library(stringr)

# read in the orginal dataset
set_min_st <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/set_min_st.rds")

# read in the sem object
fit_sem_st <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/fit_sem_st.rds")

# extract coefficients
coefs_sem_st <- coef(fit_sem_st)

# generate the yhat (kind of model matrix) object
yhat <- lavPredict(fit_sem_st, type = "yhat")

# create vector of model matrix colnames (ie the predictors in the various regression equations of the SEM )
names_vector <- colnames(yhat[,which(colnames(yhat) == "year.2008"): ncol(yhat)])

# create a "left-hand" dataframe for category levels by year
lh_df <- expand.grid(year = c("2002","2008", "2010","2012","2014"), category = c("age.1", "age.2", "age.3", "age.4", "sex.1", "sex.2", "edu.1", "edu.2", "edu.3", "hh.inc.1", "hh.inc.2", "hh.inc.3", "hh.inc.4", "race.1", "race.2", "race.3", "race.4", "lsm.1", "lsm.2", "lsm.3", "lsm.4", "lsm.5"))

# create an empty "right-hand" dataframe for the target frame
rh_df <- matrix(0,ncol = length(names_vector), nrow = nrow(lh_df))
colnames(rh_df) <- names_vector
rh_df <- data.frame(rh_df)

# combine them into a single frame to be populated 
df_input <- cbind(lh_df, rh_df)

# populating the dataframe (in this case, based on equal weights - should be comparible to default emmeans)

for(i in 1:nrow(df)) {
  for(j in 3:22) { # only the non interaction terms and excluding the first two columns (year, category)
    
    if(names(df)[j] == paste0("year.",as.character(df[i,1])) | names(df)[j] == as.character(df[i,2])) { # 1 for given level and year
      df[i,j] <- 1
    }
    
    if(!str_sub(names(df)[j], start = 1, end = (str_locate(names(df)[j], "\\.")[1]) -1) %in% str_split(as.character(df[i,2]), "\\.", simplify = TRUE) & 
       str_sub(names(df)[j], start = 1, end = (str_locate(names(df)[j], "\\.")[1]) -1) != "year") { # to generate equal weights for other category levels
      
      if(str_sub(names(df)[j], start = 1, end = (str_locate(names(df)[j], "\\.")[1]) -1) == "sex") {
        df[i,j] <- 1/2
      }
      if(str_sub(names(df)[j], start = 1, end = (str_locate(names(df)[j], "\\.")[1]) -1) == "edu") {
        df[i,j] <- 1/3
      }
      if(str_sub(names(df)[j], start = 1, end = (str_locate(names(df)[j], "\\.")[1]) -1) %in% c("age", "race", "hh")) {
        df[i,j] <- 1/4
      }
      if(str_sub(names(df)[j], start = 1, end = (str_locate(names(df)[j], "\\.")[1]) -1) == "lsm") {
        df[i,j] <- 1/5
      }
      
    }
    
  }
}



# working on option for proportional values instead of equal weighting:

# trying to create a proportions vector:
out <- list()
vec <- vector()

for(i in c("age", "sex", "edu", "hh.inc","race","lsm")) {
  temp1 <- table(set_min_st[,i])/length(set_min_st[,i])
  
  vec <- append(vec, paste0(i,".",seq(length(temp1))))
  
  out <- append(out, temp1)
  
  out <- as.numeric(as.vector(unlist(out)))
}

test <- cbind.data.frame(vec,out)

# populating the remainder of the dataframe to be used in matrix multiplication (multiplying relevant columns of non interaction terms)
df[,23:38] <- df[,3]*df[,c(7:22)]
df[,39:54] <- df[,4]*df[,c(7:22)]
df[,55:70] <- df[,5]*df[,c(7:22)]
df[,71:86] <- df[,6]*df[,c(7:22)]

## to generate marginal means want to multiply the regression coefficients with this matrix
# creating a matrix of regression coefficients:
myCoefs <- coefs_st[22:525] # need to check names(coefs_st) to id range of regression coefficients

coefs_matrix <- cbind(popPrint = myCoefs[1:84], # need to check names(myCoefs) to id range per factor outcome
                      african = myCoefs[85:168],
                      afrikaans = myCoefs[169:252],
                      news = myCoefs[253:336],
                      freeTV = myCoefs[337:420],
                      soccer = myCoefs[421:504])
rownames(coefs_matrix) <- NULL

# matrix multiplication and add back year/category in dataframe
emmeans_temp <- as.matrix(df[,-c(1:2)]) %*% coefs_matrix
emmeans_df  <- cbind.data.frame(df[,c(1:2)], emmeans_temp)

# change order of year to be similar to others
emmeans_fin <- emmeans_df %>%
  arrange(year)

# # if I want to label category level properly for visuals (later)
# levels(emmeans_fin$category)
# levels(emmeans_fin$category) <- c("a", "b") # etc... full vector of levels in the right order
