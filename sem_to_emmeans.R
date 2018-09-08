# assumes that I have a sem object created by lavaan

# packages
library(lavaan)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# read in the orginal dataset
set_min_st <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/set_min_st.rds")

# create a database of proportions by category levels:
proportion <- vector()
category <- vector()
for(i in c("age", "sex", "edu", "hh.inc","race","lsm")) {
  
  prop_temp <- as.vector(table(set_min_st[,i])/length(set_min_st[,i]))
  
  category <- append(category, paste0(i,".",seq(length(prop_temp))))
  
  proportion <- append(proportion, prop_temp)
  
  proportion_db <- cbind.data.frame(category, proportion)
  
}

# read in the sem object
fit_sem_st <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/fit_sem_st.rds")

# extract coefficients from the SEM object
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

# combine them into two single frames to be populated with either equal or proportional weights 
weights_equal<- cbind(lh_df, rh_df)
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
      
      weights_proportional[i,j] <- proportion_db[which(proportion_db$category == names(weights_proportional)[j]),2 ]

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
myCoefs <- coefs_st[22:525] # need to check names(coefs_st) to id range of regression coefficients

coefs_matrix <- cbind(popPrint = myCoefs[1:84], # need to check names(myCoefs) to id range per factor outcome
                      african = myCoefs[85:168],
                      afrikaans = myCoefs[169:252],
                      news = myCoefs[253:336],
                      freeTV = myCoefs[337:420],
                      soccer = myCoefs[421:504])
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

# doing some plots:
vector_row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2 <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")

# function for plotting fitted models
plot_fitted_emms <- function(data, factor) { # factor: one of...
  
  if(factor == "popPrint") {
    a <- "mean"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "popPrint"
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
  if(factor == "soccer") {
    a <- "mean"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "soccer"
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
  if(factor == "news") {
    a <- "mean"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "news"
  }
  
  #plot
  ggplot(data = data[which(data[,3] == factor),], aes_string("year", a, group = "category")) +
    geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
    geom_line(size = 0.2) +
    facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
    # geom_errorbar(aes_string(ymax = c, ymin = d), size = 0.3, width = 0.4, alpha = 0.5) +
    labs(y = e)
    # coord_cartesian(ylim=c(-0.5, 0.5)) + 
    # scale_y_continuous(breaks=seq(-0.5, 0.5, 0.2))
  
}

## emmeans(equal)

## popPrint
equal_popPrint_up <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1),],
                                factor = "popPrint")
equal_popPrint_down <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2),],
                                  factor = "popPrint")
jpeg("popPrint_emmeans_equal.jpeg", quality = 100)
grid.arrange(equal_popPrint_up, equal_popPrint_down, nrow = 2, top = "popPrint equal weights")
dev.off()

## afrikaans
equal_afrikaans_up <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1),],
                                      factor = "afrikaans")
equal_afrikaans_down <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2),],
                                        factor = "afrikaans")
jpeg("afrikaans_emmeans_equal.jpeg", quality = 100)
grid.arrange(equal_afrikaans_up, equal_afrikaans_down, nrow = 2, top = "afrikaans equal weights")
dev.off()

## african
equal_african_up <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1),],
                                      factor = "african")
equal_african_down <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2),],
                                        factor = "african")
jpeg("african_emmeans_equal.jpeg", quality = 100)
grid.arrange(equal_african_up, equal_african_down, nrow = 2, top = "african equal weights")
dev.off()

## soccer
equal_soccer_up <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1),],
                                      factor = "soccer")
equal_soccer_down <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2),],
                                        factor = "soccer")
jpeg("soccer_emmeans_equal.jpeg", quality = 100)
grid.arrange(equal_soccer_up, equal_soccer_down, nrow = 2, top = "soccer equal weights")
dev.off()

## social
equal_social_up <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1),],
                                      factor = "social")
equal_social_down <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2),],
                                        factor = "social")
jpeg("social_emmeans_equal.jpeg", quality = 100)
grid.arrange(equal_social_up, equal_social_down, nrow = 2, top = "social equal weights")
dev.off()

## freeTV
equal_freeTV_up <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1),],
                                      factor = "freeTV")
equal_freeTV_down <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2),],
                                        factor = "freeTV")
jpeg("freeTV_emmeans_equal.jpeg", quality = 100)
grid.arrange(equal_freeTV_up, equal_freeTV_down, nrow = 2, top = "freeTV equal weights")
dev.off()

## news
equal_news_up <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row1),],
                                      factor = "news")
equal_news_down <- plot_fitted_emms(data = emmeans_equal_long[which(emmeans_equal_long$category %in% vector_row2),],
                                        factor = "news")
jpeg("news_emmeans_equal.jpeg", quality = 100)
grid.arrange(equal_news_up, equal_news_down, nrow = 2, top = "news equal weights")
dev.off()

## emmeans(proportional)

## popPrint
proportional_popPrint_up <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1),],
                                      factor = "popPrint")
proportional_popPrint_down <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2),],
                                        factor = "popPrint")
jpeg("popPrint_emmeans_proportional.jpeg", quality = 100)
grid.arrange(proportional_popPrint_up, proportional_popPrint_down, nrow = 2, top = "popPrint proportional weights")
dev.off()

## afrikaans
proportional_afrikaans_up <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1),],
                                       factor = "afrikaans")
proportional_afrikaans_down <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2),],
                                         factor = "afrikaans")
jpeg("afrikaans_emmeans_proportional.jpeg", quality = 100)
grid.arrange(proportional_afrikaans_up, proportional_afrikaans_down, nrow = 2, top = "afrikaans proportional weights")
dev.off()

## african
proportional_african_up <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1),],
                                     factor = "african")
proportional_african_down <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2),],
                                       factor = "african")
jpeg("african_emmeans_proportional.jpeg", quality = 100)
grid.arrange(proportional_african_up, proportional_african_down, nrow = 2, top = "african proportional weights")
dev.off()

## soccer
proportional_soccer_up <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1),],
                                    factor = "soccer")
proportional_soccer_down <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2),],
                                      factor = "soccer")
jpeg("soccer_emmeans_proportional.jpeg", quality = 100)
grid.arrange(proportional_soccer_up, proportional_soccer_down, nrow = 2, top = "soccer proportional weights")
dev.off()

## social
proportional_social_up <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1),],
                                    factor = "social")
proportional_social_down <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2),],
                                      factor = "social")
jpeg("social_emmeans_proportional.jpeg", quality = 100)
grid.arrange(proportional_social_up, proportional_social_down, nrow = 2, top = "social proportional weights")
dev.off()

## freeTV
proportional_freeTV_up <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1),],
                                    factor = "freeTV")
proportional_freeTV_down <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2),],
                                      factor = "freeTV")
jpeg("freeTV_emmeans_proportional.jpeg", quality = 100)
grid.arrange(proportional_freeTV_up, proportional_freeTV_down, nrow = 2, top = "freeTV proportional weights")
dev.off()

## news
proportional_news_up <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row1),],
                                  factor = "news")
proportional_news_down <- plot_fitted_emms(data = emmeans_proportional_long[which(emmeans_proportional_long$category %in% vector_row2),],
                                    factor = "news")
jpeg("news_emmeans_proportional.jpeg", quality = 100)
grid.arrange(proportional_news_up, proportional_news_down, nrow = 2, top = "news proportional weights")
dev.off()

