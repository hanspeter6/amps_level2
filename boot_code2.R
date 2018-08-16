# code to:
#   take in the full, pooled min dataset:
#   draw a bootstrap sample, ( for now, not controlling for year)
#   estimate a latent model generating scores for each of the seven factors
#   fit a linear regression model on each of the factors as outcome with demographics and interaction effects as predictors
#   using the models, produce EMM for all factor levels (see what I did for type)

# load packages
library(lavaan)
library(dplyr)
library(emmeans)
library(caret)

set_min <- readRDS("set_min.rds")

# draw a bootstrap sample
boot_sample <- set_min[sample(nrow(set_min), size = nrow(set_min), replace = TRUE),]

# define the model
model_sem <- '

# latent variable definitions:
popPrint =~ Business.Day + Mail.n.Guardian + The.Sunday.Independent + Sunday.Times + You + Car + Cosmopolitan + Getaway + Topcar
afrikaans =~ Rapport + Huisgenoot + Sarie
soccer =~ Soccer.Laduma + Kickoff
african =~ Drum + Bona + Metro.FM
social =~ X5FM + DSTV + int_social + int_radio + int_search
freeTV =~ e.tv + SABC.1 + SABC.2 + SABC.3
news =~ int_print + int_news

# variances and covariances of factors

# variances
popPrint ~~ popPrint
african ~~ african
social ~~ social
afrikaans ~~ afrikaans
soccer ~~ soccer
news ~~ news
freeTV ~~ freeTV

#covariances
freeTV ~~ social
freeTV ~~ african
freeTV ~~ afrikaans
freeTV ~~ news
freeTV ~~ popPrint
freeTV ~~ soccer
social ~~ soccer
social ~~ popPrint
social ~~ news
social ~~ african
social ~~ afrikaans
african ~~ afrikaans
african ~~ soccer
african ~~ news
african ~~ popPrint
afrikaans ~~ popPrint
afrikaans ~~ news
afrikaans ~~ soccer
news ~~ soccer
news ~~ popPrint
popPrint ~~ soccer
'
fit_sem <- lavaan::sem(model_sem, data = boot_sample)

# extract predicted score values
pred_sem <- lavPredict(fit_sem)

# create single dataset for use in modelling
set_boot <- cbind.data.frame(boot_sample, pred_sem)

 # linear model fitting
mod_popPrint <- lm(popPrint ~ 
                     age +
                     sex +
                     edu +
                     hh_inc +
                     race +
                     lsm +
                     year +
                     year * age +
                     year * sex +
                     year * edu +
                     year * hh_inc +
                     year * race +
                     year * lsm,
                   data = set_boot)

mod_afrikaans <- lm(afrikaans ~ 
                     age +
                     sex +
                     edu +
                     hh_inc +
                     race +
                     lsm +
                     year +
                     year * age +
                     year * sex +
                     year * edu +
                     year * hh_inc +
                     year * race +
                     year * lsm,
                   data = set_boot)
mod_soccer <- lm(soccer ~ 
                     age +
                     sex +
                     edu +
                     hh_inc +
                     race +
                     lsm +
                     year +
                     year * age +
                     year * sex +
                     year * edu +
                     year * hh_inc +
                     year * race +
                     year * lsm,
                   data = set_boot)
mod_african <- lm(african ~ 
                     age +
                     sex +
                     edu +
                     hh_inc +
                     race +
                     lsm +
                     year +
                     year * age +
                     year * sex +
                     year * edu +
                     year * hh_inc +
                     year * race +
                     year * lsm,
                   data = set_boot)
mod_social <- lm(social ~ 
                     age +
                     sex +
                     edu +
                     hh_inc +
                     race +
                     lsm +
                     year +
                     year * age +
                     year * sex +
                     year * edu +
                     year * hh_inc +
                     year * race +
                     year * lsm,
                   data = set_boot)
mod_freeTV <- lm(freeTV ~ 
                     age +
                     sex +
                     edu +
                     hh_inc +
                     race +
                     lsm +
                     year +
                     year * age +
                     year * sex +
                     year * edu +
                     year * hh_inc +
                     year * race +
                     year * lsm,
                   data = set_boot)
mod_news <- lm(news ~ 
                     age +
                     sex +
                     edu +
                     hh_inc +
                     race +
                     lsm +
                     year +
                     year * age +
                     year * sex +
                     year * edu +
                     year * hh_inc +
                     year * race +
                     year * lsm,
                   data = set_boot)

# setting up marginal means objects

# creating dataset for plotting
# function:
fr_set <- function(model, spec) {
  
  # link marginal means package
  require(emmeans)
  
  # create emmeans object
  temp1 <- emmeans(model, specs = spec, by = "year")
  
  # create subsettable summary object
  temp2 <- summary(temp1)
  
  # create output set
  cbind.data.frame(category = temp2[[1]],
                   year = temp2[[2]],
                   means = temp2[[3]],
                   lower = temp2[[6]],
                   upper = temp2[[7]])
  
  
}

# joining into single frames
emmeans_popPrint <- rbind(fr_set(mod_popPrint, spec = "sex"),
                          fr_set(mod_popPrint, spec = "age"),
                          fr_set(mod_popPrint, spec = "race"),
                          fr_set(mod_popPrint, spec = "edu"),
                          fr_set(mod_popPrint, spec = "hh_inc"),
                          fr_set(mod_popPrint, spec = "lsm"))




# outputting a list of frames for this bootstrap...
list()
