# code for AWS parallel and 5 instances for 1000 bootrapped results
#   take in the full, pooled min dataset:
#   draw a bootstrap sample, ( for now, not controlling for year)
#   estimate a latent model generating scores for each of the seven factors
#   fit a linear regression model on each of the factors as outcome with demographics and interaction effects as predictors
#   using the models, produce EMM for all factor levels
#   do this 200 times. Plan to use 5 instances and 4 cpu's

# load packages
library(lavaan)
library(dplyr)
library(emmeans)
library(foreach)
library(doParallel)

# read in the pooled set
set_min <- readRDS("set_min.rds")

# define the SEM model
model_sem <- '

# latent variable definitions:
popPrint =~ Business.Day + Mail.n.Guardian + The.Sunday.Independent + Sunday.Times + You + Car + Cosmopolitan + Getaway + Topcar
afrikaans =~ Rapport + Huisgenoot + Sarie
soccer =~ Soccer.Laduma + Kickoff
african =~ Drum + Bona + Metro.FM
social =~ X5FM + DSTV + int_social + int_radio + int_search
freeTV =~ e.tv + SABC.1 + SABC.2 + SABC.3
news =~ int_print + int_news

# variances of factors
popPrint ~~ popPrint
african ~~ african
social ~~ social
afrikaans ~~ afrikaans
soccer ~~ soccer
news ~~ news
freeTV ~~ freeTV

#covariances of factors
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

# define function to use to extract emmeans by categorical level
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

# bootstrapping in parallel

# register a cluster:
cl <- makeCluster(4)
registerDoParallel(cl)

out <- foreach(i = 1:200) %dopar% {
  
  require(lavaan)
  require(emmeans)
  
  # draw a bootstrap sample
  boot_sample <- set_min[sample(nrow(set_min), size = nrow(set_min), replace = TRUE),]
  
  # fit the structural model defined above
  fit_sem <- lavaan::sem(model_sem, data = boot_sample)
  
  # extract predicted score values from the model
  pred_sem <- lavPredict(fit_sem)
  
  # create single dataset for use in modelling
  set_boot <- cbind.data.frame(boot_sample, pred_sem)
  
  # linear model fitting by factor
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
  
  # joining into single frames using function defined above (for now just two to test)
  emmeans_popPrint <- rbind(fr_set(mod_popPrint, spec = "sex"),
                            fr_set(mod_popPrint, spec = "age"),
                            fr_set(mod_popPrint, spec = "race"),
                            fr_set(mod_popPrint, spec = "edu"),
                            fr_set(mod_popPrint, spec = "hh_inc"),
                            fr_set(mod_popPrint, spec = "lsm"))
  emmeans_afrikaans <- rbind(fr_set(mod_afrikaans, spec = "sex"),
                             fr_set(mod_afrikaans, spec = "age"),
                             fr_set(mod_afrikaans, spec = "race"),
                             fr_set(mod_afrikaans, spec = "edu"),
                             fr_set(mod_afrikaans, spec = "hh_inc"),
                             fr_set(mod_afrikaans, spec = "lsm"))
  emmeans_soccer <- rbind(fr_set(mod_soccer, spec = "sex"),
                             fr_set(mod_soccer, spec = "age"),
                             fr_set(mod_soccer, spec = "race"),
                             fr_set(mod_soccer, spec = "edu"),
                             fr_set(mod_soccer, spec = "hh_inc"),
                             fr_set(mod_soccer, spec = "lsm"))
  emmeans_african <- rbind(fr_set(mod_african, spec = "sex"),
                             fr_set(mod_african, spec = "age"),
                             fr_set(mod_african, spec = "race"),
                             fr_set(mod_african, spec = "edu"),
                             fr_set(mod_african, spec = "hh_inc"),
                             fr_set(mod_african, spec = "lsm"))
  emmeans_social <- rbind(fr_set(mod_social, spec = "sex"),
                             fr_set(mod_social, spec = "age"),
                             fr_set(mod_social, spec = "race"),
                             fr_set(mod_social, spec = "edu"),
                             fr_set(mod_social, spec = "hh_inc"),
                             fr_set(mod_social, spec = "lsm"))
  emmeans_freeTV <- rbind(fr_set(mod_freeTV, spec = "sex"),
                             fr_set(mod_freeTV, spec = "age"),
                             fr_set(mod_freeTV, spec = "race"),
                             fr_set(mod_freeTV, spec = "edu"),
                             fr_set(mod_freeTV, spec = "hh_inc"),
                             fr_set(mod_freeTV, spec = "lsm"))
  emmeans_news <- rbind(fr_set(mod_news, spec = "sex"),
                             fr_set(mod_news, spec = "age"),
                             fr_set(mod_news, spec = "race"),
                             fr_set(mod_news, spec = "edu"),
                             fr_set(mod_news, spec = "hh_inc"),
                             fr_set(mod_news, spec = "lsm"))
  
  
  # create list of dataframes as output (so final output should be list of lists)
  list(popPrint = emmeans_popPrint,
       afrikaans = emmeans_afrikaans,
       soccer = emmeans_soccer,
       african = emmeans_african,
       social = emmeans_social,
       freeTV = emmeans_freeTV,
       news = emmeans_news)
}

stopCluster(cl)

saveRDS(out, file = "out1.rds")