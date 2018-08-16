#loading packages
library(tidyverse)
library(nFactors)
library(mvnormtest)
library(psych)
library(lavaan)

# reading in the mins (nationals) datasets:
set02_min <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set02_min.rds")
# set05_min <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set05_min.rds")
set08_min <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set08_min.rds")
set10_min <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_min.rds")
set12_min <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min.rds")
set14_min <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set14_min.rds")

# function to add year as variable
add_year <- function(set, year) {
  set %>%
    mutate(year = year) %>%
    dplyr::select(qn, pwgt, year, everything())
}

# combine into single dataset:
# -> excluding 'lifestages' (since NA in 2008),
#  -> excluding 'lifestyle' and 'attitudes' (since missing in 2002); 
#  -> already excluded 2005 since has only one value for internet engagement
set_min <- rbind.data.frame(add_year(set02_min[,!names(set02_min) %in% c("lifestages", "lifestyle", "attitudes")], 2002),
                            add_year(set08_min[,!names(set08_min) %in% c("lifestages", "lifestyle", "attitudes")], 2008),
                            add_year(set10_min[,!names(set10_min) %in% c("lifestages", "lifestyle", "attitudes")], 2010),
                            add_year(set12_min[,!names(set12_min) %in% c("lifestages", "lifestyle", "attitudes")], 2012),
                            add_year(set14_min[,!names(set14_min) %in% c("lifestages", "lifestyle", "attitudes")], 2014))

# for sem, want to change variable names since package doesn't take underscores
name_change <- function(set) {
  names(set)[which(names(set)  %in% c('age_actual','hh_inc','lsm_full'))] <- c("age.actual", "hh.inc", "lsm.full")
  return(set)
}
set_min <- name_change(set_min)

# indicate start and end of media vehicles:
strt <- which(names(set_min) == "Business.Day")
lst <- ncol(set_min)

# scaling the media vehicle variables to mean = 0 and sd = 1 for pooled dataset
set_min <- cbind.data.frame(set_min[,1:strt-1], scale(set_min[,strt:lst]))

# set factors in dataframe
# set factor labels (NB double check levels)
set_min$age <- factor(set_min$age, labels = c("15-24","25-44", "45-54","55+"), ordered = FALSE)
set_min$race <- factor(set_min$race,labels = c("black", "coloured", "indian", "white"), ordered = FALSE)
set_min$edu <- factor(set_min$edu, labels = c("<matric", "matric",">matric" ) ,ordered = FALSE)
set_min$lsm <- factor(set_min$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = FALSE)
set_min$sex <- factor(set_min$sex, labels = c("male", "female"), ordered = FALSE)
set_min$hh_inc <- factor(set_min$hh.inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"), ordered = FALSE)
# set_min$cluster <- factor(set_min$cluster,  labels = c("heavy", "internet", "medium", "light"), ordered = FALSE)
set_min$year <- factor(set_min$year, ordered = FALSE)


# creating separate sets by year:
set_min_02 <- set_min %>%
  filter(year ==  2002)
set_min_08 <- set_min %>%
  filter(year ==  2008)
set_min_10 <- set_min %>%
  filter(year ==  2010)
set_min_12 <- set_min %>%
  filter(year ==  2012)
set_min_14 <- set_min %>%
  filter(year ==  2014)

# save them
saveRDS(set_min, "set_min.rds")

saveRDS(set_min_02, "set_min_02.rds")
saveRDS(set_min_08, "set_min_08.rds")
saveRDS(set_min_10, "set_min_10.rds")
saveRDS(set_min_12, "set_min_12.rds")
saveRDS(set_min_14, "set_min_14.rds")


set_min <- readRDS("set_min.rds")

# pca method (package "nFactors") to estimate optimum number of factors to extract:
## Determine Number of Factors to Extract
ev <- eigen(cor(set_min[,strt:lst]))
ap <- parallel(subject=nrow(set_min[,strt:lst]),var=ncol(set_min[,strt:lst]),
               rep=100,cent=.02)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
jpeg("nScree_min_all.jpeg")
plotnScree(nS, main = "Pooled Sets (N = 126 726)") # optimal = 7
dev.off()

# checking for MV normal
mshapiro.test(t(as.matrix(set_min[sample(nrow(set_min), size = 5000),strt:lst])))

# to compare ML and PA methods of factor extraction on the full set...
# extract factors (using pa method and common factor extraction)
factors_set_min_pa <- fa(r = set_min[,strt:lst], nfactors = 7, fm = "pa")
factors_set_min_ml <- fa(r = set_min[,strt:lst], nfactors = 7, fm = "ml")

# comparing the loadings
round(factors_set_min_pa$loadings, 3)
round(factors_set_min_ml$loadings, 3)

# consider and compare latent structures of simple confirmatory model fits by year

model_cfa <- 
  '
# latent variable definitions:
popPrint =~ Business.Day + Mail.n.Guardian + The.Sunday.Independent + Sunday.Times + You + Car + Cosmopolitan + Getaway + Topcar
afrikaans =~ Rapport + Huisgenoot + Sarie
soccer =~ Soccer.Laduma + Kickoff
african =~ Drum + Bona + Metro.FM
social =~ X5FM + DSTV + int_social + int_radio + int_search
freeTV =~ e.tv + SABC.1 + SABC.2 + SABC.3
news =~ int_print + int_news
'

lavaanList_fit <- semList(model = model_cfa, dataList = list(set_min_02, set_min_08, set_min_10, set_min_12, set_min_14), store.slots = "partable") 
coeffs <- coef(lavaanList_fit)
colnames(coeffs) <- c("2002", "2008", "2010", "2012", "2014")
cor(coeffs)

