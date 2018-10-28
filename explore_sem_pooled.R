#loading packages
library(tidyverse)
library(nFactors)
library(mvnormtest)
library(psych)
library(lavaan)
library(MVN)
library(dummies)

# reading in the mins (nationals) datasets:
set02_min_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set02_min_simple_print.rds")
# set05_min_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set05_min_simple_print.rds")
set08_min_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set08_min_simple_print.rds")
set10_min_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_min_simple_print.rds")
set12_min_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min_simple_print.rds")
set14_min_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set14_min_simple_print.rds")

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
set_min_simple_print <- rbind.data.frame(add_year(set02_min_simple_print[,!names(set02_min_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2002),
                            add_year(set08_min_simple_print[,!names(set08_min_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2008),
                            add_year(set10_min_simple_print[,!names(set10_min_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2010),
                            add_year(set12_min_simple_print[,!names(set12_min_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2012),
                            add_year(set14_min_simple_print[,!names(set14_min_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2014))

# for sem, want to change variable names since package doesn't take underscores and some characters.
name_change <- function(set) {
  names(set)[which(names(set)  %in% c('age_actual','hh_inc','lsm_full', 'int_print', 'int_radio','int_news','int_social','int_search'))] <- c("age.actual", "hh.inc", "lsm.full", 'int.print', 'int.radio','int.news','int.social','int.search')
  return(set)
}
set_min_simple_print <- name_change(set_min_simple_print)

# save
saveRDS(set_min_simple_print, "set_min_simple_print.rds")

# read back
set_min_simple_print <- readRDS("set_min_simple_print.rds")

# indicate start and end of media vehicles:
strt <- which(names(set_min_simple_print) == "Business.Day")
lst <- ncol(set_min_simple_print)

# # standardise pooled set (NOT ANYMORE... THOUGHT TO STANDARDISE LATENT VARIABLES INSTEAD)
# set_min_simple_print_sd <- cbind.data.frame(set_min_simple_print[,1:strt-1], scale(set_min_simple_print[,strt:lst]))

# pca method (package "nFactors") to estimate optimum number of factors to extract:
## Determine Number of Factors to Extract
ev <- eigen(cor(set_min_simple_print[,strt:lst]))
ap <- parallel(subject=nrow(set_min_simple_print[,strt:lst]),var=ncol(set_min_simple_print[,strt:lst]),
               rep=100,cent=.02)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
jpeg("nScree_min_simple_print_all.jpeg")
plotnScree(nS, main = "Pooled Sets (N = 126 726)") # optimal = 5, but eigens > 0 = 8... so will go with six...
dev.off()

# checking for MV normal (first using package "mvnormtest" and samples of 5000)
mshapiro.test(t(as.matrix(set_min_simple_print[sample(nrow(set_min_simple_print), size = 5000),strt:lst])))

# secondly using package "MVN" ??
test_mvn <- mvn(set_min_simple_print[sample(nrow(set_min_simple_print), size = 5000),c(22:49)], multivariatePlot = "qq")
# # or try as a whole with log transform:
# hist(log(as.matrix(set_min_simple_print[,c(22:49)]))) # bell,,, discrete...


# definately not MVN data!!!

# maybe use argument of big sample size...and also compare outcomes of ml and pa.. amd use of bootrapping (see:http://web.pdx.edu/~newsomj/semclass/ho_estimate.pdf)

set_pca <- prcomp(set_min_simple_print[,strt:lst])
set_pca$sdev^2

plot(set_pca)
dim(set_pca$x) # 28 variables


jpeg("freq_scores_per.jpeg")
par(mfrow = c(3,2))
for(i in 1:6) {
  set <- set_pca$x[,i]
  h <- hist(set, breaks = 50, density = 60, col = 'lightgray',
            xlab = "", ylab = "", main = paste0("PC", i),
            yaxt = "n", cex.lab = 0.8, cex.axis = 0.8, cex.main = 0.8)
  xfit <- seq(min(set), max(set), length = 40) 
  yfit <- dnorm(xfit, mean = mean(set), sd = sd(set)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(set) 
  lines(xfit, yfit, col = "orange", lwd = 1)
  mtext("Seperate Histograms of Principal Components 1-6", side = 3, line = -1.5, outer = TRUE)
}
dev.off()

set = set_pca$x[,1:6]
jpeg("freq_scores_all.jpeg")
h <- hist(set, breaks = 50, density = 60, col = 'lightgray',
          xlab = "scores", ylab = "frequency", main = "Histogram of 1-6 Combined Principal Components",
          yaxt = "n")
xfit <- seq(min(set), max(set), length = 40) 
yfit <- dnorm(xfit, mean = mean(set), sd = sd(set)) 
yfit <- yfit * diff(h$mids[1:2]) * length(set) 
lines(xfit, yfit, col = "orange", lwd = 1)
dev.off()

# considering no of components using Afifi etal... see section on PCA in thesis:
# only those pc's  explaining at least 100/p % of the total variance are selected.



# to compare ML and PA methods of factor extraction on the full set...for now with 6 factors (between 5 and 8)
# extract factors (using pa method and common factor extraction)
set.seed(123)
factors_set_min_simple_print_pa <- fa(r = set_min_simple_print[,strt:lst], nfactors = 6, fm = "pa")
set.seed(123)
factors_set_min_simple_print_ml <- fa(r = set_min_simple_print[,strt:lst], nfactors = 6, fm = "ml")

# comparing the loadings shows much the same variables loading together....
loadings_pa <- round(factors_set_min_simple_print_pa$loadings, 3)
loadings_ml <- round(factors_set_min_simple_print_ml$loadings, 3)

## correlating the scores...and the loadings.. ??? very strong correlations..
diag(cor(factors_set_min_simple_print_pa$scores, factors_set_min_simple_print_ml$scores))
diag(cor(loadings_pa, loadings_ml))


cor(factors_set_min_simple_print_ml$scores)
# write the loadings to table for publishing
write.csv(loadings_ml, file = "efa_loadings.csv")

# define simple CFA model to apply for now based on these loadings...
model_cfa <- 
  '
# latent variable definitions:
print5 =~ Business.Day + Mail.n.Guardian + The.Sunday.Independent + Sunday.Times + You + Car + Cosmopolitan + Getaway + Topcar + X5FM
afrikaans =~ Rapport + Huisgenoot + Sarie
african =~ Drum + Bona + Metro.FM + Soccer.Laduma + Kickoff
social =~ DSTV + int.social + int.radio + int.search
freeTV =~ e.tv + SABC.1 + SABC.2 + SABC.3
intnews =~ int.print + int.news

'
# #consider cfa by year and pooled:

# creating separate sets by year (NB from unstandarised set...):
set_min_simple_print_02 <- set_min_simple_print %>%
  filter(year ==  2002)
set_min_simple_print_08 <- set_min_simple_print %>%
  filter(year ==  2008)
set_min_simple_print_10 <- set_min_simple_print %>%
  filter(year ==  2010)
set_min_simple_print_12 <- set_min_simple_print %>%
  filter(year ==  2012)
set_min_simple_print_14 <- set_min_simple_print %>%
  filter(year ==  2014)

saveRDS(set_min_simple_print_02, "set_min_simple_print_02.rds")
saveRDS(set_min_simple_print_08, "set_min_simple_print_08.rds")
saveRDS(set_min_simple_print_10, "set_min_simple_print_10.rds")
saveRDS(set_min_simple_print_12, "set_min_simple_print_12.rds")
saveRDS(set_min_simple_print_14, "set_min_simple_print_14.rds")

# read back
set_min_simple_print_02 <- readRDS("set_min_simple_print_02.rds")
set_min_simple_print_08 <- readRDS("set_min_simple_print_08.rds")
set_min_simple_print_10 <- readRDS("set_min_simple_print_10.rds")
set_min_simple_print_12 <- readRDS("set_min_simple_print_12.rds")
set_min_simple_print_14 <- readRDS("set_min_simple_print_14.rds")


# consider and compare latent structures of simple confirmatory model fits by year
set.seed(123)
lavaanList_fit <- semList(model = model_cfa, dataList = list(set_min_simple_print_02, set_min_simple_print_08, set_min_simple_print_10, set_min_simple_print_12, set_min_simple_print_14, set_min_simple_print), store.slots = "partable", std.lv = TRUE, meanstructure = TRUE) 
coeffs <- coef(lavaanList_fit)
colnames(coeffs) <- c("2002", "2008", "2010", "2012", "2014", "Pooled")
cor(coeffs)

# consider fit measures for confirmatory factor analysis per period (and then for full pooled sets)
set.seed(123)
fit_cfa_02 <- lavaan::cfa(model_cfa, data = set_min_simple_print_02, std.lv = TRUE, meanstructure = TRUE)
round(fitMeasures(fit_cfa_02)[c('ifi','rmsea','srmr','nnfi','tli')], 2)

set.seed(123)
fit_cfa_08 <- lavaan::cfa(model_cfa, data = set_min_simple_print_08, std.lv = TRUE, meanstructure = TRUE)
round(fitMeasures(fit_cfa_08)[c('ifi','rmsea','srmr','nnfi','tli')], 2)

set.seed(123)
fit_cfa_10 <- lavaan::cfa(model_cfa, data = set_min_simple_print_10, std.lv = TRUE, meanstructure = TRUE)
round(fitMeasures(fit_cfa_10)[c('ifi','rmsea','srmr','nnfi','tli')], 2)

set.seed(123)
fit_cfa_12 <- lavaan::cfa(model_cfa, data = set_min_simple_print_12, std.lv = TRUE, meanstructure = TRUE)
round(fitMeasures(fit_cfa_12)[c('ifi','rmsea','srmr','nnfi','tli')], 2)

set.seed(123)
fit_cfa_14 <- lavaan::cfa(model_cfa, data = set_min_simple_print_14, std.lv = TRUE, meanstructure = TRUE)
round(fitMeasures(fit_cfa_14)[c('ifi','rmsea','srmr','nnfi','tli')], 2)

set.seed(123)
fit_cfa <- lavaan::cfa(model = model_cfa, data = set_min_simple_print, std.lv = TRUE, meanstructure = TRUE)
round(fitMeasures(fit_cfa)[c('ifi','rmsea','srmr','nnfi','tli')], 2)

# do simple fit to assess fit of CFA on pooled and standardised data.

#https://stackoverflow.com/questions/39516772/interpreting-lavaan-sem-coefficients
# When performing latent variable modeling, there is always an issue of what scale to assign the unobserved variables. After all, you have not collected data on these latent variables, so how could you possibly know their scale or their variance?
#One way to solve this problem, which is the default in many SEM programs, including the lavaan package in R, is to fix the loading of the first variable for a given latent variable to 1. This has the effect of assigning the scale of that observed variable to the latent variable.
#Another popular alternative is to used a standardized scale for the latent variable (i.e., mean = 0, sd = 1), in which case the loading for the first variable is freely estimated by the model. In lavaan this can be implemented as follows:
#  `fit<-cfa(model, data=df, std.lv=T)`
# Adding std.lv=T tells lavaan to use a standardized scale for the latent variable instead of fixing a loading to 1.

# # consider internal reliability: cronbachs
# print5 =~ Business.Day + Mail.n.Guardian + The.Sunday.Independent + Sunday.Times + You + Car + Cosmopolitan + Getaway + Topcar + X5FM
# afrikaans =~ Rapport + Huisgenoot + Sarie
# african =~ Drum + Bona + Metro.FM + Soccer.Laduma + Kickoff
# social =~ DSTV + int.social + int.radio + int.search
# freeTV =~ e.tv + SABC.1 + SABC.2 + SABC.3
# intnews =~ int.print + int.news

alpha(set_min_simple_print[,c(which(names(set_min_simple_print) %in% c("Business.Day", "Mail.n.Guardian", "The.Sunday.Independent", "Sunday.Times", "You", "Car", "Cosmopolitan", "Getaway", "Topcar", "X5FM")))], title = "print5")
alpha(set_min_simple_print[,c(which(names(set_min_simple_print) %in% c("Drum", "Bona", "Metro.FM", "Soccer.Laduma", "Kickoff")))], title = "african")
alpha(set_min_simple_print[,c(which(names(set_min_simple_print) %in% c("Rapport", "Huisgenoot","Sarie")))], title = "afrikaans")
alpha(set_min_simple_print[,c(which(names(set_min_simple_print) %in% c("DSTV", "int.social", "int.radio", "int.search")))], title = "social")
alpha(set_min_simple_print[,c(which(names(set_min_simple_print) %in% c("e.tv", "SABC.1", "SABC.2", "SABC.3")))], title = "freeTV")
alpha(set_min_simple_print[,c(which(names(set_min_simple_print) %in% c("int.print", "int.news")))], title = "intnews")

alpha(set_min_simple_print_14[,c(which(names(set_min_simple_print_14) %in% c("Business.Day", "Mail.n.Guardian", "The.Sunday.Independent", "Sunday.Times", "You", "Car", "Cosmopolitan", "Getaway", "Topcar", "X5FM")))], title = "print5")
alpha(set_min_simple_print_14[,c(which(names(set_min_simple_print_14) %in% c("Drum", "Bona", "Metro.FM", "Soccer.Laduma", "Kickoff")))], title = "african")
alpha(set_min_simple_print_14[,c(which(names(set_min_simple_print_14) %in% c("Rapport", "Huisgenoot","Sarie")))], title = "afrikaans")
alpha(set_min_simple_print_14[,c(which(names(set_min_simple_print_14) %in% c("DSTV", "int.social", "int.radio", "int.search")))], title = "social")
alpha(set_min_simple_print_14[,c(which(names(set_min_simple_print_14) %in% c("e.tv", "SABC.1", "SABC.2", "SABC.3")))], title = "freeTV")
alpha(set_min_simple_print_14[,c(which(names(set_min_simple_print_14) %in% c("int.print", "int.news")))], title = "intnews")

# predictions (scores)
fit_cfa_predictions <- lavPredict(fit_cfa)
summary(fit_cfa_predictions)
hist(fit_cfa_predictions[,'social']) # o

apply(fit_cfa_predictions, 2, mean) # effectively zer0
apply(fit_cfa_predictions, 2, sd) # close to but not 1.

# identify highest positive score for each case in scaled dataset
top_scores <- apply(scale(fit_cfa_predictions), 1, function(v) names(v)[which.max(v)])

mean(top_scores[,2])
# manipulate set for graphics
set_tops <- set_min_simple_print %>%
  mutate(top = top_scores) %>%
  mutate(POPULATION = "POPULATION") %>%
  gather(key = "toPop", value = "tops", top, POPULATION)

# order factors
set_tops$tops <- factor(set_tops$tops, levels = c("POPULATION", "freeTV", "intnews", "african", "afrikaans", "social", "print5"))

##  some graphics to illustrate profiles of factors:

# name factor levels
# set factor labels (NB double check levels)
set_tops$age <- factor(set_tops$age, labels = c("15-24","25-44", "45-54","55+"), ordered = FALSE)
set_tops$race <- factor(set_tops$race,labels = c("black", "coloured", "indian", "white"), ordered = FALSE)
set_tops$edu <- factor(set_tops$edu, labels = c("<matric", "matric",">matric" ) ,ordered = FALSE)
set_tops$lsm <- factor(set_tops$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = FALSE)
set_tops$sex <- factor(set_tops$sex, labels = c("male", "female"), ordered = FALSE)
set_tops$hh.inc <- factor(set_tops$hh.inc, labels = c("<R5000","R5000-R10999","R11000-R19999","R20000+"), ordered = FALSE)
set_tops$year <- factor(set_tops$year, ordered = FALSE)

#plot counts and proportions of factors in population:
jpeg('factors_proportions.jpeg', quality = 100, type = "cairo")
ggplot( set_tops %>%
         filter(tops != "POPULATION") %>%
         group_by(tops) %>%
         count() %>%
         mutate(percent = (n/(nrow(set_tops)/2)*100)) ) +
  aes(x = tops, y = n, label = paste0(round(percent), "%"), fill = c("2")) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  # scale_fill_brewer(palette = "Spectral")
  geom_text(position = position_stack(vjust = 0.5), size = 4) +
  labs(y = "count", title = "Total Sample Proportions") +
  theme(axis.title.x = element_blank())
dev.off()

# plotting function used in descriptives elsewhere, amended here:
plot_demogs_tops <- function(set, category, palette = c("Dark2", "Accent", "Spectral", "Paired", "Pastel2", "Set2","Set3"), title = category) {
  by_factor <- set %>%
    group_by(tops) %>%
    count_(category) %>%
    mutate(total = sum(n)) %>%
    mutate(percent = (n/total)*100) %>%
    mutate(pos = cumsum(percent) - (0.5 * percent))
  
  label <- paste0(round(100*(by_factor$n/by_factor$total)),"%")
  
  ggplot(by_factor) +
    aes_string(x = "tops", y = "percent", fill =  category, label = "label" ) +
    geom_bar(stat = 'identity') +
    geom_text(position = position_stack(vjust = 0.5), size = 4) +
    # geom_text(aes_string(x = "top", y = "pos"), position = "stack", size = 3) +
    labs(title = title) +
    scale_fill_brewer(palette = palette) +
    guides(fill = guide_legend(title = NULL)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
          
}

jpeg('demog_factors_year.jpeg', quality = 100, type = "cairo")
plot_demogs_tops(set_tops, category = "year", palette = "Dark2", title = "Year")
dev.off()

jpeg('demog_factors_sex.jpeg', quality = 100, type = "cairo")
plot_demogs_tops(set_tops, category = "sex", palette = "Accent", title = "Gender")
dev.off()

jpeg('demog_factors_age.jpeg', quality = 100, type = "cairo")
plot_demogs_tops(set_tops, category = "age", palette = "Spectral", title = "Age Groups")
dev.off()

jpeg('demog_factors_race.jpeg', quality = 100, type = "cairo")
plot_demogs_tops(set_tops, category = "race", palette = "Paired", title = "Population Groups")
dev.off()

jpeg('demog_factors_edu.jpeg', quality = 100, type = "cairo")
plot_demogs_tops(set_tops, category = "edu", palette = "Pastel2", title = "Education")
dev.off()

jpeg('demog_factors_hh_inc.jpeg', quality = 100, type = "cairo")
plot_demogs_tops(set_tops, category = "hh.inc", palette = "Set2", title = "Household Income")
dev.off()

jpeg('demog_factors_lsm.jpeg', quality = 100, type = "cairo")
plot_demogs_tops(set_tops, category = "lsm", palette = "Set3", title = "Living Standards Measure")
dev.off()



# consider link with clusters:
# read in dataset with clusters:
set_simple_print_std_c <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/explore_type/set_simple_print_std_c.rds")

with_clusters <- set_tops %>%
  mutate(cluster = factor(c(set_simple_print_std_c$cluster,set_simple_print_std_c$cluster),  labels = c("heavy", "internet", "medium", "light"), ordered = FALSE))

jpeg("clusters_by_repertoire.jpeg", quality = 100)
plot_demogs_tops(with_clusters, category = "cluster", palette = "Spectral", title = "Cluster Proportions by Repertoire")
dev.off()

## here want to assess this model

# http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/

library(knitr)
options(knitr.kable.NA = '') # this will hide missing values in the kable table

summary(fit_cfa, fit.measures = TRUE, standardized = TRUE) # gives lots of info

## CFI (0.85) and TLI (0.83) suggest.... 
# based on (http://davidakenny.net/cm/fit.htm):
# One alternative null model is that all latent variable correlations are zero 
# the CFI should not be computed if the RMSEA of the null model is less than 0.158 or otherwise one will obtain too small a value of the CFI

# considering an alternative null model as suggested by david a kenny
fit_cfa_null_alt <- lavaan::cfa(model = model_cfa, data = set_min_simple_print, std.lv = TRUE, orthogonal = TRUE)

fitMeasures(fit_cfa_null_alt) 

# since rmsea for null = 0.06.... its less than 0.158, so CFI and TL not reliable... ie too small)

fitMeasures(fit_cfa) 

# RMSEA of 0.05 with p value > 0.05 indicates "close" fit
# NB close fit does not necessarily mean "good" model... Theory behind the model is crucial in deciding "good".. I think the choice of variables by factor can be defended...

# consider the parameters:
parameterEstimates(fit_cfa, standardized = TRUE)


parameterEstimates(fit_cfa)
###@#



library(dplyr) 
library(tidyr)
a <- parameterEstimates(fit_cfa, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  dplyr::select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

write.csv(as.data.frame(a), file = "test.csv")

# consider residuals
#Because our model implies expected relationships among the observed variables, one way to examine its performance
#is to look at the difference between the correlation matrix the model expects and the actual, observed correlation matrix you get from your raw data.
#These (or the equivalent based on covariance matrices) are the residuals of an SEM model.
#Any large residual correlations between variables suggests that there’s something about the relationship between those two indicators that the model
#is not adequately capturing.
residuals(fit_cfa, type = 'cor')$cor ## no large correlations, so OK worry about correlations larger than 0.1. NB these values drive the RMSEA value above

# for categoricals.... lavTables(fit) looks as expected vs observed counts...


# modification indices

modificationIndices(fit_cfa, sort. = TRUE, minimum.value = 3) # looks like big values...not sure how to interpret them. Think should stick to what I have..

# trying to confirm manual calculations of predictions. Q: is it reasonable to use only the coefficients to calculate the emms, what about intercepts???

# fit SEM model using small sample and including meanstructure... shows intercepts for latent variables are all = 0
fit_sem_sample <- sem(model = model_sem, data = mod_boot[sample(nrow(mod_boot), 1000),], std.lv = TRUE, meanstructure = TRUE)

summary(fit_sem_sample, fit.measures = TRUE, standardized = TRUE)
coef(fit_sem_sample)


# nice example to use:
set.seed(1234)
Data <- data.frame(y = rnorm(100), 
                   x1 = rnorm(100), 
                   x2 = rnorm(100),
                   x3 = rnorm(100))
model <- ' y ~ b1*x1 + b2*x2 + b3*x3 '
model <- ' y ~ x1 + x2 + x3 '
fit <- sem(model, data=Data, meanstructure = TRUE)
coef(fit)
summary(fit)
# compare with to show intercept the same....
lm(y ~ x1 + x2 + x3, data = Data)

# and with only a regression in the SEM:
# want to extract value of single latent variable

print5 <- lavPredict(fit_sem_sample)[,'print5']

mean(print5) # not sure why not = 0??



mod_reg <-
  'print5 ~ year.2008 + year.2010 + year.2012 + year.2014 + age.2 + age.3 + age.4 + sex.2 + edu.2 + edu.3 + hh.inc.2 + hh.inc.3 + hh.inc.4 + race.2 + race.3 + race.4 + lsm.2 + lsm.3 + lsm.4 + lsm.5 + year.2008:age.2 + year.2008:age.3 + year.2008:age.4 + year.2008:sex.2 + year.2008:edu.2 + year.2008:edu.3 + year.2008:hh.inc.2 + year.2008:hh.inc.3 + year.2008:hh.inc.4 + year.2008:race.2 + year.2008:race.3 + year.2008:race.4 + year.2008:lsm.2 + year.2008:lsm.3 + year.2008:lsm.4 + year.2008:lsm.5 + year.2010:age.2 + year.2010:age.3 + year.2010:age.4 + year.2010:sex.2 + year.2010:edu.2 + year.2010:edu.3 + year.2010:hh.inc.2 + year.2010:hh.inc.3 + year.2010:hh.inc.4 + year.2010:race.2 + year.2010:race.3 + year.2010:race.4 + year.2010:lsm.2 + year.2010:lsm.3 + year.2010:lsm.4 + year.2010:lsm.5 + year.2012:age.2 + year.2012:age.3 + year.2012:age.4 + year.2012:sex.2 + year.2012:edu.2 + year.2012:edu.3 + year.2012:hh.inc.2 + year.2012:hh.inc.3 + year.2012:hh.inc.4 + year.2012:race.2 + year.2012:race.3 + year.2012:race.4 + year.2012:lsm.2 + year.2012:lsm.3 + year.2012:lsm.4 + year.2012:lsm.5 + year.2014:age.2 + year.2014:age.3 + year.2014:age.4 + year.2014:sex.2 + year.2014:edu.2 + year.2014:edu.3 + year.2014:hh.inc.2 + year.2014:hh.inc.3 + year.2014:hh.inc.4 + year.2014:race.2 + year.2014:race.3 + year.2014:race.4 + year.2014:lsm.2 + year.2014:lsm.3 + year.2014:lsm.4 + year.2014:lsm.5
'

fit_reg <- lavaan::sem(mod_reg,  data = cbind.data.frame(print5, mod_boot[sample(nrow(mod_boot), 2000),]), meanstructure = TRUE )

summary(fit_reg) # interesting, intercept not equal to 0


lm(scale(print5) ~ year.2008 + year.2010 + year.2012 + year.2014 + age.2 + age.3 + age.4 + sex.2 + edu.2 + edu.3 + hh.inc.2 + hh.inc.3 + hh.inc.4 + race.2 + race.3 + race.4 + lsm.2 + lsm.3 + lsm.4 + lsm.5 + year.2008:age.2 + year.2008:age.3 + year.2008:age.4 + year.2008:sex.2 + year.2008:edu.2 + year.2008:edu.3 + year.2008:hh.inc.2 + year.2008:hh.inc.3 + year.2008:hh.inc.4 + year.2008:race.2 + year.2008:race.3 + year.2008:race.4 + year.2008:lsm.2 + year.2008:lsm.3 + year.2008:lsm.4 + year.2008:lsm.5 + year.2010:age.2 + year.2010:age.3 + year.2010:age.4 + year.2010:sex.2 + year.2010:edu.2 + year.2010:edu.3 + year.2010:hh.inc.2 + year.2010:hh.inc.3 + year.2010:hh.inc.4 + year.2010:race.2 + year.2010:race.3 + year.2010:race.4 + year.2010:lsm.2 + year.2010:lsm.3 + year.2010:lsm.4 + year.2010:lsm.5 + year.2012:age.2 + year.2012:age.3 + year.2012:age.4 + year.2012:sex.2 + year.2012:edu.2 + year.2012:edu.3 + year.2012:hh.inc.2 + year.2012:hh.inc.3 + year.2012:hh.inc.4 + year.2012:race.2 + year.2012:race.3 + year.2012:race.4 + year.2012:lsm.2 + year.2012:lsm.3 + year.2012:lsm.4 + year.2012:lsm.5 + year.2014:age.2 + year.2014:age.3 + year.2014:age.4 + year.2014:sex.2 + year.2014:edu.2 + year.2014:edu.3 + year.2014:hh.inc.2 + year.2014:hh.inc.3 + year.2014:hh.inc.4 + year.2014:race.2 + year.2014:race.3 + year.2014:race.4 + year.2014:lsm.2 + year.2014:lsm.3 + year.2014:lsm.4 + year.2014:lsm.5,
  data = cbind.data.frame(print5, mod_boot[sample(nrow(mod_boot), 1000),])
)









# exploring "intnews" a bit... why the disparity rich and poor etc..
# subset to consider only "intnews"
intnews <- set_tops %>%
  filter(tops == "intnews")

table(intnews$lsm)/nrow(intnews) # confirms plots...

# isolate lower income only:
intnews_lowIncome <- intnews %>%
  filter(hh.inc == "<R5000")
# isolate upper incoome only:
intnews_upIncome <- intnews %>%
  filter(hh.inc == "R20000+")

low_colSums <- colSums(intnews_lowIncome[,16:49])
up_colSums <- colSums(intnews_upIncome[,16:49])

upLow <- round(cbind.data.frame(low_colSums,up_colSums))

# isolate lower lsm only:
intnews_lowLSM <- intnews %>%
  filter(lsm == "LSM1-2")
# isolate upper lsm only:
intnews_upLSM <- intnews %>%
  filter(lsm == "LSM9-10")
low_colSums2 <- colSums(intnews_lowLSM[,16:49])
up_colSums2 <- colSums(intnews_upLSM[,16:49])

upLow2 <- round(cbind.data.frame(low_colSums2,up_colSums2))

### NO SOMETHING'S WRONG. WHY DO THESE SCORES SHOW MAX HERE.... GO BACK TO WHERE I SET TOPS...
apply(fit_cfa_predictions, 2, range)

