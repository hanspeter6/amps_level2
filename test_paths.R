library(lavaan)
library(semPlot)
library(dummies)
# try path diagram

set_min <- readRDS("set_min_simple_print.rds")

set.seed(123) # sample subset and remove some vars not in the model
set_boot <- set_min[sample(nrow(set_min), size =10000),-c(1,2,5,10:13,15:21)]

# adding dummies to prepare frame for SEM:
mod_boot <- dummy.data.frame( data = set_boot, names = c("year", "age", "sex", "edu", "hh.inc", "race", "lsm"), sep = "." )

# # for practice select small set: ( need to read in the full set after done on R itself...)


# smaller model also:

# define the SEM model
model_test <- '

# latent variable definitions:
print5 =~ You + Car + X5FM
freeTV =~ e.tv + SABC.1 + SABC.2

# regression models
print5 ~ age.2 + sex.2 + race.4
freeTV ~  age.2 + sex.2 + race.4
'

# run the small model:
fit_small <- lavaan::sem(model = model_test, data = mod_boot, std.lv = TRUE, meanstructure = TRUE)

# draw path diagramme to pdf file for small model
pdf(file = "small_sem.pdf", width = 12, height = 8, family = "Helvetica", onefile = FALSE) # defaults to 7 x 7 inches
semPaths(fit_small,
         intercept = FALSE,
         whatLabel = "est",
         residuals = TRUE,
         exoCov = FALSE,
         nCharNodes = 0,
         layout = "tree2",
         groups = list(vehicles = c("e.tv", "SABC.1","SABC.2", "Car","You", "X5FM"),
                       repertoires = c("freeTV", "print5"),
                       demographics = c("race.4", "lsm.3", "sex.2", "age.2", "age.3")),
         # col = list(man = "pink", lat = "yellow"),
         pastel = TRUE,
         legend = FALSE,
         nodeLabels = append(rep("", 2), c("1", "r"))
         
         )
dev.off()

##  full model

# define the full SEM model again
fit_full <- '

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

# fit the lavaan
fit_big <- lavaan::sem(model = fit_full, data = mod_boot, std.lv = TRUE, meanstructure = TRUE)

# define empty node names for big one:
empty_demogs <- rep("",100)
full_print <- c("print3","afrikaans","african","social","freeTV", "intnews")
first_join <- append(empty_demogs, full_print)
second_join <- append(first_join, rep("",28))

# draw path diagramme to pdf file for full model
pdf(file = "big_sem.pdf", width = 12, height = 8, family = "Helvetica", onefile = FALSE) # defaults to 7 x 7 inches
semPaths(fit_big,
         intercept = FALSE,
         whatLabel = "no",
         residuals = FALSE,
         exoCov = FALSE,
         layout = "tree",
         
         groups = list(vehicles = c("Business.Day", "Mail.n.Guardian", "The.Sunday.Independent", "Sunday.Times", "You", "Car", "Cosmopolitan", "Getaway", "Topcar", "X5FM",
                                    "Rapport", "Huisgenoot", "Sarie",
                                    "Drum", "Bona", "Metro.FM", "Soccer.Laduma", "Kickoff",
                                    "DSTV", "int.social", "int.radio", "int.search",
                                    "e.tv", "SABC.1", "SABC.2", "SABC.3",
                                    "int.print", "int.news"),
                       repertoires = c("freeTV", "print5", "social", "african", "afrikaans", "intNews"),
                       demographics = c("year.2008", "year.2010", "year.2012", "year.2014", "age.2", "age.3", "age.4", "sex.2",
                                        "edu.2", "edu.3", "hh.inc.2", "hh.inc.3", "hh.inc.4", "race.2", "race.3", "race.4", "lsm.2",
                                        "lsm.3", "lsm.4", "lsm.5", "year.2008:age.2", "year.2008:age.3", "year.2008:age.4", "year.2008:sex.2",
                                        "year.2008:edu.2", "year.2008:edu.3", "year.2008:hh.inc.2", "year.2008:hh.inc.3", "year.2008:hh.inc.4",
                                        "year.2008:race.2", "year.2008:race.3", "year.2008:race.4", "year.2008:lsm.2", "year.2008:lsm.3",
                                        "year.2008:lsm.4", "year.2008:lsm.5", "year.2010:age.2", "year.2010:age.3", "year.2010:age.4",
                                        "year.2010:sex.2", "year.2010:edu.2", "year.2010:edu.3", "year.2010:hh.inc.2", "year.2010:hh.inc.3",
                                        "year.2010:hh.inc.4", "year.2010:race.2", "year.2010:race.3", "year.2010:race.4", "year.2010:lsm.2",
                                        "year.2010:lsm.3", "year.2010:lsm.4", "year.2010:lsm.5", "year.2012:age.2", "year.2012:age.3",
                                        "year.2012:age.4", "year.2012:sex.2", "year.2012:edu.2", "year.2012:edu.3", "year.2012:hh.inc.2",
                                        "year.2012:hh.inc.3", "year.2012:hh.inc.4", "year.2012:race.2", "year.2012:race.3", "year.2012:race.4",
                                        "year.2012:lsm.2", "year.2012:lsm.3", "year.2012:lsm.4", "year.2012:lsm.5", "year.2014:age.2",
                                        "year.2014:age.3", "year.2014:age.4", "year.2014:sex.2", "year.2014:edu.2", "year.2014:edu.3",
                                        "year.2014:hh.inc.2", "year.2014:hh.inc.3", "year.2014:hh.inc.4", "year.2014:race.2", "year.2014:race.3",
                                        "year.2014:race.4", "year.2014:lsm.2", "year.2014:lsm.3", "year.2014:lsm.4", "year.2014:lsm.5")),
         nCharNodes = 0,
         pastel = TRUE,
         legend = FALSE,
         nodeLabels = append(rep("", 112),
                             c("print3","afrikaans","african","social","freeTV", "intnews"))
         
         )
dev.off()





# semPaths(object, what = "paths", whatLabels, style, layout = "tree", 
#          intercepts = TRUE, residuals = TRUE, thresholds = TRUE, 
#          intStyle = "multi", rotation = 1, curve, nCharNodes = 3, 
#          nCharEdges = 3, sizeMan = 5, sizeLat = 8, sizeInt = 2, ask, 
#          mar, title, title.color = "black", include, 
#          combineGroups = FALSE, manifests, latents, groups, color, 
#          residScale, gui = FALSE, allVars = FALSE, edge.color, 
#          reorder = TRUE, structural = FALSE, ThreshAtSide = FALSE, 
#          threshold.color, fixedStyle = 2, freeStyle = 1, 
#          as.expression, optimizeLatRes = FALSE, mixCols = TRUE, 
#          curvePivot, ...)