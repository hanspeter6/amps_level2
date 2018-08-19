# here want to process the output lists from the bootstrap procedure (see myBoot_code.R)
# want to extract the means and determine the boostrapped confidence intervals
# then plot and save as jpegs for interpretation

# load packages
library(tidyverse)
library(ggplot2)
library(gridExtra)

# function to extract means by factor
means.by.factor <- function(factor) {
  
  temp3 <- vector()
  for(i in 1:5) {
    
    # read in the bootstrap output file
    temp1 <- readRDS(paste0("out",i,".rds"))
    
    # extract the means
    temp2 <- matrix(nrow = 110, ncol = 200)
    for(j in 1:200) {
      temp2[,j] <- temp1[[j]][factor][[1]][,'means']
      }
  
    # combine with previous
    temp3 <- cbind(temp3,temp2)
    
  }
  
  # determine vectors of means, upper & lower CIs
  mean <- apply(temp3,1,mean)
  
  f_sort <- function(v) {
    temp <- sort(v)
    temp[c(25,975)]
  }
  
  lower <- t(apply(temp3, 1, f_sort))[,1]
  upper <- t(apply(temp3, 1, f_sort))[,2]
  
  # combine with category and year into single frame
  cbind.data.frame(category = temp1[[1]][[1]][,1],
                   year = temp1[[1]][[1]][,2],
                   mean,
                   lower,
                   upper)           
}

# defining the datasets
popPrint <- means.by.factor("popPrint")
afrikaans <- means.by.factor("afrikaans")
soccer <- means.by.factor("soccer")
african <- means.by.factor("african")
social <- means.by.factor("social")
freeTV <- means.by.factor("freeTV")
news <- means.by.factor("news")

# doing some plots:
vector_row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2 <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")

# function for plotting fitted models
plot_fitted_2 <- function(data, factor) { # factor: one of...
  
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
  ggplot(data = data, aes_string("year", a, group = "category")) +
    geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
    geom_line(size = 0.2) +
    facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
    geom_errorbar(aes_string(ymax = c, ymin = d), size = 0.3, width = 0.4, alpha = 0.5) +
    labs(y = e, title = f) +
    coord_cartesian(ylim=c(-0.5, 0.5)) + 
    scale_y_continuous(breaks=seq(-0.5, 0.5, 0.2))
  
}

## popPrint
pf_popPrint_up <- plot_fitted_2(data = popPrint[which(popPrint$category %in% vector_row1),],
                           factor = "popPrint")
pf_popPrint_down <- plot_fitted_2(data = popPrint[which(popPrint$category %in% vector_row2),],
                             factor = "popPrint")
jpeg("popPrint_emmeans.jpeg", quality = 100)
grid.arrange(pf_popPrint_up, pf_popPrint_down, nrow = 2)
dev.off()

## afrikaans
pf_afrikaans_up <- plot_fitted_2(data = afrikaans[which(afrikaans$category %in% vector_row1),],
                                factor = "afrikaans")
pf_afrikaans_down <- plot_fitted_2(data = afrikaans[which(afrikaans$category %in% vector_row2),],
                                  factor = "afrikaans")
jpeg("afrikaans_emmeans.jpeg", quality = 100)
grid.arrange(pf_afrikaans_up, pf_afrikaans_down, nrow = 2)
dev.off()

## african
pf_african_up <- plot_fitted_2(data = african[which(african$category %in% vector_row1),],
                                 factor = "african")
pf_african_down <- plot_fitted_2(data = african[which(african$category %in% vector_row2),],
                                   factor = "african")
jpeg("african_emmeans.jpeg", quality = 100)
grid.arrange(pf_african_up, pf_african_down, nrow = 2)
dev.off()

## soccer
pf_soccer_up <- plot_fitted_2(data = soccer[which(soccer$category %in% vector_row1),],
                               factor = "soccer")
pf_soccer_down <- plot_fitted_2(data = soccer[which(soccer$category %in% vector_row2),],
                                 factor = "soccer")
jpeg("soccer_emmeans.jpeg", quality = 100)
grid.arrange(pf_soccer_up, pf_soccer_down, nrow = 2)
dev.off()

## social
pf_social_up <- plot_fitted_2(data = social[which(social$category %in% vector_row1),],
                              factor = "social")
pf_social_down <- plot_fitted_2(data = social[which(social$category %in% vector_row2),],
                                factor = "social")
jpeg("social_emmeans.jpeg", quality = 100)
grid.arrange(pf_social_up, pf_social_down, nrow = 2)
dev.off()

## freeTV
pf_freeTV_up <- plot_fitted_2(data = freeTV[which(freeTV$category %in% vector_row1),],
                              factor = "freeTV")
pf_freeTV_down <- plot_fitted_2(data = freeTV[which(freeTV$category %in% vector_row2),],
                                factor = "freeTV")
jpeg("freeTV_emmeans.jpeg", quality = 100)
grid.arrange(pf_freeTV_up, pf_freeTV_down, nrow = 2)
dev.off()

## news
pf_news_up <- plot_fitted_2(data = news[which(news$category %in% vector_row1),],
                              factor = "news")
pf_news_down <- plot_fitted_2(data = news[which(news$category %in% vector_row2),],
                                factor = "news")
jpeg("news_emmeans.jpeg", quality = 100)
grid.arrange(pf_news_up, pf_news_down, nrow = 2)
dev.off()
