library(tidyverse)
library(gridExtra)
### for processing of boot outputs

# # read the output again
# out_1 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_10.rds")
# out_2 <-  readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/test2.rds")
# out_3 <-  readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_6.rds")
# out_4 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_20_test.rds")
# out_5 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_20_1.rds")
# out_6 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_10_3.rds")
# out_7 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_20_2.rds")
# out_8 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_10_6.rds")
# out_9 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/testing.rds")
# out_10 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_100.rds")
# out_11 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/own_boots1.rds")
# out_12 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_200.rds")
# out_13 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_200_2.rds")
# out_14 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_192_1.rds")
# out_15 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_level2/bootstrap_objects/out_192_2.rds")
# 
# 
# # combining into a single list:
# out_all <- c(out_1, out_2, out_3, out_4, out_5, out_6, out_7,out_8, out_9, out_10,out_11,out_12,out_13, out_14, out_15)
# 
# # save it
# saveRDS(out_all, "out_all.rds")

# read it back
out_all <- readRDS("out_all.rds")

# create mean values of both emmeans_equal and emmeans_proportional, with upper and lower error values
proportional_all <- sapply(out_all, function(x) cbind(x$prop.mean))
equal_all <- sapply(out_all, function(x) cbind(x$equal.mean))

proportional <- apply(proportional_all, 1, mean)
prop.upper <- apply(proportional_all, 1, function(x) sort(x)[floor(length(x) * 0.975)])
prop.lower <- apply(proportional_all, 1, function(x) sort(x)[ceiling(length(x) * 0.025)])

equal <- apply(equal_all, 1, mean)
equal.upper <- apply(equal_all, 1, function(x) sort(x)[floor(length(x) * 0.975)])
equal.lower <- apply(equal_all, 1, function(x) sort(x)[ceiling(length(x) * 0.025)])

emm_set <- cbind.data.frame(out_all[[1]][,1:3],
                      proportional,
                      equal) %>%
  gather(key = weights, value = mean, proportional, equal) %>%
  mutate(upper = c(prop.upper,equal.upper),
         lower = c(prop.lower,equal.lower))

# need to rename income levels
levels(emm_set$category)[levels(emm_set$category) == "<R2500"] <- "<R5000"
levels(emm_set$category)[levels(emm_set$category) == "R2500-R6999"] <- "R5000-R10999"
levels(emm_set$category)[levels(emm_set$category) == "R7000-R11999"] <- "R11000-R19999"
levels(emm_set$category)[levels(emm_set$category) == ">=R12000"] <- "R20000+"

# want to reorder the "category" variables to place gender first for comparible plotting with "type":
emm_set$category <- factor(emm_set$category, levels = c("male", "female",
                                            "15-24", "25-44", "45-54", "55+",
                                            "black", "coloured", "indian",  "white",
                                            "<matric", "matric",  ">matric",
                                            "<R5000", "R5000-R10999", "R11000-R19999", "R20000+",
                                            "LSM1-2",  "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"))


# need to change "intNews" to "intnews"
emm_set$factor[which(emm_set$factor == "intNews")] <- "intnews"

# function for plotting fitted models
plot_emms <- function(dataset, fact) { # factor: one of...
  
  # making sure I have the packages
  require(tidyverse)
  require(gridExtra)
  
  # define upper and lower plots
  row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
  row2 <- c("<matric", "matric",">matric", "<R5000", "R5000-R10999", "R11000-R19999", "R20000+", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
  
  # subset the data by factor
  factor_data <- dataset %>% filter(factor == fact)
    
  # row one plot
  plot_row1 <- ggplot(data = factor_data[which(factor_data$category %in% row1),], aes(x = year, y = mean, group = interaction(category,weights), col = weights)) +
    geom_line(size = 0.5) +
    facet_grid(.~ category) +
    geom_errorbar(aes(ymax = upper, ymin = lower, colour = weights), size = 0.3, width = 0.4, alpha = 0.5) +
    theme(axis.text.x = element_text(size = 6)) +
    labs(y = "engagement") +
    theme(legend.position = "bottom")
    
  # row two plot
  plot_row2 <-  ggplot(data = factor_data[which(factor_data$category %in% row2),], aes(x = year, y = mean, group = interaction(category,weights), col = weights)) +
    geom_line(size = 0.5) +
    facet_grid(.~ category) +
    geom_errorbar(aes(ymax = upper, ymin = lower, colour = weights), size = 0.3, width = 0.4, alpha = 0.5) +
    labs(y = "engagement")
  
  #extract legend
  ##https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend <- function(a.gplot) {
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-g_legend(plot_row1)
  
  grid.arrange(arrangeGrob(plot_row1 + theme(legend.position="none"),
                                 plot_row2 + theme(legend.position="none")),
               top = paste0("Estimated Marginal Means: ", "'", fact, "'"),
               mylegend,
               nrow=2,
               heights=c(10, 1))
  
  # coord_cartesian(ylim=c(-0.5, 0.5)) + 
  # scale_y_continuous(breaks=seq(-0.5, 0.5, 0.2))
}

# # testing function for plotting fitted models
# plot_emms_tester <- function(dataset, fact) { # factor: one of...
#   
#   # making sure I have the packages
#   require(tidyverse)
#   require(gridExtra)
#   
#   # define upper and lower plots
#   row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
#   row2 <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
#   
#   # subset the data by factor
#   factor_data <- dataset %>% filter(factor == fact)
#   
#   # row one plot
#   plot_row1 <- ggplot(data = factor_data[which(factor_data$category %in% row1),], aes(x = year, y = mean, group = interaction(category,weights), col = weights)) +
#     geom_line(size = 0.5) +
#     facet_grid(.~ category) +
#     geom_errorbar(aes(ymax = upper, ymin = lower, colour = weights), size = 0.3, width = 0.4, alpha = 0.5) +
#     theme(axis.text.x = element_text(size = 6)) +
#     labs(y = "engagement") +
#     theme(legend.position = "bottom")
#   
#   # row two plot
#   plot_row2 <-  ggplot(data = factor_data[which(factor_data$category %in% row2),], aes(x = year, y = mean, group = interaction(category,weights), col = weights)) +
#     geom_line(size = 0.5) +
#     facet_grid(.~ category) +
#     geom_errorbar(aes(ymax = upper, ymin = lower, colour = weights), size = 0.3, width = 0.4, alpha = 0.5) +
#     theme(axis.text.x = element_text(size = 2),
#           axis.text.y = element_text(size = 1),
#           axis.title.x = element_text(size = 2),
#           axis.title.y = element_text(size = 2),
#           strip.text.x = element_text(size = 2),
#           ) +
#     labs(y = "engagement")
#   
#   #extract legend
#   ##https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#   g_legend <- function(a.gplot) {
#     tmp <- ggplot_gtable(ggplot_build(a.gplot))
#     leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#     legend <- tmp$grobs[[leg]]
#     return(legend)}
#   
#   mylegend<-g_legend(plot_row1)
#   
#   grid.arrange(arrangeGrob(plot_row1 + theme(legend.position="none"),
#                            plot_row2 + theme(legend.position="none")),
#                top = paste0("Estimated Marginal Means: ", "'", fact, "'"),
#                mylegend,
#                nrow=2,
#                heights=c(10, 1))
#   
#   # coord_cartesian(ylim=c(-0.5, 0.5)) + 
#   # scale_y_continuous(breaks=seq(-0.5, 0.5, 0.2))
# }

jpeg("print5_emm.jpeg", quality = 100)
plot_emms(emm_set, "print5")
dev.off()

jpeg("social_emm.jpeg", quality = 100)
plot_emms(emm_set, "social")
dev.off()

jpeg("african_emm.jpeg", quality = 100)
plot_emms(emm_set, "african")
dev.off()

jpeg("afrikaans_emm.jpeg", quality = 100)
plot_emms(emm_set, "afrikaans")
dev.off()

jpeg("intNews_emm.jpeg", quality = 100)
plot_emms(emm_set, "intnews")
dev.off()

jpeg("freeTV_emm.jpeg", quality = 100)
plot_emms(emm_set, "freeTV")
dev.off()
