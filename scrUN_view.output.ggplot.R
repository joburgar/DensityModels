###############################################################
###--- plot model output to check differences in models  ---###
###############################################################
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)

setwd("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/")

SCdata <- read.csv("scrUN_modeloutput.csv")
table(SCdata$Model, SCdata$species)

glimpse(SCdata)
View(SCdata)

SCdata2 <- spread(SCdata, estimate, value)
glimpse(SCdata2)
View(SCdata2)
SCdata2$parameter <- factor(SCdata2$parameter, levels=c("sigma","lam0","psi","Nhat"))


##########################################################################
# Plot the mean and standard deviation for each model by species:  

print({
  p <- ggplot(subset(SCdata2, grepl("black bear", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, mean), size=3)
  # p <- p + scale_fill_manual(values=c("2015"="cornflowerblue","2016"="magenta4")) + scale_colour_manual(values=c("2015"="cornflowerblue","2016"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(Model, ymin = mean - sd, ymax = mean + sd), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/BB_scrUN_output.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(SCdata2, grepl("caribou", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, mean), size=3)
  # p <- p + scale_fill_manual(values=c("2015"="cornflowerblue","2016"="magenta4")) + scale_colour_manual(values=c("2015"="cornflowerblue","2016"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(Model, ymin = mean - sd, ymax = mean + sd), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/CA_scrUN_output.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(SCdata2, grepl("coyote", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, mean), size=3)
  # p <- p + scale_fill_manual(values=c("2015"="cornflowerblue","2016"="magenta4")) + scale_colour_manual(values=c("2015"="cornflowerblue","2016"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(Model, ymin = mean - sd, ymax = mean + sd), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/CO_scrUN_output.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(SCdata2, grepl("caribou", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, mean), size=3)
  # p <- p + scale_fill_manual(values=c("2015"="cornflowerblue","2016"="magenta4")) + scale_colour_manual(values=c("2015"="cornflowerblue","2016"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(Model, ymin = mean - sd, ymax = mean + sd), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/CA_scrUN_output.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(SCdata2, grepl("lynx", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, mean), size=3)
  # p <- p + scale_fill_manual(values=c("2015"="cornflowerblue","2016"="magenta4")) + scale_colour_manual(values=c("2015"="cornflowerblue","2016"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(Model, ymin = mean - sd, ymax = mean + sd), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/LY_scrUN_output.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(SCdata2, grepl("moose", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, mean), size=3)
  # p <- p + scale_fill_manual(values=c("2015"="cornflowerblue","2016"="magenta4")) + scale_colour_manual(values=c("2015"="cornflowerblue","2016"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(Model, ymin = mean - sd, ymax = mean + sd), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/MO_scrUN_output.png", width=10, height = 3)
dev.off()

print({
  p <- ggplot(subset(SCdata2, grepl("wolf", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, mean), size=3)
  # p <- p + scale_fill_manual(values=c("2015"="cornflowerblue","2016"="magenta4")) + scale_colour_manual(values=c("2015"="cornflowerblue","2016"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(Model, ymin = mean - sd, ymax = mean + sd), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/WO_scrUN_output.png", width=10, height = 3)
dev.off()



##########################################################################
# Plot the gelman diag for each model by species:  

print({
  p <- ggplot(subset(SCdata2, grepl("black bear", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, gd.point), size=3)
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/BB_scrUN_output_gelman.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(SCdata2, grepl("caribou", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, gd.point), size=3)
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/CA_scrUN_output_gelman.png", width=10, height = 3)
dev.off()

print({
  p <- ggplot(subset(SCdata2, grepl("coyote", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, gd.point), size=3)
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/CO_scrUN_output_gelman.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(SCdata2, grepl("lynx", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, gd.point), size=3)
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/LY_scrUN_output_gelman.png", width=10, height = 3)
dev.off()

print({
  p <- ggplot(subset(SCdata2, grepl("moose", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, gd.point), size=3)
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/MO_scrUN_output_gelman.png", width=10, height = 3)
dev.off()

print({
  p <- ggplot(subset(SCdata2, grepl("wolf", SCdata2$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(Model, gd.point), size=3)
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Model")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/WO_scrUN_output_gelman.png", width=10, height = 3)
dev.off()