###############################################################
###--- plot model output to check differences in models  ---###
###############################################################
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)

setwd("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/")

###--- manipulate data to be in correct format for ggplot graphs

data <- read.table("scrUN_modeloutput.txt", header = FALSE, sep="\t") 

glimpse(data)

names(data) <- c("mestimate",  "sigma", "lam0", "psi","Nhat")

data$model <- as.factor(substr(data$mestimate,1,9))
data$estimate <- as.factor(substring(data$mestimate,11))
data$species <- as.factor(substr(data$model,1,2))
glimpse(data)
names(data)

data2 <- gather(data, key=parameter, value=value, 2:5)
glimpse(data2)
data2$parameter <- as.factor(data2$parameter)
str(data2)
summary(data2)
table(data2$estimate, data2$parameter)

data2$estimate <- mapvalues(data2$estimate, from = c("Point est.", "Upper C.I."),to = c("gd.point", "gd.upper"))

data3 <- data2[c(2:6)]
data.plot <- spread(data3, estimate, value)
head(data.plot)
str(data.plot)

data.plot$updated <- as.factor(substr(data.plot$model,8,8))
data.plot$parameter <- factor(data.plot$parameter, levels = c("sigma", "lam0", "psi", "Nhat"))


##########################################################################
# Plot the Mean and standard deviation for each model by species:  

print({
  p <- ggplot(subset(data.plot, grepl("BB", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, Mean,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(model, ymin = Mean - SD, ymax = Mean + SD), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Black Bear Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/BB_scrUN_output2.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(data.plot, grepl("CA", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, Mean,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(model, ymin = Mean - SD, ymax = Mean + SD), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Caribou Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/CA_scrUN_output2.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(data.plot, grepl("CO", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, Mean,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(model, ymin = Mean - SD, ymax = Mean + SD), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Coyote Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/CO_scrUN_output2.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(data.plot, grepl("LY", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, Mean,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(model, ymin = Mean - SD, ymax = Mean + SD), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Lynx Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/LY_scrUN_output2.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(data.plot, grepl("MO", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, Mean,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(model, ymin = Mean - SD, ymax = Mean + SD), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Moose Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/MO_scrUN_output2.png", width=10, height = 3)
dev.off()

print({
  p <- ggplot(subset(data.plot, grepl("WO", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, Mean,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, scales="free_y", ncol=4)
  p <- p + geom_errorbar(aes(model, ymin = Mean - SD, ymax = Mean + SD), width = 0.25)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + ylab("Estimate ± SD") + xlab("Wolf Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/WO_scrUN_output2.png", width=10, height = 3)
dev.off()



##########################################################################
# Plot the gelman diag for each model by species:  
hline.data <- data.frame(z=c(1,1,1,1), parameter=c("lam0","Nhat","psi","sigma")) 

names(data.plot)

print({
  p <- ggplot(subset(data.plot, grepl("BB", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, gd.point,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + geom_hline(aes(yintercept = z), hline.data)
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Black Bear Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/BB_scrUN_output2_gelman.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(data.plot, grepl("CA", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, gd.point,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + geom_hline(aes(yintercept = z), hline.data)
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Caribou Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/CA_scrUN_output_gelman.png", width=10, height = 3)
dev.off()

print({
  p <- ggplot(subset(data.plot, grepl("CO", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, gd.point,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + geom_hline(aes(yintercept = z), hline.data)
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Coyote Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/CO_scrUN_output_gelman.png", width=10, height = 3)
dev.off()


print({
  p <- ggplot(subset(data.plot, grepl("LY", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, gd.point,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + geom_hline(aes(yintercept = z), hline.data)
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Lynx Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/LY_scrUN_output_gelman.png", width=10, height = 3)
dev.off()

print({
  p <- ggplot(subset(data.plot, grepl("MO", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, gd.point,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + geom_hline(aes(yintercept = z), hline.data)
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Moose Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/MO_scrUN_output_gelman.png", width=10, height = 3)
dev.off()

print({
  p <- ggplot(subset(data.plot, grepl("WO", data.plot$species)))
  p <- p + theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + geom_point(aes(model, gd.point,  colour=updated, fill=updated), size=3)
  p <- p + scale_fill_manual(values=c("T"="cornflowerblue","F"="magenta4")) + scale_colour_manual(values=c("T"="cornflowerblue","F"="magenta4"))
  p <- p + facet_wrap(~parameter, ncol=4)
  p <- p + theme(axis.text.x = element_text(angle = 90))
  p <- p + geom_hline(aes(yintercept = z), hline.data)
  p <- p + ylab("Gelman Diagnostic - Point Estimate") + xlab("Wolf Models")
})
ggsave("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/Densitymodels/WO_scrUN_output_gelman.png", width=10, height = 3)
dev.off()