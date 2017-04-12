## write loop for all outputs
setwd("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/fromWestGrid")

library(xtable)
library(coda)

load("WOout33FL.RData")
load("WOout33FL.ET.RData")

WOout33FL.ET

out <- WOout33FL

nam<-c("sigma","lam0","psi", "N")
nam<-match(nam,dimnames(out[[1]]$sims)[[2]])
#out.lst<-mcmc.list(
#  as.mcmc(out[[1]]$sims[,nam]),
#  as.mcmc(out[[2]]$sims[,nam]),
#  as.mcmc(out[[3]]$sims[,nam]))

out.lst<-mcmc.list(
  as.mcmc(out[[1]]$sims[200001:300000,nam]),
  as.mcmc(out[[2]]$sims[200001:300000,nam]),
  as.mcmc(out[[3]]$sims[200001:300000,nam]))

#gelman.diag(out.lst)
#summary(out.lst)
#effectiveSize(out.lst)
#autocorr.diag(out.lst)
#plot(out.lst)
#gelman.plot(out.lst)

s = summary(out.lst)
gd = gelman.diag(out.lst)


output_table <- rbind(WOout33FL = xtable(as.data.frame(t(s$statistics))),
                      WOout33FL = xtable(as.data.frame(t(s$quantiles))),
                      WOout33FL = xtable(as.data.frame(t(gd$psrf))))

output_table <- autoformat(output_table, zap=3)


#write.table(output_table,
#            file="C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/scrUN_modeloutput.txt",
#            quote=FALSE, sep="\t", col.names=FALSE, append=FALSE)


write.table(output_table,
            file="C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/scRUN_modeloutput.txt",
            quote=FALSE, sep="\t", col.names=FALSE, append=TRUE)

