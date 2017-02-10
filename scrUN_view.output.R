## write loop for all outputs
library(coda)

out <- COout11FL

nam<-c("sigma","lam0","psi", "N")
nam<-match(nam,dimnames(out[[1]]$sims)[[2]])
out.lst<-mcmc.list(
  as.mcmc(out[[1]]$sims[,nam]),
  as.mcmc(out[[2]]$sims[,nam]),
  as.mcmc(out[[3]]$sims[,nam]))


gelman.diag(out.lst)
summary(out.lst)
effectiveSize(out.lst)
autocorr.diag(out.lst)
plot(out.lst)
gelman.plot(out.lst)

library(xtable)
s = summary(lst)
gd = gelman.diag(lst)



output_table <- rbind(stats_table = xtable(as.data.frame(t(s$statistics))),
                      quant_table = xtable(as.data.frame(t(s$quantiles))),
                      psrf_table = xtable(as.data.frame(t(gd$psrf))))

write.table(output_table,
            file="~/R/Analysis/DensityEstimation/BorealDeerProject/BlkBear_Output/stats_table.txt",
            quote=FALSE, sep="\t", col.names=FALSE, append=FALSE)
