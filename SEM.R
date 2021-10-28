library(lavaan)
source('data.R')

#model definitions
path.ldmaom <- 'ldmaomc.perc ~ a*litter.pc1 + b*mpt.pc1
mpt.pc1 ~ c*litter.pc1
indirect := b*c
direct := a
total := a + (b*c)'

path.mse <- 'mse ~ a*litter.pc1 + b*mpt.pc1
mpt.pc1 ~ c*litter.pc1
indirect := b*c
direct := a
total := a + (b*c)'

path.sdmaom <- 'sdmaomc ~ a*litter.pc1 + b*mpt.pc1
mpt.pc1 ~ c*litter.pc1
indirect := b*c
direct := a
total := a + (b*c)'

path.sdpom <- 'sdpomc ~ a*litter.pc1 + b*mpt.pc1
mpt.pc1 ~ c*litter.pc1
indirect := b*c
direct := a
total := a + (b*c)'


#30 day litter-derived MAOM
fit.ldmaom.d30 <- sem(path.ldmaom, data = Day30)

#185 day litter-derived MAOM
fit.ldmaom.185 <- sem(path.ldmaom, data = Day185)

#30 day MSE (matrix stabilization efficiency)
fit.mse.d30 <- sem(path.mse, data = Day30)

#185 day MSE
fit.mse.185 <- sem(path.mse, data = Day185)

#30 day soil-derived MAOM
fit.sdmaom.d30 <- sem(path.sdmaom, data = Day30)

#185 day soil-derived MAOM
fit.sdmaom.185 <- sem(path.sdmaom, data = Day185)

#30 day soil-derived POM
fit.sdpom.d30 <- sem(path.sdpom, data = Day30)

#185 day soil-derived POM
fit.sdpom.185 <- sem(path.sdpom, data = Day185)

sem.mods <- list(fit.ldmaom.d30, fit.ldmaom.185, fit.mse.d30, fit.mse.185, 
                 fit.sdmaom.d30, fit.sdmaom.185, fit.sdpom.d30, fit.sdpom.185)
name.sem.mods <- list('fit.ldmaom.d30', 'fit.ldmaom.185', 'fit.mse.d30', 'fit.mse.185', 
                      'fit.sdmaom.d30', 'fit.sdmaom.185', 'fit.sdpom.d30', 'fit.sdpom.185')
sem.results <- lapply(sem.mods, standardizedsolution)
names(sem.results) <- name.sem.mods

#write output to text file
cat("SEM results", capture.output(sem.results), file="Output/sem_results.txt", sep = "\n")
