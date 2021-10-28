source('data.R')

###################
#Q1
###################

#predicting micoribal physiolgoical trait index
full_mpt <- lmer(mpt.pc1 ~  litter.pc1 + Nmin + CN + DOC + pH + Froot + (1|Site),
                        data = field_datq1z_across)


#microbial biomass
mbc_mpt <- lmer(CFE ~  litter.pc1 + mpt.pc1 +  Feo + Clay + pH + Froot + (1|Site),
                data = field_datq1z_across, REML=TRUE)

###################
#Statistical output
###################
#compiling models
q1_mods <- c(full_mpt, mbc_mpt)
q1_mod_names <- c("full_mpt","mbc_mpt")
q1_anovas <- structure(lapply(q1_mods,anova), names = q1_mod_names)
q1_summaries <- structure(lapply(q1_mods,summary), names = q1_mod_names)

#write output to text file
cat("q1 stats", capture.output(q1_anovas, q1_summaries), file="Output/q1_stats.txt", sep = "\n")

###################
#Q2
###################

#MAOM-C (MAOM-C / soil mass)
MAOMC_mpt <- lmer(log(MAOMC) ~  litter.pc1 + mpt.pc1 +  Feo + Clay + pH + Froot + (1|Site),
                  data = field_datq2z_across, REML=TRUE)

#MAOM-C (i.e. MCTC; MAOM-C / Total C)
MCTC_mpt <- lmer(MCTC ~  litter.pc1 + mpt.pc1 +  Feo + Clay + pH + Froot + (1|Site),
                 data = field_datq2z_across, REML=TRUE)

#Total microbial necromass pool
Nec_mpt <-  lmer(NecC ~  litter.pc1 + mpt.pc1 +  Feo + Clay + pH + Froot + (1|Site),
                 data = field_datq2z_across, REML=TRUE)

#Fungal necromass pool
Fung_mpt <- lmer(FungC ~  litter.pc1 + mpt.pc1 +  Feo + Clay + pH + Froot + (1|Site),
                 data = field_datq2z_across, REML=TRUE)

#Bacterial necromass pool
Bact_mpt <- lmer(BactC ~  litter.pc1 + mpt.pc1 +  Feo + Clay + pH + Froot + (1|Site),
                 data = field_datq2z_across, REML=TRUE)

###################
#Statistical output
###################
#compiling models: Output from just 4 best models
q2_mods <- c(MAOMC_mpt, MCTC_mpt, Nec_mpt, Fung_mpt, Bact_mpt)
q2_mod_names <- c("MAOMC_mpt","MCTC_mpt", "Nec_mpt", "Fung_mpt", "Bact_mpt")
q2_anovas <- structure(lapply(q2_mods,anova), names = q2_mod_names)
q2_summaries <- structure(lapply(q2_mods,summary), names = q2_mod_names)

#write output to text file
cat("q2 stats", capture.output(q2_anovas, q2_summaries), file="Output/q2_stats.txt", sep = "\n")