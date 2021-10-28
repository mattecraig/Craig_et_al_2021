source('models.R')

# Fig S1 showing PCA of litter quality (A), and MPTs (B) for microcosm experiment and PCA of litter quality (C) and MPTs (D) for field experiment
#First rename variables to display on figure
row.names(mpt_pca$rotation) <- c("MGR", "CUE", "MTR")
row.names(mpt_pca_field$rotation) <- c("MGR", "MTR", "CUE")
row.names(litter_pca$rotation) <- c("C:N", "SOL.", "AUR", "LCI", "AUR:N", "s", "k1", "k2")
row.names(lq_pca_field$rotation) <- c("AUR:N", "SOL", "AUR", "C:N", "LCI")

FigS1A <- ggbiplot(litter_pca, varname.adjust = 1.1, alpha = .5) + theme_classic() + labs(x = "PC1 (47.1%)", y = "PC2 (20.9%)") + lims (x = c(NA,2))
FigS1B <- ggbiplot(mpt_pca, varname.adjust = 1.1, alpha = .5) + theme_classic() + labs(x = "PC1 (90.1%)", y = "PC2 (7.3%)") #+ lims(x = c(-3,2))
FigS1C <- ggbiplot(lq_pca_field, varname.adjust = 1.1, alpha = .5) + theme_classic() + labs(x = "PC1 (44.9%)", y = "PC2 (34.7%)") #+ lims(y = c(-3.5,2.5))
FigS1D <- ggbiplot(mpt_pca_field, varname.adjust = 1.1, alpha = .5) + theme_classic() + labs(x = "PC1 (78.9%)", y = "PC2 (13.1%)") #+ lims(x = c(-3,2.5))

FigS1 <- plot_grid(FigS1A, FigS1B, FigS1C, FigS1D, labels = c('A', 'B', 'C', 'D'), align = 'h')
# 
# Fig S2 scatterplot showing relationship between pc2 and mpts
FigS2 <- ggplot(Cbud, aes(x = litter.pc2, y = mpt.pc1, color = Day, fill = Day)) +
  geom_point(size = 3) +
  geom_smooth(data = Day30, method = lm, se = TRUE, fullrange = FALSE) +
  labs(y = 'Microbial physiological traits', x = 'Litter nitrogen index')+
  scale_color_discrete(name = 'Decay stage', 
                       breaks = c(30,185), 
                       labels = c('Early', 'Intermediate'))+
  scale_fill_discrete(name = 'Decay stage', 
                      breaks = c(30,185), 
                      labels = c('Early', 'Intermediate'))+
  theme_classic()
get_stat(Day30$litter.pc2, Day30$mpt.pc1, Day30)
get_stat(Day185$litter.pc2, Day185$mpt.pc1, Day185)

#
# Fig S3 scatterplot showing overall relationship between litter quality and MAOM and led to higher MSE
FigS3A <- ggplot(Cbud, aes(x = litter.pc1, y = ldmaomc.perc, color = Day, fill = Day)) +
  geom_point(size = 3) +
  geom_smooth(data = Day30, method = lm, se = TRUE, fullrange = FALSE) +
  labs(y = 'MA-SOC formation (% of added litter C)', x = '')+
  scale_color_discrete(name = 'Decay stage', 
                       breaks = c(30,185), 
                       labels = c('Early', 'Intermediate'))+
  scale_fill_discrete(name = 'Decay stage', 
                      breaks = c(30,185), 
                      labels = c('Early', 'Intermediate'))+
  theme_classic()
get_stat(Day30$litter.pc1, Day30$ldmaomc.perc, Day30)
get_stat(Day185$litter.pc1, Day185$ldmaomc.perc, Day185)

FigS3B <- ggplot(Cbud, aes(x = litter.pc1, y = mse, color = Day, fill = Day)) +
  geom_point(size = 3) +
  geom_smooth(data = Day30, method = lm, se = TRUE, fullrange = FALSE) +
  labs(y = 'MA-SOC formation efficiency', x = 'Litter quality index (PC1)')+
  scale_color_discrete(name = 'Decay stage', 
                       breaks = c(30,185), 
                       labels = c('Early', 'Intermediate'))+
  scale_fill_discrete(name = 'Decay stage', 
                      breaks = c(30,185), 
                      labels = c('Early', 'Intermediate'))+
  theme_classic()
get_stat(Day30$litter.pc1, Day30$mse, Day30)
get_stat(Day185$litter.pc1, Day185$mse, Day185)

FigS3 <- plot_grid(FigS3A, FigS3B, labels = c('A', 'B'), ncol = 1, align = 'v')
# 
# Fig S4 SEM of maom formation efficiency (see output sem_results.txt)
# 
# Fig S5 MPTs promoted microbial biomass
FigS5 <- lab_plot(Cbud$mpt.pc1, Cbud$MBC.a/1000,'Microbial physiological traits', expression(Microbial~biomass~(mg~C~g~soil^-1)), fit = TRUE)
get_stat(Day30$mpt.pc1, Day30$MBC.a, Day30)
get_stat(Day185$mpt.pc1, Day185$MBC.a, Day185)
# 
# Fig S6 SEM of soil-derived POM (see output sem_results.txt)
# 
# ######################
# Field study
# ######################
# 
# Fig S7 mixed model predicting mpts
# 
FigS7 <- ggplot(data = plot.coeftab.q1(full_mpt), aes(y = Estimate, x = Predictors, color = Predictors)) +
  geom_point(size = 2) +
  scale_color_manual(values=c("grey","grey","grey","grey","grey","black"))+
  geom_linerange(aes(ymin = lwr, ymax = upr), color = "grey")+
  geom_linerange(aes(ymin = Estimate-std_error, ymax = Estimate+std_error), size = 1)+
  geom_hline(yintercept = 0)+ labs(title = "Microbial physiological traits", y = "Standardized coefficient")+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none")

#Fig S8 C:N versus mpts across plots
FigS8 <- ggplot(field_dat, aes(x = CN, y = mpt.pc1, color = Site))+
  geom_point(aes(shape = Site), size = 3)+
  scale_shape_manual(values = c(0,1,15,16,17,18))+
  labs(x = 'Soil C:N', y = "Microbial physiological traits")+
  geom_smooth(aes(x = CN, y = mpt.pc1), method=lm, formula = y~poly(x,2), 
              span = 10, se=FALSE, linetype = 1, color = 'black', size = 1)+
  theme_classic()
summary(lm(formula = mpt.pc1~poly(CN,2), data = field_dat))

# Fig S9 mixed model predicting total MAOM
FigS9 <- ggplot(data = plot.coeftab.q2(MAOMC_mpt), aes(y = Estimate, x = Preds, color = Preds)) +
  geom_point(size = 2) +
  scale_color_manual(values=c("grey","black","black","grey","black","grey"))+
  geom_linerange(aes(ymin = lwr, ymax = upr), color = "grey")+
  geom_linerange(aes(ymin = Estimate-std_error, ymax = Estimate+std_error), size = 1)+
  geom_hline(yintercept = 0) +  labs(title = expression(Mineral-associated~SOC~(g~C~g~soil^-1)), x = "Predictors", y = "Standardized coefficient") + 
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none")
# 

# Fig S10 mixed model predicting fungal (A) and bacterial (B) necromass
FigS10A <- ggplot(data = plot.coeftab.q2(Fung_mpt), aes(y = Estimate, x = Preds, color = Preds)) +
  geom_point(size = 2) +
  scale_color_manual(values=c("black","grey","grey","grey","grey","black"))+
  geom_linerange(aes(ymin = lwr, ymax = upr), color = "grey")+
  geom_linerange(aes(ymin = Estimate-std_error, ymax = Estimate+std_error), size = 1)+
  geom_hline(yintercept = 0) +  labs(title = expression(Fungal~necromass~(mg~C~g~soil^-1)), x = "Predictors", y = "Standardized coefficient") + 
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none")

FigS10B <- ggplot(data = plot.coeftab.q2(Bact_mpt), aes(y = Estimate, x = Preds, color = Preds)) +
  geom_point(size = 2) +
  scale_color_manual(values=c("grey","black","grey","grey","grey","black"))+
  geom_linerange(aes(ymin = lwr, ymax = upr), color = "grey")+
  geom_linerange(aes(ymin = Estimate-std_error, ymax = Estimate+std_error), size = 1)+
  geom_hline(yintercept = 0) +  labs(title = expression(Bacterial~necromass~(mg~C~g~soil^-1)), x = "", y = "Standardized coefficient") + 
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none", axis.text.y = element_blank())

FigS10 <- plot_grid(FigS10A,FigS10B, labels = c('A', 'B'), ncol = 2, align = 'h', rel_widths = c(.55,.45))


#Display figures
FigS1
FigS2
FigS3
FigS5
FigS7
FigS8
FigS9
FigS10