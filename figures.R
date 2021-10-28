source('models.R')

#fig2a: plot of litter quality index#1 versus microibal physiological traits index#1
fig2a <- lab_plot(Cbud$litter.pc1, Cbud$mpt.pc1,'Litter quality index', 'Microbial physiological traits')
get_stat(Day30$mpt.pc1, Day30$litter.pc1, Day30)
get_stat(Day185$mpt.pc1, Day185$litter.pc1, Day185)

#fig2b: plot of microibal physiological traits index#1 versus litter-derived MAOM formation
fig2b <- lab_plot(Cbud$mpt.pc1, Cbud$ldmaomc.perc,'Microbial physiological traits', 'MA-SOC formation (% of added litter C)', fit = FALSE) +
  theme(legend.position = 'none')
get_stat(Day30$mpt.pc1, Day30$ldmaomc.perc, Day30)
get_stat(Day185$mpt.pc1, Day185$ldmaomc.perc, Day185)

#fig3b: plot of litter quality index#2 versus cumulative soil-derived respiration
fig3b <- ggplot(Cbud, aes(x = litter.pc2, y = CumFs, color = Day, fill = Day)) +
  #commenting out code to superimpose litter C:N and Lig:N loadings on litter nitrogen index
  #geom_smooth(data = Day185, aes(x = litter.pc2, y = CN_rescale), method = lm, se = FALSE, color = 'grey90', fill = 'grey90', alpha = 0.1)+
  #geom_smooth(data = Day185, aes(x = litter.pc2, y = ligN_rescale), method = lm, se = FALSE, color = 'grey90', fill = 'grey90', alpha = 0.1)+
  geom_point(size = 3) +
  geom_smooth(method = lm, se = TRUE, fullrange = FALSE) +
  labs(x = 'Litter nitrogen index', y = 'Cumulative soil-derived respiration (mg C)')+
  scale_color_discrete(name = 'Decay stage', 
                       breaks = c(30,185), 
                       labels = c('Early', 'Intermediate'))+
  scale_fill_discrete(name = 'Decay stage', 
                      breaks = c(30,185), 
                      labels = c('Early', 'Intermediate'))+
  theme_classic()#+
  #geom_text(x = 0, y = 10.3, label = "AUR:N", color = 'grey90', angle = 325) + 
  #geom_text(x = 0.1, y = 13, label = "C:N", color = 'grey90', angle = 315)
get_stat(Day30$litter.pc2, Day30$CumFs, Day30)
get_stat(Day185$litter.pc2, Day185$CumFs, Day185)

fig4a <- ggplot(data = plot.coeftab.q2(MCTC_mpt), aes(y = Estimate, x = Preds, color = Preds)) +
  geom_point(size = 2) +
  scale_color_manual(values=c("black","grey","black","black","grey","black"))+
  geom_linerange(aes(ymin = lwr, ymax = upr), color = "grey")+
  geom_linerange(aes(ymin = Estimate-std_error, ymax = Estimate+std_error), size = 1)+
  geom_hline(yintercept = 0) +  labs(title = expression(Mineral-associated~SOC~("%"~of~total~C)), x = "Predictors", y = "Standardized coefficient") + 
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none")

fig4b <- ggplot(data = plot.coeftab.q2(Nec_mpt), aes(y = Estimate, x = Preds, color = Preds)) +
  geom_point(size = 2) +
  scale_color_manual(values=c("black","black","grey","grey","grey","black"))+
  geom_linerange(aes(ymin = lwr, ymax = upr), color = "grey")+
  geom_linerange(aes(ymin = Estimate-std_error, ymax = Estimate+std_error), size = 1)+
  geom_hline(yintercept = 0) +  labs(title = "Microbial Necromass", x = "", y = "Standardized coefficient") + 
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none", axis.text.y = element_blank())


########################
#combined figs
########################

fig2ab <- plot_grid(fig2a, fig2b, labels = c('A', 'B'), ncol = 2, align = "h", rel_widths = c(.56,.44))
fig4 <- plot_grid(fig4a, fig4b, labels = c('A','B'), ncol = 2, align = "h", rel_widths = c(.55,.45))

########################
#output
########################
tiff('output/fig2ab.tiff', res = 100, width = 25, height = 10, units = 'cm')
fig2ab
dev.off()

tiff('output/fig3b.tiff', res = 100, width = 12, height = 10, units = 'cm')
fig3b
dev.off()

tiff('output/fig4.tiff', res = 100, width = 22, height = 10, units = 'cm')
fig4
dev.off()
