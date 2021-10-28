# title: LDP_funcs
# author: MEC
# date: 02/20/2020

#if a "day" (i.e. harvest time) is specified, pca is constructed with only data from those collection
#basically, needed this because initial litter quality data is duplicated in the Cbud dataset 
ldp_pca <- function(vars, day = NULL, ...){
  o <- prcomp(if(is.null(day)){
    Cbud %>%
      dplyr::select(vars) %>%
      dplyr::rename('Growth rate' = MGR.a,
                    'Turnover rate' = MTR.a, 
                    'CUE' = CUE.a)
  } else {
    Cbud %>%
      filter(Day == day) %>%
      dplyr::select(vars) %>%
      dplyr::rename('C:N' = CN,
                    'Soluble content' = psoluble, 
                    'AUR' = plignin, 
                    'LCI' = lci, 
                    'AUR:N' = ligN)
  },center = TRUE, scale = TRUE)
  o
}

#function to format coefplot output for plotting
plot.coeftab.q1 <- function(ob){
  a <- coeftab(ob)
  Predictors <- c("Litter quality", "Nmin", "Soil C:N", "DOC", "pH", "Root mass")
  b <- cbind(Predictors, a[-1,])
  b <- plyr::rename(b, c('Std. Error' = "std_error",
                         '2.5%' = "lwr",
                         '97.5%' = "upr", '25%' = "lwr2", '75%' = "upr2"))
  as.data.frame(b)
}

#obtain R2 and p-values for figures
get_stat <- function(dep, indep, df){
  a <- summary(lm(dep ~ indep, data = df))
  round(c(a$r.squared,a$coefficients[2,4]),2)
}

lab_plot <- function(indep, dep, xlab, ylab, fit = TRUE){
  if(fit == TRUE){
  ggplot(Cbud, aes(x = indep, y = dep, color = Day, fill = Day)) +
    geom_point(size = 3) +
    geom_smooth(method = lm, se = TRUE, fullrange = FALSE) +
    labs(y = ylab, x = xlab)+
      scale_color_discrete(name = 'Decay stage', 
                           breaks = c(30,185), 
                           labels = c('Early', 'Intermediate'))+
      scale_fill_discrete(name = 'Decay stage', 
                           breaks = c(30,185), 
                           labels = c('Early', 'Intermediate'))+
    theme_classic()
  }
  else{
    ggplot(Cbud, aes(x = indep, y = dep, color = Day)) +
      geom_point(size = 3) +
      labs(y = ylab, x = xlab)+
      scale_color_discrete(name = 'Decay stage', 
                           breaks = c(30,185), 
                           labels = c('Early', 'Intermediate'))+
      theme_classic()
  }
}

plot.coeftab.q2 <- function(ob){
  a <- coeftab(ob)
  a <- a[c(3,2,4,5,6,7),]
  Preds <- c("Mic. phys. traits", "Litter quality", "Fe-ox", "Clay", "pH", "Root mass")
  a <- cbind(Preds, a)
  a <- plyr::rename(a, c('Std. Error' = "std_error",
                         '2.5%' = "lwr",
                         '97.5%' = "upr", '25%' = "lwr2", '75%' = "upr2"))
  as.data.frame(a)
  print(a)
}
