#housekeeping
rm(list=ls())

#load packages
packload <- c("standardize", "Hmisc", "dplyr", "lme4", 
              "lmerTest", "influence.ME", "ggplot2", "coefplot2", 
              "cowplot", "car", "ggbiplot", "readr", "broom")
lapply(packload, library, character.only = TRUE)

#load in custom functions
source("functions.R")

###########################
#Field Data
###########################

#load field data
field_dat <- read_csv("field_data.csv", col_types = cols(.default = "d", Site = "f", Myco = "f"))

#calculations
field_dat <- field_dat %>%
  mutate(CN = C/N,             #Soil C:N ratio       
         MCTC = (MAOMC/C)*100, #MAOM-C (as % of total soil C)
         AS = Glu + Gal + Mur, #total amino sugars
         Turnover = 1/ResTime, #turnover rate (d^-1)
         GluGal = Glu/Gal,     #Glucosamine-to-galactosamine ratio
         GluMur = Glu/Mur,     #Galactosamine-to-muramic acid ratio
         GluSc = (Glu/AS)*100, #Glucosamine (as % of total aminosugars)
         GalSc = (Gal/AS)*100, #Galactosamine (as % of total aminosugars)
         MurSc = (Mur/AS)*100, #Muramic Acid (as % of total aminosugars)  

         #Microbial necromass calculated using equations from: 
         #Liang et al. (2019) Global Change Biology 25:3578-3590
         BactC = (Mur * 45)/1000, #Bacterial necromass carbon ()
         FungC = (((Glu/179.17)-(Mur/251.23))*179.17*9)/1000, #fungal necromass carbon ()
         NecC  = BactC + FungC, #total necromass carbon ()
         NCTC  = NecC/(C*1000)*100, #Necromass carbon (as % of total soil C)
         BCTC = BactC/(C*1000)*100, #Bacterial necromass carbon (as % of total soil C)
         FCTC = FungC/(C*1000)*100, #Fungal necromass carbon (as % of total soil C)
         FNBN = FungC/BactC,        #Fungal-to-bacterial necromass ratio

        #natural-log transformations to meet assumptions of mixed models
         lnGrowth = log(Growth),
         lnTurnover = log(Turnover)
        ) 


#PCA to derive litter-quality index
lq_pca_field <- prcomp(field_dat %>%
                         dplyr::select(LitLigN,LitSol,LitLig,LitCN,LitLCI) %>%
                         dplyr::rename('AUR:N' = LitLigN,
                                       'Soluble content' = LitSol,
                                       'C:N' = LitCN,
                                       'AUR' = LitLig,
                                       'LCI' = LitLCI),
                       center = TRUE, scale = TRUE)

#PCA to derive microbial physiological trait index
mpt_pca_field <- prcomp(field_dat %>%
                          dplyr::select(lnGrowth,lnTurnover, CUE)%>%
                          dplyr::rename('Growth rate' = lnGrowth,
                                        'Turnover rate' = lnTurnover),
                        center = TRUE, scale = TRUE)

  #append pca indices to data
field_dat <- field_dat %>%
  bind_cols(data.frame(mpt_pca_field$x[,1:2])) %>%
  dplyr::rename(mpt.pc1 = PC1, mpt.pc2 = PC2) %>%
  bind_cols(data.frame(lq_pca_field$x[,1:2])) %>%
  dplyr::rename(litter.pc1 = PC1, litter.pc2 = PC2) %>%
  #reverse so high number = high litter quality and high growth, efficiency, and turnover
  mutate(mpt.pc1 = -mpt.pc1,
         litter.pc1 = -litter.pc1)

##Standardize predictors
##Variables to be standardized for Question 1 an Question 2 analyses
##q1: what are the controls on microbial physiological traits?
##q2: to what extent do microbial physiological traits (relative to other drivers) affect soil C pools?
q1_stan_var <- c("CN", "pH", "Nmin", "DOC", "LitLigN", "Froot", "litter.pc1")
q2_stan_var <- c("pH", "Feo", "Clay", "LitLigN", "lnGrowth", "lnTurnover", "CUE", "Froot", "mpt.pc1", "litter.pc1")

#Tibbles standardized to global mean (across-sites)
field_datq1z_across <- field_dat %>%
  dplyr::mutate_at(q1_stan_var, ~scale(.))

field_datq2z_across <- field_dat %>%
  dplyr::mutate_at(q2_stan_var, ~scale(.))

###########################
#Lab Data
###########################
#read in Cbud and join C loss data (i.e. C loss after 30 and 185 days)
Cbud <- read_csv("Cbud.csv", col_types = cols(.default = "d", SP = "c", Day = "f"))

#PCA output
litter_pca <- ldp_pca(vars = c('CN','psoluble', 'plignin', 'lci', 'ligN', 's', 'k1', 'k2'), day = "30")
mpt_pca <- ldp_pca(vars = c('MGR.a', 'CUE.a', 'MTR.a'))

#append mpt and litter PCA to data
Cbud <- Cbud %>% 
  bind_cols(data.frame(mpt_pca$x[,1:2])) %>%
  dplyr::rename(mpt.pc1 = PC1, mpt.pc2 = PC2) %>%
  bind_cols(bind_rows(data.frame(litter_pca$x[,1:2]),data.frame(litter_pca$x[,1:2]))) %>%
  dplyr::rename(litter.pc1 = PC1, litter.pc2 = PC2) %>%
  #reverse so high number = high litter quality and high growth, efficiency, and turnover
  mutate(mpt.pc1 = -mpt.pc1,
         litter.pc1 = -litter.pc1)

#harvest subsets
Day30 <- Cbud %>% filter(Day == "30") 
Day185 <- Cbud %>% filter(Day == "185") 

