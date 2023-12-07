

# Code for reproducing results for 
# Inland Navigation is a driver of freshwater biodiversity declines in Europe
# Sexton et al 2023

# Code created 08/09/2023

## Outline ----

# 1. GLMMs on biodiversity metrics
# 2. RLQ-Fourth Corner on traits


# 1. GLMMs ----

# library
library(lme4)
library(sjPlot)
library(car)

###  A. Fish ----
fish <- read.csv("../fish_modeldf_supp.csv")

# Fish Invasives
fish_invsvglm <- lmer(logabI ~
                        Ships + ports_5k + locks_5k + Channelization +
                        Rip_Deg + Agriculture_new + Urban_new +
                        Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                        Channelization:Rip_Deg + Channelization:Agriculture_new +
                        Channelization:Urban_new +
                        Catchment6_size + Strahler + (1|study/year),
                      data = fish, offset = logab)

# Fish Functional Richness
fish_frichglm <- lmer(fric_new ~ 
                        Ships + ports_5k + locks_5k + Channelization + 
                        Rip_Deg + Agriculture_new + Urban_new +
                        Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                        Channelization:Rip_Deg + Channelization:Agriculture_new +
                        Channelization:Urban_new +
                        Catchment6_size + Strahler + (1|study/year),
                      data = fish)

# # Fish Functional Diversity
fish_fdivglm <- lmer(fdiv_new ~
                       Ships + ports_5k + locks_5k + Channelization +
                       Rip_Deg + Agriculture_new + Urban_new +
                       Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                       Channelization:Rip_Deg + Channelization:Agriculture_new +
                       Channelization:Urban_new +
                       Catchment6_size + Strahler + (1|study/year),
                     data = fish)

# # Fish Functional Even
fish_feveglm <- lmer(feve_new ~
                       Ships + ports_5k + locks_5k + Channelization +
                       Rip_Deg + Agriculture_new + Urban_new +
                       Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                       Channelization:Rip_Deg + Channelization:Agriculture_new +
                       Channelization:Urban_new +
                       Catchment6_size + Strahler + (1|study/year),
                     data = fish)

# Fish Taxonomic Richness
fish_richglm <- glmer(Tax_Rich ~ 
                        Ships + ports_5k + locks_5k + Channelization + 
                        Rip_Deg + Agriculture_new + Urban_new +
                        Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                        Channelization:Rip_Deg + Channelization:Agriculture_new +
                        Channelization:Urban_new +
                        Catchment6_size + Strahler + (1|study/year), 
                      data = fish, family = "poisson")

# Fish Taxonomic Diversity
fish_divglm <- lmer(Tax_Div ~ 
                      Ships + ports_5k + locks_5k + Channelization + 
                      Rip_Deg + Agriculture_new + Urban_new +
                      Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                      Channelization:Rip_Deg + Channelization:Agriculture_new +
                      Channelization:Urban_new +
                      Catchment6_size + Strahler + (1|study/year), 
                    data = fish)

# Fish Taxonomic Evennes
fish_eveglm <- lmer(Tax_Even ~ 
                      Ships + ports_5k + locks_5k + Channelization + 
                      Rip_Deg + Agriculture_new + Urban_new +
                      Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                      Channelization:Rip_Deg + Channelization:Agriculture_new +
                      Channelization:Urban_new +
                      Catchment6_size + Strahler + (1|study/year),
                    data = fish)

# To visualize effects
plot_model(fish_richglm, show.values = T)

# Check VIF scores
car::vif(fish_richglm)
car::vif(fish_frichglm)
car::vif(fish_divglm)
car::vif(fish_fdivglm)
car::vif(fish_eveglm)
car::vif(fish_feveglm)
car::vif(fish_invsvglm)

# Results in a table
tab_model(fish_richglm,
          fish_frichglm,
          fish_divglm,
          fish_fdivglm,
          fish_eveglm,
          fish_feveglm,
          fish_invsvglm)




### B. Inverts ----

mim <- read.csv("../mim_modeldf_supp.csv")

# Run models

# Taxonomic Richness
mim_richglm <- glmer(Tax_Rich ~ 
                       Ships + ports_5k + locks_5k + Channelization + 
                       Rip_Deg + Agriculture_new + Urban_new +
                       Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                       Channelization:Rip_Deg + Channelization:Agriculture_new +
                       Channelization:Urban_new +
                       Catchment6_size + Strahler + 
                       (1|study/year) + poly(yr_scaled, 2), 
                     data = mim, family = "poisson")

# mim Invasives
mim_invsvglm <- lmer(logabI ~
                       Ships + ports_5k + locks_5k + Channelization +
                       Rip_Deg + Agriculture_new + Urban_new +
                       Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                       Channelization:Rip_Deg + Channelization:Agriculture_new +
                       Channelization:Urban_new +
                       Catchment6_size + Strahler + 
                       (1|study/year) + poly(yr_scaled, 2),
                     data = mim, offset = logab)


# mim Functional Richness
mim_frichglm <- lmer(fric_new ~ 
                       Ships + ports_5k + locks_5k + Channelization + 
                       Rip_Deg + Agriculture_new + Urban_new +
                       Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                       Channelization:Rip_Deg + Channelization:Agriculture_new +
                       Channelization:Urban_new +
                       Catchment6_size + Strahler + 
                       (1|study/year) + poly(yr_scaled, 2),
                     data = mim)


# # mim Functional Diversity
mim_fdivglm <- lmer(fdiv_new ~
                      Ships + ports_5k + locks_5k + Channelization +
                      Rip_Deg + Agriculture_new + Urban_new +
                      Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                      Channelization:Rip_Deg + Channelization:Agriculture_new +
                      Channelization:Urban_new +
                      Catchment6_size + Strahler + 
                      (1|study/year) + poly(yr_scaled, 2),
                    data = mim)


# # mim Functional Even
mim_feveglm <- lmer(feve_new ~
                      Ships + ports_5k + locks_5k + Channelization +
                      Rip_Deg + Agriculture_new + Urban_new +
                      Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                      Channelization:Rip_Deg + Channelization:Agriculture_new +
                      Channelization:Urban_new +
                      Catchment6_size + Strahler + 
                      (1|study/year) + poly(yr_scaled, 2),
                    data = mim)


# mim Taxonomic Diversity
mim_divglm <- lmer(Tax_Div ~ 
                     Ships + ports_5k + locks_5k + Channelization + 
                     Rip_Deg + Agriculture_new + Urban_new +
                     Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                     Channelization:Rip_Deg + Channelization:Agriculture_new +
                     Channelization:Urban_new +
                     Catchment6_size + Strahler + 
                     (1|study/year) + poly(yr_scaled, 2), 
                   data = mim)


# mim Taxonomic Evennes
mim_eveglm <- lmer(Tax_Even ~ 
                     Ships + ports_5k + locks_5k + Channelization + 
                     Rip_Deg + Agriculture_new + Urban_new +
                     Ships:Rip_Deg + Ships:Agriculture_new + Ships:Urban_new +
                     Channelization:Rip_Deg + Channelization:Agriculture_new +
                     Channelization:Urban_new +
                     Catchment6_size + Strahler + 
                     (1|study/year) + poly(yr_scaled, 2),
                   data = mim)

# Visualize effects
plot_model(mim_richglm, show.values = T)

# VIF
car::vif(mim_richglm)
car::vif(mim_frichglm)
car::vif(mim_divglm)
car::vif(mim_fdivglm)
car::vif(mim_eveglm)
car::vif(mim_feveglm)
car::vif(mim_invsvglm)
# R2
tab_model(mim_richglm,
          mim_frichglm,
          mim_divglm,
          mim_fdivglm,
          mim_eveglm,
          mim_feveglm,
          mim_invsvglm)

rm(list = ls())









# 2. RLQ-4C ----

# Library
library(ade4)
library(tidyverse)


### A. Inverts ----

spe         <- read.csv("../invert_spe_rlq.csv", row.names = 1)
env         <- read.csv("../invert_env_rlq.csv", row.names = 1)
mimm_traits <- read.csv("../invert_trt_rlq.csv", row.names = 1)


#

## RLQ
afcL.mim <- dudi.coa(spe, scannf = F)
afcR.mim <- dudi.pca(env, row.w = afcL.mim$lw, scale = T, scannf = F)
afcQ.mim <- dudi.hillsmith(mimm_traits, row.w = afcL.mim$cw, scannf = F)
rlq.mim  <- rlq(afcR.mim, afcL.mim, afcQ.mim, scannf = F)

# Plotting
s.label(rlq.mim$l1)
s.arrow(rlq.mim$c1, boxes = F, add.plot = T)
# Make a df for trait and env arrows
trt_arrows <- rlq.mim$c1
env_arrows <- rlq.mim$l1

str(trt_arrows)
trt_arrows$Trait <- rownames(trt_arrows)
trt_arrows[,4:5] <- str_split_fixed(trt_arrows$Trait, "_", 2)
trt_arrows <- rename(trt_arrows, 
                     Trait_class = V4,
                     Trait_subset = V5)
trt_arrows$Trait_subset <- gsub("_", " ",
                                as.character(trt_arrows$Trait_subset))
trt_arrows$Trait_subset <- gsub(".", " ", fixed = T,
                                as.character(trt_arrows$Trait_subset))
trt_arrows$Trait_class <- gsub("Loco", "Locomotion",
                               as.character(trt_arrows$Trait_class))
trt_arrows$Trait_subset <- str_to_title(as.character(trt_arrows$Trait_subset))

env_arrows$Predictor <- rownames(env_arrows)
env_arrows$Predictor <- str_to_title(as.character(env_arrows$Predictor))
env_arrows$Predictor <- gsub("Canals", "Channelization",
                             as.character(env_arrows$Predictor))

# Plot it
ggplot() +
  geom_text(data = env_arrows,  
            aes(x = RS1, RS2, label = Predictor),
            size = 12, fontface = "bold") +
  geom_label(data = trt_arrows, aes(x = CS1, CS2, label = Trait, 
                                    fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 6, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_discrete(l = 40)

# Plot it facet wrapped
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = env_arrows,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows, aes(x = CS1, CS2, label = Trait_subset, 
                                    fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 6, colour = "white") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray")) + 
  xlim(c(-0.8,0.5))


# Plot it with the inverse values for easier interpretation
# Shorten one of the names
trt_arrows$Trait_subset <- gsub("Flags Boulders Cobbles Pebbles", "Boulders Pebbles", trt_arrows$Trait_subset)

ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = env_arrows,  
            aes(x = -RS1, -RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows, aes(x = -CS1, -CS2, label = Trait_subset, 
                                    fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 6, colour = "white") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray")) + 
  xlim(c(-0.5,0.8))


#
#

# Now Run the 4th Corner
FC_axes_mim <- fourthcorner.rlq(rlq.mim, modeltype = 6,
                                typetest = "axes",  
                                p.adjust.method.G = "fdr",
                                p.adjust.method.D = "fdr")
plot(FC_axes_mim)
print(FC_axes_mim)

#
#


####    i. Split by land use ----



######    Urbanization ---- 

# Trim by urbanization rate low

# Read in the trimmer file
mim_u25 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/mim_u25.csv")
str(mim_u25)
mim_u25 <- select(mim_u25, operatn)

# Trim env
env$operatn <- rownames(env)
env_mim_u25 <- left_join(mim_u25, env)
env_mim_u25 <- env_mim_u25 %>% drop_na(ships)
env_mim_u25 <- env_mim_u25[!duplicated(env_mim_u25$operatn),]
rownames(env_mim_u25) <- env_mim_u25$operatn
env_mim_u25 <- select(env_mim_u25, -operatn)

# Trim spe
spe$operatn <- rownames(spe)
spe_mim_u25 <- left_join(mim_u25, spe)
str(spe_mim_u25)
spe_mim_u25 <- spe_mim_u25 %>% drop_na(Coenagrion)
spe_mim_u25 <- spe_mim_u25[!duplicated(spe_mim_u25$operatn),]
rownames(spe_mim_u25) <- spe_mim_u25$operatn
spe_mim_u25 <- select(spe_mim_u25, -operatn)

# Run RLQ
afcL.mim_u25 <- dudi.coa(spe_mim_u25, scannf = F)
afcR.mim_u25 <- dudi.pca(env_mim_u25, row.w = afcL.mim_u25$lw, scale = T, scannf = F)
afcQ.mim_u25 <- dudi.hillsmith(mimm_traits, row.w = afcL.mim_u25$cw, scannf = F)
rlq.mim_u25  <- rlq(afcR.mim_u25, afcL.mim_u25, afcQ.mim_u25, scannf = F)

# Plotting files
trt_arrows_u25 <- rlq.mim_u25$c1
env_arrows_u25 <- rlq.mim_u25$l1

trt_arrows_u25$Trait <- rownames(trt_arrows_u25)
trt_arrows_u25[,4:5] <- str_split_fixed(trt_arrows_u25$Trait, "_", 2)
trt_arrows_u25 <- rename(trt_arrows_u25, 
                         Trait_class = V4,
                         Trait_subset = V5)
trt_arrows_u25$Trait_subset <- gsub("_", " ",
                                    as.character(trt_arrows_u25$Trait_subset))
trt_arrows_u25$Trait_subset <- gsub(".", " ", fixed = T,
                                    as.character(trt_arrows_u25$Trait_subset))
trt_arrows_u25$Trait_class <- gsub("Loco", "Locomotion",
                                   as.character(trt_arrows_u25$Trait_class))
trt_arrows_u25$Trait_subset <- str_to_title(as.character(trt_arrows_u25$Trait_subset))

env_arrows_u25$Predictor <- rownames(env_arrows_u25)
env_arrows_u25$Predictor <- str_to_title(as.character(env_arrows_u25$Predictor))
env_arrows_u25$Predictor <- gsub("Canals", "Channelization",
                                 as.character(env_arrows_u25$Predictor))

ggplot() +
  geom_text(data = env_arrows_u25,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_u25, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 4, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))


# Now 4C
FC_axes_mim_u25 <- fourthcorner.rlq(rlq.mim_u25, modeltype = 6,
                                    typetest = "axes",  
                                    p.adjust.method.G = "fdr",
                                    p.adjust.method.D = "fdr")
print(FC_axes_mim_u25)
trt_arrows_u25



# Urban 75%
mim_u75 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/mim_u75.csv")
str(mim_u75)
mim_u75 <- select(mim_u75, operatn)

# Trim env
env$operatn <- rownames(env)
env_mim_u75 <- left_join(mim_u75, env)
env_mim_u75 <- env_mim_u75 %>% drop_na(ships)
env_mim_u75 <- env_mim_u75[!duplicated(env_mim_u75$operatn),]
rownames(env_mim_u75) <- env_mim_u75$operatn
env_mim_u75 <- select(env_mim_u75, -operatn)

# Trim spe
spe$operatn <- rownames(spe)
spe_mim_u75 <- left_join(mim_u75, spe)
str(spe_mim_u75)
spe_mim_u75 <- spe_mim_u75 %>% drop_na(Coenagrion)
spe_mim_u75 <- spe_mim_u75[!duplicated(spe_mim_u75$operatn),]
rownames(spe_mim_u75) <- spe_mim_u75$operatn
spe_mim_u75 <- select(spe_mim_u75, -operatn)

# Run it
afcL.mim_u75 <- dudi.coa(spe_mim_u75, scannf = F)
afcR.mim_u75 <- dudi.pca(env_mim_u75, row.w = afcL.mim_u75$lw, scale = T, scannf = F)
afcQ.mim_u75 <- dudi.hillsmith(mimm_traits, row.w = afcL.mim_u75$cw, scannf = F)
rlq.mim_u75  <- rlq(afcR.mim_u75, afcL.mim_u75, afcQ.mim_u75, scannf = F)


trt_arrows_u75 <- rlq.mim_u75$c1
env_arrows_u75 <- rlq.mim_u75$l1

str(trt_arrows_u75)
trt_arrows_u75$Trait <- rownames(trt_arrows_u75)
trt_arrows_u75[,4:5] <- str_split_fixed(trt_arrows_u75$Trait, "_", 2)
trt_arrows_u75 <- rename(trt_arrows_u75, 
                         Trait_class = V4,
                         Trait_subset = V5)
trt_arrows_u75$Trait_subset <- gsub("_", " ",
                                    as.character(trt_arrows_u75$Trait_subset))
trt_arrows_u75$Trait_subset <- gsub(".", " ", fixed = T,
                                    as.character(trt_arrows_u75$Trait_subset))
trt_arrows_u75$Trait_class <- gsub("Loco", "Locomotion",
                                   as.character(trt_arrows_u75$Trait_class))
trt_arrows_u75$Trait_subset <- str_to_title(as.character(trt_arrows_u75$Trait_subset))

env_arrows_u75$Predictor <- rownames(env_arrows_u75)
env_arrows_u75$Predictor <- str_to_title(as.character(env_arrows_u75$Predictor))
env_arrows_u75$Predictor <- gsub("Canals", "Channelization",
                                 as.character(env_arrows_u75$Predictor))

ggplot() +
  geom_text(data = env_arrows_u75,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_u75, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 4, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))


# Now 4C
FC_axes_mim.mim_u75 <- fourthcorner.rlq(rlq.mim_u75, modeltype = 6,
                                        typetest = "axes",  
                                        p.adjust.method.G = "fdr",
                                        p.adjust.method.D = "fdr")
plot(FC_axes_mim)
print(FC_axes_mim.mim_u75)






#####   Agriculture ----

# Read in the trimmer file
mim_a25 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/mim_a25.csv")
str(mim_a25)
mim_a25 <- select(mim_a25, operatn)

# Trim env
env$operatn <- rownames(env)
env_mim_a25 <- left_join(mim_a25, env)
env_mim_a25 <- env_mim_a25 %>% drop_na(ships)
env_mim_a25 <- env_mim_a25[!duplicated(env_mim_a25$operatn),]
rownames(env_mim_a25) <- env_mim_a25$operatn
env_mim_a25 <- select(env_mim_a25, -operatn)

# Trim spe
spe$operatn <- rownames(spe)
spe_mim_a25 <- left_join(mim_a25, spe)
str(spe_mim_a25)
spe_mim_a25 <- spe_mim_a25 %>% drop_na(Coenagrion)
spe_mim_a25 <- spe_mim_a25[!duplicated(spe_mim_a25$operatn),]
rownames(spe_mim_a25) <- spe_mim_a25$operatn
spe_mim_a25 <- select(spe_mim_a25, -operatn)

# Run RLQ
afcL.mim_a25 <- dudi.coa(spe_mim_a25, scannf = F)
afcR.mim_a25 <- dudi.pca(env_mim_a25, row.w = afcL.mim_a25$lw, scale = T, scannf = F)
afcQ.mim_a25 <- dudi.hillsmith(mimm_traits, row.w = afcL.mim_a25$cw, scannf = F)
rlq.mim_a25  <- rlq(afcR.mim_a25, afcL.mim_a25, afcQ.mim_a25, scannf = F)

# Plotting
trt_arrows_a25 <- rlq.mim_a25$c1
env_arrows_a25 <- rlq.mim_a25$l1

trt_arrows_a25$Trait <- rownames(trt_arrows_a25)
trt_arrows_a25[,4:5] <- str_split_fixed(trt_arrows_a25$Trait, "_", 2)
trt_arrows_a25 <- rename(trt_arrows_a25, 
                         Trait_class = V4,
                         Trait_subset = V5)
trt_arrows_a25$Trait_subset <- gsub("_", " ",
                                    as.character(trt_arrows_a25$Trait_subset))
trt_arrows_a25$Trait_subset <- gsub(".", " ", fixed = T,
                                    as.character(trt_arrows_a25$Trait_subset))
trt_arrows_a25$Trait_class <- gsub("Loco", "Locomotion",
                                   as.character(trt_arrows_a25$Trait_class))
trt_arrows_a25$Trait_subset <- str_to_title(as.character(trt_arrows_a25$Trait_subset))

env_arrows_a25$Predictor <- rownames(env_arrows_a25)
env_arrows_a25$Predictor <- str_to_title(as.character(env_arrows_a25$Predictor))
env_arrows_a25$Predictor <- gsub("Canals", "Channelization",
                                 as.character(env_arrows_a25$Predictor))

ggplot() +
  geom_text(data = env_arrows_a25,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_a25, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 4, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))


# Now 4C
FC_axes_mim_a25 <- fourthcorner.rlq(rlq.mim_a25, modeltype = 6,
                                    typetest = "axes",  
                                    p.adjust.method.G = "fdr",
                                    p.adjust.method.D = "fdr")
print(FC_axes_mim_a25)
trt_arrows_a25



# Agriculture 75%
mim_a75 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/mim_a75.csv")
str(mim_a75)
mim_a75 <- select(mim_a75, operatn)

# Trim env
env$operatn <- rownames(env)
env_mim_a75 <- left_join(mim_a75, env)
env_mim_a75 <- env_mim_a75 %>% drop_na(ships)
env_mim_a75 <- env_mim_a75[!duplicated(env_mim_a75$operatn),]
rownames(env_mim_a75) <- env_mim_a75$operatn
env_mim_a75 <- select(env_mim_a75, -operatn)

# Trim spe
spe$operatn <- rownames(spe)
spe_mim_a75 <- left_join(mim_a75, spe)
str(spe_mim_a75)
spe_mim_a75 <- spe_mim_a75 %>% drop_na(Coenagrion)
spe_mim_a75 <- spe_mim_a75[!duplicated(spe_mim_a75$operatn),]
rownames(spe_mim_a75) <- spe_mim_a75$operatn
spe_mim_a75 <- select(spe_mim_a75, -operatn)

# Run it
afcL.mim_a75 <- dudi.coa(spe_mim_a75, scannf = F)
afcR.mim_a75 <- dudi.pca(env_mim_a75, row.w = afcL.mim_a75$lw, scale = T, scannf = F)
afcQ.mim_a75 <- dudi.hillsmith(mimm_traits, row.w = afcL.mim_a75$cw, scannf = F)
rlq.mim_a75  <- rlq(afcR.mim_a75, afcL.mim_a75, afcQ.mim_a75, scannf = F)


trt_arrows_a75 <- rlq.mim_a75$c1
env_arrows_a75 <- rlq.mim_a75$l1

str(trt_arrows_a75)
trt_arrows_a75$Trait <- rownames(trt_arrows_a75)
trt_arrows_a75[,4:5] <- str_split_fixed(trt_arrows_a75$Trait, "_", 2)
trt_arrows_a75 <- rename(trt_arrows_a75, 
                         Trait_class = V4,
                         Trait_subset = V5)
trt_arrows_a75$Trait_subset <- gsub("_", " ",
                                    as.character(trt_arrows_a75$Trait_subset))
trt_arrows_a75$Trait_subset <- gsub(".", " ", fixed = T,
                                    as.character(trt_arrows_a75$Trait_subset))
trt_arrows_a75$Trait_class <- gsub("Loco", "Locomotion",
                                   as.character(trt_arrows_a75$Trait_class))
trt_arrows_a75$Trait_subset <- str_to_title(as.character(trt_arrows_a75$Trait_subset))

env_arrows_a75$Predictor <- rownames(env_arrows_a75)
env_arrows_a75$Predictor <- str_to_title(as.character(env_arrows_a75$Predictor))
env_arrows_a75$Predictor <- gsub("Canals", "Channelization",
                                 as.character(env_arrows_a75$Predictor))

ggplot() +
  geom_text(data = env_arrows_a75,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_a75, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 4, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))



# Now 4C
FC_axes_mim.mim_a75 <- fourthcorner.rlq(rlq.mim_a75, modeltype = 6,
                                        typetest = "axes",  
                                        p.adjust.method.G = "fdr",
                                        p.adjust.method.D = "fdr")
plot(FC_axes_mim)
print(FC_axes_mim.mim_a75)




#####    Riparian ----

# Read in the trimmer file
mim_r25 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/mim_r25.csv")
str(mim_r25)
mim_r25 <- select(mim_r25, operatn)

# Trim env
env$operatn <- rownames(env)
env_mim_r25 <- left_join(mim_r25, env)
env_mim_r25 <- env_mim_r25 %>% drop_na(ships)
env_mim_r25 <- env_mim_r25[!duplicated(env_mim_r25$operatn),]
rownames(env_mim_r25) <- env_mim_r25$operatn
env_mim_r25 <- select(env_mim_r25, -operatn)

# Trim spe
spe$operatn <- rownames(spe)
spe_mim_r25 <- left_join(mim_r25, spe)
str(spe_mim_r25)
spe_mim_r25 <- spe_mim_r25 %>% drop_na(Coenagrion)
spe_mim_r25 <- spe_mim_r25[!duplicated(spe_mim_r25$operatn),]
rownames(spe_mim_r25) <- spe_mim_r25$operatn
spe_mim_r25 <- select(spe_mim_r25, -operatn)

# Run RLQ
afcL.mim_r25 <- dudi.coa(spe_mim_r25, scannf = F)
afcR.mim_r25 <- dudi.pca(env_mim_r25, row.w = afcL.mim_r25$lw, scale = T, scannf = F)
afcQ.mim_r25 <- dudi.hillsmith(mimm_traits, row.w = afcL.mim_r25$cw, scannf = F)
rlq.mim_r25  <- rlq(afcR.mim_r25, afcL.mim_r25, afcQ.mim_r25, scannf = F)

# Plotting
trt_arrows_r25 <- rlq.mim_r25$c1
env_arrows_r25 <- rlq.mim_r25$l1

trt_arrows_r25$Trait <- rownames(trt_arrows_r25)
trt_arrows_r25[,4:5] <- str_split_fixed(trt_arrows_r25$Trait, "_", 2)
trt_arrows_r25 <- rename(trt_arrows_r25, 
                         Trait_class = V4,
                         Trait_subset = V5)
trt_arrows_r25$Trait_subset <- gsub("_", " ",
                                    as.character(trt_arrows_r25$Trait_subset))
trt_arrows_r25$Trait_subset <- gsub(".", " ", fixed = T,
                                    as.character(trt_arrows_r25$Trait_subset))
trt_arrows_r25$Trait_class <- gsub("Loco", "Locomotion",
                                   as.character(trt_arrows_r25$Trait_class))
trt_arrows_r25$Trait_subset <- str_to_title(as.character(trt_arrows_r25$Trait_subset))

env_arrows_r25$Predictor <- rownames(env_arrows_r25)
env_arrows_r25$Predictor <- str_to_title(as.character(env_arrows_r25$Predictor))
env_arrows_r25$Predictor <- gsub("Canals", "Channelization",
                                 as.character(env_arrows_r25$Predictor))

ggplot() +
  geom_text(data = env_arrows_r25,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_r25, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 4, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))


# Now 4C
FC_axes_mim_r25 <- fourthcorner.rlq(rlq.mim_r25, modeltype = 6,
                                    typetest = "axes",  
                                    p.adjust.method.G = "fdr",
                                    p.adjust.method.D = "fdr")
print(FC_axes_mim_r25)
trt_arrows_r25



# Riparian 75%
mim_r75 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/mim_r75.csv")
str(mim_r75)
mim_r75 <- select(mim_r75, operatn)

# Trim env
env$operatn <- rownames(env)
env_mim_r75 <- left_join(mim_r75, env)
env_mim_r75 <- env_mim_r75 %>% drop_na(ships)
env_mim_r75 <- env_mim_r75[!duplicated(env_mim_r75$operatn),]
rownames(env_mim_r75) <- env_mim_r75$operatn
env_mim_r75 <- select(env_mim_r75, -operatn)

# Trim spe
spe$operatn <- rownames(spe)
spe_mim_r75 <- left_join(mim_r75, spe)
str(spe_mim_r75)
spe_mim_r75 <- spe_mim_r75 %>% drop_na(Coenagrion)
spe_mim_r75 <- spe_mim_r75[!duplicated(spe_mim_r75$operatn),]
rownames(spe_mim_r75) <- spe_mim_r75$operatn
spe_mim_r75 <- select(spe_mim_r75, -operatn)

# Run it
afcL.mim_r75 <- dudi.coa(spe_mim_r75, scannf = F)
afcR.mim_r75 <- dudi.pca(env_mim_r75, row.w = afcL.mim_r75$lw, scale = T, scannf = F)
afcQ.mim_r75 <- dudi.hillsmith(mimm_traits, row.w = afcL.mim_r75$cw, scannf = F)
rlq.mim_r75  <- rlq(afcR.mim_r75, afcL.mim_r75, afcQ.mim_r75, scannf = F)


trt_arrows_r75 <- rlq.mim_r75$c1
env_arrows_r75 <- rlq.mim_r75$l1

str(trt_arrows_r75)
trt_arrows_r75$Trait <- rownames(trt_arrows_r75)
trt_arrows_r75[,4:5] <- str_split_fixed(trt_arrows_r75$Trait, "_", 2)
trt_arrows_r75 <- rename(trt_arrows_r75, 
                         Trait_class = V4,
                         Trait_subset = V5)
trt_arrows_r75$Trait_subset <- gsub("_", " ",
                                    as.character(trt_arrows_r75$Trait_subset))
trt_arrows_r75$Trait_subset <- gsub(".", " ", fixed = T,
                                    as.character(trt_arrows_r75$Trait_subset))
trt_arrows_r75$Trait_class <- gsub("Loco", "Locomotion",
                                   as.character(trt_arrows_r75$Trait_class))
trt_arrows_r75$Trait_subset <- str_to_title(as.character(trt_arrows_r75$Trait_subset))

env_arrows_r75$Predictor <- rownames(env_arrows_r75)
env_arrows_r75$Predictor <- str_to_title(as.character(env_arrows_r75$Predictor))
env_arrows_r75$Predictor <- gsub("Canals", "Channelization",
                                 as.character(env_arrows_r75$Predictor))

ggplot() +
  geom_text(data = env_arrows_r75,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_r75, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 4, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))



# Now 4C
FC_axes_mim.mim_r75 <- fourthcorner.rlq(rlq.mim_r75, modeltype = 6,
                                        typetest = "axes",  
                                        p.adjust.method.G = "fdr",
                                        p.adjust.method.D = "fdr")
plot(FC_axes_mim)
print(FC_axes_mim.mim_r75)




# Combine the env_arrows df's into one df
str(env_arrows)
env_arrows <- rename(env_arrows, 
                     Env_Axis1_Full = RS1,
                     Env_Axis2_Full = RS2)
env_arrows_a25 <- rename(env_arrows_a25, 
                         Env_Axis1_a25 = RS1,
                         Env_Axis2_a25 = RS2)
env_arrows_a75 <- rename(env_arrows_a75, 
                         Env_Axis1_a75 = RS1,
                         Env_Axis2_a75 = RS2)
env_arrows_u25 <- rename(env_arrows_u25, 
                         Env_Axis1_u25 = RS1,
                         Env_Axis2_u25 = RS2)
env_arrows_u75 <- rename(env_arrows_u75, 
                         Env_Axis1_u75 = RS1,
                         Env_Axis2_u75 = RS2)
env_arrows_r25 <- rename(env_arrows_r25, 
                         Env_Axis1_r25 = RS1,
                         Env_Axis2_r25 = RS2)
env_arrows_r75 <- rename(env_arrows_r75, 
                         Env_Axis1_r75 = RS1,
                         Env_Axis2_r75 = RS2)

df_list <- list(env_arrows, env_arrows_a25, env_arrows_a75,env_arrows_u25,
                env_arrows_u75,env_arrows_r25,env_arrows_r75)

env_arrows_combined <- df_list %>% reduce(full_join, by = "Predictor")
env_arrows_combined <- select(env_arrows_combined, Predictor, everything())


# Now traits df
str(trt_arrows)
trt_arrows <- rename(trt_arrows,
                     Trt_Axis1_full = CS1,
                     Trt_Axis2_full = CS2)
trt_arrows_a25 <- rename(trt_arrows_a25,
                         Trt_Axis1_a25 = CS1,
                         Trt_Axis2_a25 = CS2)
trt_arrows_a75 <- rename(trt_arrows_a75,
                         Trt_Axis1_a75 = CS1,
                         Trt_Axis2_a75 = CS2)
trt_arrows_u25 <- rename(trt_arrows_u25,
                         Trt_Axis1_u25 = CS1,
                         Trt_Axis2_u25 = CS2)
trt_arrows_u75 <- rename(trt_arrows_u75,
                         Trt_Axis1_u75 = CS1,
                         Trt_Axis2_u75 = CS2)
trt_arrows_r25 <- rename(trt_arrows_r25,
                         Trt_Axis1_r25 = CS1,
                         Trt_Axis2_r25 = CS2)
trt_arrows_r75 <- rename(trt_arrows_r75,
                         Trt_Axis1_r75 = CS1,
                         Trt_Axis2_r75 = CS2)

trt_arrows <- select(trt_arrows, -Trait_class, -Trait_subset)
trt_arrows_a25 <- select(trt_arrows_a25, -Trait_class, -Trait_subset)
trt_arrows_a75 <- select(trt_arrows_a75, -Trait_class, -Trait_subset)
trt_arrows_u25 <- select(trt_arrows_u25, -Trait_class, -Trait_subset)
trt_arrows_u75 <- select(trt_arrows_u75, -Trait_class, -Trait_subset)
trt_arrows_r25 <- select(trt_arrows_r25, -Trait_class, -Trait_subset)
trt_arrows_r75 <- select(trt_arrows_r75, -Trait_class, -Trait_subset)

trts_list <- list(trt_arrows, trt_arrows_a25, trt_arrows_a75,trt_arrows_u25,
                  trt_arrows_u75,trt_arrows_r25,trt_arrows_r75)

trt_arrows_combined <- trts_list %>% reduce(full_join, by = "Trait")
trt_arrows_combined <- select(trt_arrows_combined, Trait, everything())





























### B. Fish ----

rm(list = ls())

# Read in files
spe         <- read.csv("../fish_spe_rlq.csv", row.names = 1)
env         <- read.csv("../fish_env_rlq.csv", row.names = 1)
fish_traits <- read.csv("../fish_trt_rlq.csv", row.names = 1)
fish_traits <- fish_traits %>% dplyr::mutate_if(is.character, as.factor)

# RLQ
afcL.sword <- dudi.coa(spe, scannf = F)
afcR.sword <- dudi.pca(env, row.w = afcL.sword$lw, scale = T, scannf = F)
afcQ.sword <- dudi.hillsmith(fish_traits, row.w = afcL.sword$cw, scannf = F)
rlq.sword  <- rlq(afcR.sword, afcL.sword, afcQ.sword, scannf = F)
summary(rlq.sword)

s.label(rlq.sword$l1)
s.arrow(rlq.sword$c1, boxes = F, add.plot = T)
s.arrow(rlq.sword$c1, boxes = F)
s.label(rlq.sword$l1, ylim = c(-2,2), add.plot = T)


# Fourth Corner
FC_axes_sword <- fourthcorner.rlq(rlq.sword, modeltype = 6,
                                  typetest = "axes",  
                                  p.adjust.method.G = "fdr",
                                  p.adjust.method.D = "fdr")
print(FC_axes_sword)
beepr::beep()



# Now plotting
# Make a df for trait and env arrows

trt_arrows <- rlq.sword$c1
env_arrows <- rlq.sword$l1

str(trt_arrows)
trt_arrows$Trait <- rownames(trt_arrows)

trt_arrows <- trt_arrows %>%
  extract(Trait, into = c("First", "Second"), "^([^.]+)\\.(.*)")
trt_arrows[26,3] <- "Region"
trt_arrows[26,4] <- "Sensitivity"
trt_arrows[27,3] <- "Region"
trt_arrows[27,4] <- "FRI"
trt_arrows[28,3] <- "Region"
trt_arrows[28,4] <- "S2FRI"

trt_arrows <- rename(trt_arrows, 
                     Trait_class = First,
                     Trait_subset = Second)
trt_arrows$Trait_class <- gsub("Feedi", "Feeding",
                               as.character(trt_arrows$Trait_class))
trt_arrows$Trait_class <- gsub("Repro", "Reproduction",
                               as.character(trt_arrows$Trait_class))
trt_arrows$Trait_class <- gsub("Habit", "Habitat",
                               as.character(trt_arrows$Trait_class))
trt_arrows$Trait_class <- gsub("Migra", "Migration",
                               as.character(trt_arrows$Trait_class))
trt_arrows$Trait_class <- gsub("Rheop", "Rheophily",
                               as.character(trt_arrows$Trait_class))
trt_arrows$Trait_subset <- str_to_title(as.character(trt_arrows$Trait_subset))
trt_arrows[27,4] <- "FRI"
trt_arrows[28,4] <- "S2FRI"

trt_arrows$Trait_class2 <- trt_arrows$Trait_class
trt_arrows$Trait_class2 <- gsub("Habitat", "Habitat / Rheophily",
                                as.character(trt_arrows$Trait_class2))
trt_arrows$Trait_class2 <- gsub("Rheophily", "Habitat / Rheophily",
                                as.character(trt_arrows$Trait_class2))
trt_arrows$Trait_class2 <- gsub("Habitat / Habitat / Rheophily", "Habitat / Rheophily",
                                as.character(trt_arrows$Trait_class2))

env_arrows$Predictor <- rownames(env_arrows)
env_arrows$Predictor <- str_to_title(as.character(env_arrows$Predictor))
env_arrows$Predictor <- gsub("Prop_canal", "Channelization", 
                             as.character(env_arrows$Predictor))
env_arrows$Predictor <- gsub("Sumlocks", "Locks", 
                             as.character(env_arrows$Predictor))
env_arrows$Predictor <- gsub("Sumports", "Ports", 
                             as.character(env_arrows$Predictor))


ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = env_arrows,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows, aes(x = CS1, CS2, label = Trait_subset, 
                                    fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 6, colour = "white") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class2 ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray")) + 
  xlim(c(-2.5,1.5))





####    i. Split by land use ----


#####  Agriculture ----


# 25 %
fish_a25 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/fish_a25.csv")
fish_a25 <- select(fish_a25, operatn)

# Trim env
env$operatn <- rownames(env)
env_fish_a25 <- left_join(fish_a25, env)
env_fish_a25 <- env_fish_a25 %>% drop_na(ships)
env_fish_a25 <- select(env_fish_a25, -operatn)
# Trim spe
spe$operatn <- rownames(spe)
spe_fish_a25 <- left_join(fish_a25, spe)
spe_fish_a25 <- spe_fish_a25 %>% drop_na(Abramis.brama)
rownames(spe_fish_a25) <- spe_fish_a25$operatn
spe_fish_a25 <- select(spe_fish_a25, -operatn)

# Run it
afcL.sword_a25 <- dudi.coa(spe_fish_a25, scannf = F)
afcR.sword_a25 <- dudi.pca(env_fish_a25, row.w = afcL.sword_a25$lw, scale = T, scannf = F)
afcQ.sword_a25 <- dudi.hillsmith(fish_traits, row.w = afcL.sword_a25$cw, scannf = F)
rlq.sword_a25  <- rlq(afcR.sword_a25, afcL.sword_a25, afcQ.sword_a25, scannf = F)

# Fourth Corner
FC_axes_sword_a25 <- fourthcorner.rlq(rlq.sword_a25, modeltype = 6,
                                      typetest = "axes",  
                                      p.adjust.method.G = "fdr",
                                      p.adjust.method.D = "fdr")
print(FC_axes_sword_a25)


# Plot it

# Now plotting
# Make a df for trait and env arrows

trt_arrows_a25 <- rlq.sword_a25$c1
env_arrows_a25 <- rlq.sword_a25$l1

str(trt_arrows_a25)
trt_arrows_a25$Trait <- rownames(trt_arrows_a25)

trt_arrows_a25 <- trt_arrows_a25 %>%
  extract(Trait, into = c("First", "Second"), "^([^.]+)\\.(.*)")
trt_arrows_a25[26,3] <- "Region"
trt_arrows_a25[26,4] <- "Sensitivity"
trt_arrows_a25[27,3] <- "Region"
trt_arrows_a25[27,4] <- "FRI"
trt_arrows_a25[28,3] <- "Region"
trt_arrows_a25[28,4] <- "S2FRI"

trt_arrows_a25 <- rename(trt_arrows_a25, 
                         Trait_class = First,
                         Trait_subset = Second)
trt_arrows_a25$Trait_class <- gsub("Feedi", "Feeding",
                                   as.character(trt_arrows_a25$Trait_class))
trt_arrows_a25$Trait_class <- gsub("Repro", "Reproduction",
                                   as.character(trt_arrows_a25$Trait_class))
trt_arrows_a25$Trait_class <- gsub("Habit", "Habitat",
                                   as.character(trt_arrows_a25$Trait_class))
trt_arrows_a25$Trait_class <- gsub("Migra", "Migration",
                                   as.character(trt_arrows_a25$Trait_class))
trt_arrows_a25$Trait_class <- gsub("Rheop", "Rheophily",
                                   as.character(trt_arrows_a25$Trait_class))
trt_arrows_a25$Trait_subset <- str_to_title(as.character(trt_arrows_a25$Trait_subset))
trt_arrows_a25[27,4] <- "FRI"
trt_arrows_a25[28,4] <- "S2FRI"

trt_arrows_a25$Trait_class2 <- trt_arrows_a25$Trait_class
trt_arrows_a25$Trait_class2 <- gsub("Habitat", "Habitat / Rheophily",
                                    as.character(trt_arrows_a25$Trait_class2))
trt_arrows_a25$Trait_class2 <- gsub("Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_a25$Trait_class2))
trt_arrows_a25$Trait_class2 <- gsub("Habitat / Habitat / Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_a25$Trait_class2))



env_arrows_a25$Predictor <- rownames(env_arrows_a25)
env_arrows_a25$Predictor <- str_to_title(as.character(env_arrows_a25$Predictor))
env_arrows_a25$Predictor <- gsub("Prop_canal", "Channelization", 
                                 as.character(env_arrows_a25$Predictor))
env_arrows_a25$Predictor <- gsub("Sumlocks", "Locks", 
                                 as.character(env_arrows_a25$Predictor))
env_arrows_a25$Predictor <- gsub("Sumports", "Ports", 
                                 as.character(env_arrows_a25$Predictor))





ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = env_arrows_a25,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_a25, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 5, colour = "white") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class2 ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))








# 75 %
fish_a75 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/fish_a75.csv")
fish_a75 <- select(fish_a75, operatn)

# Trim env
env$operatn <- rownames(env)
env_fish_a75 <- left_join(fish_a75, env)
env_fish_a75 <- env_fish_a75 %>% drop_na(ships)
env_fish_a75 <- select(env_fish_a75, -operatn)
# Trim spe
spe$operatn <- rownames(spe)
spe_fish_a75 <- left_join(fish_a75, spe)
spe_fish_a75 <- spe_fish_a75 %>% drop_na(Abramis.brama)
rownames(spe_fish_a75) <- spe_fish_a75$operatn
spe_fish_a75 <- select(spe_fish_a75, -operatn)

# Run it
afcL.sword_a75 <- dudi.coa(spe_fish_a75, scannf = F)
afcR.sword_a75 <- dudi.pca(env_fish_a75, row.w = afcL.sword_a75$lw, scale = T, scannf = F)
afcQ.sword_a75 <- dudi.hillsmith(fish_traits, row.w = afcL.sword_a75$cw, scannf = F)
rlq.sword_a75  <- rlq(afcR.sword_a75, afcL.sword_a75, afcQ.sword_a75, scannf = F)

# Fourth Corner
FC_axes_sword_a75 <- fourthcorner.rlq(rlq.sword_a75, modeltype = 6,
                                      typetest = "axes",  
                                      p.adjust.method.G = "fdr",
                                      p.adjust.method.D = "fdr")
print(FC_axes_sword_a75)


# Plot it

# Now plotting
# Make a df for trait and env arrows

trt_arrows_a75 <- rlq.sword_a75$c1
env_arrows_a75 <- rlq.sword_a75$l1

str(trt_arrows_a75)
trt_arrows_a75$Trait <- rownames(trt_arrows_a75)

trt_arrows_a75 <- trt_arrows_a75 %>%
  extract(Trait, into = c("First", "Second"), "^([^.]+)\\.(.*)")
trt_arrows_a75[26,3] <- "Region"
trt_arrows_a75[26,4] <- "Sensitivity"
trt_arrows_a75[27,3] <- "Region"
trt_arrows_a75[27,4] <- "FRI"
trt_arrows_a75[28,3] <- "Region"
trt_arrows_a75[28,4] <- "S2FRI"

trt_arrows_a75 <- rename(trt_arrows_a75, 
                         Trait_class = First,
                         Trait_subset = Second)
trt_arrows_a75$Trait_class <- gsub("Feedi", "Feeding",
                                   as.character(trt_arrows_a75$Trait_class))
trt_arrows_a75$Trait_class <- gsub("Repro", "Reproduction",
                                   as.character(trt_arrows_a75$Trait_class))
trt_arrows_a75$Trait_class <- gsub("Habit", "Habitat",
                                   as.character(trt_arrows_a75$Trait_class))
trt_arrows_a75$Trait_class <- gsub("Migra", "Migration",
                                   as.character(trt_arrows_a75$Trait_class))
trt_arrows_a75$Trait_class <- gsub("Rheop", "Rheophily",
                                   as.character(trt_arrows_a75$Trait_class))
trt_arrows_a75$Trait_subset <- str_to_title(as.character(trt_arrows_a75$Trait_subset))
trt_arrows_a75[27,4] <- "FRI"
trt_arrows_a75[28,4] <- "S2FRI"

trt_arrows_a75$Trait_class2 <- trt_arrows_a75$Trait_class
trt_arrows_a75$Trait_class2 <- gsub("Habitat", "Habitat / Rheophily",
                                    as.character(trt_arrows_a75$Trait_class2))
trt_arrows_a75$Trait_class2 <- gsub("Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_a75$Trait_class2))
trt_arrows_a75$Trait_class2 <- gsub("Habitat / Habitat / Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_a75$Trait_class2))



env_arrows_a75$Predictor <- rownames(env_arrows_a75)
env_arrows_a75$Predictor <- str_to_title(as.character(env_arrows_a75$Predictor))
env_arrows_a75$Predictor <- gsub("Prop_canal", "Channelization", 
                                 as.character(env_arrows_a75$Predictor))
env_arrows_a75$Predictor <- gsub("Sumlocks", "Locks", 
                                 as.character(env_arrows_a75$Predictor))
env_arrows_a75$Predictor <- gsub("Sumports", "Ports", 
                                 as.character(env_arrows_a75$Predictor))





ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = env_arrows_a75,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_a75, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 5, colour = "white") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class2 ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))





#####  Urbanization ----


# 25 %
fish_u25 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/fish_u25.csv")
fish_u25 <- select(fish_u25, operatn)

# Trim env
env$operatn <- rownames(env)
env_fish_u25 <- left_join(fish_u25, env)
env_fish_u25 <- env_fish_u25 %>% drop_na(ships)
env_fish_u25 <- select(env_fish_u25, -operatn)
# Trim spe
spe$operatn <- rownames(spe)
spe_fish_u25 <- left_join(fish_u25, spe)
spe_fish_u25 <- spe_fish_u25 %>% drop_na(Abramis.brama)
rownames(spe_fish_u25) <- spe_fish_u25$operatn
spe_fish_u25 <- select(spe_fish_u25, -operatn)

# Run it
afcL.sword_u25 <- dudi.coa(spe_fish_u25, scannf = F)
afcR.sword_u25 <- dudi.pca(env_fish_u25, row.w = afcL.sword_u25$lw, scale = T, scannf = F)
afcQ.sword_u25 <- dudi.hillsmith(fish_traits, row.w = afcL.sword_u25$cw, scannf = F)
rlq.sword_u25  <- rlq(afcR.sword_u25, afcL.sword_u25, afcQ.sword_u25, scannf = F)

# Fourth Corner
FC_axes_sword_u25 <- fourthcorner.rlq(rlq.sword_u25, modeltype = 6,
                                      typetest = "axes",  
                                      p.adjust.method.G = "fdr",
                                      p.adjust.method.D = "fdr")
print(FC_axes_sword_u25)


# Plot it

# Now plotting
# Make a df for trait and env arrows

trt_arrows_u25 <- rlq.sword_u25$c1
env_arrows_u25 <- rlq.sword_u25$l1

str(trt_arrows_u25)
trt_arrows_u25$Trait <- rownames(trt_arrows_u25)

trt_arrows_u25 <- trt_arrows_u25 %>%
  extract(Trait, into = c("First", "Second"), "^([^.]+)\\.(.*)")
trt_arrows_u25[26,3] <- "Region"
trt_arrows_u25[26,4] <- "Sensitivity"
trt_arrows_u25[27,3] <- "Region"
trt_arrows_u25[27,4] <- "FRI"
trt_arrows_u25[28,3] <- "Region"
trt_arrows_u25[28,4] <- "S2FRI"

trt_arrows_u25 <- rename(trt_arrows_u25, 
                         Trait_class = First,
                         Trait_subset = Second)
trt_arrows_u25$Trait_class <- gsub("Feedi", "Feeding",
                                   as.character(trt_arrows_u25$Trait_class))
trt_arrows_u25$Trait_class <- gsub("Repro", "Reproduction",
                                   as.character(trt_arrows_u25$Trait_class))
trt_arrows_u25$Trait_class <- gsub("Habit", "Habitat",
                                   as.character(trt_arrows_u25$Trait_class))
trt_arrows_u25$Trait_class <- gsub("Migra", "Migration",
                                   as.character(trt_arrows_u25$Trait_class))
trt_arrows_u25$Trait_class <- gsub("Rheop", "Rheophily",
                                   as.character(trt_arrows_u25$Trait_class))
trt_arrows_u25$Trait_subset <- str_to_title(as.character(trt_arrows_u25$Trait_subset))
trt_arrows_u25[27,4] <- "FRI"
trt_arrows_u25[28,4] <- "S2FRI"

trt_arrows_u25$Trait_class2 <- trt_arrows_u25$Trait_class
trt_arrows_u25$Trait_class2 <- gsub("Habitat", "Habitat / Rheophily",
                                    as.character(trt_arrows_u25$Trait_class2))
trt_arrows_u25$Trait_class2 <- gsub("Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_u25$Trait_class2))
trt_arrows_u25$Trait_class2 <- gsub("Habitat / Habitat / Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_u25$Trait_class2))



env_arrows_u25$Predictor <- rownames(env_arrows_u25)
env_arrows_u25$Predictor <- str_to_title(as.character(env_arrows_u25$Predictor))
env_arrows_u25$Predictor <- gsub("Prop_canal", "Channelization", 
                                 as.character(env_arrows_u25$Predictor))
env_arrows_u25$Predictor <- gsub("Sumlocks", "Locks", 
                                 as.character(env_arrows_u25$Predictor))
env_arrows_u25$Predictor <- gsub("Sumports", "Ports", 
                                 as.character(env_arrows_u25$Predictor))





ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = env_arrows_u25,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_u25, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 5, colour = "white") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class2 ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))








# 75 %
fish_u75 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/fish_u75.csv")
fish_u75 <- select(fish_u75, operatn)

# Trim env
env$operatn <- rownames(env)
env_fish_u75 <- left_join(fish_u75, env)
env_fish_u75 <- env_fish_u75 %>% drop_na(ships)
env_fish_u75 <- select(env_fish_u75, -operatn)
# Trim spe
spe$operatn <- rownames(spe)
spe_fish_u75 <- left_join(fish_u75, spe)
spe_fish_u75 <- spe_fish_u75 %>% drop_na(Abramis.brama)
rownames(spe_fish_u75) <- spe_fish_u75$operatn
spe_fish_u75 <- select(spe_fish_u75, -operatn)

# Run it
afcL.sword_u75 <- dudi.coa(spe_fish_u75, scannf = F)
afcR.sword_u75 <- dudi.pca(env_fish_u75, row.w = afcL.sword_u75$lw, scale = T, scannf = F)
afcQ.sword_u75 <- dudi.hillsmith(fish_traits, row.w = afcL.sword_u75$cw, scannf = F)
rlq.sword_u75  <- rlq(afcR.sword_u75, afcL.sword_u75, afcQ.sword_u75, scannf = F)

# Fourth Corner
FC_axes_sword_u75 <- fourthcorner.rlq(rlq.sword_u75, modeltype = 6,
                                      typetest = "axes",  
                                      p.adjust.method.G = "fdr",
                                      p.adjust.method.D = "fdr")
print(FC_axes_sword_u75)


# Plot it

# Now plotting
# Make a df for trait and env arrows

trt_arrows_u75 <- rlq.sword_u75$c1
env_arrows_u75 <- rlq.sword_u75$l1

str(trt_arrows_u75)
trt_arrows_u75$Trait <- rownames(trt_arrows_u75)

trt_arrows_u75 <- trt_arrows_u75 %>%
  extract(Trait, into = c("First", "Second"), "^([^.]+)\\.(.*)")
trt_arrows_u75[26,3] <- "Region"
trt_arrows_u75[26,4] <- "Sensitivity"
trt_arrows_u75[27,3] <- "Region"
trt_arrows_u75[27,4] <- "FRI"
trt_arrows_u75[28,3] <- "Region"
trt_arrows_u75[28,4] <- "S2FRI"

trt_arrows_u75 <- rename(trt_arrows_u75, 
                         Trait_class = First,
                         Trait_subset = Second)
trt_arrows_u75$Trait_class <- gsub("Feedi", "Feeding",
                                   as.character(trt_arrows_u75$Trait_class))
trt_arrows_u75$Trait_class <- gsub("Repro", "Reproduction",
                                   as.character(trt_arrows_u75$Trait_class))
trt_arrows_u75$Trait_class <- gsub("Habit", "Habitat",
                                   as.character(trt_arrows_u75$Trait_class))
trt_arrows_u75$Trait_class <- gsub("Migra", "Migration",
                                   as.character(trt_arrows_u75$Trait_class))
trt_arrows_u75$Trait_class <- gsub("Rheop", "Rheophily",
                                   as.character(trt_arrows_u75$Trait_class))
trt_arrows_u75$Trait_subset <- str_to_title(as.character(trt_arrows_u75$Trait_subset))
trt_arrows_u75[27,4] <- "FRI"
trt_arrows_u75[28,4] <- "S2FRI"

trt_arrows_u75$Trait_class2 <- trt_arrows_u75$Trait_class
trt_arrows_u75$Trait_class2 <- gsub("Habitat", "Habitat / Rheophily",
                                    as.character(trt_arrows_u75$Trait_class2))
trt_arrows_u75$Trait_class2 <- gsub("Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_u75$Trait_class2))
trt_arrows_u75$Trait_class2 <- gsub("Habitat / Habitat / Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_u75$Trait_class2))



env_arrows_u75$Predictor <- rownames(env_arrows_u75)
env_arrows_u75$Predictor <- str_to_title(as.character(env_arrows_u75$Predictor))
env_arrows_u75$Predictor <- gsub("Prop_canal", "Channelization", 
                                 as.character(env_arrows_u75$Predictor))
env_arrows_u75$Predictor <- gsub("Sumlocks", "Locks", 
                                 as.character(env_arrows_u75$Predictor))
env_arrows_u75$Predictor <- gsub("Sumports", "Ports", 
                                 as.character(env_arrows_u75$Predictor))





ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = env_arrows_u75,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_u75, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 5, colour = "white") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class2 ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))









#####  Riparian ----


# 25 %
fish_r25 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/fish_r25.csv")
fish_r25 <- select(fish_r25, operatn)

# Trim env
env$operatn <- rownames(env)
env_fish_r25 <- left_join(fish_r25, env)
env_fish_r25 <- env_fish_r25 %>% drop_na(ships)
env_fish_r25 <- select(env_fish_r25, -operatn)
# Trim spe
spe$operatn <- rownames(spe)
spe_fish_r25 <- left_join(fish_r25, spe)
spe_fish_r25 <- spe_fish_r25 %>% drop_na(Abramis.brama)
rownames(spe_fish_r25) <- spe_fish_r25$operatn
spe_fish_r25 <- select(spe_fish_r25, -operatn)

# Run it
afcL.sword_r25 <- dudi.coa(spe_fish_r25, scannf = F)
afcR.sword_r25 <- dudi.pca(env_fish_r25, row.w = afcL.sword_r25$lw, scale = T, scannf = F)
afcQ.sword_r25 <- dudi.hillsmith(fish_traits, row.w = afcL.sword_r25$cw, scannf = F)
rlq.sword_r25  <- rlq(afcR.sword_r25, afcL.sword_r25, afcQ.sword_r25, scannf = F)

# Fourth Corner
FC_axes_sword_r25 <- fourthcorner.rlq(rlq.sword_r25, modeltype = 6,
                                      typetest = "axes",  
                                      p.adjust.method.G = "fdr",
                                      p.adjust.method.D = "fdr")
print(FC_axes_sword_r25)


# Plot it

# Now plotting
# Make a df for trait and env arrows

trt_arrows_r25 <- rlq.sword_r25$c1
env_arrows_r25 <- rlq.sword_r25$l1

str(trt_arrows_r25)
trt_arrows_r25$Trait <- rownames(trt_arrows_r25)

trt_arrows_r25 <- trt_arrows_r25 %>%
  extract(Trait, into = c("First", "Second"), "^([^.]+)\\.(.*)")
trt_arrows_r25[26,3] <- "Region"
trt_arrows_r25[26,4] <- "Sensitivity"
trt_arrows_r25[27,3] <- "Region"
trt_arrows_r25[27,4] <- "FRI"
trt_arrows_r25[28,3] <- "Region"
trt_arrows_r25[28,4] <- "S2FRI"

trt_arrows_r25 <- rename(trt_arrows_r25, 
                         Trait_class = First,
                         Trait_subset = Second)
trt_arrows_r25$Trait_class <- gsub("Feedi", "Feeding",
                                   as.character(trt_arrows_r25$Trait_class))
trt_arrows_r25$Trait_class <- gsub("Repro", "Reproduction",
                                   as.character(trt_arrows_r25$Trait_class))
trt_arrows_r25$Trait_class <- gsub("Habit", "Habitat",
                                   as.character(trt_arrows_r25$Trait_class))
trt_arrows_r25$Trait_class <- gsub("Migra", "Migration",
                                   as.character(trt_arrows_r25$Trait_class))
trt_arrows_r25$Trait_class <- gsub("Rheop", "Rheophily",
                                   as.character(trt_arrows_r25$Trait_class))
trt_arrows_r25$Trait_subset <- str_to_title(as.character(trt_arrows_r25$Trait_subset))
trt_arrows_r25[27,4] <- "FRI"
trt_arrows_r25[28,4] <- "S2FRI"

trt_arrows_r25$Trait_class2 <- trt_arrows_r25$Trait_class
trt_arrows_r25$Trait_class2 <- gsub("Habitat", "Habitat / Rheophily",
                                    as.character(trt_arrows_r25$Trait_class2))
trt_arrows_r25$Trait_class2 <- gsub("Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_r25$Trait_class2))
trt_arrows_r25$Trait_class2 <- gsub("Habitat / Habitat / Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_r25$Trait_class2))



env_arrows_r25$Predictor <- rownames(env_arrows_r25)
env_arrows_r25$Predictor <- str_to_title(as.character(env_arrows_r25$Predictor))
env_arrows_r25$Predictor <- gsub("Prop_canal", "Channelization", 
                                 as.character(env_arrows_r25$Predictor))
env_arrows_r25$Predictor <- gsub("Sumlocks", "Locks", 
                                 as.character(env_arrows_r25$Predictor))
env_arrows_r25$Predictor <- gsub("Sumports", "Ports", 
                                 as.character(env_arrows_r25$Predictor))





ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = env_arrows_r25,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_r25, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 5, colour = "white") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class2 ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))








# 75 %
fish_r75 <- read.csv("../Data/PostMeeting_Fixes/RLQ4C/fish_r75.csv")
fish_r75 <- select(fish_r75, operatn)

# Trim env
env$operatn <- rownames(env)
env_fish_r75 <- left_join(fish_r75, env)
env_fish_r75 <- env_fish_r75 %>% drop_na(ships)
env_fish_r75 <- select(env_fish_r75, -operatn)
# Trim spe
spe$operatn <- rownames(spe)
spe_fish_r75 <- left_join(fish_r75, spe)
spe_fish_r75 <- spe_fish_r75 %>% drop_na(Abramis.brama)
rownames(spe_fish_r75) <- spe_fish_r75$operatn
spe_fish_r75 <- select(spe_fish_r75, -operatn)

# Run it
afcL.sword_r75 <- dudi.coa(spe_fish_r75, scannf = F)
afcR.sword_r75 <- dudi.pca(env_fish_r75, row.w = afcL.sword_r75$lw, scale = T, scannf = F)
afcQ.sword_r75 <- dudi.hillsmith(fish_traits, row.w = afcL.sword_r75$cw, scannf = F)
rlq.sword_r75  <- rlq(afcR.sword_r75, afcL.sword_r75, afcQ.sword_r75, scannf = F)

# Fourth Corner
FC_axes_sword_r75 <- fourthcorner.rlq(rlq.sword_r75, modeltype = 6,
                                      typetest = "axes",  
                                      p.adjust.method.G = "fdr",
                                      p.adjust.method.D = "fdr")
print(FC_axes_sword_r75)


# Plot it

# Now plotting
# Make a df for trait and env arrows

trt_arrows_r75 <- rlq.sword_r75$c1
env_arrows_r75 <- rlq.sword_r75$l1

str(trt_arrows_r75)
trt_arrows_r75$Trait <- rownames(trt_arrows_r75)

trt_arrows_r75 <- trt_arrows_r75 %>%
  extract(Trait, into = c("First", "Second"), "^([^.]+)\\.(.*)")
trt_arrows_r75[26,3] <- "Region"
trt_arrows_r75[26,4] <- "Sensitivity"
trt_arrows_r75[27,3] <- "Region"
trt_arrows_r75[27,4] <- "FRI"
trt_arrows_r75[28,3] <- "Region"
trt_arrows_r75[28,4] <- "S2FRI"

trt_arrows_r75 <- rename(trt_arrows_r75, 
                         Trait_class = First,
                         Trait_subset = Second)
trt_arrows_r75$Trait_class <- gsub("Feedi", "Feeding",
                                   as.character(trt_arrows_r75$Trait_class))
trt_arrows_r75$Trait_class <- gsub("Repro", "Reproduction",
                                   as.character(trt_arrows_r75$Trait_class))
trt_arrows_r75$Trait_class <- gsub("Habit", "Habitat",
                                   as.character(trt_arrows_r75$Trait_class))
trt_arrows_r75$Trait_class <- gsub("Migra", "Migration",
                                   as.character(trt_arrows_r75$Trait_class))
trt_arrows_r75$Trait_class <- gsub("Rheop", "Rheophily",
                                   as.character(trt_arrows_r75$Trait_class))
trt_arrows_r75$Trait_subset <- str_to_title(as.character(trt_arrows_r75$Trait_subset))
trt_arrows_r75[27,4] <- "FRI"
trt_arrows_r75[28,4] <- "S2FRI"

trt_arrows_r75$Trait_class2 <- trt_arrows_r75$Trait_class
trt_arrows_r75$Trait_class2 <- gsub("Habitat", "Habitat / Rheophily",
                                    as.character(trt_arrows_r75$Trait_class2))
trt_arrows_r75$Trait_class2 <- gsub("Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_r75$Trait_class2))
trt_arrows_r75$Trait_class2 <- gsub("Habitat / Habitat / Rheophily", "Habitat / Rheophily",
                                    as.character(trt_arrows_r75$Trait_class2))



env_arrows_r75$Predictor <- rownames(env_arrows_r75)
env_arrows_r75$Predictor <- str_to_title(as.character(env_arrows_r75$Predictor))
env_arrows_r75$Predictor <- gsub("Prop_canal", "Channelization", 
                                 as.character(env_arrows_r75$Predictor))
env_arrows_r75$Predictor <- gsub("Sumlocks", "Locks", 
                                 as.character(env_arrows_r75$Predictor))
env_arrows_r75$Predictor <- gsub("Sumports", "Ports", 
                                 as.character(env_arrows_r75$Predictor))





ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = env_arrows_r75,  
            aes(x = RS1, RS2, label = Predictor),
            size = 9, fontface = "bold", colour = "darkolivegreen") +
  geom_label(data = trt_arrows_r75, aes(x = CS1, CS2, label = Trait_subset, 
                                        fill = Trait_class), 
             fontface = "bold", alpha = 0.8, size = 5, colour = "white") +
  scale_colour_discrete(l = 40) +
  facet_wrap(Trait_class2 ~ .) +
  theme_grey() +
  theme(legend.position = "") +
  theme(strip.text = element_text(size = 22, face = "bold", colour = "white")) +
  theme(strip.background = element_rect(fill = "darkslategray"))




# Now combine trait and env arrows into their perspective tables
str(env_arrows)
env_arrows <- rename(env_arrows, 
                     Env_Axis1_Full = RS1,
                     Env_Axis2_Full = RS2)
env_arrows_a25 <- rename(env_arrows_a25, 
                         Env_Axis1_a25 = RS1,
                         Env_Axis2_a25 = RS2)
env_arrows_a75 <- rename(env_arrows_a75, 
                         Env_Axis1_a75 = RS1,
                         Env_Axis2_a75 = RS2)
env_arrows_u25 <- rename(env_arrows_u25, 
                         Env_Axis1_u25 = RS1,
                         Env_Axis2_u25 = RS2)
env_arrows_u75 <- rename(env_arrows_u75, 
                         Env_Axis1_u75 = RS1,
                         Env_Axis2_u75 = RS2)
env_arrows_r25 <- rename(env_arrows_r25, 
                         Env_Axis1_r25 = RS1,
                         Env_Axis2_r25 = RS2)
env_arrows_r75 <- rename(env_arrows_r75, 
                         Env_Axis1_r75 = RS1,
                         Env_Axis2_r75 = RS2)

df_list <- list(env_arrows, env_arrows_a25, env_arrows_a75,env_arrows_u25,
                env_arrows_u75,env_arrows_r25,env_arrows_r75)

env_arrows_combined <- df_list %>% reduce(full_join, by = "Predictor")
env_arrows_combined <- select(env_arrows_combined, Predictor, everything())


# Now traits df
str(trt_arrows)
trt_arrows <- rename(trt_arrows,
                     Trt_Axis1_full = CS1,
                     Trt_Axis2_full = CS2)
trt_arrows_a25 <- rename(trt_arrows_a25,
                         Trt_Axis1_a25 = CS1,
                         Trt_Axis2_a25 = CS2)
trt_arrows_a75 <- rename(trt_arrows_a75,
                         Trt_Axis1_a75 = CS1,
                         Trt_Axis2_a75 = CS2)
trt_arrows_u25 <- rename(trt_arrows_u25,
                         Trt_Axis1_u25 = CS1,
                         Trt_Axis2_u25 = CS2)
trt_arrows_u75 <- rename(trt_arrows_u75,
                         Trt_Axis1_u75 = CS1,
                         Trt_Axis2_u75 = CS2)
trt_arrows_r25 <- rename(trt_arrows_r25,
                         Trt_Axis1_r25 = CS1,
                         Trt_Axis2_r25 = CS2)
trt_arrows_r75 <- rename(trt_arrows_r75,
                         Trt_Axis1_r75 = CS1,
                         Trt_Axis2_r75 = CS2)

trt_arrows$Trait <- rownames(trt_arrows)
trt_arrows_a25$Trait <- rownames(trt_arrows_a25)
trt_arrows_a75$Trait <- rownames(trt_arrows_a75)
trt_arrows_u25$Trait <- rownames(trt_arrows_u25)
trt_arrows_u75$Trait <- rownames(trt_arrows_u75)
trt_arrows_r25$Trait <- rownames(trt_arrows_r25)
trt_arrows_r75$Trait <- rownames(trt_arrows_r75)



trt_arrows <- select(trt_arrows, -Trait_class, -Trait_subset)
trt_arrows_a25 <- select(trt_arrows_a25, -Trait_class, -Trait_subset)
trt_arrows_a75 <- select(trt_arrows_a75, -Trait_class, -Trait_subset)
trt_arrows_u25 <- select(trt_arrows_u25, -Trait_class, -Trait_subset)
trt_arrows_u75 <- select(trt_arrows_u75, -Trait_class, -Trait_subset)
trt_arrows_r25 <- select(trt_arrows_r25, -Trait_class, -Trait_subset)
trt_arrows_r75 <- select(trt_arrows_r75, -Trait_class, -Trait_subset)

trts_list <- list(trt_arrows, trt_arrows_a25, trt_arrows_a75,trt_arrows_u25,
                  trt_arrows_u75,trt_arrows_r25,trt_arrows_r75)

trt_arrows_combined <- trts_list %>% reduce(full_join, by = "Trait")
trt_arrows_combined <- select(trt_arrows_combined, Trait, everything())






















