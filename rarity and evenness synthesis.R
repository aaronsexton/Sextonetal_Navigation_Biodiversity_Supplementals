

# Testing the relationship between rare species and evenness
# Will use the communities used in the RLQ analyses

library(tidyverse)
library(lme4)
library(ggplot2)
library(rphylopic)

# Fish ----
# Read in the assemblages file used in our RLQs
fish    <- read.csv("../fish_rlq_spe.csv", row.names = 1)

# Now lets create a proportions table
fish2 <- fish
fish2 <- fish2/rowSums(fish2)

# Replace 0 with na
fish4 <- fish2
fish4[fish4 == 0] <- NA

# Replace all values above 0.25 with na
fish5 <- fish4
fish5[fish5 >= 0.25] <- NA

# Convert these back to abundances by multiplying by 100
fish6 <- fish5
fish6 <- fish6 * rowSums(fish)


# These are now just the abundances of rare species 
#    (those that made up less than 25% of the community)

# Now sum their abundances
fish6[, 93] <- rowSums(fish6, na.rm = T)
fish6 <- rename(fish6, rare_ab = V93)

# Now their occurances
fish6$rare_occs <- rowSums(!is.na(fish6[1:92]))

# Plot abs and occs
plot(fish6$rare_ab ~ fish6$rare_occs)

# Add in mim main file
fish_pred <- read.csv("../fish_modeldf.csv")

fish_rare <- fish6
fish_rare <- select(fish_rare, rare_ab, rare_occs)
fish_rare$operatn <- rownames(fish_rare)

# Join
fish_rare <- left_join(fish_rare, fish_pred)
fish_rare <- select(fish_rare, rare_ab, rare_occs, Tax_Even) 

# Plot
plot(sqrt(fish_rare$rare_ab) ~ fish_rare$Tax_Even)
plot(fish_rare$rare_occs ~ fish_rare$Tax_Even)

# Model it
rare_ab_glm_fish <- lm(rare_ab ~ Tax_Even, data = fish_rare)
summary(rare_ab_glm_fish)
rare_oc_glm_fish <- lm(rare_occs ~ Tax_Even, data = fish_rare)
summary(rare_oc_glm_fish)


# ggplot it
salmo <- pick_phylopic(name = "Salmo trutta", n = 1)

c <- ggplot(fish_rare, aes(x = Tax_Even, y = rare_occs)) + 
  geom_point() + 
  geom_smooth() +
  theme_bw() + 
  xlab("Taxonomic Evenness") +
  ylab("Occurences of Rare Taxa") +
  theme(axis.title = element_text(size = 20)) +
  add_phylopic(img = salmo, x = 0.88, y = 32, ysize = 4) +
  annotate("text", x = 0.9, y = 28, label = "p < 0.001") +
  annotate("text", x = 0.9, y = 26, label = "R2 = 0.45")
c

d <- ggplot(fish_rare, aes(x = Tax_Even, y = sqrt(rare_ab))) + 
  geom_point() + 
  geom_smooth() +
  theme_bw() + 
  xlab("Taxonomic Evenness") +
  ylab("Abundance of Rare Taxa (sqrt)") +
  theme(axis.title = element_text(size = 20)) +
  add_phylopic(img = salmo, x = 0.88, y = 73, ysize = 10) +
  annotate("text", x = 0.9, y = 65, label = "p < 0.001") +
  annotate("text", x = 0.9, y = 60, label = "R2 = 0.11")
d

ggpubr::ggarrange(a, b, c, d ,
                  ncol = 2, nrow = 2)





# Macroinvertebrates ----
# RLQ Files
mim <- read.csv("../mimm_rlq_spe.csv", row.names = 1)
str(mim)

# Each row is an observation (3,606) and each column is a taxa (319)

# Now lets create a proportions table
mim2 <- mim
mim2 <- mim2/rowSums(mim2)

# Flip it
mim3 <- t(as.data.frame(mim2))

# Replace 0 with na
mim4 <- mim3
mim4[mim4 == 0] <- NA

# Replace all values above 0.25 with na
mim5 <- mim4
mim5[mim5 >= 0.25] <- NA

# Convert these back to abundances by multiplying by 100
mim6 <- mim5
mim6 <- t(as.data.frame(mim6))
mim6 <- mim6 * rowSums(mim)

# These are now just the abundances of rare species 
#    (those that made up less than 25% of the community)

# Now sum their abundances
mim6 <- as.data.frame(mim6)
mim6[, 320] <- rowSums(mim6, na.rm = T)
mim6 <- rename(mim6, rare_ab = V320)

# Now their occurances
mim6$rare_occs <- rowSums(!is.na(mim6[1:319]))

# Plot abs and occs
plot(mim6$rare_ab ~ mim6$rare_occs)

# Remove massive outlier
mim7 <- mim6
mim7 <- filter(mim7, rare_ab != 20083.00)
plot(mim7$rare_ab ~ mim7$rare_occs)

# Add in mim main file
mim_pred <- read.csv("../mim_modeldf.csv")

mim_rare <- mim7
mim_rare <- select(mim_rare, rare_ab, rare_occs)
mim_rare$operatn <- rownames(mim_rare)

# Join
mim_rare <- left_join(mim_rare, mim_pred)
mim_rare <- select(mim_rare, rare_ab, rare_occs, Tax_Even) 

# Plot
plot(log(mim_rare$rare_ab) ~ mim_rare$Tax_Even)
plot(mim_rare$rare_occs ~ mim_rare$Tax_Even)

# Model it
rare_ab_glm <- lm(rare_ab ~ Tax_Even, data = mim_rare)
summary(rare_ab_glm)
rare_oc_glm <- lm(rare_occs ~ Tax_Even, data = mim_rare)
summary(rare_oc_glm)


mim_pic <- pick_phylopic(name = "Anax imperator", n = 2)

a <- ggplot(mim_rare, aes(x = Tax_Even, y = rare_occs)) + 
  geom_point() + 
  geom_smooth() +
  theme_bw() + 
  xlab("Taxonomic Evenness") +
  ylab("Occurences of Rare Taxa") +
  theme(axis.title = element_text(size = 20)) +
  add_phylopic(img = mim_pic, x = 0.925, y = 55, ysize = 10) +
  annotate("text", x = 0.925, y = 48, label = "p < 0.001") +
  annotate("text", x = 0.925, y = 45, label = "R2 = 0.18")
a  


b <- ggplot(mim_rare, aes(x = Tax_Even, y = sqrt(rare_ab))) + 
  geom_point() + 
  geom_smooth() +
  theme_bw() + 
  xlab("Taxonomic Evenness") +
  ylab("Abundance of Rare Taxa (sqrt)") +
  theme(axis.title = element_text(size = 20)) +
  add_phylopic(img = mim_pic, x = 0.925, y = 83, ysize = 15) +
  annotate("text", x = 0.925, y = 72, label = "p < 0.001") +
  annotate("text", x = 0.925, y = 68, label = "R2 = 0.09")
b

ggpubr::ggarrange(a, b, 
                  ncol = 2)








