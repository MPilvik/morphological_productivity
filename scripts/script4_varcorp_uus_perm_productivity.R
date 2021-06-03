# Calculate means and confidence intervals for the permuted productivity values  
# by bias-corrected bootstrap (1000 bootstrap replicates)
# and create plots from the means and confidence intervals

# Realized productivity (suffix facets, 5 registers)
# Potential productivity (suffix facets, 5 register)
# Expanding productivity (suffix facets, 5 registers)
# Global productivity
# Types (y) vs tokens (x) (suffix x registers)

# Load the necessary packages
library(rcompanion)
library(dplyr)
library(ggplot2)

# Set working directory
setwd(dir.choose()) # data

# Read in the data
permdf <- read.csv2("varcorp_uus_100_permutations.csv", fileEncoding = "UTF-8")



#------------------------------#
#---Realized productivity---####
#------------------------------#
set.seed(1)
permdf %>% # for all subcorpora except the maximal subcorpus, find bootstrap means
  filter(subcorp <= 426000) %>% 
  groupwiseMean(realized ~ subcorp + register + suffix, data = ., R = 1000,
                boot = TRUE, bca = TRUE, traditional = FALSE) %>% 
  select(-5) -> rp_bootmean

permdf %>% # for the maximal subcorpus, find the regular mean
  filter(subcorp > 426000) %>%  
  groupwiseMean(realized ~ subcorp + register + suffix, data = .) -> rp_mean

names(rp_bootmean)[c(5, 7:8)] <- c("Mean", "Lower", "Upper") # change column names of bootstrap means
names(rp_mean)[7:8] <- c("Lower", "Upper") # change column names of the regular mean
rbind(rp_bootmean, rp_mean) -> rp_allmeans # bind the bootsrap means and the regular mean together
rp_allmeans$register <- factor(rp_allmeans$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor
rp_allmeans$suffix <- factor(rp_allmeans$suffix, levels = c("-mine", "-us", "-ja")) # make suffix a factor


# Create a graph
png(filename = "../figures/varcorp_uus_perm_realized_prod.png", width = 9, height = 4, units = "in", res = 300)
ggplot(data = rp_allmeans, # data from all means
       aes(x = subcorp, y = Mean, linetype = register)) + # subcorpus size on the x-axis, mean realized productivity on the y-axis, linetype marks the register
  geom_ribbon(aes(ymin = Lower, ymax = Upper), # draw 95% confidence intervals
              color = "transparent", fill = "grey50", # CI border lines transparent, fill in medium grey
              alpha = 0.3, show.legend = FALSE) + # transparency to 0.3, don't show a legend
  geom_point(aes(shape = register)) + # point shape marks the register
  geom_line(aes(group = register), alpha = 0.7) + # lines connecting the points
  facet_wrap("suffix", scales = "free_y") + # each suffix on a different facet
  theme_bw() + # black-and-white theme
  scale_x_continuous(breaks = seq(0, 426000, 106500)) + # subcorpus sizes increase by 106,500 tokens on the x-axis
  labs(x = "Tokens in the subcorpora", # title of the x-axis
       y = "Realized productivity", # title of the y-axis
       linetype = "Register", # name of the linetype legend
       shape = "Register") + # name of the point shape legend
  theme(text = element_text(size = 12)) # text size to 12
dev.off()



#-------------------------------#
#---Potential productivity---####
#-------------------------------#
set.seed(1)
permdf %>%  # for all subcorpora except the maximal subcorpus, find bootstrap means
  filter(subcorp <= 426000) %>% 
  groupwiseMean(potential ~ subcorp + register + suffix, data = ., R = 1000,
                boot = TRUE, bca = TRUE, traditional = FALSE) %>% 
  select(-5) -> pp_bootmean

permdf %>%  # for the maximal subcorpus, find the regular mean
  filter(subcorp > 426000) %>%  
  groupwiseMean(potential ~ subcorp + register + suffix, data = .) -> pp_mean

names(pp_bootmean)[c(5, 7:8)] <- c("Mean", "Lower", "Upper") # change column names of bootstrap means
names(pp_mean)[7:8] <- c("Lower", "Upper") # change column names of the regular mean
rbind(pp_bootmean, pp_mean) -> pp_allmeans # bind the bootsrap means and the regular mean together
pp_allmeans$register <- factor(pp_allmeans$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor
pp_allmeans$suffix <- factor(pp_allmeans$suffix, levels = c("-mine", "-us", "-ja")) # make suffix a factor


# Create a graph
png(filename = "../figures/varcorp_uus_perm_potential_prod.png", width = 9, height = 4, units = "in", res = 300)
ggplot(data = pp_allmeans, # data from all means
       aes(x = subcorp, y = Mean, linetype = register)) + # subcorpus size on the x-axis, mean realized productivity on the y-axis, linetype marks the register
  geom_ribbon(aes(ymin = Lower, ymax = Upper), # draw 95% confidence intervals
              color = "transparent", fill = "grey50", # CI border lines transparent, fill in medium grey
              alpha = 0.3, show.legend = FALSE) + # transparency to 0.3, don't show a legend
  geom_point(aes(shape = register)) + # point shape marks the register
  geom_line(aes(group = register), alpha = 0.7) + # lines connecting the points
  facet_wrap("suffix", scales = "free_y") + # each suffix on a different facet
  theme_bw() + # black-and-white theme
  scale_x_continuous(breaks = seq(0, 426000, 106500)) + # subcorpus sizes increase by 106,500 tokens on the x-axis
  labs(x = "Tokens in the subcorpora", # title of the x-axis
       y = "Potential productivity", # title of the y-axis
       linetype = "Register", # name of the linetype legend
       shape = "Register") + # name of the point shape legend
  theme(text = element_text(size = 12)) # text size to 12
dev.off()



#-------------------------------#
#---Expanding productivity---####
#-------------------------------#
set.seed(1)
permdf %>% # for all subcorpora except the maximal subcorpus, find bootstrap means
  filter(subcorp <= 426000) %>% 
  groupwiseMean(expanding ~ subcorp + register + suffix, data = ., R = 1000,
                boot = TRUE, bca = TRUE, traditional = FALSE) %>% 
  select(-5) -> ep_bootmean

permdf %>% # for the maximal subcorpus, find the regular mean
  filter(subcorp > 426000) %>%  
  groupwiseMean(expanding ~ subcorp + register + suffix, data = .) -> ep_mean

names(ep_bootmean)[c(5, 7:8)] <- c("Mean", "Lower", "Upper") # change column names of bootstrap means
names(ep_mean)[7:8] <- c("Lower", "Upper") # change column names of the regular mean
rbind(ep_bootmean, ep_mean) -> ep_allmeans # bind the bootsrap means and the regular mean together
ep_allmeans$register <- factor(ep_allmeans$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor
ep_allmeans$suffix <- factor(ep_allmeans$suffix, levels = c("-mine", "-us", "-ja")) # make suffix a factor


# Create a graph
png(filename = "../figures/varcorp_uus_perm_expanding_prod.png", width = 9, height = 4, units = "in", res = 300)
ggplot(data = ep_allmeans, # data from all means
       aes(x = subcorp, y = Mean, linetype = register)) + # subcorpus size on the x-axis, mean realized productivity on the y-axis, linetype marks the register
  geom_ribbon(aes(ymin = Lower, ymax = Upper), # draw 95% confidence intervals
              color = "transparent", fill = "grey50", # CI border lines transparent, fill in medium grey
              alpha = 0.3, show.legend = FALSE) + # transparency to 0.3, don't show a legend
  geom_point(aes(shape = register)) + # point shape marks the register
  geom_line(aes(group = register), alpha = 0.7) + # lines connecting the points
  facet_wrap("suffix", scales = "free_y") + # each suffix on a different facet
  theme_bw() +  # black-and-white theme
  scale_x_continuous(breaks = seq(0, 426000, 106500)) + # subcorpus sizes increase by 106,500 tokens on the x-axis
  labs(x = "Tokens in the subcorpora", # title of the x-axis
       y = "Expanding productivity", # title of the y-axis
       linetype = "Register", # name of the linetype legend
       shape = "Register") + # name of the point shape legend
  theme(text = element_text(size = 12)) # text size to 12
dev.off()



#----------------------------#
#---Global productivity---####
#----------------------------#
# Create a graph
png(filename = "../figures/varcorp_uus_perm_global_prod.png", width = 9, height = 3.5, units = "in", res = 300)
merge(rp_allmeans %>%  # merge realized and potential productivity mean data frames for the 426,000 token subcorpus
        filter(subcorp == 426000) %>% 
        rename(realized = "Mean") %>% 
        select(1:5),
      pp_allmeans %>% 
        filter(subcorp == 426000) %>% 
        rename(potential = "Mean") %>% 
        select(1:5)) %>% 
  ggplot(aes(x = potential, y = realized, shape = register)) + # pot. prod. on the x-axis, real. prod. on the y-axis, point shape marks the register
  geom_point(show.legend = FALSE) + # points, don't show the point legend
  geom_text(aes(label = register), hjust = -0.3, size = 3) + # register labels
  facet_wrap("suffix", scales = "free") + # each suffix on a different facet
  theme_bw() + # black-and-white theme
  labs(x = "Potential productivity (P)", # title of the x-axis
       y = "Number of types (V)") + # title of the y-axis
  expand_limits(x = 0) + # start the x-axis at 0
  scale_x_continuous(expand = c(0.2, 0)) + # expand the x-axis
  theme(text = element_text(size = 12)) # text size to 12
dev.off()



#-------------------------------------#
#---Types vs. tokens vs. hapaxes---####
#-------------------------------------#
# Tokens
set.seed(1)
permdf %>% 
  filter(subcorp <= 426000) %>% 
  groupwiseMean(tokens ~ subcorp + register + suffix, data = ., R = 1000,
                boot = TRUE, bca = TRUE, traditional = FALSE) %>% 
  select(-5) -> tok_bootmean

permdf %>% 
  filter(subcorp > 426000) %>%  
  groupwiseMean(tokens ~ subcorp + register + suffix, data = .) -> tok_mean

names(tok_bootmean)[c(5, 7:8)] <- c("Mean", "Lower", "Upper")
names(tok_mean)[7:8] <- c("Lower", "Upper")
rbind(tok_bootmean, tok_mean) -> tok_allmeans
tok_allmeans$register <- factor(tok_allmeans$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA"))
tok_allmeans$suffix <- factor(tok_allmeans$suffix, levels = c("-mine", "-us", "-ja"))


# Types
set.seed(1)
permdf %>% 
  filter(subcorp <= 426000) %>% 
  groupwiseMean(types ~ subcorp + register + suffix, data = ., R = 1000,
                boot = TRUE, bca = TRUE, traditional = FALSE) %>% 
  select(-5) -> typ_bootmean

permdf %>% 
  filter(subcorp > 426000) %>%  
  groupwiseMean(types ~ subcorp + register + suffix, data = .) -> typ_mean

names(typ_bootmean)[c(5, 7:8)] <- c("Mean", "Lower", "Upper")
names(typ_mean)[7:8] <- c("Lower", "Upper")
rbind(typ_bootmean, typ_mean) -> typ_allmeans
typ_allmeans$register <- factor(typ_allmeans$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA"))
typ_allmeans$suffix <- factor(typ_allmeans$suffix, levels = c("-mine", "-us", "-ja"))


# Hapaxes
set.seed(1)
permdf %>% 
  filter(subcorp <= 426000) %>% 
  groupwiseMean(hapaxes ~ subcorp + register + suffix, data = ., R = 1000,
                boot = TRUE, bca = TRUE, traditional = FALSE) %>% 
  select(-5) -> hap_bootmean
permdf %>%
  filter(subcorp > 426000) %>%  
  groupwiseMean(hapaxes ~ subcorp + register + suffix, data = .) -> hap_mean

names(hap_bootmean)[c(5, 7:8)] <- c("Mean", "Lower", "Upper")
names(hap_mean)[7:8] <- c("Lower", "Upper")
rbind(hap_bootmean, hap_mean) -> hap_allmeans
hap_allmeans$register <- factor(hap_allmeans$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA"))
hap_allmeans$suffix <- factor(hap_allmeans$suffix, levels = c("-mine", "-us", "-ja"))


# Create a graph
png(filename = "../figures/varcorp_uus_perm_types_tokens_hapaxes.png", width = 9, height = 6.5, units = "in", res = 300)
merge(tok_allmeans %>% 
        rename(tokens = "Mean") %>% 
        select(1:5),
      typ_allmeans %>% 
        rename(types = "Mean") %>% 
        select(1:5)) %>%
  merge(.,
        hap_allmeans %>%
          rename(hapaxes = "Mean") %>%
          select(1:5)) %>%
  ggplot(aes(x = tokens, y = types)) +
  facet_grid(c("register", "suffix")) +
  geom_line(aes(group = register, linetype = "types")) +
  geom_line(aes(y = hapaxes, group = register, linetype = "hapaxes")) +
  theme_bw() +
  labs(x = "Tokens (Nc)", y = "Types (V)") +
  theme(text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_label(data = . %>% filter(subcorp == 426000), aes(x = 13000, y = 700, label = paste("TTR: ", round(types/tokens,2), sep = "")))
dev.off()



save(list = c("ep_allmeans", "pp_allmeans", "rp_allmeans", "tok_allmeans", "typ_allmeans", "hap_allmeans"), file = "varcorp_uus_perm_prod_means.RData")
