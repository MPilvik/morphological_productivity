# Run GAM-models on averaged values from 100 sample permutations
# and interpolate potential productivity (P) values
# for a fixed token count

# Load the necessary packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Set working directory
setwd(choose.dir()) # data


# Load the permutation data
permdf <- read.csv2("varcorp_uus_100_permutations.csv", fileEncoding = "UTF-8")
load("varcorp_uus_perm_prod_means.RData")


# Find the token counts (Ncf) at which to interpolate the P values
permdf %>% 
  group_by(register, suffix) %>% # group the permutation data by register and suffix
  summarise(n = max(tokens)) %>% # find the maximum number of tokens (n) for each suffix in each register
  group_by(register) %>% # group the data again by register
  filter(n != max(n)) -> ints # leave out the maximum number of suffix tokens
# SCI: 2346 & 13987
# NEWS: 4135 & 7417
# FICT: 2570 & 1472
# SP: 947 & 1894
# DIA: 642 & 717


# Plot Nc (x) vs P (y) plot with fixed Ncf lines
merge( # merge potential productivity means and token means data frames
  pp_allmeans %>% 
    rename(potential = "Mean", 
           pot.Lower = "Lower", 
           pot.Upper = "Upper"),
  tok_allmeans %>% 
    rename(tokens = "Mean", 
           tok.Lower = "Lower", 
           tok.Upper = "Upper")) -> pottok
pottok$register <- factor(pottok$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor
pottok$suffix <- factor(pottok$suffix, levels = c("-mine", "-us", "-ja")) # make suffix a factor
ints$register <- factor(ints$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor


# Create a graph
png(filename = "../figures/varcorp_uus_perm_PbyNc_Ncf.png", width = 9, height = 3, units = "in", res = 300)
ggplot(data = pottok, # data from the "pottok" data frame
       aes(x = tokens, y = potential)) + # number of suffix tokens on the x-axis, potential productivity on the y-axis
  facet_wrap("register", nrow = 1, scales = "free") + # each register on a different facet
  geom_ribbon(aes(ymin = pot.Lower, ymax = pot.Upper, group = suffix), # 95% confidence intervals
              color = "transparent", fill = "grey50", 
              alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(group = suffix, linetype = suffix), alpha = 0.6) + # trend lines, linetype marks the suffix
  geom_point(data = pottok %>% # sampling endpoints for the suffixes
               group_by(register) %>% 
               filter(subcorp == max(subcorp)), 
             aes(x = tokens, y = potential, shape = suffix), 
             size = 3, alpha = 0.8) +
  scale_shape_manual(values = c(22,21,24)) + # choose different point shapes
  scale_linetype_manual(values = c("twodash", "longdash", "solid")) + # choose different line types
  geom_vline(data = ints, aes(xintercept = n), # draw vertical dashed lines
             linetype = "dashed", alpha = 0.5) +
  theme_bw() + # black-and-white theme
  theme(text = element_text(size = 12), # text size 12
        axis.text.x = element_text(angle = 45, hjust = 1)) + # x-axis text at a 45-degree angle
  labs(x = "Tokens (Nc)", # x-axis title
       y = "Potential productivity (P)", # y-axis title 
       shape = "Suffix", # point shape legend name
       linetype = "Suffix") # line type legend name
dev.off()



#-------------------#
#---GAM-MODELS---####
#-------------------#

# Make the generalized additive models (Wood 2017) and extract the values to the data frame "gamdf"
# Then predict the P-s at fixed token counts and save these to the data frame "preds"
# Each suffix in each register has 21*100 data points

# Empty data frame for GAM values
gamdf <- data.frame(register = character(), suffix = character(),
                    intercept = numeric(), intercept_p = numeric(),
                    edf = numeric(), edf_p = numeric(), R2 = numeric())

# Empty data frame for interpolated values of P
preds <- data.frame(register = character(), suffix = character(),
                    Nc = numeric(), P = numeric())

for(i in unique(permdf$register)){ # in each register
  for(j in unique(permdf$suffix)){ # for each suffix
    
    # Create the GAM models and extract their values
    tmp <- permdf %>% 
      filter(register == i, suffix == j) # temporary data frame for suffix i in register j
    mod <- paste0(i, gsub("-", "", j), ".gam") # create the model name
    
    assign(mod, gam(potential ~ s(tokens), data = tmp)) # make the model
    tmpmod <- get(mod)
    
    intercept <- summary(tmpmod)[[22]][1] # intercept
    pintercept <- summary(tmpmod)[[22]][4] # intercept p-value
    edf <- summary(tmpmod)[[24]][1] # edf
    pedf <- summary(tmpmod)[[24]][4] # edf p-value
    R2 <- summary(tmpmod)$r.sq # R2
    
    # add the extracted values to the data frame "gamdf"
    gamdf <- rbind(gamdf, 
                   data.frame(register = i, suffix = j, 
                              intercept = intercept, intercept_p = pintercept, 
                              edf = edf, edf_p = pedf, 
                              R2 = R2))
    
    
    # Interpolate the P-s at 2 fixed token counts (Ncf)
    ncf1 <- ints %>% filter(register == i) %>% .$n %>% min()
    ncf2 <- ints %>% filter(register == i) %>% .$n %>% max()
    
    # Predict P at the lower Ncf (ncf1) for all suffixes
    p1 <- predict(tmpmod, newdata = data.frame(tokens = ncf1))
    preds <- rbind(preds, 
                   data.frame(register = i, suffix = j, 
                              Nc = ncf1, P = p1))
   
    # Predict P at the higher Ncf (ncf2) only for the more frequent suffixes
    # For the least frequent suffix, assign a value of NA
    if(ints %>% filter(register == i & n == ncf1) %>% .$suffix != j){
      p2 <- predict(tmpmod, newdata = data.frame(tokens = ncf2))
    }
    else{
      p2 <- NA
    }
    preds <- rbind(preds, 
                   data.frame(register = i, suffix = j, 
                              Nc = ncf2, P = p2))
  }
}


preds$register <- factor(preds$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor
preds$suffix <- factor(preds$suffix, levels = c("-mine", "-us", "-ja")) # make suffix a factor



#----------------------------------------------#
#---COMPARE THE INTERPOLATED P MAGNITUDES---####
#----------------------------------------------#


preds %>% 
  tidyr::pivot_wider(data = ., # Change long format to wide format
                     names_from = suffix, # the values in the column "suffix" will become individual column names
                     values_from = P) %>% # whose values will be taken from the column "P"
  mutate(mineus = `-mine`/`-us`, # to the wide data frame, add the column "mineus" by dividing the predicted P for -mine with the predicted P for -us
         mineja = `-mine`/`-ja`, # add the column "mineja" by dividing the predicted P for -mine with the predicted P for -ja
         jaus = `-ja`/`-us`) %>% # and add the column "jaus" by dividing the predicted P for -ja with the predicted P for -us
  tidyr::pivot_longer(data = ., # change wide format back to long format
                      cols = mineus:jaus, # column names mineus, mineja, and jaus will be the names in one column
                      names_to = "ratio") -> ratios # the values in columns mineus, mineja, and jaus will be the values in column "ratio"

ratios$ratio <- factor(ratios$ratio, levels = c("mineus", "mineja", "jaus")) # make ratio a factor

# Create a graph
png(filename = "varcorp_uus_perm_Pratios.png", width = 9, height = 3, units = "in", res = 300)
ggplot(data = ratios, aes(x = Nc, y = value, shape = ratio)) +
  geom_line(aes(group = ratio), alpha = 0.6) +
  geom_point(size = 3, alpha = 0.5) +
  facet_wrap("register", nrow = 1, scales = "free_x") +
  theme_bw() +
  labs(x = "Token count (Nc)", y = "Ratio (P_suff1/P_suff2)") +
  scale_shape_discrete(name = "Ratio", labels = c("-mine/-us", "-mine/-ja", "-ja/-us")) +
  theme(text = element_text(size = 12))
dev.off()



#--------------------------------------------------------------#
#---COMPARE CORRELATIONS BETWEEN P*, P AND INTERPOLATED P---####
#--------------------------------------------------------------#

# Use ep_allmeans, pp_allmeans, preds
# Compare the predicted potential productivity P
# with the means of the actual values of P in the data
# and with the means of the actual values on expanding productivity P* in the data

merge(preds %>% # choose only those rows from "preds
        group_by(register) %>% # where Nc equals the minimum value
        filter(Nc == min(Nc)), # of Nc in each register
      merge(ep_allmeans %>% # merge those rows with the merged data frame of ep_allmeans and pp_allmeans
              rename(expanding = "Mean",  # where the columns have
                     exp.Lower = "Lower", # been renamed
                     exp.Upper = "Upper"), 
            pp_allmeans %>% 
              rename(potential = "Mean", 
                     pot.Lower = "Lower", 
                     pot.Upper = "Upper")) %>% 
        filter(subcorp > 426000), # and only the maximal subcorpus has been chosen
      by = c("register", "suffix")) -> compdf

# Create a data frame for expanding the ggplot plotting region
blank_data <- data.frame(register = c(rep("SCI",2), rep("NEWS",2), rep("FICT",2), rep("SP",2), rep("DIA",2)),
                         y = c(0, 0.033, 0, 0.035, 0, 0.05, 0, 0.04, 0, 0.035),
                         x = c(0, 0.1, 0, 0.16, 0, 0.35, 0, 0.35, 0, 0.25))
blank_data$register <- factor(blank_data$register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA"))

# Create a graph
png("varcorp_uus_perm_expvspot.png", width = 9, height = 4, units = "in", res = 300)
ggplot(data = compdf, # data from compdf
       aes(x = P, y = expanding)) + # P-value on the x-axis
  facet_wrap("register", nrow = 1) + # each register on a separate facet
  # Predicted P
  geom_line(data = compdf %>% filter(register != "SP"), # draw predicted P lines only for the registers which are not SP
            aes(group = 1, linetype = "interpolated at equal number of tokens")) +
  geom_segment(data = compdf %>% # draw separate predicted P line segments between -ja and -us for SP
                 filter(register == "SP"),
               aes(x = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-ja") %>% .$P,
                   xend = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-us") %>% .$P,
                   y = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-ja") %>% .$expanding,
                   yend = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-us") %>% .$expanding,
                   linetype = "interpolated at equal number of tokens")) +
  geom_segment(data = compdf %>% # draw separate predicted P line segments between -us and -mine for SP
                 filter(register == "SP"),
               aes(x = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-us") %>% .$P,
                   xend = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-mine") %>% .$P,
                   y = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-us") %>% .$expanding,
                   yend = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-mine") %>%.$expanding,
                   linetype = "interpolated at equal number of tokens")) +
  # Actual P
  geom_line(data = compdf %>% 
              filter(!register %in% c("SCI", "SP", "DIA")), # draw actual P lines only for the registers which are not SCI, SP or DIA
            aes(x = potential, y = expanding, group = 1, linetype = "calculated at total number of tokens"),
            color = "grey15") +
  geom_segment(data = compdf %>%  # draw separate actual P line segments between -us and -ja for SCI
                 filter(register == "SCI"),
               aes(x = compdf %>% 
                     filter(register == "SCI", 
                            suffix == "-us") %>% .$potential,
                   xend = compdf %>% 
                     filter(register == "SCI", 
                            suffix == "-ja") %>% .$potential,
                   y = compdf %>% 
                     filter(register == "SCI", 
                            suffix == "-us") %>% .$expanding,
                   yend = compdf %>% 
                     filter(register == "SCI", 
                            suffix == "-ja") %>% .$expanding,
                   linetype = "calculated at total number of tokens"),
               color = "grey15") +
  geom_segment(data = compdf %>% # draw separate actual P line segments between -ja and -mine for SCI
                 filter(register == "SCI"),
               aes(x = compdf %>% 
                     filter(register == "SCI", 
                            suffix == "-ja") %>% .$potential,
                   xend = compdf %>% 
                     filter(register == "SCI", 
                            suffix == "-mine") %>% .$potential,
                   y = compdf %>% 
                     filter(register == "SCI", 
                            suffix == "-ja") %>% .$expanding,
                   yend = compdf %>% 
                     filter(register == "SCI", 
                            suffix == "-mine") %>% .$expanding,
                   linetype = "calculated at total number of tokens"),
               color = "grey15") +
  geom_segment(data = compdf %>% # draw separate actual P line segments between -us and -ja for DIA
                 filter(register == "DIA"),
               aes(x = compdf %>% 
                     filter(register == "DIA", 
                            suffix == "-us") %>% .$potential,
                   xend = compdf %>% 
                     filter(register == "DIA", 
                            suffix == "-ja") %>% .$potential,
                   y = compdf %>% 
                     filter(register == "DIA", 
                            suffix == "-us") %>% .$expanding,
                   yend = compdf %>% 
                     filter(register == "DIA", 
                            suffix == "-ja") %>% .$expanding,
                   linetype = "calculated at total number of tokens"),
               color = "grey15") +
  geom_segment(data = compdf %>% # draw separate actual P line segments between -ja and -mine for DIA
                 filter(register == "DIA"),
               aes(x = compdf %>% 
                     filter(register == "DIA", 
                            suffix == "-ja") %>% .$potential,
                   xend = compdf %>% 
                     filter(register == "DIA", 
                            suffix == "-mine") %>% .$potential,
                   y = compdf %>% 
                     filter(register == "DIA", 
                            suffix == "-ja") %>% .$expanding,
                   yend = compdf %>% 
                     filter(register == "DIA", 
                            suffix == "-mine") %>% .$expanding,
                   linetype = "calculated at total number of tokens"),
               color = "grey15") +
  geom_segment(data = compdf %>% # draw separate actual P line segments between -ja and -us for SP
                 filter(register == "SP"),
               aes(x = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-ja") %>% .$potential,
                   xend = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-us") %>% .$potential,
                   y = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-ja") %>% .$expanding,
                   yend = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-us") %>% .$expanding,
                   linetype = "calculated at total number of tokens"),
               color = "grey15") +
  geom_segment(data = compdf %>% # draw separate actual P line segments between -us and -mine for SP
                 filter(register == "SP"),
               aes(x = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-us") %>% .$potential,
                   xend = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-mine") %>% .$potential,
                   y = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-us") %>% .$expanding,
                   yend = compdf %>% 
                     filter(register == "SP", 
                            suffix == "-mine") %>% .$expanding,
                   linetype = "calculated at total number of tokens"),
               color = "grey15") +
  geom_point(aes(shape = suffix), # draw points on the predicted P line (point shape marks the suffix)
             show.legend = FALSE) +
  geom_text(aes(label = suffix), # add suffix labels for the predicted P line
            hjust = -0.3, size = 4) +
  scale_shape_manual(values = c(22, 21, 24)) + # change point shapes
  scale_linetype_manual(values = c("dashed", "solid")) + # change line types
  geom_blank(data = blank_data, # add blank data for expanding the plotting region
             aes(x = x, y = y)) +
  expand_limits(x = 0) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(size = 12),
        legend.direction = "vertical", 
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)) +
  labs(x = "Potential productivity (P)",
       y = "Expanding productivity (P*)\n calculated at total number of tokens")
dev.off()



#-------------------------------------#
#---COMPARE DIFFERENT APPROACHES---####
#-------------------------------------#

# Original (Baayen 1992, 1993)
pp_allmeans %>% # take the means for all actual potential productivity values in permutations
  filter(subcorp > 426000) %>% # filter only the value for the largest subcorpus (at the endpoint of the sampling process)
  ggplot(aes(y = Mean, x = suffix)) + # plot the mean value on the y-axis and the suffix category on the x-axis
  geom_point(aes(shape = register)) + # show the values with points (shape marks the register)
  geom_line(aes(group = register, linetype = register)) + # join the points with lines (linetype marks the register)
  theme_bw() + # black-and-white theme
  theme(text = element_text(size = 12), # text size 12
        plot.title = element_text(hjust = 0.5), # plot title aligned to the center
        legend.position = "bottom") + # place legend at the bottom of the graph
  labs(x = "Suffix", 
       y = "Potential productivity (P)\ncalculated at total number of tokens", 
       title = "Original approach\n(Baayen 1992, 1993, etc.)", 
       shape = "Register", 
       linetype = "Register") +
  ylim(c(0, 0.27)) + # set y-axis limits
  guides(shape = guide_legend(nrow = 2), # show point shape and linetype legend values in two rows
         linetype = guide_legend(nrow = 2)) -> p1 # save the plot to an object called "p1"


# Gams (~ Gaeta & Ricca 2003, 2006)
preds %>% # take the predicted potential productivity values 
  .[complete.cases(.),] %>% # exclude NAs
  group_by(register, suffix) %>% # group data by register and suffix
  filter(Nc == min(Nc)) %>% # keep only the P values for the lowest common number of suffix tokens (for each suffix in each register)
  ggplot(aes(y = P, x = suffix)) + # plot the predicted P value on the y-axis and the suffix category on the x-axis
  geom_point(aes(shape = register)) + # show the values with points (shape marks the register)
  geom_line(aes(group = register, linetype = register)) + # join the points with lines (linetype marks the register)
  theme_bw() + # black-and-white theme
  theme(text = element_text(size = 12), # text size 12
        plot.title = element_text(hjust = 0.5), # plot title aligned to the center
        legend.position = "bottom") + # place legend at the bottom of the graph
  labs(x = "Suffix", 
       y = "Potential productivity (P)\ncalculated at equal number of tokens", 
       title = "Variable-corpus approach\n(Gaeta & Ricca 2006)", 
       shape = "Register", 
       linetype = "Register") +
  ylim(c(0, 0.27))+ # set y-axis limits
  guides(shape = guide_legend(nrow = 2), # show point shape and linetype legend values in two rows
         linetype = guide_legend(nrow = 2)) -> p2 # save the plot to an object called "p2"


# Average (~ Plag et al. 1999)
pp_allmeans %>% # take the means for all actual potential productivity values in permutations
  group_by(register, suffix) %>% # group the data by register and suffix
  summarise(potential = mean(Mean)) %>% # find the mean P (across all subcorpora) for each suffix in each register
  ggplot(aes(y = potential, x = suffix)) + # plot the mean value on the y-axis and the suffix category on the x-axis
  geom_point(aes(shape = register)) + # show the values with points (shape marks the register)
  geom_line(aes(group = register, linetype = register)) + # join the points with lines (linetype marks the register)
  theme_bw() + # black-and-white theme
  theme(text = element_text(size = 12), # text size 12
        plot.title = element_text(hjust = 0.5), # plot title aligned to the center
        legend.position = "bottom") + # place legend at the bottom of the graph
  labs(x = "Suffix", 
       y = "Potential productivity (P)\naveraged over 21 subsamples", 
       title = "Average productivity\n(Plag et al. 1999)", 
       shape = "Register", 
       linetype = "Register") +
  ylim(c(0, 0.27)) + # set y-axis limits
  guides(shape = guide_legend(nrow = 2), # show point shape and linetype legend values in two rows
         linetype = guide_legend(nrow = 2)) -> p3 # save the plot to an object called "p3"


png("../figures/varcorp_uus_perm_P_comparisons.png", width = 9, height = 5.5, units = "in", res = 300)
ggarrange(p1, p2, p3, # plot the three graphs on together
          nrow = 1, # in one row
          common.legend = TRUE, # give them a common legend
          legend = "bottom") # place the legend at the bottom of the graph
dev.off()
