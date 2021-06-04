# Calculate the ratios of derivations and their verbal bases
# Use only clean base stems, i.e. disregard compound information
# Use only the largest subcorpora for each register (i.e., the total sample)

# Set working directory
setwd(choose.dir()) # data

# Load the necessary packages
library(dplyr)
library(ggplot2)
library(xlsx)


# Load the clean data for the total samples
load("varcorp_uus_maxverbcorr.RData")


#---Make frequency lists---####

# Make frequency lists of verbs
lapply(corverb, # for each register in corverb list
       function(x) x[["verb"]] %>% # take the verbs
         table() %>% # and create a frequency table
         as.data.frame(stringsAsFactors = F)) %>% # in the form of a data frame
  do.call("rbind", .) %>% # unlist the 5 data frames and merge them into one data frame
  tibble::rownames_to_column(., var = "Register") %>% # put rownames (referring to the register) to a column "Register"
  mutate(Register = gsub("^([A-Z]+).*$", "\\1", Register), # keep only the actual register name in the column
         Class = "verb") %>% # add a column "Class" which only has the value "verb" in all rows
  rename(., Stem = ".") -> verbdf # rename the column "." to "Stem" and save the result in a data frame called "verbdf"



# Make frequency lists of nouns
lapply(corverb, # for each register in corverb list
       function(x) lapply(x[1:3], # take the first three elements (referring to the 3 suffixes)
                          function(y) y %>% # for each suffix
                            gsub("^.*_([^_]+)$", "\\1", .) %>% # keep only the simple lemma
                            table() %>% # and create a frequency table
                            as.data.frame(stringsAsFactors = F)) %>% # in the form of a data frame
         do.call("rbind", .) %>% # unlist the 3 data frames and merge them into one data frame (3 suffixes in a data frame for one register)
         tibble::rownames_to_column(., var = "Class") %>% # put rownames (referring to the suffixes) to a column "Class"
         mutate(Class = paste0("-", gsub("^(.*)\\..*$", "\\1", Class)))) %>% # keep only the actual suffix name in the column
  do.call("rbind", .) %>% # unlist the 5 data frames (for registers) and merge them into one data frame
  tibble::rownames_to_column(., var = "Register") %>% # put rownames (referring to the register) to a column "Register"
  mutate(Register = gsub("^([A-Z]+).*$", "\\1", Register)) %>% # keep only the actual name on the column
  rename(., Stem = ".") %>% # rename the column "." to "Stem" 
  mutate(Stem = if_else(Class == "-mine", # if the value in the column "Stem" is of the class "-mine"
                        gsub("^(.*)mine$", "\\1", Stem), # only keep the stem (without the ending -mine)
                        if_else(Class == "-ja", # if the value in the column "Stem" is of the class "-ja"
                                gsub("^(.*)ja$", "\\1", Stem), # only keep the stem (without the ending -ja)
                                Stem))) -> noundf # otherwise keep the value in the column "Stem" as is (-us nouns are already stripped from the ending)


# Merge the data frames
rbind(verbdf, noundf) %>% # put "verbdf" and "noundf" together
  tidyr::pivot_wider(., # convert the long format to wide format 
                     names_from = Class, # values in the column "Class" will become new column names
                     values_from = Freq) %>% # values from the column "Freq" will become the values in the new columns
  replace(is.na(.), 0) %>% # replace NA-values with 0
  tidyr::pivot_longer(., # convert the wide format back to long format
                      cols = 4:6, # only convert columns 4 to 6 (the 3 suffixes)
                      names_to = "Suffix", # column names will become values in the new column "Suffix"
                      values_to = "Value") -> alldf # values in the old columns will be put in the column "Value"

alldf$Register <- factor(alldf$Register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor
alldf$Suffix <- factor(alldf$Suffix, levels = c("-mine", "-us", "-ja")) # make suffix a factor




#---Compare relative base and derivation frequencies---####

alldf %>% # take the data frame "alldf"
  group_by(Register, Suffix) %>% # group observations by register and suffix
  mutate(regsuf = paste(Register, Suffix)) %>% # make a new column "regsuf containing both register and suffix
  ungroup() -> grouped # save the data frame to data frame "grouped"

# Make regsuf a factor
grouped$regsuf <- factor(grouped$regsuf, levels = c("SCI -mine", "SCI -us", "SCI -ja", 
                                                    "NEWS -mine", "NEWS -us", "NEWS -ja", 
                                                    "FICT -mine", "FICT -us", "FICT -ja", 
                                                    "SP -mine", "SP -us", "SP -ja", 
                                                    "DIA -mine", "DIA -us", "DIA -ja"))
grouped %>% # take the grouped data frame "grouped"
  group_by(regsuf) %>% # group the observations by the column "regsuf"
  summarise(cor = round(cor(verb, # find the correlation between base verb frequency
                            Value, # and derivation frequency from the same base
                            method = "spearman"), # us the non-parametric Spearman coefficient
                        2)) -> groupedcors # round the correlation coefficient
groupedcors$Register <- factor(gsub("^([^ ]+) [^ ]+$", "\\1", groupedcors$regsuf), # make register a factor
                               levels = c("SCI", "NEWS", "FICT", "SP", "DIA"))
groupedcors$Suffix <- factor(gsub("^[^ ]+ ([^ ]+)$", "\\1", groupedcors$regsuf), # make suffix a factor
                             levels = c("-mine", "-us", "-ja"))

# Read lexeme translations for plotting
lex <- read.xlsx2("varcorp_uus_basevsderiv_lexemelist.xlsx", sheetIndex = 1, as.data.frame = T)
# Merge the grouped data frame with the translations
merge(grouped, lex, by = c("regsuf", "Stem"), all = TRUE) -> grouped


# Create a graph
png("../figures/varcorp_uus_basevsderiv_grid.png", width = 10, height = 13, units = "in", res = 300)
ggplot(grouped, # take the "grouped" data frame
       aes(x = log1p(Value), # show log derivation frequencies on the x-axis (first add 1 to all frequencies)
           y = log1p(verb))) + # show log verb base frequencies on the y-axis (first add 1 to all frequencies)
  facet_grid(c("Register", "Suffix")) + # each suffix in each register gets its own facet/panel
  geom_point(alpha = 0.3, color = "grey50") + # plot each base as a grey, quite transparent point
  geom_text(data = grouped %>% # show stem labels only for those stems
              filter(!is.na(Translation)), # which also have a translation
            aes(label = Stem), alpha = 0.4, # label based on the column "Stem"
            hjust = -0.1, vjust = 0, size = 4) + # adjust the position and size of the label
  geom_text(data = grouped %>% # show labels also
              filter(!is.na(Translation)),
            aes(label = Translation), alpha = 0.3, # for English translations
            hjust = -0.3, vjust = 1, size = 3) + # their text size is slightly smaller
  geom_smooth(method = "gam", color = "grey75") + # draw the gam-curves
  geom_label(data = groupedcors, # add correlation coefficient labels
             aes(x = 8, y = 9, label = paste("rs =", cor))) +
  theme_bw() +
  labs(x = "Log derivation frequency",
       y = "Log base frequency") +
  theme(text = element_text(size = 14)) +
  xlim(c(0,9)) +
  ylim(c(0,11))
dev.off()


# You can also check each suffix in each register separately
# (uncomment the lines below)
# for(reg in unique(alldf$Register)){
#   for(suf in unique(alldf$Suffix)){
#     alldf %>% 
#       filter(Register == reg, Suffix == suf) %>%
#       ggplot(aes(x = log1p(Value), y = log1p(verb))) +
#       geom_text(aes(label = Stem), alpha = 0.3, color = "grey50", size = 5) +
#       geom_smooth(method = "gam", color = "grey75") +
#       theme_bw() +
#       labs(x = "Log derivation frequency",
#            y = "Log base frequency",
#            title = paste(reg, suf)) +
#       theme(text = element_text(size = 14)) +
#       xlim(c(0, alldf %>% 
#                filter(Register == reg, Suffix == suf) %>% 
#                .$Value %>% 
#                log1p(.) %>% 
#                max() %>% 
#                round()+0.5)) +
#       ylim(c(0, alldf %>%
#                filter(Register == reg, Suffix == suf) %>%
#                .$verb %>%
#                log1p(.) %>%
#                max() %>%
#                round()+0.5)) -> tmpplot
#     ggsave(tmpplot, file=paste0("../figures/varcorp_uus_basederiv_", reg, suf, ".png"), 
#            width = 9, height = 9, units = "in")
#   }
# }





#---Compare -mine and -us bases---####

noundf %>% # take the data frame "noundf"
  filter(Class != "-ja") %>% # get only the rows which do not contain data about the suffix -ja
  tidyr::pivot_wider(., # convert long format to wide format
                     names_from = "Class", # suffix names will be the new column names
                     values_from = "Freq") %>% # values in the columns will be taken from the column "Freq"
  replace(is.na(.), 0) %>% # replace NAs with 0
  filter(`-mine` > 0 & `-us` > 0) %>% # get the rows where both suffixes occur with the same base
  mutate(diff = log2(`-mine`)-log2(`-us`)) -> mineus # find the difference between log frequency of -mine and log frequency of -us (put it in the column "diff")

mineus$Register <- factor(mineus$Register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor

# In case there are many bases which show the same frequency combinations
# and whose labels would be plotted on top of each other,
# assign only one randomly chosen lexeme for each combination of frequencies
set.seed(1) # set random seed
mineus %>% # take the data frame "mineus"
  group_by(Register, `-mine`, `-us`) %>% # group observations by register and the derivation frequencies
  do(sample_n(.,1)) -> rand_lex # sample only one random lexeme from each group

# Create a graph
png("../figures/varcorp_uus_mine_vs_us_base.png", width = 9, height = 12, units = "in", res = 300)
ggplot(data = mineus, # take the data from "mineus"
       aes(x = `-mine`, y = `-us`)) + # -mine frequencies on the x-axis, -us frequencies on the y-axis
  facet_wrap("Register", nrow = 3, scales = "free") + # each register on a separate facet
  geom_point(alpha = 0.2, color = "grey50") + # each base lexeme is represented by a point
  geom_abline(alpha = 0.3, linetype = "dashed") + # draw a symmetrical, diagonal xy-line for reference
  geom_text(data = rand_lex, # add lexeme labels (not all points will get a label)
            aes(label = Stem, # the label comes from the column "Stem" 
                alpha = abs(diff)), # the greater the difference between the log frequencies of -mine and -us, the lighter the label
            hjust = 0.1, vjust = 0.3, # adjust the label position
            show.legend = F, size = 4, # don't show a legend
            check_overlap = T) + # don't plot overlapping labels
  scale_x_continuous(trans = "log2") + # log-transform the x-axis
  scale_y_continuous(trans = "log2") + # log-transform the y-axis
  scale_alpha(range = c(1, 0.1)) + # opacity ranges from 0.1 to 1 (no completely opaque lexemes)
  labs(x = "Frequency of base in -mine formations",
       y = "Frequency of base in -us formations") +
  theme_bw() +
  theme(text = element_text(size = 14))
dev.off()    





#---Compare stem endings in -mine nouns, -us nouns, and verbs---####

# Frequencies of stem endings among -mine nouns
noundf %>% # take the noun data frame
  filter(Class == "-mine") %>% # keep only the rows for -mine nouns
  group_by(Register) %>% # group observations by register
  mutate(Stemend = gsub("^.*(..)$", "\\1", Stem)) %>% # only keep the last 2 characters of each stem (before the suffix)
  group_by(Stemend, Register) %>% # group observations by register and stem ending
  mutate(Freqend = n()) -> minestemends # find the frequency of an ending in each register

minestemends %>% # take the data frame with stem ending frequencies
  select(Register, Class, Stemend, Freqend) %>% # select only 4 columns
  unique() %>% # keep unique rows (delete repeating rows)
  arrange(desc(Freqend)) -> minetbl # arrange the data frame by the frequency of the endings

minetbl %>% # take the data frame with unique rows
  group_by(Register) %>% # group observations by register
  mutate(Prop = Freqend/sum(Freqend)*100) %>% # make a new column with relative frequencies for the endings
  arrange(desc(Prop), Stemend) -> mineprops # arrange the data frame by that relative frequency and stem ending
# make register a factor
mineprops$Register <- factor(mineprops$Register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA"))


# Frequencies of stem endings among -us nouns
noundf %>%
  filter(Class == "-us") %>%
  group_by(Register) %>%
  mutate(Stemend = gsub("^.*(..)$", "\\1", Stem)) %>%
  group_by(Stemend, Register) %>%
  mutate(Freqend = n()) -> usstemends

usstemends %>%
  select(Register, Class, Stemend, Freqend) %>%
  unique() %>%
  arrange(desc(Freqend)) -> ustbl

ustbl %>%
  group_by(Register) %>%
  mutate(Prop = Freqend/sum(Freqend)*100) %>%
  arrange(desc(Prop), Stemend) -> usprops
usprops$Register <- factor(usprops$Register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA"))


# Frequencies of stem endings among verbs
verbdf %>%
  group_by(Register) %>%
  mutate(Stemend = gsub("^.*(..)$", "\\1", Stem)) %>%
  group_by(Stemend, Register) %>%
  mutate(Freqend = n()) -> verbstemends 

verbstemends %>%
  select(Register, Class, Stemend, Freqend) %>%
  unique() %>%
  arrange(desc(Freqend)) -> verbtbl

verbtbl %>%
  group_by(Register) %>%
  mutate(Prop = Freqend/sum(Freqend)*100) %>%
  arrange(desc(Prop), Stemend) -> verbprops



# Merge stem ending frequency data for -mine, -us and verbs
rbind(mineprops, usprops, verbprops) -> allprops
allprops$Stemend <- factor(allprops$Stemend) # make Stemend a factor


# Filter the endings to display
allprops %>% # take the "allprops" data frame
  filter(Stemend %in% c("da", "du", "ga", "gi", # get only rows with these endings 
                        "ka", "ki", "ku", "le", 
                        "ne", "pi", "ri", "ru", 
                        "se", "su", "ta", "tu", 
                        "va", "vi")) %>%
  group_by(Register, Stemend) %>% # group observations by register and stem ending
  mutate(maxp = max(Prop)) %>% # make a column with the maximum relative frequency for each stem ending in each register
  ungroup() %>% # ungroup the data
  select(Register, Stemend, maxp) %>% # select 3 columns 
  unique() -> t # get only unique rows

allprops$Register <- factor(allprops$Register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor
t$Register <- factor(t$Register, levels = c("SCI", "NEWS", "FICT", "SP", "DIA")) # make register a factor

# Create a graph
png("../figures/varcorp_uus_baseendseq.png", width = 10, height = 10, units = "in", res = 300)
ggplot(data = allprops) + # take data from "allprops"
  facet_grid("Register") + # show each register on a different facet/panel
  geom_line(aes(x = Stemend, # put the stem ending category on the x-axis
                y = Prop, # put the relative frequency of the stem ending among all stem endings on the y-axis
                group = Class, # group the line by the class (-mine, -us, or verb)
                linetype = Class)) + # line type marks the class
  geom_text(data = t, # add ending labels from the data frame "t"
            aes(x = Stemend, # put the stem ending category on the x-axis
                y = maxp+2, # put the maximum relative frequency of the ending on the y-axis (add 2 for visibility)
                label = Stemend)) + # label the stem ending
  scale_x_discrete(drop = F) + # don't drop unused categories
  scale_y_continuous(trans = "sqrt", # use square root transformation for the y-axis
                     breaks = c(1,5,10,15,20,40,60)) +
  scale_linetype_manual(values = c(5,1,3)) +
  labs(x = "", y = "Percent of types with given base end sequence\namong all types", linetype = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.grid.major.x = element_blank())
dev.off()