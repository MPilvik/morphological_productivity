# Extract all -mine, -us, -ja nouns, verbs and all lemmas from DIA files, 
# clean the lists,
# then divide them to subcorpora
# Output: varcorp_uus_DIA_filelists.RData
# Output: varcorp_uus_DIA_filelists_corr.RData
# Output: varcorp_uus_DIA_subcorplist.RData

setwd(choose.dir()) # Set working directory to "data"
load("samples426k_filesNtokencounts.RData") # Load the token counts and files to be sampled for the 426k samples for each subcorpus
setwd("../sample_files/sample_DIA") # Set working directory to files folder

library(dplyr) # Load the necessary packages

biglist_DIA <- list() # Create an empty list
log <- vector() # Create an empty vector


#-----------------------#
#---EXTRACT TOKENS---####
#-----------------------#

for(f in DIA_used_files){ # For each file in the list of used files
  # create empty lists for -mine, -us, -ja, verbs, and lemmas
  testlist <- list(mine = vector(), us = vector(), ja = vector(), verb = vector(), lemma = vector())
  
  cat(f, sep = "\n") # print file name
  fail <- readLines(f) # read file, one word per row
  fail1 <- grep("<u who..KJ", fail, value=T) # keep only the row with the informant's speech
  fail2 <- gsub("<mark>", "@<mark>", fail1, perl=T) # mark the beginning of words
  fail3 <- gsub("</mark>", "</mark>@", fail2, perl=T) # mark the ending of words
  fail4 <- unlist(strsplit(fail3, "@")) # split text to individual words
  fail5 <- grep("msn", fail4, value = TRUE) # keep only the rows with morphological annotation
  cat(length(fail5), sep = "\n") # print file length
  log <- append(log, paste(f, length(fail5), sep = ": ")) # append file name and length to log file
  
  # Extract all -mine, -us, and -ja nouns, verbs and lemmas. Put them in a list so that each word also has an index.
  
  for(tok in 1:length(fail5)){ # For each (morphologically annotated) token in the file
    token <- fail5[tok] # examine the token
    
    if(grepl("<mark>.*<msn>([^<]*mine)(\\+[kg]i)?</msn>.* slk=\"S\".*", token)){ # if token is a -mine noun
      tokm <- gsub("<mark>.*<msn>([^<]*mine)(\\+[kg]i)?</msn>.*", "\\1", token, perl=T) # keep lemmas of nouns ending in -mine
      tokm1 <- grep("<", tokm, value = TRUE, invert = TRUE) # keep only rows with -mine nouns
      tokm2 <- gsub("^(.*)\\+([^+]+)$", "\\1_\\2", tokm1) # replace '+' with '_'
      testlist[["mine"]] <- append(testlist[["mine"]], tokm2) # add clean lemma to testlist
      names(testlist[["mine"]])[length(testlist[["mine"]])] <- tok # name the lemma with an index
      
    }else if(grepl("<mark>.*<msn>([^<]*us)(\\+[kg]i)?</msn>.* slk=\"S\".*", token)){ # if token is an -us noun
      toku <- gsub("<mark>.*<msn>([^<]*us)(\\+[kg]i)?</msn>.*", "\\1", token, perl=T) # keep lemmas of nouns ending in -us
      toku1 <- grep("<", toku, value = TRUE, invert = TRUE) # keep only rows with -us nouns
      toku2 <- gsub("^(.*)\\+([^+]+)$", "\\1_\\2", toku1) # replace '+' with '_'
      testlist[["us"]] <- append(testlist[["us"]], toku2) # add clean lemma to testlist
      names(testlist[["us"]])[length(testlist[["us"]])] <- tok # name the lemma with an index
      
    }else if(grepl("<mark>.*<msn>([^<]*ja)(\\+[kg]i)?</msn>.* slk=\"S\".*", token)){ # if token is a -ja noun
      tokj <- gsub("<mark>.*<msn>([^<]*ja)(\\+[kg]i)?</msn>.*", "\\1", token, perl=T) # keep lemmas of nouns ending in -ja
      tokj1 <- grep("<", tokj, value = TRUE, invert = TRUE) # keep only rows with -ja nouns
      tokj2 <- gsub("^(.*)\\+([^+]+)$", "\\1_\\2", tokj1) # replace '+' with '_'
      tokj3 <- grep("_maja", tokj2, value = TRUE, invert = TRUE) # delete maja-compounds
      tokj4 <- grep("^maja$", tokj3, value = TRUE, invert = TRUE) # delete maja
      testlist[["ja"]] <- append(testlist[["ja"]], tokj4) # add clean lemma to testlist
      names(testlist[["ja"]])[length(testlist[["ja"]])] <- tok # name the lemma with an index
      
    }else if(grepl("<mark>.*<mrf slk=\"Va?\">.*", token)){ # if token is a verb
      tokv <- gsub("<mark>.*<msn>([^<]*)ma(\\+[kg]i)?\\+?(na)? ?</msn>.*", "\\1", token, perl=T) # keep only verb stems
      tokv1 <- gsub("^.*<msn>pidamas<.*$", "pida", tokv) # correct faulty lexemes
      tokv2 <- gsub("^.*<msn>[õo]nn?<.*$", "ole", tokv1)
      tokv3 <- gsub("^.*<msn>tulla<.*$", "tule", tokv2)
      tokv4 <- gsub("^.*<msn>.* ei tule<.*$", "tule", tokv3)
      tokv5 <- gsub("^.*<msn>aet.ti<.*$", "aja", tokv4)
      tokv6 <- gsub("^.*<msn>pandud<.*$", "pane", tokv5)
      tokv7 <- gsub("^.*<msn>pidid?<.*$", "pida", tokv6)
      tokv8 <- gsub("^.*<msn>lõhu<.*$", "lõhku", tokv7)
      tokv9 <- gsub("^.*<msn>seism<.*$", "seis", tokv8)
      tokv10 <- gsub("^.*<msn>saa(ned)?<.*$", "saa", tokv9)
      tokv11 <- gsub("^.*<msn>hakkab<.*$", "hakka", tokv10)
      tokv12 <- gsub("^.*<msn>teinud<.*$", "tege", tokv11)
      tokv13 <- gsub("^.*<msn>ütleam<.*$", "ütle", tokv12)
      tokv14 <- gsub("^.*<msn>kasvatatud<.*$", "kasvata", tokv13)
      tokv15 <- gsub("^.*<msn>riisutud<.*$", "riisu", tokv14)
      tokv16 <- gsub("^.*<msn>võtame<.*$", "võt", tokv15)
      tokv17 <- gsub("^.*<msn>läks<.*$", "mine", tokv16)
      tokv18 <- gsub("^.*<msn>tuli<.*$", "tule", tokv17)
      tokv19 <- gsub("^.*<msn>tuli<.*$", "tule", tokv18)
      tokv20 <- gsub("^.*<msn>sünni\\+ki<.*$", "sündi", tokv19)
      tokv21 <- gsub("^.*<msn>pesin<.*$", "pese", tokv20)
      tokv22 <- gsub("^.*<msn>antti<.*$", "and", tokv21)
      tokv23 <- gsub("^.*<msn>mõskam<.*$", "mõsk", tokv22)
      tokv24 <- gsub("^.*<msn>mängida<.*$", "mängi", tokv23)
      tokv25 <- gsub("^.*<msn>päädime<.*$", "päädi", tokv24)
      tokv26 <- gsub("^.*<msn>olliq<.*$", "ole", tokv25)
      tokv27 <- gsub("^.*<msn>käimä<.*$", "käi", tokv26)
      tokv28 <- gsub("^.*<msn>saada<.*$", "saa", tokv27)
      tokv29 <- gsub("^.*<msn>juua<.*$", "joo", tokv28)
      tokv30 <- gsub("^.*<msn>haavata<.*$", "haava", tokv29)
      tokv31 <- gsub("^.*<msn>höülütämä<.*$", "höülüta", tokv30)
      tokv32 <- grep("<", tokv31, value = TRUE, invert = TRUE) # delete other false annotations
      tokv33 <- gsub(" ", "", tokv32) # delete spaces
      tokv34 <- gsub("\\+", "_", tokv33) # replace '+' with '_'
      testlist[["verb"]] <- append(testlist[["verb"]], tokv34) # add clean lemma to testlist
      names(testlist[["verb"]])[length(testlist[["verb"]])] <- tok # name the lemma with an index
    }
    
    # lemmas
    tokt <- gsub("^(.*<msn>.*)\\+[kg]i(</msn>.*)$", "\\1\\2", token, perl=T) # delete the clitic -ki/-gi
    tokt1 <- gsub("^.*<msn>([^<]+)<.* slk=\"([^ ]+)\".*$", "\\1 \\2", tokt, perl = T) # keep only lemma and word class information
    tokt2 <- gsub(" (Pro)?Adva?", " Adv", tokt1) # unify the annotation for Adva, ProAdv, and Adv
    tokt3 <- gsub(" Va?", " V", tokt2) # unify the annotaion for V and Va
    testlist[["lemma"]] <- append(testlist[["lemma"]], tokt3) # add clean lemma to testlist
    names(testlist[["lemma"]])[length(testlist[["lemma"]])] <- tok # name the lemma with an index
  }
  biglist_DIA <- append(biglist_DIA, list(testlist)) # add file info to big list
  names(biglist_DIA)[length(biglist_DIA)] <- f # name the added element in big list after the file name
  gc() # clean memory
}

save(biglist_DIA, file = "../../data/varcorp_uus_DIA_filelists.RData")
lapply(biglist_DIA, function(x) length(x[["lemma"]])) %>% unlist() %>% sum() # sample size 427012


#------------------------#
#---CLEAN THE LISTS---####
#------------------------#

# Clean and change back to vector
# Read in previously cleaned files

# -mine suffix
DIAmine <- read.csv2("../../data/DIA_mine_varcorp_all.csv", fileEncoding = "UTF-8", header = F) # read clean file
DIAmine <- DIAmine[DIAmine$V4 == max(DIAmine$V4),] # choose only the largest subcorpus
DIAmine$V3[is.na(DIAmine$V3)] <- "" # Replace NA-s with blanks
unique(DIAmine)$V1 %>% table() %>% sort(., decreasing = TRUE) %>% head() # all tokens have only one analysis
unique(DIAmine[DIAmine$V1 != DIAmine$V2,]) # which tokens will be changed?
lapply(biglist_DIA, function(x) x$mine) %>% unlist() %>% unname() -> fs # extract all -mine nouns from the big list
length(fs) == nrow(DIAmine) # check if the number of -mine nouns in the big list is the same as in the clean file

# -ja suffix
DIAja <- read.csv2("../../data/DIA_ja_varcorp_all.csv", fileEncoding = "UTF-8", header = F) # read clean file
DIAja <- DIAja[DIAja$V4 == max(DIAja$V4),] # choose only the largest subcorpus
unique(DIAja)$V1 %>% table() %>% sort(., decreasing = TRUE) %>% head() # all tokens have only one analysis
unique(DIAja[DIAja$V1 != DIAja$V2,]) # which tokens will be changed?
lapply(biglist_DIA, function(x) x$ja) %>% unlist() %>% unname() -> fs # extract all -ja nouns from the big list
length(fs) == nrow(DIAja) # check if the number of -ja nouns in the big list is the same as in the clean file

# -us suffix
DIAus <- read.csv2("../../data/DIA_us_varcorp_all.csv", fileEncoding = "UTF-8", header = F) # read clean file
names(DIAus) <- c("V1", "V2", "V5", "V6", "V3", "V4") # rename columns
DIAus <- DIAus[DIAus$V4 == max(DIAus$V4),] # choose only the largest subcorpus
unique(DIAus)$V1 %>% table() %>% sort(., decreasing = TRUE) %>% head() # all tokens have only one analysis
lapply(biglist_DIA, function(x) x$us) %>% unlist() %>% unname() -> fs # all tokens have only one analysis
length(fs) == nrow(DIAus) # extract all -us nouns from the big list


# Function for cleaning the corresponding suffix lists
cleanfunk <- function(clean, suffix){ # function has arguments for the clean dataset and the suffix
  lngth <- length(unique(lst[[suffix]])) # get the number of suffix tokens
  for(i in unique(lst[[suffix]])){ # For each unique (lemmatized) token
    if(i %in% clean$V1){ # if the token is in the cleaned data frame
      
      if(nrow(unique(clean[clean$V1 == i,])) == 1 & # and if the token has only 1 analysis 
         unique(clean[clean$V1 == i,])$V3 != "välja"){ # and it should not be left out of the analysis
        # replace the tokens in list
        # with V2 from the corrected list
        # but keep the different names
        lst[[suffix]][lst[[suffix]] == i] <- unique(clean[clean$V1 == i,]$V2)
        n <- n+1 # increase n             
      }
      else if(nrow(unique(clean[clean$V1 == i,])) != 1 & # if the token does not have only 1 analysis
              unique(clean[clean$V1 == i,])$V3 != "välja"){ # and it should not be left out of the analysis
        cat(i, " on tabelis, aga mitme analüüsiga.", "\n") # print a message to the console 
        lst[[suffix]][lst[[suffix]] == i] <- unique(clean[clean$V1 == i,]$V2)[1] # and keep only the first analysis
      }
      
      else{ # If the token should be left out
        lst[[suffix]] <- lst[[suffix]][lst[[suffix]] != i] # leave it out
      }
    }
    else{
      # If the token is not in the cleaned data frame
      cat(i, " ei ole tabelis! Jätan nii, nagu on.", "\n") # print a message to the console, but don't do anything else
    }
  }
  cat("Replaced", n, "unique forms out of", lngth, suffix, "forms", "\n") # print a message to the console
  
  return(lst[[suffix]]) # return the cleansed tokens
}

# Clean the lists
for(el in 1:length(biglist_DIA)){ # for each file in the big list
  cat(names(biglist_DIA[el]), "\n") # print file name to the console
  lst <- biglist_DIA[[el]] # examine the file
  n <- 0 # set n to 0
  lst[["mine"]] <- cleanfunk(DIAmine, "mine") # clean -mine forms
  lst[["ja"]] <- cleanfunk(DIAja, "ja") # clean -ja forms
  lst[["us"]] <- cleanfunk(DIAus, "us") # clean -us forms
  biglist_DIA[[el]] <- lst # replace file tokens in the big list with the corrected tokens
}
biglist_DIA_corr <- biglist_DIA # make a new big list with corrected tokens (not really necessary)

save(biglist_DIA_corr, file = "../../data/varcorp_uus_DIA_filelists_corr.RData") # save file






