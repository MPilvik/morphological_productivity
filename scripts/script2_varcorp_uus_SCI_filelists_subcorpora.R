# Extract all -mine, -us, -ja nouns, verbs and all lemmas from SCI files, 
# clean the lists,
# then divide them to subcorpora
# Output: varcorp_uus_SCI_filelists.RData
# Output: varcorp_uus_SCI_filelists_corr.RData
# Output: varcorp_uus_SCI_subcorplist.RData

setwd(choose.dir()) # Set working directory to "data"
load("samples426k_filesNtokencounts.RData") # Load the token counts and files to be sampled for the 426k samples for each subcorpus
setwd("../sample_files/sample_SCI") # Set working directory to files folder

library(dplyr) # Load the necessary packages

biglist_SCI <- list() # Create an empty list
log <- vector() # Create an empty vector


#-----------------------#
#---EXTRACT TOKENS---####
#-----------------------#

for(f in SCI_used_files){ # For each file in the list of used files
  # create empty lists for -mine, -us, -ja, verbs, and lemmas
  testlist <- list(mine = vector(), us = vector(), ja = vector(), verb = vector(), lemma = vector())

  cat(f, sep = "\n") # print file name
  fail <- readLines(f, encoding = "UTF-8") # read file, one word per row
  fail1 <- grep("^.*\\/\\/.*$", fail, value=T) # extract rows with morphological tags
  fail2 <- grep("^.*_Z_.*$", fail1, value=T, invert = T) # exclude rows with punctuation
  cat(length(fail2), sep = "\n") # print file length
  log <- append(log, paste(f, length(fail2), sep = ": ")) # append file name and length to log file
  
  # Extract all -mine, -us, and -ja nouns, verbs and lemmas. Put them in a list so that each word also has an index.
  
  for(tok in 1:length(fail2)){ # For each (morphologically annotated) token in the file
    token <- fail2[tok] # examine the token
    
    if(grepl("^[^ ]+ +.* [^ ]+mine\\+[^\\/]+\\/\\/_S_.*$", token)){ # if token is a -mine noun
      tokm <- gsub("^[^ ]+ +.* ([^ ]+mine)\\+[^\\/]+\\/\\/_S_.*$", "\\1", token) # keep lemmas of nouns ending in -mine
      tokm1 <- grep("\\/\\/", tokm, value=T, invert = TRUE) # keep only rows with -mine nouns
      tokm2 <- gsub("[=\\|’]", "", tokm1) # get rid of unnecessary symbols
      tokm3 <- gsub("^(.*)-([^-]+mine)$", "\\1_\\2", tokm2) # replace '-' with '_'
      tokm4 <- gsub("−", "_", tokm3) # replace '−' with '_'
      tokm5 <- gsub("\\+", "", tokm4) # delete +-signs
      tokm6 <- gsub("^.*\\/([^\\/]+)$", "\\1", tokm5) # delete the first part of word/word structures
      testlist[["mine"]] <- append(testlist[["mine"]], tokm6) # add clean lemma to testlist
      names(testlist[["mine"]])[length(testlist[["mine"]])] <- tok # name the lemma with an index
      
    }else if(grepl("^[^ ]+ +.* [^ ]+us\\+[^\\/]+\\/\\/_S_.*$", token)){ # if token is an -us noun
      toku <- grep("\\/\\/_S_", token, value = TRUE) # keep only nouns
      toku1 <- gsub("^[^ ]+ +.* ([^ ]+us)\\+[^\\/]+\\/\\/_S_.*$", "\\1", toku) # keep lemmas of nouns ending in -us
      toku2 <- grep("\\/\\/", toku1, value=T, invert = TRUE) # keep only rows with -us nouns
      toku3 <- gsub("[=\\|’]", "", toku2) # get rid of unnecessary symbols
      toku4 <- gsub("^(.*)-([^-]+us)$", "\\1_\\2", toku3) # replace '-' with '_'
      toku5 <- gsub("−", "_", toku4) # replace '−' with '_'
      toku6 <- gsub("\\+", "", toku5) # delete +-signs
      toku7 <- gsub("^.*\\/([^\\/]+)$", "\\1", toku6) # delete the first part of word/word structures
      testlist[["us"]] <- append(testlist[["us"]], toku7) # add clean lemma to testlist
      names(testlist[["us"]])[length(testlist[["us"]])] <- tok # name the lemma with an index
      
    }else if(grepl("^[^ ]+ +.* [^ ]+ja\\+[^\\/]+\\/\\/_S_.*$", token)){ # if token is a -ja noun
      tokj <- gsub("^[^ ]+ +.* ([^ ]+ja)\\+[^\\/]+\\/\\/_S_.*$", "\\1", token) # keep lemmas of nouns ending in -ja
      tokj1 <- grep("\\/\\/", tokj, value=T, invert = TRUE) # keep only rows with -ja nouns
      tokj2 <- gsub("[=\\|’]", "", tokj1) # get rid of unnecessary symbols
      tokj3 <- gsub("^(.*)-([^-]+ja)$", "\\1_\\2", tokj2) # replace '-' with '_'
      tokj4 <- gsub("−", "_", tokj3) # replace '−' with '_'
      tokj5 <- gsub("\\+", "", tokj4) # delete +-signs
      tokj6 <- gsub("^.*\\/([^\\/]+)$", "\\1", tokj5) # delete the first part of word/word structures
      tokj7 <- grep("_maja", tokj6, value = TRUE, invert = TRUE) # delete maja-compounds
      tokj8 <- grep("^maja$", tokj7, value = TRUE, invert = TRUE) # delete maja
      testlist[["ja"]] <- append(testlist[["ja"]], tokj8) # add clean lemma to testlist
      names(testlist[["ja"]])[length(testlist[["ja"]])] <- tok # name the lemma with an index
      
    }else if(grepl("\\/\\/_V_", token) & !grepl("_V_ neg", token)){ # if token is a verb
      tokv <- gsub("^[^ ]+ +.* ([^ ]+)\\+[^\\/]+\\/\\/_V_.*$", "\\1", token) # keep only verb stems
      tokv1 <- gsub("\\+", "", tokv) # delete +-signs
      tokv2 <- gsub("[=\\)…]", "", tokv1) # delete unnecessary symbols (=, ), …)
      tokv3 <- gsub("-", "_", tokv2) # replace - with _
      tokv4 <- gsub("^.*\\.+_(.*)$", "\\1", tokv3) # delete parts where the end of the previous sentence has been attached
      tokv5 <- gsub("^.*\\/([^\\/]+)$", "\\1", tokv4) # delete the first part of word/word structures
      testlist[["verb"]] <- append(testlist[["verb"]], tokv5) # add clean lemma to testlist
      names(testlist[["verb"]])[length(testlist[["verb"]])] <- tok # name the lemma with an index
    }
    
    # lemmas
    failt0 <- grep("^.*\\/\\/.*$", token, value=T) # find rows with morphological tags
    failt <- gsub("^[^ ]+ +([^+]+)\\+[^\\/]*\\/\\/_([^_]+)_.*$", "\\1 \\2", failt0) # keep only lemma and word class information
    failt1 <- gsub("=", "", failt) # delete =-signs
    testlist[["lemma"]] <- append(testlist[["lemma"]], failt1) # add clean lemma to testlist
    names(testlist[["lemma"]])[length(testlist[["lemma"]])] <- tok # name the lemma with an index
  }
  biglist_SCI <- append(biglist_SCI, list(testlist)) # add file info to big list
  names(biglist_SCI)[length(biglist_SCI)] <- f # name the added element in big list after the file name
  gc() # clean memory
}
save(biglist_SCI, file = "../../data/varcorp_uus_SCI_filelists.RData")
lapply(biglist_SCI, function(x) length(x[["lemma"]])) %>% unlist() %>% sum() # sample size 427987


#------------------------#
#---CLEAN THE LISTS---####
#------------------------#

# Clean and change back to vector
# Read in previously cleaned files

# -mine suffix
SCImine <- read.csv2("../../data/SCI_mine_varcorp_all.csv", fileEncoding = "UTF-8", header = F) # read clean file
SCImine <- SCImine[SCImine$V4 == max(SCImine$V4),] # choose only the largest subcorpus
SCImine$V3[is.na(SCImine$V3)] <- "" # Replace NA-s with blanks
unique(SCImine)$V1 %>% table() %>% sort(., decreasing = TRUE) %>% head() # all tokens have only one analysis
unique(SCImine[SCImine$V1 != SCImine$V2,]) # which tokens will be changed?
lapply(biglist_SCI, function(x) x$mine) %>% unlist() %>% unname() -> fs # extract all -mine nouns from the big list
length(fs) == nrow(SCImine) # check if the number of -mine nouns in the big list is the same as in the clean file

# -ja suffix
SCIja <- read.csv2("../../data/SCI_ja_varcorp_all.csv", fileEncoding = "UTF-8", header = F) # read clean file
SCIja <- SCIja[SCIja$V4 == max(SCIja$V4),] # choose only the largest subcorpus
unique(SCIja)$V1 %>% table() %>% sort(., decreasing = TRUE) %>% head() # all tokens have only one analysis
unique(SCIja[SCIja$V1 != SCIja$V2,]) # which tokens will be changed?
lapply(biglist_SCI, function(x) x$ja) %>% unlist() %>% unname() -> fs # extract all -ja nouns from the big list
length(fs) == nrow(SCIja) # check if the number of -ja nouns in the big list is the same as in the clean file

# -us suffix
SCIus <- read.csv2("../../data/SCI_us_varcorp_all.csv", fileEncoding = "UTF-8", header = F) # read clean file
names(SCIus) <- c("V1", "V2", "V5", "V6", "V3", "V4") # change column names
SCIus <- SCIus[SCIus$V4 == max(SCIus$V4),] # choose only the largest subcorpus
unique(SCIus)$V1 %>% table() %>% sort(., decreasing = TRUE) %>% head() # all tokens have only one analysis
lapply(biglist_SCI, function(x) x$us) %>% unlist() %>% unname() -> fs # extract all -us nouns from the big list
length(fs) == nrow(SCIus) # check if the number of -us nouns in the big list is the same as in the clean file


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
        n <- n+1              
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

# Clean lists
for(el in 1:length(biglist_SCI)){ # for each file in the big list
  cat(names(biglist_SCI[el]), "\n") # print file name to the console
  lst <- biglist_SCI[[el]] # examine the file
  n <- 0 # set n to 0
  lst[["mine"]] <- cleanfunk(SCImine, "mine") # clean -mine forms
  lst[["ja"]] <- cleanfunk(SCIja, "ja") # clean -ja forms
  lst[["us"]] <- cleanfunk(SCIus, "us") # clean -us forms
  biglist_SCI[[el]] <- lst # replace file tokens in the big list with the corrected tokens
}
biglist_SCI_corr <- biglist_SCI # make a new big list with corrected tokens (not really necessary)

save(biglist_SCI_corr, file = "../../data/varcorp_uus_SCI_filelists_corr.RData") # save file


