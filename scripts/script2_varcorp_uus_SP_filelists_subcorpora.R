# Extract all -mine, -us, -ja nouns, verbs and all lemmas from SP files, 
# clean the lists,
# then divide them to subcorpora
# Output: varcorp_uus_SP_filelists.RData
# Output: varcorp_uus_SP_filelists_corr.RData
# Output: varcorp_uus_SP_subcorplist.RData

setwd(choose.dir()) # Set working directory to "data"
load("samples426k_filesNtokencounts.RData") # Load the token counts and files to be sampled for the 426k samples for each subcorpus
setwd("../sample_files/sample_SP") # Set working directory to files folder

library(dplyr)

biglist_SP <- list() # Create an empty list
log <- vector() # Create an empty vector


#-----------------------#
#---EXTRACT TOKENS---####
#-----------------------#

for(f in SP_used_files){ # For each file in the list of used files
  # create empty lists for -mine, -us, -ja, verbs, and lemmas
  testlist <- list(mine = vector(), us = vector(), ja = vector(), verb = vector(), lemma = vector())
  
  cat(f, sep = "\n") # print file name
  fail <- scan(f, fileEncoding = "UTF-8", what = "char", sep = "\n") # read file, one word per row
  fail1 <- gsub(" +", " ", fail) # remove extra spaces
  fail2 <- grep("\\/\\/_", fail1, value = TRUE) # extract rows with morphological tags
  fail3 <- grep("\\/\\/_Z_", fail2, value = TRUE, invert = TRUE) # leave out punctuation (if there is any)
  log <- append(log, paste(f, length(fail3), sep = ": ")) # append file name and length to log file
  
  # Extract all -mine, -us, and -ja nouns, verbs and lemmas. Put them in a list so that each word also has an index.
  
  for(tok in 1:length(fail3)){ # For each (morphologically annotated) token in the file
    token <- fail3[tok] # examine the token
    
    if(grepl("mine\\+[^\\/_]+//_S_", token)){ # if token is a -mine noun
      tokm <- grep("mine\\+[^\\/_]+//_S_", token, value = TRUE) # keep only rows with -mine nouns
      tokm1 <- gsub("^.* ([^ ]+)\\+[^\\/]+\\/\\/_S_.*$", "\\1", tokm) # only keep the lemma info
      tokm2 <- gsub("\"", "", tokm1) # remove quotation marks
      tokm3 <- gsub("=", "", tokm2) # remove =-symbols
      testlist[["mine"]] <- append(testlist[["mine"]], tokm3) # add clean lemma to testlist
      names(testlist[["mine"]])[length(testlist[["mine"]])] <- tok # name the lemma with an index
      
    }else if(grepl("us\\+[^\\/_]+//_S_", token)){ # if token is an -us noun
      toku <- grep("us\\+[^\\/_]+//_S_", token, value = TRUE) # keep only rows with -us nouns
      toku1 <- gsub("^.* ([^ ]+)\\+[^\\/]+\\/\\/_S_.*$", "\\1", toku) # only keep the lemma info
      toku2 <- gsub("\"", "", toku1) # remove quotation marks
      toku3 <- gsub("=", "", toku2) # remove =-symbols
      testlist[["us"]] <- append(testlist[["us"]], toku3) # add clean lemma to testlist
      names(testlist[["us"]])[length(testlist[["us"]])] <- tok # name the lemma with an index
      
    }else if(grepl("ja\\+[^\\/_]+//_S_", token)){ # if token is a -ja noun
      tokj <- grep("ja\\+[^\\/_]+//_S_", token, value = TRUE) # keep only the rows with -ja nouns
      tokj1 <- gsub("^.* ([^ ]+)\\+[^\\/]+\\/\\/_S_.*$", "\\1", tokj) # only keep the lemma info
      tokj2 <- gsub("\"", "", tokj1) # remove quotation marks
      tokj3 <- gsub("=", "", tokj2) # remove =-symbols
      tokj4 <- grep("_maja", tokj3, value = TRUE, invert = TRUE) # delete maja-compounds
      tokj5 <- grep("^maja$", tokj4, value = TRUE, invert = TRUE) # delete maja
      testlist[["ja"]] <- append(testlist[["ja"]], tokj5) # add clean lemma to testlist
      names(testlist[["ja"]])[length(testlist[["ja"]])] <- tok # name the lemma with an index
      
    }else if(grepl("\\/\\/_V_", token) & !grepl("_V_ neg", token)){ # if token is a verb
      tokv <- grep("_V_", token, value = TRUE) # keep only the rows with verbs
      tokv1 <- gsub("^.* ([^ ]+)\\+[^\\/]+\\/\\/_V_.*$", "\\1", tokv) # only keep the verb stem info
      tokv2 <- gsub("\"", "", tokv1) # remove quotation marks
      testlist[["verb"]] <- append(testlist[["verb"]], tokv2) # add clean lemma to testlist
      names(testlist[["verb"]])[length(testlist[["verb"]])] <- tok # name the lemma with an index
    }
    
    # lemmas
    tokt <- gsub("^.* ([^ ]+)\\+[^\\/]+\\/\\/_([^_]+)_.*$", "\\1 \\2", token) # keep only lemma and word class information
    tokt1 <- gsub("\"", "", tokt) # remove quotation marks
    tokt2 <- gsub("=", "", tokt1) # delete =-signs
    testlist[["lemma"]] <- append(testlist[["lemma"]], tokt2) # add clean lemma to testlist
    names(testlist[["lemma"]])[length(testlist[["lemma"]])] <- tok # name the lemma with an index
  }
  biglist_SP <- append(biglist_SP, list(testlist)) # add file info to big list
  names(biglist_SP)[length(biglist_SP)] <- f # name the added element in big list after the file name
  gc() # clean memory
}

save(biglist_SP, file = "../../data/varcorp_uus_SP_filelists.RData")
lapply(biglist_SP, function(x) length(x[["lemma"]])) %>% unlist() %>% sum() # sample size 426516


#------------------------#
#---CLEAN THE LISTS---####
#------------------------#

# Clean and change back to vector
# Read in previously cleaned files

# -mine suffix
SPmine <- read.csv2("../../data/SP_mine_varcorp_all.csv", fileEncoding = "UTF-8", header = F) # read clean file
SPmine <- SPmine[SPmine$V4 == max(SPmine$V4),] # choose only the largest subcorpus
SPmine$V3[is.na(SPmine$V3)] <- "" # Replace NA-s with blanks
unique(SPmine)$V1 %>% table() %>% sort(., decreasing = TRUE) %>% head() # all tokens have only one analysis
unique(SPmine[SPmine$V1 != SPmine$V2,]) # which tokens will be changed?
lapply(biglist_SP, function(x) x$mine) %>% unlist() %>% unname() -> fs # extract all -mine nouns from the big list
length(fs) == nrow(SPmine) # check if the number of -mine nouns in the big list is the same as in the clean file

# -ja suffix
SPja <- read.csv2("../../data/SP_ja_varcorp_all.csv", fileEncoding = "UTF-8", header = F) # read clean file
SPja <- SPja[SPja$V4 == max(SPja$V4),] # choose only the largest subcorpus
unique(SPja)$V1 %>% table() %>% sort(., decreasing = TRUE) %>% head() # all tokens have only one analysis
unique(SPja[SPja$V1 != SPja$V2,]) # which tokens will be changed?
lapply(biglist_SP, function(x) x$ja) %>% unlist() %>% unname() -> fs # extract all -ja nouns from the big list
length(fs) == nrow(SPja) # check if the number of -ja nouns in the big list is the same as in the clean file

# -us suffix
SPus <- read.csv2("../../data/SP_us_varcorp_all.csv", fileEncoding = "UTF-8", header = F) # read clean file
names(SPus) <- c("V1", "V2", "V5", "V6", "V3", "V4") # change column names
SPus <- SPus[SPus$V4 == max(SPus$V4),] # choose only the largest subcorpus
unique(SPus)$V1 %>% table() %>% sort(., decreasing = TRUE) %>% head() # all tokens have only one analysis
lapply(biglist_SP, function(x) x$us) %>% unlist() %>% unname() -> fs # extract all -us nouns from the big list
length(fs) == nrow(SPus) # check if the number of -us nouns in the big list is the same as in the clean file


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
        n <- n+1# increase n 
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
for(el in 1:length(biglist_SP)){ # for each file in the big list
  cat(names(biglist_SP[el]), "\n") # print file name to the console
  lst <- biglist_SP[[el]] # examine the file
  n <- 0 # set n to 0
  lst[["mine"]] <- cleanfunk(SPmine, "mine") # clean -mine forms
  lst[["ja"]] <- cleanfunk(SPja, "ja") # clean -ja forms
  lst[["us"]] <- cleanfunk(SPus, "us") # clean -us forms
  biglist_SP[[el]] <- lst # replace file tokens in the big list with the corrected tokens
}
biglist_SP_corr <- biglist_SP # make a new big list with corrected tokens (not really necessary)

save(biglist_SP_corr, file = "../../data/varcorp_uus_SP_filelists_corr.RData") # save file


