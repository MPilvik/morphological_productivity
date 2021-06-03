#### Take equal samples from corpora (~426,000) ####
# Choose randomly sampled files and count the words in those files. Stop sampling, when the total number of words has exceeded 426,000 after counting the words in the latest file.
# Dialect data is only sampled from files with older xml-markup.
# Produces the file samples426k_filesNtokencounts.RData with the following objects:
# "NEWS_used_files", "NEWS_token_count",
# "FICT_used_files", "FICT_token_count",
# "SCI_used_files", "SCI_token_count",
# "DIA_used_files", "DIA_token_count",
# "SP_used_files", "SP_token_count"

setwd(choose.dir()) # Choose the working directory (sample_files)

#-------------#
#---NEWS---####
#-------------#
setwd("sample_NEWS") # set working directory
NEWS_files <- list.files() # list files in directory
NEWS_used_files <- vector() # empty vector for the names of randomly sampled files
NEWS_token_count <- 0 # numeric vector for cumulatively storing total token count
seed <- 101010 # random seed for ensuring reproducible random sampling

while(!NEWS_token_count > 426000){ # while the total token count does not exceed 426,000
  set.seed(seed) # set random seed
  f <- sample(NEWS_files, 1, replace = FALSE) # sample one file
  while(f %in% NEWS_used_files){ # if the file has already been sampled before
    f <- sample(NEWS_files, 1, replace = FALSE) # sample another file
  }
  cat(f, sep = "\n") # print out the file name
  fail <- readLines(f, encoding = "UTF-8") # read the file (one line per row)
  fail1 <- grep("\\/\\/_[^Z]+", fail, value = TRUE) # keep only morphologically annotated words (exclude punctuation)
  tokens <- length(fail1) # count tokens (one token per line)
  NEWS_token_count <- NEWS_token_count + tokens # add the token count of the file to the total token count of the sample
  NEWS_used_files <- append(NEWS_used_files, f) # add the file name to the list of sampled files
  print(NEWS_token_count) # print out token count
  seed <- seed + 1 # increase the value of the seed
}
setwd("..")


#-------------#
#---FICT---####
#-------------#
setwd("sample_FICT") # set working directory
FICT_files <- list.files() # list files in directory
FICT_used_files <- vector() # empty vector for the names of randomly sampled files
FICT_token_count <- 0 # numeric vector for cumulatively storing total token count
seed <- 101010 # random seed for ensuring reproducible random sampling

while(!FICT_token_count > 426000){ # while the total token count does not exceed 426,000
  set.seed(seed) # set random seed
  f <- sample(FICT_files, 1, replace = FALSE) # sample one file
  while(f %in% FICT_used_files){ # if the file has already been sampled before
    f <- sample(FICT_files, 1, replace = FALSE) # sample another file
  }
  cat(f, sep = "\n") # print out the file name
  fail <- readLines(f, encoding = "UTF-8") # read the file (one line per row)
  fail1 <- grep("\\/\\/_[^Z]+", fail, value = TRUE) # keep only morphologically annotated words (exclude punctuation)
  tokens <- length(fail1) # count tokens (one token per line)
  FICT_token_count <- FICT_token_count + tokens # add the token count of the file to the total token count of the sample
  FICT_used_files <- append(FICT_used_files, f) # add the file name to the list of sampled files
  print(FICT_token_count) # print out token count
  seed <- seed + 1 # increase the value of the seed
}
setwd("..")


#------------#
#---SCI---####
#------------#
setwd("sample_SCI") # set working directory
SCI_files <- list.files() # list files in directory
SCI_used_files <- vector() # empty vector for the names of randomly sampled files
SCI_token_count <- 0 # numeric vector for cumulatively storing total token count
seed <- 101011 # random seed for ensuring reproducible random sampling

while(!SCI_token_count > 426000){ # while the total token count does not exceed 426,000
  set.seed(seed) # set random seed
  f <- sample(SCI_files, 1, replace = FALSE) # sample one file
  while(f %in% SCI_used_files){ # if the file has already been sampled before
    f <- sample(SCI_files, 1, replace = FALSE) # sample another file
  }
  cat(f, sep = "\n") # print out the file name
  fail <- readLines(f, encoding = "UTF-8") # read the file (one line per row)
  fail1 <- grep("\\/\\/_[^Z]+", fail, value = TRUE) # keep only morphologically annotated words (exclude punctuation)
  tokens <- length(fail1) # count tokens (one token per line)
  SCI_token_count <- SCI_token_count + tokens # add the token count of the file to the total token count of the sample
  SCI_used_files <- append(SCI_used_files, f) # add the file name to the list of sampled files
  print(SCI_token_count) # print out token count
  seed <- seed + 1 # increase the value of the seed
}
setwd("..")


#------------#
#---DIA---####
#------------#
setwd("sample_DIA") # set working directory
DIA_files <- list.files(pattern = "*.xml", recursive = TRUE) # list files in directory
DIA_used_files <- vector() # empty vector for the names of randomly sampled files
DIA_token_count <- 0 # numeric vector for cumulatively storing total token count
seed <- 101010 # random seed for ensuring reproducible random sampling

while(!DIA_token_count > 426000){ # while the total token count does not exceed 426,000
  set.seed(seed) # set random seed
  f <- sample(DIA_files, 1, replace = FALSE) # sample one file
  while(f %in% DIA_used_files){ # if the file has already been sampled before
    f <- sample(DIA_files, 1, replace = FALSE) # sample another file
  }
  cat(f, sep = "\n") # print out the file name
  fail <- readLines(f) # read the file (one line per row)
  fail1 <- grep("<u who..KJ", fail, value=T) # keep only the row with speech
  fail2 <- gsub("<mark>", "@<mark>", fail1, perl=T) # mark the beginning of words
  fail3 <- gsub("</mark>", "</mark>@", fail2, perl=T) # mark the ending of words
  fail4 <- unlist(strsplit(fail3, "@")) # split text on words
  fail5 <- grep("<msn>[^<]+<", fail4, value = TRUE) # keep only morphologically annotated words
  tokens <- length(fail5) # count tokens (one token per line)
  DIA_token_count <- DIA_token_count + tokens # add the token count of the file to the total token count of the sample
  DIA_used_files <- append(DIA_used_files, f) # add the file name to the list of sampled files
  print(DIA_token_count) # print out token count
  seed <- seed + 1 # increase the value of the seed
}
setwd("..")

# 5 erratic markings where there is no text between tags <msn></msn>
# Add these to count
DIA_token_count <- DIA_token_count+5


#-----------#
#---SP---####
#-----------#
setwd("sample_SP") # set working directory
SP_files <- list.files(pattern = "*.TextGrid", recursive = TRUE) # list files in directory
SP_used_files <- vector() # empty vector for the names of randomly sampled files
SP_token_count <- 0 # numeric vector for cumulatively storing total token count
seed <- 101010

while(!SP_token_count > 426000){ # while the total token count does not exceed 426,000
  set.seed(seed) # set random seed for ensuring reproducible random sampling
  f <- sample(SP_files, 1, replace = FALSE) # sample one file
  while(f %in% SP_used_files){ # if the file has already been sampled before
    f <- sample(SP_files, 1, replace = FALSE) # sample another file
  }
  cat(f, sep = "\n") # print out the file name
  fail <- scan(f, fileEncoding = "UTF-8", what = "char", sep = "\n") # read the file (one line per row)
  fail1 <- gsub(" +", " ", fail) # remove extra spaces
  fail2 <- grep("\\/\\/_[^Z]+", fail1, value = TRUE) # keep only morphologically annotated words (exclude punctuation)
  tokens <- length(fail2) # count tokens (one token per line)
  SP_token_count <- SP_token_count + tokens # add the token count of the file to the total token count of the sample
  SP_used_files <- append(SP_used_files, f) # add the file name to the list of sampled files
  print(SP_token_count) # print out token count
  seed <- seed + 1 # increase the value of the seed
}
setwd("..")


# Save the names of the samples files and the total token count for each register
save(list = c("NEWS_used_files", "NEWS_token_count",
             "FICT_used_files", "FICT_token_count",
             "SCI_used_files", "SCI_token_count",
             "DIA_used_files", "DIA_token_count",
             "SP_used_files", "SP_token_count"),
    file = "data/samples426k_filesNtokencounts.RData")
