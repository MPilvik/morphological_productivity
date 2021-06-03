# Run permutations and check productivity.
# Take corrected file lists as input
# Shuffle file ordering 100 times
# For each shuffle order
# - divide observations into subcorpora
# - find token, type, hapax counts in subcorpora
# - calculate productivity measures and save them to a table
# 21 subcorpora x 5 registers x 3 suffixes = 315 rows
# 315 x 100 = 31500 rows

# Load the necessary packages
library(svMisc)
library(dplyr)

setwd(choose.dir()) # Set working directory to "data"


#-------------------------#
#---RUN PERMUTATIONS---####
#-------------------------#

# Create an empty data frame
permdf <- data.frame(subcorp = numeric(),
                     register = character(),
                     suffix = character(),
                     tokens = numeric(),
                     types = numeric(),
                     hapaxes = numeric(),
                     s_tokens = numeric(),
                     s_types = numeric(),
                     s_hapaxes = numeric(),
                     permutation = character(),
                     realized = numeric(),
                     potential = numeric(),
                     expanding = numeric())

# Load input data
load("varcorp_uus_SCI_filelists_corr.RData")
load("varcorp_uus_NEWS_filelists_corr.RData")
load("varcorp_uus_FICT_filelists_corr.RData")
load("varcorp_uus_SP_filelists_corr.RData")
load("varcorp_uus_DIA_filelists_corr.RData")

ptm <- proc.time() # Time the script
for(seed in 1:100){ # for each permutation round
  cat("PERMUTATION", seed, "\n") # print a message to the console
  cat("###############", "\n") # print a visual break to the console
  
  for(l in ls(pattern = "biglist")){ # for each register in the workspace data
    reg <- gsub("^[^_]+_([^_]+)_.*$", "\\1", l) # get the name of the register
    cat("REGISTER", reg, "\n") # print a message to the console
    cat("---------------", "\n") # print a visual break to the console
    
    
    #---RANDOM PERMUTATIONS---####
    
    tmplst <- get(l) # take the sampled files in this register
    set.seed(seed) # set random seed for each permutation round
    tmpord <- sample(names(tmplst), # shuffle the file names in random order
                     size = length(tmplst), 
                     replace = F)
    tmplst <- tmplst[tmpord] # reorder the files
    
    
    #---CREATE SUBCORPORA---####
    
    # Create the first subcorpus for 21,300 tokens (initially empty)
    assign(paste0("subcorp", 21300), list())
    # get the names of all created subcorpora in the workspace
    corpsub <- grep("subcorp", ls(), value = TRUE)
    # get the current biggest subcorpus (character)
    maxcorp <- suppressWarnings(corpsub[which.max(as.numeric(gsub("^subcorp([0-9]+)$", "\\1", corpsub)))])
    # get the size of the biggest subcorpus
    corpmax <- as.numeric(gsub("^subcorp([0-9]+)$", "\\1", maxcorp))
    
    pb1 <- txtProgressBar(min = 0, max = length(tmplst), style = 3) # show script progress visually
    
    for(lst in 1:length(tmplst)){ # for each file in the corrected sample
      testlist <- tmplst[[lst]] # examine the file
      
      # Populate whichever is the largest subcorpus with new tokens from the file
      # First, find out how many tokens are already in the subcorpus
      old <- length(get(maxcorp)[["lemma"]])
      
      if(old == 0){ # if the corpus has not been populated yet (the case with the very first file)
        assign(maxcorp, # populate the subcorpus "maxcorp"
               lapply(testlist, function(x) # with current file tokens (incl. suffix tokens and verb tokens)
                 x[as.numeric(names(x)) <= 21300]))  # until the subcorpus is filled
        
        old <- length(get(maxcorp)[["lemma"]]) # find out how many tokens are in the subcorpus now
        
        # Check whether new subcorpus should be created
        # (if yes, there should be indexes > 21,300 in the file, which are put in the file "new")
        new <- lapply(testlist, function(x) 
          x[as.numeric(names(x)) > 21300])
        
        while(length(new[["lemma"]]) > 0){ # while there are still lemmas in the file that are unassigned to any subcorpus
          
          # create a new subcorpus
          # put the existing tokens to the new subcorpus, which is 21,300 tokens bigger than "maxcorp"
          assign(paste0("subcorp", corpmax + 21300), get(maxcorp)) 
          # get the names of all existing subcorpora in the workspace
          corpsub <- grep("subcorp", ls(), value = TRUE) 
          # get the current biggest subcorpus (now 21,3k larger)
          maxcorp <- suppressWarnings(corpsub[which.max(as.numeric(gsub("^subcorp([0-9]+)$", "\\1", corpsub)))])
          # get the size of the biggest subcorpus
          corpmax <- as.numeric(gsub("^subcorp([0-9]+)$", "\\1", maxcorp))
          
          # populate the new biggest subcorpus with the rest of 
          # the current file's tokens 
          # until the new subcorpus is filled
          # or until the lemmas are all assigned
          assign(maxcorp,
                 Map(c, # concatenate
                     get(maxcorp), # current contents of maxcorp
                     lapply(new, function(x) # and those tokens from the file "new"
                       x[as.numeric(names(x)) <= corpmax & as.numeric(names(x)) > corpmax - 21300]))) # whose indexes are smaller or equal to the size of the biggest subcorpus, but bigger than the size of the previous biggest subcorpus
          
          # overwrite the file "new" by keeping the words which did not
          # fit into the new subcorpus either 
          # and run the loop again
          # until no lemmas are left
          new <- lapply(new, function(x) 
            x[as.numeric(names(x)) > corpmax])
        }
      }
      
      else{ # otherwise (= if the subcorpus is already populated)
        # get the lemmas that don't fit in the subcorpus
        new <- lapply(testlist, function(x) 
          x[as.numeric(names(x)) + length(get(maxcorp)[["lemma"]]) > corpmax])
        
        # and overwrite the subcorpus "maxcorp"
        assign(maxcorp, # by
               Map(c, # concatenating
                   get(maxcorp), # current contents of maxcorp
                   lapply(testlist, function(x)  # and those tokens from the file "testlist"
                     x[as.numeric(names(x)) + length(get(maxcorp)[["lemma"]]) <= corpmax]))) # whose indexes are smaller than or equal to the size of the current largest subcorpus when the indexes are increased by the number of empty slots in that current largest subcorpus
        
        while(length(new[["lemma"]]) > 0){ # while there are still lemmas in the file that are unassigned to any subcorpus
          
          # create a new subcorpus
          # put the existing tokens to the new subcorpus, which is 21,300 tokens bigger than "maxcorp"
          assign(paste0("subcorp", corpmax + 21300), get(maxcorp))
          # get the names of all existing subcorpora in the workspace
          corpsub <- grep("subcorp", ls(), value = TRUE)
          # get the current biggest subcorpus (now 21,3k larger)
          maxcorp <- suppressWarnings(corpsub[which.max(as.numeric(gsub("^subcorp([0-9]+)$", "\\1", corpsub)))])
          # get the size of the biggest subcorpus
          corpmax <- as.numeric(gsub("^subcorp([0-9]+)$", "\\1", maxcorp))
          
          if(length(new[["lemma"]]) <= 21300){ # if the number of leftover lemmas is <= 21,300
            # then assign all observations 
            # to the new created largest subcorpus
            assign(maxcorp, # by
                   Map(c, # concatenating 
                       get(maxcorp), # current contents of maxcorp
                       new)) # and all (leftover) tokens from the file "new"
            new <- lapply(new, function(x) x <- NULL) # empty the file "new"
          }
          else{ # if the number of leftover lemmas is > 21,300
            # then only assign the first 21,300 observations 
            # to the new subcorpus
            assign(maxcorp, # by
                   Map(c, # concatenating
                       get(maxcorp), # current contents of maxcorp
                       lapply(new, function(x) # and those tokens from the file "new"
                         x[names(x) %in% names(new[["lemma"]][1:21300])]))) # whose indexes stay between 1 and 21,300
            new <- lapply(new, function(x) # put the leftover lemmas (indexes > 21300) to the object "new" and run the while-loop again
              x[names(x) %in% names(new[["lemma"]][21301:length(new[["lemma"]])])])
          }
        }
      }
      Sys.sleep(0.01)
      # update progress bar
      setTxtProgressBar(pb1, lst)
    }
    close(pb1)
    
    
    #---FIND TYPE, TOKEN, HAPAX COUNTS (incl. for samples)---####
    
    # Create empty vectors for
    sizes <- numeric() # subcorpus sizes
    sfx <- character() # suffix names
    tok <- numeric() # tokens
    typ <- numeric() # types
    hap <- numeric() # hapaxes
    samptok <- numeric() # sample tokens
    samptyp <- numeric() # sample types
    samphap <- numeric() # sample hapaxes
    
    pb2 <- txtProgressBar(min = 0, max = length(ls(pattern = "subcorp")), style = 3) # show script progress visually
    
    x = 1
    
    for(i in ls(pattern = "subcorp")){ # in each of the 21 subcorpora created at the previous step
      tmp <- mget(i) # examine the subcorpus
      size <- as.numeric(gsub("^subcorp([0-9]+)$", "\\1", names(tmp))) # get the size of the subcorpus
      
      mineheads <- tmp[[1]][["mine"]] %>% gsub("^.*_([^_]+)$", "\\1", .) # keep only simple heads of -mine nouns
      jaheads <- tmp[[1]][["ja"]] %>% gsub("^.*_([^_]+)$", "\\1", .) # keep only simple heads of -ja nouns
      usheads <- tmp[[1]][["us"]] %>% gsub("^.*_([^_]+)$", "\\1", .) # keep only simple heads of -us nouns
      
      minetypes <- length(unique(mineheads)) # count the number of -mine types
      jatypes <- length(unique(jaheads)) # count the number of -ja types
      ustypes <- length(unique(usheads)) # count the number of -us types
      
      minetokens <- length(mineheads) # count the number of -mine tokens
      jatokens <- length(jaheads) # count the number of -ja tokens
      ustokens <- length(usheads) # count the number of -us tokens
      
      minehapaxes <- length(table(mineheads)[table(mineheads) == 1]) # count the number of -mine hapaxes
      jahapaxes <-length(table(jaheads)[table(jaheads) == 1]) # count the number of -ja hapaxes
      ushapaxes <- length(table(usheads)[table(usheads) == 1]) # count the number of -us hapaxes
      
      if(reg == "DIA"){ # if the register is DIA
        lemmaheads <- tmp[[1]][["lemma"]] %>% gsub("^[^ ]+\\+([^\\+ ]+ .*)$", "\\1", .) # keep only simple heads of lemmas like this (+)
      }else{ # otherwise
        lemmaheads <- tmp[[1]][["lemma"]] %>% gsub("^[^ ]+_([^_ ]+ .*)$", "\\1", .) # keep only simple heads of lemmas like this (_)
      }
      
      lemmatypes <- length(unique(lemmaheads)) # count the number of lemma types
      lemmatokens <- length(lemmaheads) # count the number of lemma tokens
      lemmahapaxes <- length(table(lemmaheads)[table(lemmaheads) == 1]) # count the number of lemma hapaxes
      
      tok <- append(tok, c(minetokens, jatokens, ustokens)) # append all suffix tokens to token vector
      typ <- append(typ, c(minetypes, jatypes, ustypes)) # append all suffix types to type vector
      hap <- append(hap, c(minehapaxes, jahapaxes, ushapaxes)) # append all suffix hapaxes to hapax vector
      
      sfx <- append(sfx, c("-mine", "-ja", "-us")) # append suffix names to suffix name vector
      sizes <- append(sizes, rep(size, 3)) # append the size of the subcorpus 3 times to the size vector
      
      samptok <- append(samptok, rep(lemmatokens, 3)) # append the number of lemma tokens 3 times to the sample tokens vector
      samptyp <- append(samptyp, rep(lemmatypes, 3)) # append the number of lemma types 3 times to the sample types vector
      samphap <- append(samphap, rep(lemmahapaxes, 3)) # append the number of lemma hapaxes 3 times to the sample hapaxes vector
      
      Sys.sleep(0.01)
      # update progress bar
      setTxtProgressBar(pb2, x)
      x <- x+1 # increase x by 1
    }
    close(pb2)
    
    # Create a data frame for the 21 subcorpora in the current register
    df <- data.frame(subcorp = sizes, register = rep(reg,3), suffix = sfx, tokens = tok, types = typ, hapaxes = hap, s_tokens = samptok, s_types = samptyp, s_hapaxes = samphap)
    
    
    
    #---CALCULATE PRODUCTIVITY MEASURES---####
    
    df %>% mutate(permutation = paste0("permutation", seed),
                  realized = df$types, # realized productivity
                  potential = df$hapaxes/df$tokens, # potential productivity
                  expanding = df$hapaxes/df$s_hapaxes) -> df # expanding productivity
    
    # Add information about the 100 permutations 
    # from this register
    # to the large permdf data frame (eventually contains all registers)
    permdf <- rbind(permdf, df)
    
    rm(list = ls(pattern = "subcorp")) # remove all subcorpora from this register
    gc() # clear memory
  }
  cat("###############", "\n")
}
tottime <- proc.time() - ptm


write.csv2(permdf, "varcorp_uus_100_permutations.csv", fileEncoding = "UTF-8", quote = FALSE, row.names = FALSE)

