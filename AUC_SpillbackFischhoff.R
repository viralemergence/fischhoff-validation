
library(tidyverse); library(magrittr); library(Bolstad2)

probs <- read_csv("Probs.csv")

probs %<>% select(species, zoonotic_capacity_score) %>%
  rename(prob = 'zoonotic_capacity_score')

errors <- c("Eptesicus fuscus",
               "Mus musculus",
               "Procyon lotor",
               "Sciurus niger",
               "Sus scrofa")

newpos <- c("Arctictis binturong",
            "Cricetulus barabensis",
            "Crocuta crocuta",
            "Cynomys ludovicianus",
            "Hippopotamus amphibius",
            "Lynx canadensis",
            "Nasua nasua",
            "Prionailurus viverrinus",
            "Vulpes vulpes")

positives <- c(newpos, errors) # But this is a decision point

probs %<>% mutate(pos = (species %in% positives))

blankdf <- data.frame(Thresh = c(0:1001)/1000, PredPos = NA, Acc = NA)

for (i in 1:nrow(blankdf)) {
  blankdf$PredPos[i] <- sum(na.omit(probs$prob) > blankdf$Thresh[i]) # Less than because it's rank
  blankdf$Acc[i] <- sum(na.omit(probs[probs$pos==TRUE,'prob']) > blankdf$Thresh[i])
  print(i)
}
blankdf$PredPos <- blankdf$PredPos/max(blankdf$PredPos)
blankdf$Acc <- blankdf$Acc/max(blankdf$Acc)

s <- sintegral(blankdf$PredPos, blankdf$Acc)
s$int

# Now try those weird uncertain ones

auc <- c()
for (method in c(1:2)) {
      if(method==1) {
        positives <- c(newpos, errors) # But this is a decision point
      } else {
        positives <- c(newpos)
      }
#    positives <- c(positives, sp1, sp2)
    
    probs %<>% mutate(pos = (species %in% positives))
    
    blankdf <- data.frame(Thresh = c(0:1001)/1000, PredPos = NA, Acc = NA)
    
    for (i in 1:nrow(blankdf)) {
      blankdf$PredPos[i] <- sum(na.omit(probs$prob) > blankdf$Thresh[i]) # Less than because it's rank
      blankdf$Acc[i] <- sum(na.omit(probs[probs$pos==TRUE,'prob']) > blankdf$Thresh[i])
      #print(i)
    }
    blankdf$PredPos <- blankdf$PredPos/max(blankdf$PredPos)
    blankdf$Acc <- blankdf$Acc/max(blankdf$Acc)
    
    s <- sintegral(blankdf$PredPos, blankdf$Acc)
    auc <- c(auc, s$int)
}

auc
