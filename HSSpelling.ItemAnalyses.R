################################################################################
# High School Spelling Data
# Results from various reading and spelling tasks from ~400 high school 
# students.
# 
# Analyses - IRT and Classical Test Theory analysis of the item sets.
#
# Created: 2023/08/24
# Author: Serje Robidoux
# CHANGELOG:
#


# 0. Setup ####
include(CTT)
include(psych)
if (!all(sapply(c("DiST.i", "DiST.n", "MoST.spell"), exists))) 
  source('HSSpelling.DataImport.R')

# 1.0 CTT ####

## 1.1 DiST.i ####

DiSTi.itemAnalysis=itemAnalysis(DiST.i %>% 
               select(-ID, -School,  -Grade, -Gender, -testDate
                      , -Excluded, -DiST.Irr) %>% 
               drop_na() %>% data.frame
             , itemReport=TRUE)
DiSTi.itemAnalysis$itemReport

### 1.1.1 (looking for a good subset of items for WiL) ####
highPred = DiSTi.itemAnalysis$itemReport %>% arrange(desc(pBis)) %>% 
  slice(1:15) %>% select(itemName) %>% unlist %>% as.vector

(
  DiST.i %>% select(all_of(highPred)) %>% 
    data.frame %>% drop_na %>% itemAnalysis
)$itemReport

(DiST.i %>% select(all_of(highPred)) %>% mutate(Score = rowSums(.))) %>% 
  ggplot(aes(x=Score))+geom_histogram(bins=16)

# This deosn't particularly raise flags. There are a few items with poor 
# discriminability, but they are generally the hardest items so that isn't
# particularly surprising since you can only do so much with an item that is
# rarely answered correctly.

# Cronbach's is very high

## 1.2 DiST.n ####
DiSTn.itemAnalysis=itemAnalysis(DiST.n %>% 
                                  select(-ID, -School,  -Grade, -Gender, -testDate
                                         , -Excluded, -totalAcc) %>% 
                                  drop_na() %>% data.frame
                                , itemReport=TRUE)

DiSTn.itemAnalysis$itemReport %>% 
  mutate(itemName = gsub("_.*", "", itemName)) %>% 
  arrange(itemMean)

# Accuracies fall in a very nice range

DiSTn.itemAnalysis$itemReport %>% 
  mutate(itemName = gsub("_.*", "", itemName)) %>% 
  arrange(pBis)

# vack is pretty poor as a predictor, while drouk and zell are not terrific.

# Cronbach's is ok, at .83

## 1.3 MoST ####

MoST.itemAnalysis=itemAnalysis(MoST %>% 
                                  select(-ID, -Gender, -D.O.Assessment, -troke
                                         , -grent, -Excluded...Serbian...Resolved) %>% 
                                  drop_na() %>% data.frame
                                , itemReport=TRUE)

MoST.itemAnalysis

# Cronbachs a bit weaker at .8

MoST.itemAnalysis$itemReport %>% arrange(itemMean)

# Hm, this one might be a bit too easy.

MoST.itemAnalysis$itemReport %>% arrange(pBis)

# Several items are terrible, but again usually the very easy items:
# Using the usual ".15" for poor discriminability:
#           itemName  itemMean       pBis        bis alphaIfDeleted
# 1        prining_e 0.9549550 0.03382068 0.07394346      0.8064929
# 2          slarves 0.9729730 0.07855712 0.20420423      0.8053282
# 3        nidgeless 0.9256757 0.08074565 0.15065618      0.8061243
# 4          spiding 0.9436937 0.09465214 0.19253542      0.8054749
# 5         stomming 0.9391892 0.10438012 0.20721794      0.8053285
# 6           pilves 0.9842342 0.10572071 0.33319583      0.8048849
# 7        spiding_e 0.9639640 0.11167012 0.26298911      0.8048794
# 8         streffly 0.8445946 0.11838592 0.17968291      0.8064419
# 9         grindles 0.9617117 0.13304502 0.30700121      0.8045100
# 10           truds 0.9707207 0.14390856 0.36383256      0.8043151

# Note that slarves_f2v and prining on are also not great, killing those pairs
# quite poor.

# 11      slarves_f2v 0.7747748 0.15359395 0.21381078      0.8060921
# 12          prining 0.9752252 0.16781161 0.44970464      0.8039818

### 1.3.1 Restricted set ignoring those 12 items ####
exclItems = MoST.itemAnalysis$itemReport %>% arrange(pBis) %>% slice(1:12) %>% 
  select(itemName) %>% as.vector

MoSTr.itemAnalysis = itemAnalysis(MoST %>% 
                                    select(-ID, -Gender, -D.O.Assessment, -troke
                                           , -grent, -Excluded...Serbian...Resolved
                                           , -any_of(exclItems$itemName)) %>% 
                                    drop_na() %>% data.frame
                                  , itemReport=TRUE)

MoSTr.itemAnalysis

#improves alpha a bit

MoSTr.itemAnalysis$itemReport %>% arrange(pBis)

# generally improves pBis

merge(MoST.itemAnalysis$itemReport %>% select(itemName, pBis)
      , MoSTr.itemAnalysis$itemReport %>% select(itemName, pBis)
      , by="itemName")

# 2.0 IRT ####

## 2.1 DiST.i ####

## Let's just look at a single factor for now, since there isn't really a sense
# in which we expect items to load together particularly?

DiSTi.irt = irt.fa(DiST.i %>% select(-ID, -School,  -Grade, -Gender, -testDate
                                     , -Excluded, -totalAcc) %>% 
                     drop_na() %>% data.frame, 1)

# OK, see the excel spreadsheet "IRT.InfoProfiles"

# It looks to me like the 5 "hardest" items are not generating much explanatory
# value. Maybe connoisseur can be kept. Doing 2 factors leads to this same
# result.

## 2.2 DiST.n ####
DiSTn.irt = irt.fa(DiST.n %>% select(-ID, -School,  -Grade, -Gender, -testDate
                                     , -Excluded, -totalAcc) %>% 
                     drop_na() %>% data.frame, 1)


# There are 12 items that are not useful at all, so that's a place to start.
# The rest of the items are useful, but there are quite a few and it could
# probably be trimmed quite a bit.

## 2.3 MoST ####

MoST.irt = irt.fa(MoST %>% 
                    select(-ID, -Gender, -D.O.Assessment, -troke
                           , -grent, -Excluded...Serbian...Resolved) %>% 
                    drop_na() %>% data.frame, 1)
MoST.irt$irt$difficulty[[1]] %>% data.frame


### The following items seem to have just no discrimination value:
# prining_e
# streffly
# nidgeless
# spiding
# spiding_e
# slarves_f2v
# slarves
# stomming
# pronkily

