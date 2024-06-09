################################################################################
# High School Spelling Data
# Results from various reading and spelling tasks from ~400 high school 
# students.
# 
# Data cleaning
#
# Qs for D & S:
# Note that I am treating illegible as a missing value, since we don't know
# if it was right or wrong(?). 0.missing is treated as an error, since no response
# was provided.
# For the MoST I have ignored the "NOTES" field which specifies some "excluded"
# but they may still be in here.
#
# Created: 2023/08/10
# Author: Serje Robidoux
# CHANGELOG:
# 2023/11/08 - adding in the other testing data for profiling purposes.
# 2024/06/05 - redoing the accuracy coding based on regular expressions and the
#              actual spellings produced for the MoST.
# 2024/06/07 - redoing the MoST scoring using a finalised list of correct
#              correct responses for each item (and accuracy type)

include(readxl)

# 1.0 Demographic information about the students ####
message("getting demographics...")
demog = read_xlsx('High school data 2017 deidentified.xlsx', sheet="Master ")
str(demog)
colnames(demog)[8]="Excluded"

exclIDs = demog$ID[!is.na(demog$Excluded)]

# 2.0 Import the data ####
message("importing...")

## 2.1 DiST.i Irregular words ####
message("         ... DiST.i")

DiST.i = read_xlsx('High school data 2017 deidentified.xlsx', sheet="DiST-i-A"
                   , skip = 2, na = c("n/a", "illegible", "1*", "NS.missing"))
# str(DiST.i)
colnames(DiST.i)[c(1:5,41:42)]=c("ID", "School", "Grade", "Gender", "testDate"
                                 , "totalAcc", "Excluded")
DiST.i = DiST.i %>% filter(!ID %in% exclIDs) %>% 
  rename(DiST.Irr = totalAcc) %>% 
  mutate(ID = gsub("-", "_", ID))
# str(DiST.i)

## 2.2 DiST.n Nonword word spelling ####
message("         ... DiST.n")
# DiST.n has lots of alternative "correct" responses for many items and these
# are coded in an odd way.
DiST.n = read_xlsx('High school data 2017 deidentified.xlsx', sheet="DiST-n-A"
                   , skip = 2, na = c("n/a", "illegible", "1*", "NS.missing"
                                      , "0.illegible", "0.illegible"
                                      , "0. illegible")) %>% data.frame

colnames(DiST.n)[c(1:5, 62:63)]=c("ID", "School", "Grade", "Gender", "testDate"
                                  , "totalAcc", "Excluded")
DiST.n = DiST.n[,-c(64,65)]
colnames(DiST.n)=gsub("\\.\\.", "_", colnames(DiST.n))

for (i in 6:61) 
  DiST.n[,i] = as.numeric(
    ifelse(substr(DiST.n[,i],1,1)=="1", 1
           ,ifelse(substr(DiST.n[,i],1,1)=="0", 0,DiST.n[,i]))
  )

DiST.n = DiST.n %>% filter(!ID %in% exclIDs) %>% rename(DiST.Non=totalAcc)

## 2.3 MoST Morphological nonwords test ####
message("         ... MoST...")

# The MoST has a more complex design - each item falls into a morphological 
# category but also many items have both an "overall" accuracy, and an accuracy 
# for a specific subcomponent of the item;
# E.g., in pronkily, there is the question of whether or not the word is spelled
# correctly, but also whether or not the y in pronky was changed to an i.
# So the overall word could be incorrect, but the "i" is correct:
# e.g., 
# pronkyly would produce 1 in 'pronkily', but 0 in 'pronkily_y2i'
# proncily would produce 0, 1
# pronkily would produce 1, 1
# proncyly would produce 0, 0
message("                 ... responses")
MoST.responses = read_csv('hsspell_itemlevel.csv'
                       , skip = 0, na=c("NA", "NS.illegible")
                      ) %>% data.frame

# A9_226 has 12 of 28 datapoints missing due to illegibility.
MoST.spell = MoST.responses %>% filter(!ID %in% c(exclIDs, "A9_226"))
# Columns will be taken from the Item set in the scoring.

### 2.3.1 MoST Scoring (RegEx) ####
# message("                 ... scoring (regex)")

# Used regular expressions to score spellings for accuracy along all dimensions.
# This has run it's course, and we now have a masterlist of correct/incorrect
# spellings for each item along each of the four dimensions, so this will be
# abandoned for now, in favour of a fixed-list approach.

# We have a set of rules for each item that (hopefully) allows us to code for
# accuracy:
# a) whole item
# b) base word
# c) suffix
# d) "rule knowledge" where relevant

# MoST.scoring = read_xlsx('Information for item level scoring.xlsx'
#                          , sheet="For R") %>%
#   mutate(MiddleRegex = ifelse(is.na(MiddleRegex), "", MiddleRegex))
# 
# colnames(MoST.spell)[3:ncol(MoST.spell)]=MoST.scoring$Item
# 
# #### 2.3.1.1 Reshape and score the Response data ####
# 
# MoST.long = MoST.spell %>%
#   pivot_longer(cols=-(2:1), names_to = "Item", values_to="spelling") %>%
#   merge(MoST.scoring, by="Item", all.x=T) %>%
#   mutate(BaseAcc = !is.na(str_extract(spelling, BaseRegex))
#          , SuffixAcc = !is.na(str_extract(spelling, SuffixRegex))
#          , RuleAcc = ifelse(is.na(RuleRegex), NA
#                             , !is.na(str_extract(spelling, RuleRegex))
#                             )
#          # Any letters that appear between an acceptable BaseRegex and an
#          # acceptable SuffixRegex
#          , midLetters = str_extract(spelling
#                                     , paste0(BaseRegex, "(.*)", SuffixRegex)
#                                     , group=1)
#          # , FullWordRegex = paste0(BaseRegex, MiddleRegex, SuffixRegex)
#          , FullWordAcc = !is.na(str_extract(spelling, FullWordRegex))
#          ### Corrections for stomming, where stombing is also correct, but
#          # stombming would not be. - I think I've corrected this.
#          # , FullWordAcc = ifelse(Item=="stomming"
#          #                      , ifelse(spelling=="stombing", TRUE
#          #                               , ifelse(spelling=="stombming", FALSE
#          #                                        , FullWordAcc))
#          #                      , FullWordAcc)
#          , BaseAcc = ifelse(Item=="prining" & spelling=="prinining"
#                             , FALSE, BaseAcc)
#          ## a lot of "rule errors" are hard to capture in a general way that
#          # won't capture other "correct" answers. I'm just going to be lazy
#          # and hardcode those here.
#          , RuleAcc = ifelse(Item=="prining" &
#                               spelling %in% c("prinining", "priniging")
#                             , FALSE, RuleAcc)
#          , RuleAcc = ifelse(Item=="slarves" &
#                               spelling %in% c("slafves", "slarthves", "slavves")
#                             , FALSE, RuleAcc)
#          , RuleAcc = ifelse(Item=="sniver" &
#                               spelling %in% c("snivver"), FALSE, RuleAcc)
#          , RuleAcc = ifelse(Item == "spiding" &
#                               spelling %in% c("sidering", "spidering")
#                             , FALSE, RuleAcc)
#   )
# 
# # Just a check of what responses are being accepted as correct.
# # MoST.long %>% select(Item, FullWordRegex, FullWordAcc) %>% unique %>% filter(FullWordAcc)
# 
# # For verification purposes, for each item, produce a list of "correct" and
# # "incorrect" responses, for each of the different Accuracies.
# 
# MoST.FullWord.Profile=MoST.long %>% select(Item, FullWordAcc, spelling) %>%
#   mutate(FullWordAcc = ifelse(FullWordAcc, "Accepted", "Errors")) %>%
#   group_by(Item, FullWordAcc) %>%
#   reframe(spellings = unique(spelling)) %>%
#     group_by(Item, FullWordAcc) %>%
#     mutate(counter=1:n()) %>%
#   ungroup() %>%
#   pivot_wider(id_cols=counter
#               , names_from=c("Item", "FullWordAcc")
#               , values_from="spellings")%>%
#   mutate(across(everything(), ~sort(.x, na.last=T))
#          , across(everything(), ~replace_na(.x,"")))
# 
# 
# write.table(MoST.FullWord.Profile, file="MoST.Fullwords.Responses.csv"
#           , sep=";", row.names=F)
# 
# MoST.BaseWord.Profile=MoST.long %>% select(Item, BaseAcc, spelling) %>%
#   mutate(BaseAcc = ifelse(BaseAcc, "Accepted", "Errors")) %>%
#   group_by(Item, BaseAcc) %>%
#   reframe(spellings = unique(spelling)) %>%
#   group_by(Item, BaseAcc) %>%
#   mutate(counter=1:n()) %>%
#   ungroup() %>%
#   pivot_wider(id_cols=counter
#               , names_from=c("Item", "BaseAcc")
#               , values_from="spellings")%>%
#   mutate(across(everything(), ~sort(.x, na.last=T))
#          , across(everything(), ~replace_na(.x,"")))
# 
# 
# write.table(MoST.BaseWord.Profile, file="MoST.BaseAcc.Responses.csv"
#           , sep=";", row.names=F)
# 
# 
# MoST.Suffix.Profile=MoST.long %>% select(Item, SuffixAcc, spelling) %>%
#   mutate(SuffixAcc = ifelse(SuffixAcc, "Accepted", "Errors")) %>%
#   group_by(Item, SuffixAcc) %>%
#   reframe(spellings = unique(spelling)) %>%
#   group_by(Item, SuffixAcc) %>%
#   mutate(counter=1:n()) %>%
#   ungroup() %>%
#   pivot_wider(id_cols=counter
#               , names_from=c("Item", "SuffixAcc")
#               , values_from="spellings")%>%
#   mutate(across(everything(), ~sort(.x, na.last=T))
#          , across(everything(), ~replace_na(.x,"")))
# 
# 
# write.table(MoST.Suffix.Profile, file="MoST.Suffix.Responses.csv"
#           , sep=";", row.names=F)
# 
# 
# MoST.Rule.Profile=MoST.long %>% select(Item, RuleAcc, spelling) %>%
#   filter(!is.na(RuleAcc)) %>% # not all items are relevant to rules.
#   mutate(RuleAcc = ifelse(RuleAcc, "Accepted", "Errors")) %>%
#   group_by(Item, RuleAcc) %>%
#   reframe(spellings = unique(spelling)) %>%
#   group_by(Item, RuleAcc) %>%
#   mutate(counter=1:n()) %>%
#   ungroup() %>%
#   pivot_wider(id_cols=counter
#               , names_from=c("Item", "RuleAcc")
#               , values_from="spellings") %>%
#   mutate(across(everything(), ~sort(.x, na.last=T))
#          , across(everything(), ~replace_na(.x,"")))
# 
# write.table(MoST.Rule.Profile, file="MoST.Rule.Responses.csv"
#           , sep=";", row.names=F)

### 2.3.2 MoST Scoring - List-based ####
message("                 ... correct spellings")

# Reads coding results from an excel file. The excel file contains lists of
# correct and incorrect spellings to each item, using 4 different coding schemes
# a) fullword
# b) baseword
# c) suffix
# d) morphemic rule application

MoSTScoringSrc = "MoST.CodingResults_v3.xlsx"
MoSTScoring = as.list(grep("^MoST\\..*\\.Responses$",readxl::excel_sheets(MoSTScoringSrc)
                                 , value=T))
MoSTNames = gsub("^MoST\\.(.*)\\.Responses$", "\\1", unlist(MoSTScoring))

MoSTItems = read_excel("Information for item level scoring.xlsx"
                       , sheet="Regexes", range="B1:E29")

# MoSTScoring = as.list(MoSTScoringSheets)
names(MoSTScoring) = MoSTNames

mostScores = lapply(MoSTScoring,
       function(x) {
         list(Sheet=x
              , CorrectLists = read_excel(MoSTScoringSrc, sheet=x) %>% 
                select(ends_with("Accepted")) %>% 
                rename_with(~gsub("_Accepted", "", .x))
              )
       })

mostCorrect = mostScores %>% 
  lapply(function(x){
    x[["CorrectLists"]] = 
      x[["CorrectLists"]] %>% 
      pivot_longer(everything(), values_to="spelling", names_to="Item") %>% 
      arrange(Item) %>% drop_na()
  })


#### 2.3.2.1 Reshape and score the Response data ####
message("                 ... scoring responses")

mostLong = MoST.spell %>%
  pivot_longer(cols=-(2:1), names_to = "Item", values_to="spelling") %>%
  # merge(MoST.scoring, by="Item", all.x=T) %>%
  rowwise() %>% 
  mutate(FullWordAcc = ifelse(is.na(spelling), NA, spelling %in% 
           mostCorrect$Fullwords$spelling[mostCorrect$Fullwords$Item==Item])
         , BaseAcc = ifelse(is.na(spelling), NA, spelling %in% 
           mostCorrect$BaseAcc$spelling[mostCorrect$BaseAcc$Item==Item])
         , SuffixAcc = ifelse(is.na(spelling), NA, spelling %in% 
           mostCorrect$Suffix$spelling[mostCorrect$Suffix$Item==Item])
         , RuleAcc = ifelse(is.na(spelling), NA, spelling %in% 
           mostCorrect$Rule$spelling[mostCorrect$Rule$Item==Item])
  ) %>%
  merge(MoSTItems %>% select(Item, Rule), by="Item") %>% 
  as_tibble

### 2.3.2.2 Get some total scores ####
message("                 ... calculating subscales")

mostWide = merge(
  mostLong %>% group_by(ID, Grade) %>% 
    summarise( 
      # missing values are treated as incorrect here
      MoST.full = sum(FullWordAcc, na.rm=T)
      , MoST.base = sum(BaseAcc, na.rm=T)
      , MoST.suffix=sum(SuffixAcc, na.rm=T)
      , MoST.rule_total=sum(RuleAcc, na.rm=T)
      # This set adjusts for missing values by "scaling up" to the correct
      , MoST.full.sc = mean(FullWordAcc, na.rm=T)*28
      , MoST.base.sc = mean(BaseAcc, na.rm=T)*28
      , MoST.suffix.sc=mean(SuffixAcc, na.rm=T)*28
      , MoST.rule_total.sc=mean(RuleAcc & !is.na(Rule), na.rm=T)*17
      # A count of spellings provided
      , MoST.notNA = sum(!is.na(spelling))) %>% 
    ungroup
  # Separate subtotals for different rules
  , mostLong %>% 
    filter(!is.na(Rule)) %>% 
    group_by(ID, Grade, Rule) %>% 
    summarise(MoST=sum(RuleAcc, na.rm=T), notNA=sum(!is.na(spelling))) %>% 
    ungroup %>% 
    pivot_wider(id_cols=c(ID, Grade), names_from=Rule
                , values_from=c(MoST, notNA)
                # , names_prefix="MoST."
                , names_sep="."
                )
)

# Participant A9_226 seems problematic with 12 missing values due to illegible
# A8_137 didn't provide responses to the final 3 items.
# there are four additional "missing", but fairly spread out.

# I think we have to drop A9_226 - odd because they did well on the items that
# could be scored. Seems like a student with a handwriting issue.
# See import of data, where i remove this participant from the start

## 2.4 CC2-A ####
message("         ... CC2-A")

CC2A.row1 = read_xlsx('High school data 2017 deidentified.xlsx'
                          , sheet="CC2-A_FINAL", range="A1:FY1"
                          , skip = 0) %>% colnames
CC2A.row2 = read_xlsx('High school data 2017 deidentified.xlsx'
                      , sheet="CC2-A_FINAL", range="A2:FY2"
                      , skip = 0) %>% colnames

CC2A.colnames = 
  gsub("\\.\\.\\..*", "", 
       paste0(c(rep("",8), rep("Regular.",55), rep("", 2), rep("Irregular.", 55)
                , rep("", 2), rep("Nonword.", 55), rep("", 4)), CC2A.row1, "."
              , CC2A.row2)
  )

CC2A.colnames[c(1:6,8,65,122,178)]=c("School", "Grade", "DOB", "ID", "Gender"
                                 , "DOAx", "Ignore1", "Ignore2", "Ignore3"
                                 , "Notes")
CC2A.wide = read_xlsx(
  'High school data 2017 deidentified.xlsx', sheet="CC2-A_FINAL"
  , range = "A3:FY454", col_names = CC2A.colnames, 
  na = c("n/a")
) %>% 
  select(-Regular, -Irregular, -Nonword, -Ignore1, -Ignore2, -Ignore3) %>% 
  data.frame

# CC2A = CC2A.wide %>% 
#   pivot_longer(matches("^(Regular|Irregular|Nonword)\\.")
#                , values_to = "Accuracy"
#                , names_to = c("ItemType", "ItemNumber", "Item")
#                , names_sep = "\\.")
# 
# CC2A.Totals = aggregate(Accuracy~ID+ItemType+Regulars.Raw+Irregulars.Raw+Nonwords.Raw
#                         , data=CC2A, FUN = sum, na.rm=T)
# 
# Ok, so all of that to basically determine that we only care about the Raw
# scores.

CC2A = CC2A.wide %>% select(ID, ends_with("Raw")) %>% 
  rename(CC2.Reg = Regulars.Raw, CC2.Irr = Irregulars.Raw
         , CC2.Non = Nonwords.Raw)

## 2.5 LetPos ####
message("         ... LetPos")
# There's not much data here, but we'll bring it in anyway.
LetPos.colnames = read_xlsx('High school data 2017 deidentified.xlsx'
                      , sheet="LetPos", range="A1:F1"
                      , skip = 0) %>% #select(-5) %>% 
  rename(LetPos = `Total Score Correct`) %>% 
  colnames

LetPos = read_xlsx(
  'High school data 2017 deidentified.xlsx', sheet="LetPos"
  , range = "A3:F31", col_names = LetPos.colnames, 
  na = c("Excluded")
) %>% 
  select(ID, LetPos) %>% 
  data.frame

## 2.6 TOWRE ####
message("         ... TOWRE")
TOWRE.colnames = read_xlsx('High school data 2017 deidentified.xlsx'
                            , sheet="TOWRE", range="A1:L1"
                            , skip = 0) %>% #select(-5) %>% 
  rename(TOWRE.SW = `Total Sight Word score`
         , TOWRE.PD = `Total Phonemic Decoding score`) %>% 
  colnames

TOWRE = read_xlsx(
  'High school data 2017 deidentified.xlsx', sheet="TOWRE"
  , range = "A3:L31", col_names = TOWRE.colnames, 
  na = c("N/A")
) %>% 
  select(ID, TOWRE.SW, TOWRE.PD) %>% 
  data.frame

## 2.7 WIAT ####
message("         ... WIAT")

# There are so few WIAT's, and it's confusing data, so I will ignore it.
# WIAT = read_xlsx('High school data 2017 deidentified.xlsx'
#                            , sheet="WiAT", range="g3:M26"
#                            , skip = 0, na=c("n/a")) %>%
#   rename(WIAT.Spell = `WIAT Spelling raw score`
#          , WIAT.WR = `WIAT WR raw score`
#          , WIAT.WRspeed = `WIAT WR speed (item completed at 30secs)`
#          , WIAT.PD = `WIAT PD Reading raw score`
#          , WIAT.PDspeed = `WIAT PD Reading speed (item completed at 30secs)`
#          ) %>% slice(-12) %>% data.frame

# 3.0 Merge all scores and demog by ID ####
message("merging scores...")

HSSpelling = demog %>% 
  merge(DiST.i %>% select(ID, DiST.Irr), by="ID", all = T) %>% 
  merge(DiST.n %>% select(ID, DiST.Non), by="ID", all = T) %>% 
  merge(mostWide %>% select(-Grade), by="ID", all = T) %>%
  merge(CC2A %>% select(ID, starts_with("CC2")), by="ID", all = T) %>% 
  # These don't seem to have the right "ID" formats.
  merge(LetPos %>% select(ID, starts_with("LetPos")), by="ID", all = T) %>% 
  merge(TOWRE %>% select(ID, starts_with("TOWRE")), by="ID", all = T) %>% 
  # merge(WIAT %>% select(ID, starts_with("WIAT")), by="ID", all = T) %>% 
  filter(is.na(Excluded)) %>% select(-Excluded)

# 4.0 Reshaping DiST.* & MoST for IRT and CTT ####
message("4.0 Reshaping DiST.* & MoST for IRT and CTT...")
## 4.1 Irregular items ####

DiSTi = DiST.i %>% 
  select(-School, -Grade, -Gender, -testDate, -Excluded, -DiST.Irr) %>% 
  pivot_longer(cols = !ID, names_to="Item", values_to = "Acc")
head(DiSTi)
summary(DiSTi)
DiSTi %>% filter(is.na(Acc)) %>% data.frame

# There are 17 missing values (+1 from a subject that will be excluded anyway)

## 4.2 Nonword items ####
DiSTn = DiST.n %>% 
  select(-School, -Grade, -Gender, -testDate, -Excluded, -DiST.Non) %>% 
  pivot_longer(cols = !ID, names_to="Item", values_to = "Acc") %>% 
  rename(ItemSpellings = Item) %>% 
  mutate(Item = gsub("_.*", "", ItemSpellings))
head(DiSTn)
summary(DiSTn)
DiSTn %>% filter(is.na(Acc)) %>% data.frame %>% arrange(Item)


## 4.3 MoST items ####
MoSTl = mostWide %>% 
  mutate(across(ends_with("Acc"), ~ifelse(.x,))) %>% 
  pivot_longer(cols = !ID, names_to="Item", values_to = "Acc")
head(MoSTl)
summary(MoSTl)
MoSTl %>% filter(is.na(Acc)) %>% arrange(Item) %>% data.frame
