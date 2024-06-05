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

include(readxl)

# 1.0 Demographic information about the students ####
demog = read_xlsx('High school data 2017 deidentified.xlsx', sheet="Master")
str(demog)
colnames(demog)[8]="Excluded"

exclIDs = demog$ID[!is.na(demog$Excluded)]

# 2.0 Import the data ####

## 2.1 DiST.i Irregular words ####
DiST.i = read_xlsx('High school data 2017 deidentified.xlsx', sheet="DiST-i-A"
                   , skip = 2, na = c("n/a", "illegible", "1*", "NS.missing"))
# str(DiST.i)
colnames(DiST.i)[c(1:5,41:42)]=c("ID", "School", "Grade", "Gender", "testDate"
                                 , "totalAcc", "Excluded")
DiST.i = DiST.i %>% filter(!ID %in% exclIDs) %>% 
  rename(DiST.Irr = totalAcc)

## 2.2 DiST.n Nonword word spelling ####
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

# The MoST has an odd design - each item falls into a morphological category
# but also many items have both an "overall" accuracy, and an accuracy for a
# specific subcomponent of the item;
# E.g., in pronkily, there is the question of whether or not the word is spelled
# correctly, but also whether or not the y in pronky was changed to an i.
# So the overall word could be incorrect, but the "i" is correct:
# e.g., 
# pronkyly would produce 1 in 'pronkily', but 0 in 'pronkily_y2i'
# proncily would produce 0, 1
# pronkily would produce 1, 1
# proncyly would produce 0, 0

MoST.items = read_xlsx('High school data 2017 deidentified.xlsx', sheet="MoST-items"
                       , skip = 0) %>% data.frame
MoST = read_xlsx('High school data 2017 deidentified.xlsx', sheet="MoST-A"
                 , range = "F4:AZ483"
                 , na = c("NS.illegible", "NS.unscorable", "NS.Unscorable"
                          , "NS.missing"
                          , "NS.Stombing", "NS.stombing", "NS.Stumbing")
                 ) %>% data.frame

MoST = MoST %>% filter(!ID %in% exclIDs)

### need to calculate some total scores here.

MoST.long = MoST %>% select(-Excluded...Serbian...Resolved, troke, grent) %>% 
  pivot_longer(yocked:prining_e
               , names_sep = "_"
               , names_to = c("Item", "Subscale")
               , values_to = "Accuracy") %>% 
  mutate(Subscale = ifelse(is.na(Subscale), "FullItem", Subscale))

MoST.subscales = MoST.long %>% 
  group_by(ID, Gender, D.O.Assessment, Subscale) %>% 
  summarize(Correct = sum(Accuracy, na.rm=T)
            , Responses = sum(!is.na(Accuracy))
            , Missing = sum(is.na(Accuracy))) %>% 
  ungroup() %>% 
  filter((Subscale == "FullItem" & Responses >=20)
         | (Subscale %in% c("e", "f2v") & Responses == 2)
         | (Subscale == "y2i" & Responses>=4)
         | (Subscale == "2cons" & Responses >=3)) %>% 
  mutate(MoST = 100*Correct/Responses) %>% 
  pivot_wider(
    id_cols = ID:D.O.Assessment
    , names_from = Subscale
    , values_from = Correct:MoST
  ) %>% 
  (function(x) x)()

# Participant A9_226 seems problematic with 18 missing values
# Let's require the following minimum #s of observations:
# FullItems: 20
# e: 2
# f2v: 2
# y2i: 4
# 2cons: 3



## 2.4 CC2-A ####

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
WIAT = read_xlsx('High school data 2017 deidentified.xlsx'
                           , sheet="WiAT", range="g3:M26"
                           , skip = 0, na=c("n/a")) %>%
  rename(WIAT.Spell = `WIAT Spelling raw score`
         , WIAT.WR = `WIAT WR raw score`
         , WIAT.WRspeed = `WIAT WR speed (item completed at 30secs)`
         , WIAT.PD = `WIAT PD Reading raw score`
         , WIAT.PDspeed = `WIAT PD Reading speed (item completed at 30secs)`
         ) %>% slice(-12) %>% data.frame

# 3.0 Merge all scores and demog by ID ####

HSSpelling = demog %>% 
  merge(DiST.i %>% select(ID, DiST.Irr), by="ID", all = T) %>% 
  merge(DiST.n %>% select(ID, DiST.Non), by="ID", all = T) %>% 
  # TODO: Still need to solve this
  merge(MoST.subscales %>% select(-Gender, -D.O.Assessment), by="ID", all = T) %>%
  merge(CC2A %>% select(ID, starts_with("CC2")), by="ID", all = T) %>% 
  merge(LetPos %>% select(ID, starts_with("LetPos")), by="ID", all = T) %>% 
  merge(TOWRE %>% select(ID, starts_with("TOWRE")), by="ID", all = T) %>% 
  merge(WIAT %>% select(ID, starts_with("WIAT")), by="ID", all = T) %>% 
  filter(is.na(Excluded))

# 4.0 Reshaping DiST.* & MoST for IRT and CTT ####

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
MoSTl = MoST %>% 
  select(-Gender, -D.O.Assessment, -troke, -grent, -Excluded...Serbian...Resolved) %>% 
  pivot_longer(cols = !ID, names_to="Item", values_to = "Acc")
head(MoSTl)
summary(MoSTl)
MoSTl %>% filter(is.na(Acc)) %>% arrange(Item) %>% data.frame

# There are some unusual situations where an item is "unscorable"
# e.g., A9_216 gets prining correct, but the prining_e change is "unscorable"

