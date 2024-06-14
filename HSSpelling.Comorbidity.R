################################################################################
# High School Spelling Data
# Results from various reading and spelling tasks from ~400 high school 
# students.
# 
# Analyses - "Comorbidity" between DiSTi, DiSTn, and MoST full, base, suffix,
# rule.
#
# Created: 2024/06/10
# Author: Serje Robidoux
# CHANGELOG:
#

# 0. Setup ####
# include(quantreg)
if (!all(sapply(c("DiST.i", "DiST.n", "MoST.spell", "HSSpelling"), exists))) 
  source('HSSpelling.DataImport.R')

# Really just need the mostWide here, merged with the age in months
# Note that one kid has an age of 1390 months, which is obviously nonsense.
# there's also a 9 yr old in gr 9, which makes me think the 9 was duplicated.

# demog %>% filter(AgeM<1000) %>% 
#   ggplot(aes(y=AgeM/12))+
#   geom_boxplot()+
#   facet_grid(.~Grade)+
#   scale_y_continuous(breaks=0:18)

mostComorbidity = HSSpelling %>% select(ID, Grade, DiST.Irr, DiST.Non, MoST.full
                                , MoST.base, MoST.suffix, MoST.rule_total) %>%
  pivot_longer(cols = c(-ID, -Grade), values_to="Score", names_to="Scale") %>% 
  merge(demog %>% select(ID, AgeM, Gender), by="ID") %>% 
  filter(AgeM <1000, AgeM>120) %>% 
  mutate(Scale = factor(Scale
                        # , levels=c("MoST.full", "MoST.base", "MoST.suffix"
                        #            , "MoST.rule_total", "MoST.dbl_cons"
                        #            , "MoST.drop_e", "MoST.f_v", "MoST.y_i"
                        #            , "MoST.full.sc", "MoST.base.sc"
                        #            , "MoST.suffix.sc", "MoST.rule_total.sc")
                        )
  ) %>% filter(!is.na(Score))

# 1.0 Correlations ####

# As a first pass for whether or not MoST.base, Most.suffix and MoST.rule are
# capturing anything beyond the DiSTi and DiSTn tests, we can just look at 
# bivariate correlations.

cor(HSSpelling %>% 
      filter(AgeM>=120, AgeM<=18*12) %>% 
      select(starts_with("MoST"), -ends_with("sc"), -contains("notNA"))
    , HSSpelling %>% filter(AgeM>=120, AgeM<=18*12) %>% 
      select(starts_with("DiST"))
    , use="pairwise.complete")
# Largest correaltion is .71 between the MoST.full words and DiST.Irr, so I
# think generally there is some space for MoST to be capturing something that
# isn't already captured in the DiST*

# Some of the remaining correlation is actually just due to the fact that the
# scores tend to increase with age so that is an age confound.
# Especially for DiST.Irr
ggplot(HSSpelling %>% filter(AgeM>=120, AgeM<=18*12)
       , aes(x=AgeM, y=DiST.Irr))+
  geom_point()+geom_smooth()
ggplot(HSSpelling %>% filter(AgeM>=120, AgeM<=18*12)
       , aes(x=AgeM, y=DiST.Non))+
  geom_point()+geom_smooth()

## 1.1 correlations with age partialed out ####
# to do this, we can just partial age out of all of the relevant variables
# and redo the correlations of the residuals.

partDat = HSSpelling %>% 
  select(AgeM, MoST.full, MoST.base, MoST.suffix, MoST.rule_total
         , DiST.Irr, DiST.Non, CC2.Reg, CC2.Irr, CC2.Non) %>% 
  drop_na() # This is a bit lazy, using only complete data.

partDat = apply(partDat %>% select(-AgeM), 2
      , function(x) residuals(lm(x~partDat$AgeM)))

# 1-
cor(partDat %>% data.frame %>% select(1:4)
    , partDat %>% data.frame %>% select(5:9))#^2

# Shrinks things down a bit, but the same general pattern is there: less than
# half the variance is shared between any two of these after adjust for age.

# 2.0 "Clinical" approach ####

# Here we'll use QR to identify a "critical value" of the poorest 16%
# (age adjusted) for each of the variables, and identify which kids are in that
# category. Then we can look at contingency tables.

## 2.1 Calculate .16 QR scores for each variable ####

mostQRs = by(mostComorbidity, mostComorbidity$Scale
             , FUN = 
               function(x) 
                 data.frame(x, q16.age=predict(rq(Score~AgeM, x, tau=.16)))
)

### 2.1.1 reorganize into a wide ####
lowScores = 
  lapply(mostQRs
       , function(x) 
         x %>% mutate(ofConcern = ifelse(q16.age > Score, "concern", "ok"))
  ) %>% 
    bind_rows() %>%
    select(ID, AgeM, Scale, Score, q16.age, ofConcern) %>%
    pivot_wider(id_cols = c(ID, AgeM), names_from = "Scale"
                , values_from="ofConcern")

## 2.2 Contingency table ####

# there are two things to think about here:
# 1) of the kids ID'd by the DistI or distn as "poor", how many would be
# missed by the MoST. This is asking if DiST is picking up some kids that would
# not be flagged by the MoST.
# 2) of the kids ID'd by MoST, how many were already ID'd by the DiSTs?

# considering only kids who have the Disti, the distn and whichever Most 
# is being considered
lowScores %>% 
  # mutate(DiST =#DiST.Irr 
           # DiST.Non
           #TRUE
         # , MoST = MoST.full | MoST.base | MoST.suffix | MoST.rule_total
         # , MoST.subs = MoST.base | MoST.suffix | MoST.rule_total
         # ) %>% 
  select(
   # DiST
    # , DiST.Irr
    # , DiST.Non
    # , MoST.full
    , MoST.base
    # , MoST.suffix
    , MoST.rule_total
    # , MoST
    # , MoST.subs
    ) %>% table
  # summarise(Frequency = n()) %>% 
  # ungroup() %>% 
  # arrange(desc(Frequency)) %>% 
  # drop_na %>% 
  # mutate(n = cumsum(Frequency))

### Any MoST vs Any DiST
# of the 190 kids flagged by DiST*, 135 (71.1%) were caught by MoST as well.
# of the 186 kids flagged by MoST, 135 (72%) were already flagged by DiST
# of the 277 kids DiST did not identify, MoST ID'd 51 (18.4%)

### MoST subscales vs Any DiST
# of the 190 kids flagged by DiST*, 133 (70%) were caught by MoST as well.
# of the 184 kids flagged by MoST, 133 (72%) were already flagged by DiST
# of the 277 kids DiST did not identify, MoST ID'd 51 (18.4%)

### Most.Full vs Any DiST
# of the 190 kids flagged by DiST*, 96 (52%) were caught by MoST as well.
# of the 115 kids flagged by MoST, 96 (84%) were already flagged by DiST
# of the 277 kids DiST did not identify, MoST ID'd 19 (6.9%)

### Most.Base
# of the 190 kids flagged by DiST*, 86 (45%) were caught by MoST as well.
# of the 116 kids flagged by MoST, 86 (74%) were already flagged by DiST
# of the 277 kids DiST did not identify, MoST ID'd 30 (10.8%)

### Most.Suffix
# of the 190 kids flagged by DiST*, 92 (48%) were caught by MoST as well.
# of the 116 kids flagged by MoST, 92 (79%) were already flagged by DiST
# of the 277 kids DiST did not identify, MoST ID'd 24 (8.7%)

### MoST.Rule
# of the 190 kids flagged by DiST*, 97 (51%) were caught by MoST as well.
# of the 116 kids flagged by MoST, 97 (84%) were already flagged by DiST
# of the 277 kids DiST did not identify, MoST ID'd 17 (6.1%)

# One issue here, is that if you consider "lowest 25th percentile" on any scale
# to be an issue, then you will identify 29% overall as poor spellers on the
# MoST. The DiST picks up 41% of kids. This is because there are a lot of kids
# who struggle on one, but not the other, DiSTi. The MoST subscales are less
# distinct (since they are interrelated), so they don't suffer from this as much.

# (though the numbers would be smaller, the same issue arrives for other 
# thresholds)

# It's also worth noting that in this approach, once you've accounted for
# Base, Rule, or Suffix, "full word" no longer contributes. That is, (almost)
# anyone who has a "full word problem" will have one or more of "Base, Rule, 
# or Suffix" issues.
