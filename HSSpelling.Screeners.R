################################################################################
# High School Spelling Data
# Results from various reading and spelling tasks from ~400 high school 
# students.
# 
# Development of a Screener to identify the poorest 20%(?) of spellers
# by item type.
#
# Created: 2025/06/19
# Author: Serje Robidoux
# CHANGELOG:
#


# 0. Setup ####
include(psych)
include(quantreg)
if (!all(sapply(c("DiST.i", "DiST.n", "MoST.spell"), exists))) 
  source('HSSpelling.DataImport.R')

# 1.0 DiST.i ####

## 1.1 Quantile Regressions ####
# There are 35 items, so scores need to be adjusted to assume 1 additional
# correct and one incorrect value.

DiST.i = DiST.i %>% 
  mutate(Irr.logit = qlogis((DiST.Irr+1)/37))

(DiST.i.qr = rq(Irr.logit~Grade, DiST.i, tau=.20)) %>% summary

DiST.i$irr.fit = NA
DiST.i[rownames(DiST.i.qr$x),"irr.fit"] = 35*plogis(predict(DiST.i.qr))

# Age-based threshold table
qrMod.irr.tot = data.frame(Grade = 6:11) %>% 
  mutate(DiST.Irr=35*plogis(predict(DiST.i.qr, data.frame(Grade = 6:11))))

ggplot(DiST.i, aes(x=jitter(Grade, factor=.5), y=DiST.Irr))+
  geom_point(col="darkgrey")+
  # geom_smooth()+
  geom_line(data=qrMod.irr.tot, aes(x=Grade, y=DiST.Irr, colour=NULL), size=1, col="blue")+
  theme_minimal()+
  labs(x="Grade", y="Score (of 35)"
       , title="20th Percentile Performance (Irregs)", subtitle="as a function of grade")

## 1.2 At-risk kids (20th percentile, grade adjusted) ####
# So, let's flag the poorest 20% of SPA spellers (age adjusted)

DiST.i = DiST.i %>% 
  mutate(sub20p.irr.tot = ifelse(DiST.Irr < irr.fit, 1, 0))

ggplot(DiST.i, aes(x=jitter(Grade), y=DiST.Irr, col=sub20p.irr.tot))+
  geom_point()+
  # geom_smooth()+
  geom_line(data=qrMod.irr.tot, aes(x=Grade, y=DiST.Irr, colour=NULL), size=1, col="blue")+
  theme_minimal()+
  labs(x="Grade", y="Score (of 35)"
       , title="20th Percentile Performance (Irregs)", subtitle="as a function of grade")

## 1.3 Item selection - Overall ####
### 1.3.1 sub20p by Item correlations - Overall ####
DiST.i.ItemCors = data.frame(Cor = t(cor(DiST.i$sub20p.irr.tot
                     , DiST.i %>% select(6:40), use="pairwise.complete"))
           ) %>% 
  mutate(Item = rownames(.)) %>% arrange(Cor)

#### 1.3.2 Scores on the top 10 items, by sub20 status ####

DiST.i.screener = DiST.i.ItemCors$Item[1:10]

DiST.i = DiST.i %>% 
  mutate(Irr.screener = rowSums(pick(all_of(DiST.i.screener))))

ggplot(DiST.i, aes(x=sub20p.irr.tot, y=Irr.screener, group=sub20p.irr.tot)) +
  geom_boxplot()+scale_y_continuous(breaks=0:10)+
  geom_point(aes(x=jitter(sub20p.irr.tot)))

## 1.4 Item Selection, by grade ####
### 1.4.1 sub20p by Item correlations - Overall ####
DiST.i.ItemCors.byGrade = 
  by(DiST.i, DiST.i$Grade
     , function(x) {
       data.frame(
         Cor = t(
           cor(x$sub20p.irr.tot, x %>% select(6:40)
               , use="pairwise.complete")
         )
       ) %>% 
         mutate(Item = rownames(.)) %>% arrange(Cor)
     }
  ) 

DiST.i.screener.byGrade =
  lapply(DiST.i.ItemCors.byGrade, function(x) x$Item[1:10])

DiST.i$Irr.gradescreener=NA
for(s in 1:nrow(DiST.i))
  DiST.i$Irr.gradescreener[s] = 
  rowSums(DiST.i[s,DiST.i.screener.byGrade[[DiST.i$Grade[s]-5]]])

table(DiST.i$Grade,DiST.i$Irr.gradescreener, DiST.i$sub20p.irr.tot)

#### So a rule of 5 or less on these grade-appropriate sets, would lead to
# 100% sensitivity and the following "false positive rates"

aggregate((Irr.gradescreener<=5)~Grade+sub20p.irr.tot, DiST.i, mean)
#    Grade sub20p.irr.tot             FalsePosRate
# 1      6              0               0.10714286
# 2      7              0               0.09677419
# 3      8              0               0.00000000
# 4      9              0               0.25396825
# 5     10              0               0.16129032
# 6     11              0               0.20000000

### Could adopt different rules to reduce the FPR:
# Grade           Rule       FalsePosRate
#     6              2               0.000
#     7              4               0.016
#     8              5               0.000
#     9              4               0.158
#    10              5               0.161
#    11              2               0.000


# 2.0 DiST.m - Baseword (as Nonwords) ####
# These are based on the MoST dataset.

DiST.m.base = mostLong %>% 
  select(Item, ID, Grade, BaseAcc) %>% 
  mutate(BaseAcc = as.numeric(BaseAcc)) %>% 
  pivot_wider(names_from=Item, values_from = BaseAcc) %>% 
  mutate(Base = rowSums(pick(-ID, -Grade)))

## 2.1 Quantile Regressions ####
# There are 26 items, so scores need to be adjusted to assume 1 additional
# correct and one incorrect value.

DiST.m.base = DiST.m.base %>% 
  mutate(Base.logit = qlogis((Base+1)/28))

(DiST.m.base.qr = rq(Base.logit~Grade, DiST.m.base, tau=.20)) %>% summary

DiST.m.base$base.fit = NA
DiST.m.base[rownames(DiST.m.base.qr$x),"base.fit"] = 26*plogis(predict(DiST.m.base.qr))

# Age-based threshold table
qrMod.base.tot = data.frame(Grade = 6:11) %>% 
  mutate(Base=26*plogis(predict(DiST.m.base.qr, data.frame(Grade = 6:11))))

ggplot(DiST.m.base, aes(x=jitter(Grade, factor=.5), y=Base))+
  geom_point(col="darkgrey")+
  # geom_smooth()+
  geom_line(data=qrMod.base.tot, aes(x=Grade, y=Base, colour=NULL), size=1, col="blue")+
  theme_minimal()+
  labs(x="Grade", y="Score (of 26)"
       , title="20th Percentile Performance (Nonwrods)"
       , subtitle="as a function of grade")

## 2.2 At-risk kids (20th percentile, grade adjusted) ####
# So, let's flag the poorest 20% of SPA spellers (age adjusted)

DiST.m.base = DiST.m.base %>% 
  mutate(sub20p.base.tot = ifelse(Base < base.fit, 1, 0))

ggplot(DiST.m.base, aes(x=jitter(Grade), y=Base, col=sub20p.base.tot))+
  geom_point()+
  # geom_smooth()+
  geom_line(data=qrMod.base.tot, aes(x=Grade, y=Base, colour=NULL), size=1, col="blue")+
  theme_minimal()+
  labs(x="Grade", y="Score (of 26)"
       , title="20th Percentile Performance (Nonwrods)"
       , subtitle="as a function of grade")

## 2.3 Item selection - Overall ####
### 2.3.1 sub20p by Item correlations - Overall ####
DiST.m.base.ItemCors = data.frame(Cor = t(cor(DiST.m.base$sub20p.base.tot
                                         , DiST.m.base %>% select(3:28), use="pairwise.complete"))
) %>% 
  mutate(Item = rownames(.)) %>% arrange(Cor)
## Extremely weak correlations, so probably not worth considering

#### 2.3.2 Scores on the top 10 items, by sub20 status ####

DiST.m.base.screener = DiST.m.base.ItemCors$Item[1:10]

DiST.m.base = DiST.m.base %>% 
  mutate(Base.screener = rowSums(pick(all_of(DiST.m.base.screener))))

ggplot(DiST.m.base, aes(x=sub20p.base.tot, y=Base.screener, group=sub20p.base.tot)) +
  geom_boxplot()+scale_y_continuous(breaks=0:10)+
  geom_point(aes(x=jitter(sub20p.base.tot)))

### Yep, terrible specificity

## 2.4 Item Selection, by grade ####
### 2.4.1 sub20p by Item correlations - Overall ####
DiST.m.base.ItemCors.byGrade = 
  by(DiST.m.base, DiST.m.base$Grade
     , function(x) {
       data.frame(
         Cor = t(
           cor(x$sub20p.base.tot, x %>% select(3:28)
               , use="pairwise.complete")
         )
       ) %>% 
         mutate(Item = rownames(.)) %>% arrange(Cor)
     }
  ) 

DiST.m.base.screener.byGrade =
  lapply(DiST.m.base.ItemCors.byGrade, function(x) x$Item[1:10])

DiST.m.base$Base.gradescreener=NA
for(s in 1:nrow(DiST.m.base))
  DiST.m.base$Base.gradescreener[s] = 
  rowSums(DiST.m.base[s,DiST.m.base.screener.byGrade[[DiST.m.base$Grade[s]-5]]])

table(DiST.m.base$Grade,DiST.m.base$Base.gradescreener, DiST.m.base$sub20p.base.tot)

## This is much less sensitive than the Irregs were.

# Very high FPR across the board
aggregate((Base.gradescreener<=7)~Grade+sub20p.base.tot, DiST.m.base, mean)
#    Grade    sub.base.tot              FalsePosRate
# 1      6               0                 0.3404255
# 2      7               0                 0.2745098
# 3      8               0                 0.2500000
# 4      9               0                 0.2763158
# 5     10               0                 0.1666667
# 6     11               0                 0.3076923

### Could adopt different rules to reduce the FPR:
# Grade           Rule       FalsePosRate
#     6              5               0.042
#     7              7               0.274
#     8              6               0.119
#     9              6               0.131
#    10              6               0.021
#    11              6               0.077

## Grade 7 stays terrible, it seems they are just much messier spellers in this sample.
# If we adopted a rule of 6 for Grade 7, we would miss 1/19 or about 5% of at-risk kids
# but would reduce the FPR from 27.4% to 12.7% or so. that kid also *just* qualifies as
# at-risk.

# 3.0 DiST.m - Rule (as Morph) ####
# These are based on the MoST dataset.Only 15 items have rules

DiST.m.rule = mostLong %>% 
  filter(!is.na(Rule)) %>% 
  select(Item, ID, Grade, RuleAcc) %>% 
  mutate(RuleAcc = as.numeric(RuleAcc)) %>% 
  pivot_wider(names_from=Item, values_from = RuleAcc) %>% 
  mutate(Rule = rowSums(pick(-ID, -Grade)))

## 3.1 Quantile Regressions ####
# There are 15 items, so scores need to be adjusted to assume 1 additional
# correct and one incorrect value.

DiST.m.rule = DiST.m.rule %>% 
  mutate(Rule.logit = qlogis((Rule+1)/17))

(DiST.m.rule.qr = rq(Rule.logit~Grade, DiST.m.rule, tau=.2)) %>% summary

DiST.m.rule$rule.fit = NA
DiST.m.rule[rownames(DiST.m.rule.qr$x),"rule.fit"] = 15*plogis(predict(DiST.m.rule.qr))

# Age-ruled threshold table
qrMod.rule.tot = data.frame(Grade = 6:11) %>% 
  mutate(Rule=15*plogis(predict(DiST.m.rule.qr, data.frame(Grade = 6:11))))

ggplot(DiST.m.rule, aes(x=jitter(Grade, factor=.5), y=Rule))+
  geom_point(col="darkgrey")+
  # geom_smooth()+
  geom_line(data=qrMod.rule.tot, aes(x=Grade, y=Rule, colour=NULL), size=1, col="blue")+
  theme_minimal()+
  labs(x="Grade", y="Score (of 15)"
       , title="20th Percentile Performance (Morphology)"
       , subtitle="as a function of grade")


## 3.2 At-risk kids (20th percentile, grade adjusted) ####
# So, let's flag the poorest 20% of SPA spellers (age adjusted)

DiST.m.rule = DiST.m.rule %>% 
  mutate(sub20p.rule.tot = ifelse(Rule < rule.fit, 1, 0))

aggregate(sub20p.rule.tot~Grade, DiST.m.rule, mean, na.rm=T)
ggplot(DiST.m.rule, aes(x=jitter(Grade), y=Rule, col=sub20p.rule.tot))+
  geom_point()+
  # geom_smooth()+
  geom_line(data=qrMod.rule.tot, aes(x=Grade, y=Rule, colour=NULL), size=1, col="blue")+
  theme_minimal()+
  labs(x="Grade", y="Score (of 26)"
       , title="20th Percentile Performance (Nonwrods)"
       , subtitle="as a function of grade")

## 3.3 Item selection - Overall ####
### 3.3.1 sub20p by Item correlations - Overall ####
DiST.m.rule.ItemCors = data.frame(Cor = t(cor(DiST.m.rule$sub20p.rule.tot
                                              , DiST.m.rule %>% select(3:17), use="pairwise.complete"))
) %>% 
  mutate(Item = rownames(.)) %>% arrange(Cor)
## Weak correlations, so probably not worth considering

#### 3.3.2 Scores on the top 10 items, by sub20 status ####

DiST.m.rule.screener = DiST.m.rule.ItemCors$Item[1:10]

DiST.m.rule = DiST.m.rule %>% 
  mutate(Rule.screener = rowSums(pick(all_of(DiST.m.rule.screener))))

ggplot(DiST.m.rule, aes(x=sub20p.rule.tot, y=Rule.screener, group=sub20p.rule.tot)) +
  geom_boxplot()+scale_y_continuous(breaks=0:10)+
  geom_point(aes(x=jitter(sub20p.rule.tot)))

aggregate((Rule.screener<=6)~Grade, DiST.m.rule, mean, na.rm=T)
# Terrible specificity, especially for younger kids.

## 3.4 Item Selection, by grade ####
### 3.4.1 sub20p by Item correlations - Overall ####
DiST.m.rule.ItemCors.byGrade = 
  by(DiST.m.rule, DiST.m.rule$Grade
     , function(x) {
       data.frame(
         Cor = t(
           cor(x$sub20p.rule.tot, x %>% select(3:17)
               , use="pairwise.complete")
         )
       ) %>% 
         mutate(Item = rownames(.)) %>% arrange(Cor)
     }
  ) 

DiST.m.rule.screener.byGrade =
  lapply(DiST.m.rule.ItemCors.byGrade, function(x) x$Item[1:10])

DiST.m.rule$Rule.gradescreener=NA
for(s in 1:nrow(DiST.m.rule))
  DiST.m.rule$Rule.gradescreener[s] = 
  rowSums(DiST.m.rule[s,DiST.m.rule.screener.byGrade[[DiST.m.rule$Grade[s]-5]]])

table(DiST.m.rule$Grade,DiST.m.rule$Rule.gradescreener, DiST.m.rule$sub20p.rule.tot)

## This is much less sensitive than the Irregs were.

# Very high FPR across the board
aggregate((Rule.gradescreener<=6)~Grade+sub20p.rule.tot, DiST.m.rule, mean)
#    Grade    sub.rule.tot              FalsePosRate
# 1      6               0                 0.4390244
# 2      7               0                 0.2500000
# 3      8               0                 0.3333333
# 4      9               0                 0.1600000
# 5     10               0                 0.0250000
# 6     11               0                 0.0000000

# Quite poor, especially for G6

### If we adopt different rules to reduce the FPR:
# Grade           Rule       FalsePosRate
#     6              3               0.000
#     7              5               0.156
#     8              5               0.111
#     9              6               0.160
#    10              6               0.025
#    11              6               0.000

# 4.0 Merging Baseword and Rule item sets ####

## 4.1 - Baseword accuracy ####
DiST.m.base$Base.combined=NA
for(s in 1:nrow(DiST.m.base))
  DiST.m.base$Base.combined[s] = 
  rowSums(
    DiST.m.base[s
                , unique(c(
                  DiST.m.base.screener.byGrade[[DiST.m.base$Grade[s]-5]]
                  , DiST.m.rule.screener.byGrade[[DiST.m.base$Grade[s]-5]]
                ))
    ]
  )

table(DiST.m.base$Grade,DiST.m.base$Base.combined, DiST.m.base$sub20p.base.tot)

# of items would vary by grade now, since there are different numbers of overlap-
# ping items per grade
# Grade Items   Threshold   FPR
# 6     15      9           29.8%
# 7     15      9           19.6%**
# 8     13      8           15.4%
# 9     16      9           11.8%**
# 10    17      10           4.2%
# 11    16      11          10.3%

## ** indicates scales where this improves on the original set of 10 baseword
# items. In the other cases, it makes things worse.

## 4.1 - Rule accuracy ####
DiST.m.rule$Rule.combined=NA
for(s in 1:nrow(DiST.m.rule)){
  ruleItems = colnames(
    DiST.m.rule %>% 
      select(
        any_of(
          unique(
            c(
              DiST.m.base.screener.byGrade[[DiST.m.rule$Grade[s]-5]]
              , DiST.m.rule.screener.byGrade[[DiST.m.rule$Grade[s]-5]]
            )
          )
        )
      )
    )
  DiST.m.rule$Rule.combined[s] = 
  rowSums(
    DiST.m.rule[s
                , ruleItems
    ]
  )
}
table(DiST.m.rule$Grade,DiST.m.rule$Rule.combined, DiST.m.rule$sub20p.rule.tot)

# of items would vary by grade now, since there are different numbers of overlap-
# ping items per grade
# Grade Items   Threshold   FPR
# 6     13      5            4.9%
# 7     12      6            9.4%**
# 8     11      6           14.8%
# 9     12      7           14.7%**
# 10    12      8            5.0%
# 11    11      7            0.0%++

## ** indicates scales where this improves on the original set of 10 rule items
# items. In the other cases, it makes things worse. ++ means no difference

# OK, so for Grades 7 and 9, using the full set of "rule words" and "base 
# words" seems to help. For the others, we need different sets.

#### OK, so now what. Oof.

# 10.0 Output ####
DiST.m.base.screener.byGrade %>% data.frame %>% 
  write.csv(file = "DiST.m.Nonwords.csv", row.names=F)
DiST.m.rule.screener.byGrade %>% data.frame %>% 
  write.csv(file = "DiST.m.Morphology.csv", row.names=F)
DiST.i.screener.byGrade %>% data.frame %>% 
  write.csv(file = "DiST.i.Irregs.csv", row.names=F)
