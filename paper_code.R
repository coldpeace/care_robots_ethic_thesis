####################################################################################################
# Aurthor: Mehdi Fekari 
# Cultural and Moral Acceptance of Care Robots' Decisions for Elderly Populations
# Student: El Mehdi Fekari
# Matriculation Nr.: 03716939
# Context: Master Thesis for the Master Program Politics & Technology (Part-time)
#####################################################################################################

install.packages("tidyr")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("Hmisc")
install.packages('huxtable')
install.packages("jtools")

library(tidyr)
library(ggplot2)
library(grid)
library(stargazer)
library(plm)
library(foreign)
library(Hmisc)
library(huxtable)
library(jtools)

###################################################
# Data Manipulation 
###################################################

setwd("/Users/mehdi/Desktop/Studium/TUM/MA/care_robots_survey/")
df = read.csv("data/data_08102022_txt.csv")
df
colnames(df)

#rename columns
names(df)[names(df) == 'Q12_1'] <- 'Q_Nat'
names(df)[names(df) == 'Q14'] <- 'Q_Age'
names(df)[names(df) == 'Q15'] <- 'Q_Gen'
names(df)[names(df) == 'Q20'] <- 'Q_Edu'
names(df)[names(df) == 'Q31'] <- 'Q_Act'
# Q16_1 -> Q16_11: Multi Question regarding attitude towards care robots
# Q18_1 -> Q18_3: Ranking actions based on privacy
names(df)[names(df) == 'Q18_1'] <- 'Q_Pub_Auth_Priv_Order'
names(df)[names(df) == 'Q18_2'] <- 'Q_Fam_Doc_Priv_Order'
names(df)[names(df) == 'Q18_3'] <- 'Q_Fam_Mem_Priv_Order'

names(df)[names(df) == 'Q17'] <- 'Q_Med_Emg'
names(df)[names(df) == 'Q18'] <- 'Q_Beh_Emg'
names(df)[names(df) == 'Q16'] <- 'Q_Soc_Emg'

names(df)[names(df) == 'condition'] <- 'Med_Condition'
names(df)[names(df) == 'b_condition'] <- 'Beh_Condition'
names(df)[names(df) == 'c_condition'] <- 'Soc_Condition'

colnames(df)

# Reduced df with selected columns
df_sub = df[c("EndDate","UserLanguage","Q_Nat", "Q_Age", "Q_Gen", "Q_Edu", "Q_Act",
              "Q_Pub_Auth_Priv_Order", "Q_Fam_Doc_Priv_Order", "Q_Fam_Mem_Priv_Order",
              "Q_Med_Emg", "Q_Beh_Emg", "Q_Soc_Emg", "Med_Condition", "Beh_Condition","Soc_Condition" )]

colnames(df_sub)

levels(df_sub$Q_Med_Emg)
# remove first two rows (Question ,ID)
df_sub = df_sub[-c(1,2),]

# remove empty rows
df_sub <- df_sub[-which(df_sub$Q_Med_Emg == ""
                          | df_sub$Q_Beh_Emg  == ""
                          | df_sub$Q_Soc_Emg == ""), ]

# Clean levels: remove unused
df_sub <- droplevels(df_sub)

# WARNIN: 3th and 4th is not consistent!
# Some data is not properly set in Q_Med_Emg: 6 instead of 5! Let's fix it:
# first convert as nubmer
#df_sub$Q_Med_Emg <- as.numeric(df_sub$Q_Med_Emg)
# change value
#df_sub$Q_Med_Emg[df_sub$Q_Med_Emg == 6] <- 5
# wieder als factor
#df_sub$Q_Med_Emg <- factor(df_sub$Q_Med_Emg)

# Levels before
df_sub$Q_Med_Emg = as.factor(df_sub$Q_Med_Emg)
df_sub$Q_Beh_Emg = as.factor(df_sub$Q_Beh_Emg)
df_sub$Q_Soc_Emg = as.factor(df_sub$Q_Soc_Emg)
levels(df_sub$Q_Med_Emg)
#levels(df_sub$Q_Beh_Emg)
#levels(df_sub$Q_Soc_Emg)

actors_factor = c("Public health authorities",
                  "Family doctor",
                  "Family member",
                  "None",
                  "Other")

#levels(df_sub$Q_Med_Emg) = actors_factor
#levels(df_sub$Q_Beh_Emg) = actors_factor
#levels(df_sub$Q_Soc_Emg) = actors_factor

##########################
# Add nationality label  # NOT NEEDED FOR TEXT DATA
##########################

# Add missing a nationality from a participant
df_sub[which(df_sub$Q_Nat == ''), ]$Q_Nat = 'Egyptian'

#install.packages("forcats")
library(forcats)
library("ggplot2")
p = ggplot(data.frame(df_sub$Q_Nat), aes(y= fct_infreq(df_sub$Q_Nat))) + geom_bar()
p = p + labs(x = "Number of participants", y = "Nationalities")
p + scale_x_discrete(limits=0:28)

##########################
# Female vs Male         #
##########################

gen_factors_level = c("Not Specified",
                   "Female",
                  "Male")

levels(df_sub$Q_Gen) = gen_factors_level

p = ggplot(data.frame(df_sub$Q_Gen), aes(x = fct_infreq(df_sub$Q_Gen))) + geom_bar(stat="count")
p = p + labs(y = "Number of participants", x = "Gender")
p

############ Male/Female extraction

df_male = df_sub[which(df_sub$Q_Gen == "Male"),]
df_female = df_sub[which(df_sub$Q_Gen == "Female"),]

nrow(df_male)
nrow(df_female)

##############################################################################
#
# General Attitude Questions
#
##############################################################################

library(dplyr)

general_atd = df %>% dplyr::select(Q16_1:Q16_11, Q_Nat, Q_Gen, EndDate)
colnames(general_atd)
dim(general_atd)
head(general_atd, 7)
general_atd[1,]
# First 6 row are not relevant. Let's remove it
general_atd = general_atd[-c(1,2,3,4,5,6),]
head(general_atd)
dim(general_atd)
# # Remove empty and non numeric data
general_atd <- general_atd[-which(general_atd$Q16_1 == ""
                     | general_atd$Q16_2 == ""
                     | general_atd$Q16_3 == ""
                     | general_atd$Q16_4 == ""
                     | general_atd$Q16_5 == ""
                     | general_atd$Q16_6 == ""
                     | general_atd$Q16_7 == ""
                     | general_atd$Q16_8 == ""
                     | general_atd$Q16_9 == ""
                     | general_atd$Q16_10 == ""
                     | general_atd$Q16_11 == ""), ]

#gen_atd <- gen_atd[!apply(is.na(gen_atd) | gen_atd == "", 1, all),]

### Rework factor levels

#drop unused factor levels
general_atd <- droplevels(general_atd)

as.numeric(general_atd$Q16_1)
# 2 4 3 ... 1 4 2 3
general_atd$Q16_1
# Somewhat agree, Strongly agree, Somewhat disagree, ..., 
# ..., Neither agree nor disagree, Strongly agree, Somewhat agree, Somewhat disagree

#numeric order
# 1: Neither agree nor disagree
# 2: Somewhat agree
# 3: Somewhat disagree
# 4: Strongly agree
# 5: Strongly disagree

levels(general_atd$Q16_1)

oderder_factors = c("Strongly disagree", 
                    "Somewhat disagree", 
                    "Neither agree nor disagree",
                    "Somewhat agree",
                    "Strongly agree")

general_atd$Q16_1 <- ordered(general_atd$Q16_1, levels = oderder_factors)
levels(general_atd$Q16_1)

general_atd$Q16_1
as.numeric(general_atd$Q16_1)
# Somewhat agree, Strongly agree, Somewhat disagree, ...
# Neither agree nor disagree, Strongly agree, Somewhat agree, Somewhat disagree

general_atd$Q16_2 <- ordered(general_atd$Q16_2, levels = oderder_factors)
general_atd$Q16_3 <- ordered(general_atd$Q16_3, levels = oderder_factors)
general_atd$Q16_4 <- ordered(general_atd$Q16_4, levels = oderder_factors)
general_atd$Q16_5 <- ordered(general_atd$Q16_5, levels = oderder_factors)
general_atd$Q16_6 <- ordered(general_atd$Q16_6, levels = oderder_factors)
general_atd$Q16_7 <- ordered(general_atd$Q16_7, levels = oderder_factors)
general_atd$Q16_8 <- ordered(general_atd$Q16_8, levels = oderder_factors)
general_atd$Q16_9 <- ordered(general_atd$Q16_9, levels = oderder_factors)
general_atd$Q16_10 <- ordered(general_atd$Q16_10, levels = oderder_factors)
general_atd$Q16_11 <- ordered(general_atd$Q16_11, levels = oderder_factors)

nrow(general_atd)
colnames(general_atd)
headings = c("I am interested in using care robots",
            "There are many beneficial applications of care robots",
            "Care robots can provide new economic opportunities",
            "Care robots can help people feel happier",
            "Care robots can perform better than humans",
            "Much of society will benefit from care robots",
            "For routine care services, I would rather interact with a robot than with a human",
            "I think care robots are dangerous",
            "People like me will suffer if care robots are used more and more in society",
            "I shiver with discomfort when I think about future uses of care robots",
            "Care robots are used to spy on people",
            "Nationality",
            "Gender",
            "EndDate")

names(general_atd) = headings
colnames(general_atd)

# Plotting likert

install.packages('likert')
library(likert)
# All participant regarldess nationality and sex/gender
# general_atd_all = select(general_atd, -Nationality, -Gender)
p = likert(general_atd[,1:11])
likert.options(centered = TRUE)
plot(p) + ggtitle("General Attitude towards Care Robots")

# Participant by sex/gender
p = likert(general_atd[,8:10], grouping=general_atd_sub$Gender)
plot(p) + ggtitle("General Attitude towards Care Robots per Gender")

# Americans vs German?

general_atd_DE_US <- general_atd[which(general_atd$Nationality == "German"
                                  | general_atd$Nationality == "American"),]


nrow(general_atd[which(general_atd$Nationality == "German"),])

p_DE_US = likert(general_atd_DE_US[,8:11], grouping=general_atd_DE_US$Nationality)
plot(p_DE_US) + ggtitle("General Attitude towards Care Robots DE vs US")

#######################

# Set General Attitude

#######################

#general_atd$`I am interested in using care robots`
#as.numeric(general_atd$`I am interested in using care robots`) -3 

# Get private attitude score towards robots
getModifieIdIndex <- function(x) {
  return(as.numeric(x) - 3) # 3 is center ('neither agree or disagree')
}

getAttitudePosStatement <- function() 
{
  pos_atd = getModifieIdIndex(general_atd$`There are many beneficial applications of care robots`)
  pos_atd = pos_atd + getModifieIdIndex(general_atd$`Care robots can provide new economic opportunities`) 
  pos_atd = pos_atd + getModifieIdIndex(general_atd$`Care robots can help people feel happier`)
  pos_atd = pos_atd + getModifieIdIndex(general_atd$`Much of society will benefit from care robots`)
  return (pos_atd)
}

getAttitudeNegStatement <- function() 
{
  neg_atd = getModifieIdIndex(general_atd$`I think care robots are dangerous`) 
  neg_atd = neg_atd + getModifieIdIndex(general_atd$`People like me will suffer if care robots are used more and more in society`)
  neg_atd = neg_atd + getModifieIdIndex(general_atd$`I shiver with discomfort when I think about future uses of care robots`)
  neg_atd = neg_atd + getModifieIdIndex(general_atd$`Care robots are used to spy on people`)
  return (neg_atd)
}

getPrivAttitude <- function()
{
  return(getAttitudePosStatement() - getAttitudeNegStatement())
}

general_atd$atd_score = getPrivAttitude()
general_atd$atd_score_pos_stat = getAttitudePosStatement()
general_atd$atd_score_neg_stat = - getAttitudeNegStatement()

head(general_atd)

general_atd$atd_score
# Add to sub_df
df_sub$atd_score = NA
df_sub$atd_score_pos_stat = NA
df_sub$atd_score_neg_stat = NA

# Populate Minkov and Hofstede Insights values
for(i in 1:nrow(general_atd)) {       # for-loop over rows
  for (j in 1:nrow(df_sub))
    if (general_atd$EndDate[i] == df_sub$EndDate[j])
    {
      df_sub$atd_score[j] = general_atd$atd_score[i]
      df_sub$atd_score_pos_stat[j] = general_atd$atd_score_pos_stat[i]
      df_sub$atd_score_neg_stat[j] = general_atd$atd_score_neg_stat[i]
      break
    }
}

## ANOVA General Attitude
#NOTE! FIRST RUN: # Set IDV levels section to get Minkob index

summary(df_sub$atd_score)
var(df_sub$atd_score, na.rm=TRUE)
sd(df_sub$atd_score, na.rm=TRUE)


summary(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='Low'], na.rm=TRUE)
var(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='Low'], na.rm=TRUE)
sd(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='Low'], na.rm=TRUE)

summary(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='Medium'], na.rm=TRUE)
var(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='Medium'], na.rm=TRUE)
sd(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='Medium'], na.rm=TRUE)

summary(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='High'], na.rm=TRUE)
var(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='High'], na.rm=TRUE)
sd(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='High'], na.rm=TRUE)

summary(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='Very High'], na.rm=TRUE)
var(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='Very High'], na.rm=TRUE)
sd(df_sub$atd_score[as.character(df_sub$Minkov_IDV_index_group) =='Very High'], na.rm=TRUE)

df_sub %>% aov(atd_score ~ Minkov_IDV_index_group, data = .) %>% summary()
df_sub %>% aov(atd_score ~ Minkov_IDV_index_group, data = .) %>% TukeyHSD()
df_sub %>% aov(atd_score_pos_stat ~ Minkov_IDV_index_group, data = .) %>% summary()
df_sub %>% aov(atd_score_pos_stat ~ Minkov_IDV_index_group, data = .) %>% TukeyHSD() # %>% plot()
df_sub %>% aov(atd_score_neg_stat ~ Minkov_IDV_index_group, data = .) %>% summary()
df_sub %>% aov(atd_score_neg_stat ~ Minkov_IDV_index_group, data = .) %>% TukeyHSD() # %>% plot()

summary(df_sub$atd_score)

df_sub %>% dplyr::select(atd_score) %>% t.test(mu = 3.5)

p <- ggplot(df_sub[which(!is.na(df_sub$Minkov_IDV_index_group)),], aes(x=Minkov_IDV_index_group, y=atd_score, na.rm = TRUE)) +
  geom_boxplot(alpha=0.7) +
  labs(x = "Minlkov IDV index Group") +
  labs(y = "General Attitude Score") +
  stat_summary(fun=mean, geom="point", shape=3, size=2, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p

#Concret Questions

# Check only low and very high
low_very_high = df_sub[which(as.character(df_sub$Minkov_IDV_index_group) =='Very High' 
                             | as.character(df_sub$Minkov_IDV_index_group) =='Medium'), ]

df_sub[which(as.character(df_sub$Minkov_IDV_index_group) =='Medium'), ]$Q_Nat

low_very_high %>% aov(atd_score ~ Minkov_IDV_index_group, data = .) %>% summary()
# Df Sum Sq Mean Sq F value Pr(>F)  
# Minkov_IDV_index_group  1  199.1  199.07    5.63   0.02 *
# Residuals              83 2934.5   35.36 

# Later do the same for DE vs US
summary(df_sub$atd_score[as.character(df_sub$Q_Nat) =='American'], na.rm=TRUE)
sd(df_sub$atd_score[as.character(df_sub$Q_Nat) =='American'], na.rm=TRUE)

summary(df_sub$atd_score[as.character(df_sub$Q_Nat) =='German'], na.rm=TRUE)
sd(df_sub$atd_score[as.character(df_sub$Q_Nat) =='German'], na.rm=TRUE)

df_sub_DE_US <- df_sub[which(df_sub$Q_Nat == "German"
                                       | df_sub$Q_Nat == "American"),]

summary(df_sub[which(df_sub$Q_Nat == "American"),]$atd_score_neg_stat)
sd(df_sub[which(df_sub$Q_Nat == "American"),]$atd_score_neg_stat)

p <- ggplot(df_sub_DE_US, aes(x=Q_Nat, y=atd_score, na.rm = TRUE)) +
  geom_boxplot(alpha=0.7) +
  labs(x = "Natioanlity") +
  labs(y = "General Attitude Score") +
  stat_summary(fun=mean, geom="point", shape=3, size=2, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p

df_sub_DE_US %>% aov(atd_score ~ Q_Nat, data = .) %>% summary()
df_sub_DE_US %>% aov(atd_score_pos_stat ~ Q_Nat, data = .) %>% summary()
df_sub_DE_US %>% aov(atd_score_neg_stat ~ Q_Nat, data = .) %>% summary()

###############################
#
# Privacy-Invasive Ordering
#
###############################

names(df_sub)[names(df_sub) == 'Q18_1'] <- 'Q_Pub_Auth_Priv_Order'
names(df_sub)[names(df_sub) == 'Q18_2'] <- 'Q_Fam_Doc_Priv_Order'
names(df_sub)[names(df_sub) == 'Q18_3'] <- 'Q_Fam_Mem_Priv_Order'

# # Remove empty and non numeric data
rank_priv <- df_sub[-which(df_sub$Q_Pub_Auth_Priv_Order == ""
                          | df_sub$Q_Fam_Doc_Priv_Order == ""
                          | df_sub$Q_Fam_Mem_Priv_Order == ""), ]


rank_priv$Prio_contact <- ifelse(rank_priv$Q_Pub_Auth_Priv_Order == "1", 'Public Health Authorities',
                          ifelse(rank_priv$Q_Fam_Mem_Priv_Order == "1", 'Family Memeber', 'Family Doctor'))
                         
# 2d step filter per Gender
rank_priv_gender <- rank_priv[which(rank_priv$Q_Gen == "Female" | rank_priv$Q_Gen == "Male"), ]
rank_priv_de_us <- rank_priv[which(rank_priv$Q_Nat == "German" | rank_priv$Q_Nat == "American"), ]

nrow(rank_priv)

# First 2 row are not relevant. Let's remove it
#head(rank_priv)
#rank_priv <- droplevels(rank_priv)

# prio_table
# Use factor
priv_factors = c("Public Health Authorities", 
                    "Family Doctor", 
                    "Family Memeber")

p_priv = ggplot(data.frame(rank_priv$Prio_contact), aes(x = fct_infreq(rank_priv$Prio_contact)),) + geom_bar(stat="count")
p_priv = p_priv + labs(x = "Least Privacy-Invasive Contact ", y = "Number of paritipants")
p_priv

# Per Gender
p_priv = ggplot(rank_priv_gender, aes(fct_infreq(Prio_contact), fill = Q_Gen)) + geom_bar(stat="count", position = "dodge")
p_priv = p_priv + labs(x = "Least Privacy-Invasive Contact ", y = "Number of paritipants")
p_priv

# Per DE vs AM
p_priv = ggplot(rank_priv_de_us, aes(fct_infreq(Prio_contact), fill = Q_Nat)) + geom_bar(stat="count", position = "dodge")
p_priv = p_priv + labs(x = "Least Privacy-Invasive Contact ", y = "Number of paritipants")
p_priv

data()
df_sub
## ANOVA/POLR

rank_priv_de_us %>% aov(as.numeric(as.factor(Prio_contact)) ~ Q_Nat, data = .) %>% summary()

rank_priv %>% aov(as.numeric(as.factor(rank_priv$Prio_contact)) ~ Minkov_IDV_index_group, data = .) %>% summary()
rank_priv %>% aov(as.numeric(as.factor(rank_priv$Prio_contact)) ~ Minkov_IDV_index_group, data = .) %>% TukeyHSD() # %>% plot()
# TODO: Priv 
###########################################
#
#  Dillema Situations Results
#
###########################################

###### Medical Emergency

# Overal
p_med = ggplot(data.frame(df_sub$Q_Med_Emg), aes(y = fct_infreq(df_sub$Q_Med_Emg)),) + geom_bar(stat="count")
p_med = p_med + labs(y = "Medical Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_med

# Based on Condition
names(df_sub)[names(df_sub) == 'Med_Condition'] <- 'Health_Impact'

p_med = ggplot(df_sub, aes(y = fct_infreq(Q_Med_Emg), fill= Health_Impact)) + geom_bar(stat="count", position = "dodge")
p_med = p_med + labs(y = "Medical Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_med

# Based on Gender

names(df_sub)[names(df_sub) == 'Q_Gen'] <- 'Gender'

df_sub_gender <- df_sub[which(df_sub$Gender == "Female" | df_sub$Gender == "Male"), ]

p_med = ggplot(df_sub_gender, aes(y = fct_infreq(Q_Med_Emg), fill= Gender)) + geom_bar(stat="count", position = "dodge")
p_med = p_med + labs(y = "Medical Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_med

# Based on Nationality DE vs US
df_sub_nat <- df_sub[which(df_sub$Q_Nat == "German" | df_sub$Q_Nat == "American"), ]

# names(df_sub_nat)[names(df_sub_nat) == 'Q_Nat'] <- 'Nationality' 

p_med = ggplot(df_sub_nat, aes(y = fct_infreq(Q_Med_Emg), fill= Q_Nat)) + geom_bar(stat="count", position = "dodge")
p_med = p_med + labs(y = "Medical Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_med

###### Behavioral Emergency

# Overal
p_beh = ggplot(data.frame(df_sub$Q_Beh_Emg), aes(y = fct_infreq(df_sub$Q_Beh_Emg)),) + geom_bar(stat="count")
p_beh = p_beh + labs(y = "Behavioral Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_beh

# Based on Condition
# names(df_sub)[names(df_sub) == 'Med_Condition'] <- 'Health_Impact'

p_beh = ggplot(df_sub, aes(y = fct_infreq(Q_Beh_Emg), fill= Health_Impact)) + geom_bar(stat="count", position = "dodge")
p_beh = p_beh + labs(y = "Behavioral Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_beh

# Based on Gender

#names(df_sub)[names(df_sub) == 'Q_Gen'] <- 'Gender'
#df_sub_gender <- df_sub[which(df_sub$Gender == "Female" | df_sub$Gender == "Male"), ]

p_beh = ggplot(df_sub_gender, aes(y = fct_infreq(Q_Beh_Emg), fill= Gender)) + geom_bar(stat="count", position = "dodge")
p_beh = p_beh + labs(y = "Behavioral Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_beh

# Based on Nationality DE vs US

df_sub_nat <- df_sub[which(df_sub$Q_Nat == "German" | df_sub$Q_Nat == "American"), ]
# names(df_sub_nat)[names(df_sub_nat) == 'Q_Nat'] <- 'Nationality' 

p_beh = ggplot(df_sub_nat, aes(y = fct_infreq(Q_Beh_Emg), fill= Q_Nat)) + geom_bar(stat="count", position = "dodge")
p_beh = p_beh + labs(y = "Behavioral Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_beh

###### Social Emergency

# Overal
p_soc = ggplot(data.frame(df_sub$Q_Soc_Emg), aes(y = fct_infreq(df_sub$Q_Soc_Emg)),) + geom_bar(stat="count")
p_soc = p_soc + labs(y = "Social Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_soc

# Based on Condition

p_soc = ggplot(df_sub, aes(y = fct_infreq(Q_Soc_Emg), fill= Health_Impact)) + geom_bar(stat="count", position = "dodge")
p_soc = p_soc + labs(y = "Social Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_soc

# Based on Gender

p_soc = ggplot(df_sub_gender, aes(y = fct_infreq(Q_Soc_Emg), fill= Gender)) + geom_bar(stat="count", position = "dodge")
p_soc = p_soc + labs(y = "Social Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_soc

# Based on Nationality DE vs US

p_soc = ggplot(df_sub_nat, aes(y = fct_infreq(Q_Soc_Emg), fill= Q_Nat)) + geom_bar(stat="count", position = "dodge")
p_soc = p_soc + labs(y = "Social Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_soc

## ANOVA

# Diff based on IDV index
summary(df_sub$Q_Med_Emg[as.character(df_sub$Minkov_IDV_index_group) =='Low'], na.rm=TRUE)

summary(df_sub$Q_Med_Emg[as.character(df_sub$Minkov_IDV_index_group) =='Medium'], na.rm=TRUE)

summary(df_sub$Q_Med_Emg[as.character(df_sub$Minkov_IDV_index_group) =='High'], na.rm=TRUE)

summary(df_sub$Q_Med_Emg[as.character(df_sub$Minkov_IDV_index_group) =='Very High'], na.rm=TRUE)

# BINGOOO
df_sub %>% aov(as.numeric(df_sub$Q_Med_Emg) ~ Minkov_IDV_index_group, data = .) %>% summary()
df_sub %>% aov(as.numeric(df_sub$Q_Med_Emg) ~ Minkov_IDV_index_group, data = .) %>% TukeyHSD() # %>% plot()

df_sub %>% aov(as.numeric(df_sub$Q_Beh_Emg) ~ Minkov_IDV_index_group, data = .) %>% summary()
df_sub %>% aov(as.numeric(df_sub$Q_Soc_Emg) ~ Minkov_IDV_index_group, data = .) %>% summary()

# Health impact?
df_sub %>% aov(as.numeric(df_sub$Q_Med_Emg) ~ Health_Impact, data = .) %>% summary()
df_sub %>% aov(as.numeric(df_sub$Q_Med_Emg) ~ Health_Impact, data = .) %>% TukeyHSD()

# DE vs US
df_sub_nat %>% aov(as.numeric(df_sub_nat$Q_Soc_Emg) ~ Minkov_IDV_index_group, data = .) %>% summary()
# no statistical significance

########################

# Regression Models

########################

###################

# Set IDV levels

###################

#### Set individualism index (very high, high, medium, low)
setwd("/Users/mehdi/Desktop/Studium/TUM/MA/care_robots_survey/")
col_indexes = read.csv("data/collectivism_indexes.csv")
col_indexes

df_sub$Minkov_index = NA

# Populate Minkov and Hofstede Insights values
for(i in 1:length(df_sub$Q_Nat)) {       # for-loop over rows
  for (j in 1:length(col_indexes$Nat))
    if (df_sub$Q_Nat[i] == col_indexes$Nat[j])
    {
      df_sub$Minkov_index[i] = col_indexes$Minkov_index[j]
      break
    }
}

df_sub$Minkov_index[df_sub$Minkov_index == 0] = NA

# Approximate countries with NA score
df_sub$Q_Nat[is.na(df_sub$Minkov_index)]

df_sub$Minkov_index[df_sub$Q_Nat == 'Moroccan'] = df_sub$Minkov_index[df_sub$Q_Nat =='Egyptian']
df_sub$Minkov_index[df_sub$Q_Nat == 'Tunisian'] = df_sub$Minkov_index[df_sub$Q_Nat =='Egyptian']
df_sub$Minkov_index[df_sub$Q_Nat == 'Pakistani'] = df_sub$Minkov_index[df_sub$Q_Nat =='Indian']
df_sub$Minkov_index[df_sub$Q_Nat == 'Bangladeshi'] = df_sub$Minkov_index[df_sub$Q_Nat =='Indian']
df_sub$Minkov_index[df_sub$Q_Nat == 'Nepalese'] = df_sub$Minkov_index[df_sub$Q_Nat =='Indian']
df_sub$Minkov_index[df_sub$Q_Nat == 'Ghanaian'] = df_sub$Minkov_index[df_sub$Q_Nat =='Nigerian']
df_sub$Minkov_index[df_sub$Q_Nat == 'Armenian'] = df_sub$Minkov_index[df_sub$Q_Nat =='Turkish']
df_sub$Minkov_index[df_sub$Q_Nat == 'Belarusian'] = df_sub$Minkov_index[df_sub$Q_Nat =='Russian']
df_sub$Minkov_index[df_sub$Q_Nat == 'Bulgarian'] = -19 # Romania Minkov index
df_sub$Minkov_index[df_sub$Q_Nat == 'Barbadian'] = -95 # Venezuella Minkov index
df_sub$Minkov_index[df_sub$Q_Nat == 'Irish'] = 93 # Venezuella Minkov index
# Clustering into Quartiles
df_sub$Minkov_IDV_index_group = ntile(df_sub$Minkov_index, 4)

df_sub$Minkov_IDV_index_group = as.factor(df_sub$Minkov_IDV_index_group)

idv_factors_level = c("Low",
                      "Medium",
                      "High",
                      "Very High")

levels(df_sub$Minkov_IDV_index_group) = idv_factors_level

# Show General attitude per Cluster

general_atd$Minkov_IDV_index = NA
general_atd$Minkov_IDV_index_group = NA
general_atd$Minkov_IDV_index_group = as.factor(general_atd$Minkov_IDV_index_group)

# Populate Minkov and Hofstede Insights values
for(i in 1:length(general_atd$Nationality)) {       # for-loop over rows
  for (j in 1:length(df_sub$Q_Nat))
    if (general_atd$Nationality[i] == df_sub$Q_Nat[j])
    {
      general_atd$Minkov_IDV_index[i] = df_sub$Minkov_index[j]
      break
    }
}

general_atd$Minkov_IDV_index_group = ntile(general_atd$Minkov_IDV_index, 4)

general_atd_idv <- general_atd[-which(is.na(general_atd$Minkov_IDV_index_group)), ]

idv_factors_level = c("Low",
                      "Medium",
                      "High",
                      "Very High")

general_atd_idv$Minkov_IDV_index_group = as.factor(general_atd_idv$Minkov_IDV_index_group)
levels(general_atd_idv$Minkov_IDV_index_group) = idv_factors_level

general_atd_idv$Minkov_IDV_index_group

p_idv = likert(general_atd_idv[,4:8], grouping=general_atd_idv$Minkov_IDV_index_group)
plot(p_idv) + ggtitle("General Attitude towards Care Robots per IDV index")


# Privacy Ranking

rank_priv <- df_sub[-which(df_sub$Q_Pub_Auth_Priv_Order == ""
                           | df_sub$Q_Fam_Doc_Priv_Order == ""
                           | df_sub$Q_Fam_Mem_Priv_Order == ""), ]

rank_priv$Prio_contact <- ifelse(rank_priv$Q_Pub_Auth_Priv_Order == "1", 'Public Health Authorities',
                                 ifelse(rank_priv$Q_Fam_Mem_Priv_Order == "1", 'Family Memeber', 'Family Doctor'))


rank_priv_idv =  rank_priv[-which(is.na(rank_priv$Minkov_IDV_index_group)), ]
p_priv = ggplot(rank_priv_idv, aes(fct_infreq(Prio_contact), fill = Minkov_IDV_index_group)) + geom_bar(stat="count", position = "dodge")
p_priv = p_priv + labs(x = "Least Privacy-Invasive Contact ", y = "Number of paritipants")
p_priv


# Dilemma Situation

df_sub_idv = df_sub[-which(is.na(df_sub$Minkov_IDV_index_group)), ]

p_med = ggplot(df_sub_idv, aes(y = fct_infreq(Q_Med_Emg), fill = Minkov_IDV_index_group)) + geom_bar(stat="count", position = "dodge")
p_med = p_med + labs(y = "Medical Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_med

p_beh = ggplot(df_sub_idv, aes(y = fct_infreq(Q_Beh_Emg), fill = Minkov_IDV_index_group)) + geom_bar(stat="count", position = "dodge")
p_beh = p_beh + labs(y = "Behavioral Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_beh

p_soc = ggplot(df_sub_idv, aes(y = fct_infreq(Q_Soc_Emg), fill = Minkov_IDV_index_group)) + geom_bar(stat="count", position = "dodge")
p_soc = p_soc + labs(y = "Social Emergency: Who shall the robot alarm? ", x = "Number of responses")
p_soc

#####################

# Set Privacy levels

#####################

# Another way
df_sub$Priv_index_med = NA
df_sub$Priv_index_beh = NA
df_sub$Priv_index_soc = NA

getPrivScore <- function(selected_actor, pb_auth_index, fam_doc_index, fam_mem_index)
{
  order = -1
  if (as.numeric(selected_actor) == 4) # 'Public health authorities (e.g. emergency number)'
    order = pb_auth_index
  else if (as.character(selected_actor) == 'The family doctor') # family doctor
    order = fam_doc_index
  else if (as.character(selected_actor) == 'A family member') # family member
    order = fam_mem_index
  else if (as.character(selected_actor) == 'No one') # None
    return(4) # higher score
  else # Other:
    order = -1
  
  if (order == 1)
    return (3)
  else if (order == 2)
    return(2)
  else if (order == 3)
    return (1)
  else
    return (NA)
}

df_sub$Q_Med_Emg
as.numeric(df_sub$Q_Med_Emg)

# Public health authorities (e.g emergency number) = 4
#as.numeric(df_sub$Q_Beh_Emg)
# Public health authorities (e.g emergency number) = 4
#as.numeric(df_sub$Q_Soc_Emg)
# Public health authorities (e.g emergency number) = 4

for(i in 1:nrow(df_sub)) {       # for-loop over rows
  df_sub$Priv_index_med[i] = getPrivScore (df_sub$Q_Med_Emg[i], 
                                           df_sub$Q_Pub_Auth_Priv_Order[i],
                                           df_sub$Q_Fam_Doc_Priv_Order[i],
                                           df_sub$Q_Fam_Mem_Priv_Order[i])
}

for(i in 1:nrow(df_sub)) {       # for-loop over rows
  df_sub$Priv_index_beh[i] = getPrivScore (df_sub$Q_Beh_Emg[i],
                                           df_sub$Q_Pub_Auth_Priv_Order[i],
                                           df_sub$Q_Fam_Doc_Priv_Order[i],
                                           df_sub$Q_Fam_Mem_Priv_Order[i])
}

for(i in 1:nrow(df_sub)) {       # for-loop over rows
  df_sub$Priv_index_soc[i] = getPrivScore (df_sub$Q_Soc_Emg[i], 
                                           df_sub$Q_Pub_Auth_Priv_Order[i],
                                           df_sub$Q_Fam_Doc_Priv_Order[i],
                                           df_sub$Q_Fam_Mem_Priv_Order[i])
}

df_sub$Priv_index_med = as.factor(df_sub$Priv_index_med)
df_sub$Priv_index_beh = as.factor(df_sub$Priv_index_beh)
df_sub$Priv_index_soc = as.factor(df_sub$Priv_index_soc)

priv_factors_level = c("Low",
                       "Medium",
                       "High",
                       "Very High")

levels(df_sub$Priv_index_med) = priv_factors_level
levels(df_sub$Priv_index_beh) = priv_factors_level
levels(df_sub$Priv_index_soc) = priv_factors_level

###################################################################
# First ordinal regression model using Minkov index based grouping
###################################################################

# Use linear regression

# Order levels
df_sub$Priv_index_med = as.ordered(df_sub$Priv_index_med)
df_sub$Priv_index_beh = as.ordered(df_sub$Priv_index_beh)
df_sub$Priv_index_soc = as.ordered(df_sub$Priv_index_soc)
df_sub$Minkov_IDV_index_group = as.ordered(df_sub$Minkov_IDV_index_group)
df_sub$Q_Age = as.ordered(df_sub$Q_Age)


# Cross check we have enough data
xtabs(~Priv_index_med + Minkov_IDV_index_group, df_sub)
xtabs(~Priv_index_beh + Minkov_IDV_index_group, df_sub)
xtabs(~Priv_index_soc + Minkov_IDV_index_group, df_sub)

# Ordinal Logistic Regression/Proportional Odds Logistic Regression
#install.packages('MASS')
library(MASS)
# Order Education level
df_sub$Q_Edu
oderder_factors = c("Less than a High School Diploma",
                    "High School Diploma", 
                    "Vocational/Professional Training", 
                    "Attended College",
                    "Bachelor Degree",
                    "Master Degree",
                    "Doctoral Degree/PhD",
                    "Please Specify:")


df_sub$Q_Edu = ordered(df_sub$Q_Edu, levels = oderder_factors)

# Order servity 
df_sub$Health_Impact = as.factor(df_sub$Health_Impact)
df_sub$Beh_Condition= as.factor(df_sub$Beh_Condition)
df_sub$Soc_Condition= as.factor(df_sub$Soc_Condition)

df_sub$Health_Impact
oderder_factors = c("mild anxiety",
                    "migraine", 
                    "severe depression")
df_sub$Health_Impact = ordered(df_sub$Health_Impact, levels = oderder_factors)
df_sub$Beh_Condition = ordered(df_sub$Beh_Condition, levels = oderder_factors) 
df_sub$Soc_Condition = ordered(df_sub$Soc_Condition, levels = oderder_factors) 

df_sub$Health_Impact
df_sub$Beh_Condition
df_sub$Soc_Condition

# Medical Case

model_med_1 <- polr(Priv_index_med ~ as.numeric(Minkov_IDV_index_group)
                  , df_sub, na.action = na.omit, method = 'logistic')
model_med_2 <- polr(Priv_index_med ~ as.numeric(Minkov_IDV_index_group)
                    + as.numeric(Q_Age)
                    , df_sub, na.action = na.omit, method = 'logistic')
model_med_3 <- polr(Priv_index_med ~ as.numeric(Minkov_IDV_index_group)
                    + as.numeric(Q_Age)
                    + as.numeric(Q_Edu)
                    , df_sub, na.action = na.omit, method = 'logistic')
model_med <- polr(Priv_index_med ~ as.numeric(Minkov_IDV_index_group)
                  + as.numeric(Q_Age)
                  + as.numeric(Q_Edu)
                  + as.numeric(Health_Impact)
                  , df_sub, na.action = na.omit, method = 'logistic')

summary(model_med)
# Get P-Value
#install.packages("AER")
#install.packages("lmtest")
library(MASS)
library(lmtest)
coeftest(model_med)

# Behavioral Case
model_beh_1 <- polr(Priv_index_beh ~ as.numeric(Minkov_IDV_index_group)
                  , df_sub, na.action = na.omit, method = 'logistic')
model_beh_2 <- polr(Priv_index_beh ~ as.numeric(Minkov_IDV_index_group)
                  + as.numeric(Q_Age)
                  , df_sub, na.action = na.omit, method = 'logistic')
model_beh_3 <- polr(Priv_index_beh ~ as.numeric(Minkov_IDV_index_group)
                  + as.numeric(Q_Age)
                  + as.numeric(Q_Edu)
                  , df_sub, na.action = na.omit, method = 'logistic')
model_beh <- polr(Priv_index_beh ~ as.numeric(Minkov_IDV_index_group)
                  + as.numeric(Q_Age)
                  + as.numeric(Q_Edu)
                  + as.numeric(Beh_Condition)
                  , df_sub, na.action = na.omit, method = 'logistic')

summary(model_beh)
coeftest(model_beh)

# Social Case
model_soc_1 <- polr(Priv_index_soc ~ as.numeric(Minkov_IDV_index_group)
                  , df_sub, na.action = na.omit, method = 'logistic')
model_soc_2 <- polr(Priv_index_soc ~ as.numeric(Minkov_IDV_index_group)
                  + as.numeric(Q_Age)
                  , df_sub, na.action = na.omit, method = 'logistic')
model_soc_3 <- polr(Priv_index_soc ~ as.numeric(Minkov_IDV_index_group)
                  + as.numeric(Q_Age)
                  + as.numeric(Q_Edu)
                  , df_sub, na.action = na.omit, method = 'logistic')
model_soc <- polr(Priv_index_soc ~ as.numeric(Minkov_IDV_index_group)
                  + as.numeric(Q_Age)
                  + as.numeric(Q_Edu)
                  + as.numeric(Soc_Condition)
                  , df_sub, na.action = na.omit, method = 'logistic')

summary(model_soc)
coeftest(model_soc)

# Robustness check?

# Generates tables
#library(apaTables)
#install.packages('apaTables')
#install.packages("stargazer")
library(stargazer)

stargazer(model_med_1, model_med_2, model_med_3, model_med, type = 'html')
stargazer(model_beh_1, model_beh_2, model_beh_3, model_beh, type = 'html')
stargazer(model_soc_1, model_soc_2, model_soc_3, model_soc, type = 'html')


## ANOVA
# gen_atd is not a control variable! it's a caclulated index! to interpret design not to include it...
as.numeric(df_sub$Priv_index_med)

summary(as.numeric(df_sub$Priv_index_med))

df_sub %>% aov(as.numeric(Priv_index_med) ~ Minkov_IDV_index_group, data = .) %>% summary()
df_sub %>% aov(as.numeric(Priv_index_beh) ~ Minkov_IDV_index_group, data = .) %>% summary()
df_sub %>% aov(as.numeric(Priv_index_soc) ~ Minkov_IDV_index_group, data = .) %>% summary()

# Polr (ind parmt -> as numeric!)
# ANOVA (dep as numeric, ind parmt as categories) Passt better
# The analysis of variance, or ANOVA, 
# is among the most popular methods for analyzing how an outcome variable differs between groups,
# for example, in observational studies or in experiments with different conditions.

# Thus, ANOVA can be considered as a case of a linear regression in which all predictors are categorical.
# https://www.statsimprove.com/en/what-is-the-difference-between-anova-and-regression-and-which-one-to-choose/


#Apa tables
anov_model_med = aov(as.numeric(Priv_index_med) ~ Minkov_IDV_index_group, data = df_sub)
anov_model_beh = aov(as.numeric(Priv_index_beh) ~ Minkov_IDV_index_group, data = df_sub)
anov_model_soc = aov(as.numeric(Priv_index_soc) ~ Minkov_IDV_index_group, data = df_sub)

#install.packages('apaTables')
library(apaTables)
#install.packages('MBESS')
library(MBESS)

apa.aov.table(anov_model_med,filename="anova_med_table.doc")
apa.aov.table(anov_model_beh,filename="anova_beh_table.doc")
apa.aov.table(anov_model_soc,filename="anova_soc_table.doc")
