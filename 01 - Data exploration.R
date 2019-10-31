#--------------------------------------------------------------------------------------------------
#iLab1 code - Propensity scoring
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
#Data load and preliminary EDA
#--------------------------------------------------------------------------------------------------

#Library calls
library(tidyverse)
library(data.table)
library(lubridate)
library(DataExplorer)
library(corrplot)
library(sqldf)


#setwd("D:/UTS/MDSI/Sem 02/iLab 1/Data/Raw files")

#Read relevant files from the folder

const <- fread("dimConstituent_UTS.csv")
const_role <- fread("DimConstituentRole.csv")
inst_detail <- fread("DimInstanceDetail.csv")
inst <- fread("FactInstance_UTS.csv")
payment <- fread("FactPayment_UTS.csv")
iter <- fread("DimIteration.csv")
comm_pref <- fread("FactConstituentCommunicationPreference_UTS.csv")
appeals <- fread("FactAppealContactHistory_UTS.csv")

#Basic file exploration - column names and sample values

#Constituent table

View(colnames(dimConstituent))
head(dimConstituent)

#ConstituentRole table

View(colnames(dimConstituentRole))
head(dimConstituentRole)

#DimInstanceDetail

View(colnames(dimInstanceDetail))
head(dimInstanceDetail)

#factInstance table

View(colnames(factInstance))
head(factInstance)

#factPayment table

View(colnames(factPayment))
head(factInstance)


#--------------------------------------------------------------------------------------------------
#MasterConstituentId
nrow(const %>% group_by(MasterConstituentId) %>% summarise(count = n()) %>% filter(count > 1))

dups <- const %>% 
  group_by(MasterConstituentId) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) #23798 dups

dups <- dups %>% 
  inner_join(const, by = "MasterConstituentId") #23798 corresponds to 48984 rows

table(dups$ConstituentStatusCode)
#Active Duplicate  Inactive   Unknown 
#24085     24798        99         2

#ConstituentSubRegion, Region and LGA
subRegion_counts <- const %>% 
  group_by(ConstituentSubRegion) %>% 
  summarise(count = n_distinct(MasterConstituentId)) %>% 
  arrange(-count)

region_counts <- const %>% 
  group_by(ConstituentRegion) %>% 
  summarise(count = n_distinct(MasterConstituentId)) %>% 
  arrange(-count)

LGA_counts <- const %>% 
  group_by(LGA) %>% 
  summarise(count = n_distinct(MasterConstituentId)) %>% 
  arrange(-count)

subregion_to_region <- const %>%
  group_by(ConstituentSubRegion, ConstituentRegion) %>%
  summarise(count = n()) %>%
  arrange(ConstituentSubRegion, -count) #Subregion to Region mapping isn't unique

LGA_to_region <- const %>% 
  group_by(LGA, ConstituentRegion) %>% 
  summarise(count = n()) %>%
  arrange(LGA, - count) #LGA to Region mapping isn't unique

LGA_to_subregion <- const %>% 
  group_by(LGA, ConstituentSubRegion) %>% 
  summarise(count = n()) %>%
  arrange(LGA, - count) #LGA to SubRegion mapping isn't unique


#Title
title <- const %>% 
  group_by(Title) %>%
  summarise(count = n()) %>%
  arrange(-count) #NULLs present; Miss and Ms separate


#Gender
gender <- const %>%
  group_by(Gender) %>%
  summarise(count = n()) %>%
  arrange(-count) #Lots of Unknown


#Titles of Unknown gender
unknown_gender_titles <- const %>%
  filter(!(Gender %in% c("Male", "Female"))) %>%
  group_by(Title) %>%
  summarise(count = n()) %>%
  arrange(-count) #Can fill in some missing values from Title


#Aboriginality
aborig <- as.data.frame(table(const$Aboriginality))
colnames(aborig) <- c("Aboriginality", "count")
#Lots of NULL values


#Country of Birth
birth_country <- const %>%
  group_by(CountryOfBirth) %>%
  summarise(count = n()) %>%
  arrange(-count) #Majority NULL values


#Under 18
table(const$Under18)
#No        Yes 
#3057776   37691

#Deceased
table(const$Deceased)
#No        Yes 
#3034505   60962

#Deceased date
deceased <- const %>%
  filter(Deceased == "No") %>%
  distinct(DeceasedDate)

#Address1
nrow(const %>% distinct(Address1Suburb)) #30197
nrow(const %>% distinct(Address1State)) #2039
nrow(const %>% distinct(Address1Suburb, Address1State)) #41477

View(const %>% group_by(Address1Suburb, Address1State) %>% summarise(count = n()) %>% arrange(Address1Suburb, -count)) #Mess

View(const %>% group_by(Address1State) %>% summarise(count = n()) %>% arrange(-count)) #Lot of clean-up needed

View(const %>% group_by(Address1Postcode) %>% summarise(count = n()) %>% arrange(-count)) #Lot of clean-up needed

View(const %>% group_by(Address1Country) %>% summarise(count = n()) %>% arrange(-count)) #Lot of clean-up needed

#Age
boxplot(const$Age)
table(const$Age)

#Ageband
table(const$AgeBand)
#18-30    31-40    41-50    51-60      60+    Under 18   Unknown 
#156403   167334   155453   129231   183546    29922  2273578


#Philanthropy status
table(const$PhilanthrophyStatus)
#Largely NULL

#--------------------------------------------------------------------------------------------------

#RGAmount
class(inst$RGAmount)

inst$RGAmount <- as.numeric(inst$RGAmount)

summary(inst$RGAmount)
#Median 10, mean 14.6; over 80% NULLs


#RGAnnualAmount
class(inst$RGAnnualAmount)

inst$RGAnnualAmount <- as.numeric(inst$RGAnnualAmount)

summary(inst$RGAnnualAmount)
#Median 120, mean 166; over 80% NULLs

nrow(inst %>% filter(RGAnnualAmount >= 0 | RGAmount >= 0)) #660224

nrow(inst %>% filter(RGAmount >= 0)) #660224

nrow(inst %>% filter(RGAmount == 0)) #317690

nrow(inst %>% filter(RGAnnualAmount > 0 | RGAmount > 0)) #342534

#InstanceAmount
class(inst$InstanceAmount)

summary(inst$InstanceAmount)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max. 
# -1590       2      20     297      96   4500000

nrow(inst %>% filter(InstanceAmount <= 0)) #888755

boxplot(inst$InstanceAmount)

nrow(inst %>% filter(InstanceAmount > 1000)) #140548

ggplot(inst %>% filter(InstanceAmount <= 1000), aes(x = "InstanceAmount", y = InstanceAmount)) + geom_boxplot()

summary(inst$InstanceAmount[inst$InstanceAmount <= 1000])

# Min.      1st Qu.   Median     Mean   3rd Qu.     Max. 
# -1590.00     0.00    20.00    76.37    65.00      1000.00 


#InstanceRow
class(inst$InstanceRow)

table(inst$InstanceRow)
#All values are 1

#ConstituentIDs
class(inst$PrimaryConstituentID)

inst %>% summarise(pri = n_distinct(PrimaryConstituentID), 
                   indi = n_distinct(IndConstituentID),
                   org = n_distinct(OrgConstituentID))

#pri     indi    org
#1374663 1339984 118363

inst %>% group_by(PrimaryConstituentID) 

#--------------------------------------------------------------------------------------------------
#FactAppealContactHistory table

#Read appeals table
summary(appeals)

head(appeals)

View(colnames(appeals))

appeals$ymd <- ymd(appeals$SendDateSKey)

#Counts at appeal package level
appeal_count <- appeals %>%
  filter(ConstituentId != "Const-2") %>%
  group_by(AppealPackageSKey) %>%
  summarise(count = n()) %>%
  arrange(-count)

#Counts at constituent level
const_count <- appeals %>%
  filter(ConstituentId != "Const-2") %>%
  group_by(ConstituentId) %>%
  summarise(count = n(), 
            latest_info = max(ymd),
            earliest_info = min(ymd),
            diff = (max(ymd) - min(ymd))/365) %>%
  arrange(-count)

#Counts at const, appeal level
contact_count <- appeals %>%
  filter(ConstituentId != "Const-2") %>%
  group_by(AppealPackageSKey, ConstituentId) %>%
  summarise(count = n()) %>%
  arrange(-count)

#AppealContactHistoryRow field
table(appeals$AppealContactHistoryRow) #Only 1s identified so far (is everything 1? Can ignore col in that case)

#AppealResponse field
table(appeals$AppealResponse) #14 different values exist

appeal_response <- appeals %>%
  group_by(AppealResponse) %>%
  summarise(count = n()) %>%
  arrange(-count)
#Mode is Unknown, next highest is unsubscribe

#--------------------------------------------------------------------------------------------------