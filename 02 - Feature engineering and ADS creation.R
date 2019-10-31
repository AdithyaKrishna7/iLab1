#--------------------------------------------------------------------------------------------------
#iLab1 code - Propensity scoring
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
#Feature engineering and creating analytical datasets for different programs 
#--------------------------------------------------------------------------------------------------

#setwd("D:/UTS/MDSI/Sem 02/iLab 1/Data/Raw files")

#Read payment table
payment <- fread("FactPayment_UTS.csv")

payment$DonationDateSKey <- ymd(payment$DonationDateSKey)

#Read iteration table
iteration <- fread("DimIteration.csv")

#Joining iteration with payments to do event level analyses

pay_events <- payment %>%
  left_join(iteration %>% select(IterationSKey, IterationGroupingName, ProgramName), by = c("IterationSKey"))

#Remove the redundant tables and clear memory
rm(payment, iteration)

#Read instance table
instance <- fread("FactInstance_UTS.csv")

#Aggregate at a constituent level to obtain counts of commitments
instance_counts <- instance %>%
  group_by(IndConstituentID) %>%
  summarise(instance_count = n_distinct(InstanceDetailSKey)) %>%
  arrange(-instance_count)

#Remove instance table
rm(instance)

#--------------------------------------------------------------------------------------------------
#Features
#--------------------------------------------------------------------------------------------------

#Distinct events/programs a constituent donated to

dist_events <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  summarise(dist_events = n_distinct(ProgramName)) %>%
  arrange(-dist_events) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2')

#table(dist_events$dist_events) #87% are only one event

#--------------------------------------------------------------------------------------------------
#Tenure of constituent

tenure <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2') %>%
  filter(DonationDateSKey != "9999-12-31") %>%
  group_by(DonorConstituentID) %>%
  summarise(tenure = max(DonationDateSKey) - min(DonationDateSKey)) %>%
  arrange(-tenure)

tenure$tenure <- as.numeric(tenure$tenure)

#nrow(tenure %>% filter(tenure_yrs <= 3))/nrow(tenure) #85% of const. less than 3 yrs

#Join tenure and dist_events
const_base <- sqldf("select a.DonorConstituentID as donor,
                    a.dist_events,
                    b.tenure as tenure
                    from dist_events a
                    left join tenure b
                    on a.DonorConstituentID = b.DonorConstituentID
                    order by a.DonorConstituentID")

#Remove redundant tables and free up memory
rm(tenure, dist_events)

#--------------------------------------------------------------------------------------------------
#Years donated in for constituents

yrs_donated <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  summarise(yrs_donated = n_distinct(year(DonationDateSKey))) %>%
  arrange(-yrs_donated) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2')

#nrow(yrs_donated %>% filter(yrs_donated <= 3))/nrow(yrs_donated) #89%

#Join yrs_donated to const_base
const_base <- sqldf("select a.*,
                    b.yrs_donated
                    from const_base a
                    left join yrs_donated b
                    on a.donor = b.DonorConstituentID
                    order by a.donor")

rm(yrs_donated)

#--------------------------------------------------------------------------------------------------
#Number of credit cards used for payment
credit <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  mutate(card = paste(CreditCardExpirationMonth,"-",CreditCardExpirationYear, sep = "")) %>%
  group_by(DonorConstituentID) %>%
  summarise(dist_cards = n_distinct(card)) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2')

const_base <- sqldf("select a.*,
                    b.dist_cards
                    from const_base a
                    left join credit b
                    on a.donor = b.DonorConstituentID
                    order by a.donor")

rm(credit)

#--------------------------------------------------------------------------------------------------
#Number of events hosted by a constituent

hosted_events <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(PrimaryConstituentID) %>%
  summarise(hosted_events = n()) %>%
  arrange(-hosted_events) %>%
  rename(ConstituentID = PrimaryConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#nrow(hosted_events %>% filter(hosted_events <= 3))/nrow(hosted_events) #65.5%

#Join hosted_events to const_base
const_base <- sqldf("select a.*,
                    b.hosted_events
                    from const_base a
                    left join hosted_events b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(hosted_events)

#--------------------------------------------------------------------------------------------------
#Distinct instances based on InstanceDetailSKey

dist_instance <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  summarise(paid_instances = n_distinct(InstanceDetailSKey)) %>%
  arrange(- paid_instances) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2')

#Join dist_instances to const_base
const_base <- sqldf("select a.*,
                    b.paid_instances
                    from const_base a
                    left join dist_instance b
                    on a.donor = DonorConstituentID
                    order by a.donor")

rm(dist_instance)

#--------------------------------------------------------------------------------------------------
#Number of events a constituent has participated in

participated_events <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  summarise(participated_events = n()) %>%
  arrange(-participated_events) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#nrow(participated_events %>% filter(participated_events <= 3))/nrow(participated_events)

#Join participated_events with const_base
const_base <- sqldf("select a.*,
                    b.participated_events
                    from const_base a
                    left join participated_events b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(participated_events)

#--------------------------------------------------------------------------------------------------
#Most common payment type

pay_type <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID, PaymentMethodType) %>%
  summarise(pay_type = n()) %>%
  arrange(-pay_type) %>%
  filter(row_number() == 1) %>%
  select(-pay_type) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#Join pay_type with const_base

const_base <- sqldf("select a.*,
                    b.PaymentMethodType as pay_type
                    from const_base a
                    left join pay_type b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(pay_type)

#--------------------------------------------------------------------------------------------------
#Donated in last 3 yrs flag

last_3 <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  mutate(last_3 = case_when(max(DonationDateSKey) > 2015 ~ 1,
                            TRUE ~ 0)) %>%
  distinct(DonorConstituentID, last_3) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#Join last_3 with const_base

const_base <- sqldf("select a.*,
                    b.last_3 as last_3
                    from const_base a
                    left join last_3 b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(last_3)

#--------------------------------------------------------------------------------------------------
#Ratio of failed to successful payments

#Obtain count of failed payments
fail <- pay_events %>%
  filter(year(DonationDateSKey) < 2018 & PaymentStatus == "Error") %>%
  group_by(DonorConstituentID, PaymentStatus) %>%
  summarise(fail = n()) %>%
  distinct(DonorConstituentID, fail) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#Obtain count of successful payments
success <- pay_events %>%
  filter(year(DonationDateSKey) < 2018 & PaymentStatus == "Processed") %>%
  group_by(DonorConstituentID, PaymentStatus) %>%
  summarise(success = n()) %>%
  distinct(DonorConstituentID, success) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#Obtain ratio of fail to success
fail_ratio <- success %>%
  left_join(fail, by = c("ConstituentID")) %>%
  mutate(fail_ratio = fail/success) %>%
  distinct(ConstituentID, fail_ratio)

#Join fail_ratio with const_base

const_base <- sqldf("select a.*,
                    b.fail_ratio as fail_ratio
                    from const_base a
                    left join fail_ratio b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(fail_ratio, fail, success)

const_base$fail_ratio <- ifelse(is.na(const_base$fail_ratio), 0, const_base$fail_ratio)

#--------------------------------------------------------------------------------------------------
#Join instance_counts with const_base
const_base <- sqldf("select a.*,
                    b.instance_counts as instance_counts
                    from const_base a
                    left join instance_counts b
                    on a.donor = b.IndConstituentID
                    order by a.donor")

rm(instance_counts)
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------------
#Create tables with sums and counts of past donations at a constituent-program-year level 

#Past donations
past_don <- pay_events %>%
  filter(!is.na(ProgramName)) %>%
  filter(PaymentStatus == "Processed") %>%
  mutate(don_year = year(DonationDateSKey)) %>%
  group_by(DonorConstituentID, ProgramName, don_year) %>%
  summarise(count = n(),
            donated_amt = sum(PaymentAmount))

#Donation by event type
donor_donations_by_event <- pay_events %>%
  filter(PaymentStatus == "Processed") %>%
  group_by(DonorConstituentID, ProgramName) %>%
  summarise(total_amt = sum(PaymentAmount),
            count = n())

donor_total <- donor_donations_by_event %>%
  group_by(DonorConstituentID) %>%
  summarise(total_donated = sum(total_amt),
            total_count = sum(count))

#Reshape the table to pick specific columns based on need
donor_donations <- dcast(setDT(donor_donations_by_event), DonorConstituentID~ProgramName, value.var = c("total_amt", "count"))

#Setting all NAs to 0
donor_donations[is.na(donor_donations)] <- 0

names(donor_donations) <- gsub(" ", "_", names(donor_donations))

rm(donor_donations_by_event)

#Export tables to a specific location to read them individually as needed
write.table(donor_donations, "/home/rstudio-user/donor_donations.csv", row.names = F)
write.table(donor_total, "/home/rstudio-user/donor_total.csv", row.names = F)
write.table(past_don, "/home/rstudio-user/past_don.csv", row.names = F)
write.table(const_base, "/home/rstudio-user/const_base.csv", row.names = F)

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Joining tables to get constituent level info in one table

const_base <- fread("/home/rstudio-user/const_base.csv")
donor_donations <- fread("/home/rstudio-user/donor_donations.csv")
donor_total <-  fread("/home/rstudio-user/donor_total.csv")
past_don <-  fread("/home/rstudio-user/past_don.csv")

#--------------------------------------------------------------------------------------------------
#For RG 
#--------------------------------------------------------------------------------------------------

rg_base <- sqldf("select a.*,
                 b.total_amt_Regular_Giving as total_amt_rg,
                 b.count_Regular_Giving as count_rg,
                 c.total_donated,
                 c.total_count
                 from const_base a
                 left join donor_donations b
                 on a.donor = b.DonorConstituentID
                 left join donor_total c
                 on a.donor = c.DonorConstituentID
                 order by a.donor")

#Join with pay_don table to identify payment in 2018 or not - dependent variable

rg_base <- sqldf("select distinct a.*,
                 b.status
                 from rg_base a
                 left join 
                 (select DonorConstituentID, don_year, count, 1 as status
                 from past_don
                 where ProgramName = \"Regular Giving\"
                 and don_year = 2018
                 and count > 0) b
                 on a.donor = b.DonorConstituentID
                 order by a.donor")

rg_base$status <- ifelse(is.na(rg_base$status), 0, 1)

write.table(rg_base, "/home/rstudio-user/rg_base.csv", row.names = F)

#--------------------------------------------------------------------------------------------------
#For ABMT
#--------------------------------------------------------------------------------------------------

abmt_base <- sqldf("select a.*,
                   b.total_amt_ABMT as total_amt_abmt,
                   b.count_ABMT as count_abmt,
                   c.total_donated,
                   c.total_count
                   from const_base a
                   left join donor_donations b
                   on a.donor = b.DonorConstituentID
                   left join donor_total c
                   on a.donor = c.DonorConstituentID
                   order by a.donor")

#Join with pay_don table to identify payment in 2018 or not - dependent variable

abmt_base <- sqldf("select distinct a.*,
                   b.status
                   from abmt_base a
                   left join 
                   (select DonorConstituentID, don_year, count, 1 as status
                   from past_don
                   where ProgramName = \"ABMT\"
                   and don_year = 2018
                   and count > 0) b
                   on a.donor = b.DonorConstituentID
                   order by a.donor")

abmt_base$status <- ifelse(is.na(abmt_base$status), 0, 1)

write.table(abmt_base, "/home/rstudio-user/abmt_base.csv", row.names = F)

#--------------------------------------------------------------------------------------------------
#For Beneficiary_Events
#--------------------------------------------------------------------------------------------------

be_base <- sqldf("select a.*,
                 b.total_amt_Beneficiary_Events as total_amt_be,
                 b.count_Beneficiary_Events as count_be,
                 c.total_donated,
                 c.total_count
                 from const_base a
                 left join donor_donations b
                 on a.donor = b.DonorConstituentID
                 left join donor_total c
                 on a.donor = c.DonorConstituentID
                 order by a.donor")

#Join with pay_don table to identify payment in 2018 or not - dependent variable

be_base <- sqldf("select distinct a.*,
                 b.status
                 from be_base a
                 left join 
                 (select DonorConstituentID, don_year, count, 1 as status
                 from past_don
                 where ProgramName = \"Beneficiary Events\"
                 and don_year = 2018
                 and count > 0) b
                 on a.donor = b.DonorConstituentID
                 order by a.donor")

be_base$status <- ifelse(is.na(be_base$status), 0, 1)

write.table(be_base, "/home/rstudio-user/be_base.csv", row.names = F)

#--------------------------------------------------------------------------------------------------
#For Relay for Life
#--------------------------------------------------------------------------------------------------

rfl_base <- sqldf("select a.*,
                  b.total_amt_Relay_For_Life as total_amt_rfl,
                  b.count_Relay_For_Life as count_rfl,
                  c.total_donated,
                  c.total_count
                  from const_base a
                  left join donor_donations b
                  on a.donor = b.DonorConstituentID
                  left join donor_total c
                  on a.donor = c.DonorConstituentID
                  order by a.donor")

#Join with pay_don table to identify payment in 2018 or not - dependent variable

rfl_base <- sqldf("select distinct a.*,
                  b.status
                  from rfl_base a
                  left join 
                  (select DonorConstituentID, don_year, count, 1 as status
                  from past_don
                  where ProgramName = \"Relay For Life\"
                  and don_year = 2018
                  and count > 0) b
                  on a.donor = b.DonorConstituentID
                  order by a.donor")

rfl_base$status <- ifelse(is.na(rfl_base$status), 0, 1)

write.table(rfl_base, "/home/rstudio-user/rfl_base.csv", row.names = F)