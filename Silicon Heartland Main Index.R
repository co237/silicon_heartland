

### Figure out which degrees to tag as high tech


#### Loading Data and crosswalks

# Load 2019-2023 five-year IPUMS sample

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00036.xml")
data <- read_ipums_micro(ddi)

# Load BDS High Tech Industries definitions (NAICS Codes)
BDS_HT_definitions <- read_excel("BDS_hightech.xlsx")
BDS_HT_definitions$NAICS <- as.character(BDS_HT_definitions$NAICS)

# I have manually crosswalked BDS HT industry codes from their NAICS to their ACS definitions, which are loaded here
BDS_HT_definitions_2 <- read_excel("HT_IND_Codes.xlsx")

# Load Degree fields
Degree_field_code_details <- read_excel("Degree_fields_codes_groups.xlsx")

# Load state codes
state_codes <- read_excel("State_FIPS.xlsx")

# Load CIP to ACS degree codes crosswalk 
cip_crosswalk <- read_excel("cip_acs_crosswalk.xlsx")

#Combine data with BDS high tech industry definitions
data <- left_join(data,BDS_HT_definitions_2, by = c("IND"="IND"))

#Combine data with detailed degree fields 
data <- left_join(data, Degree_field_code_details, by = c("DEGFIELDD" = "Degree_code"))

# filter out workers in non-BDS HT industries (missing BDS HT industry description)
BDS_industry_data <- data %>%
  filter(!is.na(Ind_Description)) 

# top degrees in BDS HT industries
Top_BDS_HT_degrees <- BDS_industry_data %>%
  group_by(Degree) %>%
  summarise(total_grads = sum(PERWT)) %>%
  arrange(desc(total_grads)) %>%
  mutate(share = total_grads/sum(total_grads))

# top degrees overall
Top_degrees_overall <- data %>%
  group_by(Degree) %>%
  summarise(total_grads = sum(PERWT)) %>%
  arrange(desc(total_grads)) %>%
  mutate(share_overall_grads = total_grads/sum(total_grads))

# Join them together 
Top_BDS_HT_degrees <- left_join(Top_BDS_HT_degrees, Top_degrees_overall, by = c("Degree"="Degree"))

#Degree Location Quotient of >1.5
Top_BDS_HT_degrees <- Top_BDS_HT_degrees %>%
  mutate(degree_LQ = share/share_overall_grads) %>%
  filter(degree_LQ>=1.5)

# Merge degree descriptions
Top_BDS_HT_degrees <- left_join(Top_BDS_HT_degrees,Degree_field_code_details, by = c("Degree" = "Degree") )
Top_BDS_HT_degrees <- Top_BDS_HT_degrees %>%
  select(-total_grads.x,-total_grads.y)

# Top_BDS_HT_degrees now identifies which degrees are high-tech

# Add an indicator for high-tech
Top_BDS_HT_degrees <- Top_BDS_HT_degrees %>%
  mutate(tech_indicator = 1)

# Merge high tech indicator with the overall IPUMS sample
data <- left_join(data, Top_BDS_HT_degrees, by = c("DEGFIELDD" = "Degree_code"))

# Find the number of high-tech graduates living in each state
State_tech_degrees<- data %>%
  group_by(STATEFIP, tech_indicator) %>%
  summarise(total = sum(PERWT)) %>%
  mutate(tech_indicator = ifelse(is.na(tech_indicator), "NA", as.character(tech_indicator))) %>%
  pivot_wider(names_from = tech_indicator,
              values_from = total,
              names_prefix = "tech") %>%
  mutate(total_grads = tech1 + techNA,
         tech_grad_share = tech1/total_grads) %>%
  arrange(desc(tech_grad_share))

#Rename variables
State_tech_degrees <- State_tech_degrees %>%
  rename(
    Tech_Graduates = tech1,
    Non_Tech_Graduates = techNA,
    All_Graduates = total_grads
  )

# Merge state names
State_tech_degrees <- left_join(State_tech_degrees, state_codes, by = c("STATEFIP" = "FIPS"))

# This next section analyzes IPEDS data to find the number of high tech *degrees* awarded by state

# Bachelor's degrees by school by year, 2014-2023 - 6 digit CIP codes
ipeds_2023<- read.csv(file.path("Degrees 2023", "CSV_432025-1011.csv"))
ipeds_2022<- read.csv(file.path("Degrees 2022", "CSV_432025-237.csv"))
ipeds_2021<- read.csv(file.path("Degrees 2021", "CSV_432025-850.csv"))
ipeds_2020<- read.csv(file.path("Degrees 2020", "CSV_432025-639.csv"))
ipeds_2019<- read.csv(file.path("Degrees 2019", "CSV_432025-347.csv"))
ipeds_2018<- read.csv(file.path("Degrees 2018", "CSV_432025-641.csv"))
ipeds_2017<- read.csv(file.path("Degrees 2017", "CSV_432025-525.csv"))
ipeds_2016<- read.csv(file.path("Degrees 2016", "CSV_432025-428.csv"))
ipeds_2015<- read.csv(file.path("Degrees 2015", "CSV_432025-154.csv"))
ipeds_2014<- read.csv(file.path("Degrees 2014", "CSV_432025-587.csv"))

# Recode the variable name for CIP codes for the respective (2020 or 2010) definitions. 2014-19 are 2010 CIP codes. 2020-23 are 2020 CIP codes.
names(ipeds_2023)[5] <- "CIP_2020"
names(ipeds_2022)[5] <- "CIP_2020"
names(ipeds_2021)[5] <- "CIP_2020"
names(ipeds_2020)[5] <- "CIP_2020"
names(ipeds_2019)[5] <- "CIP_2010"
names(ipeds_2018)[5] <- "CIP_2010"
names(ipeds_2017)[5] <- "CIP_2010"
names(ipeds_2016)[5] <- "CIP_2010"
names(ipeds_2015)[5] <- "CIP_2010"
names(ipeds_2014)[5] <- "CIP_2010"

# Put degree codes in a formula usable in the crosswalk
ipeds_2023$CIP_2020 <- as.numeric(gsub("'", "", ipeds_2023$CIP_2020))
ipeds_2022$CIP_2020 <- as.numeric(gsub("'", "", ipeds_2022$CIP_2020))
ipeds_2021$CIP_2020 <- as.numeric(gsub("'", "", ipeds_2021$CIP_2020))
ipeds_2020$CIP_2020 <- as.numeric(gsub("'", "", ipeds_2020$CIP_2020))
ipeds_2019$CIP_2010 <- as.numeric(gsub("'", "", ipeds_2019$CIP_2010))
ipeds_2018$CIP_2010 <- as.numeric(gsub("'", "", ipeds_2018$CIP_2010))
ipeds_2017$CIP_2010 <- as.numeric(gsub("'", "", ipeds_2017$CIP_2010))
ipeds_2016$CIP_2010 <- as.numeric(gsub("'", "", ipeds_2016$CIP_2010))
ipeds_2015$CIP_2010 <- as.numeric(gsub("'", "", ipeds_2015$CIP_2010))
ipeds_2014$CIP_2010 <- as.numeric(gsub("'", "", ipeds_2014$CIP_2010))

# Load state-institution ID definitions
state_codes_2023 <- read.csv(file.path("CSV_432025-107","CSV_432025-107.csv"))

# For each year 2014 through 2023, join the degree data with the CIP crosswalk, then filter for high-tech degrees and summarise by state 
# this will give you 10 files, each one with the number of high-tech degrees produced by state in that year

ipeds_2023 <- left_join(ipeds_2023,cip_crosswalk, by = c("CIP_2020" = "CIP_2020") )

ipeds_2023<- left_join(ipeds_2023, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2023_A.Grand.total))

ipeds_2022 <- left_join(ipeds_2022,cip_crosswalk, by = c("CIP_2020" = "CIP_2020") )

ipeds_2022<- left_join(ipeds_2022, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2022_A_RV.Grand.total))

ipeds_2021 <- left_join(ipeds_2021,cip_crosswalk, by = c("CIP_2020" = "CIP_2020") )

ipeds_2021<- left_join(ipeds_2021, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2021_A_RV.Grand.total))

ipeds_2020 <- left_join(ipeds_2020,cip_crosswalk, by = c("CIP_2020" = "CIP_2020") )

ipeds_2020<- left_join(ipeds_2020, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2020_A_RV.Grand.total))

ipeds_2019 <- left_join(ipeds_2019,cip_crosswalk, by = c("CIP_2010" = "CIP_2010") )

ipeds_2019<- left_join(ipeds_2019, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2019_A_RV.Grand.total))

ipeds_2018 <- left_join(ipeds_2018,cip_crosswalk, by = c("CIP_2010" = "CIP_2010") )

ipeds_2018<- left_join(ipeds_2018, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2018_A_RV.Grand.total))

ipeds_2017 <- left_join(ipeds_2017,cip_crosswalk, by = c("CIP_2010" = "CIP_2010") )

ipeds_2017<- left_join(ipeds_2017, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2017_A_RV.Grand.total))

ipeds_2016 <- left_join(ipeds_2016,cip_crosswalk, by = c("CIP_2010" = "CIP_2010") )

ipeds_2016<- left_join(ipeds_2016, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2016_A_RV.Grand.total))

ipeds_2015 <- left_join(ipeds_2015,cip_crosswalk, by = c("CIP_2010" = "CIP_2010") )

ipeds_2015<- left_join(ipeds_2015, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2015_A_RV.Grand.total))

ipeds_2014 <- left_join(ipeds_2014,cip_crosswalk, by = c("CIP_2010" = "CIP_2010") )

ipeds_2014<- left_join(ipeds_2014, state_codes_2023, by = c("unitid" = "unitid")) %>%
  filter(BDS_HT_indicator == 1) %>%
  group_by(HD2023.FIPS.state.code) %>%
  summarise(total_grads = sum(C2014_A_RV.Grand.total))

# Add year variables to each file
ipeds_2023<- ipeds_2023 %>% mutate(Year=2023)
ipeds_2022<- ipeds_2022 %>% mutate(Year=2022)
ipeds_2021<- ipeds_2021 %>% mutate(Year=2021)
ipeds_2020<- ipeds_2020 %>% mutate(Year=2020)
ipeds_2019<- ipeds_2019 %>% mutate(Year=2019)
ipeds_2018<- ipeds_2018 %>% mutate(Year=2018)
ipeds_2017<- ipeds_2017 %>% mutate(Year=2017)
ipeds_2016<- ipeds_2016 %>% mutate(Year=2016)
ipeds_2015<- ipeds_2015 %>% mutate(Year=2015)
ipeds_2014<- ipeds_2014 %>% mutate(Year=2014)

# Merge all the IPEDS data into a panel 
ipeds_ht_panel<- rbind(ipeds_2023, ipeds_2022, ipeds_2021, ipeds_2020, ipeds_2019, ipeds_2018, ipeds_2017, ipeds_2016, ipeds_2015, ipeds_2014)

# write.csv(ipeds_ht_panel, "high_tech_grads_14_23.csv")

# Total tech graduates between 2014 and 2023
ipeds_14_23_totals <- ipeds_ht_panel %>% group_by(HD2023.FIPS.state.code) %>% summarise(total_ht_grads = sum(total_grads))

# Merge total tech graduates with other totals 
high_tech_retention <- left_join(State_tech_degrees,ipeds_14_23_totals, by= c("State_name"="HD2023.FIPS.state.code") )
high_tech_retention <- high_tech_retention %>%
  rename(Tech_degrees_14_to_23 = total_ht_grads) %>%
  mutate(retention_ratio = Tech_Graduates/Tech_degrees_14_to_23 )

# Load labor force data (1976-2024 annual averages)
labor_force_data <- read_excel("Labor_force_data.xlsx")
# Filter out to just the most recent year of data (2024)
labor_force_data_24<- labor_force_data %>% filter(Year==2024)
# merge
high_tech_retention <- left_join(high_tech_retention, labor_force_data_24, by = c("State_name"="State_name"))

# Compare recent tech grads to the overall size of the labor force, by state
high_tech_retention <- high_tech_retention %>% mutate(Tech_grads_14_23_to_LF_ratio = Tech_degrees_14_to_23/Labor_force)

# load regional price parities
regional_price_parities <- read_excel("regional_price_parities_23.xlsx")
# join cost of living data with high tech retention data
high_tech_retention <- left_join(high_tech_retention, regional_price_parities, by = c("State_name"="State_name"))

# reverse the signs on retention and cost of living 
high_tech_retention <- high_tech_retention %>%
  mutate(negative_retention = -retention_ratio,
         neg_affordability = -RPP_2023)

# Create Z scores for student loss (inverse of retention), affordability (negative RPP) and grad production (tech degrees '14-'23 divided by LF)
high_tech_retention$z_affordability <- (high_tech_retention$neg_affordability - mean(high_tech_retention$neg_affordability))/sd(high_tech_retention$neg_affordability)
high_tech_retention$z_student_loss <- (high_tech_retention$negative_retention - mean(high_tech_retention$negative_retention))/sd(high_tech_retention$negative_retention)
high_tech_retention$z_tech_grad_production <- (high_tech_retention$Tech_grads_14_23_to_LF_ratio- mean(high_tech_retention$Tech_grads_14_23_to_LF_ratio))/sd(high_tech_retention$Tech_grads_14_23_to_LF_ratio)

# Combine these three z scores into one index
high_tech_retention <- high_tech_retention %>%
  mutate(potential_index = z_affordability + z_student_loss + z_tech_grad_production) %>%
  arrange(desc(potential_index))




