###################################loading required pacakges for analysis######################################

if (!require("pacman")) install.packAGEs("pacman")

pacman::p_load(
  dplyr, 
  magrittr, 
  tidyverse,
  cowplot,
  lubridate,
  epiDisplay,
  Hmisc,
  forcats,
  flextable,
  scales,
  readxl,
  ggplot2,
  tmap,
  sf,
  readr, 
  stringr,
  raster,
  maps,
  viridis,
  janitor,
  plyr,
  flextable,
  officer
)

select <- dplyr::select
mutate <- dplyr::mutate
summarize <- dplyr::summarize
group_by <- dplyr::group_by
rename <- dplyr::rename
mutate_at <- dplyr::mutate_at
count <- dplyr::count
prop.table <- officer::prop_table
fct_relevel <- forcats::fct_relevel

########################### loading Data file for analysis########################################

df <- read_excel("~file.path/Prenatreg_Assess_CarePl.xlsx")

##############################DATA CLEANING/ORGANIZING AND RESTRUCTURING STEPS#################################
  
df_2 <- df %>%
  #removing test records
  filter(ID != 35)%>% 
  group_by(ID)%>% 
  #converting the following columns to date format
  mutate(ADDRESS_FROM = dmy(ADDRESS_FROM),
         REF_DATE = dmy(REF_DATE),
         ASS_DATE = dmy(ASS_DATE),
         DOB = dmy(DOB),
         ADDRESS_TO = dmy(ADDRESS_TO)) %>%
  #FILLING IN BLANK ROWS WITH corresponding values for the following columns
  mutate_at(c('ABORIGINAL', 'INTERPRETER', 'FRSTTMPARENT', 'SOMEONETOTLK', 'COMPLETEDHS', 
              'INCOMEDIFFI', 'INCOMEASSIST', 'DEPRESSED', 'LOWINTEREST', 'SOMEONETOTLK', 
              'TOBACCO', 'AROUNDSMOKER', 'STABLE_HSN'), ~ ifelse(length(unique(.[!is.na(.) & !. ==""])) == 0, NA, unique(.[!is.na(.) & !. ==""]))) %>%
  #creating an AGE column to get the age of clients
  mutate(AGE = as.integer(difftime(ASS_DATE, DOB, units = 'days')/ 365 ),
         AGE_RANGE = case_when(
           AGE < 18 ~ 'Under 18',
           AGE >= 18 & AGE <= 24 ~ '18 - 24',
           AGE >= 25 & AGE <= 34 ~ '25 - 34',
           AGE >= 35 & AGE <= 44 ~ '35 - 44',
           AGE >= 45 & AGE <= 54 ~ '45 - 54',
           TRUE ~ '55 and Above')) %>%
  
  #removing invalid rows
  filter (AGE > 10) %>%
  
  #creating a variable indicator if PER_ID had careplan. if a client has an ASS_TYPE = PHASCHCP01, i.e they have a careplan
  group_by(ID)%>%
  mutate(CAREPLAN = ifelse("PHASCHCP01" %in% ASS_TYPE, "yes", "no"))%>%
  ungroup() %>%
  filter(PATHWAY %in% c("A", "B", "C", "D"))

#creating a dataframe without these variables as they are not needed/have little information and some are not necessary
Complete_data <- df_2 %>% 
  select(-c(FORENAME, SURNAME, DOB, GENDER, 
            CBI_LU, AGE, 
            ADDRESS_FROM, REF_TYPE, REF_DATE, CLI_ID, RELIGION,
            STABLE_HSN, IMM_STATUS, ADDRESS_TO, ASS_REASON, 
            RACE, CC_BOOL)) %>%
  #cleaning the following columns with boolean and proper characters.
  mutate_at(c('ABORIGINAL', 'INTERPRETER', 'SOMEONETOTLK', 'COMPLETEDHS', 
              'INCOMEDIFFI', 'INCOMEASSIST', 'DEPRESSED','LOWINTEREST',
              'FRSTTMPARENT'), ~ case_when(
                . == 1 ~ "Yes",
                . == 2 ~ "No",
                is.na(.) ~ "Unknown")) %>%
  
  mutate_at(c('ABORIGINAL', 'INTERPRETER', 'SOMEONETOTLK', 'COMPLETEDHS', 
              'INCOMEDIFFI', 'INCOMEASSIST', 'DEPRESSED','LOWINTEREST',
              'FRSTTMPARENT'), ~ factor(., levels = c("Yes", "No", "Unknown"))) %>%
  
  mutate(IDENTIFY_FL  = case_when(
    IDENTIFY_FL == 1 ~ "Yes",
    IDENTIFY_FL == 2 ~ "No",
    IDENTIFY_FL == 3 ~ "Asked, Not Provided",
    IDENTIFY_FL == 4 ~ "Not Asked",
    is.na(IDENTIFY_FL) ~ "Unknown"),
    IDENTIFY_FL = factor(IDENTIFY_FL, levels = c("Yes", "No", "Asked, Not Provided", "Not Asked", "Unknown")),
    
    AROUNDSMOKER = case_when(
      AROUNDSMOKER %in% c("Never", "never")  ~ "Never",
      is.na(INTERPRETER) ~ "Unknown",
      TRUE ~ AROUNDSMOKER),
    
    TOBACCO = case_when(
      TOBACCO %in% c("No", "no", "I have never smoked cigarettes", "Never") ~ "No",
      TOBACCO %in% c("quit less an 1 year ago", "I quit smoking less than 1 year ago") ~ "Quit less an 1 year ago",
      TOBACCO == "I quit smoking more than 1 year ago" ~ "Quit more than 1 year ago",
      is.na(TOBACCO) ~ "Unknown",
      TRUE ~ TOBACCO), 
    
    #removing spaces from the postal code variable
    POSTCODE = str_replace_all(POSTCODE, pattern = " ", repl = "")) %>% 
  
  rename(
    Aboriginal = "ABORIGINAL",
    Need_Interpeter = "INTERPRETER",
    Completed_HS = "COMPLETEDHS",
    First_time_parent = "FRSTTMPARENT",
    Someone_to_talk_to = "SOMEONETOTLK",
    Income_difficulty = "INCOMEDIFFI",
    Income_assistance = "INCOMEASSIST",
    Depressed = "DEPRESSED",
    Little_Interest = "LOWINTEREST",
    Secondhand_smoke = "AROUNDSMOKER",
    Smoking = "TOBACCO")

##########Saving de-duplicated data###################
#write.csv(Complete_data, "~file.path/Comprehensive data.csv")


######################CREATING SEPERATE DATA TABLES FOR MY ANALYSIS################################

##############Table with information on clients receiving services################
Careplan_clients = Complete_data %>%
  filter(CAREPLAN == "yes", ASS_TYPE == "PHASCHCP01")

##############Table with information on clients not receiving services################
No_careplan_clients = Complete_data %>%
  filter(CAREPLAN == "no") 

##########comprehensive unique data table with information on both client groups##########################
complete_data_unique = Complete_data%>%
  filter((CAREPLAN == "yes" & ASS_TYPE == "PHASCHCP01") | CAREPLAN == "no")

#######saving unique data table#######################
write.csv(complete_data_unique, "~file.path/Unique data.csv")


###################################################################################################################
#############################SUMMARY ANALYSIS###################################

##CHI-SQUARE TEST FOR EACH VARIABLE##

#Aboriginal
complete_data_unique %>% 
  tabyl(Aboriginal, CAREPLAN) %>% 
  chisq.test()

#interpreter
complete_data_unique%>% 
  tabyl(Need_Interpeter, CAREPLAN) %>% 
  chisq.test()

#low education
complete_data_unique %>% 
  tabyl(Completed_HS, CAREPLAN) %>% 
  chisq.test()

#smoking
complete_data_unique %>% 
  tabyl(Smoking, CAREPLAN) %>% 
  chisq.test()

#for depression
complete_data_unique %>% 
  tabyl(Depressed, CAREPLAN) %>% 
  chisq.test()

#not having someone to talk to
complete_data_unique %>% 
  tabyl(Someone_to_talk_to, CAREPLAN) %>% 
  chisq.test()

#interest in activities
complete_data_unique %>% 
  tabyl(Little_Interest, CAREPLAN) %>% 
  chisq.test()

#income assistance
complete_data_unique %>% 
  tabyl(Income_assistance, CAREPLAN) %>% 
  chisq.test()

#income difficulties
complete_data_unique %>% 
  tabyl(Income_difficulty, CAREPLAN) %>% 
  chisq.test()

#first time parent
complete_data_unique %>% 
  tabyl(First_time_parent, CAREPLAN) %>% 
  chisq.test()

#pathways
complete_data_unique %>% 
  tabyl(PATHWAY, CAREPLAN) %>% 
  chisq.test()


####################PERFORMING MAPPING WITH tmaps####################################
#####################################################################################

#Loading shapefiles

HA <-  read_sf("~file.path/HA_2022.shp")

HSDA <- read_sf("~file.path/HSDA_2022.shp")

CHSA <- read_sf("~file.path/CHSA_2022.shp")

LHA <- read_sf("~file.path/LHA_2022.shp")



######################merging reference data to my dataset##############################

reference_data <- read_excel("~file.path/gcs202303.xlsx") %>%
  select(POSTALCODE, LATITUDE, LONGITUDE, HA, CHSA_2022, HSDA, LHA_2018)

mapped_data <- merge(complete_data_unique, reference_data, by.x = "ADD_POSTCODE", by.y = "POSTALCODE", all.x = TRUE) %>%
  filter(!is.na(LATITUDE) | !is.na(LONGITUDE)) %>%
  #keeping rows where CHSA_2022 column starts with '2' representing the FH region
  filter(grepl('^2', CHSA_2022))

#creating a table for mapping to calculate the percentage of those without careplan
mapped_data_subdivided <- mapped_data %>% 
  group_by(CHSA_2022, CAREPLAN)  %>%
  summarize(clients_per_CHSA = n()) %>%
  group_by(CHSA_2022) %>%
  mutate(TOTAL = sum(clients_per_CHSA),
         Percentage_no = (clients_per_CHSA/TOTAL) * 100) %>%
  ungroup() %>%
  filter(CAREPLAN == 'no')

#rounding up decimal digits to 2.
mapped_data_subdivided$Percentage_no <- round(mapped_data_subdivided$Percentage_no, digits = 2)

#performing attribute join of merged data subdivided to CHSA shape file
mapped_data_merged <- merge(CHSA, mapped_data_subdivided, by.x = "CHSA_CD", by.y = "CHSA_2022")

#setting palletes
palletes <- c('#bae4bc','#7bccc4','#43a2ca', "#0868ac", "#065187")



##################PLOTTING A STATIC MAP##############################
breaks = c(0, 60, 70, 80, 90, 100)

Maps <- tmap_mode("plot") +
  tm_shape(mapped_data_merged) +
  #tm_fill(col = 'LHA_Title', palette = palletes) +
  tm_borders(col="grey", lwd=2) +
  tm_polygons(col = "Percentage_no", 
              palette = palletes, 
              breaks = breaks, 
              legend.hist = TRUE, 
              title = "% not receiving services") +
  tm_legend(outside = TRUE) +
  #tm_dots(size = "Percentage_no", palette = palletes) +
  #tm_scale_bar(position = c("right", "bottom"), 
  #text.size = 0.8) +
  tm_layout(main.title = "Population of clients (Pathway A-D) not receiving services per CHSA for FY 2022/23",
            main.title.position = "center",
            main.title.size = 1.2,
            main.title.fontface = "bold",
            legend.height = 1.45, 
            legend.width = 1.5,
            frame = F,
            inner.margins= c(0.05,0.05,0.05, 0.05), 
            outer.margins = c(1,0,1,0),
            legend.position = c("right", "top")) +
  #tm_style("classic") +
  #tm_text(text = "LHA_Name", 
  #col = "black", 
  #size = "AREA", 
  #clustering = F,
  #root = 3) +
  tm_shape(LHA) +
  tm_borders(col="black", lwd= 1)


#exporting map as an image
Mapping_CHSA <- tmap_save(Maps, filename = "Clients_CHSA_Maps5.png")




################################################################################################################
##################PERFORMING GGPLOTS BY CONVERTING DATA FORMAT FROM WIDE TO LONG####################################



#######################ggplot for clients not receiving services####################################
No_careplan_clients_longer <- No_careplan_clients %>% 
  mutate(Smoking = case_when(
    Smoking == "I currently smoke cigarettes" ~ "Yes",
    Smoking == "No" ~ "No",
    Smoking == "Quit less an 1 year ago" ~ ">One",
    Smoking == "Quit more than 1 year ago" ~ "<One",
    TRUE ~ Smoking
  )) %>%
  pivot_longer(cols = c("Aboriginal", "Need_Interpeter", "First_time_parent", "Someone_to_talk_to",        
                        "Completed_HS", "Income_difficulty", "Income_assistance",
                        "Depressed", "Little_Interest", "Smoking"),
               names_to = 'variables',
               values_to = 'Responses') %>%
  group_by(variables, Responses) %>%
  summarize(Variables_counts =n()) %>%
  filter(Responses != "Unknown") %>%
  group_by(variables) %>%
  mutate(TOTAL = sum(Variables_counts),
         Percentage = (Variables_counts/TOTAL) * 100,
         variables = gsub("_", " ", variables)) %>%
  ungroup() %>%
  #always cross-check to ensure you are slicing the correct rows
  slice(2, 3, 6, 8, 10, 12, 14, 16, 20, 22)


ggplot(data = No_careplan_clients_longer)+
  geom_col(mapping = aes(y = variables, x = Percentage), fill =  "#E35205") +
  labs(
    title = "For Clients not recieving services the proportion for each vulnerability",
    # subtitle = "The vulnerable population are those who respond 'yes' to all the variables except Someonetotalk and completedHS which are 'no'",
    x = "Percent",
    y = "Vulnerability",
    caption = ""
  ) +
  geom_text(data = No_careplan_clients_longer, aes(x = Percentage, y = variables, label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.9), hjust = -0.2) + 
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) + 
  theme_bw()

###saving my ggplot###
ggsave("Clients_not_receiving_services_prop_for_each_vulnerability.png", device = "png", width = 25, height = 20, units = "cm")


#####################################ggplot for client receiving services########################################
Careplan_clients_longer <- Careplan_clients %>% 
  mutate(Smoking = case_when(
    Smoking == "I currently smoke cigarettes" ~ "Yes",
    Smoking == "No" ~ "No",
    Smoking == "Quit less an 1 year ago" ~ ">One",
    Smoking == "Quit more than 1 year ago" ~ "<One",
    TRUE ~ Smoking
  )) %>%
  pivot_longer(cols = c("Aboriginal", "Need_Interpeter", "First_time_parent", "Someone_to_talk_to",        
                        "Completed_HS", "Income_difficulty", "Income_assistance",
                        "Depressed", "Little_Interest", "Smoking"),
               names_to = 'variables',
               values_to = 'Responses') %>%
  group_by(variables, Responses) %>%
  summarize(Variables_counts =n()) %>%
  filter(Responses != "Unknown") %>%
  mutate(TOTAL = sum(Variables_counts),
         Percentage = (Variables_counts/TOTAL) * 100,
         variables = gsub("_", " ", variables)) %>%
  ungroup() %>%
  #always cross-check to ensure you are slicing the correct rows
  slice(2, 3, 6, 8, 10, 12, 14, 16, 20, 21)

ggplot(data = Careplan_clients_longer) +
  geom_col(mapping = aes(y = variables, x = Percentage), fill = "#005293") +
  labs(
    title = "For Clients recieving services the proportion for each vulnerability",
    # subtitle = "The vulnerable population are those who respond 'yes' to all the variables except Someonetotalk and completedHS which are 'no'",
    x = "Percent",
    y = "Vulnerability",
    caption = ""
  ) +
  geom_text(data = Careplan_clients_longer, aes(x = Percentage, y = variables, label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.9), hjust = -0.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) + 
  theme_bw()

###saving my ggplot###
ggsave("Clients_receiving_services_prop_each_vulnerability.png", device = "png", width = 25, height = 20, units = "cm")


##########################################################################################################
########################CALCULATING RATE RATIO###########################################################

#creating a join of tables with data

RR_Calculation <- Careplan_clients_longer %>%
  inner_join(No_careplan_clients_longer, by = 'variables') %>%
  select(variables, Responses.x, Percentage.x, Percentage.y) %>%
  mutate(Rate_Ratio = Percentage.x/Percentage.y) %>%
  rename(No_careplan = Percentage.y,
         Has_careplan =  Percentage.x)


###########################################################################################################################
##################ggplot  for clients not receiving services the total proportion for each vulnerability####################

Proportion_variable <- complete_data_unique %>%
  mutate(Smoking = case_when(
    Smoking == "I currently smoke cigarettes" ~ "Yes",
    Smoking == "No" ~ "No",
    Smoking == "Quit less an 1 year ago" ~ ">One",
    Smoking == "Quit more than 1 year ago" ~ "<One",
    TRUE ~ Smoking
  )) %>%
  pivot_longer(cols = c("Aboriginal", "Need_Interpeter", "First_time_parent", "Someone_to_talk_to",        
                        "Completed_HS", "Income_difficulty", "Income_assistance",
                        "Depressed", "Little_Interest", "Smoking"),
               names_to = 'Variables',
               values_to = 'Responses') %>%
  group_by(Variables,Responses, CAREPLAN) %>%
  summarize(Variables_counts =n()) %>%
  filter(Responses != "Unknown") %>%
  ungroup() %>%
  slice(3, 4, 5, 6, 11, 12, 15, 16, 19, 20, 23, 24, 27, 28, 31, 32, 39, 40, 42, 43) %>%
  group_by(Variables, Responses) %>%
  mutate(TOTAL = sum(Variables_counts),
         Percentage = round(((Variables_counts/TOTAL) * 100), digits = 0),
         Variables = gsub("_", " ", Variables)) %>%
  ungroup() %>%
  filter(CAREPLAN == 'no')

ggplot(data = Proportion_variable) +
  geom_col(mapping = aes(y = fct_reorder(Variables, Percentage), x = Percentage), fill = "#005293") +
  labs(
    title = "Proportion of clients not receiving services (Pathway A-D) for each vulnerability FY 2022/23",
    # subtitle = "The vulnerable population are those who respond 'yes' to all the variables except Someonetotalk and completedHS which are 'no'",
    x = "Percent",
    y = "Vulnerability",
    caption = ""
  ) +
  geom_text(data = Proportion_variable, aes(x = Percentage, y = Variables, label = paste0(round(Percentage,0 ), "%")),
            position = position_dodge(width = 0.9), hjust = -0.5, size = 4.8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) + 
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(size = 17))

###saving my ggplot###
ggsave("Clients_not_receiving_services_total_prop_each_vulnerability.png", device = "png", width = 30, height = 20, units = "cm")

####################### Pathway Distribution for clients not receiving services##############
Pathway_Distribution <- complete_data_unique %>%
  group_by(PATHWAY, CAREPLAN) %>%
  summarize(Pathway_counts = n()) %>%
  ungroup() %>%
  group_by(PATHWAY) %>%
  mutate(TOTAL = sum(Pathway_counts),
         percentage_pathway = round(((Pathway_counts/TOTAL)* 100), digits = 0)) %>%
  filter(CAREPLAN == 'no') 


ggplot(data = Pathway_Distribution) +
  geom_col(mapping = aes(x = PATHWAY, y = percentage_pathway, fill = PATHWAY)) +
  labs(
    title = "Proportion of clients (Pathway A-D) not receiving services for FY 2022/23",
    # subtitle = "The vulnerable population are those who respond 'yes' to all the variables except Someonetotalk and completedHS which are 'no'",
    x = "Pathway",
    y = "Percent",
    caption = "",
    fill = "Pathway"
  ) +
  scale_fill_manual(values = c('#7bccc4','#43a2ca', "#0868ac", "#065187"),
                    labels = c("A" = "A: 1+ of the vulnerabilities",
                               "B" = "B: Depression only",
                               "C" = "C: 2+ of the vulnerabilities",
                               "D" = "D: Smoking only")) +
  geom_text(data = Pathway_Distribution, aes(x = PATHWAY, y = percentage_pathway, label = paste0(round(percentage_pathway,0 ), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  theme_bw() +
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(size = 19))

###saving my ggplot###
ggsave("Clients_not_receiving_services_by_Pathway.png", device = "png", width = 30, height = 20, units = "cm")


################################## Age Distribution for clients not accepting services#########################

Age_Distribution <- complete_data_unique %>%
  group_by(CAREPLAN, AGE_RANGE) %>%
  summarize(Age_counts = n()) %>%
  ungroup() %>%
  group_by(AGE_RANGE) %>%
  mutate(TOTAL = sum(Age_counts),
         percentage_by_age = round(((Age_counts/TOTAL)) *100, digits = 2)) %>%
  filter(CAREPLAN == 'no' & AGE_RANGE != "55 and Above" & AGE_RANGE != "45 - 54") %>%
  ungroup()


ggplot(data = Age_Distribution) +
  geom_col(mapping = aes(x = fct_relevel(AGE_RANGE, c("Under 18", "18 - 24", "25 - 34", "35 - 44", "45 - 54")), y = percentage_by_age),  fill = "#005293") +
  labs(
    title = "Proportion of clients (Pathway A-D) not receiving services by Age group for FY 2022/23",
    # subtitle = "The vulnerable population are those who respond 'yes' to all the variables except Someonetotalk and completedHS which are 'no'",
    x = "Age",
    y = "Percent",
    caption = ""
  ) +
  geom_text(data = Age_Distribution, aes(x = AGE_RANGE, y = percentage_by_age, label = paste0(round(percentage_by_age, 0 ), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) + 
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(size = 16.5))

###saving my ggplot###
ggsave("Clients_not_receiving_services_by_Age.png", device = "png", width = 30, height = 20, units = "cm")


################################## Age Distribution for clients accepting services#########################

Age_Distribution_CP <- complete_data_unique %>%
  group_by(CAREPLAN, AGE_RANGE) %>%
  summarize(Age_counts = n()) %>%
  ungroup() %>%
  group_by(AGE_RANGE) %>%
  mutate(TOTAL = sum(Age_counts),
         percentage_by_age = round(((Age_counts/TOTAL)) *100, digits = 2)) %>%
  filter(CAREPLAN == 'yes'& AGE_RANGE != "55 and Above" & AGE_RANGE != "45 - 54") %>%
  ungroup()


ggplot(data = Age_Distribution_CP) +
  geom_col(mapping = aes(x =  fct_relevel(AGE_RANGE, c("Under 18", "18 - 24", "25 - 34", "35 - 44", "45 - 54")), y = percentage_by_age),  fill = "#005293") +
  labs(
    title = "Distribution of client receiving services by age range",
    # subtitle = "The vulnerable population are those who respond 'yes' to all the variables except Someonetotalk and completedHS which are 'no'",
    x = "AGE",
    y = "Percent",
    caption = ""
  ) +
  geom_text(data = Age_Distribution_CP, aes(x = AGE_RANGE, y = percentage_by_age, label = paste0(round(percentage_by_age, 0 ), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) + 
  theme_bw()

###saving my ggplot###
ggsave("Clients_receiving_services_by_Age.png", device = "png", width = 25, height = 20, units = "cm")



##############################################################################################################
######################creating a flextable##################################################################

Flex_table <- Careplan_clients_longer %>%
  inner_join(No_careplan_clients_longer, by = 'variables') %>%
  mutate(Rate_Ratio = round((Percentage.x/Percentage.y), digits = 2),
         Rate2 = round(Percentage.y, digits = 1),
         Rate1 = round(Percentage.x, digits = 1)) %>%
  arrange(Rate_Ratio) %>%
  rename(
    Responses = Responses.x,
    Count1 = Variables_counts.x,
    Count2= Variables_counts.y) %>%
  select(variables, Responses, Count1, Rate1, Count2, Rate2, Rate_Ratio)

Comprehensive_table <- flextable(Flex_table) %>%
  set_header_labels(
    Count1 = "Count",
    Count2 = "Count",
    Rate1 = "Rate",
    Rate2 = "Rate",
    Rate_Ratio = "Rate Ratio G1/G2")%>%
  add_header_row(top = TRUE,
                 values = c(
                   "Vunerability",
                   "Response",
                   "G1: Received services",
                   "",
                   "G2: Did not receive services",
                   "",
                   "")) %>%
  add_header_row(top = TRUE,
                 values = c("Table 1: Demographic comparison of persons who qualify for prenatal services (Pathway A-D) that don't receive services compared to those who do for FY 2022/23",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "")) %>%
  merge_at(i = 1, j = 1:7, part = "header")%>%
  merge_at(j = 1, i = 2:3, part = "header")%>%
  merge_at(j = 2, i =2:3, part = "header")%>%
  merge_at(i = 2, j = 3:4, part = "header")%>%
  merge_at(i = 2, j = 5:6, part = "header")%>%
  flextable::width (j = 1:7, width = c(2, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)) %>%
  flextable::theme_booktabs() 

###saving my flextable####
save_as_image(Comprehensive_table, path = "Comprehensive_Flex_table.png")
