# Load libraries
library(tidyverse)

# Read in data
la_mort <- 
  read_csv("https://www.dropbox.com/scl/fi/fzsnhfd3lq80v2o3sag6c/la_mort.csv?rlkey=h1vyjm2b8ppgejgsg3e8evm7i&dl=1")

la_pop <- 
  read_csv("https://www.dropbox.com/scl/fi/650k1obpczky6bwa19ex6/la_county_pop.csv?rlkey=0aokd9m76q7mxwus97uslsx7g&dl=1")

stnrd_pop <- 
  read_csv("https://www.dropbox.com/scl/fi/xzd2o5lza237so6vamqwb/stnrd_pop.csv?rlkey=zp90au2tuq6eptvi1yiyjfzua&dl=1")

# Define cancer alley parishes using alternative definition
la_mort$cancer_parish <- ifelse(la_mort$cntyrsd %in% c(5, 47, 89, 93, 95, 121), 1, 0)

# Define cancer deaths by cancer site
la_mort$stomach <- as.integer(la_mort$ucr39 == 5)
la_mort$colon <- as.integer(la_mort$ucr39 == 6)
la_mort$pancreas <- as.integer(la_mort$ucr39 == 7)
la_mort$lung <- as.integer(la_mort$ucr39 == 8)
la_mort$breast <- as.integer(la_mort$ucr39 == 9)
la_mort$cervix <- as.integer(la_mort$ucr39 == 10)
la_mort$prostate <- as.integer(la_mort$ucr39 == 11)
la_mort$bladder <- as.integer(la_mort$ucr39 == 12)
la_mort$lymphoma <- as.integer(la_mort$ucr39 == 13)
la_mort$leukemia <- as.integer(la_mort$ucr39 == 14)
la_mort$other_site <- as.integer(la_mort$ucr39 == 15)
la_mort$total <- as.integer(la_mort$ucr39 %in% c(5:15))

# Adjust age groupings
la_mort_age <- la_mort %>%
  filter(age != 9999)
la_mort_age$age <- ifelse(la_mort_age$age < 2000, la_mort_age$age - 1000, 0)
age_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)
age_labels <- c("0_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39", 
                "40_44", "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", 
                "75_79", "80_84", "85+")
la_mort_age$agegrp <- as.character(cut(la_mort_age$age, breaks = age_breaks, labels = age_labels, right = FALSE))

# Define race in the mortality file
la_mort_age <- la_mort_age %>%
  filter(racer5 < 3)
la_mort_age$black <- ifelse(la_mort_age$racer5 ==2, 1, 0)

# Define race/ethnicity in the population file
la_pop <- la_pop %>%
  mutate(
    black_pop = rowSums(select(., c("ba_male", "ba_female"))),
    white_pop = rowSums(select(., c("wa_male", "wa_female"))),
  )
la_pop_black <- select(la_pop, county, year, agegrp, black_pop)
la_pop_white <- select(la_pop, county, year, agegrp, white_pop)

# Create parish counts of cancer deaths by type and by race/ethnicity
parish_count <- la_mort_age %>%
  group_by(cntyrsd, cancer_parish, agegrp, black, year) %>%
  summarize(
            stomach = sum(stomach, na.rm = TRUE),
            colon = sum(colon, na.rm = TRUE),
            pancreas = sum(pancreas, na.rm = TRUE),
            lung = sum(lung, na.rm = TRUE),
            breast = sum(breast, na.rm = TRUE),
            cervix = sum(cervix, na.rm = TRUE),
            prostate = sum(prostate, na.rm = TRUE),
            bladder = sum(bladder, na.rm = TRUE),
            lymphoma = sum(lymphoma, na.rm = TRUE),
            leukemia = sum(leukemia, na.rm = TRUE),
            other_site = sum(other_site, na.rm = TRUE),
            total = sum(total, na.rm = TRUE)
            )

# Join population data
la_joined_black <- parish_count %>%
  filter(black == 1) %>%
  inner_join(la_pop_black, by = c("cntyrsd" = "county", "year", "agegrp"))
la_joined_black <- rename(la_joined_black, population = black_pop)

la_joined_white <- parish_count %>%
  filter(black == 0) %>%
  inner_join(la_pop_white, by = c("cntyrsd" = "county", "year", "agegrp"))
la_joined_white <- rename(la_joined_white, population = white_pop)

la_bind <- rbind(la_joined_black, la_joined_white)

# Join standard age data for age adjustment
la_joined_stnrd <- la_bind %>%
  inner_join(stnrd_pop, by = "agegrp")

# Calculate population weights
la_joined_stnrd$stnrd_pop_weight <- (la_joined_stnrd$stnrd_pop) / (sum(stnrd_pop$stnrd_pop))

# Calculate cancer mortality rates
la_joined_stnrd$stomach_rate <- ((la_joined_stnrd$stomach) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$colon_rate <- ((la_joined_stnrd$colon) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$pancreas_rate <- ((la_joined_stnrd$pancreas) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$lung_rate <- ((la_joined_stnrd$lung) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$breast_rate <- ((la_joined_stnrd$breast) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$cervix_rate <- ((la_joined_stnrd$cervix) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$prostate_rate <- ((la_joined_stnrd$prostate) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$bladder_rate <- ((la_joined_stnrd$bladder) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$lymphoma_rate <- ((la_joined_stnrd$lymphoma) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$leukemia_rate <- ((la_joined_stnrd$leukemia) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$other_rate <- ((la_joined_stnrd$other_site) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight
la_joined_stnrd$total_rate <- ((la_joined_stnrd$total) / (la_joined_stnrd$population / 100000)) * la_joined_stnrd$stnrd_pop_weight

for (col in names(la_joined_stnrd)) {
  la_joined_stnrd[[col]][is.infinite(la_joined_stnrd[[col]])] <- NA
}
                                       
# Aggregate over age
parish_rates <- la_joined_stnrd %>%
  group_by(cntyrsd, cancer_parish, black, year) %>%
  summarize(
    stomach_rate_adj = sum(stomach_rate, na.rm = TRUE),
    colon_rate_adj = sum(colon_rate, na.rm = TRUE),
    pancreas_rate_adj = sum(pancreas_rate, na.rm = TRUE),
    lung_rate_adj = sum(lung_rate, na.rm = TRUE),
    breast_rate_adj = sum(breast_rate, na.rm = TRUE),
    cervix_rate_adj = sum(cervix_rate, na.rm = TRUE),
    prostate_rate_adj = sum(prostate_rate, na.rm = TRUE),
    bladder_rate_adj = sum(bladder_rate, na.rm = TRUE),
    lymphoma_rate_adj = sum(lymphoma_rate, na.rm = TRUE),
    leukemia_rate_adj = sum(leukemia_rate, na.rm = TRUE),
    other_rate_adj = sum(other_rate, na.rm = TRUE),
    total_rate_adj = sum(total_rate, na.rm = TRUE),
    total_crude = sum(total, na.rm = TRUE),
    population = sum(population)
    )
parish_rates$total_rate_crude <- (parish_rates$total_crude) / (parish_rates$population / 100000)

# Weight by parish population
parish_rates$stomach_weight <- (parish_rates$stomach_rate_adj) * (parish_rates$population)
parish_rates$colon_weight <- (parish_rates$colon_rate_adj) * (parish_rates$population)
parish_rates$pancreas_weight <- (parish_rates$pancreas_rate_adj) * (parish_rates$population)
parish_rates$lung_weight <- (parish_rates$lung_rate_adj) * (parish_rates$population)
parish_rates$breast_weight <- (parish_rates$breast_rate_adj) * (parish_rates$population)
parish_rates$cervix_weight <- (parish_rates$cervix_rate_adj) * (parish_rates$population)
parish_rates$prostate_weight <- (parish_rates$prostate_rate_adj) * (parish_rates$population)
parish_rates$bladder_weight <- (parish_rates$bladder_rate_adj) * (parish_rates$population)
parish_rates$lymphoma_weight <- (parish_rates$lymphoma_rate_adj) * (parish_rates$population)
parish_rates$leukemia_weight <- (parish_rates$leukemia_rate_adj) * (parish_rates$population)
parish_rates$other_weight <- (parish_rates$other_rate_adj) * (parish_rates$population)
parish_rates$total_weight <- (parish_rates$total_rate_adj) * (parish_rates$population)
parish_rates$crude_weight <- (parish_rates$total_rate_crude) * (parish_rates$population)

# Aggregate to Cancer Alley and Non-Cancer Alley parishes
cancer_alley_rates <- parish_rates %>%
  group_by(cancer_parish, black, year) %>%
  summarize(
    stomach_rate_adj_wt = sum((stomach_weight) / sum(population), na.rm = TRUE),
    colon_rate_adj_wt = sum((colon_weight) / sum(population), na.rm = TRUE),
    pancreas_rate_adj_wt = sum((pancreas_weight) / sum(population), na.rm = TRUE),
    lung_rate_adj_wt = sum((lung_weight) / sum(population), na.rm = TRUE),
    breast_rate_adj_wt = sum((breast_weight) / sum(population), na.rm = TRUE),
    cervix_rate_adj_wt = sum((cervix_weight) / sum(population), na.rm = TRUE),
    prostate_rate_adj_wt = sum((prostate_weight) / sum(population), na.rm = TRUE),
    bladder_rate_adj_wt = sum((bladder_weight) / sum(population), na.rm = TRUE),
    lymphoma_rate_adj_wt = sum((lymphoma_weight) / sum(population), na.rm = TRUE),
    leukemia_rate_adj_wt = sum((leukemia_weight) / sum(population), na.rm = TRUE),
    other_rate_adj_wt = sum((other_weight) / sum(population), na.rm = TRUE),
    total_rate_adj_wt = sum((total_weight) / sum(population), na.rm = TRUE),
    crude_rate_wt = sum((crude_weight) / sum(population, na.rm = TRUE))
            )

#Plotting sequence
ggplot(data = cancer_alley_rates,
       mapping = aes(x = year, y = total_rate_adj_wt))

#Need to add a geometric object so R knows what kind of plot I want:
ggplot(data = cancer_alley_rates,
       mapping = aes(x = year, y = total_rate_adj_wt)) +
  geom_point()

#Add labels
ggplot(data = cancer_alley_rates,
       mapping = aes(x = year, y = total_rate_adj_wt)) +
  geom_point() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality")

#Fix x-axis labels
ggplot(data = cancer_alley_rates,
       mapping = aes(x = year, y = total_rate_adj_wt)) +
  geom_point() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Better, but why do we have 4 points for each year?

#Color for Black
ggplot(data = cancer_alley_rates,
       mapping = aes(x = year, y = total_rate_adj_wt, color = black)) +
  geom_point() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Color for Black as a factor
ggplot(data = cancer_alley_rates,
       mapping = aes(x = year, y = total_rate_adj_wt, color = as.factor(black))) +
  geom_point() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Color for Cancer Alley
ggplot(data = cancer_alley_rates,
       mapping = aes(x = year, y = total_rate_adj_wt, color = cancer_parish)) +
  geom_point() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Color for Cancer Alley as factor
ggplot(data = cancer_alley_rates,
       mapping = aes(x = year, y = total_rate_adj_wt, color = as.factor(cancer_parish))) +
  geom_point() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Color for Black and Cancer Alley
ggplot(data = cancer_alley_rates, 
       mapping = aes(x = year, y = total_rate_adj_wt, color = interaction(black, cancer_parish))) +
  geom_point() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Change legend labels
ggplot(data = cancer_alley_rates, 
       mapping = aes(x = year, y = total_rate_adj_wt, color = interaction(black, cancer_parish))) +
  geom_point() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = NULL, labels = c("White, NCA", "Black, NCA", "White, CA", "Black, CA"))

#Use Smoothing
ggplot(data = cancer_alley_rates, 
       mapping = aes(x = year, y = total_rate_adj_wt, color = interaction(black, cancer_parish))) +
  geom_smooth() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = NULL, labels = c("White, NCA", "Black, NCA", "White, CA", "Black, CA"))

#Use Smoothing + Points
ggplot(data = cancer_alley_rates, 
       mapping = aes(x = year, y = total_rate_adj_wt, color = interaction(black, cancer_parish))) +
  geom_smooth() +
  geom_point() +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = NULL, labels = c("White, NCA", "Black, NCA", "White, CA", "Black, CA"))

#What about crude vs. age-adjusted?
crude_adj <- cancer_alley_rates %>%
  filter(black == 0 & cancer_parish == 0)
ggplot(data = crude_adj, 
       mapping = aes(x = year)) +
  geom_smooth(aes(y = crude_rate_wt, color = "Crude")) +
  geom_smooth(aes(y = total_rate_adj_wt, color = "Age-Adjusted")) +
  geom_point(aes(y = crude_rate_wt, color = "Crude")) +
  geom_point(aes(y = total_rate_adj_wt, color = "Age-Adjusted")) +
  labs(x = NULL,
       y = "Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality Comparison") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = NULL, labels = c("Age-Adjusted", "Crude"))


#What about other cancer types?
ggplot(data = cancer_alley_rates, 
       mapping = aes(x = year, y = lung_rate_adj_wt, color = interaction(black, cancer_parish))) +
  geom_smooth() +
  geom_point() +
  labs(x = NULL,
       y = "Lung Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = NULL, labels = c("White, NCA", "Black, NCA", "White, CA", "Black, CA"))


ggplot(data = cancer_alley_rates, 
       mapping = aes(x = year, y = colon_rate_adj_wt, color = interaction(black, cancer_parish))) +
  geom_smooth() +
  geom_point() +
  labs(x = NULL,
       y = "Lung Cancer Deaths per 100,000",
       title = "Lousiana Cancer Mortality") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = NULL, labels = c("White, NCA", "Black, NCA", "White, CA", "Black, CA"))


#Mapping Cancer Rates in Louisiana

#Create quartiles of cancer mortality rates by race
cancer_quartile <- parish_rates %>%
  group_by(cntyrsd, black, year) %>%
  summarize(
    total_rate_adj_wt = sum((total_weight) / sum(population), na.rm = TRUE)
  )

cancer_quartile <- cancer_quartile %>%
  group_by(cntyrsd, black) %>%
  summarize(
    total_rate_adj_wt = mean(total_rate_adj_wt, na.rm = TRUE)
  )

cancer_quartile <- cancer_quartile %>%
  group_by(black) %>%
  mutate(quartile = ntile(total_rate_adj_wt, 4))

cancer_quartile_check <- cancer_quartile %>%
  group_by(black, quartile) %>%
  summarize(
    total_rate_adj_wt = mean(total_rate_adj_wt, na.rm = TRUE)
  )

install.packages("maps")
library(maps)
la_map <- map_data("county", region = "louisiana")

fips_map <- county.fips %>%
  filter(fips>=22000 & fips<23000) %>%
  mutate(fips = fips - 22000) %>%
  separate(polyname, into = c("region", "subregion"), sep = ",", remove = TRUE) %>%
  distinct(fips, .keep_all = TRUE) %>%
  mutate(subregion = ifelse(subregion == "st martin:north", "st martin", subregion))

map_join = la_map %>%
  inner_join(fips_map, by = c("region", "subregion"))

map_final = cancer_quartile %>%
  inner_join(map_join, by = c("cntyrsd" = "fips"))

map_final_black <- map_final %>%
  filter(black == 1)

map_final_white <- map_final %>%
  filter(black == 0)

ggplot(data = map_final_black, mapping = aes(x = long, y = lat, group = group, fill = as.factor(quartile))) + 
  geom_polygon(color = "limegreen", size = 0.1) +
  scale_fill_manual(values = c("skyblue", "royalblue", "blue3", "blue4"), name = "Quartile") +
  coord_map('mercator') +
  theme_bw()

ggplot(data = map_final_white, mapping = aes(x = long, y = lat, group = group, fill = as.factor(quartile))) + 
  geom_polygon(color = "limegreen", size = 0.1) +
  scale_fill_manual(values = c("skyblue", "royalblue", "blue3", "blue4"), name = "Quartile") +
  coord_map('mercator') +
  theme_bw()

ggplot(data = map_final_white, mapping = aes(x = long, y = lat, group = group, fill = as.factor(quartile))) + 
  geom_polygon(color = "limegreen", size = 0.1) +
  scale_fill_manual(values = c("skyblue", "royalblue", "blue3", "blue4"), name = "Quartile") +
  coord_map('mercator') +
  theme_bw() +
  labs(x = NULL,
       y = NULL,
       title = "Lousiana Cancer Mortality by Parish for White Decedents, 2005-2019") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())

save.image(file = "assignments/tutorial_8.RData")

#wesanderson
library(wesanderson)
ggplot(data = map_final_white, mapping = aes(x = long, y = lat, group = group, fill = as.factor(quartile))) + 
  geom_polygon(color = "limegreen", size = 0.1) +
  scale_fill_manual(values = wes_palette("Royal1", n = 4), name = "Quartile") +
  coord_map('mercator') +
  theme_bw() +
  labs(x = NULL,
       y = NULL,
       title = "Lousiana Cancer Mortality by Parish, 2005-2019") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())