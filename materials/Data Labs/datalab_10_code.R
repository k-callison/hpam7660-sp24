library(dplyr)
library(ggplot2)

acs_data <- readRDS("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/acs_datalab9.rds")

acs_format <- acs_data %>%
  filter(STATEFIP %in% c(1, 12, 13, 22, 28, 48),
         AGE >=26 & AGE <=64,
         POVERTY<=138) %>%
  mutate(
    UNINSURED = ifelse(HCOVANY == 1, 1, 0),
    MEDICAID = ifelse(HINSCAID == 2, 1, 0)
  )

saveRDS(acs_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/acs_format.rds")

#My Code to Load BRFSS Data
library(haven)

brfss_2012 <- read_xpt("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/LLCP2012.XPT")
brfss_2012_small <- select(brfss_2012, `_STATE`, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, MEDCOST, AGE, `_RACEGR2`, EDUCA, INCOME2, SEX)
brfss_2012_format <- brfss_2012_small %>%
  rename(RACE = `_RACEGR2`,
         STATEFIP = `_STATE`,
         INCOME = INCOME2) %>%
  mutate(YEAR = 2012)
saveRDS(brfss_2012_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_2012_format.rds")

brfss_2013 <- read_xpt("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/LLCP2013.XPT")
brfss_2013_small <- select(brfss_2013, `_STATE`, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, MEDCOST, `_AGE80`, `_RACEGR3`, EDUCA, INCOME2, SEX)
brfss_2013_format <- brfss_2013_small %>%
  rename(RACE = `_RACEGR3`,
         STATEFIP = `_STATE`,
         INCOME = INCOME2,
         AGE = `_AGE80`) %>%
  mutate(YEAR = 2013)
saveRDS(brfss_2013_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_2013_format.rds")

brfss_2014 <- read_xpt("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/LLCP2014.XPT")
brfss_2014_small <- select(brfss_2014, `_STATE`, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, MEDCOST, `_AGE80`, `_RACEGR3`, EDUCA, INCOME2, SEX)
brfss_2014_format <- brfss_2014_small %>%
  rename(RACE = `_RACEGR3`,
         STATEFIP = `_STATE`,
         INCOME = INCOME2,
         AGE = `_AGE80`) %>%
  mutate(YEAR = 2014)
saveRDS(brfss_2014_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_2014_format.rds")

brfss_2015 <- read_xpt("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/LLCP2015.XPT")
brfss_2015_small <- select(brfss_2015, `_STATE`, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, MEDCOST, `_AGE80`, `_RACEGR3`, EDUCA, INCOME2, SEX)
brfss_2015_format <- brfss_2015_small %>%
  rename(RACE = `_RACEGR3`,
         STATEFIP = `_STATE`,
         INCOME = INCOME2,
         AGE = `_AGE80`) %>%
  mutate(YEAR = 2015)
saveRDS(brfss_2015_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_2015_format.rds")

brfss_2016 <- read_xpt("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/LLCP2016.XPT")
brfss_2016_small <- select(brfss_2016, `_STATE`, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, MEDCOST, `_AGE80`, `_RACEGR3`, EDUCA, INCOME2, SEX)
brfss_2016_format <- brfss_2016_small %>%
  rename(RACE = `_RACEGR3`,
         STATEFIP = `_STATE`,
         INCOME = INCOME2,
         AGE = `_AGE80`) %>%
  mutate(YEAR = 2016)
saveRDS(brfss_2016_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_2016_format.rds")

brfss_2017 <- read_xpt("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/LLCP2017.XPT")
brfss_2017_small <- select(brfss_2017, `_STATE`, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, MEDCOST, `_AGE80`, `_RACEGR3`, EDUCA, INCOME2, SEX)
brfss_2017_format <- brfss_2017_small %>%
  rename(RACE = `_RACEGR3`,
         STATEFIP = `_STATE`,
         INCOME = INCOME2,
         AGE = `_AGE80`) %>%
  mutate(YEAR = 2017)
saveRDS(brfss_2017_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_2017_format.rds")

brfss_2018 <- read_xpt("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/LLCP2018.XPT")
brfss_2018_small <- select(brfss_2018, `_STATE`, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, MEDCOST, `_AGE80`, `_RACEGR3`, EDUCA, INCOME2, SEX1)
brfss_2018_format <- brfss_2018_small %>%
  rename(RACE = `_RACEGR3`,
         STATEFIP = `_STATE`,
         INCOME = INCOME2,
         AGE = `_AGE80`,
         SEX = SEX1) %>%
  mutate(YEAR = 2018)
saveRDS(brfss_2018_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_2018_format.rds")

brfss_2019 <- read_xpt("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/LLCP2019.XPT")
brfss_2019_small <- select(brfss_2019, `_STATE`, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, MEDCOST, `_AGE80`, `_RACEGR3`, EDUCA, INCOME2, SEXVAR)
brfss_2019_format <- brfss_2019_small %>%
  rename(RACE = `_RACEGR3`,
         STATEFIP = `_STATE`,
         INCOME = INCOME2,
         AGE = `_AGE80`,
         SEX = SEXVAR) %>%
  mutate(YEAR = 2019)
saveRDS(brfss_2019_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_2019_format.rds")


path <- "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/"

# Create a vector of filenames
years <- 2012:2019
filenames <- paste0(path,"brfss_", years, "_format.rds")

# Load and append all datasets into one
brfss_combined <- lapply(filenames, readRDS) %>% 
  bind_rows()

brfss_combined_format <- brfss_combined %>%
  mutate(GENHLTH = ifelse(GENHLTH %in% c(7, 9), NA, GENHLTH),
         PHYSHLTH = case_when(
           PHYSHLTH %in% c(77,99) ~ NA_real_,
           PHYSHLTH == 88 ~ 0,
           TRUE ~ PHYSHLTH),
         MENTHLTH = case_when(
           MENTHLTH %in% c(77,99) ~ NA_real_,
           MENTHLTH == 88 ~ 0,
           TRUE ~ MENTHLTH),
         HLTHPLN1 = ifelse(HLTHPLN1 %in% c(7,9), NA_real_, HLTHPLN1),
         PERSDOC2 = case_when(
           PERSDOC2 %in% c(7,9) ~ NA_real_,
           PERSDOC2 == 2 ~ 1,
           PERSDOC2 == 3 ~ 0,
           TRUE ~ PERSDOC2),
        MEDCOST = case_when(
          MEDCOST %in% c(7,9) ~ NA_real_,
          MEDCOST == 2 ~ 0,
          TRUE ~ MEDCOST),
        AGE = case_when(
          AGE %in% c(7,9) ~ NA_real_,
          AGE > 80 ~ 80,
          TRUE ~ AGE),
        RACE = ifelse(RACE == 9, NA_real_, RACE),
        EDUCA = ifelse(EDUCA == 9, NA_real_, EDUCA),
        INCOME = ifelse(INCOME %in% c(77,99), NA_real_, INCOME),
        SEX = ifelse(SEX %in% c(7, 9), NA_real_, SEX),
      )

saveRDS(brfss_combined_format, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/acs_datalab10.rds")

#Transform Genhealth and plot over time

brfss_restrict <- brfss_combined_format %>%
  filter(AGE>= 26 & AGE<=64,
         INCOME <= 5,
         STATEFIP %in% c(1, 12, 13, 22, 28, 48)) %>%
         mutate(GENHLTH2 = case_when(
           GENHLTH == 1 ~ 5,
           GENHLTH == 2 ~ 4,
           GENHLTH == 3 ~ 3,
           GENHLTH == 4 ~ 2,
           GENHLTH == 5 ~ 1),
           UNINSURED = ifelse(HLTHPLN1 == 1, 0, 1),
           TREAT = ifelse(STATEFIP == 22, 1, 0),
           PF_HLTH = ifelse(GENHLTH %in% c(4,5), 1, 0)
         )

saveRDS(brfss_restrict, file = "/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_restrict.rds")

brfss_summary <- brfss_restrict %>%
  group_by(YEAR, TREAT) %>%
  summarize(
    MEAN_GENHLTH2 = mean(GENHLTH2, na.rm = TRUE),
    MEAN_PHYSHLTH = mean(PHYSHLTH, na.rm = TRUE),
    MEAN_MENTHLTH = mean(MENTHLTH, na.rm = TRUE),
    MEAN_PERSDOC2 = mean(PERSDOC2, na.rm = TRUE),
    MEAN_MEDCOST = mean(MEDCOST, na.rm = TRUE),
    MEAN_UNINSURED = mean(UNINSURED, na.rm = TRUE),
    MEAN_PF_HLTH = mean(PF_HLTH, na.rm = TRUE)
  )


ggplot(brfss_summary, aes(x = YEAR)) +
  geom_line(data = subset(brfss_summary, TREAT == 1), 
            aes(y = MEAN_UNINSURED, color = "Louisiana")) +
  geom_point(data = subset(brfss_summary, TREAT == 1), 
             aes(y = MEAN_UNINSURED, color = "Louisiana")) +
  geom_line(data = subset(brfss_summary, TREAT == 0), 
            aes(y = MEAN_UNINSURED, color = "Gulf South")) +
  geom_point(data = subset(brfss_summary, TREAT == 0), 
             aes(y = MEAN_UNINSURED, color = "Gulf South")) +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  labs(title = "Insurance Coverage in Louisiana & Gulf South States",
       x = "Year",
       y = "Share Uninsured",
       color = "") +
  ylim(0, 0.6) +
  theme_minimal()

ggplot(brfss_summary, aes(x = YEAR)) +
  geom_line(data = subset(brfss_summary, TREAT == 1), 
            aes(y = MEAN_PERSDOC2, color = "Louisiana")) +
  geom_point(data = subset(brfss_summary, TREAT == 1), 
             aes(y = MEAN_PERSDOC2, color = "Louisiana")) +
  geom_line(data = subset(brfss_summary, TREAT == 0), 
            aes(y = MEAN_PERSDOC2, color = "Gulf South")) +
  geom_point(data = subset(brfss_summary, TREAT == 0), 
             aes(y = MEAN_PERSDOC2, color = "Gulf South")) +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  labs(title = "Usual Source of Care in Louisiana & Gulf South States",
       x = "Year",
       y = "Share with Usual Source of Care",
       color = "Group") +
  ylim(0, 1) +
  theme_minimal()

ggplot(brfss_summary, aes(x = YEAR)) +
  geom_line(data = subset(brfss_summary, TREAT == 1), 
            aes(y = MEAN_MEDCOST, color = "Louisiana")) +
  geom_point(data = subset(brfss_summary, TREAT == 1), 
             aes(y = MEAN_MEDCOST, color = "Louisiana")) +
  geom_line(data = subset(brfss_summary, TREAT == 0), 
            aes(y = MEAN_MEDCOST, color = "Gulf South")) +
  geom_point(data = subset(brfss_summary, TREAT == 0), 
             aes(y = MEAN_MEDCOST, color = "Gulf South")) +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  labs(title = "Forgo Care Due to Cost in Louisiana & Gulf South States",
       x = "Year",
       y = "Share Forgoing Care",
       color = "") +
  ylim(0, 0.6) +
  theme_minimal()


ggplot(brfss_summary, aes(x = YEAR)) +
  geom_line(data = subset(brfss_summary, TREAT == 1), 
            aes(y = MEAN_PF_HLTH, color = "Louisiana")) +
  geom_point(data = subset(brfss_summary, TREAT == 1), 
             aes(y = MEAN_PF_HLTH, color = "Louisiana")) +
  geom_line(data = subset(brfss_summary, TREAT == 0), 
            aes(y = MEAN_PF_HLTH, color = "Gulf South")) +
  geom_point(data = subset(brfss_summary, TREAT == 0), 
             aes(y = MEAN_PF_HLTH, color = "Gulf South")) +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  labs(title = "Forgo Care Due to Cost in Louisiana & Gulf South States",
       x = "Year",
       y = "Share Forgoing Care",
       color = "Group") +
  ylim(0, 0.6) +
  theme_minimal()


