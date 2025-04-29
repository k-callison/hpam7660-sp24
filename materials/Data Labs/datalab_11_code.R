library(dplyr)

brfss_data <- readRDS("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/brfss_datalab11.rds")

brfss_sample <- brfss_data %>%
  mutate(
    FP_HEALTH = case_when(
      GENHLTH %in% c(4, 5) ~ 1,
      GENHLTH %in% c(1, 2, 3) ~ 0
    ),
    ANY_MNTHLTH = case_when(
      MENTHLTH == 0 ~ 1,
      MENTHLTH >= 1 & MENTHLTH <= 30 ~ 0
    ),
    ANY_PHYSHLTH = case_when(
      PHYSHLTH == 0 ~ 1,
      PHYSHLTH >= 1 & PHYSHLTH <= 30 ~ 0
    ),
    TREAT = ifelse(STATEFIP == 22, 1, 0),
    POST = ifelse(YEAR >= 2016, 1, 0)
  )

uninsured_dd <- lm(UNINSURED ~ TREAT * POST, data = brfss_sample)
summary(uninsured_dd)

brfss_sample %>%
  filter(STATEFIP == 22, POST == 0) %>%
  summarize(MEAN_UNINSURED =  mean(UNINSURED, na.rm=TRUE))

#Absolute effect = -13.2 percentage points
#Relative effect = (-0.132/0.362)*100 = -36.5 percent

pers_doc_dd <- lm(PERSDOC2 ~ TREAT * POST, data = brfss_sample)
summary(pers_doc_dd)

brfss_sample %>%
  filter(STATEFIP == 22, POST == 0) %>%
  summarize(MEAN_PERSDOC2 =  mean(PERSDOC2, na.rm=TRUE))

#Absolute effect = 3.7 percentage points
#Relative effect = (0.037/0.748)*100 = 4.9 percent

medcost_dd <- lm(MEDCOST ~ TREAT * POST, data = brfss_sample)
summary(medcost_dd)

brfss_sample %>%
  filter(STATEFIP == 22, POST == 0) %>%
  summarize(MEAN_MEDCOST =  mean(MEDCOST, na.rm=TRUE))

#Absolute effect = -3.6 percentage points
#Relative effect = (-0.036/0.358)*100 = 10.1 percent

fp_health_dd <- lm(FP_HEALTH ~ TREAT * POST, data = brfss_sample)
summary(fp_health_dd)

brfss_sample %>%
  filter(STATEFIP == 22, POST == 0) %>%
  summarize(MEAN_FP_HEALTH =  mean(FP_HEALTH, na.rm=TRUE))

#Absolute effect = -2.5 percentage points
#Relative effect = (-0.025/0.405)*100 = -6.2 percent

physhlth_dd <- lm(ANY_PHYSHLTH  ~ TREAT * POST, data = brfss_sample)
summary(physhlth_dd)

brfss_sample %>%
  filter(STATEFIP == 22, POST == 0) %>%
  summarize(MEAN_ANY_PHYSHLTH =  mean(ANY_PHYSHLTH, na.rm=TRUE))

#Absolute effect = -3.3 percentage points
#Relative effect = (-0.033/0.475)*100 = -6.9 percent

menthlth_dd <- lm(ANY_MNTHLTH  ~ TREAT * POST, data = brfss_sample)
summary(menthlth_dd)

brfss_sample %>%
  filter(STATEFIP == 22, POST == 0) %>%
  summarize(MEAN_ANY_MNTHLTH =  mean(ANY_MNTHLTH, na.rm=TRUE))

#Absolute effect = -6.3 percentage points
#Relative effect = (-0.063/0.537)*100 = -11.7 percent

