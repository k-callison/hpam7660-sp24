#Data Lab 9 Code

library(dplyr)
library(ggplot2)

#Step 2 & Step 3
acs_data <- readRDS("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/acs_datalab9.rds")

acs_format <- acs_data %>%
  mutate(
    UNINSURED = ifelse(HCOVANY == 1, 1, 0),
    MEDICAID = ifelse(HINSCAID == 2, 1, 0)
  )

acs_LA <- acs_format %>% 
  filter(STATEFIP == 22,
         AGE >=26 & AGE <=64,
         POVERTY<=138
)

acs_Gulf <- acs_format %>% 
  filter(STATEFIP %in% c(1, 12, 13, 28, 48),
         AGE >=26 & AGE <=64,
         POVERTY<=138
)

acs_LA_graph <- acs_LA %>%
  group_by(YEAR) %>%
  summarize(
    MEAN_MEDICAID = mean(MEDICAID, na.rm = TRUE),
    MEAN_UNINSURED = mean(UNINSURED, na.rm = TRUE)
  )

ggplot(acs_LA_graph, aes(x = YEAR)) +
  geom_line(aes(y = MEAN_MEDICAID, color = "medicaid")) +
  geom_point(aes(y = MEAN_MEDICAID, color = "medicaid")) +
  geom_line(aes(y = MEAN_UNINSURED, color = "uninsured")) +
  geom_point(aes(y = MEAN_UNINSURED, color = "uninsured")) +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  labs(title = "Insurance Coverage in Louisiana, 2012 - 2019",
       x = "Year",
       y = "Share Covered") +
  ylim(0, 0.8) +
  theme_minimal()

acs_Gulf_graph <- acs_Gulf %>%
  group_by(YEAR) %>%
  summarize(
    MEAN_MEDICAID = mean(MEDICAID, na.rm = TRUE),
    MEAN_UNINSURED = mean(UNINSURED, na.rm = TRUE)
  )

ggplot(acs_Gulf_graph, aes(x = YEAR)) +
  geom_line(aes(y = MEAN_MEDICAID, color = "medicaid")) +
  geom_point(aes(y = MEAN_MEDICAID, color = "medicaid")) +
  geom_line(aes(y = MEAN_UNINSURED, color = "uninsured")) +
  geom_point(aes(y = MEAN_UNINSURED, color = "uninsured")) +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  labs(title = "Insurance Coverage in Gulf South, 2012 - 2019",
       x = "Year",
       y = "Share Covered") +
  ylim(0, 0.8) +
  theme_minimal()

#Step 4
acs_LA <- acs_LA %>%
  mutate(RACE2 = case_when(
    HISPAN == 1 ~ 5,
    HISPAN == 0 ~ RACE
  ))

acs_LA_race <- acs_LA %>%
  filter(RACE2 %in% c(1,2,5))

acs_LA_race_graph <- acs_LA_race %>%
  group_by(RACE2, YEAR) %>%
  summarize(
    MEAN_MEDICAID = mean(MEDICAID, na.rm = TRUE),
    MEAN_UNINSURED = mean(UNINSURED, na.rm = TRUE)
  )

ggplot(acs_LA_race_graph, aes(x = YEAR)) +
  geom_line(aes(y = MEAN_UNINSURED, color = paste("uninsured", RACE2))) +
  geom_point(aes(y = MEAN_UNINSURED, color = paste("uninsured", RACE2))) +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  labs(title = "Uninsured Rate in Louisiana by Race, 2012 - 2019",
       x = "Year",
       y = "Share Covered",
       color = "Race") +
  scale_color_discrete(labels = c("white", "Black/African American", "Hispanic")) +
  ylim(0, 0.8) +
  theme_minimal()

acs_LA_sex_graph <- acs_LA %>%
  group_by(SEX, YEAR) %>%
  summarize(
    MEAN_MEDICAID = mean(MEDICAID, na.rm = TRUE),
    MEAN_UNINSURED = mean(UNINSURED, na.rm = TRUE)
  )

ggplot(acs_LA_sex_graph, aes(x = YEAR)) +
  geom_line(aes(y = MEAN_UNINSURED, color = paste("uninsured", SEX))) +
  geom_point(aes(y = MEAN_UNINSURED, color = paste("uninsured", SEX))) +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  labs(title = "Uninsured Rate in Louisiana by Sex, 2012 - 2019",
       x = "Year",
       y = "Share Covered",
       color = "Sex") +
  scale_color_discrete(labels = c("Men", "Women")) +
  ylim(0, 0.8) +
  theme_minimal()
