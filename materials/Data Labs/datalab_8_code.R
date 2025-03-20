#Data Lab 8 Code

library(dplyr)
library(ggplot2)

#Step 2
acs_data <- readRDS("/Users/kcallison/Dropbox/Documents/Data/HPAM7660/acs_data.rds")

#Step 3
glimpse(acs_data)

#Step 4
acs_format <- acs_data %>%
  mutate(
    UNINSURED = ifelse(HCOVANY == 1, 1, 0),
    MEDICAID = ifelse(HINSCAID == 2, 1, 0)
  )

table(acs_format$UNINSURED)
table(acs_format$MEDICAID)

#Step 5
acs_LA <- acs_format %>% filter(STATEFIP == 22)
acs_Gulf <- acs_format %>% filter(STATEFIP %in% c(1, 12, 13, 28, 48))

#Step 6
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
  ylim(0, 0.4) +
  theme_minimal()


acs_GS_graph <- acs_Gulf %>%
  group_by(YEAR) %>%
  summarize(
    MEAN_MEDICAID = mean(MEDICAID, na.rm = TRUE),
    MEAN_UNINSURED = mean(UNINSURED, na.rm = TRUE)
  )

ggplot(acs_GS_graph, aes(x = YEAR)) +
  geom_line(aes(y = MEAN_MEDICAID, color = "medicaid")) +
  geom_point(aes(y = MEAN_MEDICAID, color = "medicaid")) +
  geom_line(aes(y = MEAN_UNINSURED, color = "uninsured")) +
  geom_point(aes(y = MEAN_UNINSURED, color = "uninsured")) +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  labs(title = "Insurance Coverage in Louisiana, 2012 - 2019",
       x = "Year",
       y = "Share Covered") +
  ylim(0, 0.4) +
  theme_minimal()