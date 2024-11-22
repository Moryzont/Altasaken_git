#Clear the "Global Environment"
rm(list = ls())
library(tidyverse)
library(lubridate)
library(ggplot2)

# Adjust the path as needed
setwd("/Users/vetlewisloffsandring/Documents/Altasaken")

options(scipen = 999)


r_data_tom_2022 <- read_csv("renset_data.csv")
data_2022 <- read_csv("2022_conc_renset.csv")
data_2023 <- read_csv("2023_conc_renset.csv")
data_2019 <- read_csv("2019_conc_renset.csv")


#Fjerner tv 2019

data_2019 <- data_2019 %>%
  mutate(
    extracted_pattern = str_extract(conc, "\\d\\d\\.\\d\\d\\D|\\d\\d\\:\\d\\d|\\d\\d\\s\\.\\d\\d\\D|\\d\\d\\.\\s\\d\\d\\D|\\d\\d\\s\\.\\s\\d\\d\\D|\\d\\d\\s\\d\\d\\D" )
  )

new_df <- data_2019 %>%
  filter(!is.na(extracted_pattern))


new_df <- new_df %>%
  filter(!grepl("(kl\\.)|(fakkeltog)|(\\bKL\\b)|(\\sKl\\s)|(kl\\s\\d+)|(kl\\s)", new_df$conc, ignore.case = TRUE))

data_2019_utv <- data_2019 %>%
  anti_join(new_df, by = "conc") 


#Fjerner tv 2022

data_2022 <- data_2022 %>%
  mutate(
    extracted_pattern = str_extract(conc, "\\d\\d\\.\\d\\d\\D|\\d\\d\\:\\d\\d|\\d\\d\\s\\.\\d\\d\\D|\\d\\d\\.\\s\\d\\d\\D|\\d\\d\\s\\.\\s\\d\\d\\D|\\d\\d\\s\\d\\d\\D" )
  )

new_df <- data_2022 %>%
  filter(!is.na(extracted_pattern))


new_df <- new_df %>%
  filter(!grepl("(kl\\.)|(fakkeltog)|(\\bKL\\b)|(\\sKl\\s)|(kl\\s\\d+)|(kl\\s)", new_df$conc, ignore.case = TRUE))

data_2022_utv <- data_2022 %>%
  anti_join(new_df, by = "conc") 

#Fjerner tv 2023

r_data_tom_2022 <- r_data_tom_2022 %>%
  mutate(
    extracted_pattern = str_extract(conc, "\\d\\d\\.\\d\\d\\D|\\d\\d\\:\\d\\d|\\d\\d\\s\\.\\d\\d\\D|\\d\\d\\.\\s\\d\\d\\D|\\d\\d\\s\\.\\s\\d\\d\\D|\\d\\d\\s\\d\\d\\D" )
  )

new_df <- r_data_tom_2022 %>%
  filter(!is.na(extracted_pattern))


new_df <- new_df %>%
  filter(!grepl("(kl\\.)|(fakkeltog)|(\\bKL\\b)|(\\sKl\\s)|(kl\\s\\d+)|(kl\\s)", new_df$conc, ignore.case = TRUE))

r_data_tom_2022_utv <- r_data_tom_2022 %>%
  anti_join(new_df, by = "conc") 


#Fjerner tv stortabellen

data_2023 <- data_2023 %>%
  mutate(
    extracted_pattern = str_extract(conc, "\\d\\d\\.\\d\\d\\D|\\d\\d\\:\\d\\d|\\d\\d\\s\\.\\d\\d\\D|\\d\\d\\.\\s\\d\\d\\D|\\d\\d\\s\\.\\s\\d\\d\\D|\\d\\d\\s\\d\\d\\D" )
  )

new_df <- data_2023 %>%
  filter(!is.na(extracted_pattern))


new_df <- new_df %>%
  filter(!grepl("(kl\\.)|(fakkeltog)|(\\bKL\\b)|(\\sKl\\s)|(kl\\s\\d+)|(kl\\s)", new_df$conc, ignore.case = TRUE))

data_2023_utv <- data_2023 %>%
  anti_join(new_df, by = "conc") 



#Trekker ut 2022 fra stortabellen
data_2022_orginal <- r_data_tom_2022 %>%
  filter(year == 2022)
data_2019_orginal <- r_data_tom_2022 %>%
  filter(year == 2019)

data_1990_orginal <- r_data_tom_2022 %>%
  filter(year == 1990)
#2019 fjerne Altasaken 
data_2019_orginal_filtered <- data_2019_orginal %>%
  filter(str_count(conc, "\\s") >= 2)

#Fjerne Altasaken totalt
r_data_tom_2022_filtered <- r_data_tom_2022_utv %>%
  filter(str_count(conc, "\\s") >= 2)


#Sammenligne
summary_original_2019 <- data_2019_orginal_filtered %>% summarize(across(where(is.numeric), mean, na.rm = TRUE))
summary_processed_2019 <- data_2019_utv %>% summarize(across(where(is.numeric), mean, na.rm = TRUE))

summary_original_2022 <- data_2022_orginal %>% summarize(across(where(is.numeric), mean, na.rm = TRUE))
summary_processed_2022 <- data_2022_utv %>% summarize(across(where(is.numeric), mean, na.rm = TRUE))


# Histogram for 2019 original vs. processed
ggplot() +
  geom_histogram(data = data_2019_orginal_filtered, aes(x = samesaken, fill = "Original"), alpha = 0.5) +
  geom_histogram(data = data_2019_utv, aes(x = samesaken, fill = "Processed"), alpha = 0.5) +
  labs(title = "2019 Thematic Labels Distribution", x = "Thematic Label Score", fill = "Dataset")

# Repeat for 2022


combined_data <- bind_rows(
  data_2019_utv %>% mutate(year = 2019, dataset = "Processed"),
  data_2019_orginal %>% mutate(year = 2019, dataset = "Original"),
  data_2022_utv %>% mutate(year = 2022, dataset = "Processed"),
  data_2022_orginal %>% mutate(year = 2022, dataset = "Original")
)
t_test_2019 <- t.test(data_2019_orginal$samesaken, data_2019_utv$samesaken)


####Visualiseringer




#Fjerner under 0.5
r_data_tom_2022_utv_trunkert <- r_data_tom_2022_utv %>%
  mutate(across(where(is.numeric), ~if_else(. < 0.5, 0, .)))# Function to apply the condition on each row
r_data_tom_2022_filtered_trunkert <- r_data_tom_2022_filtered %>%
  mutate(across(where(is.numeric), ~if_else(. < 0.5, 0, .)))# Function to apply the condition on each row


# Før fjerning av mellomrom
summary_table <- r_data_tom_2022_utv_trunkert %>%
  group_by(year) %>%
  summarise(
    average_value_sam = mean(samesaken, na.rm = TRUE),
    average_value_nat = mean(`natur og miljø`, na.rm = TRUE)
  )


ggplot(summary_table, aes(x = year)) +
  geom_smooth(aes(y = average_value_sam, color = "Smoothed Average samesaken"), method = "loess", se = FALSE) +
  # geom_smooth(aes(y = average_value_nat, color = "Smoothed Average natur og miljø"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_value_sam, color = "Unsmoothed Average samesaken"), size = 1, alpha = 0.4) +
  #  geom_line(aes(y = average_value_nat, color = "Unsmoothed Average natur og miljø"), size = 1, alpha = 0.4) +
  scale_x_continuous(breaks=seq(1979, 2023, by = 2)) +  # Adjust breaks to show every 5th year
  labs(
    title = "mellomrom og deler av tv ikke fjernet",
    x = "Year",
    y = "Average label score, 0 = no relationship 1 = strong relationship",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Smoothed Average samesaken" = "lightsalmon4",
                                #  "Smoothed Average natur og miljø" = "darkseagreen4",
                                "Unsmoothed Average samesaken" = "lightsalmon2"
                                #      "Unsmoothed Average natur og miljø" = "darkseagreen3"
  )) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))


# Etter fjerning av mellomrom
summary_table <- r_data_tom_2022_filtered_trunkert %>%
  group_by(year) %>%
  summarise(
    average_value_sam = mean(samesaken, na.rm = TRUE),
    average_value_nat = mean(`natur og miljø`, na.rm = TRUE)
  )


ggplot(summary_table, aes(x = year)) +
  geom_smooth(aes(y = average_value_sam, color = "Smoothed Average samesaken"), method = "loess", se = FALSE) +
  # geom_smooth(aes(y = average_value_nat, color = "Smoothed Average natur og miljø"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_value_sam, color = "Unsmoothed Average samesaken"), size = 1, alpha = 0.4) +
  #  geom_line(aes(y = average_value_nat, color = "Unsmoothed Average natur og miljø"), size = 1, alpha = 0.4) +
  scale_x_continuous(breaks=seq(1979, 2023, by = 2)) +  # Adjust breaks to show every 5th year
  labs(
    title = "MED Mellomrom fjernet. Text snippets, average yearly values, \"The sami cause\",  N = 87 854",
    x = "Year",
    y = "Average label score, 0 = no relationship 1 = strong relationship",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Smoothed Average samesaken" = "lightsalmon4",
                                #  "Smoothed Average natur og miljø" = "darkseagreen4",
                                "Unsmoothed Average samesaken" = "lightsalmon2"
                                #      "Unsmoothed Average natur og miljø" = "darkseagreen3"
  )) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))
