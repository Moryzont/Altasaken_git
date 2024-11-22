rm(list=ls()) # Clear de "Global Environment"
setwd("/Users/vetlewisloffsandring/Documents/Altasaken") # Den ser ikke slik ut på windows
options(scipen = 999)
options(encoding = "UTF-8")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)


### VISUALISERING ####

renset_data_pre2023 <- read_csv("renset_data.csv")
renset_data_2023 <- read_csv("2023_conc_renset.csv")
renset_data_pre2023 <- subset(renset_data_pre2023, select = -c(...1, extracted_pattern))

renset_data_2023[, c(9:74)] <- sapply(renset_data_2023[, c(9:74)], as.numeric)

colnames(renset_data_2023) <- gsub("\\.", " ", colnames(renset_data_2023))

# Replace periods and hyphens with spaces in renset_data_pre2023
colnames(renset_data_pre2023) <- gsub("[.-]", " ", colnames(renset_data_pre2023))

# Replace periods and hyphens with spaces in renset_data_2023
colnames(renset_data_2023) <- gsub("[.-]", " ", colnames(renset_data_2023))

renset_data <- renset_data_pre2023


# 

renset_data <- rbind(renset_data_pre2023, renset_data_2023)

### TALL TRANSFORMASJONER
# Fjerner alt under 0.5 
renset_data_trunkert <- renset_data %>%
  mutate(across(where(is.numeric), ~if_else(. < 0.5, 0, .)))# Function to apply the condition on each row

# # Trekker ut de under 0.5 ### Bare for sjekking 
# selected_data <- renset_data %>%
#   select(3, 8, 10:74)
# 
# selected_data_max <- selected_data %>%
#   mutate(Max_Value = pmap_dbl(select(., where(is.numeric)), ~ max(c(...), na.rm = TRUE)))
# 
# filtered_selected_data_max <- selected_data_max %>%
#   filter(Max_Value < 0.5)

#### Sjekker hvor mange det er av enkelte fraser##
altasaken_data <- renset_data %>%
  filter(conc == "Altasaken" | conc == "Alta saken" | conc == "Alta-saken" | conc == "Alta-saka" | conc == "Alta saka")

# Fjerner de som bare har Altasaken

renset_data_trunkert <- anti_join(renset_data_trunkert, altasaken_data, by = "conc")
# #Beholder bare topp 10% av rowwise labels -- USIKKER PÅ OM DENNE FUNKER
# transform_row <- function(row) {
#   max_value <- max(row, na.rm = TRUE) # Find the max value in the row, ignoring NA
#   threshold <- max_value * 0.9 # Calculate 10% less than the max value as the threshold
#   
#   # Replace values that are less than the threshold with 0
#   row <- ifelse(row >= threshold, row, 0)
#   return(row)
# }

# Apply the function to each row
# Assuming all columns you want to apply this to are numeric
# renset_data_transformed <- renset_data %>%
#   rowwise() %>%
#   mutate(across(where(is.numeric), transform_row, .names = "modified_{.col}")) %>%
#   ungroup()

##### VISUALIZATION #####

### ABSOLUTT FREKVENS, ÅRLIG ###

antall_table <- renset_data_trunkert %>%
  filter(year >= 1983) %>%
  group_by(year) %>%
  summarise(count = n())  # 'count' will hold the number of observations per year

# Absolutt frekvens.

ggplot(antall_table, aes(x = year, y = count)) +
  geom_vline(aes(xintercept = year), linetype = "solid", alpha = 0.1) +  # Draw vertical lines for each year
  # Add smoothed line first to be behind
  geom_smooth(aes(color = "Smoothed"), method = "loess", se = FALSE, size = 1) +
  # Add unsmoothed line on top
  geom_line(aes(color = "Unsmoothed"), size = 1) +
  # Adjust colors with adjusted alpha values
  scale_color_manual(
    values = c(
      "Smoothed" = alpha("deepskyblue2", 0.4),
      "Unsmoothed" = alpha("deepskyblue4", 0.9)
    )
  ) +
  # Customize x-axis
  scale_x_continuous(breaks = seq(1983, 2023, by = 2)) +  # Adjust breaks to show every 2 years
  # Add labels
  labs(
    title = "Yearly number of observations related to the Alta-controversy, 1983-2023.",
    x = "Year",
    y = "Total Number of Yearly Observations",
    color = "Legend"
  ) +
  # Customize legend
  guides(color = guide_legend(override.aes = list(size = 1))) +
  # Set plot theme
  theme(
    panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
    panel.grid.major = element_line(color = 'bisque3'),
    panel.grid.minor = element_line(color = 'bisque'),
    legend.background = element_rect(fill = 'bisque3'),
    legend.key = element_rect(fill = 'bisque3', colour = 'bisque4'),
    plot.background = element_rect(fill = 'bisque2')
  )

#Samesak eller miljøvernsak?

summary_table <- renset_data_trunkert %>%
  filter(year <= 2022) %>%
  group_by(year) %>%
  summarise(
    average_value_nat = mean(`natur og miljø`, na.rm = TRUE),
    average_value_sam = mean(samesaken, na.rm = TRUE)
  )


ggplot(summary_table, aes(x = year)) +
  # Add smoothed curves first to place them behind
  geom_smooth(aes(y = average_value_sam, color = "Smoothed Average samesaken"), 
              size = 1, method = "loess", se = FALSE) +
   geom_smooth(aes(y = average_value_nat, color = "Smoothed Average natur og miljø"), 
               size = 1, method = "loess", se = FALSE) +
  # Add unsmoothed lines on top
  geom_line(aes(y = average_value_sam, color = "Unsmoothed Average samesaken"), size = 1) +
   geom_line(aes(y = average_value_nat, color = "Unsmoothed Average natur og miljø"), size = 1) +
  # Customize x-axis
  scale_x_continuous(breaks = seq(1978, 2022, by = 2)) +
  # Add labels
  labs(
    title = "Text snippets, average yearly values, \"The sami cause\",  N = 82528",
    x = "Year",
    y = "Average label score, 0 = no relationship 1 = strong relationship",
    color = "Legend"
  ) +
  # Set color scheme with adjusted alpha values
  scale_color_manual(values = c(
    "Unsmoothed Average samesaken" = alpha("lightsalmon4", 0.9),
    "Unsmoothed Average natur og miljø" = alpha("darkseagreen4", 0.9),
     "Smoothed Average samesaken" = alpha("lightsalmon2", 0.4),
     "Smoothed Average natur og miljø" = alpha("darkseagreen2", 0.4)
  )) +
  # Customize legend
  guides(color = guide_legend(override.aes = list(size = 1))) +
  # Set plot theme
  theme(
    panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
    panel.grid.major = element_line(color = 'bisque3'),
    panel.grid.minor = element_line(color = 'bisque3'),
    legend.background = element_rect(fill = 'bisque3'),
    legend.key = element_rect(fill = 'bisque3', colour = 'bisque4'),
    plot.background = element_rect(fill = 'bisque2')
  )



#overdrevet maktbruk av myndigheter og politi
# 
# summary_table <- renset_data %>%
#   group_by(year) %>%
#   summarise(
#     average_value_mak = mean(`overdrevet maktbruk av myndigheter og politi`, na.rm = TRUE),
#   )
# 
# 
# ggplot(summary_table, aes(x = year)) +
#   geom_smooth(aes(y = average_value_mak, color = "Smoothed maktbruk"), method = "loess", se = FALSE) +
#   geom_line(aes(y = average_value_mak, color = "Unsmooth maktbruk"), size = 1, alpha = 0.4) +
#   labs(
#     title = "Tekstutdrag rangert som overdrevet maktbruk av myndigheter og politi, årlig gjennomsnitt, kategorisering. N ≈ 84 000",
#     x = "Årstall",
#     y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
#     color = "Legende"
#   ) +
#   scale_color_manual(values = c("Smoothed maktbruk" = "lightsalmon4",
#                                 "Unsmooth maktbruk" = "lightsalmon2"
#   )) +
#   guides(color = guide_legend(override.aes = list(size = 1))) +
#   theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
#         panel.grid.major = element_line(color = 'bisque3'),
#         panel.grid.minor = element_line(color = 'bisque3'),
#         legend.background = element_rect(fill ='bisque3'),
#         legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
#         plot.background = element_rect(fill ='bisque2'))

#natur og miljø

# summary_table <- renset_data %>%
#   group_by(year) %>%
#   summarise(
#     average_value_mak = mean(`natur og miljø`, na.rm = TRUE),
#   )
# 
# 
# ggplot(summary_table, aes(x = year)) +
#   geom_smooth(aes(y = average_value_mak, color = "Smoothed natur og miljø"), method = "loess", se = FALSE) +
#   geom_line(aes(y = average_value_mak, color = "Unsmooth natur og miljø"), size = 1, alpha = 0.4) +
#   labs(
#     title = "Tekstutdrag rangert som natur og miljø, årlig gjennomsnitt, kategorisering. N ≈ 84 000",
#     x = "Årstall",
#     y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
#     color = "Legende"
#   ) +
#   scale_color_manual(values = c("Smoothed natur og miljø" = "lightsalmon4",
#                                 "Unsmooth natur og miljø" = "lightsalmon2"
#   )) +
#   guides(color = guide_legend(override.aes = list(size = 1))) +
#   theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
#         panel.grid.major = element_line(color = 'bisque3'),
#         panel.grid.minor = element_line(color = 'bisque3'),
#         legend.background = element_rect(fill ='bisque3'),
#         legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
#         plot.background = element_rect(fill ='bisque2'))

#vern av vassdrag
# 
# summary_table <- renset_data %>%
#   group_by(year) %>%
#   summarise(
#     average_value_vav = mean(`vern av vassdrag`, na.rm = TRUE),
#   )
# 
# 
# ggplot(summary_table, aes(x = year)) +
#   geom_smooth(aes(y = average_value_vav, color = "Smoothed vern av vassdrag"), method = "loess", se = FALSE) +
#   geom_line(aes(y = average_value_vav, color = "Unsmooth vern av vassdrag"), size = 1, alpha = 0.4) +
#   labs(
#     title = "Tekstutdrag rangert som vern av vassdrag, årlig gjennomsnitt, kategorisering. N ≈ 84 000",
#     x = "Årstall",
#     y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
#     color = "Legende"
#   ) +
#   scale_color_manual(values = c("Smoothed vern av vassdrag" = "lightsalmon4",
#                                 "Unsmooth vern av vassdrag" = "lightsalmon2"
#   )) +
#   guides(color = guide_legend(override.aes = list(size = 1))) +
#   theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
#         panel.grid.major = element_line(color = 'bisque3'),
#         panel.grid.minor = element_line(color = 'bisque3'),
#         legend.background = element_rect(fill ='bisque3'),
#         legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
#         plot.background = element_rect(fill ='bisque2'))

#Arbeiderpartiet

# summary_table <- renset_data_transformed %>%
#   group_by(year) %>%
#   summarise(
#     average_value_ap = mean(`modified_arbeiderpartiet`, na.rm = TRUE),
#   )
# 
# 
# ggplot(summary_table, aes(x = year)) +
#   geom_smooth(aes(y = average_value_ap, color = "Smoothed arbeiderpartiet"), method = "loess", se = FALSE) +
#   geom_line(aes(y = average_value_ap, color = "Unsmooth arbeiderpartiet"), size = 1, alpha = 0.4) +
#   labs(
#     title = "Tekstutdrag rangert som arbeiderpartiet, årlig gjennomsnitt, kategorisering. N ≈ 84 000",
#     x = "Årstall",
#     y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
#     color = "Legende"
#   ) +
#   scale_color_manual(values = c("Smoothed arbeiderpartiet" = "lightsalmon4",
#                                 "Unsmooth arbeiderpartiet" = "lightsalmon2"
#   )) +
#   guides(color = guide_legend(override.aes = list(size = 1))) +
#   theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
#         panel.grid.major = element_line(color = 'bisque3'),
#         panel.grid.minor = element_line(color = 'bisque3'),
#         legend.background = element_rect(fill ='bisque3'),
#         legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
#         plot.background = element_rect(fill ='bisque2'))

#Historisk betydning
# 
# summary_table <- renset_data %>%
#   group_by(year) %>%
#   summarise(
#     average_value_his = mean(`historisk betydning`, na.rm = TRUE),
#   )
# 
# 
# ggplot(summary_table, aes(x = year)) +
#   geom_smooth(aes(y = average_value_his, color = "Smoothed historisk betydning"), method = "loess", se = FALSE) +
#   geom_line(aes(y = average_value_his, color = "Unsmooth historisk betydning"), size = 1, alpha = 0.4) +
#   labs(
#     title = "Tekstutdrag rangert som historisk betydning, årlig gjennomsnitt, kategorisering. N ≈ 84 000",
#     x = "Årstall",
#     y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
#     color = "Legende"
#   ) +
#   scale_color_manual(values = c("Smoothed historisk betydning" = "lightsalmon4",
#                                 "Unsmooth historisk betydning" = "lightsalmon2"
#   )) +
#   guides(color = guide_legend(override.aes = list(size = 1))) +
#   theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
#         panel.grid.major = element_line(color = 'bisque3'),
#         panel.grid.minor = element_line(color = 'bisque3'),
#         legend.background = element_rect(fill ='bisque3'),
#         legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
#         plot.background = element_rect(fill ='bisque2'))
# 

#Historisk betydning
# 
# summary_table <- renset_data %>%
#   group_by(year) %>%
#   summarise(
#     average_value_MAS = mean(`neddemming av Masi`, na.rm = TRUE),
#   )
# 
# 
# ggplot(summary_table, aes(x = year)) +
#   geom_smooth(aes(y = average_value_MAS, color = "Smoothed neddemming av Masi"), method = "loess", se = FALSE) +
#   geom_line(aes(y = average_value_MAS, color = "Unsmooth neddemming av Masi"), size = 1, alpha = 0.4) +
#   labs(
#     title = "Tekstutdrag rangert som neddemming av Masi, årlig gjennomsnitt, kategorisering. N ≈ 84 000",
#     x = "Årstall",
#     y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
#     color = "Legende"
#   ) +
#   scale_color_manual(values = c("Smoothed neddemming av Masi" = "lightsalmon4",
#                                 "Unsmooth neddemming av Masi" = "lightsalmon2"
#   )) +
#   guides(color = guide_legend(override.aes = list(size = 1))) +
#   theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
#         panel.grid.major = element_line(color = 'bisque3'),
#         panel.grid.minor = element_line(color = 'bisque3'),
#         legend.background = element_rect(fill ='bisque3'),
#         legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
#         plot.background = element_rect(fill ='bisque2'))
# 
# 



### Maktbruk 



summary_table_demo <- renset_data_trunkert %>%
  group_by(year) %>%
  filter(year <=2022) %>% 
  summarise(
    average_value_makt = mean(`overdrevet maktbruk av myndigheter og politi`, na.rm = TRUE),
    average_value_utr = mean(`Manglende utredning om konsekvenser`, na.rm = TRUE),
    average_value_sam = mean(`samesaken`, na.rm = TRUE),
  #  average_value_arb = mean(`modified_arbeiderpartiet`, na.rm = TRUE)
  )

ggplot(summary_table_demo, aes(x = year)) +
  # Add smoothed curves first to place them behind
  geom_smooth(aes(y = average_value_makt, color = "Smoothed Average overdrevet maktbruk av myndigheter og politi"), 
              method = "loess", se = FALSE, size = 1) +
  geom_smooth(aes(y = average_value_utr, color = "Smoothed Average manglende utredning om konsekvenser"), 
              method = "loess", se = FALSE, size = 1) +
  geom_smooth(aes(y = average_value_sam, color = "Smoothed Average samesaken"), 
              method = "loess", se = FALSE, size = 1) +
  # geom_smooth(aes(y = average_value_arb, color = "Smoothed Average arbeiderpartiet"), method = "loess", se = FALSE) +
  # Add unsmoothed lines on top
  geom_line(aes(y = average_value_makt, color = "Unsmoothed Average overdrevet maktbruk av myndigheter og politi")) +
  geom_line(aes(y = average_value_utr, color = "Unsmoothed Average manglende utredning om konsekvenser")) +
  geom_line(aes(y = average_value_sam, color = "Unsmoothed Average samesaken")) +
  # geom_line(aes(y = average_value_arb, color = "Unsmoothed Average arbeiderpartiet"), size = 1) +
  # Customize x-axis
  scale_x_continuous(breaks = seq(1978, 2022, by = 2)) +  # Adjust breaks to show every 2nd year
  # Add labels
  labs(
    title = "Text snippets, scored by view on government handling of the Alta controversy juxtapositioned with \"The sami cause\". N = 82528",
    x = "Year",
    y = "Average score, 0 = no relationship 1 = strong relationship",
    color = "Legend"
  ) +
  # Set color scheme with adjusted alpha values
  scale_color_manual(values = c(
    "Smoothed Average overdrevet maktbruk av myndigheter og politi" = alpha("slategray2", 0.4),
    "Smoothed Average manglende utredning om konsekvenser" = alpha("darkseagreen3", 0.4),
    "Smoothed Average samesaken" = alpha("lightsalmon2", 0.4),
    "Unsmoothed Average overdrevet maktbruk av myndigheter og politi" = alpha("slategray4", 0.9),
    "Unsmoothed Average manglende utredning om konsekvenser" = alpha("darkseagreen4", 0.9),
    "Unsmoothed Average samesaken" = alpha("lightsalmon4", 0.9)
  )) +
  # Customize legend
  guides(color = guide_legend(override.aes = list(size = 1))) +
  # Set plot theme
  theme(
    panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
    panel.grid.major = element_line(color = 'bisque2'),
    panel.grid.minor = element_line(color = 'bisque2'),
    legend.background = element_rect(fill = 'bisque2'),
    legend.key = element_rect(fill = 'bisque2', colour = 'bisque4'),
    plot.background = element_rect(fill = 'bisque1')
  )


###Syn på samesaken

summary_table_syn_sam <- renset_data %>%
  group_by(year) %>%
  summarise(
    average_value_sær = mean(`samiske særrettigheter`, na.rm = TRUE),
    average_value_trus = mean(`trussel mot samisk kultur og næring`, na.rm = TRUE),
    average_value_sam = mean(samesaken, na.rm = TRUE),
    
      )

ggplot(summary_table_syn_sam, aes(x = year)) +
  geom_vline(aes(xintercept = year), linetype="solid", alpha=0.1) +  # Draw vertical lines for each year
#  geom_smooth(aes(y = average_value_sam, color = "Smoothed Average samesaken"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_sær, color = "Smoothed Average special privelegies"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_trus, color = "Smoothed Average Threath to sami rights"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_value_sær, color = "Unsmoothed Average special privelegies"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_trus, color = "Unsmoothed Average Threath to sami rights"), size = 1, alpha = 0.4) +
  labs(
    title = "Text snippets scored by perspective on the issue of sami rights N=87854, 1978-2023",
    x = "Year",
    y = "Average, 0 = No connection 1 = Strong connection",
    color = "Legend, sorted by value in 2004:"
  ) +
  scale_color_manual(values = c("Smoothed Average special privelegies" = "lightsalmon4",
                                "Smoothed Average Threath to sami rights" = "deepskyblue4",
                                "Unsmoothed Average special privelegies" = "lightsalmon2",
                                "Unsmoothed Average Threath to sami rights" = "deepskyblue3"),
                      limits = c(
                                 "Smoothed Average Threath to sami rights",
                                 "Unsmoothed Average Threath to sami rights",
                                 "Smoothed Average special privelegies",
                                 "Unsmoothed Average special privelegies")) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))


###Traumatisk? 

summary_table_traume <- renset_data %>%
  group_by(year) %>%
#  filter(city %in% c("Alta")) %>%
  summarise(
    average_value_uts = mean(`utsatt og usikkerhet`, na.rm = TRUE),
    average_value_unø = mean(`unødvendig`, na.rm = TRUE),
    average_value_spl = mean(`splittelse og uenighet`, na.rm = TRUE),
    average_value_van = mean(`vanskelig å snakke om`, na.rm = TRUE),
    average_value_ure = mean(`urettferdig`, na.rm = TRUE),
    average_value_opp = mean(`opprivende`, na.rm = TRUE),
    average_value_fors = mean(`forsoning`, na.rm = TRUE),
    average_value_samh = mean(`samhold og enighet`, na.rm = TRUE),
    average_value_hand = mean(`handlekraft og beslutninger`, na.rm = TRUE),
    average_value_økon = mean(`økonomisk og næringsmessig nytte`, na.rm = TRUE)
    
  ) %>%
  rowwise() %>%
  mutate(
    average_traumer = mean(c_across(c(average_value_uts, average_value_unø, average_value_opp, average_value_spl, average_value_van, average_value_ure)), na.rm = TRUE),
    average_ikke_traumer = mean(c_across(c(average_value_fors, average_value_samh, average_value_hand, average_value_økon)), na.rm = TRUE)
  )

ggplot(summary_table_traume, aes(x = year)) +
  geom_smooth(aes(y = average_traumer, color = "Smoothed Average trauma"), method = "loess", se = FALSE) +
#  geom_smooth(aes(y = average_ikke_traumer, color = "Smoothed Average ikke traume"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_traumer, color = "Unsmoothed Average trauma"), size = 1, alpha = 0.4) +
 # geom_line(aes(y = average_ikke_traumer, color = "Unsmoothed Average ikke traume"), size = 1, alpha = 0.4) +
  scale_x_continuous(breaks=seq(1983, 2023, by = 2)) +  # Adjust breaks to show every 5th year
   labs(
    title = "Text snippets rated by trauma, supercategory. N = 87 854",
    x = "Year",
    y = "Average, 0 = No connection 1 = Strong connection",
    color = "Legend:"
  ) +
  scale_color_manual(values = c("Smoothed Average trauma" = "tomato4",
                              #  "Smoothed Average ikke traume" = "deepskyblue4",
                                "Unsmoothed Average trauma" = "tomato2"
                              #  "Unsmoothed Average ikke traume" = "deepskyblue3"
                              )) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))

###Test
summary_table_aktør <- renset_data %>%
  group_by(year) %>%
  summarise(
    average_value_kom = mean(`kommunestyret`, na.rm = TRUE),
    average_value_dom = mean(`domstoler`, na.rm = TRUE),
    average_value_reg = mean(`regjeringen`, na.rm = TRUE),
    average_value_myn = mean(`myndigheter`, na.rm = TRUE),
    average_value_dem = mean(`demonstranter`, na.rm = TRUE),
    average_value_pol = mean(`politiet`, na.rm = TRUE),
    average_value_sam = mean(`samesaken`, na.rm = TRUE)
    
  )

model <- lm(average_value_dem ~ average_value_pol, data = summary_table_aktør)
summary(model)

model <- lm(average_value_dem ~ average_value_sam, data = summary_table_aktør)
summary(model)


ggplot(summary_table_aktør, aes(x = year)) +
  geom_smooth(aes(y = average_value_kom, color = "Smoothed Average kommunestyret"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_dom, color = "Smoothed Average domstoler"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_reg, color = "Smoothed Average regjeringen"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_myn, color = "Smoothed Average myndigheter"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_dem, color = "Smoothed Average demonstranter"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_pol, color = "Smoothed Average politiet"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_sam, color = "Smoothed Average samesaken"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_value_kom, color = "Unsmoothed Average kommunestyret"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_dom, color = "Unsmoothed Average domstoler"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_reg, color = "Unsmoothed Average regjeringen"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_myn, color = "Unsmoothed Average myndigheter"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_dem, color = "Unsmoothed Average demonstranter"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_pol, color = "Unsmoothed Average politiet"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_sam, color = "Unsmoothed Average samesaken"), size = 1, alpha = 0.4) +
  
  labs(
    title = "Tekstutdrag rangert etter aktør, årlig gjennomsnitt, kategorisering. N ≈ 84 000",
    x = "Årstall",
    y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
    color = "Legende"
  ) +
  scale_color_manual(values = c("Smoothed Average kommunestyret" = "lightsalmon4",
                                "Smoothed Average domstoler" = "darkseagreen4",
                                "Smoothed Average regjeringen" = "slategray4",
                                "Smoothed Average myndigheter" = "gold4",
                                "Smoothed Average demonstranter" = "purple3",
                                "Smoothed Average politiet" = "steelblue4",
                                "Smoothed Average samesaken" = "cyan4",
                                "Unsmoothed Average kommunestyret" = "lightsalmon2",
                                "Unsmoothed Average domstoler" = "darkseagreen3",
                                "Unsmoothed Average regjeringen" = "slategray3",
                                "Unsmoothed Average myndigheter" = "gold3",
                                "Unsmoothed Average demonstranter" = "purple2",
                                "Unsmoothed Average politiet" = "steelblue3",
                                "Unsmoothed Average samesaken" = "cyan3"
  )) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))

#Ting statengjør

summary_table_stat <- renset_data %>%
  group_by(year) %>%
  summarise(
    average_value_sak = mean(`saksbehandling og juridiske prosesser`, na.rm = TRUE),
    average_value_byg = mean(`bygging av vei og anleggsvirksomhet`, na.rm = TRUE),
    average_value_und = mean(`undersøkelser og utredning`, na.rm = TRUE),
    average_value_arr = mean(`arrestasjoner og bruk av tvangsmidler`, na.rm = TRUE)
  )

model <- lm(average_value_und ~ average_value_sak, data = summary_table_stat)

summary(model)


ggplot(summary_table_stat, aes(x = year)) +
  geom_smooth(aes(y = average_value_sak, color = "Smoothed Average saksbehandling og juridiske prosesser"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_byg, color = "Smoothed Average bygging av vei og anleggsvirksomhet"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_und, color = "Smoothed Average undersøkelser og utredning"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_arr, color = "Smoothed Average arrestasjoner og bruk av tvangsmidler"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_value_sak, color = "Unsmoothed Average saksbehandling og juridiske prosesser"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_byg, color = "Unsmoothed Average bygging av vei og anleggsvirksomhet"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_und, color = "Unsmoothed Average undersøkelser og utredning"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_arr, color = "Unsmoothed Average arrestasjoner og bruk av tvangsmidler"), size = 1, alpha = 0.4) +
  
  labs(
    title = "Tekstutdrag rangert etter hva staten har gjort, årlig gjennomsnitt, kategorisering. N ≈ 84 000",
    x = "Årstall",
    y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
    color = "Legende"
  ) +
  scale_color_manual(values = c("Smoothed Average saksbehandling og juridiske prosesser" = "lightsalmon4",
                                "Smoothed Average bygging av vei og anleggsvirksomhet" = "darkseagreen4",
                                "Smoothed Average undersøkelser og utredning" = "slategray4",
                                "Smoothed Average arrestasjoner og bruk av tvangsmidler" = "gold4",
                                "Unsmoothed Average saksbehandling og juridiske prosesser" = "lightsalmon2",
                                "Unsmoothed Average bygging av vei og anleggsvirksomhet" = "darkseagreen3",
                                "Unsmoothed Average undersøkelser og utredning" = "slategray3",
                                "Unsmoothed Average arrestasjoner og bruk av tvangsmidler" = "gold3"
  )) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))
#Syn på demonstranter

summary_table_demo <- renset_data %>%
  group_by(year) %>%
  summarise(
    average_value_ude = mean(`udemokratiske protester`, na.rm = TRUE),
    average_value_siv = mean(`sivil ulydighet`, na.rm = TRUE),
    average_value_dem = mean(`demonstrantene opptrer dårlig`, na.rm = TRUE),
    average_value_ikk = mean(`ikke voldelige`, na.rm = TRUE)
  )

model <- lm(average_value_ude ~ average_value_ikk, data = summary_table_demo)

summary(model)


ggplot(summary_table_demo, aes(x = year)) +
  geom_smooth(aes(y = average_value_ude, color = "Smoothed Average udemokratiske protester"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_siv, color = "Smoothed Average sivil ulydighet"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_dem, color = "Smoothed Average demonstrantene opptrer dårlig"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_ikk, color = "Smoothed Average ikke voldelige"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_value_ude, color = "Unsmoothed Average udemokratiske protester"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_siv, color = "Unsmoothed Average sivil ulydighet"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_dem, color = "Unsmoothed Average demonstrantene opptrer dårlig"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_ikk, color = "Unsmoothed Average ikke voldelige"), size = 1, alpha = 0.4) +
  
  labs(
    title = "Tekstutdrag rangert etter syn på demonstranter, årlig gjennomsnitt, kategorisering. N ≈ 84 000",
    x = "Årstall",
    y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
    color = "Legende"
  ) +
  scale_color_manual(values = c("Smoothed Average udemokratiske protester" = "lightsalmon4",
                                "Smoothed Average sivil ulydighet" = "darkseagreen4",
                                "Smoothed Average udemonstrantene opptrer dårlig" = "slategray4",
                                "Smoothed Average ikke-voldelige" = "gold4",
                                "Unsmoothed Average udemokratiske protester" = "lightsalmon2",
                                "Unsmoothed Average sivil ulydighet" = "darkseagreen3",
                                "Unsmoothed Average demonstrantene opptrer dårlig" = "slategray3",
                                "Unsmoothed Average ikke-voldelige" = "gold3"
  )) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))

#### Juridisk
summary_table_rettsaker <- renset_data %>%
  group_by(year) %>%
  summarise(
    average_value_dom = mean(`domstoler`, na.rm = TRUE),
    average_value_sak = mean(`saksbehandling og juridiske prosesser`, na.rm = TRUE),
  )

ggplot(summary_table_rettsaker, aes(x = year)) +
  geom_smooth(aes(y = average_value_dom, color = "Smoothed Average domstoler"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_sak, color = "Smoothed Average saksbehandling"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_value_dom, color = "Unsmoothed Average domstoler"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_sak, color = "Unsmoothed Average saksbehandling"), size = 1, alpha = 0.4) +
  labs(
    title = "Juridisk",
    x = "Årstall",
    y = "Gjennomsnitt, 0 = ingen sammenheng 1 = sterk",
    color = "Legende"
  ) +
  scale_color_manual(values = c("Smoothed Average domstoler" = "lightsalmon4",
                                "Smoothed Average saksbehandling" = "darkseagreen4",
                                "Unsmoothed Average domstoler" = "lightsalmon2",
                                "Unsmoothed Average saksbehandling" = "darkseagreen3")) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))

#####
# to do

#average_value_leg1 = mean(`fredelige demonstranter`, na.rm = TRUE),
#average_value_leg1,
#  filter(year %in% 1978:1986) %>%

print(names(renset_data))

### Syn på konflikten. Super-kategorier ####

#Periode 1

summary_table_syn_per1 <- renset_data %>%
  group_by(year) %>%
  summarise(
    average_value_sam1 = mean(`samesaken`, na.rm = TRUE),
    average_value_sam2 = mean(`trussel mot samisk kultur og næring`, na.rm = TRUE),
    average_value_nat1 = mean(`natur og miljø`, na.rm = TRUE),
    average_value_nat2 = mean(`vern av vassdrag`, na.rm = TRUE),
    average_value_utb1 = mean(`økonomisk og næringsmessig nytte`, na.rm = TRUE),
    average_value_utb2 = mean(`må ha kraft og strøm`, na.rm = TRUE),
    average_value_leg2 = mean(`Konstruktiv sivil ulydighet`, na.rm = TRUE),
    average_value_leg3 = mean(`overdrevet maktbruk av myndigheter og politi`, na.rm = TRUE),
    average_value_nat3 = mean(`Manglende utredning om konsekvenser`, na.rm = TRUE),
    average_value_leg5 = mean(`skeptisk kraftprognose-motstand`, na.rm = TRUE),
    average_value_leg6 = mean(`meningsløs kraftutbygging`, na.rm = TRUE),
    average_value_mak1 = mean(`lov og orden`, na.rm = TRUE),
    average_value_mak2 = mean(`udemokratiske protester`, na.rm = TRUE),
    average_value_mak3 = mean(`demonstrantene opptrer dårlig`, na.rm = TRUE),
    average_value_mak4 = mean(`stortingets vedtak`, na.rm = TRUE),
    
  ) %>%
  rowwise() %>%
  mutate(
    average_sam = mean(c_across(c(average_value_sam1, average_value_sam2)), na.rm = TRUE),
    average_nat = mean(c_across(c(average_value_nat1, average_value_nat2, average_value_nat3)), na.rm = TRUE),
    average_utb = mean(c_across(c(average_value_utb1, average_value_utb2)), na.rm = TRUE),
    average_leg = mean(c_across(c(average_value_leg2, average_value_leg3, average_value_leg5, average_value_leg6)), na.rm = TRUE),
    average_mak = mean(c_across(c(average_value_mak1, average_value_mak2, average_value_mak3, average_value_mak4)), na.rm = TRUE)
    
  )



ggplot(summary_table_syn_per1, aes(x = year)) +
  geom_smooth(aes(y = average_sam, color = "Saami aspect"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_nat, color = "Enviromentalist aspect"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_utb, color = "Pro building of the dam"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_leg, color = "Pro protestor"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_mak, color = "Pro government"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_sam, color = "Actual saami aspect"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_nat, color = "Actual enviromentalist aspect"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_utb, color = "Actual pro building of the dam"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_leg, color = "Actual pro protestor"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_mak, color = "Actual pro government"), size = 1, alpha = 0.4) +
  labs(
    title = "Smoothed and unsmoothed average rating of text snippets by year, super-categories 1978-1987",
    x = "Year",
    y = "Yearly Average, 0 = No connection 1 = Strong connection",
    color = "Legend, sorted by value in 1987:"
  ) +
  scale_color_manual(values = c("Saami aspect" = "tomato4",
                                "Enviromentalist aspect" = "deepskyblue4",
                                "Pro building of the dam" = "palegreen4",
                                "Pro protestor" = "hotpink4",
                                "Pro government" = "gold4",
                                "Actual saami aspect" = "tomato2",
                                "Actual enviromentalist aspect" = "deepskyblue3",
                                "Actual pro building of the dam" = "palegreen2",
                                "Actual pro protestor" = "hotpink2",
                                "Actual pro government" = "gold2"),
  limits = c("Pro protestor",
             "Actual pro protestor",
             "Enviromentalist aspect",
             "Actual enviromentalist aspect",
             "Saami aspect",
             "Actual saami aspect",
             "Pro building of the dam",
             "Actual pro building of the dam",
             "Pro government",
             "Actual pro government")) +
  
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))


ggplot(summary_table_syn_per1, aes(x = year)) +
  geom_smooth(aes(y = average_sam, color = "Saami aspect"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_leg, color = "Pro protestor"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_leg, color = "Actual pro protestor"), size = 1, alpha = 0.4) +
  labs(
    title = "Smoothed and unsmoothed average rating of text snippets by year, super-categories 1978-1987",
    x = "Year",
    y = "Yearly Average, 0 = No connection 1 = Strong connection",
    color = "Legend, sorted by value in 1987:"
  ) +
  scale_color_manual(values = c("Saami aspect" = "lightsalmon4",
                                "Pro protestor" = "gold4",
                                "Actual pro protestor" = "gold2")) + 

  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))

####
#Periode 2

summary_table_syn_per2 <- renset_data %>%
  group_by(year) %>%
  filter(year %in% 1987:2004) %>%
  summarise(
    average_value_sam1 = mean(`samesaken`, na.rm = TRUE),
    average_value_sam2 = mean(`trussel mot samisk kultur og næring`, na.rm = TRUE),
    average_value_nat1 = mean(`natur og miljø`, na.rm = TRUE),
    average_value_nat2 = mean(`vern av vassdrag`, na.rm = TRUE),
    average_value_utb1 = mean(`økonomisk og næringsmessig nytte`, na.rm = TRUE),
    average_value_utb2 = mean(`må ha kraft og strøm`, na.rm = TRUE),
    average_value_leg2 = mean(`Konstruktiv sivil ulydighet`, na.rm = TRUE),
    average_value_leg3 = mean(`overdrevet maktbruk av myndigheter og politi`, na.rm = TRUE),
    average_value_nat3 = mean(`Manglende utredning om konsekvenser`, na.rm = TRUE),
    average_value_leg5 = mean(`skeptisk kraftprognose-motstand`, na.rm = TRUE),
    average_value_leg6 = mean(`meningsløs kraftutbygging`, na.rm = TRUE),
    average_value_mak1 = mean(`lov og orden`, na.rm = TRUE),
    average_value_mak2 = mean(`udemokratiske protester`, na.rm = TRUE),
    average_value_mak3 = mean(`demonstrantene opptrer dårlig`, na.rm = TRUE),
    average_value_mak4 = mean(`stortingets vedtak`, na.rm = TRUE),
    
  ) %>%
  rowwise() %>%
  mutate(
    average_sam = mean(c_across(c(average_value_sam1, average_value_sam2)), na.rm = TRUE),
    average_nat = mean(c_across(c(average_value_nat1, average_value_nat2, average_value_nat3)), na.rm = TRUE),
    average_utb = mean(c_across(c(average_value_utb1, average_value_utb2)), na.rm = TRUE),
    average_leg = mean(c_across(c(average_value_leg2, average_value_leg3, average_value_leg5, average_value_leg6)), na.rm = TRUE),
    average_mak = mean(c_across(c(average_value_mak1, average_value_mak2, average_value_mak3, average_value_mak4)), na.rm = TRUE)
    
  )



ggplot(summary_table_syn_per2, aes(x = year)) +
  geom_smooth(aes(y = average_sam, color = "Saami aspect"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_nat, color = "Enviromentalist aspect"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_utb, color = "Pro building of the dam"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_leg, color = "Pro protestor"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_mak, color = "Pro government"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_sam, color = "Actual saami aspect"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_nat, color = "Actual enviromentalist aspect"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_utb, color = "Actual pro building of the dam"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_leg, color = "Actual pro protestor"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_mak, color = "Actual pro government"), size = 1, alpha = 0.4) +
  labs(
    title = "Smoothed and unsmoothed average rating of text snippets by year, super-categories 1987-2004",
    x = "Year",
    y = "Average, 0 = No connection 1 = Strong connection",
    color = "Legend, sorted by value in 2004:"
  ) +
  scale_x_continuous(breaks=seq(1987,2004)) +
  scale_color_manual(values = c("Saami aspect" = "tomato4",
                                "Enviromentalist aspect" = "deepskyblue4",
                                "Pro building of the dam" = "palegreen4",
                                "Pro protestor" = "hotpink4",
                                "Pro government" = "gold4",
                                "Actual saami aspect" = "tomato2",
                                "Actual enviromentalist aspect" = "deepskyblue3",
                                "Actual pro building of the dam" = "palegreen2",
                                "Actual pro protestor" = "hotpink2",
                                "Actual pro government" = "gold2"),
                     limits = c("Pro protestor",
                                "Actual pro protestor",
                                "Enviromentalist aspect",
                                "Actual enviromentalist aspect",
                                "Saami aspect",
                                "Actual saami aspect",
                                "Pro building of the dam",
                                "Actual pro building of the dam",
                                "Pro government",
                                "Actual pro government")) +
  
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))

