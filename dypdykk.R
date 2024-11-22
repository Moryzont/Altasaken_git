rm(list=ls()) # Clear de "Global Environment"
setwd("/Users/vetlewisloffsandring/Documents/Altasaken") # Den ser ikke slik ut på windows
options(scipen = 999)
options(encoding = "UTF-8")
library(tidyverse)
library(lubridate)
library(ggplot2)



renset_data_pre2023 <- read_csv("renset_data.csv")
renset_data_2023 <- read_csv("2023_conc_renset.csv")
renset_data_pre2023 <- subset(renset_data_pre2023, select = -c(...1, extracted_pattern))


colnames(renset_data_2023) <- gsub("\\.", " ", colnames(renset_data_2023))

# Replace periods and hyphens with spaces in renset_data_pre2023
colnames(renset_data_pre2023) <- gsub("[.-]", " ", colnames(renset_data_pre2023))

# Replace periods and hyphens with spaces in renset_data_2023
colnames(renset_data_2023) <- gsub("[.-]", " ", colnames(renset_data_2023))


renset_data <- rbind(renset_data_pre2023, renset_data_2023)





sample_data <- sample(renset_data$conc, 50)

# Convert each item into a properly escaped string literal and wrap it in quotes
formatted_strings <- sapply(sample_data, function(x) {
  # Escape single quotes by replacing them with '\''
  escaped_string <- gsub("'", "\\'", x)
  # Return the string with single quotes added around it
  sprintf("'%s'", escaped_string)
})

# Concatenate all formatted strings into one, separated by commas and newline characters
concatenated_strings <- paste(formatted_strings, collapse = ",\n    ")

# Format the final output as a Python list of strings
final_output <- sprintf("sequences = [\n    %s\n]", concatenated_strings)

# Print the final output
cat(final_output)

#Dette må du definere for å se nærmere på en år og en kolonne. 
#bare skriv inn året ditt her
#Sjekk snittscore for alle labelene 
ditt_år = 2023
#Her skriver du inn kolonnen/ labelen du vil se nærmere på. Husk parantes
din_kolonne = "trussel mot samisk kultur og næring"

# Lager en funksjon for å sjekke en topplabel
is_top_10_perc <- function(value, values) {
  threshold <- quantile(values, 0.9, na.rm = TRUE)
  return(value > threshold)
}

# Arrange, filter, and apply the function row-wise
Hva_snakker_de_om <- renset_data %>%
  arrange(desc(year), desc(!!sym(din_kolonne))) %>%
  filter(year == ditt_år) %>%
  rowwise() %>%
  mutate(top_label = is_top_10_perc(!!sym(din_kolonne), c_across(9:74))) %>%
  select(conc, !!sym(din_kolonne), top_label) %>%
  ungroup()

# Her kan du se på toppsetningene rundt om kring 
head(Hva_snakker_de_om$conc, 20)

# Her kalkulerer vi bare snitt og standardavvik
mean_value <- mean(Hva_snakker_de_om[[din_kolonne]])
sd_value <- sd(Hva_snakker_de_om[[din_kolonne]])

# Denne har mye å si for hvordan det ser ut. 
#Den bestemmer hvor store hopp den skal regne i. ta 0.01 / 0.02 om du vil finne dupliktar
bin_width = 0.10 # Her kan du justere for å se mer eller mindre detaljer. 

# Her lager vi et histogram for å se hvilke verdier som har mye å si.

ggplot(Hva_snakker_de_om, aes(x = !!sym(din_kolonne))) +
  geom_histogram(aes(y = ..density..), binwidth = bin_width, fill = "skyblue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean_value, sd = sd_value), color = "red", size = 1.5, linetype = "dashed") +
  labs(title = "Distribution with Normal Curve", x = din_kolonne, y = "Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#### Årlig oppsummering
yearly_average <- renset_data %>%
  group_by(year) %>%
  select(!1:9) %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE)))

yearly_high <- renset_data %>%
  group_by(year) %>%
  select(!1:9) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(-year, names_to = "column", values_to = "average") %>%
  group_by(year) %>%
  arrange(year, desc(average)) %>%
  mutate(rank = paste("rank", row_number(), sep = "_")) %>%
  filter(row_number() <= 3) %>%
  pivot_wider(names_from = rank, values_from = column, values_fill = list(column = NA)) %>%
  select(year, rank_1, rank_2, rank_3) %>% 
  fill(rank_1, rank_2, rank_3, .direction = "updown") %>% 
  distinct()

write.csv(yearly_high, "årlig_topp3.csv", row.names = FALSE)

# Generelle opppsummeringer:
variables <- c(
  # Når snakkes det om samiske ting
  "samesaken", 
  # Disse to forsøker beskrive forskjellen i perspektivet på samiske rettigheter
  "samiske særrettigheter", 
  "trussel mot samisk kultur og næring",
  # Disse to prøver å ta forskjellene mellom nasjonale og regionale protester
  "Alta samfunnet er i mot utbygging", "protestbevegelse", 
  #Dette prøver å fange ulike aktører
  "partipolitikk", "kommunestyret", "domstoler", "regjeringen", "myndigheter", "demonstranter", "politiet", 
  #Ting som staten gjør
  "saksbehandling og juridiske prosesser", "bygging av vei og anleggsvirksomhet", "undersøkelser og utredning", "arrestasjoner og bruk av tvangsmidler", 
  #Pro alta-utbyggingen
  "økonomisk og næringsmessig nytte", "må ha kraft og strøm", "Alta kautokeinovassdraget bør bygges ut", 
  #Etnisk dimensjon
  "konflikt mellom samer og nordmenn", 
  #Landrettigheter
  "rettigheter til land og ressurser", 
  #Miljøvern
  "natur og miljø", "vern av vassdrag", 
  #Laksesaken
  "trussel mot laksefiske", 
  #Traumer
  "utsatt og usikkerhet", "unødvendig", "splittelse og uenighet", "vanskelig å snakke om", "urettferdig", "opprivende", 
  #Anti-traumer
  "forsoning", "samhold og enighet", "handlekraft og beslutninger","tydelig og bestemt",
  #Hendelser
  "leiren i Stilla", "sultestreik", "politiaksjonen", "stortingets vedtak", "opprettelsen av Sametinget", "Finnmarksloven", "rettsak",
  #Negativt syn på demonstranter
  "udemokratiske protester", "sivil ulydighet", "demonstrantene opptrer dårlig", "Skeptisk til ulydighet", "lov og orden", 
  #Positivt syn på demonstranter
  "Konstruktiv sivil ulydighet", "overdrevet maktbruk av myndigheter og politi", "fredelige demonstranter", "ikke voldelige","Manglende utredning om konsekvenser", "skeptisk kraftprognose motstand", "meningsløs kraftutbygging",
  #Meta
  "mediedekning", "historisk betydning", "kunst og kultur", "husker eller minnes", "feilinformasjon",
  #Hva er det som skjer? 
  "at et møte blir arrangert", "tv program", "produksjon av film" 
)
#Sjekk snittscore for alle labelene 
ditt_år = 2023
#Her skriver du inn kolonnen/ labelen du vil se nærmere på. Husk parantes
din_kolonne = "samiske særrettigheter"

daily_averages <- renset_data %>%
  arrange(desc(year), desc(!!sym(din_kolonne))) %>%
  filter(year == ditt_år) %>%
  group_by(date) %>%
  summarise(
    average_score = mean(!!sym(din_kolonne), na.rm = TRUE),
    total_observations = n()
  ) %>%
  mutate(andel = total_observations * average_score)

plot_title <- paste("Average Daily Score for:", din_kolonne,"As Well as Total Observations. In the year", ditt_år)

top_days <- daily_averages %>%
  top_n(5, total_observations) 

ggplot(daily_averages, aes(x = date)) +
  geom_line(aes(y = andel), color = "blue") +
  geom_line(aes(y = total_observations), color = "red") +
  geom_label(data = top_days, aes(y = total_observations, label = as.character(date)), 
             vjust = 1, color = "black", fill = "white") +  # Label with the date for top days with white background
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Show month abbreviations
  labs(title = plot_title, x = "Dates", y = "Number of Observations & Average Label Score") +
  theme_minimal()
######Konkordanser og topp labels######

ditt_år = 2023
#Her skriver du inn kolonnen/ labelen du vil se nærmere på. 

#Her skal du skrive hvor mange av toppdagene du vil ha med
top_days <- renset_data %>%
  filter(year == ditt_år) %>%
  group_by(date) %>%
  summarise(
    total_observations = n()
  ) %>%
  top_n(5, total_observations)
        
# Step 1: Filter `renset_data` for the top 5 days
filtered_data <- renset_data %>%
  semi_join(top_days, by = "date")

# Step 2: Reshape the data to long format and rank labels
results <- filtered_data %>%
  gather(key = "label", value = "score", one_of(variables)) %>%
  group_by(date, conc) %>%
  arrange(desc(score), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3)  # Keep only top 3 labels

results <- filtered_data %>%
  gather(key = "label", value = "score", one_of(variables)) %>%
  distinct(date, conc, label, .keep_all = TRUE) %>%  # Remove duplicates
  group_by(date, conc) %>%
  mutate(total_labels = n()) %>%
  arrange(desc(score), .by_group = TRUE) %>%
  mutate(
    rank = row_number(),
    top_10_percent = n() * 0.1
  ) %>%
  filter(rank <= ceiling(top_10_percent))  # Keep only top 10% labels


# Step 3: Transform back to wide format for readability
results_wide <- results %>%
  pivot_wider(
    id_cols = c(date, conc),
    names_from = rank,
    values_from = c(label, score),
    names_prefix = "top",
    names_sep = "_"
  )

print(results_wide$conc)
