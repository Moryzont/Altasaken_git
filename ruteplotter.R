rm(list=ls()) # Clear de "Global Environment"
setwd("/Users/vetlewisloffsandring/Documents/Altasaken") # Den ser ikke slik ut på windows
options(scipen = 999)
options(encoding = "UTF-8")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra) 


### VISUALISERING ####

renset_data <- read_csv("renset_data.csv")

renset_data_agg <- renset_data


# Define the summarization function
summarize_data <- function(city_group, variable_name) {
  if (is.null(city_group)) {
    data <- renset_data
  } else {
    data <- renset_data %>%
      filter(city %in% city_group)
  }
  
  data %>%
    group_by(year) %>%
    summarise(average_value = mean(!!as.name(variable_name), na.rm = TRUE))
}

# Define city groups and variables
city_groups <- list(
  lokal = c("Alta", "Lakselv", "Kautokeino", "Karasjok"),
  storby = c("Oslo", "Bergen", "Trondheim", "Stavanger"),
  regional = c("Tromsø", "Hammerfest", "Finnsnes", "Honningsvåg", "Vadsø", "Vardø", "Honningsvåg", "Harstad"),
  total = NULL
)

variables <- c(
               # Når snakkes det om samiske ting
               "samesaken", 
               # Disse to forsøker beskrive forskjellen i perspektivet på samiske rettigheter
               "samiske særrettigheter", 
               "trussel mot samisk kultur og næring",
               # Disse to prøver å ta forskjellene mellom nasjonale og regionale protester
               "Alta-samfunnet er i mot utbygging", "protestbevegelse", 
               #Dette prøver å fange ulike aktører
               "partipolitikk", "kommunestyret", "domstoler", "regjeringen", "myndigheter", "demonstranter", "politiet", 
               #Ting som staten gjør
               "saksbehandling og juridiske prosesser", "bygging av vei og anleggsvirksomhet", "undersøkelser og utredning", "arrestasjoner og bruk av tvangsmidler", 
               #Pro alta-utbyggingen
               "økonomisk og næringsmessig nytte", "må ha kraft og strøm", "Alta-kautokeinovassdraget bør bygges ut", 
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
               "Konstruktiv sivil ulydighet", "overdrevet maktbruk av myndigheter og politi", "fredelige demonstranter", "ikke-voldelige","Manglende utredning om konsekvenser", "skeptisk kraftprognose-motstand", "meningsløs kraftutbygging",
               #Meta
               "mediedekning", "historisk betydning", "kunst og kultur", "husker eller minnes", "feilinformasjon",
               #Hva er det som skjer? 
               "at et møte blir arrangert", "tv-program", "produksjon av film" 
                )

# Iterate and store results
results <- list()
for (group_name in names(city_groups)) {
  for (var in variables) {
    result_name <- paste("summary_table", group_name, gsub("`| ", "", var), sep = "_")
    results[[result_name]] <- summarize_data(city_groups[[group_name]], var)
  }
}

# Accessing results
results$summary_table_lokal_mediedekning
results$summary_table_storby_mediedekning
results$summary_table_storby_historiskbetydning


#Visualisering: 

variables_viz <- c(
  
  "opprivende", "historisk betydning", "samesaken", "samiske særrettigheter"
)

results_viz <- list()
for (group_name in names(city_groups)) {
  for (var in variables_viz) {
    result_name <- paste("summary_table", group_name, gsub("`| ", "", var), sep = "_")
    results_viz[[result_name]] <- summarize_data(city_groups[[group_name]], var)
  }
}

plot_list <- list()
index <- 1


for (group_name in names(city_groups)) {
  for (var in variables) {
    result_name <- paste("summary_table", group_name, gsub("`| ", "", var), sep = "_")
    data_frame <- results_viz[[result_name]]
    
    if (!is.null(data_frame)) {
      plot_list[[index]] <- ggplot(data_frame, aes(x = year, y = average_value)) +
        geom_line() + 
        geom_smooth(method = "loess", se = FALSE) +
        scale_y_continuous(limits = c(0.1, 0.6)) + 
        ggtitle(paste(group_name, var))
      index <- index + 1
    }
  }
}

# Assuming you have 16 plots now
grid.arrange(grobs = plot_list, ncol = 4, nrow = 4)



# Aviser



# Define city groups and variables
newspaper_groups <- list(
  lokal = c("altaposten", "sagat", "ruijankaiku", "kronstadposten", "avvir"),
  venstreaviser = c("klassekampen"),
  høyreaviser = c("aftenposten", "aftenpostenukensnytt", "dagensnaeringsliv"),
  total = NULL
)


variables <- c(
  # Når snakkes det om samiske ting
  "samesaken", 
  # Disse to forsøker beskrive forskjellen i perspektivet på samiske rettigheter
  "samiske særrettigheter", 
  "trussel mot samisk kultur og næring",
  # Disse to prøver å ta forskjellene mellom nasjonale og regionale protester
  "Alta-samfunnet er i mot utbygging", "protestbevegelse", 
  #Dette prøver å fange ulike aktører
  "partipolitikk", "kommunestyret", "domstoler", "regjeringen", "myndigheter", "demonstranter", "politiet", 
  #Ting som staten gjør
  "saksbehandling og juridiske prosesser", "bygging av vei og anleggsvirksomhet", "undersøkelser og utredning", "arrestasjoner og bruk av tvangsmidler", 
  #Pro alta-utbyggingen
  "økonomisk og næringsmessig nytte", "må ha kraft og strøm", "Alta-kautokeinovassdraget bør bygges ut", 
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
  "Konstruktiv sivil ulydighet", "overdrevet maktbruk av myndigheter og politi", "fredelige demonstranter", "ikke-voldelige","Manglende utredning om konsekvenser", "skeptisk kraftprognose-motstand", "meningsløs kraftutbygging",
  #Meta
  "mediedekning", "historisk betydning", "kunst og kultur", "husker eller minnes", "feilinformasjon",
  #Hva er det som skjer? 
  "at et møte blir arrangert", "tv-program", "produksjon av film" 
)

summarize_data <- function(newspaper_group, variable_name) {
  if (is.null(newspaper_group)) {
    data <- renset_data
  } else {
    data <- renset_data %>%
      filter(newspaper %in% newspaper_group) # Assume 'newspaper' is the correct column name
  }
  
  data %>%
    group_by(year) %>%
    summarise(average_value = mean(!!as.name(variable_name), na.rm = TRUE))
}


results <- list()
for (group_name in names(newspaper_groups)) {
  for (var in variables) {
    result_name <- paste("summary_table", group_name, gsub("`| ", "", var), sep = "_")
    results[[result_name]] <- summarize_data(newspaper_groups[[group_name]], var)
  }}

plot_list <- list()
index <- 1


for (group_name in names(newspaper_groups)) {
  for (var in variables) {
    result_name <- paste("summary_table", group_name, gsub("`| ", "", var), sep = "_")
    data_frame <- results_viz[[result_name]]
    
    if (!is.null(data_frame)) {
      plot_list[[index]] <- ggplot(data_frame, aes(x = year, y = average_value)) +
        geom_line() + 
        geom_smooth(method = "loess", se = FALSE) +
        scale_y_continuous(limits = c(0.1, 0.6)) + 
        ggtitle(paste(group_name, var))
      index <- index + 1
    }
  }
}

grid.arrange(grobs = plot_list, ncol = 4, nrow = 4)

