library(viridisLite)
library(highcharter)
library(tidyverse)
library(htmlwidgets)

options(browser = "/usr/bin/firefox")

data <- read_csv("../african_latest_data.csv")
data$dateRep <- as.Date(data$dateRep, format="%d/%m/%Y")
data$popData2018 <- data$popData2018 / 1000000
data$cases_per_million <- data$cumulative_cases / data$popData2018
data$deaths_per_million <- data$cumulative_deaths / data$popData2018

data_graphic <- data[, c("countryterritoryCode", "geoId", "dateRep", "countriesAndTerritories", "cases_per_million", "cumulative_cases", "cases", "deaths", "cumulative_deaths", "deaths_per_million")]

## remove france

data_graphic <- subset(data_graphic, countriesAndTerritories != "France")
data_graphic <- subset(data_graphic, countriesAndTerritories != "Seychelles")
# data_graphic <- subset(data_graphic, countriesAndTerritories != "Mauritius")

## rename
data_graphic$`iso-a3` <- data_graphic$countryterritoryCode

## get latest day
latest_day <- max(unique(data_graphic$dateRep))

data_graphic_latest <- subset(data_graphic, dateRep == latest_day) 
## merge in WB data

wb_data <- read_csv("../WB_data/wb_data.csv")
wb_data$geoId <- wb_data$`hc-key`

data_graphic_latest <- merge(data_graphic_latest, wb_data, by = "geoId", all.x = TRUE)

wb_data$countriesAndTerritories <- wb_data$country_

## now merge Namibia
data_graphic_latest <- merge(data_graphic_latest, wb_data, by = "countriesAndTerritories", all.x = TRUE)


data_graphic_latest$per_pop_65 <-  data_graphic_latest$SP.POP.65UP.TO.ZS.x
data_graphic_latest$pop_urban <- data_graphic_latest$SP.URB.TOTL.IN.ZS.x
data_graphic_latest$health_exp_per_cap <- data_graphic_latest$SH.XPD.CHEX.PC.CD.x
data_graphic_latest$int_ext_debt <- data_graphic_latest$DT.INT.DECT.EX.ZS.x

## graphic

data_graphic_latest$colors <- colorize(data_graphic_latest$per_pop_65, colors = c("#0066CC", "#999999"))

## data_graphic_latest <- data_graphic_latest[, names(data_graphic_latest) %in% c("cases_per_million", "deaths_per_million", "health_exp_per_cap", "colors", "countriesAndTerritories")]

## data_graphic_latest <- data_graphic_latest[complete.cases(data_graphic_latest), ]

data_graphic_latest <- data_graphic_latest[!is.na(data_graphic_latest$colors), ]

x <- c("Country", "Number Cases Per Million", "Deaths per Million", "Percent Pop. Over 65", "Health Expenditure Per. Cap. (US$)")
y <- c( "{point.countriesAndTerritories}", sprintf("{point.%s:.2f}", c("cases_per_million")), "{point.deaths_per_million:.2f}", "{point.per_pop_65:.2f}", "{point.health_exp_per_cap:.2f}")

tltip <- tooltip_table(x, y)


health_scatter <- hchart(data_graphic_latest, "scatter", hcaes(cases_per_million, deaths_per_million, size = health_exp_per_cap, color = colors)) %>% 
  hc_boost(enabled=FALSE) %>%
  hc_xAxis(type = "logarithmic", reversed = FALSE) %>% 
  hc_yAxis(type = "logarithmic", gridLineWidth = 0) %>% 
  hc_title(text = "COVID-19 in Africa: Cumulative Cases, Deaths, and Health Expenditure by Country", align = "left") %>% 
    hc_subtitle(text = "Bubble Size is health expenditure per capita (in US$). Bubble color is % of population over 65 (more gray means larger %). Cumulative Cases and Deaths are in a Logarithmic Scale", align = "left") %>%
    hc_add_theme(hc_theme_google()) %>%
    hc_xAxis(title = list(text = "Number of Cases Per Million (in log)")) %>%
        hc_yAxis(title = list(text = "Deaths per Million (in log)")) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)


  ## hc_size(height = 600)
health_scatter

## Save vis
saveWidget(health_scatter, file="cases_deaths_health.html")



## map to create: per capita health expenditure per case (in hundred millions): the higher it is the better the country is able to care for the cases---assuming more spending means more readiness
