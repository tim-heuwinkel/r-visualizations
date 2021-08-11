library(ggplot2)
library(dplyr)
library(zoo)
library(utils)
library(directlabels)
library(scales)
library(gghighlight)

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

data$dateRep <- as.Date(data$dateRep, format = "%d/%m/%Y")

data <- data %>%
  filter(dateRep < as.Date("2020-05-27"))

flags <- c("US", "UK", "IT", "FR", "ES", "DE", "AU", "NO", "AT")
group_weeks <- c(2.4285714, 2.0000000, 1.8571429, 1.7142857, 1.2857143, 1.4285714, 0.4285714, 0.1428571, 0.1428571)
deaths_sma <- c(174.285714, 101.928571, 100.85714, 46.785714, 54.571429, 27.214286, 4.785714, 3.071429, 3.642857)

data_stars <- data.frame(flags, group_weeks, deaths_sma)

data_clean_sorted_cut_id <- data %>%
  group_by(geoId) %>%
  mutate(deaths_sma = rollmean(deaths, k = 14, fill = NA, align = "center")) %>%
  na.omit() %>%
  arrange(dateRep, .by_group = TRUE) %>%
  filter(cumsum(deaths_sma >= 3.) > 0) %>%
  mutate(group_weeks = row_number() / 7) %>%
  mutate(country_flag = geoId %in% flags)

data_points <- data.frame(data_clean_sorted_cut_id)

data_points_clean <- data_points %>%
  group_by(geoId) %>%
  arrange(dateRep, .by_group = TRUE) %>%
  filter(row_number() == n())

data_clean_sorted_cut_id$countriesAndTerritories <- as.character(data_clean_sorted_cut_id$countriesAndTerritories)
data_clean_sorted_cut_id$countriesAndTerritories[data_clean_sorted_cut_id$countriesAndTerritories == "United_States_of_America"] <- "USA"
data_clean_sorted_cut_id$countriesAndTerritories[data_clean_sorted_cut_id$countriesAndTerritories == "United_Kingdom"] <- "UK"
data_clean_sorted_cut_id$countriesAndTerritories[!(data_clean_sorted_cut_id$geoId %in% flags)] <- " "
data_clean_sorted_cut_id$countriesAndTerritories <- as.factor(data_clean_sorted_cut_id$countriesAndTerritories)

com <- format_format(big.mark = ",", decimal.mark = " ", scientific = FALSE)

ggplot(data_clean_sorted_cut_id, aes(x = group_weeks, y = deaths_sma, col = geoId)) +
  geom_hline(yintercept = 0, color = "grey30", size = 1.5) +
  geom_line(alpha=0.8, show.legend = FALSE) +
  geom_dl(aes(label = countriesAndTerritories), method = list("last.points", cex = 0.8, hjust = -0.3, show.legend = FALSE)) +
  geom_point(data = data_points_clean, aes(x = group_weeks, y = deaths_sma, fill = geoId), show.legend = FALSE) +
  geom_point(data = data_stars, aes(x = group_weeks, y = deaths_sma, fill = flags), color = "grey30", shape = 23, show.legend = FALSE, size = 2.5) +
  gghighlight(max(country_flag) >= 1, use_direct_label = FALSE) +
  scale_y_continuous(trans = "pseudo_log", labels = com, sec.axis = sec_axis(~ ., breaks = derive(), labels = com), name = NULL, expand = c(0, 0), limits = c(0, 3000), breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,16), name = "Weeks since average daily deaths passed 3 ⟶", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)) +
  labs(title = "Daily death tolls are now falling in almost every western country",
       subtitle = "Daily deaths with coronavirus (14-day rolling average), by number of weeks since 3 average daily deaths first recorded\nDiamonds represent national lockdowns ◆",
       caption = "Heuwinkel graphic: Tim Heuwinkel / @timheu\nSource: European Centre for Disease Prevention and Control. Data updated May 26 2020, 13:00 CEST") +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(hjust = 0, colour = "grey30"),
        plot.caption = element_text(hjust = 0, colour = "grey30"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey85"),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "antiquewhite1"),
        axis.title.x = element_text(colour = "grey30"),
        axis.ticks = element_blank()) +
  annotate("text", x = 14, y = 1410, label = "HEUWINKEL", colour = "grey70", size = 7, family = "serif")

