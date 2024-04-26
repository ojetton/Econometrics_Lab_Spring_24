


library(pacman)
p_load(ggplot2)



# Load package and data
p_load(tidyquant, dplyr)

un_data = tq_get("UNRATE", get = "economic.data") %>% mutate(UNRATE = price)

ff_data = tq_get("FEDFUNDS", get = "economic.data") %>% mutate(FEDFUNDS = price)


p_load(fredr)

fredr_set_key("dc51717c249f72d5abdc1cc19afe0e8a")

inf_data = fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2014-01-01"),
  frequency = "m", # quarterly
  units = "pc1" # means "percent change from 1 year ago"
) %>% mutate(INF = value) %>% select(date, INF)



time_data = merge(un_data, ff_data, by = "date") %>% select(date, UNRATE, FEDFUNDS)

time_data = merge(time_data, inf_data, by = "date")


# Download
write.csv(time_data,"/Users/owenjetton/Documents/PhD Year 3/Metrics_Spring/time_data", row.names = FALSE)









### Planet data!


# Data get Wikipedia
p_load(rvest)

solar_html = read_html("https://en.wikipedia.org/wiki/List_of_Solar_System_objects_by_size")


solar_table = solar_html %>% 
  html_nodes("table") %>% 
  .[2] %>% 
  html_table() %>%
  .[[1]]

# delete first row
solar_table = solar_table[-1,]

# delete last row
solar_table = solar_table[-37,]


# Volume and Body name (first and fifth columns)
solar_clean = solar_table %>% dplyr::select(1,5,14)

new_names = c("Body", "Volume", "Type")

# My favorite package <3
p_load(magrittr)

solar_clean %<>%
  rename_all(~new_names) %>%
  mutate(Type = if_else(grepl("star", Type) == T, "Star",
                        if_else(grepl("planet", Type) == T, "Planet",
                        if_else(grepl("moon", Type) == T, "Moon",
                                "Other"))))




solar_clean = solar_clean %>%
  mutate(Volume = gsub("\\[\\d+\\]", "", Volume),
         Volume = gsub("\\[\\c\\]", "", Volume))

solar_clean$Body = c("Sun", "Jupiter", "Saturn", "Uranus", "Neptune", "Earth", "Venus", "Mars",
  "Ganymede", "Titan", "Mercury", "Callisto", "Io", "Moon", "Europa", "Triton",
  "Pluto", "Eris", "Haumea", "Titania", "Rhea", "Oberon", "Iapetus", "Makemake",
  "Gonggong", "Charon", "Umbriel", "Ariel", "Dione", "Quaoar", "Tethys", "Ceres",
  "Orcus", "Sedna", "Salacia", "2002 MS4")


solar_clean %<>%
  mutate(Volume = as.numeric(gsub("\\,", "", Volume)))



ggplot(solar_clean, aes(x = reorder(Body,Volume), y = log(Volume))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Celestial Body", y = "Log of Volume in Billions of KM3") +
  theme_minimal()


space_data = solar_clean

# Download
write.csv(space_data,"/Users/owenjetton/Documents/PhD Year 3/Metrics_Spring/space_data", row.names = FALSE)


