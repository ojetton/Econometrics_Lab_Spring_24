# Load packages
library(pacman)
p_load(dplyr, tidyquant)

un_data = tq_get("UNRATE", get = "economic.data", from = "1948-01-01")

summary(un_data)




# fredr
p_load(fredr, ggplot2)

fredr_set_key("dc51717c249f72d5abdc1cc19afe0e8a")

# Download the UNRATE again
un_data_2 = fredr(series_id = "UNRATE")

ggplot(un_data_2, aes(x=date, y = value)) +
  geom_line()

# Quarterly Unemployment data
un_data_q = fredr(series_id = "UNRATE",
                  frequency = "q")

ggplot(un_data_q, aes(x=date, y = value)) +
  geom_line()


# Annual Unemployment data
un_data_a = fredr(series_id = "UNRATE",
                  frequency = "a")

ggplot(un_data_a, aes(x=date, y = value)) +
  geom_line()


# Quarterly Inflation Data
inflation_data = fredr(
  series_id = "CPIAUCSL",
  frequency = "q", # quarterly
  units = "pc1" # means "percent change from 1 year ago"
)

ggplot(inflation_data, aes(x=date, y = value)) +
  geom_line()



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
solar_clean = solar_table %>% dplyr::select(1,5)

new_names = c("Body", "Volume")

solar_clean = solar_clean %>%
  rename_all(~new_names)

solar_clean = solar_clean %>%
  mutate(Volume = gsub("\\[\\d+\\]", "", Volume)) %>%
  mutate(Volume = gsub("\\[\\c\\]", "", Volume))

summary(solar_clean)

solar_clean = solar_clean %>%
  mutate(Volume = as.numeric(gsub("\\,", "", Volume)))


ggplot(solar_clean, aes(x = reorder(Body,Volume), y = log(Volume))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Celestial Body", y = "Log of Volume in Billions of KM3") +
  theme_minimal()


