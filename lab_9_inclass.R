

library(pacman) 
p_load(maps)

us.cities


# dplyr
p_load(dplyr)
capitals = us.cities %>% filter(capital == 2)

map(database = "usa")
points(x = capitals$long, y = capitals$lat)
title("US Capital Cities")


map("world")
map(database = "usa", add = T)
points(x = capitals$long, y = capitals$lat, col = "red")



world_capitals = world.cities %>% filter(capital == 1)

map("world")
points(x = world_capitals$long, y = world_capitals$lat, 
       col = "blue", pch = 20, cex = 0.5)


map("world", "china")
map.cities(country = "China", capitals = 0,
           pch = 20, col = "blue", cex = 0.5)
map.cities(country = "China", capitals = 3,
           pch = 20, col = "green", cex  = 1)
map.cities(country = "China", capitals = 2,
           pch = 20, col = "red", cex  = 1.5)
map.cities(country = "China", capitals = 1,
           pch = 20, col = "orange", cex  = 2)




map(database = "state")
points(x = capitals$long, y = capitals$lat)
title("US Capital Cities")



map(database = "state", region = "oregon")

map(database = "state", regions = c("Oregon", "Washington", "California"))


# get most populous city within these 3 states
west_city = us.cities %>%
  filter(country.etc %in% c("OR", "WA", "CA")) %>%
  group_by(country.etc) %>%
  slice_max(order_by = pop, n = 1) %>%
  ungroup()


map(database = "state", regions = c("Oregon", "Washington", "California"))
points(x = west_city$long, y = west_city$lat, col = "blue")



# usmap and ggplot2
p_load(usmap, ggplot2)

plot_usmap(regions = "counties") + 
  labs(title = "US Counties") +
  theme_bw()




# readr
p_load(readr)

url_name = "https://raw.githubusercontent.com/ojetton/Econometrics_Lab_Spring_24/main/state_inflation_data"

inflation = read_csv(url(url_name))


inflation = inflation %>%
  select(1,4) %>%
  rename_all(~c("state", "inflation")) %>%
  mutate(state = tolower(state))

state_data = map_data("state")

state_map = merge(x = inflation, y = state_data,
                  by.x = "state", by.y = "region")


ggplot(state_map, aes(x = long, y = lat, group = group, fill = inflation)) +
  geom_polygon(color = "black") +
  scale_fill_continuous(low = "white", high = "red") +
  theme_void()






