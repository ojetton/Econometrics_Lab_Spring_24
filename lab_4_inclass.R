# Loading ggplot
library(pacman) 
p_load(ggplot2)


time_url = "https://raw.githubusercontent.com/ojetton/Econometrics_Lab_Spring_24/main/time_data"

p_load(readr)

time_data = read_csv(url(time_url))

# Unemployment Rate over Time
ggplot(time_data, aes(x = date, y = UNRATE)) +
  geom_line() +
  labs(title = "Unemployment Rate over Time", 
       x = "Date",
       y = "Unemployment Rate (%)") +
  theme_bw()

# New Graph
ggplot(time_data, aes(x = date, y = UNRATE)) +
  geom_line(linetype = "dashed",
            color = "blue") +
  labs(title = "Unemployment Rate over Time", 
       x = "Date",
       y = "Unemployment Rate (%)") +
  ylim(0,15) +
  theme_dark()



# Multiple Lines in the Same Data set
ggplot(time_data, aes(x = date)) +
  labs(x = "Date", y = "Percent") +
  geom_line(aes(y = UNRATE, color = "blue")) +
  geom_line(aes(y = INF, color = "red")) +
  scale_color_identity(name = "",
                       guide = "legend",
                       labels = c("Unemployment Rate", "Inflation Rate")) +
  theme_bw() +
  theme(legend.position = "bottom")



# Scatter Plot
ggplot(time_data, aes(x = UNRATE, y = INF, color = FEDFUNDS)) +
  geom_point() +
  scale_color_gradient(low = "lightblue", high = "darkblue")


# Space Data
space_url = "https://raw.githubusercontent.com/ojetton/Econometrics_Lab_Spring_24/main/space_data"

space_data = read_csv(url(space_url))


# Bar graph
plot1 = ggplot(space_data, aes(y = reorder(Body,Volume), x = log(Volume))) +
  geom_bar(aes(fill = Type), stat = "identity") +
  labs(x = "Logged Cubic Kilometers (Bil)", y = "Celestial Body")


# Pie Chart
plot2 = ggplot(space_data, aes(x = "", y = log(Volume))) +
  geom_bar(aes(fill = Type), stat = "identity") +
  # Pie Chart:
  coord_polar("y") +
  theme_void()

# Multiple Plots
p_load(gridExtra)
grid.arrange(plot1, plot2)

