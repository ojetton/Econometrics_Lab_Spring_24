
# Create Fake Muddy data to clean

library(pacman)
p_load(dplyr, magrittr, ggplot2)

N = 180

muddy_data = tibble(
  City = c(rep("Portland",90), rep("Salem", 40), rep("Eugene", 50)),
  
  # a "score" for each city means there are city fixed effects
  score = c(rep(3,90), rep(2, 40), rep(1, 50)),
  Number = round(rnorm(N, mean = 300 + 20*score, sd = 20) + rbinom(N,1,0.7)*rnorm(N, mean = 200, sd = 40) - rbinom(N,1,0.5)*rnorm(N, mean = 150, sd = 40)),
  gender = round(rnorm(N, mean = 0.5, sd = 0.02), digits = 2),
  NEIGHBORHOOD = round(Number*5 + rnorm(N + 20*score, mean = 10, sd = 25)),
  Teachers = if_else(City == "Portland", round(Number/20 + rnorm(N, mean = 0, sd = 3)),
                     if_else(City == "Salem", round(Number/15  + rnorm(N, mean = 0, sd = 4)),
                             round(Number/17  + rnorm(N, mean = 0, sd = 4)))),
  GPA = 4 - 0.1*Number/Teachers - 0.02*gender + 0.13*score + if_else(Number < 100, 0.2, 0) - rnorm(N,0,0.25),
)


# Insert random NAs
for (i in 2:6) {
  for (j in 1:N) {
    
    H = rbinom(1,1,0.09)
    
    if (H == 0){
      muddy_data[j,i] = muddy_data[j,i]
    }
    else {
      muddy_data[j,i] = NA
    }
  }
}


# Take a look
ggplot(muddy_data) +
  geom_histogram(aes(x = GPA))


ggplot(muddy_data) +
  geom_histogram(aes(x = Number))


# Remove score variable
muddy_data %<>% select(-c(score))

ggplot(muddy_data, aes(x = Number, y = Teachers, color = factor(City))) +
  geom_point()

# Download
write.csv(muddy_data,"/Users/owenjetton/Documents/PhD Year 3/Metrics_Spring/muddy_data", row.names = FALSE)

