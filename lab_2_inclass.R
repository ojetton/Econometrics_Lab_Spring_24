
# Load Package
library(pacman)
p_load(dplyr)

# Read the data:
p_load(readr)
data_url = "https://raw.githubusercontent.com/ojetton/Econometrics_Lab_Spring_24/main/muddy_data"

muddy_data = read_csv(url(data_url))

new_names = c("city", "number", "percent_male", "neighborhood_pop", "teachers", "gpa")

# Correct the variable names
school_data = muddy_data %>% rename_all(~new_names)

# Remove NAs
school_data = school_data %>% na.omit()

# Remove all schools with under 100 students
school_data = school_data %>% filter(number > 100)
  # Logical expressions: ==, !=, >, <, >=, <=

# Two new variables: student/teacher ratio, PERCENT male
school_data = school_data %>% 
                  mutate(percent_male = percent_male*100,
                         student_teacher_ratio = number/teachers)

# Run our boss' regression:
reg = lm(data = school_data,
         formula = gpa ~ student_teacher_ratio + neighborhood_pop + percent_male + factor(city))

summary(reg)
# coefficient about = -0.0099 or -0.01



### Other Dplyr Features

# What is the average GPA in each city?
  
  # Data for each city
portland_data = school_data %>% filter(city == "Portland")

mean(portland_data$gpa)


  # group_by() and summarize()
school_data %>% group_by(city) %>% summarize(mean(gpa))

  # find the average GPA for all the data
muddy_data %>% group_by(City) %>% summarize(mean(GPA))






