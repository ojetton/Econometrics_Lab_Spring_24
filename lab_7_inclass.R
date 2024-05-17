## Lab 7 In-Class

library(pacman)
p_load(tidyverse, dplyr, janitor, tidymodels)

# load data
data = iris %>% clean_names()

# set seed
set.seed(213565)

# split data into training and testing
train_test_split = data %>% initial_split(prop = 0.75)

data_train = train_test_split %>% training()
data_test = train_test_split %>% testing()

# cross validation groups
data_cv = data_train %>% vfold_cv(v = 2)

# Create Recipe
data_recipe = data_train %>% recipe(species ~.)

# Clean your recipe
data_clean = data_recipe %>% prep() %>% juice()
  # prep tells it to estimate parameters
  # juice apply operations from the workflow


# Define the decision tree
model_tree = decision_tree(
  mode = "classification",
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = 5) %>%
  set_engine("rpart")


# Workflow
tree_workflow = workflow() %>%
  add_model(model_tree) %>%
  add_recipe(data_recipe)

# Tune
tree_tune_fit = tree_workflow %>%
  tune_grid(
    data_cv,
    grid = expand_grid(
      cost_complexity = seq(0, 10, by = 0.5),
      tree_depth = seq(1, 10, by = 1)
    ),
    metrics = metric_set(accuracy)
)

# Select best mode
best_flow = tree_workflow %>%
  finalize_workflow(select_best(tree_tune_fit, 
                                metric = "accuracy")) %>%
  fit(data = data_clean)


# Extract fitted model
best_tree = best_flow %>% extract_fit_parsnip()



# Test on testing data
y_hat = best_flow %>%
          predict(new_data = data_test,
                  type = "class")





