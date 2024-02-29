library(tidyverse)
library(ggplot2)
library(dplyr)
my_data <- read_csv("https://www.dropbox.com/scl/fi/p7qg5yosglzczlwv6697m/17_train.csv?rlkey=qsw29qqhe9dy2lnfzv2jv3g7t&dl=1")

my_data %>% group_by(Label) %>% count()


# Load necessary libraries
library(tidyverse)

binary_data <- my_data
variable <- "Label"
# Generate sample binary data


# 1. Bar Chart
bar_chart <- ggplot(binary_data, aes(x = factor(variable))) +
  geom_bar() +
  labs(title = "Bar Chart of Binary Variable",
       x = "Variable",
       y = "Count")

# 2. Pie Chart
pie_chart <- ggplot(binary_data, aes(x = factor(variable))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Binary Variable",
       x = "Variable",
       y = "")

# 3. Histogram
histogram <- ggplot(binary_data, aes(x = factor(variable))) +
  geom_bar() +
  labs(title = "Histogram of Binary Variable",
       x = "Variable",
       y = "Count")

# 4. Area Chart
area_chart <- ggplot(binary_data, aes(x = seq_along(variable), fill = factor(variable))) +
  geom_area() +
  labs(title = "Area Chart of Binary Variable",
       x = "Index",
       y = "Count")

# 5. Stacked Bar Chart
stacked_bar_chart <- ggplot(binary_data, aes(x = factor(variable), fill = factor(variable))) +
  geom_bar() +
  labs(title = "Stacked Bar Chart of Binary Variable",
       x = "Variable",
       y = "Count")

# 6. Heatmap
heatmap_data <- data.frame(
  variable1 = sample(c(0, 1), 100, replace = TRUE),
  variable2 = sample(c(0, 1), 100, replace = TRUE)
)
heatmap <- ggplot(heatmap_data, aes(x = factor(variable1), y = factor(variable2))) +
  geom_tile(aes(fill = factor(variable1)), color = "white") +
  labs(title = "Heatmap of Binary Variables",
       x = "Variable 1",
       y = "Variable 2",
       fill = "Count")

# 7. Violin Plot or Box Plot (Choosing one)
violin_plot <- ggplot(binary_data, aes(x = factor(variable))) +
  geom_violin() +
  labs(title = "Violin Plot of Binary Variable",
       x = "Variable",
       y = "Count")

# 8. Scatter Plot with Jitter
scatter_plot <- ggplot(binary_data, aes(x = seq_along(variable), y = variable)) +
  geom_jitter(width = 0.2) +
  labs(title = "Scatter Plot with Jitter of Binary Variable",
       x = "Index",
       y = "Variable")

# 9. Stacked Area Chart
stacked_area_chart <- ggplot(binary_data, aes(x = seq_along(variable), fill = factor(variable))) +
  geom_area() +
  labs(title = "Stacked Area Chart of Binary Variable",
       x = "Index",
       y = "Count")


# 10. Chi Square Test Tibble w/ Categorical Data ALL DATA

    # Converting the data to categorical
cat_data <- data.frame(lapply(my_data, as.factor))
cat_data <- cat_data[-1, ]

    #Chi Square Test
    # Prepare a vector of variable names, excluding 'Label'
variable_names <- names(cat_data)[!names(cat_data) %in% "Label"]

    # Initialize an empty list to store the results
results_list <- list()

    # Loop through each variable to perform the Chi-squared test
for(var_name in variable_names) {
  # Perform the Chi-squared test
  test_result <- chisq.test(table(cat_data[[var_name]], cat_data$Label))
  
  # Store the results with the variable name
  results_list[[var_name]] <- broom::tidy(test_result) %>%
    mutate(variable = var_name)
}

    # Combine the results into a single dataframe
results_df <- bind_rows(results_list, .id = "variable")

    # Correcting the issue by ensuring variable names are included
results_df <- results_df %>%
  select(variable, statistic = statistic, p.value = p.value, df = parameter)

results_df



# 10.5. Chi Square Test Tibble w/ Categorical Data CLEAN

    #This whole section is finding the variables that have less than 100 counts of '1'
    #and then getting the variable names then removing them from the data set
    #and saving them to a data set called cleaned_cat_data
counts_of_ones <- cat_data %>%
  summarise(across(everything(), ~sum(.x == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "count_of_ones")

columns_less_than_hundres_ones <- counts_of_ones %>% 
  filter(count_of_ones < 100) %>% 
  mutate(count_of_ones = NULL)

columns_to_drop <- columns_less_than_hundres_ones[[1]]

cleaned_cat_data <- cat_data %>% 
  select(-all_of(columns_to_drop))

cleaned_cat_data %>% 
  glimpse()


    #This section does the same as above, tests all the chi squared for the new data set
    #then filters them to have a p value of 0.05 or less
# Prepare a vector of variable names, excluding 'Label'
variable_names_2 <- names(cleaned_cat_data)[!names(cleaned_cat_data) %in% "Label"]

# Initialize an empty list to store the results
results_list_2 <- list()

# Loop through each variable to perform the Chi-squared test
for(var_name in variable_names_2) {
  # Perform the Chi-squared test
  test_result_2 <- chisq.test(table(cleaned_cat_data[[var_name]], cleaned_cat_data$Label))
  
  # Store the results with the variable name
  results_list_2[[var_name]] <- broom::tidy(test_result_2) %>%
    mutate(variable_2 = var_name)
}

# Combine the results into a single dataframe
results_df_2 <- bind_rows(results_list_2, .id = "variable")

# Correcting the issue by ensuring variable names are included
results_df_2 <- results_df_2 %>%
  select(variable_2, statistic = statistic, p.value = p.value, df = parameter)

results_df_2$df <- NULL
results_cleaned_2 <- results_df_2 %>%
  filter(p.value <= 0.05)

#Getting these names and then dropping all the columns not with these named
#to the cleaned data set

columns_to_keep <- results_cleaned_2[[1]]
cleaned_cat_data_final <- cleaned_cat_data %>%
  select(all_of(columns_to_keep), Label)


cleaned_cat_data_final %>% 
  glimpse()

    #Order the variables according to p score on the chi square test
results_cleaned_2 %>% 
  arrange(p.value)





# 11. Individual Variable Analysis (Confusion Matrix and Graphs)

    # Enter variable where GET_TASKS is
cat_data %>%
  group_by(Label, GET_TASKS) %>%
  summarise(Count = n(), .groups = "drop") %>%
  # Calculate the total count for each Label category
  group_by(Label) %>%
  # Add a new column for percentage
  mutate(Total = sum(Count),
         Percentage = (Count / Total) * 100)

ggplot(cat_data, aes(x = GET_TASKS, fill = Label)) + 
  geom_bar(position = "fill")+
  labs(title = "Relationship between Variable and Malware Label")




