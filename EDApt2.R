library(tidyverse)
library(ggplot2)
library(dplyr)
library(DataExplorer)

# Read in the data
my_data <- read_csv("https://www.dropbox.com/scl/fi/p7qg5yosglzczlwv6697m/17_train.csv?rlkey=qsw29qqhe9dy2lnfzv2jv3g7t&dl=1")

# Drop the first row of Null data. no other data was null we can just use drop_na. Clean names
dataset <- my_data %>% drop_na() %>% janitor::clean_names()

# Sum up all the access columns. The columns are binary data so a sum of 0 and 1 is 1, 1 and 1 is 2. This will result in a count
# Of acceess peremission. I want ot see if this is related to the label.
dataset <- dataset %>%
  mutate(access_count = rowSums(select(., contains("access")), na.rm = TRUE))


# Converting columns to factors except 'access_count' which is numeric
dataset <- dataset %>%
  mutate(across(-access_count, as.factor))

# Run an IND T-Test. We need to do this because it is a numeric column and a categorical column with only two categories.
# This code is going to be partially ChatGPT. I wrote the analysis in Python and and had GPT convert it to R. This is because
# we haven't learned t-tests or ANOVAs in R and I'm quite familiar with them in Python.




# Explore the structure of the dataset
str(dataset)

introduce(dataset)

# Generate a quick overview of the dataset
# exploreData(dataset)

# Plot histogram for numeric variables
# plot_histogram(dataset)

# Plot bar chart for categorical variables
plot_bar(dataset)

plot_missing(dataset)
plot_correlation(dataset)
plot_prcomp(dataset)
setwd("/home/rstudio/EDA-Toa/")
create_report(dataset,output_dir = getwd())
