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







# Independent Variable Analysis

                                  # FIRST LOOK

#The first row is all NA's so we are gonna remove that
ind_data <- my_data %>% 
  slice(-1)

#First a look at the number of columns in the data
ind_data %>% 
  ncol()

#Every column has either the value 0 or 1 so now we looked at the count of each
# for every variable
ind_data %>% 
pivot_longer(cols = everything()) %>%
  group_by(name) %>% 
  summarise(count_0 = sum(value == 0),
            count_1 = sum(value == 1))

#We can see that there are a lot of variables with none or very little 
# values of 1
ind_data %>% 
  pivot_longer(cols = everything()) %>%
  group_by(name) %>% 
  summarise(count_0 = sum(value == 0),
            count_1 = sum(value == 1)) %>% 
  arrange(count_1) %>% 
  print(n = 100)

#We can also see there are some variables with a lot of values of 1

ind_data %>% 
  pivot_longer(cols = everything()) %>%
  group_by(name) %>% 
  summarise(count_0 = sum(value == 0),
            count_1 = sum(value == 1)) %>% 
  arrange(desc(count_1)) %>% 
  print(n = 20)

#We then can compare a particular variable against the label (dependent) variable
#to see the counts of 0's and 1's for both. The table below show
#when the label is 0 or 1, how many times a particular independent variable is 0 and 1.
#*** We are using GET_TASKS as the independent variable here but it can be switched with anything

ind_data %>%
  group_by(Label, GET_TASKS) %>%
  summarise(count = n())


#The graph below shows the same numbers as the table above but with the dependent variable along
#the x axis and the fill as the Label
ind_data %>% 
  mutate(across(everything(),factor)) %>% 
ggplot(aes(x = GET_TASKS, fill = Label)) + 
  geom_bar(position = "fill")+
  labs(title = "Relationship between Variable and Malware Label")



                              # GROUPING EXPLORATION

# We noticed a lot of the independent variables has a system event as the first word in their name
#For example ACCESS or WRITE or READ. We decided to see how many variables had the same system flag
#name in the tibble below. Note we looked up regular expressions to pull the first word from the 
#variable names


ind_data %>% 
  pivot_longer(cols = everything()) %>%
  distinct(name) %>% 
  mutate(category = str_extract(name, "^[^_/]+")) %>% 
  group_by(category) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  print(n = 100)

#Now we wanted to see a similiar thing above but with the groupings. We want to see the count
#of 0's per groupd, the count of 1's per group, the total count per group, and the 
#percent of 1's per group. The first tibble is that infor for the label as reference.
# The second tibble is the groupings but arranged by highest percentage of 1 values

ind_data %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(category = str_extract(name, "^[^_/]+")) %>% 
  group_by(category) %>%
  summarise(count_0 = sum(value == 0),
            count_1 = sum(value == 1),
            total = (count_0 + count_1)) %>%
  mutate(percent_of_1 = (count_1)/(total)) %>% 
  filter(category == 'Label')

ind_data %>%
  pivot_longer(cols = everything()) %>% 
  mutate(category = str_extract(name, "^[^_/]+")) %>% 
  group_by(category) %>%
  summarise(count_0 = sum(value == 0),
            count_1 = sum(value == 1),
            total = (count_0 + count_1)) %>%
  mutate(percent_of_1 = (count_1)/(total)) %>% 
  arrange(desc(percent_of_1))


#Last part of this grouping independent variables is a graphically look of what we
# did above. Since there are so many variables, we filtered out the variables that
# have a percent of 1 below 20%. What we are left with are groupings of variables
# that have a good amount of 1's as their values that we could be potentially
# use for modeling

ind_data %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(category = str_extract(name, "^[^_/]+")) %>% 
  group_by(category) %>%
  summarise(count_0 = sum(value == 0),
            count_1 = sum(value == 1)) %>%
  mutate(percent_of_1 = (count_1)/(count_1+count_0)) %>% 
  filter(percent_of_1 >= 0.2) %>% 
  ggplot(aes(x = category, y = percent_of_1, fill = category))+
  geom_bar(stat = "identity")+
  facet_wrap(~category)+
  theme_bw()+
  theme(legend.position = "none")
#Note we looked up this function to get rid of the legend because
#it was annoying and in the way





                         # CHI SQUARE TESTING

#The last part of the independent variable analysis we did was Chi Square testing
#for all the independent variables. We will walk through each step but please note that
#we haven't done Chi Square testing at all so we had to look some stuff up

#This is a package we had to install
install.packages("purrr")
library(purrr)

#We we first tried to make the tibble it got mad that we had variables with only
#one value (0) so we had to drop all of the columns that only had a single value in
#them.

#First we pivoted the data and got the column names that had no counts of 1
#as a value. We saved those columns names out to a tibble named tibble_with_columns_to_drop
tibble_with_columns_to_drop <- ind_data %>% 
  pivot_longer(cols = everything()) %>%
  group_by(name) %>% 
  summarise(count_0 = sum(value == 0),
            count_1 = sum(value == 1)) %>% 
  filter(count_1 == 0) %>% 
  select(name)

#Then we grabbed just the names from the tibble and saved those to a vector
columns_to_drop <- tibble_with_columns_to_drop[[1]]

#Then we used the names in the vector to drop all those columns in our dataset. We then
#converted the columns to factors. Then we selected everything but the Label to use
#for the chi square test. Then we had to look up how to do a chi square test and we
#passed in the data to do that
chi_square_results <- ind_data %>% 
  select(!all_of(columns_to_drop)) %>% 
  mutate(across(everything(), factor)) %>% 
  select(!Label) %>%
  map(~chisq.test(ind_data[["Label"]], .x))

#We now take the p value and statistic from the chi square test and map it to 
#results_summary. Like I said before, we had to look up how to do this
results_summary <- map(chi_square_results, ~list(p_value = .x$p.value, statistic = .x$statistic))

# We then converted the results_summary into a tibble to read easier. Again,
# we looked up how to do this and how to use the command map_df
chi_square_tibble <- map_df(results_summary, bind_rows, .id = "column")


#We did a little exploring of the variables with the p values.
chi_square_tibble %>% 
  arrange(p_value)

chi_square_tibble %>% 
  arrange(desc(p_value))

#Here we are grouping the variables like we did above but with a p-value. We are also
#computing the average p value based on category
chi_square_tibble %>% 
  mutate(category = str_extract(column, "^[^_/]+")) %>% 
  group_by(category) %>% 
  mutate(avg_p_value = mean(p_value),
         avg_statistic = mean(statistic))

#Now we are l0oking at the average p value for each category and arranging them by
#p value
chi_square_tibble %>% 
  mutate(category = str_extract(column, "^[^_/]+")) %>% 
  group_by(category) %>% 
  mutate(avg_p_value = mean(p_value),
         avg_statistic = mean(statistic)) %>% 
  group_by(category) %>% 
  summarise(avg_p_value = first(avg_p_value),
            avg_statistic = first(avg_statistic))

#It's interesting to note the similar categories here that have a large amount of
#1's as values that we found before, such as the KILL and WAKE categories
chi_square_tibble %>% 
  mutate(category = str_extract(column, "^[^_/]+")) %>% 
  group_by(category) %>% 
  mutate(avg_p_value = mean(p_value),
         avg_statistic = mean(statistic)) %>% 
  group_by(category) %>% 
  summarise(avg_p_value = first(avg_p_value),
            avg_statistic = first(avg_statistic)) %>% 
  arrange(avg_p_value)
  





