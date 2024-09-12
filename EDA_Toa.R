# Names: Bruyn Decker, Lillie Heath, Riley Marshall, Toa Pita
# Sections: 2, 3, 1, 2
# Description: This is our analysis file for the security dataset

# Bring in libraries

# These are pretty standard
library(tidyverse)
library(ggplot2)
library(dplyr)


# Bring in the data
my_data <- read_csv("https://www.dropbox.com/scl/fi/p7qg5yosglzczlwv6697m/17_train.csv?rlkey=qsw29qqhe9dy2lnfzv2jv3g7t&dl=1")

# Show that there is only one missing value. A look at the data shows the first row is null all the way across.
my_data %>% group_by(Label) %>% count()

# Drop the null row
my_data <- my_data %>% slice_tail(n=-1)

# Make sure that ggplot treats the label as categorical (I wrote this code was before I know about factors)
graph_data <- my_data %>% mutate(Label = as.character(Label))

# Plot the distribution of the data. It is a categorical label so we can't do much more than a count. 
# Measures of center, spread, etc. only apply to numeric columns

# Make sure the title tells the story we want instead of just being a generic name
# Remove the unnecessary labels - The title tells us everything we would have gotten from axis labels
# Set a good theme. (Would have removed the border and background grid but I didn't know how to)
# Use color to draw the eye to the information we want to present. We want people to notice that there is
# Significantly more malware than not in the dataset. The exact counts don't matter so much as the proportion
# so I also removed the counts
# Remove all other clutter from the graph
ggplot(graph_data, aes(x = Label, fill=Label)) + 
  geom_bar() + 
  ggtitle("Malware shows up more often than Non-Malware in the dataset") + 
  ylab("") +
  theme_bw() +
  scale_fill_manual(values = c("grey","blue"),labels=c("Non-Malware","Malware")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())



# Independent Variable Analysis

                                  # FIRST LOOK

#The first row is all NA's so we are gonna remove that
# This was leftover after from when we were working in different files
# ind_data <- my_data %>% 
#   slice(-1)
ind_data <- my_data
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

#Below is a graph showing the density of the distributions of the counts of 1
# across all the columns. As you can see, it is very skewed and most columns
# contain a low amount of 1's
ind_data %>% 
  pivot_longer(cols = everything()) %>%
  group_by(name) %>% 
  summarise(count_0 = sum(value == 0),
            count_1 = sum(value == 1)) %>% 
  ggplot(aes(x = count_1))+
  geom_density(fill = 'blue')+
  theme_bw()+
  labs(title = "Density of the counts of 1 across variables",
      x = "Counts of 1's")

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
#install.packages("purrr")
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

chi_square_tibble

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

#Now we are looking at the average p value for each category and arranging them by
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
  





