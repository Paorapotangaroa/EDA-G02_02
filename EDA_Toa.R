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
