#1
# Setting up the environment
library(tidyverse)

#2
# Reading in data
entrepreneurs_data <- read_csv("Entrepreneurs_data.csv")

#3
# Calculating total number of entrepreneurs
number_of_entrepreneurs <- nrow(entrepreneurs_data)

#4
# Calculating the average age
average_age <- mean(entrepreneurs_data$Age)

#5
# Creating a subset of the data for young entrepreneurs
young_entrepreneurs <- entrepreneurs_data %>%
  filter(Age <= 35)

#6
# Calculating the number of young entrepreneurs
number_of_young_entrepreneurs <- nrow(young_entrepreneurs)

#7
# Calculating the mean annual income of young entrepreneurs
mean_annual_income <- mean(young_entrepreneurs$Annual_Income)

#8
# Calculating the median annual income of young entrepreneurs
median_annual_income <- median(young_entrepreneurs$Annual_Income)

#9
# Calculating the percent of young entrepreneurs
percent_of_young_entrepreneurs <- number_of_young_entrepreneurs / number_of_entrepreneurs * 100

#10
# Calculating the average percentage of increase in income for young entrepreneurs
income_increase_percent <- mean(young_entrepreneurs$Income_Increase_%)

#11
# Calculating the average number of start-ups for young entrepreneurs
average_start_ups <- mean(young_entrepreneurs$Number_of_Startups)

#12
# Creating a pie chart to visualize the type of business of the young entrepreneurs 
business_type_pie <- ggplot(young_entrepreneurs, aes(x=1, fill=Business_Type)) + 
  geom_bar(width=1) +
  labs(title="Young Entrepreneurs by Business Type", x="", y="") 

#13
# Creating a histogram to visualize the age of the young entrepreneurs
age_histogram <- ggplot(young_entrepreneurs, aes(x=Age)) +
  geom_histogram(binwidth = 2) +
  labs(title="Age Distribution of Young Entrepreneurs", x="Age", y="")

#14
# Creating a line graph to visualize the annual income of the young entrepreneurs
income_graph <- ggplot(young_entrepreneurs, aes(x=Age, y=Annual_Income)) +
  geom_line() +
  labs(title="Annual Income of Young Entrepreneurs", x="Age", y="Annual Income")

#15
# Creating a scatter plot to visualize the relationship between age and number of startups
startup_scatter <- ggplot(young_entrepreneurs, aes(x=Age, y=Number_of_Startups)) +
  geom_point() +
  labs(title="Relationship between Age and Number of Startups", x="Age", y="Number of Startups")

#16
# Outputting the results to a word document
word_doc <- 
  rmarkdown::word_document(
    paste0("Young_Entrepreneurs_", Sys.Date(), ".docx"),
    title = "Young Entrepreneurs Summary"
  )

#17
# Writing a summary of the analysis
writeLines(
  c(
    "This analysis examined the characteristics of young entrepreneurs based on data from 500 individuals.",
    paste("There are a total of", number_of_entrepreneurs, "entrepreneurs in the dataset with an average age of", round(average_age,2),"years old."),
    paste("Of these entrepreneurs,", number_of_young_entrepreneurs, "are classified as young entrepreneurs with an average annual income of", round(mean_annual_income,2), "and a median annual income of", round(median_annual_income,2),"."),
    paste("The young entrepreneurs comprise", round(percent_of_young_entrepreneurs,2),"% of the total entrepreneurs and have an average income increase of", round(income_increase_percent,2),"%."),
    paste("They have also have an average of", round(average_start_ups,2),"startups."),
    sep = "\n"
  )
  , con=word_doc
)

#18
# Closing the word document
close(word_doc)