# Required package(s)
require(dplyr)

# Load data
data <- unz("data/PostsData.zip", filename = "posts.csv") %>% # Link to zipped file
  read.csv(header = TRUE, sep = ",", stringsAsFactors = FALSE) %>% # Read zipped file
  select(Id, Name, AcceptedAnswerId, CleanBody) # Select fields needed for joins

# Question and Answer dataframes
#q <-
#a <-