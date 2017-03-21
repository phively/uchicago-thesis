# Required package(s)
require(dplyr)

# Load data
data <- unz("data/PostsData.zip", filename = "posts.csv") %>% # Link to zipped file
  read.csv(header = TRUE, sep = ",", stringsAsFactors = FALSE) %>% # Read zipped file
  select(Id, Name, ParentId, CleanBody) # Select fields needed for joins

# Question and Answer merged dataframe
merged <-
  bind_rows(
    # Questions
    data %>% filter(Name == "Question") %>% select(Id, CleanBody),
    # Answers, with associated question's ID inserted
    data %>% filter(Name == "Answer") %>% select(Id = ParentId, CleanBody)
  ) %>%
  group_by(Id) %>% # Answers roll up into associated question
  summarise_each(
    funs(paste(., collapse = " ")) # Merge associated questions/answers
  )

# Cleanup
remove(data)