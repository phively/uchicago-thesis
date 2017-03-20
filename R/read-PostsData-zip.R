# 2017-03-19 update: leave LaTeX code in the document

# Required package(s)
require(dplyr)

# Load data
data <- unz("data/PostsData.zip", filename = "posts.csv") %>% # Link to zipped file
  read.csv(header = TRUE, sep = ",", stringsAsFactors = FALSE) %>% # Read zipped file
  select(Id, Name, ParentId, Title, Body) # Select fields needed for joins

#
data <- data %>% mutate(
  CleanBody = paste(Title, Body, sep = " "), # Concatenate Title to left of Body
  CleanBody = CleanBody %>%
    gsub(pattern = "<code>.*?</code>", replacement = " ", x = .), # Strip <code> tags
  CleanBody = CleanBody %>%
    lapply(FUN = read_html) %>% # Read CleanBody as an html file
    lapply(FUN = xml_text) %>% # Extract any text between valid <tags></tags>
    unlist() %>%
    # Remove URLs starting with http and ending with a space
    str_replace_all("http[^[:space:]]*", " ") %>%
    # Remove newlines
    str_replace_all("[\r\n]" , " ")
)

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