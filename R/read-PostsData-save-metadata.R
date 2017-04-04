# Does inline $ ... $ stripping before $$ ... $$ and iterates through $ ... $ removal step
# Uses experimental regex patterns tested in MathJax.Rmd

# Required package(s)
require(dplyr)
require(xml2)
require(stringr)

# Load data
data <- unz("data/PostsData.zip", filename = "posts.csv") %>% # Link to zipped file
  read.csv(header = TRUE, sep = ",", stringsAsFactors = FALSE) %>% # Read zipped file
  select(Id, Name, ParentId, Title, Body, # Select fields needed for joins
    CreationDate, Score, ViewCount, LastActivityDate, AnswerCount, FavoriteCount, Tags)  # Select metadata

data$DisplayLaTeX <- ""
data$InlineLaTeX <- ""

# Long regex pattern to remove \\begin{} \\end{}
pattern <- paste(
  "(\\\\begin(\\{align.*?\\})(\r|\n|.)*?\\\\end(\\{align.*?\\}))", # \\begin{align}
  "(\\\\begin(\\{array.*?\\})(\r|\n|.)*?\\\\end(\\{array.*?\\}))", # \\begin{array}
  "(\\\\begin(\\{case.*?\\})(\r|\n|.)*?\\\\end(\\{case.*?\\}))", # \\begin{case}
  "(\\\\begin(\\{eq.*?\\})(\r|\n|.)*?\\\\end(\\{eq.*?\\}))", # \\begin{eq.}
  "(\\\\begin(\\{gat.*?\\})(\r|\n|.)*?\\\\end(\\{gat.*?\\}))", # \\begin{gather}
  "(\\\\begin(\\{split.*?\\})(\r|\n|.)*?\\\\end(\\{split.*?\\}))", # \\begin{split}
  "\\\\\\[.*?\\\\\\]", # brackets
  sep = "|"
)
# Strip display LaTeX, round 1
data <- data %>% mutate(
  # Concatenate Title to left of Body
  CleanBody = paste(Title, Body, sep = " "),
  # Remove <code>
  CleanBody = CleanBody %>%
    gsub(pattern = "<code>.*?</code>", replacement = " ", x = .), # Strip <code> tags
  # Strip \\begin{} \\end{}
  DisplayLaTeX = CleanBody %>%
    str_extract_all(pattern) %>%
    lapply(paste, collapse = " ") %>%
    unlist(),
  CleanBody = CleanBody %>% str_replace_all(pattern, replacement = " ")
)

# Short regex pattern to remove $ ... $
pattern <- "(?<!(\\\\|\\$))\\$(?!\\$)(\r|\n|.)*?(?<!(\\\\|\\$))\\$"
# Recursively strip inline LaTeX
# Iterate to make sure I get them all
while ({str_detect(data$CleanBody, pattern) %>% sum()} > 0) {
  data <- data %>% mutate(
   # Collapse all found $ ... $ with a whitespace separator
   InlineLaTeX = paste(InlineLaTeX,
     CleanBody %>%
     str_extract(pattern),
     sep = " "),
   # Iterate through Body and remove the matches
   CleanBody = CleanBody %>%
     str_replace(pattern, replacement = " ")
  )
}

# Short regex pattern to remove $$ ... $$
pattern <- "(?<!\\\\)\\${2}(\r|\n|.)*?\\${2}" # $$ delimiter
# Strip display LaTeX, round 2
data <- data %>% mutate(
  # Collapse all found $$ ... $$ with a whitespace separator
  DisplayLaTeX = CleanBody %>%
    str_extract_all(pattern) %>%
    lapply(paste, collapse = " ") %>% # Collapse all found $$ blocks with a whitespace separator
    unlist(),
  # Iterate through each Body removing the identified LaTeX
  CleanBody = CleanBody %>% str_replace_all(pattern, replacement = " ")
)

data <- data %>% mutate(
  # Remove remaining HTML
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

# Add metadata
metadata <- merged %>% left_join(
  data %>% select(Id, Title, CreationDate, Score, ViewCount, LastActivityDate, AnswerCount, FavoriteCount, Tags),
  by = "Id"
) %>% mutate(
  CreationDate = ymd_hms(CreationDate),
  LastActivityDate = ymd_hms(LastActivityDate)
)

# Cleanup
remove(pattern, data, merged)