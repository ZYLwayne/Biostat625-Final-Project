#### One.Read Data ####
setwd("/Users/15792/Desktop/HW/625/Final Project") # Set Working Directory
data <- read.csv("INF002v4.csv", header = T) # Read data

## 1. Convert to numeric variables
clean_and_convert_to_numeric <- function(x) {
  x_cleaned <- gsub(",", "", x)  # Remove thousandths comma
  as.numeric(x_cleaned)  # Convert to numeric variables
}
data <- data %>% mutate(Total.Charges = clean_and_convert_to_numeric(Total.Charges),
                        Total.Costs = clean_and_convert_to_numeric(Total.Costs),
                        Length.of.Stay = as.numeric(Length.of.Stay))

# 2. Convert to categorical variable
data <- data %>% mutate(across(where(is.character), as.factor))

# 3. Variables Desciption table
data_str <- capture.output(str(data))  # Captures the output of str()

var_info <- lapply(data_str[-1], function(line) { # Parsing output to extract variable names and types
  parts <- strsplit(line, ":")  # split by colon
  name <- trimws(parts[[1]][1]) 
  type <- trimws(parts[[1]][2])
  list(name = name, type = type)
})
var_info_df <- do.call(rbind, lapply(var_info, as.data.frame))
var_info_df$type <- gsub("^(Factor w/ \\d+ levels).*", "\\1", var_info_df$type)  
var_info_df$type <- gsub("^(Factor w/ \\d+ level).*", "\\1", var_info_df$type)  
var_info_df$type <- gsub("^(num).*", "\\1", var_info_df$type)  
var_info_df$type <- gsub("^(int).*", "\\1", var_info_df$type)  
var_info_df$name <- gsub("^\\$\\s*", "", var_info_df$name)

var_info_df <- var_info_df %>% 
  mutate(Role = ifelse(name == "Length.of.Stay", "Target", "Feature"))

kbl(var_info_df, caption = "the Description of Variables", booktabs = T) %>% 
  kable_styling(latex_options = "striped", full_width = FALSE, font_size = 7)

#### Two.Exploratory Data Analysis ####

# 1. Data preprocessing
## 1.1 Missing data
data <- data %>% 
  mutate_all(~na_if(trimws(.), "")) %>% # Replace blank values with NA
  mutate(Ethnicity = na_if(Ethnicity, "Unknown")) %>% 
  mutate(Gender = na_if(Gender, "U")) %>% 
  mutate(Total.Charges = clean_and_convert_to_numeric(Total.Charges),
         Total.Costs = clean_and_convert_to_numeric(Total.Costs),
         Length.of.Stay = as.numeric(Length.of.Stay)) %>% 
  mutate(across(where(is.character), as.factor))

missing_values_df <- data.frame(
  Variable = names(sapply(data, function(x) sum(is.na(x)))),
  Missing_ratio = round(sapply(data, function(x) sum(is.na(x))) / nrow(data),5),
  Missing_cnt = sapply(data, function(x) sum(is.na(x)))
)
missing_values_df %>% 
  filter(Missing_ratio > 0) %>% 
  ggplot(aes(x = reorder(Variable, -Missing_ratio), y = Missing_ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = scales::percent(Missing_ratio)), hjust = 0.5, size = 3) +  
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "VariableS", y = "Proportion of Missing Values") +
  theme_minimal()
sum(is.na(data))
MissingDropVar <- c("Payment.Typology.3", "Payment.Typology.2") # Remove variables with more than 40% missing values
data <- data %>%  select(-one_of(MissingDropVar))

sum(is.na(data))