# Text Analysis Model

# Load necessary libraries
library(tidytext)
library(dplyr)
library(ggplot2)

# Define the text analysis function
text_analysis <- function(text_data) {
  # Tokenize the text data
  tokens <- text_data %>%
    unnest_tokens(word, text)
  
  # Calculate word frequency
  word_freq <- tokens %>%
    count(word, sort = TRUE)
  
  # Plot the most common words
  word_freq %>%
    top_n(10, n) %>%
    ggplot(aes(reorder(word, n), n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    ggtitle("Most Common Words")
}

# Example usage
# text_data <- data.frame(text = c("Sample text for analysis."))
# text_analysis(text_data)