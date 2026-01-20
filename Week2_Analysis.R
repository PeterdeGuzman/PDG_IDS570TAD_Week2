# IDS 570 Text as Data
# Week 02: Basics Assignment
# Author: Peter de Guzman (ped19)

##### Packages -----------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)

#### Load Data -----------------------------------------------------------------
# You will need the correct file paths if you don't follow my naming conventions:
file_a <- "texts/A07594__Circle_of_Commerce.txt"
file_b <- "texts/B14801__Free_Trade.txt"

# Read the raw text files into R
text_a <- read_file(file_a)
text_b <- read_file(file_b)

# Combine into a tibble for tidytext workflows
texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)

texts

# Analysis --------------------------------------------------------------------

# Tokenization, stopwords, and counting words

# Start with tidytext's built-in stopword list
data("stop_words")

# Add our own project-specific stopwords (you can, and will, expand this list later)
custom_stopwords <- tibble(
  word = c(
    "vnto", "haue", "doo", "hath", "bee", "ye", "thee"
  )
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

all_stopwords %>% slice(1:10)

#creating diagnostics table
#cols are doc_title, n_chars, n_word_tokens, n_word_types

corpus_diagnostics <- texts %>%
  mutate(n_chars = str_length(text)) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  group_by(doc_title) %>%
  summarise(
    n_chars = unique(n_chars),
    n_word_tokens = n(),
    n_word_types = n_distinct(word)
  ) %>%
  ungroup()

corpus_diagnostics


#calculating word counts

word_counts <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE) 

# Compare normalized "trade" across the texts

doc_lengths <- word_counts %>%
  group_by(doc_title) %>%
  summarise(total_words = sum(n)) 

word_counts_normalized <- word_counts %>%
  left_join(doc_lengths, by = "doc_title") %>%
  mutate(relative_freq = n / total_words)

trade_counts_normalized <- word_counts_normalized %>%
  filter(word == "trade")




# Comparing word frequencies across texts

plot_n_words <- 20  # you can change this as needed

# Select the most frequent words overall
word_comparison_tbl <- word_counts %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

#plotting relative frequencies 
top20_words <- word_comparison_tbl %>% 
  slice_head(n = plot_n_words) %>%
  select(word)

#filter data
normalized_word_plot_data <- word_counts_normalized %>%
  semi_join(top20_words, by ="word") %>%
  mutate(word = fct_reorder(word, relative_freq, .fun=max))

plot <- ggplot(normalized_word_plot_data, aes(x = relative_freq, y = word)) + 
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  scale_x_continuous(limits = c(0, 0.04)) +
  labs(
    title = "Most frequent words (after normalization)",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by relative frequency across both texts"
    ),
    x = "Relative word frequency",
    y = NULL
  ) +
  theme_minimal()

plot

ggsave(
  filename = file.path("Outputs", "normalized_word_plot.png"),
  plot = plot,
  width = 8,
  height = 6,
  dpi = 300
)

# original plot from example code

word_plot_data <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "n"
  ) %>%
  mutate(word = fct_reorder(word, n, .fun = max))

ggplot(word_plot_data, aes(x = n, y = word)) + #black magic happens thanks to ggplot
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words (stopwords removed)",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by maximum frequency across both texts"
    ),
    x = "Word frequency",
    y = NULL
  ) +
  theme_minimal()

# Bigrams: starting to think about context

bigrams <- texts %>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

#separate to remove stopwords
bigrams_separated <- bigrams %>%
  separate(bigram, into=c("word1", "word2"), sep=" ")

#remove all stopwords
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% all_stopwords$word,
         !word2 %in% all_stopwords$word)

bigram_counts <- bigrams_filtered %>% 
  count(doc_title, word1, word2, sort = TRUE)

#unite to put the bigrams back together
bigram_counts <- bigram_counts %>%
  unite(bigram, word1, word2, sep = " ")

# Comparing bigrams

bigram_relative <- bigram_counts %>% 
  group_by(doc_title) %>%
  mutate(
    total_bigrams = sum(n),
    proportion = n / total_bigrams) %>%
  ungroup()

bigram_wide <- bigram_relative %>%
  select(doc_title, bigram, proportion) %>%
  pivot_wider(
    names_from = doc_title, 
    values_from = proportion,
    values_fill = 0
  )

#calculate differences

bigram_diff <- bigram_wide %>%
  mutate(
    diff = `Text A` - `Text B` 
  ) %>%
  arrange(desc(abs(diff)))

bigram_diff %>% slice(1:20)



