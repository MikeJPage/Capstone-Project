library(tidyverse)
library(tidytext)
library(readr)
library(tidyr)

# Load Datasets

perf_news <- read_rds("/Users/mikepage/Documents/Data Science/Springboard/Capstone_Project/Data_Wrangling/perf_news.RDS")
tidy_news <- read_rds("/Users/mikepage/Documents/Data Science/Springboard/Capstone_Project/Data_Wrangling/tidy_news.RDS")

# Find and plot the most common words in tidy_news (pg.6)

tidy_news %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 50) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Create custom stop words

custom_stop_words <-  bind_rows(tibble(word = c("perfect", "perfection", "perfectionism", "perfectly"), lexicon = c("custom")), stop_words)

# Sentiment analysis using unigrams. Be aware that there are two main limitations at this point: (1) the sentiment lexicons used may not be appropriate for this area of research; (2) the unigrams do not take into account sarcasm of negated text (e.g., no good, not true). Compare three different sentiment lexicons, dividing text by setence length (as opposed to calculating sentiment by summing individual word scores). Perhaps positive and negative sentiments for each article should be plotted side by side to show how each article is divided.

tidy_sentences <- perf_news %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% 
  group_by(title) %>% 
  mutate(sentence_number = row_number()) %>% 
  ungroup()

tidier_news <- tidy_sentences %>%
  unnest_tokens(word, sentence) %>% 
  anti_join(custom_stop_words)


# AFINN

afinn <- tidier_news %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(title, sentence_number) %>% 
  mutate(sentiment = sum(score)) %>%
  select(date, title, sentence_number, sentiment) %>% 
  distinct() %>% 
  group_by(title) %>% 
  mutate(sent_sum = sum(sentiment)) %>% 
  ungroup() %>% 
  select(date, title, sent_sum) %>% 
  distinct()

ggplot(afinn, aes(date, sent_sum, fill = date)) +
  geom_col(position = position_dodge(0.7), width = 2, show.legend = FALSE)

# Bing

bing <- tidier_news %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(date, title, sentence_number, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  group_by(title) %>% 
  mutate(sent_sum = sum(sentiment)) %>% 
  ungroup() %>% 
  select(date, title, sent_sum) %>% 
  distinct()

ggplot(bing, aes(date, sent_sum, fill = date)) +
  geom_col(position = position_dodge(0.5), width = 2, show.legend = FALSE)

# NRC

nrc <- tidier_news %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("positive", "negative")) %>%
  count(date, title, sentence_number, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  group_by(title) %>% 
  mutate(sent_sum = sum(sentiment)) %>% 
  ungroup() %>% 
  select(date, title, sent_sum) %>% 
  distinct()
  
ggplot(nrc, aes(date, sent_sum, fill = date)) +
  geom_col(position = position_dodge(0.5), width = 2, show.legend = FALSE)

# Plot all three sentiment analyses on one graph

bind_rows(afinn %>% mutate(method = "AFINN"), bing %>% mutate(method = "Bing et al."), nrc %>% mutate(method = "NRC")) %>% 
  ggplot(aes(date, sent_sum, fill = method)) +
  geom_col(position = position_dodge(0.5), show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# ---------------------------

# Find the most common positive and negative words

bing_word_counts <- tidier_news %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "contribution to sentiment", x = NULL) +
  coord_flip()

