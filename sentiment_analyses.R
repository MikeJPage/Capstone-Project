library(tidyverse)
library(tidytext)
library(readr)
library(tidyr)
library(magrittr)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)
library(ldatuning)

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

custom_stop_words <-  bind_rows(tibble(word = c("perfect", "perfection", "perfectionism", "perfectly", "perfectionist", "perfectionists", "curran", "thomas", "andy", "hill"), lexicon = c("custom")), stop_words)

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

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

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

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# Word Clouds

library(wordcloud)

tidier_news %>%
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100, colors= c("steelblue1","steelblue2","steelblue3","steelblue")))

library(reshape2)

tidier_news %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray8", "darkorange"), max.words = 100)

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# tf-idf = ln(n documents /n documents containing term).

words_news <- perf_news %>% 
  unnest_tokens(word, text) %>% 
  count(title, word, sort = TRUE) %>% 
  ungroup()

words_total <- words_news %>% 
  group_by(title) %>% 
  summarise(total = sum(n))

words_news <- left_join(words_news, words_total)

# calculate tf (term frequency) for each article: the number of times a word appears in an aritcle divided by the total number of words (terms) in that article.

ggplot(words_news, aes(n/total, fill = title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.025) +
  facet_wrap(~title, nrow = 7, scales = "free_y", labeller = label_wrap_gen(width = 43)) +
  theme(strip.text = element_text(size = 7))

# Zipf's Law

freq_by_rank <- words_news %>% 
  group_by(title) %>% 
  mutate(rank = row_number(), `term frequency` = n/total)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = title)) +
  geom_line(size = 0.5, alpha = 0.5, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# Fit and plot a linear model to freq_by_rank to demonstrate Zipf's law has been maintained. A perfect inverse relationship will have a coefficient of -1.

lm(log10(`term frequency`) ~ log10(rank), data = freq_by_rank)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = title)) +
  geom_abline(intercept = -1.1027,slope = -0.7577, color = "black", linetype = 2) +
  geom_line(size = 0.5, alpha = 0.5, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# tf-idf

words_news <- words_news %>% 
  bind_tf_idf(word, title, n)

# Terms with high tf-idf

words_news %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

words_news %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(title) %>% 
  arrange(desc(n)) %>% 
  top_n(3) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~title, nrow = 7, scales = "free") +
  coord_flip()

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# Bigrams

# Unnest by bigrams keeping track of sentence

bigram_news <- perf_news %>%
  unnest_tokens(sentence, text, token = "sentences") %>% 
  group_by(title) %>% 
  mutate(sentence_number = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(bigram, sentence, token = "ngrams", n = 2)

# Count most frequent bigrams

bigram_news %>%
  count(bigram, sort = TRUE)

# Separate bigrams into two columns, "word1", and "word2".

bigrams_separated <- bigram_news %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# Unite word1 and word2 and recount

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united %>% 
  count(bigram, sort = TRUE)

# Count most common trigrams

perf_news %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>% 
  count(word1, word2, word3, sort = TRUE)

# Count tf_idf for bigrams

bigram_tf_idf <- bigrams_united %>% 
  count(title, bigram) %>% 
  bind_tf_idf(bigram, title, n) %>%
  arrange(desc(tf_idf))

# Using bigrams to perform sentiment analyses by reversing the sentiment score of negated words

AFINN <- get_sentiments("afinn")

negation_words <- c("not", "never", "no", "without")

bigrams_afinn <- bigrams_separated %>% 
  filter(!word1 %in% c("perfect", "perfection", "perfectionism", "perfectly", "perfectionist", "perfectionists")) %>% 
  filter(!word2 %in% c("perfect", "perfection", "perfectionism", "perfectly", "perfectionist", "perfectionists")) %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>%
  mutate(score = ifelse(word1 %in% negation_words, -score, score))
  
bigrams_afinn_sentiment <- bigrams_afinn %>%
  group_by(title, sentence_number) %>% 
  mutate(sentiment = sum(score)) %>%
  select(date, title, sentence_number, sentiment) %>% 
  distinct() %>% 
  group_by(title) %>% 
  mutate(sent_sum = sum(sentiment)) %>% 
  ungroup() %>% 
  select(date, title, sent_sum) %>% 
  distinct()

ggplot(bigrams_afinn_sentiment, aes(date, sent_sum, fill = date)) +
  geom_col(position = position_dodge(0.7), width = 2, show.legend = FALSE)

# Count most frequent bigrams not keeping track of sentence

bigram_counts <- perf_news %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE)

# Create igraph object for most frequent bigrams
# This represents a Markov Chain showing only the most common word-to-word connections

bigram_graph <- bigram_counts %>% 
  filter(n > 3) %>% 
  graph_from_data_frame()

# Plot igraph object using ggraph
# Transparency of links denotes how common or rare the bigram is

set.seed(1234)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# Correlating pairs of words (co-occur)

# Count words that appear within the same articles

word_pairs <- tidy_sentences %>%
  unnest_tokens(word, sentence) %>% 
  filter(!word %in% stop_words$word) %>% 
  pairwise_count(word, title, sort = TRUE) 

# Which words are most likely to co-occur with "perfectionism" across articles

word_pairs %>% 
  filter(item1 == "perfectionism")

# Pairwise correlation using phi coefficient which tells us hw much more likely it is that either both word X and Y appear, or neither do, than that one appears without the other.

word_cors <- tidy_sentences %>%
  unnest_tokens(word, sentence) %>% 
  filter(!word %in% stop_words$word) %>% 
  group_by(word) %>% 
  filter(n() >= 20) %>% 
  pairwise_cor(word, title, sort = TRUE)

# Find words most correlated with different perfectionism terms

word_cors %>% 
  filter(item1 %in% c("perfect", "perfection", "perfectionism", "perfectly", "perfectionist", "perfectionists")) %>% 
  print(n = 50)

word_cors %>%
  filter(item1 %in% c("perfect", "perfection", "perfectionism", "perfectly", "perfectionist", "perfectionists")) %>% 
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>% 
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# Create Document Term Matrix

news_dtm <- tidy_news %>% 
  anti_join(custom_stop_words) %>%
  anti_join(stop_words) %>% 
  count(title, word) %>% 
  cast_dtm(title, word, n)

# Select number of topics (k) for LDA model using the {ldatuninig} package. Vignette: https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

lda_fit <-  FindTopicsNumber(news_dtm, topics = seq(from = 2, to = 50, by = 1), metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), method = "Gibbs", control = list(seed = 77), mc.cores = 2L, verbose = TRUE)

# find the extremum to determine optimal k

FindTopicsNumber_plot(lda_fit)

# Topic Modelling - a method for unsupervised classification of documents which finds natural groups of items. Latent Dirichlet allocation (LDA) is one method for fitting topics. It treats each document as a mixture of topics, and each topic as a mixture of words. This allows documents to “overlap” each other in terms of content, rather than being separated into discrete groups, in a way that mirrors typical use of natural language.

perf_lda <- LDA(news_dtm, k = 9, control = list(seed = 1234))

# Extract the per-topic-per-word probabilities (beta).

perf_topics <- tidy(perf_lda, matrix = "beta")

# Find most common terms for each topic.

perf_top_terms <-  perf_topics %>% 
  group_by(topic) %>% 
  top_n(5, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

perf_top_terms %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Only works when k = 2. Greatest difference in β between topic 1 and topic 2. This can be estimated based on the log ratio of the two: log2(β2/β1)(a log ratio is useful because it makes the difference symmetrical: β2 being twice as large leads to a log ratio of 1, while β1 being twice as large results in -1). To constrain it to a set of especially relevant words, we can filter for relatively common words, such as those that have a β greater than 1/1000 in at least one topic.


# beta_spread <- perf_topics %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   spread(topic, beta) %>%
#   filter(topic1 > .001 | topic2 > .001) %>%
#   mutate(log_ratio = log2(topic2 / topic1))
# 
# beta_spread %>%
#   group_by(direction = log_ratio > 0) %>%
#   top_n(10, abs(log_ratio)) %>%
#   ungroup() %>%
#   mutate(term = reorder(term, log_ratio)) %>%
#   ggplot(aes(term, log_ratio)) +
#   geom_col() +
#   labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
#   coord_flip()

# Extract the per-document-per-topic probabilities (gamma).

perf_documents <- tidy(perf_lda, matrix = "gamma")


# Find which words in each document were assigned to which topic.

assignments <- augment(perf_lda, data = news_dtm)
