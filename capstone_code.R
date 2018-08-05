# --------------------
# LIBRARIES
# --------------------

# Load libraries

library(tidyverse)
library(httr)
library(jsonlite)
library(xml2)
library(urltools)
library(lubridate)
library(magrittr)
library(tidytext)
library(tidyr)
library(wordcloud)
library(reshape2)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)
library(ldatuning)

# --------------------
# DATA WRANGLING
# --------------------

# Create Newsriver API

# Create function to retrieve data based on query paramters. See 'API REFERENCE' on newsriver.io for a list of query parameters. The function returns a tibble containing the title, text, date, and website relating to the query parameters.

newsriver_api <- function(query) {
  
  # Set rate limit and show progress. The API rate limit is 25 calls per window per API token. The rate limiting window is 15 minutes long.
  
  p$tick()$print()
  Sys.sleep(37)
  
  # Create URL encoded base to be passed custom query parameters
  
  url_base <- "https://api.newsriver.io/v2/search" %>% 
    param_set("query", url_encode(query)) %>% 
    param_set("sortBy", "_score") %>% 
    param_set("sortOrder", "DESC") %>% 
    param_set("limit", "100")
  
  # Make GET request
  
  resp <- GET(url_base, ua, add_headers(Authorization = api_token))
  
  # Return error and stop execution if a json is not returned
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  # Return error if there is a http error, else parse the content from the json file and store the title, text, date, and website in a tibble
  
  if (http_error(resp) == TRUE) {
    warning("The request failed")
  } else {
    news_tbl <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)  %>%
      as_tibble()
    
    if (nrow(news_tbl) != 0) {
      news_tbl <- news_tbl %>% mutate(date = as.Date(discoverDate), website = website.domainName) %>% select(title, text, date, website)
    }
    
    news_tbl
  }
}

# Set user agent and api token

ua <- user_agent("inser user agent here")
api_token <-  # "insert token here"
  
# Set date range for api search
  
search_dates <- seq(as.Date("2017-07-01"), as.Date("2018-07-01"), by = "months")

# Set query parameters to be called

query <- sprintf('title:("perfectionism" OR "perfect") AND text:"perfectionism" AND language:en AND discoverDate:[%s TO %s]', search_dates, search_dates %m+% months(1))

# Initialise progress bar

p <- progress_estimated(length(search_dates))

# Call newsriver_api function over the vector of query parameters and return a tibble

perf_news <- map_dfr(query, newsriver_api)

# Remove unicode characters from title and text

perf_news %<>% mutate(text = stringi::stri_trans_general(perf_news$text, "latin-ascii"), title = stringi::stri_trans_general(perf_news$title, "latin-ascii"))

# Convert titles to lower case to enable duplicate detection

perf_news %<>% mutate(title = str_to_lower(title))

# Remove duplicates

perf_news %<>% distinct(title, .keep_all = TRUE)

# Inspect each article and remove any errors or unrelated texts

perf_news <- perf_news %>%
  filter(!(perf_news$title == perf_news$title[52])) %>% 
  filter(!(perf_news$title == perf_news$title[3]))

# Unnest perf_news text column

tidy_news <- perf_news %>% unnest_tokens(word, text)

# Remove stop words

tidy_news <- tidy_news %>% anti_join(stop_words)

# --------------------
# DATA ANALYSIS
# --------------------

# Examine the distribution of words in the data set. First, calculate the term frequency of each term in each article, then divide by the total number of terms in that article. Finally, plot the results.

words_news <- perf_news %>% 
  unnest_tokens(word, text) %>% 
  count(title, word, sort = TRUE) %>% 
  ungroup()

words_total <- words_news %>% 
  group_by(title) %>% 
  summarise(total = sum(n))

words_news <- left_join(words_news, words_total)

ggplot(words_news, aes(n/total, fill = title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.025) +
  theme(strip.text = element_text(size = 7))

# To further examine the structure of the data, fit and plot a linear model to freq_by_rank to demonstrate Zipf's law has been maintained.

freq_by_rank <- words_news %>% 
  group_by(title) %>% 
  mutate(rank = row_number(), `term frequency` = n/total)

lm(log10(`term frequency`) ~ log10(rank), data = freq_by_rank)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = title)) +
  geom_abline(intercept = -1.1029,slope = -0.7564, color = "black", linetype = 2) +
  geom_line(size = 0.5, alpha = 0.5, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# Create custom stop words

custom_stop_words <-  bind_rows(tibble(word = c("perfect", 
                                                "perfection", 
                                                "perfectionism", 
                                                "perfectly", 
                                                "perfectionist", 
                                                "perfectionists", 
                                                "curran", 
                                                "thomas", 
                                                "andy", 
                                                "hill"), 
                                       lexicon = c("custom")), stop_words)

# Find and plot the most common words in tidy_news after applying custom stop words

# As a bar chart:

tidy_news %>%
  anti_join(custom_stop_words) %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 50) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Word Frequency (n)")

# As a word cloud:

tidy_news %>%
  anti_join(custom_stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100, colors= c("steelblue1","steelblue2","steelblue3","steelblue")))

# Create a new data set called tidier_news which is tokenized by word, but keeps track of sentence number

tidier_news <- perf_news %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% 
  group_by(title) %>% 
  mutate(sentence_number = row_number()) %>% 
  ungroup() %>%
  unnest_tokens(word, sentence) %>% 
  anti_join(custom_stop_words)

# Perform sentiment analyses using three different sentiment lexicons (AFINN, Bing, and NRC). Compute sentiment in sentence units by summing individual word sentiment scores across each sentence.

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

# Plot all three sentiment analyses on one graph

bind_rows(afinn %>% mutate(method = "AFINN"), bing %>% mutate(method = "Bing et al."), nrc %>% mutate(method = "NRC")) %>% 
  ggplot(aes(date, sent_sum, fill = method)) +
  geom_col(position = position_dodge(0.5), show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Sentiment Score")

# Find the most common positive and negative words

bing_word_counts <- tidier_news %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

# Plot the most common positive and negative words

# As a bar chart:

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

# As a word cloud:

tidier_news %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray8", "darkorange"), max.words = 100)

# Unnest tokens by bigrams keeping track of sentence number

bigram_news <- perf_news %>%
  unnest_tokens(sentence, text, token = "sentences") %>% 
  group_by(title) %>% 
  mutate(sentence_number = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(bigram, sentence, token = "ngrams", n = 2)

# Separate bigrams into two columns, "word1", and "word2".

bigrams_separated <- bigram_news %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

# Use bigrams to perform sentiment analyses by reversing the sentiment score of negated words

AFINN <- get_sentiments("afinn")

negation_words <- c("not", "never", "no", "without")

bigrams_afinn <- bigrams_separated %>% 
  filter(!word1 %in% custom_stop_words$word) %>% 
  filter(!word2 %in% custom_stop_words$word) %>% 
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

# Count most frequent trigrams not keeping track of sentence

perf_news %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>% 
  count(word1, word2, word3, sort = TRUE)


# Create igraph object for the most frequent bigrams. The transparency of links denotes how common or rare the bigram is

bigram_graph <- bigram_counts %>% 
  filter(n > 5) %>% 
  graph_from_data_frame()

set.seed(1234)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
 
# Calculate pairwise correlations of words using the phi coefficient. This identifies how much more likely it is that either both word X and Y appear, or neither do, than that one appears without the other.

word_cors <- perf_news %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% 
  group_by(title) %>% 
  mutate(sentence_number = row_number()) %>% 
  ungroup() %>%
  unnest_tokens(word, sentence) %>% 
  filter(!word %in% stop_words$word) %>% 
  group_by(word) %>% 
  filter(n() >= 20) %>% 
  pairwise_cor(word, title, sort = TRUE)

# Plot words most correlated with different perfectionism terms

word_cors %>%
  filter(item1 %in% c("perfect", "perfection", "perfectionism", "perfectly", "perfectionist", "perfectionists")) %>% 
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>% 
  group_by(item1, item2) %>%                  
  arrange(desc(correlation)) %>%                
  ungroup() %>%
  mutate(item2 = factor(paste(item2, item1, sep = "__"), levels = rev(paste(item2, item1, sep = "__")))) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab(NULL)

# Create Document Term Matrix

news_dtm <- tidy_news %>% 
  anti_join(custom_stop_words) %>%
  count(title, word) %>% 
  cast_dtm(title, word, n)

# Select number of topics (k) for LDA model using the 'ldatuninig' package.

lda_fit <-FindTopicsNumber(news_dtm,
                           topics = seq(from = 2, to = 50, by = 1),
                           metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                           method = "Gibbs", control = list(seed = 77), mc.cores = 2L, verbose = TRUE)

# find the extremum to determine optimal k

FindTopicsNumber_plot(lda_fit)

# Fit topic models using latent Dirichlet allocation

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
  group_by(topic, term) %>%                  
  arrange(desc(beta)) %>%                
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), levels = rev(paste(term, topic, sep = "__")))) %>%
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab(NULL)

# Extract the per-document-per-topic probabilities (gamma).

perf_documents <- tidy(perf_lda, matrix = "gamma")


# Find which words in each document were assigned to which topic.

assignments <- augment(perf_lda, data = news_dtm)