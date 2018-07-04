# Load libraries

library(tidyverse)
library(httr)
library(jsonlite)
library(xml2)
library(urltools)
library(lubridate)
library(magrittr)
library(tidytext)

# Create Newsriver API

# Create function to retrieve data based on query paramters. See 'API REFERENE' on newsriver.io for a list of query parameters. The function returns a tibble containing the title, text, date, and website relating to the query parameters.

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

ua <- user_agent("Student Project, email: m.page.j@gmail.com")
api_token <- "sBBqsGXiYgF0Db5OV5tAw5l64-zG5zztO6GH_jjfWC9vXDcSfvuih9joCDc_qgJVn2pHZrSf1gT2PUujH1YaQA"

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

# Save the perf_news tibble as an R object for later use and analysis

write_rds(perf_news, "perf_news.RDS")

# Unnest perf_news text column

tidy_news <- perf_news %>% unnest_tokens(word, text)

# Remove stop words

tidy_news <- tidy_news %>% anti_join(stop_words)

# Save the tidy_news tibble as an R object for later use

write_rds(tidy_news, "tidy_news.RDS")