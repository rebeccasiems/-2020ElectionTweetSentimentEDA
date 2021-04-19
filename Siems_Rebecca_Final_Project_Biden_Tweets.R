library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)

# Read in Biden tweets dataset
hashtag_joebiden <- read_csv("archive/hashtag_joebiden.csv")
View(hashtag_joebiden)

# Condense the Biden dataset
hashtag_joebiden <- hashtag_joebiden %>%
  select(created_at,tweet, likes, retweet_count, user_name, user_screen_name, user_description, user_followers_count, city, country, state_code) %>%
  # Remove tweets from users outside the US
  filter(country == "United States")

# bing categorizes words into a binary of negative or positive words
get_sentiments("bing")

# afinn assigns words a score from -5 to 5 indicating how positive or negative they are
get_sentiments("afinn")



########## Work on Biden dataframe

# List words in tweets so the it's easier to get the sentiments of each tweet
biden_main <- hashtag_joebiden %>%
  mutate(
    # Removes whitespace and punctuation
    biden_words = str_replace_all(tweet, "[^[A-Za-z,]]", " ") %>%
      str_squish(.) %>%
      str_replace_all(.,",",",") %>%
      str_split(boundary("word")), 
    # Adds a tweet id so I can know which words are associated with each tweet
    tweet_id = row_number()
  ) %>% 
  # Takes them out of the list
  unnest(biden_words) 

biden_main <- biden_main %>%
  inner_join(bing_sent, by = c("biden_words" = "word")) 

write_csv(biden_main, "biden_bing.csv")

# Counts the number of positive and negative words for each 
biden_main %>% 
  count(tweet_id, sentiment)

# Calculates and graphs the time percent positive and negative words out of all the tweets over time
biden_main %>% 
  group_by(created_at, tweet_id) %>% 
  count(sentiment) %>% 
  mutate(perc = (n / nrow(biden_main) * 100)) %>% 
  ggplot(mapping = aes(x = created_at, y = perc)) +
  geom_line() +
  facet_wrap(~sentiment) +
  ggtitle("Percentage of Biden Sentiments Over Time") +
  labs(x = "Date", y = "Percentage")

# Finds the total number of words in each tweet to use in my calculation of the percent of each sentiment in each tweet
biden_num_words <- hashtag_joebiden %>%
  mutate(
    biden_words = str_replace_all(tweet, "[^[A-Za-z,]]", " ") %>%
      str_squish(.) %>%
      str_replace_all(.,",",",") %>%
      str_split(boundary("word")), 
    tweet_id = row_number()
  ) %>% 
  unnest(biden_words) %>% 
  count(tweet_id)

# Calculating the percentage of each sentiment in each tweet over time BUT WHY ARE MY BIDEN GRAPHS ALL WORSE THAN MY TRUMP GRAPHS
biden_main %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  group_by(created_at, tweet_id) %>% 
  count(sentiment) %>% 
  inner_join(biden_num_words, by = "tweet_id", "tweet_id") %>% 
  mutate(perc = ((n.x / n.y) * 100)) %>% 
  group_by(date(created_at), sentiment) %>% 
  summarise(perc = mean(perc)) %>% 
  ggplot(aes(x = `date(created_at)`, y = perc), color = sentiment) +
  geom_line() +
  facet_wrap(~sentiment) +
  ggtitle("Percentage of Biden Sentiment Words Per Tweet Over Time") +
  labs(x = "Date", y = "Percentage")

## Question: Which sentiments have the most likes and retweets? 
# This is to see which kinds of tweets are receiving the most likes and retweets and getting the most outreach

# Counts the number of positive or negative words for the tweets with the most retweets
biden_main %>%
  group_by(tweet_id) %>% 
  count(retweet_count, sentiment) %>% 
  # I arranged the tibble descending by retweets to see the tweets with the most retweets
  arrange(desc(retweet_count)) 

# Graph of the number of retweets with positive or negative words
ggplot(biden_main, aes(x = sentiment, y = retweet_count)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Retweets For All Biden Words") +
  labs(x = "Sentiment", y = "Number of Retweets")

# Graph of the number of retweets each tweet got with the color being the sentiment of the tweet  
ggplot(biden_main, aes(x = tweet_id, y = retweet_count, color = sentiment)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Retweets Per Biden Tweet By Sentiment") +
  labs(x = "Individual Tweet", y = "Number of Retweets")

# Counts the number of positive or negative words for the tweets with the most likes
biden_main %>% 
  group_by(tweet_id) %>% 
  count(likes, sentiment) %>% 
  arrange(desc(likes))

# Graph of the number of likes of tweets with positive or negative words
ggplot(biden_main, aes(x = sentiment, y = likes)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Likes for All Biden Words") +
  labs(x = "Sentiment", y = "Number of Likes")

# Graph of the number of likes each tweet got with the color being the sentiment of the tweet
ggplot(biden_main, aes(x = tweet_id, y = likes, color = sentiment)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Likes Per Biden Tweet By Sentiment") +
  labs(x = "Individual Tweet", y = "Number of Likes")

## Ratio of positive to negative words in Biden tweets

# Find the ratio of negative to positive words and then graph the distribution
biden_main %>% 
  group_by(tweet_id) %>% 
  count(sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  mutate(ratio = negative / positive) %>% 
  mutate(negative = replace_na(negative, 0)) %>% 
  mutate(positive = replace_na(positive, 0)) %>% 
  mutate(ratio = replace_na(ratio, 0)) %>% 
  ggplot(aes(x = ratio), stat_bin(drop = FALSE), na.rm = FALSE) +
  geom_histogram() +
  ggtitle("Number of Biden Tweets with Different Negative/Positive Sentiment Ratios") +
  labs(x = "Negative/Positve Sentiment Ratios", y = "Number of Tweets")

### Afinn Data

biden_main <- hashtag_joebiden %>%
  mutate(
    biden_words = str_replace_all(tweet, "[^[A-Za-z,]]", " ") %>%
      str_squish(.) %>%
      str_replace_all(.,",",",") %>%
      str_split(boundary("word")), 
    tweet_id = row_number()
  ) %>% 
  unnest(biden_words)

afinn_sent <- get_sentiments("afinn")

biden_main <- biden_main %>% 
  inner_join(afinn_sent, by = c("biden_words" = "word")) 

write_csv(biden_main, "biden_afinn.csv")

#### BUT WAIT, THERE'S MORE

# Graph average sentiment value over time
biden_main %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  group_by(date(created_at)) %>% 
  summarise(value = mean(value)) %>%
  ggplot(aes(x = `date(created_at)`, y = value)) +
  geom_line() +
  ggtitle("Average Sentiment Value of Biden Tweets Over Time") +
  labs(x = "Date", "Sentiment Value")


# Regression

summary(
  trump_main %>% 
    group_by(tweet_id) %>% 
    mutate(value = mean(value)) %>% 
    lm(formula = likes ~ value + user_followers_count + retweet_count))

summary(
  biden_main %>% 
    group_by(tweet_id) %>% 
    mutate(value = mean(value)) %>% 
    lm(formula = likes ~ value + user_followers_count + retweet_count))

summary(
  biden_main %>% 
    group_by(tweet_id) %>% 
    mutate(value = mean(value)) %>% 
    lm(formula = retweet_count ~ value + user_followers_count + likes))

