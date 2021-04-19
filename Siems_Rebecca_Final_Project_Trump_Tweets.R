library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)

# Read in Trump tweets dataset
hashtag_donaldtrump <- read_csv("archive/hashtag_donaldtrump.csv")
View(hashtag_donaldtrump)

# Read in Biden tweets dataset
hashtag_joebiden <- read_csv("archive/hashtag_joebiden.csv")
View(hashtag_joebiden)

# Condense the Trump dataset 
hashtag_donaldtrump <- hashtag_donaldtrump %>%
  select(created_at, tweet, likes, retweet_count, user_name, user_screen_name, user_description, user_followers_count, city, country, state_code) %>%
  # Remove tweets from users outside the US
  filter(country == "United States") 
# I think the created_at variable shows the dates and times already and I don't know if I have to do anything with that column


# Load the sentiment lexicons to see what I'm working with here

# afinn assigns words a score from -5 to 5 indicating how positive or negative they are
get_sentiments("afinn")

# bing categorizes words into a binary of negative or positive words
get_sentiments("bing")


########## Work on Trump dataframe

trump_main <- hashtag_donaldtrump %>%
  # Will remove soon
  # slice_sample(n = 1000) %>%
  mutate(
    trump_words = str_replace_all(tweet, "[^[A-Za-z,]]", " ") %>%
      str_squish(.) %>%
      str_replace_all(.,",",",") %>%
      str_split(boundary("word")), 
    tweet_id = row_number()
  ) %>% 
  unnest(trump_words) 
  

bing_sent <- get_sentiments("bing") 

trump_main <- trump_main %>%
  inner_join(bing_sent, by = c("trump_words" = "word")) 

write_csv(trump_main, "trump_bing.csv")

#counts the sentiments of each tweet
trump_main %>%
  count(tweet_id, sentiment) 

# Counts the number of sentiment words
trump_main %>% 
  count(sent)

# Visualization of the sentiments
ggplot(trump_main, aes(x = sentiment)) +
  geom_bar() +
  ggtitle("Number of Positive and Negative Words in Trump Tweets") +
  labs(x = "Sentiment", y = "Number of Words")

## Question: Which sentiments have the most likes and retweets? 
# This is to see which kinds of tweets are receiving the most likes and retweets and getting the most outreach

# Counts the number of positive or negative words for the tweets with the most retweets
trump_main %>%
  group_by(tweet_id) %>% 
  count(retweet_count, sentiment) %>% 
  # I arranged the tibble descending by retweets to see the tweets with the most retweets
  arrange(desc(retweet_count)) 

# Graph of the number of retweets with positive or negative words
ggplot(trump_main, aes(x = sentiment, y = retweet_count)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Retweets For All Trump Words") +
  labs(x = "Sentiment", y = "Number of Retweets")

# Graph of the number of retweets each tweet got with the color being the sentiment of the tweet  
ggplot(trump_main, aes(x = tweet_id, y = retweet_count, color = sentiment)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Retweets Per Trump Tweet By Sentiment") +
  labs(x = "Individual Tweet", y = "Number of Retweets")

# Counts the number of positive or negative words for the tweets with the most likes
trump_main %>% 
  group_by(tweet_id) %>% 
  count(likes, sentiment) %>% 
  arrange(desc(likes))

# Graph of the number of likes of tweets with positive or negative words
ggplot(trump_main, aes(x = sentiment, y = likes)) +
  geom_bar(stat = "identity") +
  ggtitle("Nummber of Likes for All Trump Words") +
  labs(x = "Sentiment", y = "Number of Likes")

# Graph of the number of likes each tweet got with the color being the sentiment of the tweet
ggplot(trump_main, aes(x = tweet_id, y = likes, color = sentiment)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Likes Per Trump Tweet By Sentiment") +
  labs(x = "Individual Tweet", y = "Number of Likes")


# Question: How did the percentage of positive and negative words in each tweet change over the course of the election month?
# This graph gives the percentage of positive and negative words per tweet out of all the sentiment words in all the tweets
trump_main %>% 
  group_by(created_at, tweet_id) %>% 
  count(sentiment) %>% 
  mutate(perc = (n / nrow(trump_main) * 100)) %>% 
  ggplot(mapping = aes(x = created_at, y = perc)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~sentiment) +
  ggtitle("Percentage of Trump Sentiments Over Time") +
  labs(x = "Date", y = "Percentage")

# Finds the number of words per tweet (raw tweets that haven't been filtered by bing) 
trump_num_words <- hashtag_donaldtrump %>%
  # Will remove soon
  # slice_sample(n = 1000) %>%
  mutate(
    trump_words = str_replace_all(tweet, "[^[A-Za-z,]]", " ") %>%
      str_squish(.) %>%
      str_replace_all(.,",",",") %>%
      str_split(boundary("word")), 
    tweet_id = row_number()
  ) %>% 
  unnest(trump_words) %>% 
  count(tweet_id)

# I joined the number of total words per tweet with the sentiment words tibble so I could figure out the percentage of sentiment words per tweet
trump_main %>% 
  group_by(created_at, tweet_id) %>% 
  count(sentiment) %>% 
  inner_join(trump_num_words, by = "tweet_id", "tweet_id") %>% 
  mutate(perc = ((n.x / n.y) * 100)) %>% 
  ggplot(aes(x = created_at, y = perc)) +
  geom_line() +
  facet_wrap(~sentiment) +
  ggtitle("Percentage of Trump Sentiment Words Per Tweet Over Time") +
  labs(x = "Date", y = "Percentage")

# Calculate ratio of negative to positive words per tweet
# The 0 column includes tweets that only use negative words (no positive words found)

trump_main %>% 
  group_by(tweet_id) %>% 
  count(sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  mutate(ratio = negative / positive) %>% 
  mutate(negative = replace_na(negative, 0)) %>% 
  mutate(positive = replace_na(positive, 0)) %>% 
  mutate(ratio = replace_na(ratio, 0)) %>% 
  ggplot(aes(x = ratio), stat_bin(drop = FALSE), na.rm = FALSE) +
  geom_histogram() +
  ggtitle("Number of Trump Tweets with Different Negative/Positive Sentiment Ratios") +
  labs(x = "Negative/Positve Sentiment Ratios", y = "Number of Tweets")



### afinn data

trump_main <- hashtag_donaldtrump %>%
  # Will remove soon
  # slice_sample(n = 1000) %>%
  mutate(
    trump_words = str_replace_all(tweet, "[^[A-Za-z,]]", " ") %>%
      str_squish(.) %>%
      str_replace_all(.,",",",") %>%
      str_split(boundary("word")), 
    tweet_id = row_number()
  ) %>% 
  unnest(trump_words)

afinn_sent <- get_sentiments("afinn")

trump_main <- trump_main %>% 
  inner_join(afinn_sent, by = c("trump_words" = "word")) 

write_csv(trump_main, "trump_afinn.csv")

# Counts the number of words with each sentiment value
trump_main %>% 
  count(value)

# Graph the number of words with each sentiment value
trump_main %>% 
  count(value) %>% 
  ggplot(aes(x = value, y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Words With Each Sentiment Value") +
  labs(x = "Sentiment Value", y = "Number of Words")

# Graph the number of words with each value per tweet
trump_main %>% 
  group_by(tweet_id) %>% 
  count(trump_words, value)  

# Finding the descriptive statistics of the sentiment values for all the words
trump_main %>% 
  group_by(tweet_id) %>% 
  summarise(mean = mean(value), median = median(value), sd = sd(value), n = n())

# I think it would be cool to count/graph the number of tweets with average sentiment values above and below 0 to see what that ratio is

# Graphs the average sentiment of each tweet over time
trump_main %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  group_by(date(created_at)) %>% 
  summarise(value = mean(value)) %>%
  ggplot(aes(x = `date(created_at)`, y = value)) +
  geom_line() +
  ggtitle("Average Sentiment Value of Trump Tweets Over Time") +
  labs(x = "Date", "Sentiment Value")

# Finds the average sentiment value of the tweets with the most likes
trump_main %>% 
  group_by(tweet_id) %>% 
  select(likes, value) %>% 
  mutate(value = mean(value)) %>% 
  arrange(desc(likes))

# Model to do a regression? between sentiment value and likes
ggplot(reg1, aes(x = value.x, y = likes)) +
  geom_jitter() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Relationship Between the Sentiment Value of Words in Trump Tweets and Number of Likes") +
  labs(x = "Sentiment Value", y = "Number of Likes")

# Graphs relationship between average sentiment value per tweet and number of likes
trump_main %>% 
  group_by(tweet_id) %>% 
  mutate(value = mean(value)) %>% 
  ggplot(aes(x = value, y = likes)) +
  geom_jitter() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Relationship Between the Sentiment Value of Words in Trump Tweets and Number of Likes") +
  labs(x = "Sentiment Value", y = "Number of Likes")



# Summary of the relationship I just graphed
summary(
  trump_main %>% 
    group_by(tweet_id) %>% 
    mutate(value = mean(value)) %>% 
    lm(formula = likes ~ value + user_followers_count + retweet_count))

# Also want to do retweet count as DV
summary(trump_main %>% 
  group_by(tweet_id) %>% 
  mutate(value = mean(value)) %>% 
  lm(formula = retweet_count ~ value + user_followers_count + likes))

trump_main %>% 
  group_by(tweet_id) %>% 
  mutate(value = mean(value)) %>% 
  ggplot(aes(x = value, y = retweet_count)) +
  geom_jitter() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Relationship Between the Sentiment Value of Trump Tweets and Number of Retweets") %>% 
  labs(x = "Sentiment Value", y = "Number of Retweets")

# Could filter out tweets that actually have likes and then do another regression

trump_main %>% 
  group_by(tweet_id) %>% 
  filter(likes > 0) %>% 
  mutate(value = mean(value)) %>% 
  lm(formula = likes ~ value + user_followers_count)

summary(trump_main %>% 
  group_by(tweet_id) %>% 
  filter(likes > 0) %>% 
  mutate(value = mean(value)) %>% 
  lm(formula = likes ~ value + user_followers_count + retweet_count))

trump_main %>% 
  group_by(tweet_id) %>% 
  filter(likes > 0) %>% 
  mutate(value = mean(value)) %>% 
  ggplot(aes(x = value, y = likes)) +
  geom_jitter() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Relationship Between Sentiment Value of Trump Tweets with Likes and Number of Likes") +
  labs(x = "Sentiment Value", "Number of Likes")


# With retweets
trump_main %>% 
  group_by(tweet_id) %>% 
  filter(retweet_count > 0) %>% 
  mutate(value = mean(value)) %>% 
  lm(formula = retweet_count ~ value + user_followers_count + likes)

summary(trump_main %>% 
          group_by(tweet_id) %>% 
          filter(retweet_count > 0) %>% 
          mutate(value = mean(value)) %>% 
          lm(formula = retweet_count ~ value + user_followers_count + likes))

trump_main %>% 
  group_by(tweet_id) %>% 
  filter(retweet_count > 0) %>% 
  mutate(value = mean(value)) %>% 
  ggplot(aes(x = value, y = retweet_count)) +
  geom_jitter() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Relationship Between Sentiment Value of Trump Tweets with Retweets and Number of Retweets") +
  labs(x = "Sentiment Value", "Number of Retweets")


# Could mutate the value to include absolute value

# With likes
trump_main %>% 
  group_by(tweet_id) %>% 
  mutate(value = abs(value)) %>% 
  lm(formula = likes ~ value + user_followers_count + retweet_count)

summary(trump_main %>% 
  group_by(tweet_id) %>% 
  mutate(value = abs(value)) %>% 
  lm(formula = likes ~ value + user_followers_count + retweet_count)) 

trump_main %>% 
  group_by(tweet_id) %>% 
  mutate(value = abs(value)) %>% 
  ggplot(aes(x = value, y = likes)) +
  geom_jitter() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Relationship Between the Absolute Value of Sentiment Value of Words in Trump Tweets and Number of Likes") +
  labs(x = "Sentiment Value", y = "Number of Likes")
  
# With retweets
trump_main %>% 
  group_by(tweet_id) %>% 
  mutate(value = abs(value)) %>% 
  lm(formula = retweet_count ~ value + user_followers_count + likes)

summary(trump_main %>% 
          group_by(tweet_id) %>% 
          mutate(value = abs(value)) %>% 
          lm(formula = retweet_count ~ value + user_followers_count + likes)) 

trump_main %>% 
  group_by(tweet_id) %>% 
  mutate(value = abs(value)) %>% 
  ggplot(aes(x = value, y = retweet_count)) +
  geom_jitter() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Relationship Between the Absolute Value of Sentiment Value of Words in Trump Tweets and Number of Retweets") +
  labs(x = "Sentiment Value", y = "Number of Retweets")







