library(datetime)
library(foreign)
library(stats)
library(jsonlite)
library(readxl)
library(rtweet)
library(stringr)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(tictoc) # Track runtime for R functions
library(lubridate) # Dealing with system time


setwd("")

# Set up Twitter API codes
twitter_token <- create_token(
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "")

#--------------------------------------------------------------------------------------------------------
# Twitter API rate limits (reset every 15 min)
#   Calls for friends/followers: 15
#   Calls for user timelines: 900
rate_limit <- rate_limit() # Get all rate limit info as a dataframe

rate_limit("get_timelines")
rate_limit("get_followers")
rate_limit("get_friends")

#--------------------------------------------------------------------------------------------------------


# Get all tweets from a user
twitter_user <- "AdamJKucharski" # Select user
tw <- get_timeline(twitter_user)
write_as_csv(tw, paste0(twitter_user,".csv"))

# Get user profile
twitter_user <- "@NateSilver538" # Select user
profile <- lookup_users(twitter_user)

# Function to show select features of profiles
simple_profiles <- function(profiles) {
  profile_variables <- c(
    "screen_name",
    "name",
    "location",
    "description",
    "followers_count",
    "friends_count",
    "statuses_count",
    "account_created_at"
  )
  profiles <- profiles %>% select(all_of(profile_variables))
  return(profiles)
}

#--------------------------------------------------------------------------------------------------------

write_as_csv(profile, paste0("profile_",twitter_user,".csv"))

list <- lists_members(list_id="1250047523756048390")
write_as_csv(list, "twitter_list.csv")

#--------------------------------------------------------------------------------------------------------
user_list <- c("@in_pubs", "@d1Vinman", "@LoraBLawrence")
follower_list <- sapply(user_list, get_followers)
follower_ids <- data.frame(id=matrix(unlist(follower_list)),stringsAsFactors=FALSE)
follower_ids <- unique(follower_ids)
follower_profiles <- get_profiles(follower_ids$id)


#--------------------------------------------------------------------------------------------------------
# Create a dataframe of profiles for followers of a seed user

seed_user <- "trvrb" # Select user
followers <- get_followers(seed_user) # Max 5,000 followers
friends <- get_friends(seed_user) # Max 5,000 friends (friends are users that the seed user follows)

# Function to get a set of profiles given a set of user IDs
get_profiles <- function(ids) {
  profiles <- lookup_users(ids)
  profiles <- simple_profiles(profiles)
  return(profiles)
}

follower_profiles <- get_profiles(followers$user_id) # Get follower profiles
write_as_csv(follower_profiles, paste0("./data/",seed_user,"_followers.csv")) # Save follower profiles as a CSV file named with seed_user

friend_profiles <- get_profiles(friends$user_id) # Get friend profiles
write_as_csv(friend_profiles, paste0("./data/",seed_user,"_friends.csv")) # Save friend profiles as a CSV file named with seed_user

#--------------------------------------------------------------------------------------------------------
friend_profiles <- friend_profiles %>%
  arrange(desc(followers_count))

profiles <- friend_profiles

#--------------------------------------------------------------------------------------------------------

# Extract text from profile descriptions
text_df <- tibble(text=profiles$description) # Extract profile descriptions and turn it into a tibble
text_df <- text_df %>%
  unnest_tokens(word, text, token = "tweets") %>% # Break out each word into its own row
  anti_join(stop_words) # Remove "the" "and" etc.

# Create word cloud
windows()
text_df %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

# Get word counts
text_wordcounts <- text_df %>%
  count(word, sort=TRUE)

text_wordcounts <- text_wordcounts %>%
  filter(n>100) %>% # Limit word count analysis to words that appear at least n times
  mutate(word=reorder(word, n))

# Create barchart with wordcounts
windows()
ggplot(data=text_wordcounts, aes(word, n)) +
  geom_col(fill="blue") +
  coord_flip() +
  labs(x ="Word", y="Count") +
  geom_text(aes(label=n), hjust=1.2, color="white")

#--------------------------------------------------------------------------------------------------------
# Find friends who match keywords, then create a CSV file of tweets for each friend

# List of keywords to check for
keywords_health <- c("health", "infect", "disease", "epi", "model", "medic", "bio", "trop", "ecol", "evol", "virol")
keywords <- keywords_health # Set of keywords to detect in profiles
pattern = paste(keywords, collapse="|") # Concatenate keywords into a single string
profiles$match <- grepl(pattern, profiles$description, ignore.case=TRUE) # For each profile, show whether there was a match to any keywords TRUE/FALSE

# Create a df of just Twitter profiles that match keywords, sort by number of followers
ids <- profiles %>%
  filter(match==TRUE) %>%
  arrange(desc(followers_count))

# Check and remove Twitter timelines that have already been downloaded
filenames <- list.files(path="./data/timelines/") # Get all file names in folder
filenames <- tibble(screen_name = gsub(".csv","",filenames)) # Remove ".csv" suffix and save as tibble
ids <- ids %>%
  anti_join(filenames) # Remove already downloaded Twitter accounts from total list of IDs

# Get the top n Twitter accounts by number of followers
n <- 100
ids_top_n <- ids %>%
  top_n(n, followers_count)

# Function: for a given Twitter account ID, create a CSV file with max number of their tweets
timeline_csv <- function(id) {
  tic(id) # Track runtime for function
  timeline <- get_timeline(id, n=3200)
  timeline <- plain_tweets(timeline) # Clean up text in tweets (get rid of strange formatting)
  timeline <- timeline[,1:62] # Drop all extra data about the original user (rtweet adds user profile info to every tweet row)
  write_as_csv(timeline, paste0("./data/timelines/",id,".csv"))
  toc() # Track runtime for function
}

# Create CSV tweet files for all Twitter accounts in ids vector
ids <- ids_top_n$screen_name
lapply(ids, timeline_csv)

ids <- ids %>%
  filter(screen_name!="BioAndBaseball") %>%
  filter(screen_name!="doubs10") %>%
  filter(screen_name!="fwoodhouse")


ids_test <- ids$screen_name


id <- "adamjkucharski"
id_test <- c("adamjkucharski", "mlipsitch", "trvrb")
friends <- get_friends(id_test)

rate_limit <- rate_limit()

# Function: for a given Twitter account ID, create a CSV file with all friends
friends_csv <- function(id) {
  
  # Check if this Twitter account's friends have already been downloaded
  filenames <- list.files(path="./data/friends/") # Get all file names in folder
  if(any(grepl(id, filenames, ignore.case=TRUE))) {
    print(paste("Already done:",id))
    return()
  }
  
  # Check if Twitter API rate limit has been reached. If yes, then wait for required time.
  if(rate_limit("get_friends")$remaining==0) { # Is the number of remaining calls 0?
    wait <- as.numeric(rate_limit("get_friends")$reset, units="secs") # Set wait time to the difference between the current time and the reset_at time, plus an extra 5 sec of buffer
    print(paste("Current time: ", Sys.time()))
    print(paste("Rate limit reached. Wait until:",Sys.time() + wait))
    Sys.sleep(wait) # Wait for set amount of time
    print(paste("Resumed running at: ", Sys.time()))
  }
  
  # Regular code
  tic(id) # Track runtime for function
  print(paste("Rate limit remaining: ", rate_limit("get_friends")$remaining))
  friends <- get_friends(id) # Get list of all friends
  if(lookup_users(id)$friends_count<1) { # Check if user has any friends. If not friends, then skip
    print(paste(id, "skipped due to no friends"))
    return()
  }
  friend_profiles <- get_profiles(friends$user_id) # Get the profiles of all these friends
  write_as_csv(friend_profiles, paste0("./data/friends/",id,"_friends.csv")) # Save as a CSV in the friends folder
  toc() # Track runtime for function
}

lapply(ids_test, friends_csv)



#--------------------------------------------------------------------------------------------------------
# Function: given a user's timeline, return what share of tweets are original tweets (i.e. not retweets)
get_original_tweet_share <- function(timeline) {
  total_tweet_count <- count(timeline)
  original_tweet_count <- sum(timeline$is_retweet == FALSE)
  retweet_count <- sum(timeline$is_retweet == TRUE)
  original_tweet_share <- original_tweet_count/total_tweet_count
  return(original_tweet_share)
}

#--------------------------------------------------------------------------------------------------------
# Quick download of user tweets
target_user <- "Stefunny4444" # Input user ID here
timeline <- get_timeline(target_user, n=3200) # Max 3,200 latest tweets
write_as_csv(timeline, paste0("./data/",target_user,"_tweets.csv")) # Save tweets as a CSV file named with target_user

twitter_stats <- tibble(user=character(), real=logical(), original_tweets=integer(), tweet_share=numeric())
twitter_stats <- tibble(
  user=target_user,
  real=TRUE,
  original_tweets=sum(timeline$is_retweet == FALSE),
  tweet_share=get_original_tweet_share(timeline)
)




#--------------------------------------------------------------------------------------------------------

# Social network graph
# Copied directly from the rtweet documentation: https://rtweet.info/reference/network_graph.html

library(igraph)
rstats <- search_tweets("#rstats", n = 200)
rstats_net <- network_data(rstats, "retweet,mention,reply")
attr(rstats_net, "idsn")
rstats_net <- network_graph(rstats)
windows()
plot(rstats_net)


#--------------------------------------------------------------------------------------------------------
