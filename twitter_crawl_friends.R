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
  consumer_secret "",
  access_token = "",
  access_secret = "")


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

# Function to get a set of profiles given a set of user IDs
get_profiles <- function(ids) {
  profiles <- lookup_users(ids)
  profiles <- simple_profiles(profiles)
  return(profiles)
}

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
  
  id_profile <- lookup_users(id)
  
  # Check if Twitter account is private
  if(is.na(id_profile$text)) {
    print(paste(id, "skipped due to private account"))
    return()
  }
  
  # Check if user has any friends. If not friends, then skip
  if(id_profile$friends_count<1) { 
    print(paste(id, "skipped due to no friends"))
    return()
  }
  
  # Regular code
  tic(id) # Track runtime for function
  print(paste("Rate limit remaining: ", rate_limit("get_friends")$remaining))
  friends <- get_friends(id) # Get list of all friends
  friend_profiles <- get_profiles(friends$user_id) # Get the profiles of all these friends
  write_as_csv(friend_profiles, paste0("./data/friends/",id,"_friends.csv")) # Save as a CSV in the friends folder
  toc() # Track runtime for function
}



#--------------------------------------------------------------------------------------------------------

# Check rate limit first, pause if reached
if(rate_limit("get_friends")$remaining==0) { # Is the number of remaining calls 0?
  wait <- as.numeric(rate_limit("get_friends")$reset, units="secs") # Set wait time to the difference between the current time and the reset_at time, plus an extra 5 sec of buffer
  print(paste("Current time: ", Sys.time()))
  print(paste("Rate limit reached. Wait until:",Sys.time() + wait))
  Sys.sleep(wait) # Wait for set amount of time
  print(paste("Resumed running at: ", Sys.time()))
}

seed_user <- "trvrb" # Select user
friends <- get_friends(seed_user) # Max 5,000 friends (friends are users that the seed user follows)
friend_profiles <- get_profiles(friends$user_id) # Get friend profiles
friend_profiles <- friend_profiles %>%
  arrange(desc(followers_count))

profiles <- friend_profiles

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
filenames <- list.files(path="./data/friends/") # Get all file names in folder
filenames <- tibble(screen_name = gsub("_friends.csv","",filenames)) # Remove ".csv" suffix and save as tibble
ids <- ids %>%
  anti_join(filenames) # Remove already downloaded Twitter accounts from total list of IDs

ids_test <- ids$screen_name


lapply(ids_test, try(friends_csv, true))

#--------------------------------------------------------------------------------------------------------



