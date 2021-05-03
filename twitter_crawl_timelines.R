

profiles <- read_csv("./data/friends/trvrb_friends.csv")

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

#----------------------------------------------------------------------------------------------------------------------------------------------------------


filenames_friends <- list.files(path="./data/friends/") # Get all file names in folder
filenames_friends <- tibble(screen_name = gsub("_friends.csv","",filenames_friends)) # Remove ".csv" suffix and save as tibble

filenames_timelines <- list.files(path="./data/timelines/") # Get all file names in folder
filenames_timelines <- tibble(screen_name = gsub(".csv","",filenames_timelines)) # Remove ".csv" suffix and save as tibble


filenames <- filenames_friends %>%
  anti_join(filenames_timelines)



ids <- filenames$screen_name

# Function: for a given Twitter account ID, create a CSV file with all friends
timeline_csv <- function(id) {
  
  # Check if this Twitter account's friends have already been downloaded
  filenames <- list.files(path="./data/timelines/") # Get all file names in folder
  if(any(grepl(id, filenames, ignore.case=TRUE))) {
    print(paste("Already done:",id))
    return()
  }
  
  # Check if Twitter API rate limit has been reached. If yes, then wait for required time.
  if(rate_limit("get_timeline")$remaining<400) { # Is the number of remaining calls 0?
    wait <- 15*60 # Set wait time to the difference between the current time and the reset_at time, plus an extra 5 sec of buffer
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
  
  # Regular code
  tic(id) # Track runtime for function
  print(paste("Rate limit remaining: ", rate_limit("get_timeline")$remaining))
  timeline <- get_timeline(id, n=3200) # Get list of all friends
  timeline <- tweets_data(timeline) # Get tweets only, drop profile info
  timeline <- plain_tweets(timeline) # Clean up characters in tweet text
  write_as_csv(timeline, paste0("./data/timelines/",id,".csv")) # Save as a CSV in the friends folder
  toc() # Track runtime for function
}

lapply(ids, try(timeline_csv, true))
