library(httr)
library(dplyr)
library(jsonlite)

base_uri <- "https://api.themoviedb.org/3"

source("apikey.R")

key_param <- 
  list("api_key" = tmdb_key)


#########################
### 1 Trending Actors ###
#########################

# function that expects media_type and time window as arguements and returns df 
# that includes the names and  images of the top 5 trending actors.

get_trending_table <- 
  function(media_type, time_window) {
      resource <- paste0("/trending/", media_type, "/", time_window)
    response <- GET( paste0(base_uri, resource, "?", "api_key=", query = key_param) ) 
    body <- content( response,"text") %>% 
      fromJSON()
    data <- body$results
    actors <- data %>% 
      filter(known_for_department == "Acting")
    top_5_actors <- actors[1:5, ]
    Names <- c(top_5_actors$name)
    image_uris <- 
      (paste0("https://image.tmdb.org/t/p/h100", 
              top_5_actors$profile_path))
    trending_df <- as.data.frame(Names)
    trending_df$Photo = 
      c(paste0("![", top_5_actors$name, "]","(", image_uris, ")")) 
    return(trending_df)
  }

get_trending_table("person", "week") 

#############################
### 2 Specific Actor Data ###
#############################

# Function that expects person as arguement and returns a list that includes the 
# actor name, imdb id, id, biography, and top three movie titles. 

get_actor_data <- function(person) {
  params_list <-
    list(api_key = key_param, query = person)
  resource <- paste0( "/search/person?")
  response <- 
  GET(paste0(base_uri, resource), query = params_list) 
  body <- content(response, "text") %>% 
    fromJSON()
  data <- body$results
  actor <-
    data[1, ]
  
 actor_id <- 
  actor[c("id")] %>% 
 as.list() 
 
  movie_titles <- 
    actor$known_for %>% 
      as.data.frame() %>% 
     select(title) %>% 
    as.list() 

  resource2 <- 
    paste0(base_uri, "/person/", actor_id)
   response2 <- 
  GET(resource2, query = key_param)
  body2 <- content(response2, "text") %>% 
    fromJSON()
  body2_info <- 
  body2[c("name", "imdb_id", "biography")]
  
  return(c(body2_info, movie_titles, actor_id))
  
  } 

get_actor_data("Brad Pitt") 

###########################
### 3 Analysis an Actor ###
###########################

# Function that expects person as an argument and returns a list of raw data, a 
# tibble showing year and average movie popularity, and a tibble showing the
# most populary year and the popularity for that year.

# Question: Which year did Brad Pitt's movies peak in popularity?

analyze_actor <- function(person) {
  params_list <-
  list(api_key = key_param, query = person)
resource <- 
  paste0( "/search/person?")
response <- 
  GET(paste0(base_uri, resource), query = params_list) 
  body <- content(response, "text") %>% 
  fromJSON()
  data <- 
    body$results
  
  actor_id <- 
      data[c("id")]
  
  param_list <- list(
    api_key = key_param,
    sort_by = "revenue.desc",
    with_people = actor_id)
  resource2 <- paste0(base_uri, "/discover/movie?")
  response2 <- GET(resource2, query = param_list)
  body2 <- 
  content(response2, "text") %>% 
    fromJSON()
  data2 <- 
  body2$results  
  avg_pop_year <- 
  data2 %>% 
    mutate(
      year = substr(release_date, 1,4)
    ) %>% 
    group_by(year) %>% 
    summarize(
      avg_popularity = mean(popularity)
    ) %>% 
    arrange(desc(avg_popularity))
  
most_popular_year <-   
  avg_pop_year %>% 
    filter(avg_popularity == max(avg_popularity))
      
return(
  list(data2, most_popular_year, avg_pop_year)
)  

  }

analyze_actor("Brad Pitt") 















