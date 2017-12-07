#################### New York Times Movie Review - API ####################
# Used opening date range 2017-10-13 through 2017-11-21

# Required libraries 
  library(jsonlite)
  library(knitr)

# Identifying New York Times Development Network API Key
  NYT_APIKEY = "3bcb75c8465042e18eff19cf103131ef"

# Results 0-20 reviews as of November 21, 2017
  NYTReviews_URL_1 = paste0('http://api.nytimes.com/svc/movies/v2/reviews/search.json?opening-date=2017-10-13;2017-11-21&api-key=', NYT_APIKEY , '&order=by-title', collapse = "") # Results 0-20 URL
  json_file1 <- fromJSON(URLencode(NYTReviews_URL_1)) # Getting JSON
  df1 <- as.data.frame(json_file1$results) # Making dataframe for results 
  df1.2 <- df1[,c("display_title","mpaa_rating","critics_pick","byline","headline","summary_short","publication_date","opening_date")] # Subset of specific columns from df1 dataframe

# Results 21-40 reviews as of November 21, 2017
  NYTReviews_URL_2 = paste0('http://api.nytimes.com/svc/movies/v2/reviews/search.json?opening-date=2017-10-13;2017-11-21&api-key=', NYT_APIKEY , '&offset=20&order=by-title', collapse = "") # Results 21-40 URL  
  json_file2 <- fromJSON(NYTReviews_URL_2) # Getting JSON
  df2 <- as.data.frame(json_file2$results) # Making dataframe for results 
  df2.2 <- df2[,c("display_title","mpaa_rating","critics_pick","byline","headline","summary_short","publication_date","opening_date")] # Subset of specific columns from df2 dataframe

# Results 41-60 reviews as of November 21, 2017
  NYTReviews_URL_3 = paste0('http://api.nytimes.com/svc/movies/v2/reviews/search.json?opening-date=2017-10-13;2017-11-21&api-key=', NYT_APIKEY , '&offset=40&order=by-title', collapse = "") # Results 41-60 URL 
  json_file3 <- fromJSON(NYTReviews_URL_3) # Getting JSON
  df3 <- as.data.frame(json_file3$results) # Making dataframe for results 
  df3.2 <- df3[,c("display_title","mpaa_rating","critics_pick","byline","headline","summary_short","publication_date","opening_date")] # Subset of specific columns from df3 dataframe

# Results 61-65 reviews as of November 21, 2017
  NYTReviews_URL_4 = paste0('http://api.nytimes.com/svc/movies/v2/reviews/search.json?opening-date=2017-10-13;2017-11-21&api-key=', NYT_APIKEY , '&offset=60&order=by-title', collapse = "")  # Results 61-65 URL
  json_file4 <- fromJSON(NYTReviews_URL_4) # Getting JSON
  df4 <- as.data.frame(json_file4$results) # Making dataframe for results
  df4.2 <- df4[,c("display_title","mpaa_rating","critics_pick","byline","headline","summary_short","publication_date","opening_date")] # Subset of specific columns from df4 dataframe

#Results 0-65 reviews combined 
  reviews.df <- rbind(df1.2, df2.2, df3.2, df4.2)
  
# Adding URL column to results
  URL1 <- json_file1$results$link # Getting URLs for results 0-20
  URL1.2 <- URL1[,c("type", "url")] # Subset of specific columns 

  URL2 <- json_file2$results$link # Getting URLs for results 21-40
  URL2.2 <- URL2[,c("type", "url")] # Subset of specific columns 

  URL3 <- json_file3$results$link # Getting URLs for results 41-60
  URL3.2 <- URL3[,c("type", "url")] # Subset of specific columns 

  URL4 <- json_file4$results$link # Getting URLs for results 61-65
  URL4.2 <- URL4[,c("type", "url")] # Subset of specific columns 

#Results 0-66 URLs combined  
  URLs.df <- rbind(URL1.2, URL2.2, URL3.2, URL4.2)

# Reviews and Links combined
combo.reviews.links.df <- cbind(reviews.df, URLs.df) # Combining all 65 reviews and URL links
combo.reviews.links.df <- combo.reviews.links.df[,c(1:3,7,8,10)] # Subset of specific columns
combo.reviews.links.df <- combo.reviews.links.df[,c(1,5,4,6,2,3)] # Reorder or specific columns

kable(combo.reviews.links.df, row.names = TRUE) # Display combined reviews and URL links dataframe with row numbers
write.table(combo.reviews.links.df, "New York Times Movie Reviews1.csv") # Export reviews and URL links dataframe to .csv


######################################################################################################################################
#################### New York Times Movie Review - Review, Genre, and Sentiment ####################
# Required Libraries
  library(RCurl)
  library(XML)
  library(dplyr)
  library(RAdwords)


# Installing Indico package
  #source("http://bioconductor.org/biocLite.R") # Remove hashtag to run
  #biocLite("EBImage") # Remove hashtag to run

  library(devtools)
  #devtools::install_github("IndicoDataSolutions/IndicoIo-R") # Remove hashtag to run
  library(indicoio)

# indico.io API Key
  indico.io_APIkey <-"2cf9508e4628b67c9b69ae7d1059efda"

# Retrieving list of 65 movies curated from the NYT API pull
  movie_list_url <- "https://raw.githubusercontent.com/ashleyrodondi/NYT-Sentiment-Analysis/master/New%20York%20Times%20Movie%20Reviews.csv"
  movie_list <-read.csv(movie_list_url, header = TRUE, sep = "")
  
  movie_list[1:10,] # Checking the first 10 rows from the movie_list dataframe 
  str(movie_list) # Checking the strings for the movie_list variables

# Creating an empty dataframe and list for review, genre, and sentiment score to be put into
  movie_score <- data.frame()
  movie_review_text <- list()
  
# Setting agent for RCurl pars in forloop
  agent <- paste(c(R.version$version.string, ",", R.version$platform),collapse = "")
  
# Forloop to get review text, genre, and sentiment score for each NYT movie URL
  for (i in 1:nrow(movie_list)){
    
    #Set RCurl pars; initialize CURL context/handle which can be used for multiple requests
    curl <- getCurlHandle()
    curlSetOpt(cookiejar="cookies.txt",  useragent = agent, followlocation = TRUE, curl=curl) #If I do not need to read cookies use "curlSetOpt(cookiejar="", useragent = agent, followlocation = TRUE, curl=curl)"
    
    # Selecting movie URL from movie_list 
    movie_url <- movie_list$url[i]
    
    # Get review for corresponding movie URL
    movie_specific <- getURL(as.character(movie_url), curl = curl, verbose = TRUE)
    content_review = htmlTreeParse(movie_specific, asText = TRUE, useInternalNodes = TRUE, encoding = 'UTF-8')
    
    # Code to get review text 
    plain_text <- xpathSApply(xmlRoot(content_review), "//p[@class = 'story-body-text story-content']", xmlValue)
    review_df <- as.data.frame(as.character(paste(plain_text, collapse = "\r\n")))
    colnames(review_df) <- "review"
    
    review_df$movie <- movie_list$movie[i]
    movie_review_text[[i]] <- plain_text
    
    # Code to get sentiment score 
    review_df$sentiment_score <- unlist(sentiment(as.character(review_df$review), api_key = indico.io_APIkey))
    
    # Code to get genre
    genre <- xpathSApply(xmlRoot(content_review), "(//span[@itemprop='genre'][@class = 'genre'])[1]", xmlValue)
    review_df$genre <- genre
    
    movie_score <- rbind(movie_score, review_df)
  }

# write out data to .csv
  nyt_movie_score <- inner_join(movie_list, movie_score, by = "movie")
  write.csv(nyt_movie_score, "New York Times Movie Reviews Sentiment.csv") #export sentiment 
  
  
######################################################################################################################################  
#################### New York Times Movie Review - Sentiment Analysis (Sensitivity and Specificity) ####################
# Required Libraries
  library (dplyr)
  
# Retrieving New York Times Reviews Sentiment csv 
  URL <- "https://raw.githubusercontent.com/ashleyrodondi/NYT-Sentiment-Analysis/master/New%20York%20Times%20Movie%20Reviews%20Sentiment.csv"
  NYT_review_sentiment <- read.csv(URL, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Creating dataframes for sensitivity and specificity analysis - 0.75 test  
  # Dataframe True Positive
  true_positive <- NYT_review_sentiment %>% filter(sentiment_score >= 0.75 & critics_pick == 1) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)
  
  # Dataframe True Negative
    true_negative <- NYT_review_sentiment %>% filter(sentiment_score < 0.75 & critics_pick == 0) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)
  
  # Dataframe False Positive 
    false_positive <- NYT_review_sentiment %>% filter(sentiment_score >= 0.75 & critics_pick == 0) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)
  
  # Dataframe False Negative
    false_negative <- NYT_review_sentiment %>% filter(sentiment_score < 0.75 & critics_pick == 1) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)

# Calculating percentage for sensitivity and specificity analysis 
  # True Positive Percentage - (ANSWER: 0.1846154)
    12/65
    
  # True Negative Percentage - (ANSWER: 0.4153846)
    27/65
    
  # False Positive Percentage - (ANSWER: 0.3076923)
    20/65
    
  # False Negative Percentage - (ANSWER: 0.09230769)
    6/65
    
# Creating dataframes for sensitivity and specificity analysis - 0.50/0.80 test
  # Dataframe True Positive
    true_positive <- NYT_review_sentiment %>% filter(sentiment_score >= 0.80 & critics_pick == 1) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)
  
  # Dataframe True Neutral
    true_neutral <- NYT_review_sentiment %>% filter(sentiment_score >= 0.50 & sentiment_score < 0.80 & critics_pick == 0) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)
    
  # Dataframe True Negative
    true_negative <- NYT_review_sentiment %>% filter(sentiment_score < 0.50 & critics_pick == 0) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)
    
    # Dataframe False Positive
    False_positive <- NYT_review_sentiment %>% filter(sentiment_score >= 0.80 & critics_pick == 0) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)
    
    # Dataframe False Neutral
    False_neutral <- NYT_review_sentiment %>% filter(sentiment_score >= 0.50 & sentiment_score < 0.80 & critics_pick == 1) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)
    
    # Dataframe False Negative
    False_negative <- NYT_review_sentiment %>% filter(sentiment_score < 0.50 & critics_pick == 1) %>% 
      select(movie, mpaa_rating, genre, sentiment_score, critics_pick)
    
# Calculating percentage for sensitivity and specificity analysis 
  # True Positive Percentage - (ANSWER: 0.1846154)
    12/65
    
  # True Neutral Percentage - (ANSWER: 0.1538462)
    10/65
    
  # True Negative Percentage - (ANSWER: 0.2923077)
    19/65
    
  # False Positive Percentage - (ANSWER: 0.2769231)
    18/65
    
  # False Neutral Percentage - (ANSWER: 0.04615385)
    3/65
    
  # False Negative Percentage - (ANSWER: 0.04615385)
    3/65
    
    
#####################################################################################################################################  
#################### New York Times Movie Review - Logistic Regression Test ####################
# Retrieving New York Times Reviews Sentiment csv 
  URL <- "https://raw.githubusercontent.com/ashleyrodondi/NYT-Sentiment-Analysis/master/New%20York%20Times%20Movie%20Reviews%20Sentiment.csv"
  NYT_review_sentiment <- read.csv(URL, header = TRUE, sep = ",", stringsAsFactors = FALSE)     
    
  NYT.glm <- glm(critics_pick ~ 
                  sentiment_score +
                  genre,
                data = NYT_review_sentiment,
                family = "binomial")
  
  summary(NYT.glm)

# Critics Pick by Sentiment Score   
  plot(x = NYT_review_sentiment$sentiment_score,
       y = NYT_review_sentiment$critics_pick,
       type = "p",
       main = "Critics Pick by Sentiment Score",
       xlab = "Sentiment Score",
       ylab = "Critics Pick")
  
  
######################################################################################################################################  
#################### New York Times Movie Review - Density Plot of Sentiment Score ####################
# Required Libraries
  library(ggplot2)

# Retrieving New York Times Reviews Sentiment csv 
  URL <- "https://raw.githubusercontent.com/ashleyrodondi/NYT-Sentiment-Analysis/master/New%20York%20Times%20Movie%20Reviews%20Sentiment.csv"
  NYT_review_sentiment <- read.csv(URL, header = TRUE, sep = ",", stringsAsFactors = FALSE) 
  
# Density Plot Code
  p <- ggplot(NYT_review_sentiment, aes(x = sentiment_score, color = genre)) + 
    geom_density(alpha = 0.1, size =1) +
    ggtitle("Density Plot of Sentiment Score") +
    scale_y_continuous("Density") +
    scale_x_continuous("Sentiment Score") +
    labs(colour = "Genre") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
 
  print(p)

  p <- ggplot(NYT_review_sentiment, aes(x = sentiment_score)) + 
#    geom_density(alpha = 0.1, size =1) +
    geom_histogram() + #alpha = 0.1, size =1) +
    facet_wrap(~genre) +
    ggtitle("Density Plot of Sentiment Score") +
    scale_y_continuous("Density") +
    scale_x_continuous("Sentiment Score") +
    labs(colour = "Genre") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  print(p)
  
  p <- ggplot(NYT_review_sentiment, aes(x = sentiment_score)) + 
        geom_density(alpha = 0.1, size =1) +
    #geom_histogram() + #alpha = 0.1, size =1) +
    facet_wrap(~genre) +
    ggtitle("Density Plot of Sentiment Score") +
    scale_y_continuous("Density") +
    scale_x_continuous("Sentiment Score") +
    labs(colour = "Genre") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  print(p)
  
######################################################################################################################################  
#################### New York Times Movie Review - Summary and Describe of NYT Review Sentiment Dataframe ####################
# Required Libraries
  library(Hmisc)
  
# Retrieving New York Times Reviews Sentiment csv 
  URL <- "https://raw.githubusercontent.com/ashleyrodondi/NYT-Sentiment-Analysis/master/New%20York%20Times%20Movie%20Reviews%20Sentiment.csv"
  NYT_review_sentiment <- read.csv(URL, header = TRUE, sep = ",", stringsAsFactors = FALSE) 

# Describe and Summary test   
  describe(NYT_review_sentiment)
  summary(NYT_review_sentiment)




















 ..