# Name: Martha Gizaw
# Homework 2
# Due February 20, 2020

# SOLVING A STATISTICAL PROBLEM
# 1. Load in the "movies.csv" dataset.

movies <- read.csv("/Users/marth/OneDrive/Documents/R/Advanced R Portfolio/tmdb_5000_movies.csv", stringsAsFactors = F)
#View(movies)

# 2. Write a for loop that will produce summary statistics for each non-character variable 
# in the dataset. HINT: Use the summary() function.

# names(movies)
movies_numeric <- movies[, sapply(movies, is.numeric)]
# names(movies_numeric)

for(i in 1:ncol(movies_numeric)){
  print(names(movies_numeric[i]))
  print(summary(movies_numeric[,i]))
}

# For confirmation: summary(movies_numeric)
# View(movies_numeric)

# 3. Using an if-else statement, create a new variable designating movies as either high- or
# low-revenue movies. Justify your process.

movies$revenue_status <- ifelse(movies$revenue >= mean(movies$revenue), "high-revenue", "low-revenue")
# Wanna find out this number?
mean(movies$revenue)
# names(movies)
# View(movies)
# JUSTIFICATION: After creating a new variable for the dataset "movies", I used the ifelse 
# function to determine if a movie has revenue greater than or equal to the average revenue 
# among all movies. Otherwise, the movie is said to be low-revenue.

# 4. Write a function that, given a movie overview (use the "overview" variable) as an 
# argument, will return the three most common words from the overview.

# install.packages("qdap")
library(qdap)

three_words <- function(dataset){
  x <- freq_terms(dataset, 3)
  print(x)
}

three_words(dataset=movies$overview) # "the", "a", and "to"

# 5. Drop ALL movies that do not have either "Action" or "Adventure" as a genre.
#    (a) Then produce a histogram of the vote_average variable
#    (b) Identify the most common word for the entire "overview" variable.

for(i in 1:nrow(movies)) {
  temp <- gsub('[\\"\\"]', "", regmatches(movies$genres[i], gregexpr('\\".*?\\"', movies$genres[i]))[[1]])
  temp <- temp[temp != "id" & temp != "name"]
  movies$genres_clean[i] <- list(temp)
  y <- movies$genres_clean[[i]]
  movies$bool_genre[i] <- any(y == "Adventure" | y == "Action")
  movies_new <- movies[movies$bool_genre == TRUE, ]
}
View(movies_new)
movies_new$bool_genre <- NULL

# Create a histogram of the vote_average data in movies_new. Manually set the 
# margins of the histogram to avoid the plot.new() error for the oversizing of 
# margins.
par(mar=c(5,5,5,5))
hist(movies_new$vote_average)

# Create a new function called "best_word" for movies_new$overview. Base it off of 
# the previous function for finding the best three variables in Question 4.
best_word <- function(dataset){
  x <- freq_terms(dataset, 1)
  print(x)
}

best_word(dataset=movies_new$overview) # "the"
