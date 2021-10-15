install.packages("recommenderlab", "ggplot2", "data.table", "reshape2")
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

  #now I'm retrieving the data
movie_data <- read.csv("movies.csv",stringsAsFactor=FALSE)
rating_data <- read.csv("ratings.csv")
str(movie_data)

  #ran into some issues, getting some more info
getwd()
list.files()
source()

  #fixed the issue
summary(movie_data)
head(movie_data)
summary(rating_data)
head(rating_data)
  #creating a one-hot encoding to make a matrix of the genres for each film
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
library(data.table)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1],'[|]',
                                       type.convert=TRUE),
                              stringsAsFactors=FALSE)
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
    genre_mat1[index+1,gen_col] <- 1
}
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE)

for (col in 1:ncol(genre_mat1)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}
str(genre_mat2)

  #Making a search matrix of the films by genre
SearchMatrix <- cbind(movie_data[,1:2],
                      genre_mat2[])
head(SearchMatrix)

  #Converting into a sparse matrix
ratingMatrix <- dcast(rating_data,
                      userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1])
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")

  #Implementing an Item Based Collaborative Filtering Model
recommendation_model$IBCF_realRatingMatrix$parameters

  #Computing similarities usings cosine, pearson, and jaccard operators
similarity_mat <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "user")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

movie_similarity <- similarity(ratingMatrix[, 1:4], method =
                                 "cosine",
                               which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies similarity")

  #Extracting the unique ratings
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values)

  #Creating a table of the ratings
Table_of_Ratings <- table(rating_values)
Table_of_Ratings

  #Counting the number of views for each film and creating a table, grouping them in descending order
library(ggplot2)
movie_views <- colCounts(ratingMatrix)
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views)
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ]
table_views$title <- NA
for (index in 1:10325) {
  table_views[index,3] <- as.character(subset(movie_data,
                                        movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,]

  #Making a bar graph for views of the top films, still using ggplot2
ggplot(table_views[1:6, ], aes(x= title, y = views)) + 
  geom_bar(stat="identity", fill = 'steelblue') + 
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggtitle("Total Views of the Top Films")

  #Creating a heatmap of the movie ratings
image(ratingMatrix[1:20, 1:25], axes = FALSE,
      main = "Heatmap of the first 25 rows and 25 columns")

  #Doing some data prep, first setting a minimum rating score of 50 for data
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings

  #Delineating the matrix of relevant users
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
main = "Heatmap of the top users and movies")

  #Visualizing the distribution of the average ratings per user
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"),
      col=I("red")) +
  ggtitle("Distribution of the average rating per user")

  #Time to normalize the data! Woohoo!
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalozed Ratings of the Top Users")

  #Binarizing the data. Rating above a 3=1, anything below a 3=0
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)

good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies, colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")
