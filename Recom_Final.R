#libraries used in the project
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)


#The dataset used was from MovieLens, used the files movies.csv and ratings.csv to build a recommendation system.
movie<-read.csv("D:/Junaid/Movie/movies.csv",stringsAsFactors=FALSE)
rating<-read.csv("D:/Junaid/Movie/ratings.csv")

#top 6 rows and summary of movie.csv file
head(movie)
summary(movie)
#top 6 rows and summary of ratings.csv file
head(rating)
summary(rating)

#Data Pre-processing
#Both usersId and movieId are presented as integers and should be changed to factors.
movie$movieId<-factor(movie$movieId)
rating$userId<-factor(rating$userId)
rating$movieId<-factor(rating$movieId)

#afer preprocessing the data summary funcation of the files
head(movie)
summary(movie)
head(rating)
summary(rating)

#Genres of the movies are not easily usable because of their format, so extract them convert them according to.
genres <- as.data.frame(movie$genres, stringsAsFactors=FALSE)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]',type.convert=TRUE),stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

#finding the differnt genre types
#unique(genres2[,1])
genre_list<-unique(genres2[,1])
genre_list<-genre_list[-length(genre_list)] #reboving the null value
genre_list

#empty matrix, nrow(movie)+1=912, length(genre_list)=no of genres(18)
genre_matrix <- matrix(0,nrow(movie)+1,length(genre_list))

#set first row to genre list
genre_matrix[1,] <- genre_list

#set column names to genre list
colnames(genre_matrix) <- genre_list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
#convert from characters to integers
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
}

#Now, what we need is a user profile matrix. This can be easily done with the dcast() function in the reshape2 package. 
#I first convert the ratings into a binary format to keep things simple. ratings of  above 3 are mapped to 1, representing likes, 
#and ratings of 3 and below are mapped to -1, representing dislikes.

binaryratings <- rating
for (i in 1:nrow(binaryratings)){
 if (binaryratings[i,3] > 3){
   binaryratings[i,3] <- 1
 }
 else{
   binaryratings[i,3] <- -1
 }
}

binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

movieIds <- length(unique(movie$movieId)) #9125
ratingmovieIds <- length(unique(rating$movieId)) #9066
movies2 <- movie[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from genre_matrix2

genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL

#Calculate dot product for User Profiles
result = matrix(0,length(genre_list),ncol(binaryratings2))
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}
 
#Convert to Binary scale
for (i in 1:nrow(result)){
  if (result[i] < 0){
    result[i] <- 0
  }
  else {
    result[i] <- 1
  }
}

recom <- function(n){
  result2 <- result[,n]
  sim_mat <- rbind.data.frame(result2, genre_matrix3)
  sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer
  
  #Calculate Jaccard distance between user profile and all movies
  sim_results <- dist(sim_mat, method = "Jaccard")
  sim_results <- as.data.frame(as.matrix(sim_results[1:9066]))
	rows <- which(sim_results == min(sim_results))
	return(rows)
}

#movie recommendation for first 10 users and there are 671 users 

for(i in seq(1:10))
print(movie[recom(i),2])

#we can recommend movie to any user by movie[recom(userId),2]