install.packages("ggthemes")
install.packages("qdap")
install.packages("dplyr")
install.packages("tm")
install.packages("wordcloud")
install.packages("plotrix")
install.packages("dendextend")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("RWeka")
install.packages("reshape2")
install.packages("quanteda")


library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)
library(qdapTools)


#Reading the CSV file

review = read.csv("E:/Christ University/Semester-3/04_NLP/Case_Study/data/Womens Clothing E-Commerce Reviews.csv", stringsAsFactors = FALSE)

names(review)

## Make a vector source and a corpus
corpus_review=Corpus(VectorSource(review$Review.Text))

#Convert to lower case
corpus_review = tm_map(corpus_review, tolower)
      
#Remove punctuation
corpus_review = tm_map(corpus_review, removePunctuation)  
      
#Remove stopwords
corpus_review = tm_map(corpus_review, removeWords, stopwords("english"))

# Remove context specific stop words
corpus_review = tm_map(corpus_review, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i"))

## Stem document
corpus_review = tm_map(corpus_review, stemDocument)

##Viewing the corpus content
corpus_review[[8]][1]

# Find the 20 most frequent terms: term_count
#term_count <- freq_terms(corpus_review, 20)

# Plot 20 most frequent terms
#plot(term_count)


review_dtm <- DocumentTermMatrix(corpus_review)

review_tdm <- TermDocumentMatrix(corpus_review)

# Convert TDM to matrix
review_m <- as.matrix(review_tdm)

# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)

# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)

# View the top 10 most common words
review_term_freq[1:10]
  
# Plot a barchart of the 20 most common words
barplot(review_term_freq[1:20], col = "steel blue", las = 2)


review_word_freq <- data.frame(term = names(review_term_freq),
                               num = review_term_freq)

# Create a wordcloud for the values in word_freqs
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = "red")

# Print the word cloud with the specified colors
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))


#View(review)

review_yes = review['Recommended.IND'] == 1
review_yes

review_no = review['Recommended.IND'] == 0
review_no


## Combine both corpora: all reviews
all_yes <- paste(review_yes, collapse = "")
all_no <- paste(review_no, collapse = "")
all_combine <- c(all_yes, all_no)

## Creating corpus for combination
corpus_review_all=Corpus(VectorSource(all_combine)) 

## Pre-processing corpus - all
#Convert to lower-case
corpus_review_all=tm_map(corpus_review_all, tolower)

#Remove punctuation
corpus_review_all=tm_map(corpus_review_all, removePunctuation)

#Remove stopwords
corpus_review_all=tm_map(corpus_review_all, removeWords, stopwords("english"))
corpus_review_all=tm_map(corpus_review_all, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress","just","i"))

#Stem document
corpus_review_all=tm_map(corpus_review_all, stemDocument)
review_tdm_all <- TermDocumentMatrix(corpus_review_all)
all_m=as.matrix(review_tdm_all)
colnames(all_m)=c("Yes","No")

#Sum rows and frequency data frame
review_term_freq_all <- rowSums(all_m)
review_word_freq_all <- data.frame(term=names(review_term_freq_all), num = review_term_freq_all)

#Make commonality cloud
commonality.cloud(all_m, 
                  colors = "steelblue1",
                  max.words = 50)

# Create comparison cloud
comparison.cloud(all_m,
                 colors = c("green", "red"),
                 max.words = 50)






      
      