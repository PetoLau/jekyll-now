rm(list=ls())
gc()

# Script for a creation of the word-cloud of html pages
library(rvest)
library(reshape)
library(tm)
library(wordcloud)
library(RColorBrewer)

# scan 1.post
test <- read_html("https://petolau.github.io/Forecast-double-seasonal-time-series-with-multiple-linear-regression-in-R/")
text <- html_text(test) 
content_2 <- stringi::stri_extract_all_words(text, simplify = TRUE)

# scan 2.post
test <- read_html("https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/")
text <- html_text(test) 
content_3 <- stringi::stri_extract_all_words(text, simplify = TRUE)

# scan 3.post
test <- read_html("https://petolau.github.io/Forecast-electricity-consumption-with-similar-day-approach-in-R/")
text <- html_text(test) 
content_1 <- stringi::stri_extract_all_words(text, simplify = TRUE)

# merge all post to one 1 data.frame
contents_all <- data.frame(words = c(content_1, content_2, content_3))
labels_all <- c(rep("Intro_Smart_Meters", length(content_1)),
                rep("MLR", length(content_2)),
                rep("GAM", length(content_3)))


dataset_s <- sapply(unique(labels_all), function(label) list(contents_all[labels_all %in% label, 1]) )

# convert each list content into a corpus
dataset_corpus <- lapply(dataset_s, function(x) Corpus(VectorSource( toString(x) ))) 

# merge all documents into one single corpus
dataset_corpus_all <- dataset_corpus[[1]]
for (i in 2:length(unique(labels_all))) { dataset_corpus_all <- c(dataset_corpus_all, dataset_corpus[[i]]) }

# remove punctuation, numbers and stopwords
dataset_corpus_all <- tm_map(dataset_corpus_all, removePunctuation)
dataset_corpus_all <- tm_map(dataset_corpus_all, removeNumbers)
dataset_corpus_all <- tm_map(dataset_corpus_all, function(x) removeWords(x,stopwords("english")))

# remove some unintersting words
words_to_remove <- c("said","from","what","told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today","about","both","only","they")
dataset_corpus_all <- tm_map(dataset_corpus_all, removeWords, words_to_remove)

# compute term matrix & convert to matrix class --> you get a table summarizing the occurence of each word in each class.
document_tm <- TermDocumentMatrix(dataset_corpus_all)
document_tm_mat <- as.matrix(document_tm)
colnames(document_tm_mat) <- unique(labels_all)
document_tm_clean <- removeSparseTerms(document_tm, 0.8)
document_tm_clean_mat <- as.matrix(document_tm_clean)
colnames(document_tm_clean_mat) <- unique(labels_all)

# remove words in term matrix with length < 4
index <- as.logical(sapply(rownames(document_tm_clean_mat), function(x) (nchar(x)>3) ))
document_tm_clean_mat_s <- document_tm_clean_mat[index,]

head(document_tm_clean_mat_s)

# Plot it

comparison.cloud(document_tm_clean_mat_s, max.words = 400, random.order = FALSE, scale = c(3.8, 0.8),
                 colors = c("dodgerblue3", "mediumseagreen", "orangered1"), title.size = 1.5)

# Old (alternative) solution:

# library(wordcloud2)
# library(data.table)

# Preprocess words nad frequencies

# words_best <- rowSums(document_tm_clean_mat_s)
# words_best <- data.frame(word = names(words_best), freq = words_best)
# head(words_best)

# wordcloud2(as.data.frame(words_best), size = 0.4, minSize = 5,
#            backgroundColor = brewer.pal(6,"Greys")[2],
#            color = "random-dark")
