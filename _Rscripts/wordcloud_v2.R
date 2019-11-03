rm(list=ls())
gc()

# Script for a creation of the word-cloud of html pages
library(rvest)
library(reshape)
library(tm)
library(wordcloud)
library(RColorBrewer)

# scan 1.post
test <- read_html("https://petolau.github.io/Forecast-electricity-consumption-with-similar-day-approach-in-R/")
page_subset1 <- html_nodes(test, c("p"));page_subset2 <- html_nodes(test, c("h2"));page_subset3 <- html_nodes(test, c("ul"))
text1 <- html_text(page_subset1);text2 <- html_text(page_subset2);text3 <- html_text(page_subset3)
content_1 <- matrix(c(stringi::stri_extract_all_words(text1, simplify = TRUE),
                      stringi::stri_extract_all_words(text2, simplify = TRUE),
                      stringi::stri_extract_all_words(text3, simplify = TRUE)), nrow = 1)

# scan 2.post
test <- read_html("https://petolau.github.io/Forecast-double-seasonal-time-series-with-multiple-linear-regression-in-R/")
page_subset1 <- html_nodes(test, c("p"));page_subset2 <- html_nodes(test, c("h2"));page_subset3 <- html_nodes(test, c("ul"))
text1 <- html_text(page_subset1);text2 <- html_text(page_subset2);text3 <- html_text(page_subset3)
content_2 <- matrix(c(stringi::stri_extract_all_words(text1, simplify = TRUE),
                      stringi::stri_extract_all_words(text2, simplify = TRUE),
                      stringi::stri_extract_all_words(text3, simplify = TRUE)), nrow = 1)

# scan 3.post
test <- read_html("https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/")
page_subset1 <- html_nodes(test, c("p"));page_subset2 <- html_nodes(test, c("h2"));page_subset3 <- html_nodes(test, c("ul"))
text1 <- html_text(page_subset1);text2 <- html_text(page_subset2);text3 <- html_text(page_subset3)
content_3 <- matrix(c(stringi::stri_extract_all_words(text1, simplify = TRUE),
                      stringi::stri_extract_all_words(text2, simplify = TRUE),
                      stringi::stri_extract_all_words(text3, simplify = TRUE)), nrow = 1)

# scan 4.post
test <- read_html("https://petolau.github.io/Regression-trees-for-forecasting-time-series-in-R/")
page_subset1 <- html_nodes(test, c("p"));page_subset2 <- html_nodes(test, c("h2"));page_subset3 <- html_nodes(test, c("ul"))
text1 <- html_text(page_subset1);text2 <- html_text(page_subset2);text3 <- html_text(page_subset3)
content_4 <- matrix(c(stringi::stri_extract_all_words(text1, simplify = TRUE),
                      stringi::stri_extract_all_words(text2, simplify = TRUE),
                      stringi::stri_extract_all_words(text3, simplify = TRUE)), nrow = 1)

# scan 5.post
test <- read_html("https://petolau.github.io/Ensemble-of-trees-for-forecasting-time-series/")
page_subset1 <- html_nodes(test, c("p"));page_subset2 <- html_nodes(test, c("h2"));page_subset3 <- html_nodes(test, c("ul"))
text1 <- html_text(page_subset1);text2 <- html_text(page_subset2);text3 <- html_text(page_subset3)
content_5 <- matrix(c(stringi::stri_extract_all_words(text1, simplify = TRUE),
                      stringi::stri_extract_all_words(text2, simplify = TRUE),
                      stringi::stri_extract_all_words(text3, simplify = TRUE)), nrow = 1)

# scan 6.post
test <- read_html("https://petolau.github.io/TSrepr-time-series-representations/")
page_subset1 <- html_nodes(test, c("p"));page_subset2 <- html_nodes(test, c("h2"));page_subset3 <- html_nodes(test, c("ul"))
text1 <- html_text(page_subset1);text2 <- html_text(page_subset2);text3 <- html_text(page_subset3)
content_6 <- matrix(c(stringi::stri_extract_all_words(text1, simplify = TRUE),
                      stringi::stri_extract_all_words(text2, simplify = TRUE),
                      stringi::stri_extract_all_words(text3, simplify = TRUE)), nrow = 1)

# scan 7.post
test <- read_html("https://petolau.github.io/TSrepr-clustering-time-series-representations/")
page_subset1 <- html_nodes(test, c("p"));page_subset2 <- html_nodes(test, c("h2"));page_subset3 <- html_nodes(test, c("ul"))
text1 <- html_text(page_subset1);text2 <- html_text(page_subset2);text3 <- html_text(page_subset3)
content_7 <- matrix(c(stringi::stri_extract_all_words(text1, simplify = TRUE),
                      stringi::stri_extract_all_words(text2, simplify = TRUE),
                      stringi::stri_extract_all_words(text3, simplify = TRUE)), nrow = 1)

# scan 8.post
test <- read_html("https://petolau.github.io/Multiple-data-streams-clustering-in-r/")
page_subset1 <- html_nodes(test, c("p"));page_subset2 <- html_nodes(test, c("h2"));page_subset3 <- html_nodes(test, c("ul"))
text1 <- html_text(page_subset1);text2 <- html_text(page_subset2);text3 <- html_text(page_subset3)
content_8 <- matrix(c(stringi::stri_extract_all_words(text1, simplify = TRUE),
                      stringi::stri_extract_all_words(text2, simplify = TRUE),
                      stringi::stri_extract_all_words(text3, simplify = TRUE)), nrow = 1)


# scan 9.post
test <- read_html("https://petolau.github.io/Bootstrapping-time-series-for-improving-forecasting-in-R/")
page_subset1 <- html_nodes(test, c("p"));page_subset2 <- html_nodes(test, c("h2"));page_subset3 <- html_nodes(test, c("ul"))
text1 <- html_text(page_subset1);text2 <- html_text(page_subset2);text3 <- html_text(page_subset3)
content_9 <- matrix(c(stringi::stri_extract_all_words(text1, simplify = TRUE),
                      stringi::stri_extract_all_words(text2, simplify = TRUE),
                      stringi::stri_extract_all_words(text3, simplify = TRUE)), nrow = 1)

# merge all post to one 1 data.frame
contents_all <- data.frame(words = c(content_1, content_2, content_3, content_4, content_5,
                                     content_6, content_7, content_8, content_9),
                           stringsAsFactors = F)

# convert each list content into a corpus
dataset_corpus <- Corpus(VectorSource(contents_all))

# remove punctuation, numbers and stopwords
dataset_corpus_all <- tm_map(dataset_corpus, removePunctuation)
dataset_corpus_all <- tm_map(dataset_corpus_all, removeNumbers)
dataset_corpus_all <- tm_map(dataset_corpus_all, removeWords, stopwords("english"))
dataset_corpus_all <- tm_map(dataset_corpus_all, stripWhitespace)

# remove some unintersting words
words_to_remove <- c("said","till","just","like","well","it's","from","what","told","over","more","other","have",
                     "last","with","this","that","such","when","been","says","will","also","where","why","would",
                     "today","about","both","only","they","can")
dataset_corpus_all <- tm_map(dataset_corpus_all, function(x) removeWords(x, words_to_remove))

# compute term matrix & convert to matrix class --> you get a table summarizing the occurence of each word in each class.
document_tm <- TermDocumentMatrix(dataset_corpus_all)
document_tm_mat <- as.matrix(document_tm)
rownames(document_tm_mat) <- iconv(enc2utf8(rownames(document_tm_mat)), sub="byte")


# remove words in term matrix with length < 4
index <- as.logical(sapply(row.names(document_tm_mat), function(x) nchar(x)>3 ))
document_tm_mat_s <- document_tm_mat[index,]

head(document_tm_mat_s)
names(document_tm_mat_s)

# Plot it
# 
# wordcloud(words = names(document_tm_mat_s), freq = document_tm_mat_s, min.freq = 10,
#           max.words=200, random.order=FALSE, rot.per=0.35, scale=c(4, .5),
#           colors=brewer.pal(4, "Dark2"))
# 
# # Old (alternative) solution:

library(wordcloud2)
library(data.table)

dt <- data.table(word = names(document_tm_mat_s),
                 freq = document_tm_mat_s)

wordcloud2(data=dt, size=1.6, color = "random-dark", minSize = 19, shape = "diamond", shuffle = F,
           minRotation = 0, maxRotation = 0)


# Preprocess words nad frequencies

# words_best <- rowSums(document_tm_clean_mat_s)
# words_best <- data.frame(word = names(words_best), freq = words_best)
# head(words_best)

# wordcloud2(as.data.frame(words_best), size = 0.4, minSize = 5,
#            backgroundColor = brewer.pal(6,"Greys")[2],
#            color = "random-dark")
