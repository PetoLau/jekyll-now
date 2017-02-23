library(rvest)
library(wordcloud2)
library(data.table)
library(RColorBrewer)

test <- read_html("https://petolau.github.io/Forecast-double-seasonal-time-series-with-multiple-linear-regression-in-R/")
text <- html_text(test) 
content <- stringi::stri_extract_all_words(text, simplify = TRUE)

test <- read_html("https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/")
text <- html_text(test) 
content_2 <- stringi::stri_extract_all_words(text, simplify = TRUE)

test <- read_html("https://petolau.github.io/Forecast-electricity-consumption-with-similar-day-approach-in-R/")
text <- html_text(test) 
content_3 <- stringi::stri_extract_all_words(text, simplify = TRUE)

freq.word <- as.data.table(table(cbind(content, content_2, content_3)))
setkey(freq.word, N)
setnames(freq.word, "V1", "word")
setnames(freq.word, "N", "freq")

words_best <- freq.word[(freq > 9) & (freq < 65), ]
words_best <- words_best[which(words_best[, lapply(word, nchar)] > 4)]

wordcloud2(as.data.frame(words_best), size = 0.2, minSize = 10,
           backgroundColor = brewer.pal(6,"Greys")[2],
           color = "random-dark")
