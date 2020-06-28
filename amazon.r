install.packages(c("rvest","XML","magrittr"))

library(rvest)
library(XML)
library(magrittr)
# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Redmi-Note-Neptune-Blue-128GB/dp/B07SSGJYH3/ref=sr_1_1?dchild=1&keywords=mi+note7pro+reviews&qid=1591598741&sr=8-1#customerReviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".review-text") %>% html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)
write.table(amazon_reviews,"apple.txt",row.names = F)
install.packages("tm")  # for text mining
install.packages(c("SnowballC","textstem")) # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes


library('tm')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library('textstem')

# Importing apple reviews data
x <- as.character(amazon_reviews)
x <- iconv(x, "UTF-8") #Unicode Transformation Format. The '8' means it uses 8-bit blocks to represent a character
# Load the data as a corpus
x <- Corpus(VectorSource(x))
inspect(x[1])
# Convert the text to lower case
x1 <- tm_map(x, tolower)
inspect(x1[1])
# Remove numbers
x1 <- tm_map(x1, removeNumbers)
# Remove punctuations
x1 <- tm_map(x1, removePunctuation)
# Remove english common stopwords
x1 <- tm_map(x1, removeWords, stopwords('english'))
# Remove your own stop word
# specify your stopwords as a character vector
x1 <- tm_map(x1, removeWords, c("phone", "mi","the","will"))
#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])
# Text stemming
x1<-lemmatize_words(x1)
#x1 <- tm_map(x1, stemDocument)
# Term document matrix 
# converting unstructured data to structured format using TDM
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
#Frequency
v <- sort(rowSums(tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
# Bar plot
w <- rowSums(tdm)
w_sub <- subset(w, w >= 10)
barplot(w_sub, las=3, col = rainbow(20))

# Term laptop repeats in all most all documents
x1 <- tm_map(x1, removeWords, c('phone','air',"mobile",'can','will',"amazon",'phone','mi','product'))
x1 <- tm_map(x1, stripWhitespace)
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
w1 <- rowSums(tdm)
# Word cloud
#with all the words
wordcloud(words = names(w1), freq = w1, random.order = F, colors = rainbow(20), scale=c(2,.2), rot.per = 0.3)
# lOADING +VE AND -VE dictonaries

pos.words = scan(file.choose(), what="character", comment.char=";")
neg.words = scan(file.choose(), what="character", comment.char=";")
pos.words = c(pos.words,"wow", "kudos", "hurray")
# Positive wordcloud
pos.matches = match(names(w), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w[pos.matches]
p_names <- names(freq_pos)

wordcloud(p_names,freq_pos,scale=c(3.5,.2),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w[neg.matches]
n_names <- names(freq_neg)

wordcloud(n_names,freq_neg,scale=c(3.5,.2),colors = brewer.pal(8,"Dark2"))
#Association between words
tdm <- TermDocumentMatrix(x1)
findAssocs(tdm, c("screen"),corlimit = 0.3)

# Sentiment Analysis #
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read File 
amzon_reviews <- read.delim('apple.TXT')
reviews <- as.character(amzon_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[6]
# on tweet 6, you have 3 for anger,8 for anticipation ,2 for disgust ,4 for fear 
#  4 for joy, each one for sadness and surprise, 8 for trust , 9 words for negative and 10 positive.
get_nrc_sentiment('ridiculous')
#ridiculous has 1 anger 1 disgust and 1 negative
# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),ylab = 'Count',main= 'Sentiment scores for Amazon Reviews
        for mobile')



