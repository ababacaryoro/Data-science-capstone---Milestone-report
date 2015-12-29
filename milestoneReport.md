# Data-science-capstone---Milestone-report

---
title: "Data Science Coursera capstone - Milestone Report"
output: html_document
---

```{r echo=FALSE, cache=TRUE, warning=FALSE, message=FALSE}
library(rJava)
library(NLP)
library(RWeka)
library(magrittr)
library(clue)
library(tm)
library(quanteda)
require(wordcloud)
require(RColorBrewer)
```

The goal of this project is just to display that we've gotten used to working with the data and that we are on track to create our prediction algorithm. This document explains only the major features of the data wou have identified and briefly summarize our plans for creating the prediction algorithm and Shiny app in a way that would be understandable to a non-data scientist manager. 

# Loading data

This exercise uses 3 the files named "en\_US.blogs.txt", "en\_US.news.txt" and "en_US.twitter.txt". The data is from a corpus called HC Corpora (<http://corpora.heliohost.org>).

After downloading data, we load it with the _readLines_ and _read\_lines_ functions.

```{r echo=TRUE, cache=TRUE, warning=FALSE}
###################################################
### chunk number 1: Data import
###################################################
## Import or Load if already created
if(!file.exists("D:\\Data science\\Data science - Johns Hopkins University\\Data Science Capstone\\Données\\.RData")) { 
  ## Make a sample
  filenames <- Sys.glob("*.txt") # to list all text files in the directory
  # n <- R.utils::countLines(con)
  db_blogs <- readr::read_lines("en_US.blogs.txt")
  
  db_news <- readr::read_lines("en_US.news.txt")
  
  con <- file("en_US.twitter.txt", "r")
  db_tweet <- readLines(con, encoding="UTF-8")
  close(con)
  
  # prob <- rbinom(1000, prob = 0.5, size = 1) # binom de taille 1 donne bernoulli
  # sample <- NULL
  # for (i in 1:200){
  #   if(prob[i]) sample <- rbind(sample, readLines(con, 1))
  # }
  
} else {
  load("D:\\Data science\\Data science - Johns Hopkins University\\Data Science Capstone\\Données\\.RData")
}


```

After loading data, we just use a sample of it to make some basic analysis and also to train our model.

```{r echo=TRUE, cache=TRUE, warning=FALSE}
###################################################
### chunk number 2: Sampling
###################################################
## Import or Load if already created

if(!file.exists("D:\\Data science\\Data science - Johns Hopkins University\\Data Science Capstone\\Données\\.RData")) { 
#### sample blogs
prob <- rbinom(length(db_blogs), prob = 1000/length(db_blogs), size = 1) # binom of length 1 is bernoulli
sample_blogs <- NULL
for (i in 1:length(db_blogs)){
  if(prob[i]) sample_blogs <- c(sample_blogs, db_blogs[i])
}

#### sample news
prob <- rbinom(length(db_news), prob = 1000/length(db_news), size = 1) # binom of length 1 is bernoulli
sample_news <- NULL
for (i in 1:length(db_news)){
  if(prob[i]) sample_news <- c(sample_news, db_news[i])
}

#### sample tweets
prob <- rbinom(length(db_tweets), prob = 1000/length(db_tweets), size = 1) # binom of length 1 is bernoulli
sample_tweets <- NULL
for (i in 1:length(db_tweets)){
  if(prob[i]) sample_tweets <- c(sample_tweets, db_tweets[i])
}

}




```

A low proportion of data is used to avoid issues of memory, but it will be increased during upcoming phases.

# Basic report of summary statistics

We use here the _quanteda_ and _stylo_ packages to wrangle data, particularly, to generate the frequency of words, words pairs ...

First, we tokenize the corpus in order to have basic elements of the language.

```{r echo=TRUE, cache=TRUE, warning=FALSE}
###################################################
### chunk number 3: Tokenization
###################################################
token_blogs = quanteda::tokenize(sample_blogs, removeNumbers=TRUE, removePunct=TRUE)
token_news = quanteda::tokenize(sample_news, removeNumbers=TRUE, removePunct=TRUE)
token_tweets = quanteda::tokenize(sample_tweets, removeNumbers=TRUE, removePunct=TRUE)


```


Then we start by counting the number of worder and unique words in each sample.
```{r echo=TRUE, cache=TRUE, warning=FALSE}
###################################################
### chunk number 4: Exploratory analysis
###################################################


#####--- Distribution of words ---####
## Number of words
sum(ntoken(token_blogs)) # 39526
sum(ntoken(token_news)) # 31850
sum(ntoken(token_tweets)) # 12479
## Number of unique words in the corpus
dfm_blogs <- dfm(sample_blogs, ignoredFeatures = stopwords("english"), stem = TRUE)
dfm_news <- dfm(sample_news, ignoredFeatures = stopwords("english"), stem = TRUE)
dfm_tweets <- dfm(sample_tweets, ignoredFeatures = stopwords("english"), stem = TRUE)

ncol(dfm_blogs) # 7509
ncol(dfm_news) # 7534
ncol(dfm_tweets) # 3305
## Frequency tables
freq_blogs <- colSums(sort(dfm_blogs))
freq_blogs <- cbind(as.data.frame(freq_blogs), rownames(as.data.frame(freq_blogs)))
names(freq_blogs) <- c('Frequency', 'Word')

freq_news <- colSums(sort(dfm_news))
freq_news <- cbind(as.data.frame(freq_news), rownames(as.data.frame(freq_news)))
names(freq_news) <- c('Frequency', 'Word')

freq_tweets <- colSums(sort(dfm_tweets))
freq_tweets <- cbind(as.data.frame(freq_tweets), rownames(as.data.frame(freq_tweets)))
names(freq_tweets) <- c('Frequency', 'Word')

```

In the code above, we already have the frequency tables for each sample file, but be prefer to present, in the section below, the word cloud that is more illustrative. 

# Basic plots

First, we have the following word clouds for the 3 respective files (blogs, news, tweets).

```{r echo=FALSE, cache=TRUE, warning=FALSE}
pal2 <- brewer.pal(8,"Dark2")

wordcloud(freq_blogs$Word,freq_blogs$Frequency, scale=c(8,.2),min.freq=2,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

wordcloud(freq_news$Word,freq_news$Frequency, scale=c(8,.2),min.freq=2,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

wordcloud(freq_tweets$Word,freq_tweets$Frequency, scale=c(8,.2),min.freq=2,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)



```

Also, for the we have chosen to figure out which n-grams are the most frequent in our data (n<=3). We have got the following histograms for the blogs file (respectively for 1, 2 and 3-grams): 

```{r echo=FALSE, warning=FALSE, message=FALSE}
##################################################################
### chunk number 2: Understand frequencies of words and word pairs
##################################################################
require(stylo)
if(TRUE) {
## 1-grams
gam1_blogs<-make.ngrams(txt.to.words(sample_blogs), ngram.size=1)
gam1_news<-make.ngrams(txt.to.words(sample_news), ngram.size=1)
#gam1_tweets<-make.ngrams(txt.to.words(sample_tweets), ngram.size=1)
## 2-grams 
gam2_blogs<-make.ngrams(txt.to.words(sample_blogs), ngram.size=2)
gam2_news<-make.ngrams(txt.to.words(sample_news), ngram.size=2)
#gam2_tweets<-make.ngrams(txt.to.words(sample_tweets), ngram.size=2)
## 3-grams
gam3_blogs<-make.ngrams(txt.to.words(sample_blogs), ngram.size=3)
gam3_news<-make.ngrams(txt.to.words(sample_news), ngram.size=3)
#gam3_tweets<-make.ngrams(txt.to.words(sample_tweets), ngram.size=3)

}

table_gam1_blogs = as.data.frame(table(gam1_blogs))
table_gam1_blogs = table_gam1_blogs[order(table_gam1_blogs$Freq, decreasing = T),]

table_gam2_blogs = as.data.frame(table(gam2_blogs))
table_gam2_blogs = table_gam2_blogs[order(table_gam2_blogs$Freq, decreasing = T),]


table_gam3_blogs = as.data.frame(table(gam3_blogs))
table_gam3_blogs = table_gam3_blogs[order(table_gam3_blogs$Freq, decreasing = T),]


library(plotly)
p <- plot_ly(
  x = table_gam1_blogs[1:10,1],
  y = table_gam1_blogs[1:10,2],
  name = "SF Zoo",
  type = "bar",
  filename="r-docs/simple-bar"
)
p

p <- plot_ly(
  x = table_gam2_blogs[1:10,1],
  y = table_gam2_blogs[1:10,2],
  name = "SF Zoo",
  type = "bar",
  filename="r-docs/simple-bar"
)
p

p <- plot_ly(
  x = table_gam3_blogs[1:10,1],
  y = table_gam3_blogs[1:10,2],
  name = "SF Zoo",
  type = "bar",
  filename="r-docs/simple-bar"
)
p
```

# To be continued...

Looking at the charts, we can se that we have a lot of adverbs and prepositions in the data. So in the next step, we will focus on cleaning again data, before write a model that will suggest to the user the next word, based on the frequencies stored with n-grams. 
