library(tm)
library(wordcloud)
library(memoise)
library(corpus)
LegoData <- read.csv('LEGO2019.csv', stringsAsFactors = FALSE)
all_themes <- unique(LegoData$Theme)
getTermMatrix <- memoise(function(sub_legodata, theme) {
  sep_theme <- LegoData[sub_legodata$Theme == theme , 5]
  theme_corpus = Corpus(VectorSource(sep_theme))
  theme_corpus = tm_map(theme_corpus, content_transformer(tolower))
  theme_corpus = tm_map(theme_corpus, removeNumbers)
  theme_corpus = tm_map(theme_corpus, removePunctuation)
  theme_corpus = tm_map(theme_corpus, removeWords, stopwords())
  name_cluster = TermDocumentMatrix(theme_corpus,
                             control = list(minWordLength = 1))
  c = as.matrix(name_cluster)
  sort(rowSums(c), decreasing = TRUE)
})
