## installing all packages reuired
install.packages("tm")
install.packages("topicmodels")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("pals")
install.packages("SnowballC")
install.packages("lda")
install.packages("ldatuning")
install.packages("kableExtra")
install.packages("DT")
install.packages("flextable")
# install klippy for copy-to-clipboard button in code chunks
install.packages("remotes")
remotes::install_github("rlesur/klippy")
remotes::install_github("rlesur/klippy")
install.packages("corpus")
install.packages("rio")
install.packages("moments")
install.packages("readxl")
install.packages("tidyverse")
install.packages("igraph")
install.packages("ggraph")
install.packages("networkD3")
install.packages("readr")
install.packages("netCoin")
install.packages("text2vec")
## checking the installed packages

library(knitr) 
library(kableExtra) 
library(DT)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(flextable)
# activate klippy for copy-to-clipboard button
klippy::klippy()
library(rio)
library(moments)
library(corpus)
library(readxl)
library(networkD3)
library(ggraph)
library(igraph)
library(readr)
library(netCoin)
library(text2vec)
library(tidyverse)

# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation

## importing the Excel file Mark_Lombordi_study_citation_summary

Mark=read_excel("C:/Users/HP/Downloads/Mark Lombardi study- citation summary.xlsx")
Mark

## separating the title column

df_title = data.frame(doc_id=row.names(Mark), text= Mark$Title)
df_title

##creating a corpus object for the title column

Corpus = Corpus(DataframeSource(df_title))
Corpus

## filtering the data, stopwords is not used as we dont have many of them, the only repeating words we have are
## Information and Visualization which we need in our analysis>

processedCorpus <- tm_map(Corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")

## setting the minimum frequency
minimumFrequency <- 3
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
DTM
## set the dimension 
dim(DTM)

##converting dataframe into list
raw.sum = apply(DTM, 1, FUN = sum)
DTM= DTM[raw.sum!=0,]

##to create a topic modeling image, not allowing me to set seq from 1

result <- ldatuning::FindTopicsNumber(
    DTM, topics = seq(from = 2, to = 239, by = 1),
    metrics = c("CaoJuan2009",  "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 239),
  verbose = TRUE
)
FindTopicsNumber_plot(result)

result1 <- ldatuning::FindTopicsNumber(
  DTM, topics = seq(from = 2, to = 60, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 239),
  verbose = TRUE
)
FindTopicsNumber_plot(result1)

result2 <- ldatuning::FindTopicsNumber(
  DTM, topics = seq(from = 61, to = 121, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 239),
  verbose = TRUE
)
FindTopicsNumber_plot(result2)

result3 <- ldatuning::FindTopicsNumber(
  DTM, topics = seq(from = 122, to = 182, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 239),
  verbose = TRUE
)
FindTopicsNumber_plot(result3)

result4 <- ldatuning::FindTopicsNumber(
  DTM, topics = seq(from = 183, to = 239, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 239),
  verbose = TRUE
)
FindTopicsNumber_plot(result4)
##no of topics
K <- 60
## creating a LDA model with gibbs sampling 
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))
tmResult <- posterior(topicModel)
attributes(tmResult)

##length of the vocabulary
nTerms(DTM)

## to know of probability distribution
beta= tmResult$terms
dim(beta)

## all rows are made sum to 1
rowSums(beta)

## distribution over all the topics in dataset
nDocs(DTM)
theta <- tmResult$topics 
dim(theta) 
rowSums(theta)[1:10]

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

exampleTermData <- terms(topicModel, 10)
exampleTermData[, 1:8]

## selecting a random topic of interest in place of 'Inform' highlights the selected topic

topicToViz <- grep('inform', topicNames)[1] 
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)


# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]

# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

## creating a network graph.

x= networkD3::simpleNetwork(df_title)
x


















































                  
