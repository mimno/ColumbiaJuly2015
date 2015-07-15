library(dplyr)
library(mallet)
library(ggplot2)
library(reshape2)

n.topics <- 25 # this might need to be, and probably should be, adjusted based on the total number of questions whose responses are being included as documents
num_long_words = 25 # the number of words to use for "long" topic labels
num.runs = 1 # number of total topic model solution runs

	col.types <- rep("factor", 44)

	## Identify character columns: change this to the correct fields
	col.types[1] <- "character"
	col.types[2] <- "character"


data <- read.csv("FILENAME.csv", colClasses=col.types, quote="\"\"")

mallet.instances <- mallet.import(documents$id, documents$text, "STOPLIST FILE", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

all.topics <- matrix(nrow = n.topics * num.runs, ncol = num_long_words)
all.topic.labels <- character(n.topics * num.runs)

## Create a topic trainer object.
topic.model <- MalletLDA(num.topics=n.topics)

## Load our documents. We could also pass in the filename of a 
##  saved instance list file that we build from the command-line tools.
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
#vocabulary <- topic.model$getVocabulary()
#word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations, 
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model.
##  We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(1000)
doc.topics <- 0.1 * mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- 0.1 * mallet.topic.words(topic.model, smoothed=T, normalized=T)

for (sample.iter in 1:9) {
	topic.model$train(100)
	doc.topics <- doc.topics + 0.1 * mallet.doc.topics(topic.model, smoothed=T, normalized=T)
	topic.words <- topic.words + 0.1 * mallet.topic.words(topic.model, smoothed=T, normalized=T)
}

#topics.labels <- gsub("\\W", "_", mallet.topic.labels(topic.model, topic.words, 5))
topics.labels <- vector(length=n.topics)
for (topic.i in 1:n.topics) 
{
	topics.labels[topic.i] <- gsub("\\W", "_", paste(as.vector(mallet.top.words(topic.model, topic.words[topic.i,], 5)[,1]), collapse="_"))
}
#topics.long.labels <- mallet.topic.labels(topic.model, topic.words, num.top.words=50)
topics.long.labels <- vector(length=n.topics)
for (topic.i in 1:n.topics)
{
	topics.long.labels[topic.i] <- paste(as.vector(mallet.top.words(topic.model, topic.words[topic.i,], 25)[,1]), collapse = " ")
}

#library("sets") # NB: masks %>% from dplyr

topic.sets <- matrix(nrow=n.topics, ncol=num_long_words)
for (topic.i in 1:n.topics)
{
	topic.sets[topic.i,] <- as.vector(mallet.top.words(topic.model, topic.words[topic.i,], 25)[,1])
}


doc.topics.frame <- data.frame(doc.topics)
#names(doc.topics.frame) <- paste("Topic", 1:n.topics, sep="")
names(doc.topics.frame) <- topics.labels
docs.and.topics <- cbind(documents, doc.topics.frame)

## CHANGE 6:30 to be the topic columns

wide.data <- dcast(melted.docs.and.topics %>% group_by(VARIABLES) %>% summarise(value = mean(log(value))), VARIABLES ~ topic)
wide.data[,6:30] <- wide.data[,6:30] - (rowSums(wide.data[,6:30]) / n.topics)
formula.string <- paste(names(wide.data)[6:30][mask], collapse=" + ")
user.topics <- as.matrix(wide.data[,6:30])

summary(glm(paste("PREDICTED VARIABLE", " ~ ", formula.string), data=wide.data, family=binomial()))
