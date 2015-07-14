library(mallet)
library(ggplot2)

n.topics <- 30

documents <- read.table("yelp_phoenix_10k.txt", colClasses=c("character", "character", "character"), col.names=c("id", "tags", "text"), sep="\t", quote="")

mallet.instances <- mallet.import(documents$id, documents$text, "stoplist.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

## Create a topic trainer object.
topic.model <- MalletLDA(num.topics=n.topics)

topic.model$loadDocuments(mallet.instances)

topic.model$setAlphaOptimization(20, 50)

## Now train a model.
##  We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(500)

## NEW: run through a few iterations where we pick the best topic for each token, 
##  rather than sampling from the posterior distribution.
topic.model$maximize(50)

doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

onestar.topic.words <- mallet.subset.topic.words(topic.model, grepl("1_star", documents$tags), smoothed=T, normalized=T)
fivestar.topic.words <- mallet.subset.topic.words(topic.model, grepl("5_star", documents$tags), smoothed=T, normalized=T)

mallet.top.words(topic.model, onestar.topic.words[10,], 30)
mallet.top.words(topic.model, fivestar.topic.words[10,], 30)

topic.labels <- mallet.topic.labels(topic.model, topic.words, 3)
plot(mallet.topic.hclust(doc.topics, topic.words, 0.3), labels=topic.labels)

one.five.sums <- rbind(
data.frame(Topic = topic.labels, Docs = colSums(doc.topics[grepl("5_star", documents$tags),]), Tag="5_star"),
data.frame(Topic = topic.labels, Docs = colSums(doc.topics[grepl("1_star", documents$tags),]), Tag="1_star"))

p <- ggplot(one.five.sums, aes(Topic, Docs, fill=Tag))
p + geom_bar(stat="identity", position="dodge") + coord_flip()
