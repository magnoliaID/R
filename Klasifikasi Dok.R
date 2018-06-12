library(caret)
library(tm)
library(SnowballC)
library(arm)
# Training data.
cname <- file.path("/resources/data/doc")   
cname   
dir(cname)  
docs <- Corpus(DirSource(cname))   

summary(docs)
inspect(docs[2])
docs <- tm_map(docs, removePunctuation)  
docs <- tm_map(docs, removeNumbers)  
docs <- tm_map(docs, tolower)  
docs <- tm_map(docs, removeWords, c("dapat", "yang","adalah","untuk","dan"))  
corpus <- VCorpus(VectorSource(docs))

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))

# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(tdm)
train <- cbind(train, c(0, 1))
colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)
data
train
# Train.
fit <- train(y ~ ., data = train, method = 'bayesglm')

# Check accuracy on training.
predict(fit, newdata = train)
