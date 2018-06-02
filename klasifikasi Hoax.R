library(caret)
library(tm)
library(SnowballC)
library(arm)
# Training data.
data <- c('Nasa : ada belasan ribu Satelit di angkasa',
          'Bumi berbentuk bulat ',
          'Antartika adalah wilayah kecil',
          'Amstrong adalah manusia pertama yang mendarat di bulan',
          'Roket mampu menembus atmosfer bumi',
          'Game PUBG merupakan game balapan',
          'Mobile Legend adalah game dengan grafik terbaik',
          'Manusia mampu menciptakan mesin waktu',
          'Kucing terlahir dengan warna pink',
          'Burung merpati adalah hewan mamalia',
          'Tidak ada satupun satelit di angkasa',
          'Bentuk bumi adalah datar',
          'Antartika adalah wilayah es besar yang mengelilingi bumi',
          'Tidak ada manusia yang mendarat di bulan',
          'Roket tidak mampu menembus atmosfer bumi',
          'PUBG merupakan game survival',
          'Grafik mobile legend jelek',
          'Manusia tidak bisa membuat mesin waktu',
          'kucing terlahir dengan warna coklat,hitam,putih,oranye dan kombinasi',
          'Burung merpati adalah hewan aves')
corpus <- VCorpus(VectorSource(data))

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

# Test data.
data2 <- c('Peneliti buktikan tidak ada satupun satelit di angkasa',
           'Bentuk bumi yang kita tempati adalah datar',
           'Antartika wilayah es yang mengelilingi bumi',
           'Manusia tidak mungkin mendarat di bulan',
           'Roket tidak akan mampu menembus atmosfer bumi',
           'PUBG adalah game survival terlaris',
           'Game mobile legend memiliki grafik jelek',
           'Tidak mungkin mesin waktu dapat dibuat',
           'seekor kucing persia berwarnan hitam menjadi viral',
           'Burung merupakan kelompok aves')
corpus <- VCorpus(VectorSource(data2))
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)

# Check accuracy on test.
predict(fit, newdata = test)

