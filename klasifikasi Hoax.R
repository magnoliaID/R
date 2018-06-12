library(caret)
library(tm)
library(SnowballC)
library(arm)
# Training data.
data <- c('Viral Nomor IMEI Ponsel Bisa Disadap, Kemkominfo: Itu Hoax',
          'Kemkominfo Siapkan Satelit untuk Perkuat Sinyal Daerah Terpencil',
          'Kemkominfo Uji Algoritma Bigo untuk Berburu Konten Pornografi',
          'Mesin Pengais Konten Negatif Kemkominfo Batasi Gerak Teroris di Dunia Maya',
          'Kemkominfo Gandeng Facebook Cs Babat Ratusan Akun Radikalisme',
          'INASGOG Gandeng Kemkominfo Amankan Asian Games 2018 dari Hacker',
          'Pemerintah Terus Berupaya Antisipasi Penyebaran Hoax',
          'Kemkominfo: Satu NIK Bisa Registrasikan Banyak Nomor',
          'Kemkominfo Siap Tangkis Lebih Banyak Konten Porno di Internet',
          'Kemkominfo: Penyebar Konten Porno Running Text Solo Tertangkap',
          'Kemkominfo sadap ponsel lewat IMEI',
          'Sinyal satelit tidak menjangkau daerah terpencil',
          'Bigo adalah situs porno',
          'Teroris terbantu oleh konten kemkomifo',
          'Kemkominfo bantu facebook membuat akun radikal',
          'Asian games di hack',
          'Pemerintah menyebarkan berita hoax',
          'Satu NIK hanya bisa registrasi sekali',
          'Banyak konten porno dilegalkan oleh kemkominfo',
          'Penyebar konten porno dibebaskan')
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
data2 <- c('Sadap ponsel lewat IMEI adalah hoax',
           'Sinyal satelit tidak akan mencapai wilayah terpencil',
           'Algoritma bigo diuji oleh kemkominfo',
           'Teroris berterimakasih pada kemkominfo',
           'Kemkominfo dan facebook berantas akun radikal',
           'Asian games diserang hacker',
           'Pemerintah akan antisipasi berita hoax',
           'satu NIK sekali registrasi',
           'Kemkominfo tangkis konten porno',
           'Kemkominfo bebaskan konten porno')
corpus <- VCorpus(VectorSource(data2))
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)

# Check accuracy on test.
predict(fit, newdata = test)

