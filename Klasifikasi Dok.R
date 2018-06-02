library(tm)
library(plyr)
library(class)
library(pdftools)
libs=c("tm","plyr","class","pdftools")
lapply(libs,require,character.only = T)
options(stringsAsFactors= F)

#referensi https://github.com/sureshgorakala/Text-Mining/blob/master/R%20Text%20classfication%20using%20CSV%20files
#hasil = error , kurang paham . pendalaman ilmu kurang mencukupi

cat = c("246-PK-Pdt-17-tl.pdf",
        "1410-K-Pdt-17-YG-tl-Tgt.pdf",
        "1443.k.pdt.17.tlk.agm.rk.pdf",
        "1455_K.Pdt.2017.Kbl_KPPBAR_KP3.pdf",
        "2183_K-PDT-2017_MINUT.pdf")
pathname = "D:/Data kuliah/SMT 6/Senin/Informasi retrieval/Text Mining/Doc_class/data1"

cleanCorpus <-function(corpus) {
  corpus.tmp = tm_map(corpus,removePunctuation)
  corpus.tmp = tm_map(corpus.tmp,stripWhitespace)
  corpus.tmp = tm_map(corpus.tmp,tolower)
  corpus.tmp = tm_map(corpus.tmp,removeWords,stopwords("english"))
  corpus.tmp = tm_map(corpus.tmp,stemDocument)
  return(corpus.tmp)
}

generateTDM <- function(cate,path) {
  s.path =  sprintf("%s/%s",path,cate)
  pdf = readPDF(s.path)
  s.cor = Corpus(DataframeSource(pdf))
  s.cor.cl = cleanCorpus(s.cor)
  s.tdm= TermDocumentMatrix(s.cor.cl)
  s.tdm = removeSparseTerms(s.tdm,0.7)
  result <-list(name= cate,tdm = s.tdm)
}

tdm = lapply(cat,generateTDM,path = pathname)

# attach name
bindCategoryTDM <- function(tdm) {
  s.mat = t(data.matrix(tdm[["tdm"]]))
  s.df = as.data.frame(s.mat,stringsAsFactors = F)
  s.df = cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetCat"
  return(s.df)
  
}

catTDM = lapply(tdm,bindCategoryTDM)

#Stack 
tdm.stack = do.call(rbind.fill,catTDM)
tdm.stack[is.na(tdm.stack)] = 0

#holdout
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) * 0.7))
text.idx = (1:nrow(tdm.stack))[-train.idx]


#model
tdm.cat = tdm.stack[,"targetCat"]
tdm.stack.nl = tdm.stack[,!colnames(tdm.stack) %in% "targetCat"]
knn.pred = knn(tdm.stack.nl[train.idx,],tdm.stack.nl[text.idx,],tdm.cat[train.idx])


#accuracy
conf.mat = table("predictions" = knn.pred,Actual = tdm.cat[text.idx])
accuracy = sum(diag(conf.mat)/length(text.idx) *100)
accuracy
conf.mat 


---------------------------------------------------------------------
  inspect(stem) - display the content in the Corpus
corpus <- Corpus(DataframeSource(csvpath)) - read all the documents in csv
findFreqTerms(tdm, 300) - frequency of terms

