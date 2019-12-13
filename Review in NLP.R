#Natural Language Processing

#Importing the dataset
dataset_original = read.delim('C:\\Users\\satya\\Desktop\\TM\\Restaurant_Reviews.tsv' , quote = '', stringsAsFactors = FALSE)

#Cleaning the texts
install.packages('tm')
install.packages('SnowballC')
library(tm)
library(SnowballC)

#New dataset which consist of 1000 reviews.
corpus = VCorpus(VectorSource(dataset_original$Review))

#Convert the corpus into lowercase.
#Content_tranformer is a function used to lowercase the review section.
corpus = tm_map(corpus, content_transformer(tolower))

#Removes the numbers from the corpus
corpus = tm_map(corpus, content_transformer(removeNumbers))

#Remove punctuation methods
corpus = tm_map(corpus, content_transformer(removePunctuation))

#Removing non-relevant words in the corpus using stopwords.
corpus = tm_map(corpus, removeWords, stopwords())

#Perform stemmization
corpus = tm_map(corpus, stemDocument)

#Remove spaces
corpus = tm_map(corpus, stripWhitespace)

#Access the first element in an corpus
as.character(corpus[[1]])



#Creating the bag of words Model
#SpareMatrix contains alot of zeroes. We create a matrix wherein we have independent variables. 
##These indepedent varibaes are nothing but words in each reviews of the corpus. 
##We will count how many times each word appears in all the reviews of the corpus.
dtm = DocumentTermMatrix(corpus)
dtm

#Now we filter all the words that appear only once.
dtm = removeSparseTerms(dtm, 0.99)
dtm
## Collapse matrix by summing over column - this gets total counts (over all docs)
freq<- colSums(as.matrix(dtm))
##Here Length should be showing total number of terms
length(freq)

## create sort order (asc)
ord1 <- order(freq, decreasing = TRUE)

##check the most frequent words
freq [head(ord1)]

#inspect least frequent words
freq [ tail(ord1)]

## find frequency of  a word
findFreqTerms(dtm, lowfreq =50)

#Create a dataset
dataset = as.data.frame(as.matrix(dtm))

## Histogram
wf= data.frame(term =names(freq), occurances = freq)
install.packages('ggplot2')
library('ggplot2')
p<- ggplot (subset(wf,freq >50), aes(term, occurances ))
p<- p+ geom_bar(stat = "identity")
p<- p+theme (axis.text.x = element_text(angle = 45, hjust = 1))
p

# wordCloud
install.packages('wordcloud')
library(wordcloud)
set.seed(123)

#limits words by the frequency 
wordcloud(names(freq),freq, min.freq = 20)
wordcloud(names(freq),freq, min.freq = 20,colors = brewer.pal(6,"Dark2"))

#Adding the dependent varibale to the new dataset.
dataset$Liked = dataset_original$Liked


# Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-97],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-97])


# Making the Confusion Matrix
cm = table(test_set[, 97], y_pred)
cm

