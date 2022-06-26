library(quanteda)
library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(reshape2)
library(stopwords)
library(utf8)
library(stringr)
library(tokenizers.bpe)
library(topicmodels)
library(tidyverse)
library(tidytext)
library(glue)
library(syuzhet)



dracula_beg<- readLines("https://www.gutenberg.org/cache/epub/345/pg345.txt", encoding = "UTF-8")

length(dracula_beg) # Number of entries that our book has
summary(dracula_beg) # with this instruction, it is shown the type of data that the file contains
dracula_beg[!utf8_valid(dracula_beg)] # we see if the text has the right encoding
grep(pattern = "***", dracula_beg, fixed = TRUE) 
prueba_NFC <- utf8_normalize(dracula_beg)
sum(prueba_NFC != dracula_beg) # check character normalization. 0 means is ok and the text is in NFC (normalized composed form)

#getTransformations()
head(dracula_beg) # to see the first lines of the document in order to remove the information not relevant for the analysis
dracula_beg= dracula_beg[-c(1:107)] # we delete the lines unnecessary for the analysis
head(dracula_beg)
tail(dracula_beg,438) # we extract the last lines of the file that are not relevant for the analysis
dracula_beg= dracula_beg[-c(15313:15751)] 
dracula_beg# at this moment, the file should contain only the relevant data, and the cleaning tasks can begin



#CLEANING DATA

dracula_beg<- gsub("\\n", " ", dracula_beg) #deleting new lines
dracula_beg<- gsub("[ ]{2,}", " ", dracula_beg) #deleting double spaces
dracula_beg<- gsub("\\r", " ", dracula_beg) #deleting carrier return
dracula_beg<- gsub("\\--", " ", dracula_beg) #deleting double -
dracula_beg<- gsub("_(.*?)_", "", dracula_beg) #the text contains comments between _ _
dracula_beg<- gsub("\\(,", "\\(", dracula_beg) #deleting commas between parentheses
dracula_beg<- gsub("\\*", "", dracula_beg) # deleting *
dracula_beg<- gsub(",", "", dracula_beg) #deleting punctuation symbols
dracula_beg<- gsub(";", "", dracula_beg)
dracula_beg<- gsub("!", "", dracula_beg)
dracula_beg<- gsub(":", "", dracula_beg)
dracula_beg<- gsub("'", "", dracula_beg)
dracula_beg<- gsub("\\?", "", dracula_beg)
dracula_beg<-tolower(dracula_beg) #all text to lower

head(dracula_beg,200)

dracula_clean <- paste(dracula_beg, collapse = ' ')
length(dracula_clean) # Number of entries that our book has
#stopwords("english") # search for relevance of the words
head(dracula_clean)


#SOME NUMBERS


#spacy_install()
#spacy_finalize()
#if(!require("spacyr")) install.packages("spacyr")
#spacy_install(prompt = FALSE) # install acanconda 3.x for windows
#spacy_download_langmodel(model = 'en_core_web_sm')
#spacy_initialize(model = "en_core_web_sm")

# Sentence Analysis 

#Using udpipes

model_file <- 'english-lines-ud-2.5-191206.udpipe'
if(!file.exists(model_file)){
  model <- udpipe_download_model(language = "english-lines") 
  udmodel_es <- udpipe_load_model(file = model$file_model)
}else{
  udmodel_es <- udpipe_load_model(file = model_file)
}

tic <- Sys.time()
anno <- udpipe_annotate(udmodel_es,
                        x = dracula_beg[1:100],
                        parallel.cores = 10 #Check your system!!
)
df <- as.data.frame(anno)
Sys.time()-tic
cat(anno$conllu, file = "udpipes_es_Dracula.conllu")

library(kableExtra) #
kable_styling(kable(df[1:20, c(5:14)]), #The first cols UNSHOWN are doc_id, paragraph_id, sentence_id and sentence
              font_size = 15
)


#Count the word of the book
sapply(gregexpr("\\W+", dracula_clean), length) + 1

#develop an dfm matrix to obtain the frequence of the words
text_lines <- unlist(dracula_beg)
text_lines
corpus_lines <- corpus(text_lines)
corpus_lines
dataframe_drac_1 = convert(corpus_lines, to = "data.frame")

#creamos la matriz document-feature

dfm_lines <- dfm(tokens(text_lines),)


topfeatures(dfm_lines)
#It can be seen that the words with more repetition are conectors, but for the analysis it is not relevant. Because of this, the stopwords must be deleted

topfeatures(dfm_lines, decreasing = FALSE)
dfm_lines_WOSTP <- dfm_remove(dfm_lines, stopwords("en"))
topfeatures(dfm_lines_WOSTP)
# Comparison without puntctuation symbols
dfm_noPunct <- dfm(tokens(text_lines, remove_punct = TRUE))
dfm_noPunct_noSW <- dfm_remove(dfm_noPunct, stopwords("en"))
topfeatures(dfm_noPunct_noSW)
barplot(topfeatures(dfm_noPunct_noSW), main = "Most Repeted Words", col = "blue")


# Analysis of the sentiments in the novel

dracula_bing <- dracula_beg
dracula_bing_clean <- glue(read_file(dracula_bing)) # read the new file
tokens <- tibble(text =dracula_bing_clean) %>% unnest_tokens(word, text) #tokenize

sentiments <- data_frame()
sentiments <- tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the number of positive & negative words
  spread(sentiment, n, fill = 0) %>%  # make data wide
  mutate(sentiment = positive - negative) # positive words - negative words

sentiments

# Second analysis with syuzhet 

asyuzhet = get_nrc_sentiment(dracula_clean)

head(asyuzhet)
td_new <- data.frame(t(asyuzhet[c(1,3,4,6,9)])) #transpose the data selecting only bad feelings
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)

quickplot(sentiment, data=td_new, weight=count, geom="bar", fill=sentiment, ylab="count") + ggtitle("Dracula's sentiments")

#Count of words for every sentiment as percentage
barplot(
  sort(colSums(prop.table(asyuzhet[c(1,3,4,6,9)]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Dracula's novel", xlab="Percentage"
)