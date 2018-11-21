#later
#filter segregated tables
#posUser <- subset(fullVac, user_grouping=="Positive")
#negUser <- subset(fullVac, user_grouping=="Negative")
#neuUser <- subset(fullVac, user_grouping=="Neutral")


#load grouping data and packages
#install.packages("quanteda")
#install.packages('readtext')
library(quanteda)
library(stringr)
library(plyr)
library(dplyr)
require(readtext)


##################### LOADING
fullVac <- readtext("~/Desktop/Fall 2018/Text Scrap/Anti Vac/groupings.csv")


##################### CLEANING
#remove @s from tweet text
#try test first
test <- data.frame(fullVac$text[180:190])
test$abc <-gsub("\\@[a-z,A-Z,0-9,_]+","",test$fullVac.text.180.190.)

#remove text column
fullVac$text <- NULL

#remove @s from fullvac
fullVac$tweet <-gsub("\\@[a-z,A-Z,0-9,_]+","",fullVac$tweet)

#remove strange characters
fullVac$tweet <-gsub("[¤º-»«Ã¢ùâ¬Å¥¡Â¿°£·©äôË¦¼¹¸±???ð\u201E\u201F\u0097\u0083\u0082\u0080\u0081\u0090\u0095\u009f\u0098\u008d\u008b\u0089\u0087\u008a¦??.]+",
                     "",fullVac$tweet)
#remove links
fullVac$tweet <-gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)",
                     "",fullVac$tweet)

#lower all text
fullVac$tweet <-tolower(fullVac$tweet)

#remove last rows
fullVac <- fullVac[1:1177, ]


##################### CORPUS
#create corpus
fullVac_corpus <- corpus(fullVac$tweet)

#explore data
summary(fullVac_corpus)
texts(fullVac_corpus)[5]


##################### DICTIONARIES
#create dictionary
dict1 <- dictionary(list(conspiracy = c('betray', 'big pharma', 'bigpharma', 'CDC', 
                                        'consider', 'contaminated', 'conspiracy', 
                                        'damage', 'damaged', 'danger', 'dangerous', 
                                        'dishonesty', 'drug', 'government', 'justice', 
                                        'lies', 'misinformation', 'outbreak', 'pharma', 
                                        'reaction', 'scam', 'scandal', 'seeking', 'sheep', 
                                        'side effect', 'skepticism', 'stop lies', 'trust', 
                                        'unproven', 'untested', 'zealot'),
                         death = c('death', 'dies', 'dying', 'grief', 'grieving'),
                         fear = c('babies', 'baby', 'family', 'fear', 'mainstream media', 
                                  'media', 'news'),
                         healthPositive = c('confidence', 'easy', 'herd immunity', 'immunity', 
                                             'immunization', 'immunize', 'lower', 
                                             'lower your risk', 'prevention', 'protect', 
                                            'protection',  'quick', 'spread', 'stop', 'talk to', 
                                            'vaccinate', 'vaccinated', 'works'),
                         healthRisks = c('adverse', 'autism', 'disease', 'epilepsy', 'increases risk',
                                         'induce', 'ineffective', 'infection', 'malaria', 'measles', 
                                         'MMR', 'polio', 'risk', 'risks', 'shingles', 'toxic', 
                                         'vaccine injury', 'virus'),
                         promotion = c('available', 'awareness', 'book', 'effective', 
                                       'free flu shot', 'get', 'get vaccine', 'get your vaccine', 
                                       'raise', 'shot clinic'), 
                         choice = c('available', 'choice', 'bigpharma', 'CDC', 'consider', 
                                    'contaminated', 'damage', 'damaged', 'danger', 'dangerous', 
                                    'dishonesty', 'drug', 'government', 'justice', 'lies', 
                                    'misinformation', 'outbreak', 'pharma', 'reaction', 'scam', 
                                    'scandal', 'seeking', 'sheep', 'side effect', 'skepticism', 
                                    'stop lies', 'unproven', 'untested', 'zealot', 'choices', 'get'), 
                         communication = c('book', 'easy', 'quick', 'spread', 'stop', 'talk to', 'speak to', 'speak'), 
                         family = c('babies', 'baby', 'family'), 
                         government = c('big pharma', 'republican', 'democrat', 'law', 'evil', 'ploy'),
                         health = c('autism', 'death', 'dies', 'disease', 'dying', 'epilepsy', 
                                    'free flu shot', 'get vaccine', 'get your vaccine', 'herd immunity', 
                                    'immunity', 'immunization', 'immunize', 'induce', 'infection', 'lifelong', 
                                    'malaria', 'measles', 'MMR', 'polio', 'research', 'shingles', 'shot clinic',
                                    'vaccinate', 'vaccinated', 'vaccine injury', 'virus'),
                         media = c('mainstream media', 'media', 'news'),
                         trust = c('adverse', 'awareness', 'betray', 'confidence', 'conspiracy', 'effective', 
                                   'fear', 'grief', 'grieving', 'increases risk', 'ineffective', 'lower', 
                                   'lower your risk', 'prevention', 'promotion', 'protect', 'protection', 
                                   'raise', 'recommend', 'risk', 'risks', 'toxic', 'trust')))


##################### MERGE CORPSE & DICT
#attach dict to corpus
head(dfm(fullVac_corpus, dictionary = dict1))

#create dfm object
dictCount <- dfm(fullVac_corpus, dictionary = dict1)

#convert dfm to dataframe, column to doc_id
dictCount.df <- data.frame(dictCount)
colnames(dictCount.df)[colnames(dictCount.df)=="document"] <- "doc_id"

#change document IDs
dictCount.df$doc_id <-gsub("text","tweet",dictCount.df$doc_id)
fullVac$doc_id <-gsub("groupings.csv.","tweet",fullVac$doc_id)

#MERGE
dictVac <- join(fullVac,dictCount.df,by="doc_id",type="inner")

