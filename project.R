# Andrew Fairless
# December 2015

# The project below was created to produce word clouds from text messages that
# were forwarded from cell phone to email with the app 'txtForward'.

# These emails were exported from Mozilla Thunderbird into a '.csv' file.
# To protect privacy, the text message contents in the example 'messages.csv'
# file has been replaced with lines from Shakespeare's 'The Comedy of Errors'.
# The file 'messages.csv' also contains fake dates/times and a fake email address.

# The functions below were created to retain (some) punctuation (for, e.g., 
# emoticons) and manual control over some 'text cleaning' processes, instead of 
# using only the functions provided in the library 'tm'.

# imports:  text messages from 'messages.csv' in current working directory
# exports:  'png' and 'pdf' files of word clouds constructed from the text messages
# dependencies:  libraries 'tm' and 'wordcloud'


# ----------------------------
# import and pre-process data


texts <- read.csv("messages.csv", header = F, stringsAsFactors = F)

texts[grepl("You sent a text message", texts[ , 6]), 1] <- c("sent")
texts[grepl("You received a text message", texts[ , 6]), 1] <- c("received")
texts[ , 2] = as.POSIXct(texts[ , 4], format = "%m/%d/%Y %H:%M")
texts[ , 3] = as.Date(substr(texts[ , 4], start = 1, stop = 10), format = "%m/%d/%Y")

# remove all extraneous information from text column; retain content of texts
for (iter in 1:dim(texts)[1]) {
     # '9371' is the last 4 digits of the phone number
     texts[iter, 6] <- gsub(".*9371)\n", "", texts[iter, 6])
}

colnames(texts) <- c("sentreceived", "datetime", "date", "textdatetime", "empty", "texts")

senttexts <- texts[texts$sentreceived == "sent", 6]
receivedtexts <- texts[texts$sentreceived == "received", 6]


# ----------------------------
# functions


# takes a vector of type 'character' (i.e., strings) and returns a vector of 
# numbers of the words/tokens contained within original vector
corpfreqprep <- function(vectoroftexts, removepunct = TRUE) {
     corpus <- Corpus(VectorSource(vectoroftexts))
     corpus <- tm_map(corpus, content_transformer(removeNumbers))
     corpus <- tm_map(corpus, content_transformer(stripWhitespace))
     if (removepunct) corpus <- tm_map(corpus, content_transformer(removePunctuation))
     corpfreq <- colSums(as.matrix(DocumentTermMatrix(corpus)))
     corpfreq <- sort(corpfreq, decreasing = TRUE)
     return(corpfreq)
}


# takes a vector of strings/texts ('textsvector') and a vector of words to be
# counted in 'textsvector' ('wordstofind') and returns a vector of the 
# counts/frequencies
# this function was written to find multi-word phrases that are not detected
# after a text has been tokenized, but it can search for single words, too
findmultiwords <- function(textsvector, wordstofind) {
     wordsfreqs <- 0
     wordsfreqs[1:length(wordstofind)] <- 0
     # for each word/word-phrase to find
     for (iter in 1:length(wordstofind)) {
          tftable <- table(grepl(wordstofind[iter], textsvector))
          names(wordsfreqs)[iter] <- wordstofind[iter]
          for (iter2 in 1:length(tftable)) {
               if (names(tftable)[iter2] == TRUE) wordsfreqs[iter] <- tftable[iter2]
          }
     }
     return(wordsfreqs)
}


# takes a vector of strings/texts ('textsvector'), a vector of words to be
# counted in 'textsvector' ('wordstofind'), and a vector of existing word
# counts/frequencies ('corpfreq')
# words specified in 'wordstofind' are counted in 'textsvector'
# if counted word is already in 'corpfreq', its count calculated here replaces
# the existing count in 'corpfreq'; if it's not found, the count is added to 
# the end of 'corpfreq'
# returns vector of modified word counts/frequencies
replacecounts <- function(textsvector, corpfreq, wordstofind) {
     # for each word/word-phrase to find
     for (iter in 1:length(wordstofind)) {
          tftable <- table(grepl(wordstofind[iter], textsvector))
          for (iter2 in 1:length(tftable)) {
               # if the word in 'wordstofind' is found in 'textsvector'...
               if (names(tftable)[iter2] == TRUE) {
                    # ...and if it exactly matches any word in 'corpfreq'...
                    if (wordstofind[iter] %in% names(corpfreq) == TRUE) {
                         # ...replace the count/frequency in 'corpfreq' with the one calculated in this function
                         replaceindex <- which(names(corpfreq) == wordstofind[iter]) 
                         corpfreq[replaceindex] <- tftable[iter2]
                         # if the word in 'wordstofind' is found in 'textsvector'...    
                         # ...and if it is not found in 'corpfreq'...
                    } else {
                         #...add it to the end of 'corpfreq'
                         corpfreq <- append(corpfreq, tftable[iter2])
                         names(corpfreq)[length(corpfreq)] <- wordstofind[iter]
                    }
               }  
          }
     }
     return(corpfreq)
}


# takes a vector of numbers/frequencies of words ('corpfreq') and a vector of 
# words to be removed from 'corpfreq' ('wordstoremove') and returns 'corpfreq' 
# minus the removed words
removewords <- function(corpfreq, wordstoremove) {
     rmwordsindex <- NA
     rmwordsindex[1:length(rmwords)] <- NA
     for (iter in 1:length(rmwords)){
          temp <- which(names(corpfreq) == rmwords[iter])
          if (length(temp) == 1) rmwordsindex[iter] <- temp
     }
     rmwordsindex <- rmwordsindex[!is.na(rmwordsindex)]
     corpfreq <- corpfreq[-rmwordsindex]
     return(corpfreq)
}


# takes a vector of numbers/frequencies of words ('corpfreq') and a nested list
# of words to combine ('wordstocombine') in 'corpfreq'
# each element of 'wordstocombine' is a vector of words to be combined with each
# other
# combined words (e.g., "you", "you.", and "you,") have their frequencies 
# summed/totaled and redundant entries removed, leaving only a single entry in 
# 'corpfreq'
# the single, remaining, combined entry is named as the 1st word listed in the
# element of 'wordstocombine' (e.g., "you", because it is listed first)
# returns 'corpfreq' with the words' frequencies totaled and redundant entries 
# deleted
combinewords <- function(corpfreq, wordstocombine) {
     # for each element in list 'wordstocombine'
     for (iter in 1:length(wordstocombine)) {     # nested loop bad; should use 'lapply'
          wordindices <- NA
          wordindices[1:length(wordstocombine[[iter]])] <- NA
          # for each word in each element of the list 'wordstocombine'
          for (iter2 in 1:length(wordstocombine[[iter]])) {
               # wordfound <- any(grep(wordstocombine[[iter]][iter2], names(corpfreq), fixed = TRUE))
               wordfound <- wordstocombine[[iter]][iter2] %in% names(corpfreq)
               # if the word appears anywhere in 'corpfreq'...
               if (wordfound == TRUE) {
                    # ...save its index to 'wordindices'
                    wordindices[iter2] <- which(names(corpfreq) == wordstocombine[[iter]][iter2])
               }
          }
          # if all elements of 'wordindices' are 'NA's, skip code block below
          # avoids error if none of words in 'wordstocombine[[iter]]' are found in 'corpfreq'
          if (any(!is.na(wordindices))) {    
               wordindices <- wordindices[!is.na(wordindices)]   # remove 'NA's
               names(corpfreq)[wordindices] <- wordstocombine[[iter]][1]
               corpfreq[wordindices[1]] <- sum(corpfreq[wordindices])
               if (length(wordindices) > 1) corpfreq <- corpfreq[-wordindices[-1]]
          }
     }
     return(corpfreq)
}


# takes a vector of numbers/frequencies of words ('corpfreq') and a list of 
# word pairs ('changewords') wherein the count/frequency of the 2nd word in 
# each pair is subtracted from the count/frequency of the 1st word
countchange <- function(corpfreq, changewords) {
     for (iter in 1:length(changewords)) {
          # if both words are found in 'corpfreq'...
          if (changewords[[iter]][1] %in% names(corpfreq) && 
              changewords[[iter]][2] %in% names(corpfreq)) {
               wordindex1 <- which(names(corpfreq) == changewords[[iter]][1])
               wordindex2 <- which(names(corpfreq) == changewords[[iter]][2])
               # subtract the count of the 2nd word from the count of the 1st word
               corpfreq[wordindex1] <- corpfreq[wordindex1] - corpfreq[wordindex2]
          }
     }
     return(corpfreq)
}


mainfunction <- function(textsvector) {
     corpfreq <- corpfreqprep(textsvector, removepunct = TRUE)
     corpfreqpunct <- corpfreqprep(textsvector, removepunct = FALSE)
     punctinclude <- grep("[!?:;)()=/\']", names(corpfreqpunct))
     corpfreq <- append(corpfreq, corpfreqpunct[punctinclude])
     temp <- findmultiwords(tolower(textsvector), findwords)
     corpfreq <- append(corpfreq, temp)
     corpfreq <- removewords(corpfreq)
     corpfreq <- combinewords(corpfreq, combwords)
     corpfreq <- countchange(corpfreq, countchangewords)
     corpfreq <- sort(corpfreq, decreasing = TRUE)
     return(corpfreq)
}


# replaces words in 'corpfreq'
# for example, can use to capitalize proper nouns that are inappropriately lower case
replacenames <- function(corpfreq, replacepairs) {
     for (iter in 1:length(replacepairs)) {
          nameindex <- which(names(corpfreq) == replacepairs[[iter]][1])
          names(corpfreq)[nameindex] <- replacepairs[[iter]][2]
     }
     return(corpfreq)
}


# ----------------------------
# word lists called by functions
# should be adjusted to obtain desired word clouds


# phrases or compound words with spaces that should be included
# e.g., "North America", "high school", "cell phone"
findwords <- c("dromio of ephesus",
               "dromio of syracuse")

# words to remove
rmwords <- c("and", "for", "of",
             "that", "the")

# words (e.g., misspelled words) that should be counted as other words
# e.g., 'recieve' should be counted as 'receive' or 'america' as 'America'
# the 'correct' word should be the first word listed for each list element
# the 'correct' word is the only one that will be retained as a name
# ensure that a word is listed only once in each list element; if word is listed
# more than once, the final sum will be too large
combwords <- list(c("dromio", "drommio"), 
                  c("antipholus", "antifolus"),
                  c("was", "wuz"))

# this word list adjusts single-word counts based on the multi-word phrases in
# 'findwords' and called by the function 'findmultiwords'
# really, it turns out this list is not necessary; all the information needed
# for this list is already in 'findwords'
countchangewords <- list(c("dromio", "dromio of ephesus"),
                        c("ephesus", "dromio of ephesus"),
                        c("syracuse", "dromio of syracuse"))

# this word list is called by the function 'replacenames'
# the first word in each element of the list is replaced by the second word
replacewords <- list(c("adriana", "Adriana"),
                     c("aegeon", "Aegeon"),
                     c("angelo", "Angelo"),
                     c("antipholus", "Antipholus"),
                     c("dromio", "Dromio"),
                     c("dromio of ephesus", "Dromio of Ephesus"),
                     c("dromio of syracuse", "Dromio of Syracuse"),
                     c("ephesus", "Ephesus"),
                     c("i'll", "I'll"),
                     c("luciana", "Luciana"),
                     c("solinus", "Solinus"),
                     c("syracuse", "Syracuse"))


# ----------------------------
# main script:  sets up data frames of word frequencies


library(tm)
library(wordcloud)

# number of words to include in each 'sent' and 'received' texts
wordnum <- 100

# font colors for word clouds of sent and received texts
sentcolor <- "blue"
receivedcolor <- "red"

sentcorpfreq <- mainfunction(senttexts)
receivedcorpfreq <- mainfunction(receivedtexts)

sentcorpfreq <- replacenames(sentcorpfreq, replacewords)
receivedcorpfreq <- replacenames(receivedcorpfreq, replacewords)

sentcorpfreq[1:wordnum]
receivedcorpfreq[1:wordnum]

combcorpfreq <- append(sentcorpfreq[1:wordnum], receivedcorpfreq[1:wordnum])
combfreq <- as.data.frame(combcorpfreq)
combfreq[ , 2] <- names(combcorpfreq)
combfreq[ , 3] <- append(rep(sentcolor, wordnum), rep(receivedcolor, wordnum))
combfreq <- combfreq[order(combfreq[ , 1], decreasing = T), ]


# ----------------------------
# output script:  creates and saves word clouds in external files


rotationprop <- 0.35     # proportion of words to be vertical in word cloud
pdfwdth <- 10            # 'pdf' width in inches
pdfhght <- 8             # 'pdf' height in inches
pngwdth <- 1280 * 2      # 'png' width in pixels
pnghght <- 1024 * 2      # 'png' height in pixels
pngres <- 350            # 'png' resolution in pixels per inch
 

# wordclouds in 'png' files
png(file = "sentwordcloud.png", width = pngwdth, height = pnghght, res = pngres)
wordcloud(names(sentcorpfreq)[1:wordnum], sentcorpfreq[1:wordnum])
dev.off()

png(file = "receivedwordcloud.png", width = pngwdth, height = pnghght, res = pngres)
wordcloud(names(receivedcorpfreq)[1:wordnum], receivedcorpfreq[1:wordnum])
dev.off()

png(file = "combowordcloud.png", width = pngwdth, height = pnghght, res = pngres, bg = "transparent")
wordcloud(combfreq[ , 2], combfreq[ , 1], 
          rot.per = rotationprop, colors = combfreq[ , 3], random.order = F,
          ordered.colors = T, random.color = F)
dev.off()


# wordclouds in 'pdf' files
pdf(file = "sentwordcloud.pdf", width = pdfwdth, height = pdfhght)
wordcloud(names(sentcorpfreq)[1:wordnum], sentcorpfreq[1:wordnum], 
          rot.per = rotationprop, colors = sentcolor)
dev.off()

pdf(file = "receivedwordcloud.pdf", width = pdfwdth, height = pdfhght)
wordcloud(names(receivedcorpfreq)[1:wordnum], receivedcorpfreq[1:wordnum], 
          rot.per = rotationprop, colors = receivedcolor)
dev.off()

pdf(file = "combowordcloud.pdf", width = pdfwdth, height = pdfhght)
wordcloud(combfreq[ , 2], combfreq[ , 1], 
          rot.per = rotationprop, colors = combfreq[ , 3], random.order = F,
          ordered.colors = T, random.color = F)
dev.off()
