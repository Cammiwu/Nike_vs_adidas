if(!require('twitteR')) install.packages('twitteR', repos = "http://cran.us.r-project.org")
if(!require('ROAuth')) install.packages('ROAuth', repos = "http://cran.us.r-project.org")
if(!require('tm')) install.packages('tm', repos = "http://cran.us.r-project.org")
if(!require('wordcloud')) install.packages('wordcloud', repos = "http://cran.us.r-project.org")
if(!require('RColorBrewer')) install.packages('RColorBrewer', repos = "http://cran.us.r-project.org")
if(!require('stringr')) install.packages('stringr', repos = "http://cran.us.r-project.org")
if(!require('plyr')) install.packages('plyr', repos = "http://cran.us.r-project.org")
if(!require('ggplot2')) install.packages('ggplot2', repos = "http://cran.us.r-project.org")
if(!require('XML')) install.packages('XML', repos = "http://cran.us.r-project.org")
if(!require('gridExtra')) install.packages('gridExtra', repos = "http://cran.us.r-project.org")


library(twitteR)
library(ROAuth)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(plyr)
library(ggplot2)
library(XML)
library(gridExtra)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "oGuB54OBIEmfZkLIEPyRquzUE"
consumer_secret <- "ajrnEZH0Dc3WFyUqbmRm7uWKIxI5TAr2kQoKJWcr9LijWywGNd"
access_token <- "1013366490488176640-XgQbsFokxVO3Vfg4bbAKzuDJKgTOzM"
access_secret <- "PufijegJFZryRulrOQjTnmTAYdVgViivOm86mwKOZoOR2"


## Function for scoring 
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    
    # clean up sentences with R gsub() - remove punctuatoin
    sentence = gsub('\\d+', '', sentence)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sentence)
    
    # convert to lower case:
    sentence = tolower(sentence)
    
    # split into words
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # Change position of the matched term/NA to TRUE/FALSE
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}



## Search adidas and nike on twitter
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
dataDir=getwd()
ni_tw = searchTwitter('@nike', n=500)
save(ni_tw, file=file.path(dataDir, 'ni_tw.RData' ), ascii=T)
ad_tw = searchTwitter('@adidas', n=500)
save(ad_tw, file=file.path(dataDir, 'ad_tw.RData' ), ascii=T)


## Perform Sentiment Analysis
## Read in positive and negative words from the lexicon 
poswords <- scan("positive-words.txt", what="character", comment.char=";")
negwords <- scan("negative-words.txt", what="character", comment.char=";")

# add a few twitter and industry favorites
pos.words = c(poswords, 'upgrade','luv', 'comfortable','easy','refresh')
neg.words = c(negwords, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical','shitty','atleast')


adidas.text = plyr::laply(ad_tw, function(t) t$getText())
adidas.text=str_replace_all(adidas.text,"[^[:graph:]]", " ")
adidas.scores = score.sentiment(adidas.text, pos.words, neg.words, .progress='text')
adidas.scores$brand = "adidas"

nike.text = plyr::laply(ni_tw, function(t) t$getText())
nike.text=str_replace_all(nike.text,"[^[:graph:]]", " ")
nike.scores = score.sentiment(nike.text, poswords, negwords, .progress='text')
nike.scores$brand = "nike"

## Join the two scores together
all.scores = rbind(adidas.scores, nike.scores)

g.hist = ggplot(data=all.scores, mapping=aes(x=score, fill=brand))
# add a bar graph layer. Let it bin the data and compute frequencies
# (set binwidth=1 since scores are integers)
g.hist = g.hist + geom_histogram( binwidth=1 )

# Create histogram using facet grid
g.hist = g.hist + facet_grid(brand~.)

# Add Colors
g.hist = g.hist + theme_bw() + scale_fill_brewer() 
print(g.hist)


## Compare scores with asci 
acsi.url = 'http://www.theacsi.org/index.php?option=com_content&view=article&id=149&catid=&Itemid=214&c=Adidas'
readacsi.raw = readHTMLTable(acsi.url, header=T, which=1, stringsAsFactors=F)

## Extract only rows with adidas and nike -- using YEAR 2017
asci.scores = readacsi.raw[c(1,4),c(1,25)]
colnames(asci.scores) = c("brand", "score")

## Scoring all.scores
all.scores$very.pos.bool = all.scores$score >= 2
all.scores$very.neg.bool = all.scores$score <= -2
all.scores$very.pos = as.numeric( all.scores$very.pos.bool )
all.scores$very.neg = as.numeric( all.scores$very.neg.bool )
twitter.df = ddply(all.scores, c('brand'), summarise, 
                   very.pos.count=sum( very.pos ), 
                   very.neg.count=sum( very.neg ))
twitter.df$very.tot = twitter.df$very.pos.count + twitter.df$very.neg.count

## Find the percentage of positive scores
twitter.df$score = round( 100 * twitter.df$very.pos.count / 
                            twitter.df$very.tot )

## Compare the 2 dfs
compare.df = cbind(asci.scores, twitter.df[5])
colnames(compare.df) = c("Brand", "ASCI Score", "Twitter Score")

## Look at datatable to compare the 2 scores
compare.df
grid.table(compare.df)
