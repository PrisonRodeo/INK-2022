###################################################
# SU INK workshop (Spring 2022)
#
# Topic: Text-y things!
#
# Note that things you see here in green are 
# commented out, via the presence of a "#" at
# the beginning of the line. R will not read /
# run anything that is commented out in that
# way...
########################################################
# Load packages (install as needed):

library(readr)
library(plyr)
library(statmod)
library(MASS)
library(plot.matrix)
# install.views("NaturalLanguageProcessing")
library(stopwords)
library(tm)
library(SnowballC)
library(tokenizers)
library(stringdist)
library(stylo)
library(lsa)
library(wordcloud)
library(topicmodels)
library(qdap)
library(SentimentAnalysis)
library(readtext)
library(rvest)
library(quanteda)
library(devtools)
library(tidytext)
library(textplot)
library(ggraph)
library(concaveman)
#install.packages("remotes")
#remotes::install_github("almogsi/TATE")
library(TATE)

# Set working directory:

setwd("~/Dropbox (Personal)/Text-Workshop-SU") # <-- change as necessary...

# Set options:

options(scipen = 8) # bias against scientific notation
options(digits = 3) # show fewer decimal places
####################################################
# Basic text manipulation: a really tiny example:
#
# Raw text:

Joke <- "A man visits a tattoo parlor with a rather simple, but strange request. He requests a short, straight line tattooed on his upper arm. Once the first tattoo heals, he returns, asking for another, exactly the same as the first. After a few more visits, it becomes clear to the tattoo artist that he's tattooing tally marks on the customer's arm. Curiosity gets the better of the tattoo artist, and she asks, 'What are you counting?' The man answers, 'How many tattoos I have.'"

# Basic operations:
#
# Replace capitals (all-caps is "toupper"):

tolower(Joke)

# Replace characters (ex: "a" with "A"):
# 
# chartr("a","A",Joke)
#
# Punctuation removal:

removePunctuation(Joke)

# Remove words:
# 
# removeWords(Joke, "tattoo")
#
# # From a list:
# 
# wordsGone<-c("tattoo","man","request")
# removeWords(Joke, wordsGone)
#
# Can also removeNumbers and stripWhitespace...
#
# Tokenize: Break into sentences:

Joke.sent <- tokenize_sentences(Joke)
Joke.sent
length(Joke.sent[[1]])

# Tokenize II: Break into words:

Joke.words <- tokenize_words(Joke)
Joke.words
length(Joke.words[[1]]) # total word count

# Tokenize III: Break sentences into words:
# 
# Joke.sw <- tokenize_words(Joke.sent[[1]])
# Joke.sw
#
# Count words per sentence:

Joke.wordcount <- sapply(tokenize_words(Joke.sent[[1]]), length)
Joke.wordcount
mean(Joke.wordcount) # mean words per sentence...

# Term frequencies:

termFreq(Joke,control=list(removePunctuation=TRUE,
                           wordLengths=c(1,Inf)))

# Eliminate stop-words:

removeWords(Joke,stopwords("en"))

# Basic stemming (uses the Snowball stemmer):

stemDocument(Joke)

# Creating a (simple) corpus from sentences/words (NLP package):

Joke.clean <- removePunctuation(Joke.sent[[1]])
IDJ<-Corpus(VectorSource(Joke.clean))
inspect(IDJ)

# Term-Document Matrix:

IDJ.TDM <- TermDocumentMatrix(IDJ,control=list(tolower=TRUE,
                                          stemming=TRUE))
inspect(IDJ.TDM)


#########################################
# Toy cosine similarity example:

A<-"Accidentally drank invisible ink. I'm at the hospital waiting to be seen."
B<-"Accidentally Accidentally drank drank invisible invisible ink ink. I'm I'm at at the the hospital hospital waiting waiting to to be be seen seen."
C<-"Accidentally prank invisible mink. I'm cat the hospitality waiting too be scene."
D<-"Why were all the ink spots crying? Their father was in the pen."
E<-"What do you call a bear with no teeth? A gummy bear."

dtmm<-DocumentTermMatrix(Corpus(VectorSource(rbind(A,B,C,D,E))),
                         control=list(removePunctuation=TRUE))
proxy::dist(as.matrix(dtmm),method="cosine")


#########################################
# Now... DAD JOKES!
#
# Go get those sweet, sweet dad jokes rn:

DadJokes<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/INK-2022/main/Data/DadJokes.csv")

# Document similarity...
#
# First, preprocess and put the jokes into a corpus --> DTM:

DJ<-data.frame(Joke=rep(" ",times=nrow(DadJokes)))
for(i in 1:nrow(DJ)){
  DJ[i,1]<-tolower(removePunctuation(DadJokes[i,1]))
}
DJ$JokeID<-seq(1:nrow(DJ))

DJ2 <- with(DJ, data.frame(doc_id = JokeID,
                           text = Joke))

dj <- DataframeSource(DJ2) # make a list...
DJC<-Corpus(dj) # create a corpus object

# TDM and DTM:

TDM<-TermDocumentMatrix(DJC)
DTM<-DocumentTermMatrix(DJC)

# Cosine similarity among all 145 jokes:

CSDist<-as.matrix(proxy::dist(as.matrix(DTM),method="cosine"))

# Calculate minimum + average distance for each:

diag(CSDist)<-NA       # get rid of the zeros on the main diagonal

# Most similar / "closest" jokes to each other:

close<-which(CSDist==min(CSDist,na.rm=TRUE),arr.ind=TRUE)
print(DadJokes[close[1,1],])
print(DadJokes[close[1,2],])
print(DadJokes[close[2,1],])
print(DadJokes[close[2,2],])

# How similar are the first 20 jokes to each other?

pdf("Slides/CosineDistanceJokes.pdf",7,6)
par(mar=c(4,4,2,6))
plot(as.matrix(CSDist)[1:20,1:20],
     main="",xlab="Joke Number",ylab="Joke Number",
     cex.axis=0.7)
dev.off()

# Which jokes are most and least distinctive? 
#
# Calculate mean/average distances:

mean_dists<-apply(CSDist,2,mean,na.rm=TRUE)

leastd<-which(mean_dists==min(mean_dists,na.rm=TRUE),arr.ind=TRUE)
minmeanD<-mean_dists[leastd]
mostd<-which(mean_dists==max(mean_dists,na.rm=TRUE),arr.ind=TRUE)
maxmeanD<-mean_dists[mostd]

print(DadJokes[leastd,])
print(DadJokes[mostd,])

# Histogram of the distances:

pdf("Slides/DistHist.pdf",7,6)
par(mar=c(4,4,2,2))
hist(mean_dists,breaks=12,main="",xlim=c(0.84,1),
     ylim=c(0,25),xlab="Mean Distance")
abline(v=minmeanD,lty=2,lwd=2,col="navy")
abline(v=maxmeanD,lty=2,lwd=2,col="orange")
text(0.852,19,"What did the zero say to the eight?\nThat belt looks good on you.",
     pos=4,col="navy")
text(0.998,25,"It takes guts to be an organ donor.",
     pos=2,col="orange")
dev.off()

####################################################
# Topic models...
#
# ...using a biterm topic model...
#
# Remove stopwords & create data frame:

DJ3<-DJ2
DJ3$text<-removeWords(DJ3$text,stopwords("english"))
DJ3$text<-stemDocument(DJ3$text)

BTMDF<-unnest_tokens(DJ3,input=text,output=word)

# Biterm topic model:

set.seed(7222009)
topix<-BTM(BTMDF,5,window=5)

# Plot:

pdf("Slides/BTM-Plot.pdf",7,6)
plot(topix,top_n=10,title="")
dev.off()

# Bigger (75K) joke database:

JDF<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/INK-2022/main/Data/QAJokes.csv")
JDF$Joke<-paste0(JDF$Question," ",JDF$Answer)
JDF$Question<-NULL
JDF$Answer<-NULL
JDF$ID<-1:nrow(JDF)
JDF$Joke<-tolower(JDF$Joke)
JDF$Joke<-removeWords(JDF$Joke,stopwords("english"))
JDF$Joke<-stemDocument(JDF$Joke)
BigJoke<-unnest_tokens(JDF,input=Joke,output=word)

# Topic model:

topix2<-BTM(BigJoke,k=13,window=5,background=TRUE,
            trace=TRUE,iter=500)

# Plot:

pdf("Slides/BTM-Bigger.pdf",9,7)
par(mar=c(0.5,0.5,0.5,0.5))
plot(topix2,top_n=7,title="",
     which=c(3,5,7:13))
dev.off()

####################################################
# Dictionary-based methods...
#
# Calculate the humorousness of each joke:

DJ$Funny<-NA
for(i in 1:nrow(DJ)) {
  DJ$Funny[i]<-funny(DJ$Joke[i]) 
}

pdf("Slides/HowFunnyHist.pdf",12,6)
par(mar=c(4,4,2,2))
hist(DJ$Funny,main="",xlab="Humor Rating",ylim=c(0,40))
abline(v=mean(DJ$Funny,na.rm=TRUE),lwd=2,lty=2)
text(2.23,35,"Mean Humorousness= 2.23",pos=4)
dev.off()

funniest<-which(DJ$Funny==max(DJ$Funny,na.rm=TRUE),arr.ind=TRUE)
sonotfunny<-which(DJ$Funny==min(DJ$Funny,na.rm=TRUE),arr.ind=TRUE)

print(DadJokes[funniest,])
print(DadJokes[sonotfunny,])

# DAV:

DJ$Valence<-NA
DJ$Arousal<-NA
DJ$Dominance<-NA
for(i in 1:nrow(DJ)) {
  DJ$Valence[i]<-NoVAD(DJ$Joke[i])[1]
  DJ$Arousal[i]<-NoVAD(DJ$Joke[i])[2] 
  DJ$Dominance[i]<-NoVAD(DJ$Joke[i])[3] 
}
