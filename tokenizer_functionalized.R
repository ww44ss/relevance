library(readr)

##
##
##
## FUNCTIONS

## Condition Text
## does some operations on text to make functions well behaved
condition_text<-function(s){
    s<-tolower(s)
    s<-gsub('[[:digit:]]+', '', s, perl=TRUE)  ## remove numbers
    s<-gsub('[[:punct:]]+', '', s, perl=TRUE)  ## remove punctuation
    s<-gsub('[[:digit:]]+', '', s, perl=TRUE)  ## remove numbers
    s<-gsub(' [[:alpha:]] ', '', s, perl=TRUE)  ## isolated single letters
    return(s)
}

## Condition Numbers
## does some operations on text to just pull out numbers
condition_numbers<-function(s){
    ##
    ## this function strips out all alpha characters
    s<-tolower(s)
    s<-gsub('[[:punct:]]+', '', s, perl=TRUE)  ##remove punctuation
    s<-gsub('[[:alpha:]]+', '', s, perl=TRUE)  ## remover letters
    return(s)
}

# Use readr to read in the training and test data
train = read_csv("./relevance/train.csv")
test  = read_csv("./relevance/test.csv")

# Print a sample of the training data
print(head(train))

# Let's look at the data a bit more
summary(train)
train[1:4,"query"]
train[1:4, "product_title"]
hist(train$median_relevance)
hist(train$relevance_variance)

test[1,]
test[1:4,"query"]
test[1:4,"product_title"]
test[1:4,"product_description"]
names(test)
names(train)

##okay, let's make a relevance model

    ## load the {tm} package
    require(tm)
    require(SnowballC)
    require(ggplot2)

    ## create model training and test subsets
    set.seed(8675309)
    sample_rows <- sample(1:nrow(train), size=0.4*nrow(train))
    train_data<-train[sample_rows,]
    test_data<-train[-sample_rows,]

    ## make a massive text file
    product_titles <- paste(train_data$product_title, collapse=" ")
    product_descriptions <- paste(train_data$product_description, collapse=" ")


    ## include a <s> to identify start-stop for later n-gram
    #product_titles <- paste(train_data$product_title, collapse=" startstop ")
    #product_descriptions <- paste(train_data$product_description, collapse=" startstop ")

    ## clean up the text
    product_titles <-condition_text(product_titles)
    product_descriptions <- condition_text(product_descriptions)

    ## compute title text corpus
    corpus_titles <- Corpus(VectorSource(product_titles))
    corpus_descriptions <- Corpus(VectorSource(product_descriptions))
    #t_c <- tm_map(t_c, content_transformer(tolower))
    #t_c <- tm_map(t_c, removePunctuation)
    #t_c <- tm_map(t_c, removeNumbers)
    corpus_titles <- tm_map(corpus_titles, removeWords, stopwords("english"))
    corpus_descriptions <- tm_map(corpus_descriptions, removeWords, stopwords("english"))
    
    
    titles_tdm<-TermDocumentMatrix(corpus_titles)
    descriptions_tdm<-TermDocumentMatrix(corpus_descriptions)


    ## convert to matrix
    titles_matrix<-as.matrix(titles_tdm)
    descriptions_matrix<-as.matrix(descriptions_tdm)

    titles_word_Sums <- rowSums(titles_matrix)
    descriptions_word_Sums <- rowSums(descriptions_matrix)
    titles_word_Sums <- sort(titles_word_Sums, decreasing=TRUE)
    descriptions_word_Sums <- sort(descriptions_word_Sums, decreasing=TRUE)

    ## convert to a data frame
    titles_word_Sums <- as.data.frame(titles_word_Sums)
    descriptions_word_Sums <- as.data.frame(descriptions_word_Sums)

    ## add some interesting data
    titles_word_Sums$word<-rownames(titles_word_Sums)
    titles_word_Sums$nchar<-nchar(titles_word_Sums$word)
    
    descriptions_word_Sums$word<-rownames(descriptions_word_Sums)
    descriptions_word_Sums$nchar<-nchar(descriptions_word_Sums$word)

    ##keep nchar sane
    #word_Sums<-word_Sums[word_Sums$nchar<11,]

    ## add a rank index
    word_Sums$rank<-1:dim(word_Sums)[1]

    ## keep only the first 5000 
    word_Sums <- word_Sums[word_Sums$rank<5000,] 
    word_Sums<-word_Sums[-1,]

    ## plot the data (use Log10 since that is easier  for most people to interpret)
    p <- ggplot(word_Sums, aes(y=log10(word_Sums), x=log10(rank), size=factor(nchar))) 
    p <- p + geom_point(color="orange4", alpha=0.8)
    p<- p + ggtitle("Word Frequency follows Zipf's Law")
    
    print(p)

    head(word_Sums, 100)

    ## tokenize titles
    library(RWeka)

    ##Need to specify one core for Java to work properly
    options(mc.cores=1)

    OneGramTokenizer <- function(x) {NGramTokenizer(x,Weka_control(min=1, max=1))}
    TwoGramTokenizer <- function(x) {NGramTokenizer(x,Weka_control(min=2, max=2))}
    ThreeGramTokenizer <- function(x) {NGramTokenizer(x,Weka_control(min=3, max=3))}

    #DTM<-DocumentTermMatrix(t_c)
    TermDM_1<-TermDocumentMatrix(t_c, control=list(tokenize=OneGramTokenizer))
    TermDM_2<-TermDocumentMatrix(t_c, control=list(tokenize=TwoGramTokenizer))
    TermDM_3<-TermDocumentMatrix(t_c, control=list(tokenize=ThreeGramTokenizer))
    #TermDM <- removeSparseTerms(TermDM, 0.75)

    findFreqTerms(TermDM_1, 10)[1:5]
    findFreqTerms(TermDM_2, 10)[1:5]
    findFreqTerms(TermDM_3, 10)[1:5]
    
    term_freq_list <- function(TermDM) {
        ##
        ## Turns a TDM into an n-gram frequency table
        ##
        ##
        ## turn TDM into matrix
        x_gram_matrix<-as.matrix(TermDM)
        ## capture n-grams
        n_gram<-rownames(x_gram_matrix)
        ## convert to data frame
        n_gram<-as.data.frame(n_gram)
        ## aggregate term frequencies
        frequency<-rowSums(x_gram_matrix)
        ## bind terms and frequency data into columns of data frame
        n_gram<-cbind(n_gram, frequency)
        ## sort by decreasing frequency
        n_gram<-n_gram[order(-frequency),]
        ## renumber row names
        rownames(n_gram)<-1:dim(n_gram)[1]
        
        return(n_gram)
        
    }

    ## get three grams 
    three_gram<-term_freq_list(TermDM_3)
    two_gram<-term_freq_list(TermDM_2)
    one_gram<-term_freq_list(TermDM_1)
    
    head(three_gram)
    head(two_gram)
    head(one_gram)


    ## TWO GRAM
    TermDM<-TermDocumentMatrix(t_c, control=list(tokenize=TwoGramTokenizer))
    
    ## get three grams 
    two_gram<-term_freq_list(TermDM)
    two_gram<-two_gram[two_gram$frequency>2,]
    
    title_two_gram<-two_gram

    ## here is a way to use grep to get rid of "startstop" terms
    ## grep("startstop", two_gram$n_gram)

## DESCRIPTION TOKENIZE

    ## condition text
    t_c <- Corpus(VectorSource(product_descriptions))
    t_c <- tm_map(t_c, content_transformer(tolower))
    t_c <- tm_map(t_c, removePunctuation)
    t_c <- tm_map(t_c, removeNumbers)
    t_c <- tm_map(t_c, removeWords, stopwords("english"))

    ## Three_grams
    TermDM<-TermDocumentMatrix(t_c, control=list(tokenize=ThreeGramTokenizer))
    ## Get three grams 
    three_gram<-term_freq_list(TermDM)
    three_gram<-three_gram[three_gram$frequency>2,]
    
    description_three_gram<-three_gram

    TermDM<-TermDocumentMatrix(t_c, control=list(tokenize=TwoGramTokenizer))

    ## get two grams 
    two_gram<-term_freq_list(TermDM)
    two_gram<-two_gram[two_gram$frequency>2,]

    description_two_gram<-two_gram
