library(readr)

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
    sample_rows <- sample(1:nrow(train), size=0.2*nrow(train))
    train_data<-train[sample_rows,]
    test_data<-train[-sample_rows,]

    ## make a massive text file
    ## include a <s> to identify start-stop for later n-gram
    product_titles <- paste(train_data$product_title, collapse=" startstop ")
    
    ## compute text corpus
    t_c <- Corpus(VectorSource(product_titles))
    t_c <- tm_map(t_c, content_transformer(tolower))
    t_c <- tm_map(t_c, removePunctuation)
    t_c <- tm_map(t_c, removeNumbers)
    t_c <- tm_map(t_c, removeWords, stopwords("english"))
    
    
    tdm<-TermDocumentMatrix(t_c)

    ## explore
    findAssocs(tdm, "startstop", 0.0001)
    findAssocs(tdm, "black", 1)
    findAssocs(tdm, "men", 0.1)

    ## convert to matrix
    tdm_matrix<-as.matrix(tdm)

    word_Sums <- rowSums(tdm_matrix)
    word_Sums <- sort(word_Sums, decreasing=TRUE)

    ## convert to a data frame
    word_Sums <- as.data.frame(word_Sums)

    ## add some interesting data
    word_Sums$word<-rownames(word_Sums)
    word_Sums$nchar<-nchar(word_Sums$word)

    ##keep nchar sane
    word_Sums<-word_Sums[word_Sums$nchar<11,]

    ## add a rank index
    word_Sums$rank<-1:dim(word_Sums)[1]

    ## keep only the first 5000 
    word_Sums <- word_Sums[word_Sums$rank<5000,] 

    ## plot the data (use Log10 since that is easier  for most people to interpret)
    p <- ggplot(word_Sums, aes(y=log10(word_Sums), x=log10(rank), size=factor(nchar))) 
    p <- p + geom_point(color="orange4", alpha=0.8)
    p<- p + ggtitle("Word Frequency follows Zipf's Law")
    
    print(p)

    head(word_Sums, 100)

    ## toeknize
    library(RWeka)

    ## condition text
    t_c <- Corpus(VectorSource(product_titles))
    t_c <- tm_map(t_c, content_transformer(tolower))
    t_c <- tm_map(t_c, removePunctuation)
    t_c <- tm_map(t_c, removeNumbers)
    t_c <- tm_map(t_c, removeWords, stopwords("english"))

    ##Need to specify one core for Java to work properly
    options(mc.cores=1)

    TwoGramTokenizer <- function(x) {NGramTokenizer(x,Weka_control(min=2, max=2))}
    ThreeGramTokenizer <- function(x) {NGramTokenizer(x,Weka_control(min=3, max=3))}

    #DTM<-DocumentTermMatrix(t_c)
    TermDM<-TermDocumentMatrix(t_c, control=list(tokenize=ThreeGramTokenizer))
    #TermDM <- removeSparseTerms(TermDM, 0.75)

    b<-findFreqTerms(TermDM, 10)
    


