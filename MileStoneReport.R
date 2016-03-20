# R Script for my analysis - can pick the code snippet after my internal testing and 
# paste it into the Rmd file for my report

# Common variables required
langs <- c("en_US", "de_DE", "fi_FI", "ru_RU")
pathname <- "C:/Users/ctsuser1/Desktop/Santhosh/DataScience/workingdir/CapStone/Coursera-SwiftKey/final"
mainPath <- "C:/Users/ctsuser1/Desktop/Santhosh/DataScience/workingdir/CapStone"
zipFile <- "Coursera-SwiftKey.zip"
fileTypes <- c("blogs", "news", "twitter")
libs <- c("tm", "plyr", "R.utils", "stringi", "ggplot2", "stringr")

# Initialize
initialize <- function() {
    # Load required libraries
    lapply(libs, require, character.only = TRUE)
    
    # Set options
    options(stringsAsFactors = FALSE)
    
    # Setting seed for using in Sampling of corupis
    set.seed(1000)

    print("Initialied with all libraries")
}

# Getting the Data - Download if not already done so
getCorpusData <- function(path) {
    # Check if the directory exists
    dataFileExists <- dir.exists(path)
    
    if (dataFileExists != TRUE) {

        # specify the source and destination of the download
        dest.file <- paste(mainPath, "/", zipFile)
        src.file <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        
        # execute the download
        download.file(src.file, dest.file)
        # extract the files from the zip file
        unzip(dest.file)
    }    
}

# Utility function - to get File Size
getFileSize <- function(filename) {
    return(file.info(filename)$size)
}

# Utility function - to get Word count
getWordCount <- function(filename) {
    # Readlines from the file
    fileContent <- readLines(filename, encoding = "UTF-8")
    
    stri_stats_general(fileContent)
    summary(fileContent)
    
    # Get Word Count
    wordsTotal <- stri_count_words(fileContent)

    # Collect the sample
    sampleText <- fileContent[sample(1:length(fileContent), 1000)]; 

    # Remove fileContents that has been read into memory, to save memory
    rm(fileContent)
    
    return(list("wordCount" = wordsTotal,
                "sample" = sampleText))
}

# Utility function - to get Line Count
getLineCount <- function(filename) {
    n <- countLines(filename)
    return(n[1])
}
# Utility function - to get Word Frequency

# Construct the file name give path and language
constructFilename <- function(fileType, path, language) {
    f.name <- sprintf("%s/%s/%s.%s.txt", path, language, language, fileType)
    return(f.name)
}

# Analyze/Text mine the given file
textMineFile <- function(filename) {
    # File size
    fileSz <- getFileSize(filename)
    
    # get Number of Lines
    lineCount <- getLineCount(filename)
    
    # Word Count
    wordsnsample <- getWordCount(filename)
    
    # Perform additional analysis
    # TBD - To Be Done
    
    tdm <- list("fileSz" = fileSz, 
                "lineCount" = lineCount,
                "wordsnsample" = wordsnsample,
                "fileName" = filename)
    return(tdm)
}

# Find the given N-Gram
computeNgrams <- findNGrams <- function(corp, grams, top) {
    ngram <- RWeka::NGramTokenizer(corp, Weka_control(min = grams, max = grams,
                                               delimiters = " \\r\\n\\t.,;:\"()?!"))
    ngram <- data.frame(table(ngram))
    ngram <- ngram[order(ngram$Freq, decreasing = TRUE),][1:top,]
    colnames(ngram) <- c("Words","Count")
    ngram
}

# Clean the Corpus
cleanCorpus <- function(sampleText) {
    sampleText <- str_replace_all(sampleText,"[^[:graph:]]", " ") 
    sampleText <- iconv(sampleText, "UTF-8", "ASCII")
    myCorpus <- Corpus(VectorSource(sampleText))
    toSpace  <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    myCorpus <- tm_map(myCorpus, toSpace,"\"|/|@|\\|")

    #myCorpus <- tm_map(myCorpus, function(x) iconv(enc2utf8(x$content), sub = "bytes"))
    #myCorpus <- tm_map(myCorpus, function(x) iconv(x, to='UTF-8', sub='bytes'))
    
    myCorpus <- tm_map(myCorpus, removeNumbers)
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
    myCorpus <- tm_map(myCorpus, stripWhitespace)
    
    # Remove stop words
    myCorpus <- tm_map(myCorpus , removeWords, stopwords('english'))    
    
    return(myCorpus)
}

nGramAnalysis <- function(filenames, tdm) {

    # Load as late as possible
    library(RWeka)
    
    # Put together Sample from all three files
    consolodatedSample <- c(tdm[[1]]$wordsnsample$sample,
                            tdm[[2]]$wordsnsample$sample,
                            tdm[[3]]$wordsnsample$sample)
    
    cleanedCorpus <- cleanCorpus(consolodatedSample)
    
    corpusDF <- data.frame(text = unlist(sapply(cleanedCorpus, `[`, "content")), 
                           stringsAsFactors = FALSE)
    
    monoGrams   <- findNGrams(corpusDF, 1, 100)
    biGrams     <- findNGrams(corpusDF, 2, 100)
    triGrams    <- findNGrams(corpusDF, 3, 100)
    quadriGrams <- findNGrams(corpusDF, 4, 100)
    
    return(list("monoGrams" = monoGrams,
                "biGrams" = biGrams,
                "triGrams" = triGrams,
                "quadriGrams" = quadriGrams))
}

# Analysis/text of the files like - Size, Line Count and Word Count and n-Gram Analysis
textMine <- function(language, path) {
    print(paste("Input Language: ", language))
    print(paste("Path: ", path))
    
    # Construct file names for each kind of file we are analyzing
    filenames <- lapply(fileTypes, constructFilename, path, language)
    
    # Analyze each file
    tdm <- lapply(filenames, textMineFile)

    #Perform Additional Analysis on sample from all files together
    myNGrams <- nGramAnalysis(filenames, tdm)
    
    print("Function Text Mine!")
    return(list("tdm" = tdm,
                "myNGrams" = myNGrams))
}

printResultTDM <- function(tdm) {
    for(i in 1:3) {
        print(paste("Analysis for file: \n", tdm[[i]]$fileName))
        print(paste("File Size (in MB): ", tdm[[i]]$fileSz/(1024^2)))      
        print(paste("Line Count: ", tdm[[i]]$lineCount))

        print(paste("Word Count: ", sum(tdm[[i]]$wordsnsample$wordCount)))
        print("Words Summary Details:")
        print(summary(tdm[[i]]$wordsnsample$wordCount))
    }
}

printnGrams <- function(nGramResult) {
    # number of ngrams to show in the graph
    n <- 20
    # Plotting of the various nGrams    
    ggplot(nGramResult$monoGrams[1:n,], aes(Words, Count)) + geom_bar(stat = "identity") + 
        coord_flip()
    ggplot(nGramResult$biGrams[1:n,], aes(Words, Count)) + geom_bar(stat = "identity") + 
        coord_flip()
    ggplot(nGramResult$triGrams[1:n,], aes(Words, Count)) + geom_bar(stat = "identity") + 
        coord_flip()
    ggplot(nGramResult$quadriGrams[1:n,], aes(Words, Count)) + geom_bar(stat = "identity") + 
        coord_flip()
}

printResult <- function(analysisResult) {
    printResultTDM(analysisResult$tdm)
    printnGrams(analysisResult$myNGrams)
}

mainFunction <- function() {
    # Initialize
    initialize()
    
    # Download the data if required
    getCorpusData(pathname)
    
    # Perform Text Mining on all Language set of files sequentially
    #tdm <- lapply(langs, textMine, pathname)
    # For now, just to Enginlis, "en_US"
    textAnaResult <- textMine("en_US", pathname)

    print("Results of the Analysis")
    printResult(textAnaResult)

}
