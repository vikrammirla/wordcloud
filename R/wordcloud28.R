
#' wordcloud28
#' 
#' upload files
#' generate wordcloud
#' 
#' @return null
#' @author Vikram Mirla
#' @export
wordcloud28 <- function() {


	library(tm)
	cname <- file.path("/root", "texts")
	


	dir(cname)
	docs <- Corpus(DirSource(cname))  
	inspect(docs)
	docs$content

	docs <- tm_map(docs, removePunctuation)   

	for(j in seq(docs))   
	{   
	  docs[[j]] <- gsub("/", " ", docs[[j]])   
	  docs[[j]] <- gsub("@", " ", docs[[j]])   
	  docs[[j]] <- gsub("\\|", " ", docs[[j]])
	  docs[[j]] <- gsub("\\|", " ", docs[[j]])
	  docs[[j]] <- gsub("\\t", " ", docs[[j]])
	  docs[[j]] <- gsub("\\]", " ", docs[[j]])
	  docs[[j]] <- gsub("\\[", " ", docs[[j]])
	} 

	docs <- tm_map(docs, removeNumbers) 
	docs <- tm_map(docs, tolower)   
	docs <- tm_map(docs, removeWords, stopwords("english"))   
	docs <- tm_map(docs, removeWords, c("department", "email"))


	#Removing particular words:
	docs <- tm_map(docs, removeWords, c("initiation", "email","june","bfams")) 
	inspect(docs)
	#Combining words that should stay together


	for (j in seq(docs))
	{
	  docs[[j]] <- gsub("qualitative research", "QDA", docs[[j]])
	  docs[[j]] <- gsub("goldman sachs global investment research", "GSGIR", docs[[j]])
	  docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]])
	  docs[[j]] <- gsub("research methods", "research_methods", docs[[j]])
	}


	#Removing common word endings. (e.g., “ing”, “es”, “s”)
	library(SnowballC)  
	#docs <- tm_map(docs, stemDocument)   

	#Stripping unnecesary whitespace from your documents
	docs <- tm_map(docs, stripWhitespace)   



	#This tells R to treat your preprocessed documents as text documents.
	docs <- tm_map(docs, PlainTextDocument) 



	#create a document term matrix.
	dtm <- DocumentTermMatrix(docs)   
	inspect(dtm)
	dim(dtm)
	inspect(dtm[1, 1:20])




	#transpose of this matrix
	tdm <- TermDocumentMatrix(docs)  


	#Organize terms by their frequency:

	colSums(as.matrix(as.matrix(dtm)))
	freq <- colSums(as.matrix(dtm))   
	length(freq)  
	ord <- order(freq)
	m <- as.matrix(dtm)   
	dim(m) 
	write.csv(m, file="dtm.csv") 



	#  Start by removing sparse terms:
	dtms <- removeSparseTerms(dtm, 0.3)
	inspect(dtms)


	#Word Frequency
	freq[tail(ord)]
	head(table(freq), 20) 
	tail(table(freq), 20)

	freq <- colSums(as.matrix(dtms))   
	freq   
	freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
	head(freq, 14) 


	findFreqTerms(dtm, lowfreq=20) 

	wf <- data.frame(word=names(freq), freq=freq)   
	head(wf)

	library(ggplot2)   
	#p <- ggplot(subset(wf, freq>15), aes(word, freq))    
	#p <- p + geom_bar(stat="identity")   
	#p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
	#p   






	#Relationships Between Terms
	findAssocs(dtm, c("revenue" , "revenue"), corlimit=0.08)


	library(wordcloud)   
	wordcloud(names(freq), freq, min.freq=10)  


    invisible();
}

