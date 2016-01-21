
#' wordcloud28
#' 
#' upload files
#' generate wordcloud
#' 
#' @return null
#' @author Vikram Mirla
#' @export

make_map <- function(title = "This is a test", lat, lng){
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)
  m <- leaflet::addMarkers(m, lng = as.numeric(lng), lat = as.numeric(lat), popup = title)
  htmlwidgets::saveWidget(m, "mymap.html", selfcontained = FALSE)
}


rChartsTest <- function() {
	library(devtools)
	#install_github("ramnathv/rCharts")
	library(rCharts)

	# interactive bar chart with d3js(NVD3)
	# this code example was taken from http://ramnathv.github.io/rCharts/
	hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
	n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart")
	#n1$print("chart42")
   # n1$save
   #n1$save('myChart.html')
	htmlwidgets::saveWidget(n1, "myChart.html", selfcontained = FALSE)


}
wordcloud28 <- function() {


	library(tm)
	#readLines(system.file(package='wordcloud28', 'texts/gs.txt'))

	cname <- file.path(".", "texts")
	

    #method1
	dir(cname)
	#docs <- Corpus(DirSource(cname))  
	#inspect(docs)
	#docs$content

	#method2
	library("magrittr")
	url <- "http://www.textfiles.com/stories/3lpigs.txt"

	corpus <- url %>% 
	  paste(readLines(url), collapse=" ") %>% 
	  gsub("http://www.textfiles.com/stories/3lpigs.txt", "", .)
	head(corpus)

	z <- corpus %>% 
	  gsub(" +", " ", .) %>% 
	  strsplit(split = "[\\.?!] ")
	z
	#I copied above code from stackoverflow, and tried to create a corpus from the data frame
	(docs <- DataframeSource(data.frame(z)))
	inspect(Corpus(docs))

   #method3
	library(tm)

   GreekShippingContent <- "The Greek administration is coming under increasing pressure over it foot-dragging regarding its meeting international convention deadlines, especially when it relies on classification societies as an Recognised Organisation (RO) on its behalf. " 
   GreekShippingContent0 <-  Corpus(VectorSource(GreekShippingContent))
   docs<-tm_map(GreekShippingContent0, PlainTextDocument)




	#Removing common word endings. (e.g., “ing”, “es”, “s”)
	library(SnowballC)  
	#docs <- tm_map(docs, stemDocument)   

	
	#create a document term matrix.
	dtm <- DocumentTermMatrix(docs)   
#	inspect(dtm)
	dim(dtm)
#	inspect(dtm[1, 1:20])

	#transpose of this matrix
	tdm <- TermDocumentMatrix(docs)  

	#Organize terms by their frequency:
	colSums(as.matrix(as.matrix(dtm)))
	freq <- colSums(as.matrix(dtm))   
	length(freq)  
	ord <- order(freq)
	m <- as.matrix(dtm)   
	dim(m) 
#	write.csv(m, file="dtm.csv") 



	#  Start by removing sparse terms:
	dtms <- removeSparseTerms(dtm, 0.3)


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
	#findAssocs(dtm, c("revenue" , "revenue"), corlimit=0.08)


	library(wordcloud)   
	wordcloud(names(freq), freq, min.freq=10)  


    invisible();
}

