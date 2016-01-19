
#' wordcloud28
#' 
#' upload files
#' generate wordcloud
#' 
#' @return null
#' @author Vikram Mirla
#' @export
wordcloud28 <- function() {

	library(Quandl)
	Quandl.api_key("pk6yzCsxF1k-ygB29nfS")
	data<-Quandl(c("NS1/CSCO_MM"), trim_start="2014-11-13")
	plot(data[,4])
    invisible();
}

