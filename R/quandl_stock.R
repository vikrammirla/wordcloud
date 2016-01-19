
#' quandlstock
#' 
#' Quandl
#' Test
#' 
#' @return null
#' @author Vikram Mirla
#' @export
quandlstock <- function() {

	library(Quandl)
	Quandl.api_key("pk6yzCsxF1k-ygB29nfS")
	data<-Quandl(c("NS1/CSCO_MM"), trim_start="2014-11-13")
	plot(data[,4])
    invisible();
}

