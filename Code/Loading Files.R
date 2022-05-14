# dopilnujmy by w ramce nie bylo kolumn typu factor
if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE) # dla R w wersji < 4.0
# ww. pliki znajduja sie w katalogu travel_stackexchange_com
Tags <- read.csv("travel_stackexchange_com/Tags.csv")
head(Tags)