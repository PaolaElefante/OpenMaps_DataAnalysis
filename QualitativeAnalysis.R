library(dplyr)
library(aod)
library(ggplot2)
library(xlsx)
library(stringr)
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Qualitative analysis of area
# Paola Elefante, paolaelefante.com@gmail.com 

BDS_df <- read.csv("/Users/elefante/Documents/R_programming/Dataset.csv")
natarget_df <- subset(BDS_df,subset=is.na(BDS_df$target1))
yestarget_df <- subset(BDS_df,subset=(BDS_df$target1==1)|(BDS_df$target1==0))
## abt 30% are NA, the others have or have not a target


v<-colnames(yestarget_df)

v05km<-v[substr(v,1,18)=="i_geocode_poi_km05"]  # points of interest within 500 m
v2km <-v[substr(v,1,17)=="i_geocode_poi_km2"] # points of interest within 2 km
yestarget_df <- subset( yestarget_df, select = c(v05km,v2km)) # ignore points of interest >2km away

# Clean up 
dfnames <- names(yestarget_df)
c1 <- c(2,19,35,46,58,141,146,167,176,202,203,212,221,222,227,228,237,271,272,273,275)
dfnames[c1] <- "0"
dfnames <- dfnames[(dfnames!="0")]
c1 <- c(473,474,485, 510,511,516,515,553,552,601,605:608)
dfnames[c1] <- "0"
dfnames <- dfnames[(dfnames!="0")]
c1<- c(299,318,352,432,438,439,442)
dfnames[c1] <- "0"
dfnames <- dfnames[(dfnames!="0")]
yestarget_df<-subset(yestarget_df,select=dfnames)

# select random customer and compute wordcloud of locations
rand_cust <- sample(c(1:nrow(yestarget_df)),1)
poi <- yestarget_df[rand_cust,]
indices <- which(poi[1,]!=0)
poi <- poi[1,indices]
# Clean up strings for PoI names
names(poi) <- str_replace_all(names(poi), "i_geocode_poi_km05_", "")
ind2km <- which(substr(names(poi),1,17)=="i_geocode_poi_km2")
poi[1,ind2km]<-poi[1,ind2km]*0.5         # PoI 2km away are weighted less
names(poi) <- str_replace_all(names(poi), "i_geocode_poi_km2_", "")
names(poi) <- str_replace_all(names(poi), "_", " ")
poi_df <- data.frame(names(poi),unlist(poi),stringsAsFactors = FALSE)
names(poi_df)<- c("word","freq")

# Generate wordcloud
pal <- brewer.pal(9, "Set3")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1280,height=800)
wordcloud(poi_df$word,poi_df$freq,scale=c(10,1),random.order = F,rot.per=.15,max.words = 100,colors=pal,vfont=c("serif","plain"))