


#Retrieving data
require(rjson)
urls = c("http://hypem.com/playlist/search/geographer/json/1/data.js", "http://hypem.com/playlist/search/geographer/json/2/data.js")
url = ("http://hypem.com/playlist/search/geographer/json/2/data.js")

 
geo <- fromJSON(readLines(url, warn="F"))
geo2 <- fromJSON(readLines(url, warn="F"))
geo

delist <- function(x) {
    for (i in length(x))
        Y <- as.matrix(rbind(x[i]))
    #return y
}

geodf <- lapply(as.character(seq(length(geo))), function(x) geo[[x]][[x]])


#sam as above. Also requires subset
names(mapply(geo[1],length))

#running Geo Hypem

collect_names <- function(l) {
    if (!is.list(l)) return(NULL)
    names <- Map(paste, names(l), lapply(l, collect_names), sep = "$")
    gsub("[^a-zA-Z]", "", unlist(names, use.names = FALSE))
}

n <- collect_names(geo[1])
colnames(geodf) <- n
colnames(geodf2) <- n

geo <- geo[-1]
#geosplit <- lapply(geo2, function(x) split(x, x$i))
z=length(geo)
geodf <- data.frame(matrix(unlist(geo2), nrow=z, byrow=T), stringsAsFactors=FALSE)
geo2 <- geo2[-1]
#geosplit <- lapply(geo2, function(x) split(x, x$i))
z=length(geo2)
geodf2 <- data.frame(matrix(unlist(geo2), nrow=z, byrow=T), stringsAsFactors=FALSE)

geodf <- geodf[-16]
geodf2 <- geodf[-16]
Geo <- rbind(geodf, geodf2)
#Geoc(c(Geo$lovedcount, Geo$postedcount)) <- as.numeric(as.character(c(Geo$lovedcount, Geo$postedcount)))
Geo[, 9:10] <- sapply(Geo[, 9:10], as.numeric)
fit1 <- lm(Geo$lovedcount ~ Geo$postedcount, data=Geo)
summary(fit1)

ggplot(Geo, aes(x=Geo$postedcount, y=Geo$lovedcount)) +
    geom_point(shape=1) +
    geom_smooth(method=lm)

geosplit

for (i in 1:2){
    parts <- unlist(str_split(links[i],"n/"))
    outName <- parts[length(parts)]
    print(outName)
    download.file(links[i],outName)
}

require(plyr)
dat <- ldply(urls,bbcScraper)
dat


#Accessing the numbers
dat$data[[1]]$like_count
dat$data[[1]]$share_count
dat$data[[1]]$comment_count
