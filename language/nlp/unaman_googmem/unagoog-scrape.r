library("rvest")

# url: http://gizmodo.com/exclusive-heres-the-full-10-page-anti-diversity-screed-1797564320
# start of main text: "</h3><p>I value"
# end of main text:   "authoritarians.</em>"


# url: http://www.washingtonpost.com/wp-srv/national/longterm/unabomber/manifesto.text.htm
# start of main text: "<h3>INDUSTRIAL"
# end of main text: "freedom.</font>"

urlG <- "http://gizmodo.com/exclusive-heres-the-full-10-page-anti-diversity-screed-1797564320"
startG <- "</h3><p>I value"
stopG <- "authoritarians.</em>"
regexG <- paste(startG, stopG, sep=".+")

urlU <- "http://www.washingtonpost.com/wp-srv/national/longterm/unabomber/manifesto.text.htm"
startU <- "<h3>INDUSTRIAL"
stopU <- "freedom.</font><p>"
regexU <- paste(startU, stopU, sep=".")

download.file(urlG, destfile="text/googal_page.html", quiet=FALSE)
download.file(urlU, destfile="text/unaman_page.html", quiet=FALSE)

# G <- readLines(urlG)
G <- readLines("text/google_page.html", warn=FALSE)
G <- gsub(startG, "BLAOWWIE", G)
G <- gsub(stopG, "BOOSH", G)
G <- grep(pattern="^.+BLAOWWIE.+BOOSH.*$", G, value=TRUE)
G <- paste0("I value ", G, " authoritarians.")
# G <- gsub("BLAOWWIE", "I value", G)
# G <- gsub("BOOSH", "authoritarians", G)
G <- gsub("<.*?>", " ", G)
writeLines(G, "text/gclean.txt")


