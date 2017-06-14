####################
### BibTexMine.R ###
####################
# Author:  Thomas Girke
# Last update: Nov 8, 2006
# Utility: Imports BibTeX DB into R and stores it in list object. From there the individual references 
# can be retrieved and exported to a subsetted BibTeX DB. A search function allows subsetting based on string 
# matching and viewing of the results in HubMed.
#
# How to run the script:
# 	source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/BibTex.txt")

## (1) Load BibTeX DB 
bibtexImp <- function(file) {
	bibtexv <- readLines(file)
	bibtexv <- bibtexv[-grep("^(\\s|\\t){0,}%", bibtexv)] # removes all comment lines
	bibtexv <- bibtexv[-grep("^(\\s|\\t){0,}$", bibtexv)] # removes all empty fields
	bibtexv <- gsub("^(\\s|\t){1,}","", bibtexv) # removes all tabs and spaces at beginning of fields/lines (indenting)
	start <- regexpr("^@", bibtexv, perl=T)
	start <- which(start==1)
	end <- regexpr("^}", bibtexv, perl=T)
	end <- which(end==1)
	if(length(start)!=length(end)) { cat("Error in BibTex source: number of '^@' differes from number of '^}' \n") }
	index <- data.frame(start=start, end=end)
	myfactor <- rep(1:length(index[,1]), index[,2]-index[,1]+1)
	bibtexList <- split(bibtexv, myfactor) # generates BibTex list
	names(bibtexList) <- unlist(lapply(names(bibtexList), function(x) gsub(".*\\{|,$", "", bibtexList[[x]][1])))
	bibtexList
}

## (2) BibTeX Search Function: shows search results in HubMed
bibtexSearch <- function(bibTexDB, string, browser=T) {
	bibTexDBsearch <- lapply(names(bibTexDB), function(x) paste(bibTexDB[[x]], collapse=" "))
	bibTexDBsearch <- lapply(bibTexDBsearch, function(x) gsub("\\{|\\}", "", x)) # removes braces around capital letters (e.g. title line)
	hits <- lapply(bibTexDBsearch, function(x) grep(string, x, ignore.case=T, perl=T))
	hits <- paste(hits)
	hits[hits=="integer(0)"] <- 0
	hits <- which(hits==1)
	hits <<- hits
	cat("The list indices for the search hits are stored in \"hits\" vector containing the following values: \n", hits, "\n")
	bibTexDBsub <- bibTexDB[hits]
	if(browser==T) {
		PubMedIDs <- paste(unlist(lapply(names(bibTexDBsub), function(x) gsub(".*\"(.*)\".*", "\\1", bibTexDBsub[[x]][grep("pmid", bibTexDBsub[[x]])]))), collapse=",")
		myURL <- paste("http://www.hubmed.org/display.cgi?uids=", PubMedIDs, sep="")
		browseURL(myURL)
	}
	noPubMed <- lapply(names(bibTexDBsub), function(x) gsub(".*\"(.*)\".*", "\\1", bibTexDBsub[[x]][grep("pmid", bibTexDBsub[[x]])]))
	noPubMed <- lapply(noPubMed, function(x) x[1]) # necessary to turn empty list components into vector with "NA"
	noPubMed <- is.na(as.numeric(unlist(noPubMed)))
	if(sum(noPubMed)>=1) { 
		noPubMedList <- lapply(names(bibTexDBsub[noPubMed]), function(x) bibTexDBsub[[x]][c(1,12)])
		cat("Warning: these references have no PubMed ID:\n", unlist(noPubMedList), "\n") 
	}
	bibTexDBsub
}

## (3) Return refs in text format
returnText <- function(bibTexSubDB=bibTexSubDB, printurl=T, abs=F) {
	bibTexSubDB <- lapply(bibTexSubDB, function(x) gsub("(= {0,})\\{{1,}|\\}{1,} {0,}(,)$", "\\1\"\\2", x)) # replaces all start and end braces by quotation marks
	authors <- lapply(names(bibTexSubDB), function(x) gsub(".* \\\"(.*)\".*", "\\1", bibTexSubDB[[x]][grep("^author", bibTexSubDB[[x]])]))
	names(authors) <- 1:length(authors)
	authors <- lapply(names(authors), function(x) gsub(" {1,}and {1,}", ", ", authors[x]))
	authors <- lapply(authors, function(x) x[1]) # necessary to turn empty list components into vector with "NA"
	year <- lapply(names(bibTexSubDB), function(x) gsub(".*\"(.*)\".*", "\\1", bibTexSubDB[[x]][grep("^year", bibTexSubDB[[x]])]))
	year <- lapply(year, function(x) x[1]) # necessary to turn empty list components into vector with "NA"
	title <- lapply(names(bibTexSubDB), function(x) gsub(".*\"(.*)\".*", "\\1", bibTexSubDB[[x]][grep("^title", bibTexSubDB[[x]])]))
	names(title) <- 1:length(title)
	title <- lapply(names(title), function(x) gsub("\\{|\\}|,$", "", title[x]))
	title <- lapply(title, function(x) x[1]) # necessary to turn empty list components into vector with "NA"
	journal <- lapply(names(bibTexSubDB), function(x) gsub(".*\"(.*)\".*", "\\1", bibTexSubDB[[x]][grep("^journal", bibTexSubDB[[x]])]))
	journal <- lapply(journal, function(x) x[1]) # necessary to turn empty list components into vector with "NA"
	volume <- lapply(names(bibTexSubDB), function(x) gsub(".*\"(.*)\".*", "\\1", bibTexSubDB[[x]][grep("^volume", bibTexSubDB[[x]])]))
	volume <- lapply(volume, function(x) x[1]) # necessary to turn empty list components into vector with "NA"
	pages <- lapply(names(bibTexSubDB), function(x) gsub(".*\"(.*)\".*", "\\1", bibTexSubDB[[x]][grep("^pages", bibTexSubDB[[x]])]))
	pages <- lapply(pages, function(x) x[1]) # necessary to turn empty list components into vector with "NA"
	abstract <- lapply(names(bibTexSubDB), function(x) gsub(".*\"(.*)\".*", "\\1", bibTexSubDB[[x]][grep("^abstract", bibTexSubDB[[x]])]))
	names(abstract) <- 1:length(abstract)
	abstract <- lapply(names(abstract), function(x) gsub("\\{|\\}|,$", "", abstract[x]))
	abstract <- lapply(abstract, function(x) paste("ABSTRACT:", x[1])) # necessary to turn empty list components into vector with "NA"
	myurl <- lapply(names(bibTexSubDB), function(x) gsub(".*\"(.*)\".*", "\\1", bibTexSubDB[[x]][grep("^url", bibTexSubDB[[x]])]))
	myurl <- lapply(myurl, function(x) x[1]) # necessary to turn empty list components into vector with "NA"
	if(abs==F) {
		if(printurl==T) {
			refv <- paste("\n", unlist(authors), " (", unlist(year), ") ", unlist(title), ". ", unlist(journal), ", ", unlist(volume), ": ", unlist(pages), ".\n", unlist(myurl), "\n", sep="")	
			} else {
			refv <- paste("\n", unlist(authors), " (", unlist(year), ") ", unlist(title), ". ", unlist(journal), ", ", unlist(volume), ": ", unlist(pages), ".\n", sep="")	
		}
	}
	if(abs==T) {
		if(printurl==T) {
			refv <- paste("\n", unlist(authors), " (", unlist(year), ") ", unlist(title), ". ", unlist(journal), ", ", unlist(volume), ": ", unlist(pages), ".\n", unlist(myurl), "\n", unlist(abstract), "\n", sep="")	
			} else {
			refv <- paste("\n", unlist(authors), " (", unlist(year), ") ", unlist(title), ". ", unlist(journal), ", ", unlist(volume), ": ", unlist(pages), ".\n", unlist(abstract), "\n", sep="")	
		}
	}
	refv
}

## (4) Run everything within one function
bibTexAll <- function(file="MyBibTex.bib", string="array", bibtex2PDF=F, printurl=T, abs=T, printtext=T, browser=T) {
	bibTexDB <- bibtexImp(file=file)
	bibTexSubDB <- bibtexSearch(bibTexDB=bibTexDB, string=string, browser=browser)
	if(length(bibTexSubDB)==0) { return("No hits found!") }
	if(printtext==T) {
		cat(sort(returnText(bibTexSubDB=bibTexSubDB, printurl=printurl, abs=abs)), sep="\n")
	}
	if(bibtex2PDF==T) {
		bibdir <- unlist(strsplit(file, "/"))
		filename <- gsub("\\.bib", "", bibdir[length(bibdir)])
		bibdir <- paste(unlist(strsplit(bibdir, "/"))[-length(unlist(strsplit(bibdir, "/")))], collapse="/")
		# mydir <- getwd()
		# setwd(bibdir)
		selected <<- paste(sort(names(bibTexSubDB)), collapse=", ")
		myRnw <- paste(filename, ".Rnw", sep="")
		Sweave(myRnw)
		mytex <- paste(filename, ".tex", sep="")
		system(paste("pdflatex", mytex, "; bibtex", filename, "; pdflatex", mytex, "; pdflatex", mytex))
		# setwd(mydir)
		cat("Open BibTeX PDF with this command:\n\t system(\"xpdf MyBibTex.pdf &\")\n")
	}
}

## (5) Instructionu
cat("USAGE
#(1.1) Import of BibTeX DB
        bibTexDB <- bibtexImp(file=\"~/Manuscripts/BibTeX/MyBibTex.bib\")\n
#(2.1) Search BibTeX DB
	bibTexSubDB <- bibtexSearch(bibTexDB=bibTexDB, string=\"array\", browser=T)\n
#(3.1) Export references in text format
	cat(sort(returnText(bibTexSubDB=bibTexSubDB, printurl=T, abs=F)), sep=\"\\n\") # View result in R.
	cat(sort(returnText(bibTexSubDB=bibTexSubDB, printurl=T, abs=F)), file=\"zzz.bib\", sep=\"\\n\\n\") # Exports results to file.\n
#(3.2) Export bibTexSubDB
	lapply(bibTexSubDB, cat, sep=\"\\n\") # View result in R.
	lapply(bibTexDB[hits], cat, sep=\"\\n\", file=\"zzz.bib\", append=T) # Exports results to file. \n
#(4.1) Search HubMed
	browseURL(\"http://www.hubmed.org/search.cgi?q=promoter+motif+enrichment\")\n
#(4.2) Run almost everything within one function
	bibTexAll(file=\"~/Manuscripts/BibTeX/MyBibTex.bib\", bibtex2PDF=F, printurl=T, abs=F, printtext=T, browser=T, string=\"promoter\")\n"
)




