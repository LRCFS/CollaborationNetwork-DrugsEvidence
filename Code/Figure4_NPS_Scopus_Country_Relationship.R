#######################################################
######            Keywords vs Country            ######
#######################################################

#######################################################
######           Scopus NPS references           ######
######              Figure 4                     ######
#######################################################

Dataset <- read.csv("Scopus/NPS-LegalHighs/scopusNPS.csv", header = TRUE)

# rename some of the columns to remove special characters or encoding
names(Dataset)[1:2] <- c("Authors", "AuthorID")
names(Dataset)[5]<-c("Source.title")
names(Dataset)[18:19]<-c("Author.Keywords", "Index.Keywords")

#############################################################
#####                     countries                     #####
#############################################################

# get city/country data
data(world.cities)

Dataset$Affiliations <- gsub("\\.", "", Dataset$Affiliations)
Dataset$Affiliations <- gsub("USA\\, United States", "USA", Dataset$Affiliations)
Dataset$Affiliations <- gsub("Russian Federation", "Russia", Dataset$Affiliations)
Dataset$Affiliations <- gsub("Cairo Egypt", "Cairo\\, Egypt", Dataset$Affiliations)
Dataset$Affiliations <- gsub("Hong Kong", "Hong Kong\\, China", Dataset$Affiliations)

# replace "United States" with USA & "United Kingdom" with UK.
aff.lst <- gsub("United States", "USA", Dataset$Affiliations, perl = TRUE)
aff.lst <- gsub("United Kingdom", "UK", aff.lst, perl = TRUE)
aff.lst <- gsub("South Korea", "Korea South", aff.lst, perl = TRUE)

# replace ';' with ',' as multiple affiliations are separated with ';'
# but that doesn't fit with the strsplit()
aff.lst <- gsub(";", ",", aff.lst)
# split fields by ", "
splt.lst <- sapply(aff.lst, strsplit, split = ", ", USE.NAMES = FALSE)
# extract fields which match a known city making sure that diacritics aren't a problem...
city.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])
# ... or country
# cntry.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])
# this version only returns unique instances of countries per publication
cntry.lst <- lapply(splt.lst, function(x)unique(x[which(x %in% world.cities$country.etc)]))

#extract the list of country per paper and place in dataframe
cntry.lst.dat <- as.data.table(matrix(cntry.lst),stringsAsFactors=FALSE)

# bind to the original data
dat_csv_temp <- cbind(Dataset,cntry.lst.dat)

# cntry.lst.dat.map <- Map(as.data.table,cntry.lst)
# cntry.lst.dat <- rbindlist(cntry.lst.dat.map)

#convert to character - may not be necessary
dat_csv_temp$V1 <- as.character(dat_csv_temp$V1)

# remove unwanted characters  
dat_csv_temp$V1 <-  gsub("c\\(","",dat_csv_temp$V1)
dat_csv_temp$V1 <-  gsub("\\)","",dat_csv_temp$V1)
dat_csv_temp$V1 <-  gsub("\"","",dat_csv_temp$V1)
dat_csv_temp$V1 <-  gsub(", ",",",dat_csv_temp$V1)
dat_csv_temp$V1 <-  gsub("character\\(0",NA,dat_csv_temp$V1)

# rename column to country
names(dat_csv_temp)[26] <- "CountryName"

# write to file and read again to remove unwanted list !!! Should look  for a better solution
write.csv(dat_csv_temp,file = "Scopus/NPS-LegalHighs/ScopusNPS-Countries.csv",row.names = FALSE)

dat_csv_temp <- read.csv(file = "Scopus/NPS-LegalHighs/ScopusNPS-Countries.csv")
dat_csv_temp <- as.data.frame(dat_csv_temp)
# Select column label $Year, $Title,  $Source.title, $Author.Keywords, $Index.Keyword
dat_csvReduced <- dat_csv_temp %>%
  select(Year,Title,EID,Abstract,Source.title,Authors,AuthorID,Author.Keywords,Index.Keywords,CountryName)%>%
  distinct()


# dat_csv_temp_Reduced <- dat_csv_temp %>%
#   select(Affiliations, CountryName)

#############################################################
#####                     Keywords                      #####
#############################################################

# This section is looks at Keywords

# Select column label $Year, $Title,  $Source.title, $Author.Keywords, $Index.Keyword
ReducedDataSet <- dat_csv_temp %>%
  select(Year,Title,Source.title,Abstract,Author.Keywords,Index.Keywords,EID,CountryName)%>%
  distinct()

if (KeywordEntries == "Author.Keywords"){
  #   Author Keywords only
  names(ReducedDataSet) <- sub("Author.Keywords","AIKeywords", names(ReducedDataSet))
  Keyname <- "A_Keywords"
} else{
  if (KeywordEntries == "Database.Keywords"){
    #   Index Keywords only
    names(ReducedDataSet) <- sub("Index.Keywords","AIKeywords", names(ReducedDataSet))
    Keyname <- "I_Keywords"
  }
  else {
    # Index and Author Keywords
    # Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
    ReducedDataSet <- ReducedDataSet %>%
      unite("AIKeywords", Author.Keywords, Index.Keywords,sep = ";", remove = TRUE)
    Keyname <- "AI_Keywords"
  }}

#############################################################

docs<- ReducedDataSet %>%
  select(EID, Abstract, Title, AIKeywords)

docs$Abstract <- gsub('[^[:alnum:] ]', ' ', docs$Abstract)
docs$Abstract <- gsub("\\s+", " ", docs$Abstract)

docs$Abstract <- tolower(docs$Abstract)

docs$Title <- gsub('[^[:alnum:] ]', ' ', docs$Title)
docs$Title <- gsub("\\s+", " ", docs$Title)

docs$Title <- tolower(docs$Title)

docs$AIKeywords <- gsub('[^[:alnum:] ]', ' ', docs$AIKeywords)
docs$AIKeywords <- gsub("\\s+", " ", docs$AIKeywords)

docs$AIKeywords <- tolower(docs$AIKeywords)

# Load the Corpus of interest to search in the Scopus output entries
NPSCorpusList <- read.csv("Scopus/NPS-LegalHighs/NPS-Corpus-List.txt", sep = "\t", header = TRUE)

NPSCorpusList$HighResNPS <- NPSCorpusList$NPSCompound

NPSCorpusList$NPSCompound <- gsub('[^[:alnum:] ]', ' ', NPSCorpusList$NPSCompound)
NPSCorpusList$NPSCompound <- gsub("\\s+", " ", NPSCorpusList$NPSCompound)

NPSCorpusList$NPSCompound <- tolower(NPSCorpusList$NPSCompound)
NPSCorpusList$NPSCompound2 <- paste(" ", NPSCorpusList$NPSCompound, " ", sep = "")
NPSCorpusList$NPSCompound2 <- gsub('  ', ' ', NPSCorpusList$NPSCompound2)

# Convert to Object
strings <- NPSCorpusList$NPSCompound2

# strings   <- c("synthetic", "methylenedioxypyrovalerone", "new psychoactive substances")

texts <- docs

# #########################################################
# #####                For full text                  #####
# 
# docstemp <- read.table("Scopus/FullTest.txt", header = TRUE)
# 
# docstemp$FullText <- gsub('[^[:alnum:] ]', ' ', docstemp$FullText)
# docstemp$FullText <- gsub('      ', ' ', docstemp$FullText)
# docstemp$FullText <- gsub('     ', ' ', docstemp$FullText)
# docstemp$FullText <- gsub('    ', ' ', docstemp$FullText)
# docstemp$FullText <- gsub('   ', ' ', docstemp$FullText)
# docstemp$FullText <- gsub('  ', ' ', docstemp$FullText)
# 
# docstemp$FullText <- tolower(docstemp$FullText)
# 
# texts <- docstemp
# 
# ##############################################
# # Check for presence of each word in Full text
# Fulltext <- vector("list", nrow(texts))
# 
# for(d in 1:nrow(texts)){
#   for(w in seq_along(strings)){
#     intermed   <- grep(strings[[w]], texts[[d,1]])
#     Fulltext[[d]] <- c(Fulltext[[d]], 
#                        strings[[w]][ (length(intermed) > 0) ])
#   }
# }
# 
# # combined together
# FullTEXT <- sapply(Fulltext, paste0, collapse=";")
# FullTEXT <- as.data.frame(FullTEXT)
# 
# FullTEXT_temp <- data.frame(texts[,1],FullTEXT)
# 
# ##############################################

##############################################
# Check for presence of each word in Abstract
abstract <- vector("list", nrow(texts))

for(d in 1:nrow(texts)){
  for(w in seq_along(strings)){
    intermed   <- grep(strings[[w]], texts[[d,2]])
    abstract[[d]] <- c(abstract[[d]], 
                       strings[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
Abstract <- sapply(abstract, paste0, collapse=";")
Abstract <- as.data.frame(Abstract)

EID_Abstract <- data.frame(texts[,1],Abstract)

# remane fist column to match original dataset 
names(EID_Abstract)[1] <- c("EID")

##############################################
# Check for presence of each word in Title
titles <- vector("list", nrow(texts))

for(d in 1:nrow(texts)){
  for(w in seq_along(strings)){
    intermed   <- grep(strings[[w]], texts[[d,3]])
    titles[[d]] <- c(titles[[d]], 
                     strings[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
Titles    <- sapply(titles, paste0, collapse=";")
Titles <- as.data.frame(Titles)

EID_Titles <- data.frame(texts[,1],Titles)

# remane fist column to match original dataset 
names(EID_Titles)[1] <- c("EID")

EID_Abstract_Titles <- full_join(EID_Abstract,EID_Titles)

##############################################
# Check for presence of each word in Keywords
keywords <- vector("list", nrow(texts))

for(d in 1:nrow(texts)){
  for(w in seq_along(strings)){
    intermed   <- grep(strings[[w]], texts[[d,4]])
    keywords[[d]] <- c(keywords[[d]], 
                       strings[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
Keywords    <- sapply(keywords, paste0, collapse=";")
Keywords <- as.data.frame(Keywords)

EID_Keywords <- data.frame(texts[,1],Keywords)

# remane fist column to match original dataset 
names(EID_Keywords)[1] <- c("EID")

EID_Abstract_Titles_Keywords <- full_join(EID_Abstract_Titles,EID_Keywords)

############
# Combine Columns Abstract,Titles,Keywords and place in Column name "NPS_Corpus" and remove original columns
EID_Abstract_Titles_Keywords <- EID_Abstract_Titles_Keywords %>%
  unite("NPS_Corpus", Abstract, Titles, Keywords,sep = ";", remove = TRUE)

############
# get the EID and respective country names to join to the EID_Abstract_Titles_Keywords

EID_Country <- ReducedDataSet %>%
  select(EID,Year,CountryName)

EID_Abstract_Titles_Keywords_Country <- full_join(EID_Abstract_Titles_Keywords,EID_Country)

# count the number of records with a match to the NPS Corpus List 
EID_Abstract_Titles_Keywords_CountryTemp <- EID_Abstract_Titles_Keywords_Country
EID_Abstract_Titles_Keywords_CountryTemp$NPS_Corpus <- gsub(";;","",EID_Abstract_Titles_Keywords_CountryTemp$NPS_Corpus)
# replace blank with NA
EID_Abstract_Titles_Keywords_CountryTemp[EID_Abstract_Titles_Keywords_CountryTemp==""] <- NA
EID_Abstract_Titles_Keywords_CountryTemp <- EID_Abstract_Titles_Keywords_CountryTemp[complete.cases(EID_Abstract_Titles_Keywords_CountryTemp[ ,2]),]

############
#Split Column "Country" in row by the separator ",", remove leading white space to generate list
dat_csvKeywordListCountry <- EID_Abstract_Titles_Keywords_Country %>% 
  mutate(CountryName = strsplit(as.character(CountryName), ",")) %>% 
  unnest(CountryName) %>%
  mutate_if(is.character, str_trim)

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
dat_csvKeywordList <- dat_csvKeywordListCountry %>% 
  mutate(NPS_Corpus = strsplit(as.character(NPS_Corpus), ";")) %>% 
  unnest(NPS_Corpus) %>%
  mutate_if(is.character, str_trim)

# remove duplicates
dat_csvKeywordList <- dat_csvKeywordList %>%
  distinct()

# replace blank with NA
dat_csvKeywordList[dat_csvKeywordList==""] <- NA
# remove NA in NPS_corpus
dat_csvKeywordList <- dat_csvKeywordList[complete.cases(dat_csvKeywordList[ ,2]),]

dat_csvKeyword_CountryList <- dat_csvKeywordList %>%
  select(NPS_Corpus,CountryName)

dat_csvKeyword_CountryList2 <- dat_csvKeywordList %>%
  select(NPS_Corpus,CountryName) %>%
  distinct()

dat_csvKeyword_Country_Count <- aggregate(dat_csvKeyword_CountryList$CountryName, by=list(CountryName=dat_csvKeyword_CountryList$CountryName, NPS_Corpus=dat_csvKeyword_CountryList$NPS_Corpus), FUN=length)

# count the number of match to the NPS_Corpus list per country
dat_csvKeyword_Country_Country_Count <- aggregate(dat_csvKeyword_CountryList2$CountryName, by=list(CountryName=dat_csvKeyword_CountryList2$CountryName), FUN=length)

#select the top 10 countries with the most matching keywords
dat_csvKeyword_Country_Country_Count <- top_n(dat_csvKeyword_Country_Country_Count,70)

# subset against the top list
dat_csvKeyword_Country_Count_reduced <- subset(dat_csvKeyword_Country_Count, CountryName %in% dat_csvKeyword_Country_Country_Count$CountryName)

##################################################################
# select one of the two possible display for match to NPS_Corpus #
##################################################################

##### Either combined across all countries together

# This will include the top n countries with the highest number of NPS_Corpus match. This is different from the country map output as this would include all records regardless of match.
# This part is however bias toward country with the highest number of records as they may focus on specific entries

# count the number of match to the NPS_Corpus
dat_csvKeyword_Keyword_Count <- aggregate(dat_csvKeyword_CountryList$NPS_Corpus, by=list(NPS_Corpus = dat_csvKeyword_CountryList$NPS_Corpus), FUN=length)

#select the top 100 NPS_Corpus appearing in list
dat_csvKeyword_Keyword_Count <- top_n(dat_csvKeyword_Keyword_Count,100)
write.csv(dat_csvKeyword_Keyword_Count, file = "dat_csvKeyword_Keyword_Count.csv", row.names = FALSE)

# subset against the top list
dat_csvKeyword_Country_NPS_Corpus_reduced <- subset(dat_csvKeyword_Country_Count_reduced, NPS_Corpus %in% dat_csvKeyword_Keyword_Count$NPS_Corpus)


##### OR by considering the top match of each country separately before merging together

# This part instead consider the top n countries and return their individual top n list of NPS_orpus. They are then meged together in a list. 
dat_csvKeyword_Country_Country_Count_list <- dat_csvKeyword_Country_Country_Count$CountryName
# remove existing dataframe that may have been run on previous option selection
rm(dat_csvKeyword_Keyword_Count)

for (i in 1:length(dat_csvKeyword_Country_Country_Count_list)){
  
  dat_csvKeyword_Country_Count_reduced_temp <- dat_csvKeyword_CountryList %>%
    filter(CountryName == dat_csvKeyword_Country_Country_Count_list[i])
  
  # count the number of match to the NPS_Corpus
  dat_csvKeyword_Keyword_Count_temp <- aggregate(dat_csvKeyword_Country_Count_reduced_temp$NPS_Corpus, by=list(NPS_Corpus = dat_csvKeyword_Country_Count_reduced_temp$NPS_Corpus), FUN=length)
  
  #select the top 100 NPS_Corpus appearing in list
  dat_csvKeyword_Keyword_Count_temp <- top_n(dat_csvKeyword_Keyword_Count_temp,3)
  dat_csvKeyword_Keyword_Count_temp$CountryName <- dat_csvKeyword_Country_Country_Count_list[i]
  dat_csvKeyword_Keyword_Count_temp <- dat_csvKeyword_Keyword_Count_temp %>%
    select(CountryName,NPS_Corpus,x)
  # if the merged dataset doesn't exist, create it
  if (!exists("dat_csvKeyword_Keyword_Count")){
    dat_csvKeyword_Keyword_Count <- dat_csvKeyword_Keyword_Count_temp
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dat_csvKeyword_Keyword_Count")){
    dat_csvKeyword_Keyword_Count <-rbind(dat_csvKeyword_Keyword_Count, dat_csvKeyword_Keyword_Count_temp)
  }
  
}

# subset against the top list
dat_csvKeyword_Country_NPS_Corpus_reduced <- subset(dat_csvKeyword_Country_Count_reduced, NPS_Corpus %in% dat_csvKeyword_Keyword_Count$NPS_Corpus)

#write.table(dat_csvKeyword_CountryList, file = "Keyword_CountryList.txt", quote = F, sep = "\t", row.names = F)

############################
##### For top keywords #####
############################

names(dat_csvKeyword_Country_NPS_Corpus_reduced)[3] <- "Frequency"
dat_csvKeyword_Country_NPS_Corpus_reduced$CountryName <- as.factor(dat_csvKeyword_Country_NPS_Corpus_reduced$CountryName)
dat_csvKeyword_Country_NPS_Corpus_reduced$NPS_Corpus <- as.factor(dat_csvKeyword_Country_NPS_Corpus_reduced$NPS_Corpus)
dat_csvKeyword_Country_NPS_Corpus_reduced$NPS_Corpus <- toupper(dat_csvKeyword_Country_NPS_Corpus_reduced$NPS_Corpus)
# dat_csvKeyword_Country_NPS_Corpus_reduced$Frequency <- factor(dat_csvKeyword_Country_NPS_Corpus_reduced$Frequency)

# ordering keywords in plot by most frequent across all countries
Graph_order_keywords <- aggregate(dat_csvKeyword_Country_NPS_Corpus_reduced$NPS_Corpus, by=list(dat_csvKeyword_Country_NPS_Corpus_reduced$NPS_Corpus), FUN=length)

dat_csvKeyword_Country_NPS_Corpus_reduced$Graph_order_Keywords <-  gsr(as.character(dat_csvKeyword_Country_NPS_Corpus_reduced$NPS_Corpus),as.character(Graph_order_keywords$Group.1),Graph_order_keywords$x)
dat_csvKeyword_Country_NPS_Corpus_reduced$Graph_order_Keywords <- as.numeric(dat_csvKeyword_Country_NPS_Corpus_reduced$Graph_order_Keywords)

# ordering countries by highest number of keywords
Graph_order_countries <- aggregate(dat_csvKeyword_Country_NPS_Corpus_reduced$CountryName, by=list(dat_csvKeyword_Country_NPS_Corpus_reduced$CountryName), FUN=length)

dat_csvKeyword_Country_NPS_Corpus_reduced$Graph_order_Countries <-  gsr(as.character(dat_csvKeyword_Country_NPS_Corpus_reduced$CountryName),as.character(Graph_order_countries$Group.1),Graph_order_countries$x)
dat_csvKeyword_Country_NPS_Corpus_reduced$Graph_order_Countries <- as.numeric(dat_csvKeyword_Country_NPS_Corpus_reduced$Graph_order_Countries)

# this is to set the range for the keyword figure
dat_csvKeyword_Country_NPS_Corpus_reduced$groups <- cut(dat_csvKeyword_Country_NPS_Corpus_reduced$Frequency,               # Add group column
                                                        breaks = c(-1, 0, 2, 5, 10, 50, 100, 400, max(dat_csvKeyword_Country_NPS_Corpus_reduced$Frequency,na.rm = T)),
                                                        labels=c("0","1-2","3-5","6-10","11-50","51-100","101-400",">400"))

GraphTemp1 <- dat_csvKeyword_Country_NPS_Corpus_reduced %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(NPS_Corpus,levels=rev(sort(unique(NPS_Corpus))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(Frequency,breaks=c(-1, 0, 2, 5, 10, 50, 100, 400, max(Frequency,na.rm = T)),
                         labels=c("0","1-2","3-5","6-10","11-50","51-100","101-400",">400")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# ScopusKeywordList$WYear <- gsr(ScopusKeywordList$Year,year$Var1,1/year$Freq)

# Change the name back to the their format in HighResNPS
GraphTemp1$NPS_Corpus <- tolower(GraphTemp1$NPS_Corpus)

GraphTemp1$HighResNPS <-  gsr(as.character(GraphTemp1$NPS_Corpus),as.character(NPSCorpusList$NPSCompound),NPSCorpusList$HighResNPS)

textcol <- "black"

p <- ggplot(GraphTemp1,aes(x=reorder(CountryName, Graph_order_Countries),y=reorder(HighResNPS,desc(Graph_order_Keywords)),fill=countfactor))+
  geom_tile(colour="white",size=0.2) + 
  guides(fill=guide_legend(title="Count")) +
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#d5ee52","#77c86c","#66afc6","#ddf1da"),na.value = "grey90") +
  theme(text = element_text(family = "Palatino"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=6),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.y=element_text(size=5, vjust=0.2),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size=5, angle = 60, hjust=1))

# show(p)

#save figure
Var1 <- paste0("Fig_4_NPS_Corpus_Country")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), p, width = 6.88, height = 8.5, units = "in", dpi=300)

print("Processing complete. Please check 'Figures/' folder for output")
