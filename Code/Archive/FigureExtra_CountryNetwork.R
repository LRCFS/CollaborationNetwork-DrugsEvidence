#######################################################
######             Authors' network              ######
#######################################################

#######################################################
######           Scopus NPS references           ######
######              Figure extra                 ######
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
# AuthorListTemp1 <- CAIKeywordTemp1 %>%
#   select(Year,Title,Author.s..ID)

# Split Column "AuthorID" in row by first replacing "," with the separator ";" and place it in AuthorListTemp2Extended
AuthorListTemp2 <- dat_csv_temp %>%
  select(Year,Title,Authors,EID, CountryName) #%>%

# expand the country list for each record
AuthorListTemp2Extended <- AuthorListTemp2 %>% 
  mutate(Country = strsplit(as.character(CountryName), ","))%>% 
  unnest(Country) %>%
  mutate_if(is.character, str_trim)

# filter against a specific country
# Ireland

CountryChoice <- "Ireland"

AuthorListTemp2Extended <- AuthorListTemp2Extended %>% 
  filter(Country == CountryChoice )

# expand the AuthorID list for each record
AuthorListTemp2ExtendedAuthors <- AuthorListTemp2Extended %>% 
  mutate(Author = strsplit(as.character(Authors), ","))%>% 
  unnest(Author) %>%
  mutate_if(is.character, str_trim)

GephiNodes <- AuthorListTemp2ExtendedAuthors %>%
  select(Author) %>% distinct()

GephiNodes$ID <- GephiNodes$Author

names(GephiNodes)[1] <- "ID"
names(GephiNodes)[2] <- "Label"

# export the Nodes table for Gephi import
write.table(GephiNodes,paste0(Results.dir,paste0(CountryChoice,"_AuthorGephiNodes.csv")),sep = ",", row.names = F, quote = F)

# create the adjacency list for Gephi
AdjacencyListGephi <- AuthorListTemp2Extended %>%
  select(Authors)

# export the adjacency list for Gephi import
write.table(AdjacencyListGephi,paste0(Results.dir,paste0(CountryChoice,"AdjacencyList.csv")), row.names = F, quote=F,col.names=FALSE)

# For Gephi, import the Adjacency list first, selecting comma instead of space separator.
# In Data Laboratory, in Nodes, Import Spreadsheet
# Select the Nodes files (i.e. GephiNodes.csv), importing as a nodes table, click Next and Finish
# On the next Screen, make sure to select "Append to existing workspace"
