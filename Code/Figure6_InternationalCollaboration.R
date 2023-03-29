
#############################################################
#####                     countries                     #####
#############################################################

# get city/country data
data(world.cities)

#######################################################
###### Percentage international collaboration    ######
######              Figure 6                     ######
#######################################################

dat_csv_temp <- read.csv(file = "Scopus/NPS-LegalHighs/ScopusNPS-Countries.csv")
dat_csv_temp_SE <- read.csv(file = "ScienceAndEngineering/ScienceAndEngineering2020.csv")


dat_csv_temp3 <- dat_csv_temp
dat_csv_temp3$CountryCount <- str_count(dat_csv_temp3$CountryName, ',')
dat_csv_temp3$CountryCount <- dat_csv_temp3$CountryCount + 1

dat_csv_temp3 <- dat_csv_temp3 %>%
  filter(CountryCount > 0)

dat_csv_temp3$Affiliations <- gsub("\\.", "", dat_csv_temp3$Affiliations)
dat_csv_temp3$Affiliations <- gsub("USA\\, United States", "USA", dat_csv_temp3$Affiliations)
dat_csv_temp3$Affiliations <- gsub("Russian Federation", "Russia", dat_csv_temp3$Affiliations)
dat_csv_temp3$Affiliations <- gsub("Cairo Egypt", "Cairo\\, Egypt", dat_csv_temp3$Affiliations)
dat_csv_temp3$Affiliations <- gsub("Hong Kong", "Hong Kong\\, China", dat_csv_temp3$Affiliations)

# replace "United States" with USA & "United Kingdom" with UK.
aff.lst <- gsub("United States", "USA", dat_csv_temp3$Affiliations, perl = TRUE)
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

cntry.dat <- data.frame(Country = removeDiacritics(unlist(cntry.lst)), stringsAsFactors = FALSE)

# define continent for each country
cntry.dat$Continent <- countrycode(sourcevar = cntry.dat[, "Country"],
                                   origin = "country.name",
                                   destination = "continent")

# Collate counts for countries over threshold
cntry.dat.Total <- cntry.dat %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n())

names(cntry.dat.Total)[3] <- "TotalCountryCount"

############################################################
#####       repeat for external collaboration         ######
############################################################
dat_csv_temp3 <- dat_csv_temp
dat_csv_temp3$CountryCount <- str_count(dat_csv_temp3$CountryName, ',')
dat_csv_temp3$CountryCount <- dat_csv_temp3$CountryCount + 1

dat_csv_temp3 <- dat_csv_temp3 %>%
  filter(CountryCount > 1)

dat_csv_temp3$Affiliations <- gsub("\\.", "", dat_csv_temp3$Affiliations)
dat_csv_temp3$Affiliations <- gsub("USA\\, United States", "USA", dat_csv_temp3$Affiliations)
dat_csv_temp3$Affiliations <- gsub("Russian Federation", "Russia", dat_csv_temp3$Affiliations)
dat_csv_temp3$Affiliations <- gsub("Cairo Egypt", "Cairo\\, Egypt", dat_csv_temp3$Affiliations)
dat_csv_temp3$Affiliations <- gsub("Hong Kong", "Hong Kong\\, China", dat_csv_temp3$Affiliations)

# replace "United States" with USA & "United Kingdom" with UK.
aff.lst <- gsub("United States", "USA", dat_csv_temp3$Affiliations, perl = TRUE)
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

cntry.dat <- data.frame(Country = removeDiacritics(unlist(cntry.lst)), stringsAsFactors = FALSE)

# define continent for each country
cntry.dat$Continent <- countrycode(sourcevar = cntry.dat[, "Country"],
                                   origin = "country.name",
                                   destination = "continent")

# Collate counts for countries over threshold
cntry.dat.External.Col <- cntry.dat %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n())

names(cntry.dat.External.Col)[3] <- "ExternalCountryCount"

cntry.dat.Summary <- full_join(cntry.dat.Total, cntry.dat.External.Col)

cntry.dat.Summary$NPS_Scopus <- (cntry.dat.Summary$ExternalCountryCount/cntry.dat.Summary$TotalCountryCount)*100

cntry.dat.Summary <- full_join(cntry.dat.Summary, dat_csv_temp_SE)

## generate plot per country
threshold <- 40 

# Collate counts for countries over threshold
cntry.dat.Summary <- cntry.dat.Summary %>% 
  filter(TotalCountryCount > threshold)
# order by count

cntry.dat.Summary$Country <-  gsub("Serbia","Serbia *",cntry.dat.Summary$Country)
cntry.dat.Summary$Country <-  gsub("Romania","Romania *",cntry.dat.Summary$Country)
cntry.dat.Summary$Country <-  gsub("Nigeria","Nigeria *",cntry.dat.Summary$Country)
cntry.dat.Summary$Country <-  gsub("Hungary","Hungary *",cntry.dat.Summary$Country)
cntry.dat.Summary$Country <-  gsub("Czech Republic","Czech Republic *",cntry.dat.Summary$Country)
cntry.dat.Summary$Country <-  gsub("Croatia","Croatia *",cntry.dat.Summary$Country)

cntry.dat.Summary$Country <- reorder(cntry.dat.Summary$Country, cntry.dat.Summary$NPS_Scopus)
cntry.dat.Summary$Country <- as.factor(cntry.dat.Summary$Country)

cntry.dat.Summary$Science_Eng <- cntry.dat.Summary$SE_Percentage - cntry.dat.Summary$NPS_Scopus

cntry.dat.Summary.l <- cntry.dat.Summary %>%
  select(Country, NPS_Scopus, Science_Eng) %>%
  gather(source, count, -Country)

# plot
p <-  ggplot(cntry.dat.Summary, (aes(x=Country, y=NPS_Scopus, fill=Continent))) + 
  geom_col() +
  scale_fill_manual(values = c("gray", brewer.pal(5, "Set1")), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania")) +
  xlab('Country Affiliation') +
  ylab('Percentage of outputs with international collaboration') +
  coord_flip() +
  theme_minimal() +
    theme(text = element_text(family = "Arial"))

show(p)

ggsave(paste0(Figure.dir,"Fig_6_NPS_Scopus_Country_International_Collaboration.png"), p, width = 8, height = 6, units = "in", dpi=1200)

# plot as stacked barplot
plt1 = ggplot(cntry.dat.Summary.l, aes(x = as.factor(Country), y = count, fill = rev(source))) + 
  geom_col() + 
  coord_flip() + 
  #scale_fill_manual(values = c("gray", brewer.pal(5, "Set1")), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania")) +
  scale_fill_manual(labels = c('Science &\nEngineering [38]','NPS references\non Scopus'), values = brewer.pal(3, 'Blues')[1:2]) + 
  #ggtitle('IFSMS Report') +
  xlab('Country Affiliation') +
  ylab('Percentage of outputs with international collaboration') +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major.y = element_blank(),legend.key.size = unit(1.0, "cm"))

show(plt1)

ggsave(paste0(Figure.dir,"Fig_6_NPS_Scopus_Country_International_Collaboration_S_And_E.tiff"), plt1, width = 8, height = 6, units = "in", dpi=300)

