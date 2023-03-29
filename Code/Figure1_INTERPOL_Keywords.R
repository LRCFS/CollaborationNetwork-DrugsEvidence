#######################################################
######                 Keywords                  ######
#######################################################

#######################################################
######          INTERPOL Reference List          ######
######              Figure 1                     ######
#######################################################

#####                    Data INTERPOL                  #####
# load INTERPOL data
dat_csv = read.csv("INTERPOL/IFSMS_DG.csv", sep=",", header=TRUE, encoding='UTF-8')

# rename some of the columns to remove special characters or encoding
names(dat_csv)[1:2] <- c("Authors", "AuthorID")

#rename some of the columns for dat_csvReduced
names(dat_csv)[5]<-c("Source.title")
names(dat_csv)[17:18]<-c("Author.Keywords", "Index.Keywords")

dat_csvReduced <- dat_csv %>%
  select(Year,Title,Source.title,Authors,AuthorID,Author.Keywords,Index.Keywords,Report)%>%
  distinct()

#############################################################
#####     Select one of the following three options     #####
#############################################################

# This section is looks at Keywords

if (KeywordEntries == "Author_Keywords"){
  #   Author Keywords only
  names(dat_csvReduced) <- sub("Author.Keywords","AIKeywords", names(dat_csvReduced))
  Keyname <- "A_Keywords"
} else{
  if (KeywordEntries == "Database_Keywords"){
    #   Index Keywords only
    names(dat_csvReduced) <- sub("Index.Keywords","AIKeywords", names(dat_csvReduced))
    Keyname <- "I_Keywords"
  }
  else {
    # Index and Author Keywords
    # Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
    dat_csvReduced <- dat_csvReduced %>%
      unite("AIKeywords", Author.Keywords, Index.Keywords,sep = ";", remove = TRUE)
    Keyname <- "AI_Keywords"
  }}

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
DatasetKeywordList <- dat_csvReduced %>% 
  mutate(AIKeywords = strsplit(as.character(AIKeywords), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIKeywords" in "DatasetKeywordList" and save in dataframe
# Extract list of "AIkeywords" and remove duplicate
DatasetKeywordList$AIKeywords <- toupper(DatasetKeywordList$AIKeywords)

KeywordList <- DatasetKeywordList %>%
  select(AIKeywords)
Keyword <- KeywordList %>%
  distinct()

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list
KeywordsCorrected <- read.csv("CorrectionLists/KeywordsCorrectionFull.txt", sep="\t", header=TRUE)
KeywordsCorrected <- as.data.frame(KeywordsCorrected)

# KeywordsCorrected <- as.data.frame(KeywordsCorrected)
# DatasetKeywordList$KeywordsCorrected <- gsr(as.character(DatasetKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorrectedAIKeywords))
DatasetKeywordList$KeywordsCorrected <- gsr(as.character(DatasetKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorAIKeywordsAcronym))

# number of distinct Keywords after correction
KeywordList2 <- DatasetKeywordList %>%
  select(KeywordsCorrected) %>%
  distinct()

#############################################################
#####               Data analysis - Keywords            #####
#############################################################

#Count to number of time the same year is repeated in the "DatasetKeywordList$Year" and save in a data.frame "Year" 
PublicationYear<- data.frame(table(dat_csvReduced$Year))
names(PublicationYear) <- c("Year","Publications")
PublicationYear$Year <- as.numeric(as.character(PublicationYear$Year))

#count the number of keywords per title paper 
DatasetKeywordListTemp1 <- DatasetKeywordList  %>%
  select(Year,Title,Source.title,KeywordsCorrected) %>%
  distinct()
DatasetKeywordListTemp2 <-DatasetKeywordListTemp1[complete.cases(DatasetKeywordListTemp1), ]
sum(is.na(DatasetKeywordListTemp2$KeywordsCorrected))

DatasetKeywordYearCount <- aggregate(DatasetKeywordListTemp2$Year, by=list(Year=DatasetKeywordListTemp2$Year, Keyword=DatasetKeywordListTemp2$KeywordsCorrected), FUN=length)
DatasetKeywordTotalCount <- aggregate(DatasetKeywordListTemp2$Year, by=list(Keyword=DatasetKeywordListTemp2$KeywordsCorrected), FUN=length)

# narrowing range for plot
DatasetKeywordNarrowRangeGraph <- top_n(DatasetKeywordTotalCount, Count)

# count the number of rows, hense the number of keywords in figure
a <- nrow(DatasetKeywordNarrowRangeGraph)

while (a>maximum) {
  Count <- Count-1
  DatasetKeywordNarrowRangeGraph <- top_n(DatasetKeywordTotalCount, Count)
  a <- nrow(DatasetKeywordNarrowRangeGraph)
}

# DatasetKeywordNarrowRangeGraph <- subset(DatasetKeywordTotalCount,x>Count)
SubsetKeywordNarrowRangeGraph <-subset(DatasetKeywordYearCount,Keyword %in% DatasetKeywordNarrowRangeGraph$Keyword)

# this is to set the range for the keyword figure
range <- as.numeric(max(SubsetKeywordNarrowRangeGraph$x, na.rm = TRUE))

source("Functions/KeywordRange.R")

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence (breaks to be changed to fit Interpol vs. Scopus data)
#Breaks and labels for Interpol
SubsetKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetKeywordNarrowRangeGraph$x,
                                                     breaks = c(BreakRange,max(SubsetKeywordNarrowRangeGraph$x,na.rm=T)),
                                                     labels=DatasetRange)

GraphTemp1 <- SubsetKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Keyword,levels=rev(sort(unique(Keyword))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(BreakRange,max(x,na.rm=T)),
                         labels=DatasetRange))  %>%
  
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
GraphTemp2 <- aggregate(GraphTemp1[, 1], list(GraphTemp1$KeywordsCorrected), min)

GraphTemp1$graphorder <- as.numeric(gsr(GraphTemp1$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))

# further modified ggplot
p <- ggplot(GraphTemp1,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015,2020))+
  scale_fill_manual(values=c(pal),na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=8)+
  theme(text = element_text(family = "Arial"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(-0.2,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))

#save figure
Var1 <- paste0("Fig_1_INTERPOL_",KeywordEntries)

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), p, width = 6.88, height = 8.5, units = "in", dpi=300)

#Export to top keywords list
write.csv(SubsetKeywordNarrowRangeGraph, file=paste0(Results.dir,sprintf("%s.csv",Var1)), row.names = F)

print("Processing complete. Please check 'Results/' and 'Figures/' folders for output")
