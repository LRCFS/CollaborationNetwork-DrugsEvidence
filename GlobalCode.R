###########################################################################
#
# TBC - Copyright (C) 2021
#
# Leverhulme Research Centre for Forensic Science

# Centre for Forensic Science, Department of Pure and Applied Chemistry,
# University of Strathclyde, Royal College, 204 George Street, Glasgow
# Chiron AS, Stiklestadveien 1, NO-7041 Trondheim, Norway

# Caitlyn Norman, Dorothy Xi Yue Lim , Taylor Henderson, Fabio Casali,
# Craig McKenzie, Niamh Nic Daéid, Lorna Nisbet, Hervé Ménard

# Website: https://github.com/HerveMenard/CollaborationNetwork-DrugsEvidence
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

###########################################################################

# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported are generated from Scopus and Web Of Science databases
# The columns will need to contain:
# Year; Title; Source.title; Authors; AuthorID; DE; DES; EID; SO; DT
library(plyr)
library(dplyr)
library(expss)
library(stringr)
library(tidyr)
library(ggplot2)
library(maps)
library(countrycode)
library(RColorBrewer)
library(tidyverse)
library(reshape2)
library(tikzDevice)
library(Cairo)
library(extrafont)
library(ggmap)
library(expss)
library(mgsub)

#############################################################
#####                      Function                     #####
#############################################################

source("Functions/SearchAndReplace.R")

#############################################################
#####                      Function                     #####
#############################################################

# function to replace accented characters with unaccented equivalents 
# adapted from https://stackoverflow.com/questions/15253954/replace-multiple-letters-with-accents-with-gsub
removeDiacritics <- function(string) {
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöøùúûüýÿ",
    "SZszYAAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyy", 
    string
  )
}

#############################################################
#####                 Folders and files                 #####
#############################################################

# set extension and Citation
extension <- ".csv"
cit.path.INTERPOL <- "INTERPOL/"
cit.path.SCOPUS.DRUGS <- "Scopus/Drugs-Forensic/"
cit.path.SCOPUS.NPS <- "Scopus/NPS-LegalHighs/"

# where the generated figures are saved, create folder if not existing
Results.dir <- "Results/"
dir.create(file.path(Results.dir),recursive = TRUE)
Figure.dir <- "Figures/"
dir.create(file.path(Figure.dir),recursive = TRUE)

#############################################################
##### Country and Institution figures
# Number of individual country appearing on the figures
NumberCountry <- 20

#############################################################
##### Figure colours
# assign text colour
textcol <- "black"

#############################################################
##### Keyword Figures
# the number of keywords (top most frequent) appearing in the figure
number <- 100   # target number of keywords apearing in the keyword figure
maximum <- 105  # maximum number of keywords appearing in the keyword figure

# colour palette for the Keyword figure
pal <- c("#990000","#FF5D00","#FFB900","#FFFF00","#ACFF00","#00CC00","#33FFFF","#008BFF","#0000FF","#8968CD","#551A8B")

#     Select one of the following three options
KeywordEntries <- "Author_Keywords"
# KeywordEntries <- "Database_Keywords"
# KeywordEntries <- "All_Keywords"

# # # filename for figure export
# FigureName <- "Fig_INTERPOL_Keyword_"
# TableName <- "Table1_INTERPOL_Keyword_"


#############################################################
#####                    Data loading                   #####
#############################################################

# filename for figure export
FigureName <- "Fig1_Scopus_NPS_Keyword_"
TableName <- "Table1_Scopus_NPS_Keyword_"

#############################################################
#####  This is the code for generating Keyword figures  #####
#############################################################

# for (file in filenames){
# # remove the extension and path of the file in column reference  
#   name <- gsub(extension, "", file)
name <- "_NPS_Scopus"
Count <- number

#############################################################
#####                       Codes                       #####
#############################################################

# These codes can be run subsequently or independently as each create necessary outputs for the next codes.

# # Figure 1, INTERPOL Keywords as a function of year
# source("Code/Figure1_INTERPOL_Keywords.R")
#
# # Figure 2, Scopus Keywords as a function of year
# source("Code/Figure2_Scopus_Keywords.R")
#
# # Figure 3, NPS on Scopus - Country publications
# source("Code/Figure3_NPS_Scopus_Country_Affiliation.R")
#
# # Figure 4, NPS on Scopus - Compounds per country and relationship
# source("Code/Figure4_NPS_Scopus_Country_Relationship.R")

# # Figure 5, NPS on Scopus - Country network map
# source("Code/Figure5_CountryNetwork.R")

# # Figure 6, NPS on Scopus - Country collaboration percentage
# source("Code/Figure6_InternationalCollaboration.R")
