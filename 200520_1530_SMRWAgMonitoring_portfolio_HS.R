#R-Script by Hiram Sarabia-Ramirez, M.S.
#Environmental Scientist
#San Diego Water Board
#May 2020 - Work in progress
#Goal: Visualize, analyze and map data for SMRW Ag Monitoring Project 
#Required libraries:
#Required Files:

#Load required packages if not installed
#if(!require(dplyr)){install.packages("dplyr")}
if(!require(data.table)){install.packages('data.table')}
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(lubridate)){install.packages('lubridate')}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(scales)){install.packages("scales")}
if(!require(gridExtra)){install.packages("ggpubr")}
if(!require(shiny)){install.packages("shiny")}
if(!require(Sleuth2)){install.packages("Sleuth2")}
if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

############################
###  Indicator Bacteria  ###
############################

#Set working directory
#change working directtory as needed
dir <- setwd('C:/R/Santa Margarita/Lab Data/Bacteria/Enviromatrix')

#Load all csv files in directory with bacteria data and stack onto a single data frame
library(data.table)  
files <- list.files(path = (dir),pattern = ".csv")
temp <- lapply(files, fread, sep=",")
bact.data <- rbindlist( temp )

#Save data frame as csv file with sys date and time
#REF: https://stackoverflow.com/questions/39134991/write-csv-with-with-sys-time-in-file-name
write.csv(bact.data, paste0((dir), format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))

#CLEANDATA - Use E.coli data only, rename site column, remove spaces
#Add Date column
#https://stackoverflow.com/questions/8214303/conditional-replacement-of-values-in-a-data-frame
bact.data$SAMPLENAME[bact.data$SAMPLENAME == "902DELV30"] <- "902DELU30"
bact.data.c <- subset(bact.data, ANALYTE == "E. Coli")
names(bact.data.c)[5] <- "SITE"  
bact.data.c$SITE <- gsub('\\s+', '', bact.data.c$SITE)
#https://lubridate.tidyverse.org/
library(lubridate)
bact.data.c$DATE <- mdy_hms(bact.data.c$SAMPDATE)

#LAT/LONG
#ASSIGN LAT AND LONG BASED ON LOOK UP TABLE
coordinates <- read.csv('C:/R/Santa Margarita/Lab Data/Bacteria/191107_Site_Matrix.csv', stringsAsFactors = FALSE)
names(coordinates)[1] <- "SITE" 
bact.data.c$SITE[bact.data.c$SITE == '902DEV20'] <- '902DEVI20'
#Add LAT/LONG based on site name in coordinates df
#https://stackoverflow.com/questions/21712384/updating-column-in-one-dataframe-with-value-from-another-dataframe-based-on-matc/40364973
bact.data.c$LATITUDE <- coordinates[match(bact.data.c$SITE, coordinates$SITE),3]
bact.data.c$LONGITUDE <- coordinates[match(bact.data.c$SITE, coordinates$SITE),4]
bact.data.wdw <- bact.data.c[-c(1:4), ]

#Plot histogram 
#http://www.sthda.com/english/wiki/ggplot2-add-straight-lines-to-a-plot-horizontal-vertical-and-regression-lines
library(Sleuth2)
library(ggplot2)
ggplot(bact.data.wdw, aes(x=Result)) + 
  geom_histogram(color="black", fill="purple") +
  geom_vline(xintercept = 126, linetype="dotted", 
                color = "red", size=1.5)+
  xlab("E. coli (MPN)") +
  ylab("Count")+
  #ggtitle("Dry weather monitoring E. coli histogram (n=30)")+
  theme(plot.title = element_text(hjust = 0.5))

#Scatter Plot
#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
dew.new()
ggplot(bact.data.wdw, aes(x=bact.data.wdw$DATE, y=bact.data.wdw$Result, color=SITE, shape=SITE, size=)) +
  geom_point(shape=19, aes(size = Result))+ 
  #scale_x_log10()+
  #geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17, 1, 14, 7))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9', '#1BD46C', '#B51BD4','#FF0000'))+
  theme(legend.position="right")+
  #geom_rug()+
  geom_hline(yintercept=126, linetype="dashed", color = "red")+
  xlab("Sampling Date")+
  ylab("E. coli (MPN)") 
  #ggtitle("Dry weather monitoring total phosphorus histogram (n=32)")+
  #theme(plot.title = element_text(hjust = 0.5))

#Preeceding Rain in last 7 days
#Scatter Plot
#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
library(dplyr)
bact.data.rain <- filter(bact.data.wdw, PROJECTNUM %in% c("December 3, 2019", "February 26, 2020"))
bact.data.rain <- bact.data.rain[-c(9),] 
dew.new()
ggplot(bact.data.rain, aes(x=bact.data.rain$DATE, y=bact.data.rain$Result, color=SITE, shape=SITE)) +
  geom_point(shape=20, aes(size = Result)) + 
  # geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 12, 11))+ 
  scale_color_manual(values=c('#999999','#E69F00','#56B4E9', '#1BD46C'))+
  theme(legend.position="right")+
  geom_rug()+
  geom_hline(yintercept=126, linetype="dashed", color = "red")+
  xlab("Sampling Date")+
  ylab("E. coli (MPN)")

#Statistics
#Print statistics to to Txt file
sink('Stats_Bact.txt')
#Summary statistics
summary(bact.data.wdw$Result)
#Shapiro-Wilk Normality test
#The null-hypothesis of this test is that the population is normally distributed. 
#Thus, if the p value is less than the chosen alpha level, 
#then the null hypothesis is rejected and there is evidence that the data tested are not normally distributed.
normality <- shapiro.test(bact.data.c$Result)
normality
sink()

#Boxplot
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
dev.new()
ggplot(bact.data.c, aes(x=SITE, y=Result, color=SITE)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=42,
               outlier.size=4)+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  theme(legend.position="right")+
  xlab("Sampling Date")+
  ylab("E. coli (MPN)")


#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
bact.data.bp <- bact.data.c[-c(25,34), ] 
library(ggpubr)
dev.new()
#my_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(bact.data.bp, x = "SITE", y = "Result",
          color = "SITE", palette = "jco")+ 
  #stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1000)     # Add global p-value


###########
#NUTRIENTS#
###########

#Set wd for nutrients
dir2 <- setwd('C:/R/Santa Margarita/Lab Data/Nutrients')

#Load all csv files in directory with bacteria data and stack onto a single data frame
library(data.table)  
files2 <- list.files(path = (dir2),pattern = ".csv")
temp2 <- lapply(files2, fread, sep=",")
nutrient.data <- rbindlist( temp2 )

#Save data frame as csv file with sys date and time
#REF: https://stackoverflow.com/questions/39134991/write-csv-with-with-sys-time-in-file-name
write.csv(nutrient.data, paste0((dir2), format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))

#CLEANDATA - Use TN and TP data only, rename site column, remove spaces
#Add Date column
#https://stackoverflow.com/questions/8214303/conditional-replacement-of-values-in-a-data-frame
nutrient.data$StationCode[nutrient.data$StationCode == "902DELV30"] <- "902DELU30"
nutrient.data$StationCode[nutrient.data$StationCode == "Station B"] <- "902SAND10"
nutrient.data$StationCode[nutrient.data$StationCode == "Station C"] <- "902DEVI10"
nutrient.data$StationCode[nutrient.data$StationCode == "Station A"] <- "FIELDBLANK"
nutrient.data.aa1 <- subset(nutrient.data, LocationCode == "AA1")
nutrient.data.grab <- subset(nutrient.data.aa1, SampleTypeCode == "Grab")
# subset the rows of dataframe with multiple conditions

library(dplyr)
nutrient.data.tntp <- filter(nutrient.data.grab, AnalyteName %in% c("Nitrogen, Total", "Phosphorus as P"))

#Remove Lab Duplicates
nutrient.data.tntp.nodups <- nutrient.data.tntp[!(nutrient.data.tntp$LabSampleID %like% "DUP"), ]

#RENAME INCORRECT STATION IDs
nutrient.data.tntp.nodups$StationCode[nutrient.data.tntp.nodups$StationCode == "902DEVIL1"] <- "902DEVI10"
nutrient.data.tntp.nodups$StationCode[nutrient.data.tntp.nodups$StationCode == "902DEVIL2"] <- "902DEVI20"
nutrient.data.tntp.nodups$StationCode[nutrient.data.tntp.nodups$StationCode == "902SNDIA1"] <- "902SAND10"
nutrient.data.tntp.nodups$StationCode[nutrient.data.tntp.nodups$StationCode == "902SNDIA2"] <- "902SAND20"
nutrient.data.tntp.nodups$StationCode[nutrient.data.tntp.nodups$StationCode == "902DELV30"] <- "902DELU30"

#Rename Column
names(nutrient.data.tntp.nodups)[2] <- "SITE"
names(nutrient.data.tntp.nodups)[6] <- "SampleDate"

#https://lubridate.tidyverse.org/
library(lubridate)
nutrient.data.tntp.nodups$DATE <- dmy(nutrient.data.tntp.nodups$SampleDate)

#LAT/LONG
#ASSIGN LAT AND LONG BASED ON LOOK UP TABLE
coordinates <- read.csv('C:/R/Santa Margarita/Lab Data/Bacteria/191107_Site_Matrix.csv', stringsAsFactors = FALSE)
names(coordinates)[1] <- "SITE" 
#bact.data.c$SITE[bact.data.c$SITE == '902DEV20'] <- '902DEVI20'
#Add LAT/LONG based on site name in coordinates df
#https://stackoverflow.com/questions/21712384/updating-column-in-one-dataframe-with-value-from-another-dataframe-based-on-matc/40364973
nutrient.data.tntp.nodups$LATITUDE <- coordinates[match(nutrient.data.tntp.nodups$SITE, coordinates$SITE),3]
nutrient.data.tntp.nodups$LONGITUDE <- coordinates[match(nutrient.data.tntp.nodups$SITE, coordinates$SITE),4]
nutrient.data.tntp.nodups <- filter(nutrient.data.tntp.nodups, SITE %in% c("902DEVI10", "902DEVI20", "902SAND10", "902SAND20","902DELU20","902DELU30"))

#Subset TN and TP
nutrient.data.tn.6sites = filter(nutrient.data.tntp.nodups, AnalyteName %in% "Nitrogen, Total")
nutrient.data.tp.6sites = filter(nutrient.data.tntp.nodups, AnalyteName %in% "Phosphorus as P")
nutrient.data.tn.6sites[12,2] <- "902SAND20"
nutrient.data.tp.6sites[12,2] <- "902SAND20"

dev.new()
ggplot(nutrient.data.tn.6sites, aes(x=Result)) + 
  geom_histogram(color="black", fill="purple") +
  geom_vline(xintercept = 1.0, linetype="dotted", 
             color = "red", size=1.5)+
  xlab("TN (mg/L)")+
  ylab("Count")+
  #ggtitle("Dry weather monitoring total nitrogen histogram (n=32)")+
  theme(plot.title = element_text(hjust = 0.5))


#Scatter Plot TN
#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
dew.new()
ggplot(nutrient.data.tn.6sites, aes(x=nutrient.data.tn.6sites$DATE, y=nutrient.data.tn.6sites$Result, color=SITE, shape=SITE)) +
  geom_point(shape=19, aes(size = Result))+
  
  #geom_point(shape=19) + 
  # geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17, 1, 14, 7, 20))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9', '#1BD46C', '#B51BD4','#FF0000', '#888888'))+
  theme(legend.position="top")+
  #geom_rug()+
  geom_hline(yintercept=1.0, linetype="dashed", color = "red")+
  xlab("Sampling Date")+
  ylab("TN (mg/L)")

#Scatter Plot TP
#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
dew.new()
ggplot(nutrient.data.tp.6sites, aes(x=nutrient.data.tp.6sites$DATE, y=nutrient.data.tp.6sites$Result, color=SITE, shape=SITE)) +
  geom_point(shape=19, aes(size = Result))+
  # geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17, 1, 14, 7, 20))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9', '#1BD46C', '#B51BD4','#FF0000', '#888888'))+
  theme(legend.position="top")+
  geom_rug()+
  geom_hline(yintercept=0.1, linetype="dashed", color = "red")+
  xlab("Sampling Date")+
  ylab("TP (mg/L)")

#Preeceding Rain in last 7 days
library(dplyr)
nutrient.data.rain.tn <- filter(nutrient.data.tn, SampleDate %in% c("03/Dec/2019", "26/Feb/2020"))
dew.new()
ggplot(nutrient.data.rain.tn, aes(x=nutrient.data.rain.tn$DATE, y=nutrient.data.rain.tn$Result, color=SITE, shape=SITE)) +
  geom_point(shape=15, size=5) + 
  # geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 12, 11))+ 
  scale_color_manual(values=c('#999999','#E69F00','#56B4E9', '#1BD46C'))+
  theme(legend.position="top")+
  geom_rug()+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  xlab("Sampling Date")+
  ylab("TN (mg/L)")

nutrient.data.rain.tp <- filter(nutrient.data.tp, SampleDate %in% c("03/Dec/2019", "26/Feb/2020"))
dew.new()
ggplot(nutrient.data.rain.tp, aes(x=nutrient.data.rain.tp$DATE, y=nutrient.data.rain.tp$Result, color=SITE, shape=SITE)) +
  geom_point(shape=15, size=5) + 
  # geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 12, 11))+ 
  scale_color_manual(values=c('#999999','#E69F00','#56B4E9', '#1BD46C'))+
  theme(legend.position="top")+
  geom_rug()+
  geom_hline(yintercept=0.1, linetype="dashed", color = "red")+
  xlab("Sampling Date")+
  ylab("TP (mg/L)")


#Statistics TN
#Print statistics to to Txt file
sink('Stats_TN.txt')
#Summary statistics
summary(nutrient.data.tn.6sites$Result)
#Shapiro-Wilk Normality test
#The null-hypothesis of this test is that the population is normally distributed. 
#Thus, if the p value is less than the chosen alpha level, 
#then the null hypothesis is rejected and there is evidence that the data tested are not normally distributed.
normality <- shapiro.test(nutrient.data.tn.6sites$Result)
normality
sink()

#Statistics TP
#Print statistics to to Txt file
sink('Stats_TP.txt')
#Summary statistics
summary(nutrient.data.tp.6sites$Result)
#Shapiro-Wilk Normality test
#The null-hypothesis of this test is that the population is normally distributed. 
#Thus, if the p value is less than the chosen alpha level, 
#then the null hypothesis is rejected and there is evidence that the data tested are not normally distributed.
normality <- shapiro.test(nutrient.data.tp.6sites$Result)
normality
sink()

#Subset 4 sites
nutrient.data.tntp.4sites <- filter(nutrient.data.tntp.nodups, SITE %in% c("902DEVI10", "902DEVI20", "902SAND10", "902SAND20"))
#Alternate Solution
#https://stackoverflow.com/questions/34444295/how-to-specify-does-not-contain-in-dplyr-filter
#SE_CSVLinelist_filtered <- filter(SE_CSVLinelist_clean, 
#!where_case_travelled_1 %in% 
#  c('Outside Canada','Outside province/territory of residence but within Canada'))

#Subset TN and TP
nutrient.data.tn.4sites = filter(nutrient.data.tntp.4sites, AnalyteName %in% "Nitrogen, Total")
nutrient.data.tp.4sites = filter(nutrient.data.tntp.4sites, AnalyteName %in% "Phosphorus as P")
nutrient.data.tn.4sites[12,2] <- "902SAND20"
nutrient.data.tp.4sites[12,2] <- "902SAND20"

#nutrient.data.tn.date = filter(nutrient.data.tn, SampleDate %in% "26/Feb/2020")
#nutrient.data.tp.date = filter(nutrient.data.tp, SampleDate %in% "26/Feb/2020")


#Boxplot TN
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
dev.new()
ggplot(nutrient.data.tn.4sites, aes(x=SITE, y=Result, color=SITE)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=42,
               outlier.size=4)+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  #pl + geom_jitter(shape=16, position=position_jitter(0.1))
  theme(legend.position="top")+
  xlab("Sampling Date")+
  ylab("TN (mg/L)")


#Boxplot TP
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
dev.new()
ggplot(nutrient.data.tp.4sites, aes(x=SITE, y=Result, color=SITE))+ 
  geom_boxplot(outlier.colour="black", outlier.shape=42,
               outlier.size=4)+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  #pl + geom_jitter(shape=16, position=position_jitter(0.1))
  theme(legend.position="top")+
  xlab("Sampling Date")+
  ylab("TP (mg/L)")


#Comparison TN
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
library(ggpubr)
dev.new()
#my_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(nutrient.data.tn.4sites, x = "SITE", y = "Result",
          color = "SITE", palette = "jco")+ 
  #stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 30)+     # Add global p-value
  theme(legend.position="top")+
  xlab("Sampling Date")+
  ylab("TN (mg/L)")


#Comparison TP
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
library(ggpubr)
dev.new()
my_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(nutrient.data.tp, x = "SITE", y = "Result",
          color = "SITE", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.5)+     # Add global p-value
  xlab("Sampling Date")+
  ylab("TP (mg/L)")

###################
#### LOADING ######
###################

setwd('C:/R/Santa Margarita/Field Data/')
physchem <- read.csv('200515_SMRW_Ag_PhysChem.csv', stringsAsFactors = FALSE)
names(physchem)[1] <- "SITE"
names(physchem)[9] <- "Flow_CFS"
library(lubridate)
physchem$DATE <- mdy(physchem$Date)
physchem2 <- physchem[-(29), ] 

########################################################ERROR###########
#Boxplot Estimated Monthly TN Mass Loading per Acre
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
library(ggplot2)
dev.new()
ggplot(physchem2, aes(x=SITE, y=Flow_CFS, color=SITE)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=42,
               outlier.size=4)
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  #geom_jitter(shape=16, position=position_jitter(0.1))

###########################################################################

#Comparison Flow (CFS)
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
library(ggpubr)
dev.new()
site_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(physchem2, x = "SITE", y = "Flow_CFS",
          color = "SITE", palette = "jco")+ 
  stat_compare_means(comparisons = site_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 20)+     # Add global p-value
  xlab("Site")+
  ylab("Flow (CFS)")


#Comparison Monthly Flow per Acre
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
library(ggpubr)
dev.new()
#site_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(physchem2, x = "SITE", y = "Mo_Flow_Acre_CF",
          color = "SITE", palette = "jco")+ 
#  stat_compare_means(comparisons = site_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 30)+     # Add global p-value
  xlab("Site")+
  ylab("Monthly Flow per Acre (CFS)")


#Comparison Mo TN Acre
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
dev.new()
#site_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(physchem2, x = "SITE", y = "TN_Mo_Acre",
          color = "SITE", palette = "jco")+ 
  #stat_compare_means(comparisons = site_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.6)+     # Add global p-value
  xlab("Site")+
  ylab("Monthly TN Load per Acre (Kg)")

#Comparison Mo TP Acre
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
dev.new()
site_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(physchem2, x = "SITE", y = "TP_Mo_Acre",
          color = "SITE", palette = "jco")+ 
#  stat_compare_means(comparisons = site_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.010)+     # Add global p-value
  xlab("Site")+
  ylab("Monthly TP Load per Acre (Kg)")

#######################
## Dissolved Oxygen ###
#######################

#Scatter Plot DO
#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
dew.new()
ggplot(physchem2, aes(x=DATE, y=DO, color=SITE, shape=SITE)) +
  geom_point(shape=19, aes(size = DO))+
  # geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17, 1, 14, 7, 20))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9', '#1BD46C', '#B51BD4','#FF0000', '#888888'))+
  theme(legend.position="top")+
  #geom_rug()+
  geom_hline(yintercept=6.0, linetype="dashed", color = "red")+
  xlab("Sampling Date")+
  ylab("DO (mg/L)")


dev.new()
ggplot(physchem2, aes(x=DO)) + 
  geom_histogram(color="black", fill="purple") +
  geom_vline(xintercept = 6.0, linetype="dotted", 
             color = "red", size=1.0)+
  xlab("DO (mg/L)")+
  ylab("Count")
  #ggtitle("Dry weather monitoring total nitrogen histogram (n=32)")+
  #theme(plot.title = element_text(hjust = 0.5))

#Boxplot DO
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
dev.new()
ggplot(physchem2, aes(x=SITE, y=DO, color=SITE))+ 
  geom_boxplot(outlier.colour="black", outlier.shape=42,
               outlier.size=4)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  geom_jitter(shape=16, position=position_jitter(0.1))

#Comparison DO
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
library(ggpubr)
dev.new()
DO_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(physchem2, x = "SITE", y = "DO",
          color = "SITE", palette = "jco")+ 
  stat_compare_means(method = "anova", label.y = 20)+ 
  stat_compare_means(comparisons = DO_comparisons, method = "t.test")+ # Add pairwise comparisons p-value
  #stat_compare_means(method = "t.test", label.y = 15)+     # Add global p-value
  xlab("Site")+
  ylab("Dissolved Oxygen (mg/L)")

#Statistics
#Print statistics to to Txt file
sink('Stats_DO.txt')
#Summary statistics
summary(physchem2$DO)
#Shapiro-Wilk Normality test
#The null-hypothesis of this test is that the population is normally distributed. 
#Thus, if the p value is less than the chosen alpha level, 
#then the null hypothesis is rejected and there is evidence that the data tested are not normally distributed.
normality <- shapiro.test(physchem2$DO)
normality
sink()

#Boxplot pH
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
dev.new()
box.pH <- ggplot(physchem2, aes(x=SITE, y=pH, color=SITE)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=42,
               outlier.size=4)
box.pH + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
#pl + geom_jitter(shape=16, position=position_jitter(0.1))
box.pH

#Comparison pH
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
library(ggpubr)
dev.new()
pH_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(physchem2, x = "SITE", y = "pH",
          color = "SITE", palette = "jco")+ 
  stat_compare_means(comparisons = pH_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)     # Add global p-value

#Boxplot Cond
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
dev.new()
box.cond <- ggplot(physchem2, aes(x=SITE, y=Cond, color=SITE)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=42,
               outlier.size=4)
box.cond + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
#pl + geom_jitter(shape=16, position=position_jitter(0.1))
box.cond

#Comparison Cond
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
library(ggpubr)
dev.new()
site_comparisons <- list( c("902DEVI10", "902DEVI20"), c("902SAND10", "902SAND20"), c("902DEVI20", "902SAND10"), c("902SAND20", "902DEVI20"), c("902DEVI10", "902SAND10"), c("902DEVI10", "902SAND20"))
ggboxplot(physchem2, x = "SITE", y = "Cond",
          color = "SITE", palette = "jco")+ 
  stat_compare_means(comparisons = site_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 2250)     # Add global p-value

#######################################
### SPATIAL CORRELATION PLACEHOLDER ###
#######################################

#Correlation Analysis
#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#https://rpubs.com/skyrosepark/383870
#DEV10vsDEV20
library(dplyr)
dev.cor <- bind_rows(DEV10.bact.data.c, DEV20.bact.data.c)
dev.new()
ggplot(dev.cor, aes(x=Result, y=Result)) + geom_point() + ggtitle("DEV10 vs DEV20") + geom_smooth(method=lm, se=FALSE) + scale_x_continuous(name = "DEV10", limits = c(5, 15), breaks = seq(5, 15, 2)) + scale_y_continuous(name = "DEV20", limits = c(5,15), breaks = seq(5, 15, 2)) + theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black")) + theme_bw()

library("ggpubr")
dev.new()
par(mfrow=c(2,2))
par(mar=c(5,6,4,1)+.1)
#cor.data <- merge.data.frame(Tn.data, fallbrook.weather.data)
cor(DEV10.bact.data.c$Result, DEV20.bact.data.c$Result,  method = "pearson")
#fill in missing with NA to solve incompatible dimensions issue...

#########################
### SHINY PLACEHOLDER ###
#########################

#SHINY
#https://stackoverflow.com/questions/37575452/how-to-subset-a-dataframe-and-plot-with-leaflet-depending-on-inputselect-in-shin

##########################
##Site Map PLACEHOLDER ###
##########################

#Sites from Table 5-5 Rainbow Creek Nutrient TMDL Monitoring Station Locations
library(leaflet)

#https://gis.stackexchange.com/questions/283658/add-layers-with-leaflet-from-different-spatial-data-frames-in-r
#Main Stem Sites
#https://stackoverflow.com/questions/40295363/plotting-lat-long-info-in-dataframe-or-vector-using-leaflet
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  setView(lng=-117.13571, lat=33.42042, zoom =10) %>%
  addCircleMarkers(lng=DEV10.bact.data.c$LONGITUDE, lat=DEV10.bact.data.c$LATITUDE, popup="DEV10 - Devils Creek 10", col='red') %>%
  addCircleMarkers(lng=-117.15199, lat=33.41544, popup="RBC02 - Rainbow Creek @ Huffstatler Road",col='red') %>%
  addCircleMarkers(lng=-117.15853, lat=33.41272, popup="RBC04 - Rainbow Creek @ Old Highway 395", col='red') %>%
  addCircleMarkers(lng=-117.20539, lat=33.40881, popup="RBC06 - Rainbow Creek @ 2219 Willow Glen Road", col='red') %>%
  #addCircleMarkers(lng=-117.18344, lat=33.40696, popup="RBC10 - Rainbow Creek @ MWD Crossing", col='grey') %>%
  #addCircleMarkers(lng=-117.20104, lat=33.40788, popup="SMG05 - Rainbow Creek @ Willow Glen Road",col='grey') %>%
  addCircleMarkers(lng=-117.21477, lat=33.41056, popup="SMG06 - Rainbow Creek @ Stage Coach Lane", col='red') %>%
  addCircleMarkers(lng=-117.20000, lat=33.40750, popup="USGS GAGE 11044250", col='blue') %>%
  addCircleMarkers(lng=-117.1904, lat=33.4392, popup="NOAA Weather Station - Fallbrook 5 NE", col='purple')
m  # Print the map

### HOME DIR ###
### USERNAME ###
#Path commands
#path.expand("~")
#paste("/home/",system ("whoami",intern=TRUE),sep="")
#Sys.info()