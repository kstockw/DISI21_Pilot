getwd()
library(tidyverse)
library(psych)
library(rstatix)
library(ggpubr)
library(yarrr)
library(viridis)
#gets rid of scientific notation
options(scipen = 999)

##############July 22, 2021 Kayden Stockwell, DISI 2021 project on gestures as a social marker##############

#Read in the previously cleaned and analyzed American/French pilot data
FrenchGraphs <- read.csv("FrenchPilotAccSDT.csv")
#Convert to long format to make the graphs
#Select only needed columns for signal detection graphs (might want to include accuracy etc in future)
colnames(FrenchGraphs)
FrenchGraphs <- FrenchGraphs %>% dplyr::select(ID, Gender, dprime:c)
#Make list of the column names except for ID level variables like ID, Gender etc for use in reshaping
(FrenchGraphNames <- names(FrenchGraphs[, -c(1:2)]))
#Conver to long
FGraphsLong <- reshape(FrenchGraphs, direction = "long", timevar = "Video", times = FrenchGraphNames, 
        idvar = c("ID", "Gender"), 
        ids = FrenchGraphs$ID, 
        varying=list(video = c(3:ncol(FrenchGraphs))), 
        v.names = "Choice")
#Order by subject
FGraphsLong  <- FGraphsLong  %>% arrange(ID)
#Fix the weird rownames that come with converting to long data
row.names(FGraphsLong) <- NULL
#Prepare data to merge with Spanish data to make A' plots 
AprimeF <- FGraphsLong %>% filter(Video == "aprime") 
AprimeF$Video <- gsub(x = AprimeF$Video, pattern = "aprime", replacement = "American/French") 

#Read in the previously cleaned and analyzed American/Spanish pilot data
SpanishGraphs <- read.csv("SpanishPilotAccSDT.csv")
#Convert to long format
#Select only needed columns for signal detection graphs (might want to include accuracy etc in future)
colnames(SpanishGraphs)
SpanishGraphs <- SpanishGraphs %>% dplyr::select(ID, Gender, dprime:c)
#Make list of the column names except for ID level variables like ID, Gender etc for use in reshaping
(SpanishGraphNames <- names(SpanishGraphs[, -c(1:2)]))
#Convert to long format in order to make graphs
SGraphsLong <- reshape(SpanishGraphs, direction = "long", timevar = "Video", times = SpanishGraphNames, 
                       idvar = c("ID", "Gender"), 
                       ids = SpanishGraphs$ID, 
                       varying=list(video = c(3:ncol(SpanishGraphs))), 
                       v.names = "Choice")
#Order by subject
SGraphsLong  <- SGraphsLong  %>% arrange(ID)
#Fix the weird rownames that come with converting to long data
row.names(SGraphsLong) <- NULL
#Prepare data to merge with French data to make A' plots 
AprimeS <- SGraphsLong %>% filter(Video == "aprime") 
AprimeS$Video <- gsub(x = AprimeS$Video, pattern = "aprime", replacement = "American/Spanish") 

#Create dataframe that hold both American/French and American/Spanish A' data (note the issue with repeat ID number, doesn't
#impact current graphs but should fix)
ComboAprime <- bind_rows(AprimeF, AprimeS)

#Density plot comparing A' for American/French and American/Spanish samples (view in zoom preview, not RStudio preview)
#Must run/save this main plot first, then run the lines below!!
AprimePirate <- pirateplot(Choice ~ Video, data = ComboAprime,
                      theme = 0,
                      pal = viridis(3), #makes colorblind friendly
                      inf.f.o = 0, # Turn off inf fill
                      inf.b.o = 0.8, # Turn on inf border
                      inf.b.col = "black",
                      inf.method = "ci", #confidence interval
                      point.o = 0.2,   # Turn up points
                      bar.f.o = 0.5, # Turn up bars
                      bean.f.o = 0, # no bean filling
                      bean.b.o = 1.0, # Light bean border
                      jitter.val = 0.09, # how much to jitter individual data points
                      avg.line.o = 1, # Turn on average line
                      avg.line.col = "black", # avg line col
                      point.col = "black",
                      ylim = c(0, 1),
                      #sortx = "mean"), # sorts bar order by mean
                      yaxt = "n", #removes automatic y axis 
                      ylab = "")
#                      xaxt = "n", #hide default x axis label
#                      xlab = "")
#Don't run these until you've run/saved the plot object above!!
#put y axis 1 - 10 on both sides
par(mar = c(4, 6, 1.2, 4.451)) #changes plot margins so doesn't run off screen
axis(2, at = seq(0, 1.1, by = 0.1), las = 1, font = 1, cex.axis = 1, line = 0) #left y axis
#axis(4, at = seq(0, 100, by = 10), las = 1, font = 1, cex.axis = 1)  #right y axis
mtext(side = 3, "Similar Sensativity (A') Across Two Samples", line = 0.1, font = 2, cex = 1) #title
mtext(side = 2, "A'", line = 3.5, font = 2, cex = 1)
#mtext(side = 1, "     American/Spanish", adj = 0, line = 1, font = 2, cex = 1) 
#mtext(side = 1, "Spanish                       ", adj = 1, line = 1, font = 2, cex = 1) #x axis 23 spaces 
#text(x = 10, y = 10, "TEST", font = 2, cex = 1) #sig marker NOT WORKING
#saves plot as object (doesn't seem to work but needed for zoom preview to update)
AprimePirate <- recordPlot()



####Not used in presentation, be careful of name duplications from the above graphs####
##Overall accuracy for American/Spanish sample
##Select only columns needed for accuracy
##Read in the previously cleaned and analyzed American/Spanish pilot data
#SpanishGraphs <- read.csv("SpanishPilotAccSDT.csv")
#colnames(SpanishGraphs)
#SpanishGraphs <- SpanishGraphs %>% dplyr::select(ID, Gender, American.Acc:S2_DummyCat)
##Make list of the column names except for ID level variables like ID, Gender etc for use in reshaping
#(SpanishGraphNames <- names(SpanishGraphs[, -c(1:2)]))
#
#SGraphsLong <- reshape(SpanishGraphs, direction = "long", timevar = "Video", times = SpanishGraphNames, 
#                       idvar = c("ID", "Gender"), 
#                       ids = SpanishGraphs$ID, 
#                       varying=list(video = c(3:ncol(SpanishGraphs))), 
#                       v.names = "Choice")
##Order by subject
#SGraphsLong  <- SGraphsLong  %>% arrange(ID)
##Fix the weird rownames that come with converting to long data
#row.names(SGraphsLong) <- NULL
##Mean accuracy plots for Spanish/American data
#OverallAccS <- SGraphsLong %>% filter(Video == "American.Acc"| Video == "Spanish.Acc") 
##density plot (look at in zoom preview, not just in the RStudio preview)
##Must run/save this main plot first, then run the lines below!!
#Spirate <- pirateplot(Choice*10 ~ Video, data = OverallAccS,
#                      theme = 0,
#                      pal = viridis(3), #makes colorblind friendly
#                      inf.f.o = 0, # Turn off inf fill
#                      inf.b.o = 0.8, # Turn on inf border
#                      inf.b.col = "black",
#                      inf.method = "ci", #confidence interval
#                      point.o = 0.2,   # Turn up points
#                      bar.f.o = 0.5, # Turn up bars
#                      bean.f.o = 0, # no bean filling
#                      bean.b.o = 1.0, # Light bean border
#                      jitter.val = 0.09, # how much to jitter individual data points
#                      avg.line.o = 1, # Turn on average line
#                      avg.line.col = "black", # avg line col
#                      point.col = "black",
#                      #                   sortx = "mean", # sorts bar order by mean
#                      yaxt = "n", #removes automatic y axis 
#                      ylab = "",
#                      xaxt = "n", #hide default x axis label
#                      xlab = "")
##Don't run these until you've run/saved the plot object above!!
##put y axis 1 - 10 on both sides
#par(mar = c(4, 6, 1, 4.451)) #changes plot margins so doesn't run off screen
#axis(2, at = seq(0, 100, by = 10), las = 1, font = 1, cex.axis = 1, line = 0) #left y axis
#axis(4, at = seq(0, 100, by = 10), las = 1, font = 1, cex.axis = 1)  #right y axis
##mtext(side = 3, "Mean Percent Accuracy for Videos of Spanish and of American Speakers", line = 0.7, font = 2, cex = 1) #title
#mtext(side = 2, "Percent Accuracy", line = 3.5, font = 2, cex = 1)
#mtext(side = 1, "                    American", adj = 0, line = 1, font = 2, cex = 1) #x axis 20 spaces 
#mtext(side = 1, "Spanish                      ", adj = 1, line = 1, font = 2, cex = 1) #x axis 22 spaces 
#text(x = 10, y = 10, "TEST", font = 2, cex = 1) #sig marker NOT WORKING
##saves plot as object (doesn't seem to work but needed for zoom preview to update)
#Spirate <- recordPlot()
#






