getwd()
library(tidyverse)
library(psych)
#gets rid of scientific notation
options(scipen = 999)

##############July 22, 2021 Kayden Stockwell, DISI 2021 project on gestures as a social marker##############

####Data Cleaning####
#Load in the American/Spanish Pilot data for the DISI 2021 social gategorization from gestures task 
#Remove the row containing the complete questions text (e.g. How many langueges other than English do you speak?)
SpanishPilot <- read.csv("Gesture_SpanishPilot.csv", stringsAsFactors = FALSE)[-1,]
#Remove fake data from prior to opening the survey (those without a real ProlificID)
SpanishPilot <- subset(SpanishPilot, ProlificID != "KAYDEN")
#Remove unwanted/empty columns
colnames(SpanishPilot)
SpanishPilot <- SpanishPilot %>% select(-c(Status, IPAddress, Finished, RecordedDate:DistributionChannel))
#Rename _Time_Click.Count columns to just _Click.Count
names(SpanishPilot) <- gsub(x = names(SpanishPilot), pattern = "\\Time_Click.Count", replacement = "Click.Count") 
#Change some columns to correct data type
SpanishPilot <- SpanishPilot %>% mutate(across(c(Progress, Duration..in.seconds., Age, matches("Time")), as.numeric))
SpanishPilot <- SpanishPilot %>% mutate(across(c(UserLanguage, Gender, Languages, Countries, Sound_check, Recognition), 
                                             as.factor))
str(SpanishPilot)
table(SpanishPilot$Gender)
#Check if any page took longer than ~35 seconds to submit (shouldn't unless there was a technical error, look at max column)
SpanishPilot %>% select(ends_with("Page.Submit")) %>% describe() %>% arrange(desc(max))
#For right now, I don't care about the time it took for page to submit or the page click count, so remove those columns
SpanishPilot <- SpanishPilot %>% select(-(contains(c("Time", "Click"))))
#Give participants a simple ID number (if ever combine with the French Pilot file, will need to think about ID number and
#American column names)
SpanishPilot <- add_column(.data = SpanishPilot, .before = 1, "ID" = 1:nrow(SpanishPilot))


##Decide if want to exclude participants who said practice trial was Not American, those that didn't give expected answer
##for catch trials, those who lived in other countires, those who said tech difficulty for seeing video etc

#Check for who didn't see videos. 22 didn't see 3 videos, 30, 63, and 70 didn't see 1 video
DidntSeeS <- SpanishPilot %>% filter(if_any(ends_with("Count"), ~ . == "I did not see the video")) %>% select(ID, matches("Count"))
#Checking if any of above gave feecback on why they missed videos
SpanishPilot %>% filter(ID == 22 | ID == 30 | ID == 63 | ID == 70) %>% select(Categorization, Feedback)
#Removing anyone who missed a video (can just not run/edit this line if want to include them)
SpanishPilot <- SpanishPilot %>% filter(ID != 22 & ID != 30 & ID != 63 & ID!= 70)


####Analyses####
#Calculate the accuracy percentage for when speaker was American
#Creates columns with dummy coded (0 = American, 1 = Not American) Categorization Columns for American video and a sum column
#showing how many times (10 possible) participants said Not American for American video (Acc.American)
#Doesn't included catch trials as those end in "Categ"
SpanishPilot <- SpanishPilot %>%
  mutate(American = across(.cols = starts_with("A") & ends_with("Categorization"), 
                           .fns = str_count, "Not American")) %>% 
  rowwise() %>% 
  mutate(American.Acc = across(.cols = contains("American"), .fns = sum))
#Shows all new columns created by mutate, kind of weird it shows 1 column for 10 columns. Might be issue later...
SpanishPilot$American
#To prevent possible issue, unnest the 10 columns and store. 
DummyAmericanColsS <- unnest(SpanishPilot$American)
#Rename them to avoid confusion with original columns
names(DummyAmericanColsS) <- gsub(x = names(DummyAmericanColsS), pattern = "\\Categorization", replacement = "DummyCat")
#Remove the 10 columns that are somehow nested in the main dataframe (column number of American may change so double check)
colnames(SpanishPilot)
SpanishPilot <- cbind(SpanishPilot[,-(112)], DummyAmericanColsS)
#Make Acc.American and A_DummyCat columns clearer by having them show how many times American was selected (Acc shows sum 
#of how many times American was selected for American vidoes (10 possible) and DummyCats reflect 1 = American and 
#0 = Not American)
SpanishPilot$American.Acc <- abs(SpanishPilot$American.Acc - 10) %>% unlist() %>% as.numeric()
#Note that this line makes all these columns characters, don't think it matters as of now
SpanishPilot <- SpanishPilot %>% mutate(across(ends_with("DummyCat"), ~recode(., "0" = "1", "1" = "0")))
#What is the pilot sample mean accuracy for American videos?
mean(SpanishPilot$American.Acc) #6.848485 or ~69%
#Mean overall accuracy differences by gender (1 = female, 2 = male)? No
SpanishPilot %>% filter(as.numeric(Gender) == 1) %>% select(American.Acc) %>% describe() #6.84 or ~68%
SpanishPilot %>% filter(as.numeric(Gender) == 2) %>% select(American.Acc) %>% describe() #6.78 or ~68%

#Calculate the accuracy percentage for when speaker was Spanish
#Creates columns with dummy coded (0 = American, 1 = Not American) Categorization Columns for Spanish video and a sum column
#showing how many times (10 possible) participants said Not American for Spanish video (Acc.Spanish). Note here the accuracy
#column is fine as is
#Doesn't included catch trials as those end in "Categ"
SpanishPilot <- SpanishPilot %>%
  mutate(Spanish = across(.cols = starts_with("S") & ends_with("Categorization"), 
                         .fns = str_count, "Not American")) %>% 
  rowwise() %>% 
  mutate(Spanish.Acc = across(.cols = contains("Spanish"), .fns = sum))
#Shows all new columns created by mutate, kind of weird it shows 1 column for 10 columns. Might be issue later...
SpanishPilot$Spanish
#To prevent possible issue, unnest the 10 columns and store. 
DummySpanishCols <- unnest(SpanishPilot$Spanish)
#Rename them to avoid confusion with original columns
names(DummySpanishCols) <- gsub(x = names(DummySpanishCols), pattern = "\\Categorization", replacement = "DummyCat")
#Remove the 10 columns that are somehow nested in the main dataframe (column number of Spanish may change so double check)
colnames(SpanishPilot)
SpanishPilot <- cbind(SpanishPilot[,-(123)], DummySpanishCols)
SpanishPilot$Spanish.Acc <- SpanishPilot$Spanish.Acc %>% unlist() %>% as.numeric()
#What is the sample mean accuracy for Spanish videos?
mean(SpanishPilot$Spanish.Acc) #4.818182 or ~48%
#Mean overall accuracy differences by gender(1 = female, 2 = male)? No
SpanishPilot %>% filter(as.numeric(Gender) == 1) %>% select(Spanish.Acc) %>% describe() #4.77 or ~48%
SpanishPilot %>% filter(as.numeric(Gender) == 2) %>% select(Spanish.Acc) %>% describe() #4.84 or ~48%

#Do participants differ from chance level guessing (50%)?
t.test(SpanishPilot$Spanish.Acc, mu = 5) #p = 0.4694, no
t.test(SpanishPilot$American.Acc, mu = 5) #p = 0.0000000001991, yes
#Do accuracy scores differ for Spanish and for American videos?
t.test(SpanishPilot$Spanish.Acc, SpanishPilot$American.Acc) #p = 0.00000004859, yes

#Mean accuracy for each Spanish video
SpanishPilot %>% select(contains("S") & contains("Dummy")) %>% describe() %>% arrange(mean)
#Mean accuracy for each American video
SpanishPilot %>% select(matches("A", ignore.case = FALSE)  & contains("Dummy")) %>% describe() %>% arrange(mean)

#Plotting relationship between American and Spanish overall accuracy (check if should be Spearman or Pearson correlation)
scatter.hist(SpanishPilot$American.Acc/10, SpanishPilot$Spanish.Acc/10, xlim = c(0, 1), ylim = c(0, 1), ellipse = FALSE,
             method = "spearman", smooth = FALSE, ab = TRUE)
cor.test(SpanishPilot$American.Acc, SpanishPilot$Spanish.Acc, method = "spearman")


#Use psycho library to calculate signal detection theory metrics
library(psycho)
#Calculating for each participant
#Correct Rejection is saying a Spanish speaker is Not American
Correct_RejectS <- SpanishPilot$Spanish.Acc
#Miss is saying an American speaker is Not American
MissesS <- abs(SpanishPilot$American.Acc - 10)
#False Alarm is saying a Spanish speaker is American
False_AlarmS <- abs(SpanishPilot$Spanish.Acc - 10)
#Hit is saying an American speaker is American
HitS <- SpanishPilot$American.Acc
#Confirm all equal total stimulus number (20 as of July 22, 2021)
Correct_RejectS + MissesS + False_AlarmS + HitS
#Calculates d prime, beta, and c for each participant. a prime and bppd are non-parametric estimates (see help documentation)
#Looks like this comes up with one NA and one NaN in the non-parametric output? KAYDEN
dPrimeS <- dprime(n_hit = HitS, n_fa= False_AlarmS, n_miss = MissesS, n_cr = Correct_RejectS) %>% as.data.frame()
#Parametric measure of sensitivity, I think chance guessing is at at 0.5, but not 100% sure on that
mean(dPrimeS$dprime) #0.4432545
describe(dPrimeS$dprime)
#Think this suggests pretty unbiased or just the slightest liberal bias (parametric)
mean(dPrimeS$beta) #0.9113943
#Not really sure how to interpret c
mean(dPrimeS$c) #-0.2937359
#mean aPrime (non-parametric accuracy), 0.5 means chance performance
mean(dPrimeS$aprime, na.rm = TRUE) #0.6421259
#mean non-parametric bias estimate, 0 is no bias, negative number is slight liberal (answer yes/American) bias
mean(dPrimeS$bppd, na.rm = TRUE) #-0.1174111
#Add output to main dataframe
SpanishPilot <- cbind(SpanishPilot, dPrimeS)

#aprime does not differ by gender (1 = female, 2 = male)
SpanishPilot %>% filter(as.numeric(Gender) == 1) %>% select(aprime) %>% describe() #0.64
SpanishPilot %>% filter(as.numeric(Gender) == 2) %>% select(aprime) %>% describe() #0.63

#bppd does not differ by gender (numerically but not signiciantly different according to 2 sample t test)
SpanishPilot %>% filter(as.numeric(Gender) == 1) %>% select(bppd) %>% describe() #-0.14
SpanishPilot %>% filter(as.numeric(Gender) == 2) %>% select(bppd) %>% describe() #-0.09

#not sure if this is correct mu for dprime, other mus I am pretty sure are correct
t.test(SpanishPilot$dprime, mu = 0, alternative = "two.sided") #p = 0.00000003024
t.test(SpanishPilot$aprime, mu = 0.5, alternative = "two.sided") #p = 0.00000001308
t.test(SpanishPilot$beta, mu = 1, alternative = "two.sided") #p = 0.008894
t.test(SpanishPilot$bppd, mu = 0, alternative = "two.sided") #p = 0.002733

#Plot relationship between A' sensativity and B" bias measure (check if spearman is correct correlation)
scatter.hist(SpanishPilot$aprime, SpanishPilot$bppd, xlim = c(0, 1.0), ylim = c(-1, 1), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
#Not significant
cor.test(SpanishPilot$aprime, SpanishPilot$bppd, method = "spearman")

#When bias is more liberal, American accuracy goes up significantly, but not significant positive cor for Spanish
cor.test(SpanishPilot$Spanish.Acc, SpanishPilot$bppd, method = "spearman")
cor.test(SpanishPilot$American.Acc, SpanishPilot$bppd, method = "spearman")
plot(SpanishPilot$American.Acc, SpanishPilot$bppd, ylim = c(-1, 1))
plot(SpanishPilot$Spanish.Acc, SpanishPilot$bppd, ylim = c(-1, 1))
scatter.hist(SpanishPilot$American.Acc*10, SpanishPilot$bppd, xlim = c(0, 100), ylim = c(-1, 1), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman",  xlab = "Percent Accuracy: American Videos", ylab = "Response Bias (B'')",
             title = "Relation of Percenty Accuracy (American Videos)\nand Response Bias")
scatter.hist(SpanishPilot$Spanish.Acc*10, SpanishPilot$bppd, xlim = c(0, 100), ylim = c(-1, 1), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman",  xlab = "Percent Accuracy: Spanish Videos", ylab = "Response Bias (B'')",
             title = "Relation of Percenty Accuracy (Spanish Videos)\nand Response Bias")

#save SpanishhPilot csv so you can use it in graphs
#write.csv(SpanishPilot, file = "SpanishPilotAccSDT.csv", row.names = FALSE)



##Confidence of Rating, and Use/Exposure to Gestures

#Turn Confidence data into numeric to look at means, median etc
SpanishPilotConf <- SpanishPilot 
SpanishPilotConf <- SpanishPilotConf %>% mutate(across(contains("Confi"), ~recode(., "Not confident at all" = "1", "Slightly confident" = "2",
                                                                                "Somewhat confident" = "3", "Fairly confident" = "4",
                                                                                "Very confident" = "5")))
#Confirm
SpanishPilotConf %>% select(contains("Confi")) %>% str()
#Select only relevant columns and exclude Catch trials
SpanishPilotConf <- SpanishPilotConf %>% select(ID, contains("Confi"), 
                                              -c("Practice_Confidence", "A_Catch_Confidence", "S_Catch_Confidence"))
#Transfrom the character columns to numeric
SpanishPilotConf <- SpanishPilotConf %>% mutate(across(A9_Confidence:A2_Confidende, ~as.numeric(.)))
str(SpanishPilotConf)
#Gets overall confidence mean for each participant, excluding the Catch trial
SpanishPilotConf$A_MeanConfidence <- SpanishPilotConf %>% select(matches("A", ignore.case = FALSE), -c(ID)) %>% rowMeans()
SpanishPilotConf$S_MeanConfidence <- SpanishPilotConf %>% select(matches("S", ignore.case = FALSE), -c(ID)) %>% rowMeans()
describe(SpanishPilotConf$A_MeanConfidence) #mean = 3.04
describe(SpanishPilotConf$S_MeanConfidence) #mean = 3.03

#Not a significant correlation for American accuracy and American confidence
scatter.hist(SpanishPilot$American.Acc*10, SpanishPilotConf$A_MeanConfidence, xlim = c(0, 100), ylim = c(1, 5), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
cor.test(SpanishPilot$American.Acc, SpanishPilotConf$A_MeanConfidence, method = "spearman")

#Not a significant correlation for Spanish accuracy and Spanish confidence
scatter.hist(SpanishPilot$Spanish.Acc*10, SpanishPilotConf$S_MeanConfidence, xlim = c(0, 100), ylim = c(1, 5), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
cor.test(SpanishPilot$Spanish.Acc, SpanishPilotConf$S_MeanConfidence, method = "spearman")


#Turn gesture use data into numeric to look at means, median etc
SpanishPilotUse <- SpanishPilot 
SpanishPilotUse <- SpanishPilotUse %>% mutate(across(ends_with("Use"), ~recode(., "Never" = "1", "Rarely" = "2", 
                                                                             "Sometimes" = "3", "Often" = "4", 
                                                                             "All the time" = "5")))
#Confirm
SpanishPilotUse %>% select(ends_with("Use")) %>% str()
#Select only relevant columns and exclude Catch trials
SpanishPilotUse <- SpanishPilotUse %>% select(ID, ends_with("Use"), 
                                            -c("Practice_Use", "A_Catch_Use", "S_Catch_Use"))
#Transfrom the character columns to numeric
SpanishPilotUse <- SpanishPilotUse %>% mutate(across(A9_Use:A2_Use, ~as.numeric(.)))
str(SpanishPilotUse)
#Gets overall confidence mean for each participant, excluding the Catch trial
SpanishPilotUse$A_MeanUse <- SpanishPilotUse %>% select(matches("A", ignore.case = FALSE), -c(ID)) %>% rowMeans()
SpanishPilotUse$S_MeanUse <- SpanishPilotUse %>% select(matches("S", ignore.case = FALSE), -c(ID)) %>% rowMeans()
describe(SpanishPilotUse$A_MeanUse) #mean = 3.17
describe(SpanishPilotUse$S_MeanUse) #mean = 2.87
#American gestures seen/used significantly more than Spanish gestures
t.test(SpanishPilotUse$A_MeanUse, SpanishPilotUse$S_MeanUse)

#Not a significant correlation for American accuracy and American gesture use
scatter.hist(SpanishPilot$American.Acc*10, SpanishPilotUse$A_MeanUse, xlim = c(0, 100), ylim = c(1, 5), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
cor.test(SpanishPilot$American.Acc, SpanishPilotUse$A_MeanUse, method = "spearman")

#Not a significant correlation for Spanish accuracy and Spanish gesture use
scatter.hist(SpanishPilot$Spanish.Acc*10, SpanishPilotUse$S_MeanUse, xlim = c(0, 100), ylim = c(1, 5), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
cor.test(SpanishPilot$Spanish.Acc, SpanishPilotUse$S_MeanUse, method = "spearman")



