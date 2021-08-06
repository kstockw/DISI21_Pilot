getwd()
library(tidyverse)
library(psych)
#gets rid of scientific notation
options(scipen = 999)

##############July 21, 2021 Kayden Stockwell, DISI 2021 project on gestures as a social marker##############

####Data Cleaning####
#Load in the American/French Pilot data for the DISI 2021 social gategorization from gestures task 
#Remove the row containing the complete questions text (e.g. How many langueges other than English do you speak?)
FrenchPilot <- read.csv("Gesture_FrenchPilot.csv", stringsAsFactors = FALSE)[-1,]
#Remove fake data from prior to opening the survey (those without a ProlificID)
FrenchPilot <- subset(FrenchPilot, ProlificID != "")
#Remove unwanted/empty columns
colnames(FrenchPilot)
FrenchPilot <- FrenchPilot %>% select(-c(Status, IPAddress, Finished, RecordedDate:DistributionChannel))
#Rename _Time_Click.Count columns to just _Click.Count
names(FrenchPilot) <- gsub(x = names(FrenchPilot), pattern = "\\Time_Click.Count", replacement = "Click.Count") 
#Change some columns to correct data type
FrenchPilot <- FrenchPilot %>% mutate(across(c(Progress, Duration..in.seconds., Age, matches("Time")), as.numeric))
FrenchPilot <- FrenchPilot %>% mutate(across(c(UserLanguage, Gender, Languages, Countries, Sound_check, Recognition), 
                                         as.factor))
str(FrenchPilot)
table(FrenchPilot$Gender)
#Check if any page took longer than ~35 seconds to submit (shouldn't unless there was a technical error, look at max column)
#One participant had F5 take ~44 seconds to submit, this only occured once and was likely an error or that person's internet
#took a minute to think, probably fine to keep in
FrenchPilot %>% select(ends_with("Page.Submit")) %>% describe() %>% arrange(desc(max))
#For right now, I don't care about the time it took for page to submit or the page click count, so remove those columns
FrenchPilot <- FrenchPilot %>% select(-(contains(c("Time", "Click"))))
#Give participants a simple ID number
FrenchPilot <- add_column(.data = FrenchPilot, .before = 1, "ID" = 1:nrow(FrenchPilot))


##Decide if want to exclude participants who said practice trial was Not American, those that didn't give expected answer
##for catch trials, those who lived in other countires, those who said tech difficulty for seeing video etc 

#Check for who didn't see videos. 63 didn't see 9 videos, 70 didn't see 4 videos, 68 misclicked "didn't see", 
#18 didn't see 1 video
DidntSeeF <- FrenchPilot %>% filter(if_any(ends_with("Count"), ~ . == "I did not see the video")) %>% select(ID, matches("Count"))
#Checking if any of above gave feecback on why they missed videos
FrenchPilot %>% filter(ID == 18 | ID == 70 | ID == 63 | ID == 68) %>% select(Categorization, Feedback)
#Removing anyone who actually missed a video (can just not run/edit this line if want to include them)
FrenchPilot <- FrenchPilot %>% filter(ID != 18 & ID != 70 & ID != 63)


####Analyses####
#Calculate the accuracy percentage for when speaker was American
#Creates columns with dummy coded (0 = American, 1 = Not American) Categorization Columns for American video and a sum column
#showing how many times (10 possible) participants said Not American for American video (Acc.American)
#Doesn't included catch trials as those end in "Categ"
FrenchPilot <- FrenchPilot %>%
  mutate(American = across(.cols = starts_with("A") & ends_with("Categorization"), 
                           .fns = str_count, "Not American")) %>% 
  rowwise() %>% 
  mutate(American.Acc = across(.cols = contains("American"), .fns = sum))
#Shows all new columns created by mutate, kind of weird it shows 1 column for 10 columns. Might be issue later...
FrenchPilot$American
#To prevent possible issue, unnest the 10 columns and store. 
DummyAmericanColsF <- unnest(FrenchPilot$American)
#Rename them to avoid confusion with original columns
names(DummyAmericanColsF) <- gsub(x = names(DummyAmericanColsF), pattern = "\\Categorization", replacement = "DummyCat")
#Remove the 10 columns that are somehow nested in the main dataframe (column number of American may change so double check)
colnames(FrenchPilot)
FrenchPilot <- cbind(FrenchPilot[,-(112)], DummyAmericanColsF)
#Make Acc.American and A_DummyCat columns clearer by having them show how many times American was selected (Acc shows sum 
#of how many time American was selected for American vidoes (10 possible) and DummyCats reflect 1 = American and 
#0 = Not American)
FrenchPilot$American.Acc <- abs(FrenchPilot$American.Acc - 10) %>% unlist() %>% as.numeric()
#Note that this line makes all these columns characters, don't think it matters as of now
FrenchPilot <- FrenchPilot %>% mutate(across(ends_with("DummyCat"), ~recode(., "0" = "1", "1" = "0")))
#What is the pilot sample mean accuracy for American videos?
mean(FrenchPilot$American.Acc) #6.119403 or ~61%
#Mean overall accuracy differences by gender (1 = female, 2 = male)? No
FrenchPilot %>% filter(as.numeric(Gender) == 1) %>% select(American.Acc) %>% describe() #6.16 or ~62%
FrenchPilot %>% filter(as.numeric(Gender) == 2) %>% select(American.Acc) %>% describe() #6.15 or ~62%

#Calculate the accuracy percentage for when speaker was French
#Creates columns with dummy coded (0 = American, 1 = Not American) Categorization Columns for French video and a sum column
#showing how many times (10 possible) participants said Not American for French video (Acc.French). Note here the accuracy
#column is fine as is
#Doesn't included catch trials as those end in "Categ"
FrenchPilot <- FrenchPilot %>%
  mutate(French = across(.cols = starts_with("F") & ends_with("Categorization"), 
                         .fns = str_count, "Not American")) %>% 
  rowwise() %>% 
  mutate(French.Acc = across(.cols = contains("French"), .fns = sum))
#Shows all new columns created by mutate, kind of weird it shows 1 column for 10 columns. Might be issue later...
FrenchPilot$French
#To prevent possible issue, unnest the 10 columns and store. 
DummyFrenchCols <- unnest(FrenchPilot$French)
#Rename them to avoid confusion with original columns
names(DummyFrenchCols) <- gsub(x = names(DummyFrenchCols), pattern = "\\Categorization", replacement = "DummyCat")
#Remove the 10 columns that are somehow nested in the main dataframe (column number of French may change so double check)
colnames(FrenchPilot)
FrenchPilot <- cbind(FrenchPilot[,-(123)], DummyFrenchCols)
FrenchPilot$French.Acc <- FrenchPilot$French.Acc %>% unlist() %>% as.numeric()
#What is the sample mean accuracy for French videos?
mean(FrenchPilot$French.Acc) #5.880597 or ~59%
#Mean overall accuracy differences by gender(1 = female, 2 = male)? No
FrenchPilot %>% filter(as.numeric(Gender) == 1) %>% select(French.Acc) %>% describe() #5.81 or ~58%
FrenchPilot %>% filter(as.numeric(Gender) == 2) %>% select(French.Acc) %>% describe() #5.85 or ~59%

#Do participants differ from chance level guessing (50%)?
t.test(FrenchPilot$French.Acc, mu = 5) #p = 0.00218, yes
t.test(FrenchPilot$American.Acc, mu = 5) #p = 0.00000001229, yes
#Do accuracy scores differ for French and for American videos?
t.test(FrenchPilot$French.Acc, FrenchPilot$American.Acc) #p = 0.4645, no

#Mean accuracy for each French video
FrenchPilot %>% select(contains("F") & contains("Dummy")) %>% describe() %>% arrange(mean)
#Mean accuracy for each American video
FrenchPilot %>% select(matches("A", ignore.case = FALSE)  & contains("Dummy")) %>% describe() %>% arrange(mean)

#Plotting relationship between American and French overall accuracy (check if should be Spearman or Pearson correlation)
scatter.hist(FrenchPilot$American.Acc/10, FrenchPilot$French.Acc/10, xlim = c(0, 1), ellipse = FALSE, method = "spearman",
             smooth = FALSE, ab = TRUE)
cor.test(FrenchPilot$American.Acc, FrenchPilot$French.Acc, method = "spearman")


#Use psycho library to calculate signal detection theory metrics
library(psycho)
#Calculating for each participant
#Correct Rejection is saying a French speaker is Not American
Correct_RejectF <- FrenchPilot$French.Acc
#Miss is saying an American speaker is Not American
MissesF <- abs(FrenchPilot$American.Acc - 10)
#False Alarm is saying a French speaker is American
False_AlarmF <- abs(FrenchPilot$French.Acc - 10)
#Hit is saying an American speaker is American
HitF <- FrenchPilot$American.Acc
#Confirm all equal total stimulus number (20 as of July 21, 2021)
Correct_RejectF + MissesF + False_AlarmF + HitF
#Calculates d prime, beta, and c for each participant. a prime and bppd are non-parametric estimates (see help documentation)
#Unbiased observer has beta around 1.0, as bias to say yes (American) increases beta gets closer to 0.0, as bias 
#to say no (Not American) increases beta is larger than 1.0.
#Looks like this comes up with one NA and one NaN in the non-parametric output? Not sure why
dPrimeF <- dprime(n_hit = HitF, n_fa= False_AlarmF, n_miss = MissesF, n_cr = Correct_RejectF) %>% as.data.frame()
##Parametric measure of sensitivity; I think chance guessing is at at 0.5, but not 100% sure on that
mean(dPrimeF$dprime) #0.4928188
describe(dPrimeF$dprime)
#Think this suggests pretty unbiased or just the slightest conservative bias
mean(dPrimeF$beta) #1.164805
#Not really sure how to interpret c
mean(dPrimeF$c) #-0.03526052
#mean aPrime (non-parametric accuracy), 0.5 means chance performance
mean(dPrimeF$aprime, na.rm = TRUE) #0.6518639
#mean non-parametric bias estimate, 0 is no bias, positive number is slight conservative (answer no/Not American) bias
mean(dPrimeF$bppd, na.rm = TRUE) #0.09008934
#Add output to main dataframe
FrenchPilot <- cbind(FrenchPilot, dPrimeF)

#aprime does not differ by gender (1 = female, 2 = male)
FrenchPilot %>% filter(as.numeric(Gender) == 1) %>% select(aprime) %>% describe() #0.65
FrenchPilot %>% filter(as.numeric(Gender) == 2) %>% select(aprime) %>% describe() #0.65

#bppd does not differ by gender (numerically but not signiciantly different according to 2 sample t test)
FrenchPilot %>% filter(as.numeric(Gender) == 1) %>% select(bppd) %>% describe() #0.13
FrenchPilot %>% filter(as.numeric(Gender) == 2) %>% select(bppd) %>% describe() #0.05

#not sure if this is correct mu for dprime, other mus I am pretty sure are correct
t.test(FrenchPilot$dprime, mu = 0, alternative = "two.sided") #p = 0.0000000003126
t.test(FrenchPilot$aprime, mu = 0.5, alternative = "two.sided") #p = 0.0000000000204
t.test(FrenchPilot$beta, mu = 1, alternative = "two.sided") #p = 0.005159
t.test(FrenchPilot$bppd, mu = 0, alternative = "two.sided") #p = 0.001957

#Plot relationship between A' sensativity and B" bias measure (check if spearman is correct correlation)
scatter.hist(FrenchPilot$aprime, FrenchPilot$bppd, xlim = c(0, 1.0), ylim = c(-1, 1), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
#Significant positive correlation
cor.test(FrenchPilot$aprime, FrenchPilot$bppd, method = "spearman")

#When bias is more conservative, French accuracy goes up significantly, but not significant negative cor for American
cor.test(FrenchPilot$French.Acc, FrenchPilot$bppd, method = "spearman")
cor.test(FrenchPilot$American.Acc, FrenchPilot$bppd, method = "spearman")
plot(FrenchPilot$French.Acc, FrenchPilot$bppd)
plot(FrenchPilot$American.Acc, FrenchPilot$bppd)
scatter.hist(FrenchPilot$American.Acc*10, FrenchPilot$bppd, xlim = c(0, 100), ylim = c(-1, 1), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman", xlab = "Percent Accuracy: American Videos", ylab = "Response Bias (B'')",
             title = "Relation of Percenty Accuracy (American Videos)\nand Response Bias")
scatter.hist(FrenchPilot$French.Acc*10, FrenchPilot$bppd, xlim = c(0, 100), ylim = c(-1, 1), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman", xlab = "Percent Accuracy: French Videos", ylab = "Response Bias (B'')",
             title = "Relation of Percenty Accuracy (French Videos)\nand Response Bias")

#save FrenchPilot csv so you can use it in graphs
#write.csv(FrenchPilot, file = "FrenchPilotAccSDT.csv", row.names = FALSE)



##Confidence of Rating, and Use/Exposure to Gestures

#Turn Confidence data into numeric to look at means, median etc
FrenchPilotConf <- FrenchPilot 
FrenchPilotConf <- FrenchPilotConf %>% mutate(across(contains("Confi"), ~recode(., "Not confident at all" = "1", "Slightly confident" = "2",
                                                         "Somewhat confident" = "3", "Fairly confident" = "4",
                                                         "Very confident" = "5")))
#Confirm
FrenchPilotConf %>% select(contains("Confi")) %>% str()
#Select only relevant columns and exclude Catch trials
FrenchPilotConf <- FrenchPilotConf %>% select(ID, contains("Confi"), 
                                              -c("Practice_Confidence", "A_Catch_Confidence", "F_Catch_Confidence"))
#Transfrom the character columns to numeric
FrenchPilotConf <- FrenchPilotConf %>% mutate(across(A9_Confidence:A2_Confidende, ~as.numeric(.)))
str(FrenchPilotConf)
#Gets overall confidence mean for each participant, excluding the Catch trial
FrenchPilotConf$A_MeanConfidence <- FrenchPilotConf %>% select(matches("A", ignore.case = FALSE), -c(ID)) %>% rowMeans()
FrenchPilotConf$F_MeanConfidence <- FrenchPilotConf %>% select(matches("F", ignore.case = FALSE), -c(ID)) %>% rowMeans()
describe(FrenchPilotConf$A_MeanConfidence) #mean = 2.8
describe(FrenchPilotConf$F_MeanConfidence) #mean = 2.72

#Significant positive correlation for American accuracy and American confidence (check if Spearman is right method)
scatter.hist(FrenchPilot$American.Acc*10, FrenchPilotConf$A_MeanConfidence, xlim = c(0, 100), ylim = c(1, 5), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
cor.test(FrenchPilot$American.Acc, FrenchPilotConf$A_MeanConfidence, method = "spearman")

#Not a significant correlation for French accuracy and French confidence
scatter.hist(FrenchPilot$French.Acc*10, FrenchPilotConf$F_MeanConfidence, xlim = c(0, 100), ylim = c(1, 5), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
cor.test(FrenchPilot$French.Acc, FrenchPilotConf$F_MeanConfidence, method = "spearman")


#Turn gesture use data into numeric to look at means, median etc
FrenchPilotUse <- FrenchPilot 
FrenchPilotUse <- FrenchPilotUse %>% mutate(across(ends_with("Use"), ~recode(., "Never" = "1", "Rarely" = "2", 
                                                                            "Sometimes" = "3", "Often" = "4", 
                                                                            "All the time" = "5")))
#Confirm
FrenchPilotUse %>% select(ends_with("Use")) %>% str()
#Select only relevant columns and exclude Catch trials
FrenchPilotUse <- FrenchPilotUse %>% select(ID, ends_with("Use"), 
                                              -c("Practice_Use", "A_Catch_Use", "F_Catch_Use"))
#Transfrom the character columns to numeric
FrenchPilotUse <- FrenchPilotUse %>% mutate(across(A9_Use:A2_Use, ~as.numeric(.)))
str(FrenchPilotUse)
#Gets overall confidence mean for each participant, excluding the Catch trial
FrenchPilotUse$A_MeanUse <- FrenchPilotUse %>% select(matches("A", ignore.case = FALSE), -c(ID)) %>% rowMeans()
FrenchPilotUse$F_MeanUse <- FrenchPilotUse %>% select(matches("F", ignore.case = FALSE), -c(ID)) %>% rowMeans()
describe(FrenchPilotUse$A_MeanUse) #mean = 2.99
describe(FrenchPilotUse$F_MeanUse) #mean = 2.66
#American gestures seen/used significantly more than French gestures
t.test(FrenchPilotUse$A_MeanUse, FrenchPilotUse$F_MeanUse)

#Not a significant correlation for American accuracy and American gesture use
scatter.hist(FrenchPilot$American.Acc*10, FrenchPilotUse$A_MeanUse, xlim = c(0, 100), ylim = c(1, 5), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
cor.test(FrenchPilot$American.Acc, FrenchPilotUse$A_MeanUse, method = "spearman")

#Not a significant correlation for French accuracy and French gesture use
scatter.hist(FrenchPilot$French.Acc*10, FrenchPilotUse$F_MeanUse, xlim = c(0, 100), ylim = c(1, 5), ellipse = FALSE, smooth = FALSE,
             ab = TRUE, method = "spearman")
cor.test(FrenchPilot$French.Acc, FrenchPilotUse$F_MeanUse, method = "spearman")









