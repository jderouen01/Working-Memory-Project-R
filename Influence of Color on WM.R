
## load packages
library(readxl)
library(dplyr)
library(ggpubr)

## Set Directory
setwd("~/R/InfCol")

## Import the data from excel
InfCol <- read_excel("Influence of Color on Working Memory_April 25, 2022_15.28.xlsx")

## Data Cleaning

# Unnecessary data/ outliers
InfCol <- InfCol[-c(1,34), -c(1:5,8:19)]
InfCol <- InfCol[,-c(36:81)]

# Filtering out incomplete/irrelevant data
InfCol <- InfCol[(InfCol$Finished == "True"),]
InfCol <- InfCol[(InfCol$`Colorblind ` == "No"),]
InfCol <- InfCol[,-c(2,3)]

# Correcting incongruities
InfCol$Age[10] <- "22"
InfCol$Age[44] <- "20"

#Cleaning Column Names and Classes
InfCol <- InfCol %>% rename(c(Duration = `Duration (in seconds)`))

# Count Variable
# Need a variable to count all the words given by the participant that were correctly remembered.
InfCol <- InfCol %>% sapply(function(.) {
      if(is.character(.)) return(tolower(.))
      else return(.)
})
InfCol <- data.frame(InfCol)

# Our List of words
W <- list("unit", "golf", "solo", "slam", "fate", "iron", "rear", "grip", "rage", "room", "tone",
          "pour", "snap", "lily","easy","good","band","fame","lump","mile","part","mole","snub",
          "case","club","dance","solve","green", "utter", "terms","spare","creed","blank","choke",
          "noble","place","trial","dough","ridge","obese","elite","sweep","lover","feign", "truth",
          "seize", "smart","aware","grind","clean","carpet","tender","wonder","ballot","manual",
          "empire","critic","reject","reader","sleeve","cheese","chorus","galaxy","listen","infect",
          "makeup","barrel","banish","bronze","stroke","action","doctor","exotic","deputy","gutter")

# Checking to see if participants words matched the words given 
InfCol[,5:33] <- apply(InfCol[,5:33], 2, function(x) x %in% W)

# Summing them to form Count Variable
InfCol[,5:33] <- sapply(InfCol[,5:33], as.integer)
InfCol <- InfCol %>% mutate(Count = apply(InfCol[,5:33], 1, sum))
InfCol <- InfCol[,-c(5:33)]

# Renaming factors 
InfCol <- InfCol %>% mutate(Color = 
            case_when(FL_18_DO == "block4" ~ "Red", FL_18_DO == "block5" ~ "Blue", FL_18_DO == "block6" ~ "Black"))
InfCol <- InfCol[,-c(5)]

# Fixing Coloumn Classes
# Race ~ "white" = 1, "black" =2, "latino" = 3, "asian" = 4
InfCol$Race <- as.factor(InfCol$Race)
levels(InfCol$Race) <- c("white", "black or african american", "latino", "asian")
IC_num <- c("Duration", "Age")
InfCol[IC_num] <- sapply(InfCol[IC_num], as.integer)
# Gender ~ "female" = 1, "male" = 2
InfCol$Gender <- as.factor(InfCol$Gender)
# Color ~ "Black" = 1, "Blue" = 2, "Red" = 3
InfCol$Color <- as.factor(InfCol$Color)

##Significance Testing

## Color & Count

# descriptive statistics by group
group_by(InfCol, Color) %>% summarise(
  count = n(),
  mean = mean(Count, na.rm = TRUE),
  sd = sd(Count, na.rm = TRUE),
  median = median(Count, na.rm = TRUE),
  IQR = IQR(Count, na.rm = TRUE)
)

hist(InfCol$Count)

# non-normal distribution so nonparametric tests
IC_KW <- kruskal.test(Count ~ Color, InfCol)
IC_KW

# Data visualization
ggboxplot(InfCol, x = "Color", y = "Count",
          color = "Color", palette = c("#fc3700", "#000dff", "#000000"),
          order = c("Red", "Blue", "Black"),
          ylab = "Count", xlab = "Color"
)


## Count and Duration

#removing Outliers
InfColD <- InfCol %>% filter(Duration < 1000)

#linear regression
#nonparametric, we will use Kendall's correlation
ICcor <- cor.test(InfColD$Duration, InfColD$Count, method = "kendall")
ICcor

# medium positive correlation found between Duration and Count
# Plotting Duration and Count, clear posiitvie correlation
ggplot(InfColD, aes(x = Duration, y = Count)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      ggtitle("Effects of Duration on Working Memory")

