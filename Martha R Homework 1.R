# Name: Martha Gizaw
# Homework #1
# Due February 11, 2020

# Working with Real-World Data

#=========================================================================================================#

# (1) Load the trees data into R. It is available in the Blackboard folder for homework 1.
# HINT: stringsAsFactors = F

trees <- read.csv("/Users/marth/OneDrive/Documents/R/Advanced R Portfolio/trees.csv", stringsAsFactors = F)
View(trees)

#=========================================================================================================#

# (2) For the "Spread" variable, replace any empty observations with NA.
trees$Spread[trees$Spread == ""] <- NA
# View(trees)

#=========================================================================================================#

# (3) Remove observations for which the "Height" variable is Not Available (NA).

# Create a subset that keeps all observation rows with available heights.
# trees <- na.omit(trees$Height)
trees <- trees[!is.na(trees$Height), ]
View(trees)

#=========================================================================================================#

# (4) Change the name of the "Spread" variable to "Diameter".

names(trees)[names(trees) == "Spread"] <- "Diameter" 
# View(trees)

#=========================================================================================================#

# (5) Remove the "Trunk" variable from the dataset.

trees$Trunk <- NULL
names(trees)
# View(trees)

#=========================================================================================================#

# (6) Change the "Tree_ID" variable to start at 1 and increase in increments of 1 for each additional 
# observation.

# First, clear the Tree_ID column with NA.
trees$Tree_ID <- NA

# Then, create a function that increments any number by 1 and fill the Tree_ID column with resulting
# values.

add.one <- function(number) {
  new_id <- number + 1
  return(new_id)
}

trees$Tree_ID[c(1:636)] <- add.one(c(0:635))

# Another way to write this...
# trees$Tree_ID[1] <- add.one(number=0)
# trees$Tree_ID[c(2:636)] <- add.one(c(1:635))

# View(trees)

#=========================================================================================================#

# (7) Create a new variable called "Town" that contains the town each tree is located in.
# HINT 1: this is the portion of the "Address" varible that comes after the comma.
# HINT 2: Use the stringr library.
# 
#     (7A) Then determine which town has the most trees in the dataset.
#     (7B) And report the average trunk size for trees in this town.

#install.packages("stringr")
library(stringr)

# Create the Town variable with an empty column.
trees$Town <- NA

# Then, extract the town portion of Address after the comma using the stringr library, and fill the 
# Town column with the resulting strings.
trees$Town[c(1:636)] <- str_extract(trees$Address[c(1:636)], '\\b[^,]+$')

# Another way to approach this without using Hint 2:
# trees$Town <- NA
# trees$Town[c(1:636)] <- gsub(".*, ","",trees$Address[c(1:636)])

# (A) Which town carries the most observations?
# Sort the values of Town and order them from most to least frequent. Check your answer.
freq_town <- names(sort(table(trees$Town), decreasing = T, na.last = T)[1])
freq_town

# (B) What is the mean/average tree trunk size in the most frequent town?
# Create a new subset called trunk_avg, and delete rows where the trunk size is NA.
trunk_avg <- trees[trees$Town == freq_town, ]
trunk_avg <- trunk_avg[!is.na(trunk_avg$Actual_Trunk_cm), ]

# Calculate the average trunk size in the most frequent town with the mean() function.
mean(trunk_avg$Actual_Trunk_cm)

View(trunk_avg)

#=========================================================================================================#
