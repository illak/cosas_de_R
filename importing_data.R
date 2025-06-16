# Import swimming_pools.csv: pools
pools <- read.csv("data/swimming_pools.csv")

# Print the structure of pools
str(pools)


# Define the path
path <- file.path("~", "dataset", "swimming_pools.csv")

path


# Import hotdogs.txt: hotdogs
hotdogs <- read.delim("data/hotdogs.txt", header=FALSE,)

# Summarize hotdogs
summary(hotdogs)
str(hotdogs)


# Finish the read.delim() call
hotdogs <- read.delim("data/hotdogs.txt", header = FALSE, col.names = c("type", "calories", "sodium"))

# Select the hot dog with the least calories: lily
lily <- hotdogs[which.min(hotdogs$calories), ]

# Select the observation with the most sodium: tom
tom <- hotdogs[which.max(hotdogs$sodium), ]

# Print lily and tom
lily
tom


# Column classes ===============================================

# Previous call to import hotdogs.txt
hotdogs <- read.delim("data/hotdogs.txt", header = FALSE, col.names = c("type", "calories", "sodium"))

# Display structure of hotdogs
str(hotdogs)

# Edit the colClasses argument to import the data correctly: hotdogs2
hotdogs2 <- read.delim("data/hotdogs.txt", header = FALSE, 
                       col.names = c("type", "calories", "sodium"),
                       colClasses = c("factor","NULL","numeric"))


# Display structure of hotdogs2
str(hotdogs2)




# Path to the hotdogs.txt file: path
path <- file.path("data", "hotdogs.txt")

# Import the hotdogs.txt file: hotdogs
hotdogs <- read.table(path, 
                      sep = "\t", 
                      col.names = c("type", "calories", "sodium"))

# Call head() on hotdogs
head(hotdogs)



# Load the readr package
library(readr)

# Import potatoes.csv with read_csv(): potatoes
potatoes <- read_csv("data/potatoes.csv")





# Column names
properties <- c("area", "temp", "size", "storage", "method",
                "texture", "flavor", "moistness")

# Import potatoes.txt: potatoes
potatoes <- read_tsv("data/potatoes.txt", col_names = properties)

# Call head() on potatoes
head(potatoes)



# Column names
properties <- c("area", "temp", "size", "storage", "method",
                "texture", "flavor", "moistness")

# Import potatoes.txt using read_delim(): potatoes
potatoes <- read_delim("data/potatoes.txt", delim = "\t", col_names = properties)

# Print out potatoes
potatoes


# skip and n_max ===========================================

# Column names
properties <- c("area", "temp", "size", "storage", "method",
                "texture", "flavor", "moistness")

# Import 5 observations from potatoes.txt: potatoes_fragment
potatoes_fragment <- read_tsv("data/potatoes.txt", skip = 6, n_max = 5, col_names = properties)

potatoes_fragment



# col_types ================================================

# Column names
properties <- c("area", "temp", "size", "storage", "method",
                "texture", "flavor", "moistness")

# Import all data, but force all columns to be character: potatoes_char
potatoes_char <- read_tsv("data/potatoes.txt", col_types = "cccccccc", col_names = properties)

# Print out structure of potatoes_char
str(potatoes_char)


# col_types with collectors ==================================


# Import without col_types
hotdogs <- read_tsv("data/hotdogs.txt", col_names = c("type", "calories", "sodium"))

# Display the summary of hotdogs
summary(hotdogs)

# The collectors you will need to import the data
fac <- col_factor(levels = c("Beef", "Meat", "Poultry"))
int <- col_integer()

# Edit the col_types argument to import the data correctly: hotdogs_factor
hotdogs_factor <- read_tsv("data/hotdogs.txt",
                           col_names = c("type", "calories", "sodium"),
                           col_types = list(fac,int,int))

# Display the summary of hotdogs_factor
summary(hotdogs_factor)


# fread ====

# load the data.table package using library()
library(data.table)

# Import potatoes.csv with fread(): potatoes
potatoes <- fread("data/potatoes.csv")

# Print out potatoes
potatoes

#Suppose you have a dataset that contains 5 variables and you want to keep
# the first and fifth variable, named "a" and "e". The following options 
# will all do the trick:

# fread("path/to/file.txt", drop = 2:4)
# fread("path/to/file.txt", select = c(1, 5))
# fread("path/to/file.txt", drop = c("b", "c", "d"))
# fread("path/to/file.txt", select = c("a", "e"))


# Import columns 6 and 8 of potatoes.csv: potatoes
potatoes <- fread("data/potatoes.csv", select = c("texture","moistness"))

# Plot texture (x) and moistness (y) of potatoes
plot(potatoes$texture, potatoes$moistness)


# List the sheets of an Excel file ====
# Load the readxl package
library("readxl")

# Print the names of all worksheets
excel_sheets("data/urbanpop.xlsx")


# Read the sheets, one by one
pop_1 <- read_excel("data/urbanpop.xlsx", sheet = 1)
pop_2 <- read_excel("data/urbanpop.xlsx", sheet = 2)
pop_3 <- read_excel("data/urbanpop.xlsx", sheet = 3)

# Put pop_1, pop_2 and pop_3 in a list: pop_list
pop_list <- list(pop_1, pop_2, pop_3)

# Display the structure of pop_list
str(pop_list)


# Reading a workbook ====
# Read all Excel sheets with lapply(): pop_list
pop_list <- lapply(excel_sheets("data/urbanpop.xlsx"),
                   read_excel,
                   path = "data/urbanpop.xlsx")

# Display the structure of pop_list
str(pop_list)


# The col_names argument ====
# Import the first Excel sheet of urbanpop_nonames.xlsx (R gives names): pop_a
pop_a <- read_excel("data/urbanpop.xlsx", col_names = FALSE)

# Import the first Excel sheet of urbanpop_nonames.xlsx (specify col_names): pop_b
cols <- c("country", paste0("year_", 1960:1966))
pop_b <- read_excel("data/urbanpop.xlsx", col_names = cols)

# Print the summary of pop_a
summary(pop_a)

# Print the summary of pop_b
summary(pop_b)




# Import the second sheet of urbanpop.xlsx, skipping the first 21 rows: urbanpop_sel
urbanpop_sel <- read_excel("data/urbanpop.xlsx", skip = 21, col_names = FALSE, sheet = 2)

# Print out the first observation from urbanpop_sel
head(urbanpop_sel, n = 1)



# Connect to a workbook ====

# Load the XLConnect package
library("XLConnect")

# Build connection to urbanpop.xlsx: my_book
my_book <- loadWorkbook("data/urbanpop.xlsx")

# Print out the class of my_book
class(my_book)