# Load Libraries
library(tidyverse)
library(data.table)
library(class)  # kNN 
library(gmodels) # CrossTable

# Load Data

data.path <- "S1_Dataset"
csv.files <- list.files(path=data.path, full.names = T, recursive = T)

# Filtering description files
csv.files <- csv.files[str_detect(csv.files, "..*(M|F)$")]

LoadData <- function(file_list) { 
  tables <- lapply(file_list, ReadCsv)
  data.frame <- do.call(rbind, tables)
  
  # Transform activity to lables
  data.frame$activity <- factor(data.frame$activity, levels = c(1, 2, 3, 4),
                                labels = c("SitOnBed","SitOnChair","Lying","Ambulating"))
  
  return(data.frame)
}

ReadCsv <- function(file_name) {
  # Read csv file
  table <- read_csv(file_name, 
                    col_names = c("time", "x-axis", "y-axis", "z-axis", "sensor", "rssi", "phase", "frequency", "activity"))
  
  # Adding columns by parsing filenames and directories
  file.name <- str_split(file_name, "/", simplify = TRUE, n = 5)
  table['gender'] <- as.integer(ifelse(str_sub(file.name[5], -1) == "F", 0, 1))
  table['room_id'] <- as.integer(ifelse(str_sub(file.name[4], 1,2) == "S1", 1, 2))
  
  return(table)
}

data.frame <- LoadData(csv.files)


# Data Analysis

str(data.frame)
summary(data.frame)
head(data.frame)
data <- data.frame[, 1:9]
# Check the percentage of Missing values 
missing_values <- data %>% summarize_all(funs(sum(is.na(.))/n()))
gather(missing_values, key="feature", value="missing_pct")

# Data visulization
data %>% 
  ggplot(aes(x = activity)) +
  geom_histogram(stat="count")

# Machine Learning
set.seed(1985)
data <- data[order(runif(nrow(data))),]

# Spliting data into train and test
train.data        <- data[1:round((.9 * nrow(data))), -9]
train.data.lables <- data[1:round((.9 * nrow(data))), 9, drop = TRUE]

test.data        <- data[(round(.9 * nrow(data)) + 1):nrow(data), -9]
test.data.lables <- data[(round(.9 * nrow(data)) + 1):nrow(data), 9, drop = TRUE]

# Constructing model
model.pred <- knn(train = train.data, 
                  test = test.data,
                  cl = train.data.lables, 
                  k=260)

# Check the percentage of each activity
table(test.data.lables)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = test.data.lables, 
           y = model.pred,
           prop.chisq=FALSE)
# Accuracy (First Try kNN)
(0.107 + 0.003 + 0.640 + 0.003)

data.z<- as.data.frame(scale(data[-9]))
train.data.z        <- data.z[1:round((.9 * nrow(data.z))),]
train.data.z.lables <- data[1:round((.9 * nrow(data))), 9, drop = TRUE]

test.data.z        <- data.z[(round(.9 * nrow(data.z)) + 1):nrow(data.z),]
test.data.z.lables <- data[(round(.9 * nrow(data)) + 1):nrow(data), 9, drop = TRUE]


model.pred.z <- knn(train = train.data.z, 
                    test = test.data.z,
                    cl = train.data.z.lables, 
                    k=260)


CrossTable(x = test.data.z.lables, 
           y = model.pred.z,
           prop.chisq=FALSE)


# Accuracy (After z-score standardization)
(.21 + .058 + .683 + .01)























