library(readr)
library(dplyr)
library(ggplot2)
library(caTools)
library(stringr)
library(rpart)
library(rpart.plot)

# load data

path = "metacritic_output.csv"
dataset <- read_csv(path)

# clean data

dataset <- dataset[-which(dataset$genre == "No Data"), ]

dataset = na.omit(dataset)
print(colSums(is.na(dataset)))

dataset = dataset %>% 
  dplyr::select(genre, metascore, platform)

# remove nums from platform names

for (i in 1:length(dataset$platform)) {
  g = dataset$platform[i]
  g = str_replace_all(g, "[^a-zA-Z]", "")
  g = str_trim(g)
  dataset$platform[i] = g
}

# split genres into columns

genres_list = list()
for (i in 1:length(dataset$genre)) {
  g = dataset[i, 'genre']
  g = str_sub(g, start=2, end=-2)
  g = str_split(g, ",")[[1]]
  for (ii in 1:length(g)) {
    g[ii] = str_sub(str_trim(g[ii]), start=2, end=-2)
    g[ii] = str_replace_all(g[ii], "[^[:alnum:]]", "")
  }
  genres_list[i] = list(g)
}

# set a column to true for games that contain that genre

for (gli in 1:length(genres_list)) {
  for (genre in genres_list[[gli]]) {
    dataset[gli, genre] = TRUE
  }
}

# set all else to false

for (i in 4:length(colnames(dataset))) {
  dataset[i] = as.logical(unlist(dataset[i]))
  dataset[which(is.na(dataset[i])), i] <- FALSE
}

dataset = dataset %>% dplyr::select(-genre)

var_factors = c('platform')
dataset[var_factors] <- lapply(dataset[var_factors], factor)

# exploratory data analysis

sort(table(unlist(genres_list)))

ggplot(dataset) +
  aes(x = metascore, fill = platform) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  ggthemes::theme_stata()
 
# clean data some more

dataset$score <- ifelse(dataset$metascore >= 72, 'Good', dataset$metascore)
dataset$score <- ifelse(dataset$score < 72, 'Bad', dataset$score)
dataset$score <- as.factor(dataset$score)

dataset <- dataset %>% dplyr::select(-metascore)

# split data

set.seed(41)
split <- sample.split(Y=dataset$score, SplitRatio = 0.8)
train_set <- dataset[split,]
test_set <- dataset[!split,]

# make model

t_model <- rpart(score ~., data=train_set, method='class', cp = 0.00001)
# rpart.plot(t_model, box.palette="RdGn", fallen.leaves = F)

# check accuracy

pred <- predict(t_model, newdata=test_set, type='class')
confusion_matrix <- table(truth = test_set$score, predictions = pred)
print(confusion_matrix)

print(sprintf("Error: %g", round((confusion_matrix[1] / (confusion_matrix[1] + confusion_matrix[3]) +
               confusion_matrix[2] / (confusion_matrix[2] + confusion_matrix[4])) / 2, 4)))
