library(tidyverse)
library(tuneR)
library(seewave)
library(fftw)
library(kernlab)
library(e1071)
library(caret)

setwd("/home/p2p/Documents/Datasets/FMA/")

files_path_list <- list.files("fma_small_separated/") %>% map(~ list.files(paste0("fma_small_separated/", ., "/"), full.names = T))
names(files_path_list) <- list.files("fma_small_separated/")

songs_list <- files_path_list[names(files_path_list) %in% c("Rock", "Hip-Hop")]  %>% map(~ head(., 500)) %>% map(~ map(., ~ readWave(.)))

songs_list %>% map(~ map_int(., ~ NROW(.@left)) %>% min)
song_length <- 639450
print(song_length/22050)

alignSongLength <- function(wav, sample_length){
  wav@left <- wav@left[1:sample_length]
  return(wav)
}

songs_list <- songs_list %>% map(~ map(., ~ alignSongLength(., song_length)))
songs_list %>% map(~ map_int(., ~ NROW(.@left)) %>% summary())

songs_list <- songs_list %>% map(~ map(., ~ downsample(., 11025)))

melfcc_list <- songs_list %>% map(~ map(., ~ melfcc(., wintime = 0.04496124 * 2, hoptime = 0.01124031 * 2, numcep = 64))) 
melfcc_tbl_list <- melfcc_list %>% map(~ map_dfr(., ~ tibble(t(c(.)))))
melfcc_tbl_list <- melfcc_tbl_list %>% imap(~ mutate(.x, genre = names(melfcc_tbl_list[.y])))


# svm ---------------------------------------------------------------------
samplenum_list <- melfcc_tbl_list %>% map(~ nrow(.))
train_size <- 0.7

train_sampleids_list <- map(samplenum_list, ~ sample(., . * train_size))

training <- map2_dfr(melfcc_tbl_list, train_sampleids_list, ~ .x[.y, ])
predicting <- map2_dfr(melfcc_tbl_list, train_sampleids_list, ~ .x[-.y, ])

training <- training[complete.cases(training), ]
predicting <- predicting[complete.cases(predicting), ]

training$genre <- training$genre %>% as.factor()
predicting$genre <- predicting$genre %>% as.factor()

svm_model <- train(genre ~ ., data = training, method="svmRadial", tuneLength=3, preProcess=c("center", "scale"), trControl = trainControl(method = "cv"))

svm_predict <- predict(svm_model, predicting)
confusionMatrix(data = svm_predict, predicting$genre)

