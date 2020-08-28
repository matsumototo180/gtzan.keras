## FMAデータセットの楽曲をジャンル毎にフォルダ分けする
## （元のフォルダから楽曲ファイルをコピーし、別のフォルダにジャンル分けする）

library(tidyverse)
# setwd("/home/p2p/Documents/Datasets/FMA/")

## メタデータの読み込み
metadata <- read_csv("fma_metadata/tracks.csv", skip = 1)
metadata_tbl <- metadata %>% tibble()
metadata_tbl %>% select(genre_top) %>% table()

## すべての楽曲ファイルのパスを取得
files_path <- list.files("fma_small/", recursive = T, full.names = T)
files_path <- files_path[files_path %>% str_detect(".mp3")]

## ファイル名からファイルIDを抽出
files_id <- files_path %>% substr(16, 21) %>% str_remove_all("^0*")

## IDとジャンルの対応表を作る
id_genre_tbl <- tibble(ID = metadata_tbl$X1, genre = metadata_tbl$genre_top)[-1,]
## small版の対応表
id_genre_tbl_small <- tibble(ID = files_id) %>% mutate(genre = map_chr(files_id, ~ id_genre_tbl[which(id_genre_tbl$ID == .), ]$genre))
genre_list_small <- id_genre_tbl_small$genre %>% unique()
id_genre_tbl_small$genre %>% table()

## ファイルIDをジャンルごとにリスト化
files_id_sep <- genre_list_small %>% map(~ filter(id_genre_tbl_small, genre == .)) %>% map(~ .$ID)
names(files_id_sep) <- genre_list_small
## ファイルのパスをジャンルごとにリスト化
files_path_sep <- map(files_id_sep, ~ files_path[((files_path %>% str_sub(-10, -5) %>% str_remove_all("^0*")) %in% .)])

## コピー先のディレクトリを作成
dir.create("fma_small_separated/")
genre_list_small %>% map(~ dir.create(paste0("fma_small_separated/", .)))

## ファイル名をGTZANと同様のフォーマットになるようにしてコピーする
for (i in seq_along(genre_list_small)) {
  dest_dir <- paste0("fma_small_separated/", genre_list_small[i], "/")
  files_path_sep[[i]] %>% map(~ file.copy(., paste0(dest_dir, genre_list_small[i], ".", str_sub(., -10))))
}