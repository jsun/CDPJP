library(tidyverse)
library(RSQLite)
library(digest)
library(stringi)



#' load_dataset
#' 
#' Load JPP-NET dataset from the specific directory
#' 
#' @param db_host a path to a directory saving original JPP-NET dataset
#' @return tibble
load_dataset <- function(db_host) {
    x <- NULL
    d_files <- list.files(db_host)
    for(d_file in d_files){
        x <- rbind(x,
                   read_csv(file.path(db_host, d_file),
                            locale = locale(encoding='CP932'),
                            show_col_types = FALSE))
    }
    x |>
        dplyr::rename(
            date = 調査年月日,
            prefecture = 都道府県名称,
            crop = 作物名称,
            damage = 病害虫名称,
            survey = 調査名,
            degree = 程度,
            value = データ,
            unit = 単位
        ) |>
        dplyr::filter(unit != '文字') |>
        dplyr::mutate(
            # normalize Japanese
            crop = stringi::stri_trans_nfkc(crop),
            damage = stringi::stri_trans_nfkc(damage),
            value = stringi::stri_trans_nfkc(value),
            # shorten long disease/pest names
            damage = stringr::str_replace_all(damage, '細菌性腐敗\\(腐敗病・軟\\)腐病', '細菌性腐敗'),
            damage = stringr::str_replace_all(damage, 'ナシマルカイガラムシ\\(サンホーゼカイガラムシ\\)', 'ナシマルカイガラムシ'),
            damage = stringr::str_replace_all(damage, 'カキノヘタムシガ\\(カキミガ\\)', 'カキノヘタムシガ'),
            # normalize survey methods
            survey = stringr::str_replace_all(survey, '^(本圃|本田)', ''),
            survey = stringr::str_replace(survey, '^被害.+率$', 'DR'),
            survey = stringr::str_replace(survey, '^寄生.+率$', 'DR'),
            survey = stringr::str_replace(survey, '^発病.+率$', 'DR'),
            # normalize crop names
            crop = stringr::str_replace_all(crop, '(春|夏|秋|冬)', ''),
        ) |>
        dplyr::filter(survey == 'DR') |>
        dplyr::select(- c(survey, degree, unit)) |>
        dplyr::mutate(value = as.numeric(value)) |>
        dplyr::filter(value != is.na(value)) |>
        dplyr::filter(value <= 100) |>
        dplyr::filter(date != is.na(date))
}




#' summarise_dataset
#' 
#' Summarise datasset.  After the summarisation, only one record will be
#' remained for each year-month combination.
summarise_dataset <- function(x) {
    dplyr::mutate(x, date = as.character(date)) |>
         dplyr::mutate(date = paste0(str_sub(date, start = 1, end = 7), '-01')) |>
         dplyr::group_by(date, prefecture, crop, damage) |>
         dplyr::summarise(value = max(value), .groups = 'drop')
}




#' create_table_prefectures
#'
#' Create table to store prefecture JIS code and name.
create_table_prefectures <- function(con, mst_data_dpath) {
    RSQLite::dbSendQuery(con, 'CREATE TABLE IF NOT EXISTS prefectures(name TEXT PRIMARY KEY, jis_code TEXT);')
    pref_df <- read.table(file.path(mst_data_dpath, 'prefectures.mst.tsv'),
                          header = TRUE, sep = '\t', colClasses = c('character', 'character'))
    if (RSQLite::dbGetQuery(con, 'SELECT count(*) FROM prefectures;')[1, 1] != 47) { 
        RSQLite::dbBegin(con)
        RSQLite::dbWriteTable(con, 'prefectures', pref_df, append = TRUE)
        RSQLite::dbCommit(con)
    }
}


#' create_table_damages
#'
#' Create table to store damages.
create_table_damages <- function(con, x) {
    RSQLite::dbSendQuery(con, 'CREATE TABLE damages(name TEXT PRIMARY KEY, type TEXT);')
    damage_df = data.frame(name = unique(x$damage),
                           type = ifelse(str_detect(unique(x$damage), '病'), '病害', '虫害'))
    RSQLite::dbBegin(con)
    RSQLite::dbWriteTable(con, 'damages', damage_df, append = TRUE)
    RSQLite::dbCommit(con)
}


#' create_table_crops
#'
#' Create table to store crops.
create_table_crops <- function(con, x) {
    RSQLite::dbSendQuery(con, 'CREATE TABLE crops(name TEXT PRIMARY KEY);')
    crop_df = data.frame(name = unique(x$crop))
    RSQLite::dbBegin(con)
    RSQLite::dbWriteTable(con, 'crops', crop_df, append = TRUE)
    RSQLite::dbCommit(con)
}


#' create_table_cropsdamages
#'
#' Create table to store cropsdamages
create_table_cropsdamages <- function(con, x) {
    RSQLite::dbSendQuery(con, paste0('CREATE TABLE cropsdamages(',
                                        'crop TEXT, ',
                                        'damage TEXT, ',
                                        'FOREIGN KEY (crop) REFERENCES crops(name), ',
                                        'FOREIGN KEY (damage) REFERENCES damages(name), ',
                                        'PRIMARY KEY (crop, damage));'))
    cropdamage_df = unique(data.frame(crop = x$crop, damage = x$damage))
    RSQLite::dbBegin(con)
    RSQLite::dbWriteTable(con, 'cropsdamages', cropdamage_df, append = TRUE)
    RSQLite::dbCommit(con)
}



#' create_table_data_sources
#' 
#' Create table to store data sources.
create_table_data_sources <- function(con, x) {
    RSQLite::dbSendQuery(con, paste0('CREATE TABLE data_sources(',
                                        'id INTEGER PRIMARY KEY, ',
                                        'name TEXT, desc TEXT, type TEXT, meta TEXT);'))
    damage_df = data.frame(id = c(1, 2, 3),
                           name = c('農水省予察事業', '調整値', '状態空間モデル'),
                           desc = c('農林水産省消費・安全局植物防疫課発生予察事業', '病害発病率、虫害被害率、害虫寄生率等のデータを統計的に統合して得られた値。', ''),
                           type = c('調査値', '調整値', '予測値'),
                           meta = c('', '', ''))
    RSQLite::dbBegin(con)
    RSQLite::dbWriteTable(con, 'data_sources', damage_df, append = TRUE)
    RSQLite::dbCommit(con)
}


#' create_table_records
#'
#' Create table to store records.
create_table_data <- function(con, x, source_id=NULL) {
    x$date <- as.character(x$date)
    x$source <- source_id
    RSQLite::dbSendQuery(con, paste0('CREATE TABLE IF NOT EXISTS data(',
                                     'date TEXT, ',
                                     'prefecture TEXT, ',
                                     'crop TEXT, ',
                                     'damage TEXT, ',
                                     'value REAL, ',
                                     'source INTEGER, ', 
                                     'FOREIGN KEY (prefecture) REFERENCES prefectures(name), ',
                                     'FOREIGN KEY (crop, damage) REFERENCES cropsdamages(crop, damage) ',
                                     'FOREIGN KEY (source) REFERENCES data_sources(id));'))
    RSQLite::dbBegin(con)
    RSQLite::dbWriteTable(con, 'data', x, append = TRUE)
    RSQLite::dbCommit(con) 
}





main <- function(damage_records_dpath, mst_data_dpath, db_host) {
    
    X <- load_dataset(damage_records_dpath)
    
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_host, synchronous = 'off')
    RSQLite::dbExecute(con, 'PRAGMA FOREIGN_KEYS = ON;')
    create_table_prefectures(con, mst_data_dpath)
    create_table_crops(con, X)
    create_table_damages(con, X)
    create_table_cropsdamages(con, X)
    create_table_data_sources(con)
    create_table_data(con, X, 1)
    create_table_data(con, summarise_dataset(X), 2)
    RSQLite::dbDisconnect(con)

}


args = commandArgs(trailingOnly = TRUE)
if (length(args) == 3) {
    damage_records_dpath <- args[1]
    mst_data_dpath <- args[2]
    db_host <- args[3]
    main(damage_records_dpath, mst_data_dpath, db_host)
} else {
    print('Rscript --vanilla create_db_damagerecords.R ./damage_records_data ./mst ../db.sqlite3')
}







