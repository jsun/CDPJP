source('model_core.R')

#' fit
#' 
#' @param rdata_fpath A file path to save the fitting result.
#' @param model_type A character of model type
#' @param min_x If not `NA`, then round values less than `min_x` to `min_x`.
#' @param verbose Show process messages if `TRUE`.
#' 
#' source('run_modeling.R')
#' model_type <- 'PA'
#' min_x <- NA
#' rdata_fpath <-MODELS_RDATA
#' 
fit <- function(rdata_fpath, model_type, min_x = NA, verbose = TRUE) {
    rdata_fpath <- paste0(rdata_fpath, '.', model_type)
    
    models <- rmses <- list()
    models[[model_type]] <- list()
    rmses[[model_type]] <- data.frame()
    save(models, rmses, file = rdata_fpath)
    
    n_models <- prod(sapply(dimnames(DR), length)[c(1, 2, 4)])
    tries_i <- 0
    
    for (crop in dimnames(DR)[[1]]) {
        for (damage in dimnames(DR)[[2]]) {
            for (prefecture in dimnames(DR)[[4]]) {
                tries_i <- tries_i + 1
                x <- arrange_dataset(DR, WD, crop, damage, prefecture) |>
                        dplyr::select(date, value, temperature_avg, precipitation_ttl, humidity_avg, sunshineduration_ttl)
                
                # scaling
                x$temperature_avg <- as.numeric(scale(x$temperature_avg))
                x$precipitation_ttl <- as.numeric(scale(x$precipitation_ttl))
                x$humidity_avg <- as.numeric(scale(x$humidity_avg))
                x$sunshineduration_ttl <- as.numeric(scale(x$sunshineduration_ttl))
                
                if (!all(is.na(x$value))) {
                    # create a placeholder
                    if (verbose) message(paste0(
                        'MODELING. TYPE: ', model_type, '; ',
                        'DATA: ', crop, ' ', damage, ' ', prefecture, ' ',
                        '[', tries_i, '/', n_models, '(', round(tries_i / n_models, 4) * 100, '%)]'))
                    if (!(crop %in% names(models[[model_type]]))) models[[model_type]][[crop]] <- list()
                    if (!(damage %in% names(models[[model_type]][[crop]]))) models[[model_type]][[crop]][[damage]] <- list()
                    if (!is.na(min_x))
                        x$value[x$value < min_x] <- min_x
                    
                    # fitting
                    m <- switch (model_type,
                                 'RAND' = CDF_RAND$new(),
                                 'LOGRAND' = CDF_LOGRAND$new(),
                                 'PA' = CDF_PA$new(),
                                 'PAX' = CDF_PAX$new(),
                                 'ARIMA' = CDF_ARIMA$new(),
                                 'SARIMA' = CDF_SARIMA$new(),
                                 'SARIMAX' = CDF_SARIMAX$new(),
                                 'GPR' = CDF_GPR$new(),
                                 'RF' = CDF_RF$new(),
                                 stop('Unsupported Model Type.')
                    )
                    m$fit_ts(y = x$value, d = x$date, x = x[, 3:ncol(x)], n_min = 3, valid_from = -Inf)
                    
                    # save fitting results
                    models[[model_type]][[crop]][[damage]][[prefecture]] <- NULL
                    if (!is.null(m$get_field('model')) && !is.null(m$get_field('valid_stats')$rmse)) {
                        models[[model_type]][[crop]][[damage]][[prefecture]] <- m
                        rmses[[model_type]] <- dplyr::bind_rows(
                                                    rmses[[model_type]],
                                                    data.frame(crop = crop,
                                                                damage = damage,
                                                                prefecture = prefecture, 
                                                                model = model_type,
                                                                rmse = m$get_field('valid_stats')$rmse,
                                                                n_samples = length(x$value[!is.na(x$value)]), 
                                                                m$params))
                    }
                }
            }
            save(models, rmses, file = rdata_fpath)
        }
    }
    save(models, rmses, file = rdata_fpath)
}







#' merge_RData
#' 
#' Merge RData files into a single file.
merge_RData <- function(rdata_dpath) {
    model_types <- NULL
    n_mins <- NULL
    
    # check RData files
    rdata_fpaths <- list.files(rdata_dpath, full.names = T)
    for (rdata_fpath in rdata_fpaths) {
        if(nchar(basename(rdata_fpath)) > 8 && stringr::str_sub(basename(rdata_fpath), 1, 6) == 'MODELS') {
            model_info <- unlist(stringr::str_split(basename(rdata_fpath), '\\.'))
            if (length(model_info) == 4) {
                model_types <- c(model_types, model_info[4])
                n_mins <- c(n_mins, model_info[2])
            }
        }
    }
    model_types <- unique(model_types)
    n_mins <- rev(unique(n_mins))
    
    for (n_min in n_mins) {
        cat(n_min, '\n')
        models_merged <- list()
        rmses_merged <- list()
        for (model_type in model_types) {
            load(file.path(rdata_dpath, paste0('MODELS.', n_min, '.RData.', model_type)))
            models_merged[[model_type]] <- models[[model_type]]
            rmses_merged[[model_type]] <- format_pref(rmses[[model_type]])
            rm(models, rmses)
        }
        models <- models_merged
        rmses <- rmses_merged
        save(models, rmses, file = file.path(rdata_dpath, paste0('MODELS.', n_min, '.RData')))
        rm(models, rmses, models_merged, rmses_merged)
        
        xxx
    }

}


if (!interactive()) {
    # Rscript run_modeling.R NA RAND PA ARIMA GPR RF
    
    args <- commandArgs(trailingOnly = T)
    
    # if not values less than x_min, round the values to x_min
    min_x <- ifelse(args[1] == 'NA', NA, as.numeric(args[1]))
    model_types <- args[-1]
    
    # settings
    PROJECT_DPATH <- '~/projects/jp_pdpdb'
    DB_HOST <- file.path(PROJECT_DPATH, 'data/db.sqlite3')
    MODELS_RDATA <- file.path(PROJECT_DPATH, paste0('data/MODELS.', min_x,'.RData'))
    
    con <- RSQLite::dbConnect(RSQLite::SQLite(), DB_HOST, synchronous = 'off')
    DR <- select_damageData(con)
    WD <- select_weatherData(con)

    for (model_type in model_types) {
        fit(MODELS_RDATA, model_type, min_x)
    }
    
    merge_RData(dirname(MODELS_RDATA))
}


