library(rlang)
library(lubridate)
library(forcats)
library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(purrr)
library(stringi)
library(patchwork)
library(RSQLite)
library(R6)
library(forecast)
library(kernlab)
library(imputeTS)
library(randomForest)
library(fitdistrplus)


MONTH_CODE = c('01', '02', '03', '04', '05', '06',
               '07', '08', '09', '10', '11', '12')

PREFECTURE_CODE = c('北海道','青森県', '岩手県', '宮城県', '秋田県', '山形県',
                    '福島県', '茨城県', '栃木県', '群馬県', '埼玉県', '千葉県',
                    '東京都', '神奈川県', '新潟県', '富山県', '石川県',
                    '福井県', '山梨県', '長野県', '岐阜県', '静岡県', '愛知県',
                    '三重県', '滋賀県', '京都府', '大阪府', '兵庫県',
                    '奈良県', '和歌山県', '鳥取県', '島根県', '岡山県',
                    '広島県', '山口県', '徳島県', '香川県', '愛媛県', '高知県',
                    '福岡県', '佐賀県', '長崎県', '熊本県', '大分県', '宮崎県',
                    '鹿児島県', '沖縄県')



format_pref <- function(x) {
    jacode <- c('北海道','青森県', '岩手県', '宮城県', '秋田県', '山形県',
                '福島県', '茨城県', '栃木県', '群馬県', '埼玉県', '千葉県',
                '東京都', '神奈川県', '新潟県', '富山県', '石川県',
                '福井県', '山梨県', '長野県', '岐阜県', '静岡県', '愛知県',
                '三重県', '滋賀県', '京都府', '大阪府', '兵庫県',
                '奈良県', '和歌山県', '鳥取県', '島根県', '岡山県',
                '広島県', '山口県', '徳島県', '香川県', '愛媛県', '高知県',
                '福岡県', '佐賀県', '長崎県', '熊本県', '大分県', '宮崎県',
                '鹿児島県', '沖縄県')
    egcode <- c('HOKKAIDO','AOMORI', 'IWATE', 'MIYAGI', 'AKITA', 'YAMAGATA',
                'FUKUSHIMA', 'IBARAKI', 'TOCHIGI', 'GUNMA', 'SAITAMA', 'CHIBA',
                'TOKYO', 'KANAGAWA', 'NIIGATA', 'TOYAMA', 'ISHIKAWA',
                'FUKUI', 'YAMANASHI', 'NAGANO', 'GIFU', 'SHIZUOKA', 'AICHI',
                'MIE', 'SHIGA', 'KYOTO', 'OSAKA', 'HYOGO',
                'NARA', 'WAKAYAMA', 'TOTTORI', 'SHIMANE', 'OKAYAMA',
                'HIROSHIMA', 'YAMAGUCHI', 'TOKUSHIMA', 'KAGAWA', 'EHIME', 'KOCHI',
                'FUKUOKA', 'SAGA', 'NAGASAKI', 'KUMAMOTO', 'OITA', 'MIYAZAKI',
                'KAGOSHIMA', 'OKINAWA')
    for (i in 1:length(jacode)) {
        x$prefecture <- gsub(jacode[i], egcode[i], x$prefecture)
    }
    x
}


#' select_weatherData
#' 
#' Load weather data from SQLite database.
select_weatherData <- function(con) {
    con |>
        RSQLite::dbGetQuery(paste('SELECT * FROM weather',
                    'INNER JOIN observatories ON weather.observatory = observatories.name;')) |>
        dplyr::select(- observatory, - name) |>
        dplyr::group_by(prefecture, date) |>
        dplyr::summarise_all(mean, na.rm = TRUE) |>
        dplyr::ungroup() |>
        dplyr::filter_all(all_vars(!is.na(.)))
}

#' df2arr
#'
#' Convert data.frame selected from SQLite to 4-dims array
df2arr <- function(x, con) {
    d1 <- con |>
            RSQLite::dbGetQuery('SELECT name FROM crops ORDER BY name;') |>
            unlist(use.names = FALSE)
    d2 <- con |>
            RSQLite::dbGetQuery('SELECT name FROM damages ORDER BY name;') |>
            unlist(use.names = FALSE)
    .d3_from <- con |>
            RSQLite::dbGetQuery('SELECT date FROM data ORDER BY date LIMIT 1;') |>
            unlist(use.names = FALSE)
    .d3_to <- con |>
            RSQLite::dbGetQuery('SELECT date FROM data ORDER BY date DESC LIMIT 1;') |>
            unlist(use.names = FALSE)
    d3 <- as.character(seq(as.Date(.d3_from), as.Date(.d3_to), by = 'month'))
    d4 <- con |>
            RSQLite::dbGetQuery('SELECT name FROM prefectures ORDER BY jis_code;') |>
            unlist(use.names = FALSE)
 
    z <- array(NA, dim = c(length(d1), length(d2), length(d3), length(d4)),
               dimnames = list(d1, d2, d3, d4))
    for (i in d1) {
        for (j in d2) {
            .z <- array(NA, dim = c(length(d3), length(d4)), dimnames = list(d3, d4))
            .x <- x |> dplyr::filter(crop == UQ(i), damage == UQ(j))
            if (nrow(.x) > 0) {
                for (k in 1:nrow(.x)) {
                    .z[.x$date[k], .x$prefecture[k]] <- .x$value[k]
                }
            }
            z[i, j, , ] <- .z
        }
    }
    z
}


#' select_damageData
#' 
#' Load damage rate data from SQLite database
#' and convert the data to 4-dims array using the \code{\link{df2arr}} function.
select_damageData <- function(con, format = 'array') {
    x <- con |>
            RSQLite::dbGetQuery('SELECT * FROM data WHERE source == 2;') |>
            dplyr::select(- source)
    if (format == 'array') {
        x <- df2arr(x, con)
    }
    x
}


#' arrange_dataset
#'
#' Bind damage rate and weather datasets into one data.frame for modeling.
arrange_dataset <- function(dr, wd, crop = NULL, damage = NULL, prefecture = NULL) {
    dr <- dr[crop, damage, , prefecture] |>
        dplyr::as_tibble(rownames = 'date')
    wd <- wd |>
        dplyr::filter(prefecture == UQ(prefecture)) |>
        dplyr::select(- prefecture)
    dplyr::left_join(dr, wd, by = 'date') |>
        arrange(date)
}


#' calc_rmse
#' 
#' Calculate normalized-RMSE with true and predicted values.
#' 
#' @param y_true A vector of true values.
#' @param y_pred A vector of predicted values.
#' @param n_samples An integer to specify the minimum samples for calculation.
#'                  The number of samples less than `n_samples` will give `NA`.
#' @param nrom Normalize RMSE with s.d.
#' @return A float value of RMSE
calc_rmse <- function(y_true, y_pred, n_samples = 1, norm = TRUE) {
    is_na <- (is.na(y_true) | is.na(y_pred))
    y_true <- y_true[!is_na]
    y_pred <- y_pred[!is_na]
    rmse <- NA
    if (length(y_true) >= n_samples) {
        rmse <- sqrt(sum(y_true - y_pred)^2 / length(y_true))
        if (norm && !is.infinite(sd(y_true))) rmse <- rmse / sd(y_true)
    }
    if (is.infinite(rmse)) rmse <- NA
    rmse
}


#' calc_rmse_ts
#' 
#' Calculate RMSE for each year by repeatly calling the \code{\link{calc_rmse}} function.
#' 
#' @param y_true A vector of true values.
#' @param y_pred A vector of predicted values.
#' @param x A vector of dates.
#' @param n_samples An integer to specify the minimum samples for calculation.
#'                  The number of samples less than `n_samples` will give `NA`.
#' @param nrom Normalize RMSE with s.d.
#' @return A named vector of RMSE for each year.
calc_rmse_ts <- function(y_true, y_pred, x, n_samples = 1, norm = TRUE) {
    xy <- data.frame(date = x, true = y_true, pred = y_pred) |>
            dplyr::mutate(year = stringr::str_sub(date, 1, 4))
    rmse <- rep(NA, length = length(unique(xy$year)))
    names(rmse) <- sort(unique(xy$year))
    for (yr in names(rmse)) {
        xy_subset <- xy[xy$year == yr, c('true', 'pred')]
        rmse[yr] <- calc_rmse(xy_subset$true, xy_subset$pred, n_samples, norm)
    }
    rmse
}


#' impute_ts
#' 
#' Impute missing values using the averaged values for each month.
#'
#' @param x date in %Y-%m-%d format
#' @param y value
#' @return a vector after missing value imputation
impute_ts <- function(x, y) {
    if (length(x) != length(y)) {
        stop('The length of x and y must be the same.')
    }
    # omit NAs in the head of vector (unable to impute)
    left_na <- (cumsum(is.na(y)) == seq_along(y))
    
    ave_vals_df <- data.frame(date = x, value = y) |>
        dplyr::mutate(month = stringr::str_sub(date, 6, 7)) |>
        dplyr::group_by(month) |>
        dplyr::summarise(mean = mean(value, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::select(month, mean)
    ave_vals <- ave_vals_df$mean
    names(ave_vals) <- ave_vals_df$month
    
    y[is.na(y)] <- ave_vals[stringr::str_sub(x[is.na(y)], 6, 7)]
    y[left_na] <- NA
    y
}


#' calc_monavg
#' 
#' Calculate monthly average from given values.
#' 
#' @param y A vector of values for average calculation.
#' @param d A vector of dates for grouping values for average calculation.
#' @param n An integer to specify how many years to use for calculation.
#'          Set `Inf` to use the all past data.
#' @param b A integer of year of baseline. If `NULL`, the baseline is the last year.
#'          Average calculation does not include the baseline year.
#' @return a data.frame containing 4 columns: year, month, monthly averages,
#'          and monthly s.d and 12 rows of each month.
#'          Values in the column year are NA.
calc_monavg <- function(y, d, n = Inf, b = NULL) {
    yd <- data.frame(date  = as.character(d),
                     year  = as.integer(stringr::str_sub(as.character(d), 1, 4)),
                     month = stringr::str_sub(as.character(d), 6, 7),
                     value = y) |>
              dplyr::arrange(date)
    .calc_monavg_(yd, n, b)
}
.calc_monavg_ <- function(yd, n, b) {
    if (is.null(b)) {
        warnings('The baseline year is not specified. The last year is used as the baseline.')
        b <- max(as.integer(stringr::str_sub(yd$date, 1, 4)))
    }
    yr_to <- as.Date(paste(b, '01', '01', sep = '-'))
    yr_from <- ifelse(is.infinite(n),
                      as.Date('1800-12-31'),
                      yr_to %m-% years(n))
    yd |>
        dplyr::mutate(date = as.Date(date)) |>
        dplyr::filter(UQ(yr_from) <= date & date < UQ(yr_to)) |>
        dplyr::select(month, value) |>
        dplyr::bind_rows(data.frame(month = MONTH_CODE, value = NA)) |> # let output contain all months
        dplyr::group_by(month) |>
        dplyr::summarise(mean = mean(value, na.rm = TRUE),
                         sd   = sd(value, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::mutate(year = NA,
                      mean = replace(mean, which(is.nan(mean)), NA),
                      sd = replace(sd, which(is.nan(sd)), NA)) |>
        dplyr::select(year, month, mean, sd) |>
        dplyr::arrange(month) |>
        as.data.frame()
}


#' calc_monavg_ts
#' 
#' Calculate monthly average from given values for each year.
#' 
#' @param y A vector of values for average calculation.
#' @param d A vector of dates for grouping values for average calculation.
#' @param n An integer to specify how many years to use for calculation.
#'          Set `Inf` to use the all past data.
#' @return a data.frame containing 4 columns: year, month, monthly averages,
#'          and monthly s.d and 12 rows of each month.
calc_monavg_ts <- function(y, d, n = Inf) {
    yd <- data.frame(date  = as.character(d),
                     year  = as.integer(stringr::str_sub(as.character(d), 1, 4)),
                     month = stringr::str_sub(as.character(d), 6, 7),
                     value = y) |>
        dplyr::arrange(date)
    # calculate averages from past n years for each year
    mo_avg <- data.frame()
    for (yr in sort(as.integer(unique(yd$year)))) {
        mo_avg_ <- .calc_monavg_(yd, n, yr + 1) |>
            dplyr::mutate(year = UQ(yr + 1))
        mo_avg <- rbind(mo_avg, mo_avg_)
    }
    mo_avg
}




#' calc_diff_months
#' 
#' Calculate the difference in months between two dates.
#' 
#' @param m_from A character of date in %Y-%m-%d format.
#' @param m_to A character of date in %Y-%m-%d format.
calc_diff_months <- function(m_from, m_to) {
    abs_n_months <- function(x) {
        lt <- as.POSIXlt(as.Date(x, origin='1970-01-01'))
        lt$year * 12 + lt$mon
    }
    abs_n_months(m_to) - abs_n_months(m_from)
}



#' CDF_BASE
#' 
#' Base class to build Crop Disease Forecast model.
CDF_BASE <- R6Class('CDF_BASE',
    private = list(
        # input data
        inputs_ = NULL,
        valid_month_  = NULL,
        
        # model
        model_ = NULL,  # the model object
        valid_stats_ = list(rmse_ts = NA,    # validation RMSEs of each year 
                            rmse = NA,       # minimum validation RMSEs 
                            valid_from = NA,
                            data = NULL),
        min_timepoints_ = 5,
        
        # settings
        month_code_ = MONTH_CODE,
        prefecture_code_ = PREFECTURE_CODE,
        
        
        #' check_valid_months_
        #' 
        #' Check the months with only NAs and returns TRUE/FALSE to represent the
        #' month has valid data or NA.
        #' 
        #' @param y A vector storing damage data.
        #' @param d A vector storing date data in `YYYY-MM-DD` format.
        #' @param n_min Threshold for removing months with small sample sizes.
        set_valid_months_ = function(y, d, n_min = 3) {
            private$valid_month_ <- private$month_code_ %in% (
                data.frame(value = y, date = d) |>
                dplyr::mutate(month = str_sub(date, 6, 7)) |>
                dplyr::group_by(month) |>
                dplyr::summarise(n_nonna = sum(!is.na(value)), n_months = n()) |>
                dplyr::filter(n_nonna > UQ(n_min)) |>
                dplyr::select(month) |>
                unlist(use.names = FALSE))
        },
        
        
        #' reshape_fit_data_
        #' 
        #' Merge explanatory variables (weather data), response variables, and 
        #' date variables to prepare fitting.
        #' 
        #' @param y Response variable for fitting.
        #' @param d A vector of date.
        #' @param x a matrix of variables (e.g., weather data) for fitting.
        #' @param n_min Threshold for removing months with small sample sizes.
        #' @param imupte_na Impute NAs if TRUE.
        #' @param past_n_years Fill NAs with mean calculated from the past n years.
        #' @return a data.frame containing explanatory and response variables
        #'         using for fitting.
        reshape_fit_data_ = function(y, d, x = NULL, n_min = 3, impute_na = TRUE, past_n_years = 1) {
            # get valid time points for fitting
            is_valid_data <- data.frame(value = y, date = d) |>
                dplyr::mutate(year = stringr::str_sub(date, 1, 4),
                              month = stringr::str_sub(date, 6, 7)) |>
                dplyr::mutate(f_ = as.logical(month %in% private$month_code_[private$valid_month_])) |>
                dplyr::select(f_) |>
                unlist(use.names = FALSE)
            
            # impute NAs
            if (impute_na) {
                y_adj <- impute_ts(d[is_valid_data], y[is_valid_data])
            } else {
                y_adj <- y[is_valid_data]
            }
           
            # remove NAs in the head/tail of vector
            is_NA_left <- (cumsum(is.na(y_adj)) == seq_along(y_adj))
            is_NA_right <- (rev(cumsum(rev(is.na(y_adj))) == seq_along(y_adj)))
            y_adj <- y_adj[!is_NA_left & !is_NA_right]
            is_valid_data[is_valid_data] <- is_valid_data[is_valid_data] & (!is_NA_left & !is_NA_right)
            
            # return
            list(inputs = list(x = x, y = y, d = d),
                 x = as.data.frame(x[is_valid_data, ]),
                 y = y[is_valid_data],
                 d = d[is_valid_data],
                 y_adj = y_adj,
                 is_valid_data = is_valid_data)
        },                               

        
        #' check_leakage_
        #' 
        #' Check if the prediction date is earlier than the last date of the training data.
        #' 
        #' @param d A vector of date.
        #' @param d_train A vector of date for training data.
        #' @param force_stop Stop the process if TRUE when leakage.
        check_leakage_ = function(d, d_train, force_stop = TRUE) {
            is_leakage <- min(as.Date(d)) < max(as.Date(d_train))
            if (is_leakage) {
                warning('The prediction date is earlier than the last date of the training data.')
                if (force_stop) {
                    stop('The process is stopped due to data leakage.')
                }
            }
            is_leakage
        },
       
        
        #' fill_dates_
        #' 
        #' Make dates continuous for prediction.
        #' 
        #' @param d A vector of date.
        fill_dates_ = function(d) {
            d <- as.Date(d)
            seq(min(d), max(d), by = 'month')
        },
        
        
        #' calc_forecast_timepoints_
        #'
        #' Calculate the number of points for prediciton.
        #' 
        #' The input data `d` may far from training data `d_train`. In this case,
        #' some models such as ARIMA need to predict the values between `d` and `d_train`,
        #' This function calculate the difference between `d` and `d_train` by
        #' considering the valid months.
        #' 
        #' @param d A vector of date for prediction.
        #' @param d_train A vector of date for training data.
        #' @param valid_months A vector of characters of valid months.
        calc_forecast_timepoints_ = function(d, d_train, valid_months) {
            min_d <- min(as.Date(d))
            max_d_train <- max(as.Date(d_train))
            d_mid <- NULL
            if (calc_diff_months(max_d_train, min_d) > 1) {
                d_mid <- seq(from = max_d_train %m+% months(1), to = min_d %m-% months(1), by = 'month')
            }
            d4pred <- c(as.character(d_mid), as.character(d))
            d4pred[stringr::str_sub(d4pred, 6, 7) %in% valid_months]
        },
        
        
        
        #' Fitting with time-series data
        #' 
        fit_ts_ = function(y, d, x = NULL, n_min = 3, impute_na = TRUE, valid_from = -Inf, ...) {
            private$set_valid_months_(y, d, n_min)
            if (sum(private$valid_month_) < 1) {
                warning('Unable to fit data due to too small samples.')
                return(invisible(NULL))
            }
            
            # reshape data for fitting
            xyd <- private$reshape_fit_data_(y, d, x, n_min, impute_na)
            
            # fitting for each year to calculate RMSE; skip if valid_from is unset
            if (!(is.null(valid_from) || is.na(valid_from))){
                xyd_pred <- list(y = NULL, y_pred = NULL, d = NULL)
                rmse_ <- rmse_ts_ <- NULL
                
                d_yr <- as.integer(str_sub(xyd$d, 1, 4))
                for (yr in sort(unique(d_yr))) {
                    x_ <-     xyd$x[d_yr < yr, ]
                    y_ <-     xyd$y[d_yr < yr]
                    y_adj_ <- xyd$y_adj[d_yr < yr]  # same to y_ if impute_na is FALSE
                    d_ <-     xyd$d[d_yr < yr]
                    
                    if (length(y_adj_) > private$min_timepoints_ && sum(is.na(y_adj_)) < private$min_timepoints_) {
                        # fitting with data before `d_yr`
                        private$fit_(y = y_adj_, d = d_, x = x_, ...)
                        
                        # predict values for the year `d_yr`
                        d_new_ <- as.character(as.Date(paste(yr, '-01-01', sep = '')) %m+% months(0:11))[private$valid_month_]
                        x_new_ <- x[d %in% d_new_, ]
                        y_new_ <- private$predict_(d = d_new_, x = x_new_)
                        
                        # record predictions and true values
                        if (!is.null(y_new_)) {
                            xyd_pred$y <- c(xyd_pred$y, xyd$y[xyd$d %in% d_new_]) # calculating of RMSEs requreis true values
                            xyd_pred$y_pred <- c(xyd_pred$y_pred, y_new_$fit)
                            xyd_pred$d <- c(xyd_pred$d, d_new_)
                        }
                    }
                }
                
                if (length(xyd_pred$y_pred) > 1) {
                    rmse_ts_ <- calc_rmse_ts(xyd_pred$y, xyd_pred$y_pred, xyd_pred$d)
                    valid_from_idx <- ifelse(is.infinite(valid_from),
                                         1, which(rownames(rmse_ts_) == as.character(valid_from)))
                    rmse_ <- mean(rmse_ts_[valid_from_idx:length(rmse_ts_)], na.rm = TRUE)
                }
                private$valid_stats_ <- list(
                    rmse_ts    = rmse_ts_,
                    rmse       = rmse_,
                    valid_from = valid_from,
                    data       = xyd_pred
                )
            }
            
            # fit with all samples
            if (impute_na) {
                private$fit_(y = xyd$y_adj, d = xyd$d, x = xyd$x, ...)
            } else {
                private$fit_(y = xyd$y, d = xyd$d, x = xyd$x, ...)
            }
        },
        
        #' format_pred_outputs_
        #' 
        #' Format the output of prediction.
        format_pred_outputs_ = function(d, forecast_info, y_fit, y_lwr, y_upr) {
            y_pred <- data.frame(fit = rep(NA, length(d)), lwr = NA, upr = NA)
            y_pred$fit <- y_fit[forecast_info %in% d]
            y_pred$lwr <- y_lwr[forecast_info %in% d]
            y_pred$upr <- y_upr[forecast_info %in% d]
            y_pred
        },
        
        #' Fitting with all input data
        fit_ = function() {
            stop('Method Not Implemented.')
        },
        
        predict_ = function() {
            stop('Method Not Implemented.')
        }
        
    ),
    
    
    
    public = list(
        params = NULL,
        
        #' get_field
        #' 
        #' @param x A character of variable name in this class without '_' at the end.
        #' @return an object.
        get_field = function(x) {
            if (!is.null(private[[x]])) {
                private[[x]]
            } else {
                private[[paste0(x, '_')]]
            }
        },
        
        fit_ts = function(...) {
            private$fit_ts_(...)
        },
        
        fit = function(...) {
            if (sum(is.na(y)) < private$min_timepoints_) {
                private$fit_(...)
            }
        },
        
        predict = function(...) {
            private$predict_(...)
        }
    )
)


#' CDF_RAND
#'
#' Generate random number with the given mean and s.d as a prediction.
#' The mean and s.d. is calculated from the given training samples among all terms.
CDF_RAND <- R6Class('CDF_RAND',
    inherit = CDF_BASE,
    
    private = list(
        random_seed_ = NULL,
        
        fit_ = function(y, d, ...) {
            # store inputs for prediction
            private$inputs_ <- list(y = y, d = d)
            
            self$params <- list(
                mean = ifelse(is.nan(mean(y, na.rm = TRUE)), NA, mean(y, na.rm = TRUE)),
                sd = ifelse(is.nan(sd(y, na.rm = TRUE)), NA, sd(y, na.rm = TRUE))
            )
            private$model_ <- self$params
        },
        
        predict_ = function(d, ...) {
            private$check_leakage_(d, private$inputs_$d)
            
            set.seed(private$random_seed_)
            private$random_seed_ <- private$random_seed_ + 1
            y <- data.frame(fit = rep(NA, length(d)), lwr = NA, uwr = NA)
            if (!(is.na(self$params$mean) || is.na(self$params$sd))) {
                y$fit <- rnorm(length(d), mean = self$params$mean, sd = self$params$sd)
            }
            y
        }
    ),
    
    public = list(
        initialize = function(random_seed = 0) {
            private$random_seed_ <- random_seed
            self$params <- list(mean = NULL, sd = NULL)
        }
    )
)




#' CDF_LOGRAND
#'
#' Generate random number with the given mean and s.d as a prediction in log-norma dist.
#' The mean and s.d. is calculated from the given training samples among all terms.
CDF_LOGRAND <- R6Class('CDF_LOGRAND',
    inherit = CDF_BASE,
                    
    private = list(
        random_seed_ = NULL,
        
        fit_ = function(y, d, ...) {
            # store inputs for prediction
            private$inputs_ <- list(y = y, d = d)
            
            self$params <- list(
                mean = ifelse(is.nan(mean(log(y), na.rm = TRUE)) || is.infinite(mean(log(y), na.rm = TRUE)),
                              NA, mean(log(y), na.rm = TRUE)),
                sd = ifelse(is.nan(sd(log(y), na.rm = TRUE)) || is.infinite(sd(log(y), na.rm = TRUE)),
                            NA, sd(log(y), na.rm = TRUE))
            )
            private$model_ <- self$params
        },
        
        predict_ = function(d, ...) {
            private$check_leakage_(d, private$inputs_$d)
            
            set.seed(private$random_seed_)
            private$random_seed_ <- private$random_seed_ + 1
            y <- data.frame(fit = rep(NA, length(d)), lwr = NA, uwr = NA)
            if (!(is.na(self$params$mean) || is.na(self$params$sd))) {
                y$fit <- exp(rnorm(length(d), mean = self$params$mean, sd = self$params$sd))
            }
            y
        }
    ),
                    
    public = list(
        initialize = function(random_seed = 0) {
            private$random_seed_ <- random_seed
            self$params <- list(mean = NULL, sd = NULL)
        }
    )
)



#' CDF_PA
#'
#' Caculate mean and s.d. as a prediction from the past n years.
CDF_PAX <- R6Class('CDF_PAX',
    inherit = CDF_BASE,
    
    private = list(
        fit_ = function(y, d, n = c(1:10, Inf), ...) {
            # store inputs for prediction
            private$inputs_ <- list(y = y, d = d)
            
            # placeholder to find best hyper-param `n`
            rmse <- matrix(NA, nrow = length(unique(stringr::str_sub(d, 1, 4))), ncol = length(n))
            colnames(rmse) <- paste0('n=', as.character(n))
            rownames(rmse) <- sort(unique(stringr::str_sub(d, 1, 4)))
            
            # find best n with the given samples
            for (i in n) {
                df <- dplyr::inner_join(
                    (data.frame(value = y, date = d) |>
                        dplyr::mutate(yrmon_ = stringr::str_sub(date, 1, 7))),
                    (calc_monavg_ts(y, d, n = i) |>
                         dplyr::mutate(yrmon_ = paste0(year, '-', month))),
                    by = 'yrmon_')
                rmse_ <- calc_rmse_ts(df$value, df$mean, df$date)
                rmse[names(rmse_), paste0('n=', as.character(i))] <- unname(rmse_)
            }
            rmse_mean <- apply(rmse, 2, function(x) mean(x, na.rm = TRUE))
            self$params <- list(n = ifelse(all(is.na(rmse_mean)), NA, n[which.min(rmse_mean)]))
            
            # fit with the best n
            private$model_ <- calc_monavg(y, d, n = self$params$n)
        },
        
        predict_ = function(d, level = 0.95, ...) {
            private$check_leakage_(d, private$inputs_$d)
            y <- data.frame(fit = rep(NA, length(d)), lwr = NA, uwr = NA)
            if (!is.na(self$params$n)) {
                y_pred <- rep(NA, length(d))
                idx4pred <- match(stringr::str_sub(d, 6, 7), private$model_$month)
                # prediction
                level_prob <- qnorm(level + (1 - level) / 2, 0, 1)
                y$fit <- private$model_$mean[idx4pred]
                y$lwr <- private$model_$mean[idx4pred] - level_prob * private$model_$sd[idx4pred]
                y$uwr <- private$model_$mean[idx4pred] + level_prob * private$model_$sd[idx4pred]
            }
            y
        }
    ),
    
    public = list(
        initialize = function() {
            self$params <- list(n = NULL)
        }
    )
)



#' CDF_PA
#'
#' Caculate mean and s.d. as a prediction from the past n years.
CDF_PA <- R6Class('CDF_PA',
    inherit = CDF_BASE,
    
    private = list(
        fit_ = function(y, d, n = c(1:5), ...) {
            # store inputs for prediction
            private$inputs_ <- list(y = y, d = d)
            
            # placeholder to find best hyper-param `n`
            rmse <- matrix(NA, nrow = length(unique(stringr::str_sub(d, 1, 4))), ncol = length(n))
            colnames(rmse) <- paste0('n=', as.character(n))
            rownames(rmse) <- sort(unique(stringr::str_sub(d, 1, 4)))
            
            # find best n with the given samples
            for (i in n) {
                df <- dplyr::inner_join(
                    (data.frame(value = y, date = d) |>
                         dplyr::mutate(yrmon_ = stringr::str_sub(date, 1, 7))),
                    (calc_monavg_ts(y, d, n = i) |>
                         dplyr::mutate(yrmon_ = paste0(year, '-', month))),
                    by = 'yrmon_')
                rmse_ <- calc_rmse_ts(df$value, df$mean, df$date)
                rmse[names(rmse_), paste0('n=', as.character(i))] <- unname(rmse_)
            }
            rmse_mean <- apply(rmse, 2, function(x) mean(x, na.rm = TRUE))
            self$params <- list(n = ifelse(all(is.na(rmse_mean)), NA, n[which.min(rmse_mean)]))
            
            # fit with the best n
            private$model_ <- calc_monavg(y, d, n = self$params$n)
        },
        
        predict_ = function(d, level = 0.95, ...) {
            private$check_leakage_(d, private$inputs_$d)
            y <- data.frame(fit = rep(NA, length(d)), lwr = NA, uwr = NA)
            if (!is.na(self$params$n)) {
                y_pred <- rep(NA, length(d))
                idx4pred <- match(stringr::str_sub(d, 6, 7), private$model_$month)
                # prediction
                level_prob <- qnorm(level + (1 - level) / 2, 0, 1)
                y$fit <- private$model_$mean[idx4pred]
                y$lwr <- private$model_$mean[idx4pred] - level_prob * private$model_$sd[idx4pred]
                y$uwr <- private$model_$mean[idx4pred] + level_prob * private$model_$sd[idx4pred]
            }
            y
        }
    ),      
    
    public = list(
        initialize = function() {
            self$params <- list(n = NULL)
        }
    )
)


CDF_ARIMA <- R6Class('CDF_ARIMA',
    inherit = CDF_BASE,
    
    private = list(
        fit_ = function(y, d, ...) {
            private$inputs_ <- list(y = y, d = d)
            private$model_ <- auto.arima(y,
                                         ic = 'aic', stepwise = TRUE, approximation = FALSE,
                                         start.p = 1, start.q = 1, max.p = 5, max.q = 5, max.order = 10)
            self$params <- split(unname(arimaorder(private$model_)),
                                 names(arimaorder(private$model_)))
        },
        
        predict_ = function(d, level = 0.95, ...) {
            private$check_leakage_(d, private$inputs_$d)
            d_fill <- private$fill_dates_(d)
            # prediction
            forecast_info <- private$calc_forecast_timepoints_(d_fill, private$inputs_$d, private$month_code_[private$valid_month_])
            y_ <- forecast::forecast(private$model_, h = length(forecast_info), level = level)
        
            private$format_pred_outputs_(d, forecast_info,
                as.numeric(y_$mean), as.numeric(y_$lower), as.numeric(y_$upper))
        }
        
    ),

    public = list(
        initialize = function() {
            self$params  <- list()
        }
    )
)



CDF_SARIMA <- R6Class('CDF_SARIMA',
    inherit = CDF_BASE,
    
    private = list(
        fit_ = function(y, d, ...) {
            y <- ts(y, frequency = sum(private$valid_month_))
            private$inputs_ <- list(y = y, d = d)
            private$model_ <- auto.arima(y,
                                         ic = 'aic', stepwise = TRUE, approximation = FALSE, seasonal = TRUE,
                                         start.p = 1, start.q = 1, max.p = 5, max.q = 5, max.order = 10)
            self$params <- split(unname(arimaorder(private$model_)),
                                 names(arimaorder(private$model_)))
        },
        
        predict_ = function(d, level = 0.95, ...) {
            private$check_leakage_(d, private$inputs_$d)
            d_fill <- private$fill_dates_(d)
            # prediction
            forecast_info <- private$calc_forecast_timepoints_(d_fill, private$inputs_$d, private$month_code_[private$valid_month_])
            y_ <- forecast::forecast(private$model_, h = length(forecast_info), level = level)
            
            private$format_pred_outputs_(d, forecast_info,
                                         as.numeric(y_$mean), as.numeric(y_$lower), as.numeric(y_$upper))
        }
        
    ),
                     
    public = list(
        initialize = function() {
            self$params  <- list()
        }
    )
)




CDF_SARIMAX <- R6Class('CDF_SARIMAX',
    inherit = CDF_BASE,
    
    private = list(
        fit_ = function(y, d, x, ...) {
            y <- ts(y, frequency = sum(private$valid_month_))
            private$inputs_ <- list(y = y, d = d, x = x)
            tryCatch({
                private$model_ <- auto.arima(y,
                           ic = 'aic', stepwise = TRUE, approximation = FALSE, seasonal = TRUE,
                           start.p = 1, start.q = 1, max.p = 5, max.q = 5, max.order = 10,
                           xreg = as.matrix(x))
                self$params <- split(unname(arimaorder(private$model_)),
                                     names(arimaorder(private$model_)))
            }, error = function(e) {
                private$model_ <- NULL
            })
            
        },
        
        predict_ = function(d, x, level = 0.95, ...) {
            private$check_leakage_(d, private$inputs_$d)
            d_fill <- private$fill_dates_(d)

            # prediction
            forecast_info <- private$calc_forecast_timepoints_(d_fill, private$inputs_$d, private$month_code_[private$valid_month_])
            x <- x[forecast_info %in% d_fill, ]
            if (length(forecast_info) != nrow(x)) {
                browser()
                stop('The number of rows in x must be the same as the length of d.')
            }
            
            pred_outputs <- NULL
            if (!is.null(private$model_)) {
                y_ <- forecast::forecast(private$model_, h = length(forecast_info), xreg = as.matrix(x), level = level)
                pred_outputs <- private$format_pred_outputs_(d, forecast_info,
                                             as.numeric(y_$mean), as.numeric(y_$lower), as.numeric(y_$upper))
            }
            pred_outputs
        }
        
    ),
                      
    public = list(
        initialize = function() {
            self$params  <- list()
        }
    )
)


CDF_GPR <- R6Class('CDF_GPR',
    inherit = CDF_BASE,
                  
    private = list(
        fit_ = function(y, d, ...) {
            private$inputs_ <- list(y = y, d = d)
            capture.output(
                private$model_ <- kernlab::gausspr(seq_along(y), y,
                                                   type = 'regression', scaled = TRUE,
                                                   kernel = 'rbfdot', kpar = 'automatic',
                                                   variance.model = TRUE)
            )
            self$params <- private$model_@kernelf@kpar
        },
        
        predict_ = function(d, level = 0.95, ...) {
            private$check_leakage_(d, private$inputs_$d)
            d_fill <- private$fill_dates_(d)
            # prediction
            forecast_info <- private$calc_forecast_timepoints_(d_fill, private$inputs_$d, private$month_code_[private$valid_month_])
            d_new <- length(private$inputs_$d) + 1:length(forecast_info)
            level_prob <- qnorm(level + (1 - level) / 2, 0, 1)
            ym_ <- kernlab::predict(private$model_, d_new, type = 'response')[, 1]
            ys_ <- kernlab::predict(private$model_, d_new, type = 'sdeviation')
            # format
            private$format_pred_outputs_(d, forecast_info,
                ym_, ym_ - level_prob * ys_, ym_ + level_prob * ys_)
        }
    ),
    
    public = list(
        initialize = function() {
            self$params <- list()
        }
    )
)




CDF_RF <- R6Class('CDF_RF',
    inherit = CDF_BASE,
                  
    private = list(
        fit_ = function(y, d, x, ...) {
            private$inputs_ <- list(y = y, d = d, x = x)
            private$model_ <- randomForest::randomForest(x = x, y = y, type = 'regression')
            self$params <- as.list(private$model_$importance)
            names(self$params) <- rownames(private$model_$importance)
        },
        
        predict_ = function(d, x, ...) {
            private$check_leakage_(d, private$inputs_$d)
            data.frame(fit = predict(private$model_, newdata = x), lwr = NA, uwr = NA)
        }
        
    ),
                  
    public = list(
        initialize = function() {
            self$params <- list()
        }
    )
)


CDF_LSTM <- R6Class('CDF_LSTM',
    inherit = CDF_BASE,
    
    private = list(
        model_ = NULL,
        
        fit_ = function(y, d, ...) {
            1
        },
        
        predict_ = function(d, ...) {
            1
        },
        
        init_model_ = function(n_units, windowsize, batchsize) {
            m <- keras::keras_model_sequential()
            m %>%
                kras::layer_lstm(units = n_units,
                           input_shape = c(windowsize, 2),
                           batch_size = batchsize,
                           return_sequences = TRUE,
                           stateful = TRUE) %>%
                keras::layer_dropout(rate = 0.5) %>%
                keras::layer_lstm(units = round(n_units / 2),
                           return_sequences=FALSE,
                           stateful=TRUE) %>%
                keras::layer_dropout(rate=0.5) %>%
                keras::layer_dense(units=1)
            m %>%
                compile(loss = 'mse',
                        optimizer = optimizer_adam(),
                        metric = list('mean_absolute_error'))
            m
        },
        
        #' fit_cv_
        #'
        #' Use the 1/3 of inputs to determine the number of units in LSTM.
        fit_cv_ = function(y, d, ...) {
            y1 <- y[1:(length(y) %/% 3 * 2)]
            y2 <- y[(length(y) %/% 3 * 2 + 1):(length(y))]
        }
        
    ),
    
    public = list(
        initialize = function() {
            self$params <- list()
        }
    )
)




if (F) {
    PROJECT_DPATH <- '~/projects/jp_pdpdb'
    DB_HOST <- file.path(PROJECT_DPATH, 'data/db.sqlite3')
    con <- RSQLite::dbConnect(RSQLite::SQLite(), DB_HOST, synchronous = 'off')
    DR <- select_damageData(con)
    WD <- select_weatherData(con)
    
    x1 <- arrange_dataset(DR, WD, crop = 'キュウリ', damage = 'うどんこ病', prefecture = '愛媛県')
    x1_new <- as.character(as.Date(max(x1$date)) %m+% months(1:12))

    x2 <- arrange_dataset(DR, WD, crop = 'イチゴ', damage = 'ハダニ類', prefecture = '長崎県')
    x2_new <- as.character(as.Date(max(x2$date)) %m+% months(1:12))   
    
    x3 <- arrange_dataset(DR, WD, crop = 'かき', damage = 'カキクダアザミウマ', prefecture = '福島県')
    x3_new <- as.character(as.Date(max(x3$date)) %m+% months(1:12))   
    
    x4 <- arrange_dataset(DR, WD, crop = 'うめ', damage = 'かいよう病', prefecture = '福井県')
    x4_new <- as.character(as.Date(max(x4$date)) %m+% months(1:12))   
    
    
    
    x <- x1
    x_new <- x1_new
 
    m1 <- CDF_RAND$new()
    m1$fit_ts(x$value, x$date)
    f1 <- m1$get_field('valid_stats')
    plot(f1$data$y_pred, f1$data$y)
    
    m2 <- CDF_PA$new()
    m2$fit_ts(x$value, x$date)
    f2 <- m2$get_field('valid_stats')
    plot(f2$data$y_pred, f2$data$y)
    
    m3 <- CDF_ARIMA$new()
    m3$fit_ts(x$value, x$date)
    f3 <- m3$get_field('valid_stats')
    plot(f3$data$y_pred, f3$data$y)
    
    m4 <- CDF_GPR$new()
    m4$fit_ts(x$value, x$date)
    f4 <- m4$get_field('valid_stats')
    plot(f4$data$y_pred, f4$data$y)
    
    m5 <- CDF_RF$new()
    m5$fit_ts(x$value, x$date, x[3:ncol(x)])
    f5 <- m5$get_field('valid_stats')
    plot(f5$data$y_pred, f5$data$y)

    
    par(mfrow = c(1, 5))
    plot(f1$data$y_pred, f1$data$y)
    plot(f2$data$y_pred, f2$data$y)
    plot(f3$data$y_pred, f3$data$y)
    plot(f4$data$y_pred, f4$data$y)
    plot(f5$data$y_pred, f5$data$y)
    
    
    
}

