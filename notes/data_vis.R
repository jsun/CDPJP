library(knitr)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(ggthemes)
library(RColorBrewer)

source('model_core.R')
if (!exists('DR')) {
    PROJECT_DPATH <- '~/projects/jp_pdpdb'
    DB_HOST <- file.path(PROJECT_DPATH, 'data/db.sqlite3')
    con <- RSQLite::dbConnect(RSQLite::SQLite(), DB_HOST, synchronous = 'off')
    DI <- format_pref(select_damageData(con, format = 'df'))
    DR <- select_damageData(con)
}
VIZ_RDATA <- file.path(PROJECT_DPATH, 'data', 'DATA_VIZ.RData')
if (file.exists(VIZ_RDATA)) {
    load(VIZ_RDATA)
}


# functions
get_cols <- function(n, pal_name = NULL, pal_n = NULL) {
    if (is.null(pal_name)) {
        colorRampPalette(pal_frontiers()(7))(n)
    } else {
        colorRampPalette(brewer.pal(pal_n, pal_name))(n)
    }
}

data_summary_func <- function(x) {
    c(y = mean(x), ymin = mean(x) - sd(x), ymax = mean(x) + sd(x))
}

calc_corr <- function(x, item = NULL) {
    cormat <- NULL
    x <- x[, item, , ]
    .x.keep <- (apply(!is.na(x), 1, sum, na.rm=TRUE) > 0)
    if (sum(.x.keep) > 1) {
        x <- x[.x.keep, , ]
        y <- matrix(NA, ncol = dim(x)[1], nrow = dim(x)[2] * dim(x)[3])
        colnames(y) <- dimnames(x)[[1]]
        combn <- subset(expand.grid(colnames(y), colnames(y)),
                        unclass(Var1) < unclass(Var2))
        combn <- data.frame(x = as.character(combn$Var1),
                            y = as.character(combn$Var2))
        for (ci in 1:nrow(combn)) {
            .d <- cbind(as.numeric(x[combn$x[ci], , ]),
                        as.numeric(x[combn$y[ci], , ]))
            .d.keep <- apply(!is.na(.d), 1, all)
            if (sum(.d.keep) > 1) {
                .d <- .d[.d.keep, ]
                cormat <- rbind(cormat,
                                data.frame(item = item,
                                           crops = paste(combn$x[ci], combn$y[ci], sep = ' x '),
                                           cor = cor(.d[, 1], .d[, 2], method = 'pearson'),
                                           n_samples = nrow(.d)))
            }
        }
    }
    cormat
}

calc_corr <- function(x, item = NULL, verbose = TRUE) {
    cormat <- NULL
    x_item <- x[, item, , ]
    .x.keep <- (apply(!is.na(x_item), 1, sum, na.rm=TRUE) > 0)
    for (pref in dimnames(x_item)[[3]]) {
        if (verbose) message(paste0('COR COEF for ', item, ' ', pref))
        if (sum(.x.keep) > 1) {
            x <- x_item[.x.keep, , pref]
            y <- matrix(NA, ncol = dim(x)[1], nrow = dim(x)[2])
            colnames(y) <- dimnames(x)[[1]]
            combn <- subset(expand.grid(colnames(y), colnames(y)),
                            unclass(Var1) < unclass(Var2))
            combn <- data.frame(x = as.character(combn$Var1),
                                y = as.character(combn$Var2))
            for (ci in 1:nrow(combn)) {
                .d <- cbind(as.numeric(x[combn$x[ci], ]),
                            as.numeric(x[combn$y[ci], ]))
                .d.keep <- apply(!is.na(.d), 1, all)
                if (sum(.d.keep) > 1) {
                    .d <- .d[.d.keep, ]
                    cormat <- rbind(cormat,
                                    data.frame(item = item,
                                               prefecture = pref,
                                               crops = paste(combn$x[ci], combn$y[ci], sep = ' x '),
                                               cor = cor(.d[, 1], .d[, 2], method = 'pearson'),
                                               n_samples = nrow(.d)))
                }
            }
        }
            
    }
    cormat
}

calc_ANOVA <- function(x, verbose = TRUE) {
    pmat <- NULL
    
    n_cases <- prod(sapply(dimnames(DR), length)[c(1, 2, 4)])
    tries_i <- 0
    
    for (crop in dimnames(x)[[1]]) {
        for (cdp in dimnames(x)[[2]]) {
            for (loc in dimnames(x)[[4]]) {
                tries_i <- tries_i + 1
                if (verbose) message(paste0('ANOVA for ', crop, ' ', cdp, ' ', loc,
                        ' [', tries_i, '/', n_cases, '(', round(tries_i / n_cases, 4) * 100, '%)]'))
                
                dd <- x[crop, cdp, , loc]
                dd <- data.frame(date = names(dd), value = as.numeric(dd)) |>
                    dplyr::mutate(month = substr(date, 6, 7)) |>
                    dplyr::select(month, value) |>
                    dplyr::filter(!is.na(value))
                n_sample <- length(dd$value)
                n_month <- length(unique(dd$month))
                n_sample_month <- dd |> group_by(month) |> summarise(n = n()) |> select(n) |> pull()
                if (n_sample > 3 && n_month > 1 && min(n_sample_month) > 1) {
                    p_ <- summary(aov(value ~ month, data = dd))[[1]][["Pr(>F)"]][1]
                } else {
                    p_ <- NA
                }
                pmat <- rbind(pmat,
                              data.frame(crop = crop, cdp = cdp, loc = loc,
                                         n_sample = n_sample, n_month = n_month,
                                         p = p_)
                              )
            }
        }
    }
    
    pmat$q <- p.adjust(pmat$p, method = 'BH')
    pmat
}






# ------------------------------------------------------------------------------
# The Distribution of DI
# ------------------------------------------------------------------------------


# Composition of MAFF CDP Data
# ------------------------------------------------------------------------------

n_crops <- DI |>
    group_by(crop) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    as.data.frame()

n_damages <- DI |>
    group_by(damage) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    as.data.frame()

n_combn <- DI |>
    group_by(crop, damage) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    as.data.frame()

n_combnpref <- DI |>
    group_by(crop, damage, prefecture) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    as.data.frame()

write.table(n_crops, file = 'trends/n_crops.tsv', sep = '\t', row.names = FALSE, quote = FALSE)
write.table(n_damages, file = 'trends/n_damages.tsv', sep = '\t', row.names = FALSE, quote = FALSE)
write.table(n_combn, file = 'trends/n_cropxdamages.tsv', sep = '\t', row.names = FALSE, quote = FALSE)
n_combnpref |> format_pref() |>
write.table(file = 'trends/n_cropxdamagesxpref.tsv', sep = '\t', row.names = FALSE, quote = FALSE)


cat('--- #crops with samples more than 0, 10, 100, 1000\n')
cat(paste0(sum(n_crops$n > 0), ' (', 100 * sum(n_crops$n > 0) / length(n_crops$n), '%)', '\n'))
cat(paste0(sum(n_crops$n > 10), ' (', 100 * sum(n_crops$n > 10) / length(n_crops$n), '%)', '\n'))
cat(paste0(sum(n_crops$n > 100), ' (', 100 * sum(n_crops$n > 100) / length(n_crops$n), '%)', '\n'))
cat(paste0(sum(n_crops$n > 1000), ' (', 100 * sum(n_crops$n > 1000) / length(n_crops$n), '%)', '\n'))


cat('--- #damages with samples more than 0, 10, 100, 1000\n')
cat(paste0(sum(n_damages$n > 0), ' (', 100 * sum(n_damages$n > 0) / length(n_damages$n), '%)', '\n'))
cat(paste0(sum(n_damages$n > 10), ' (', 100 * sum(n_damages$n > 10) / length(n_damages$n), '%)', '\n'))
cat(paste0(sum(n_damages$n > 100), ' (', 100 * sum(n_damages$n > 100) / length(n_damages$n), '%)', '\n'))
cat(paste0(sum(n_damages$n > 1000), ' (', 100 * sum(n_damages$n > 1000) / length(n_damages$n), '%)', '\n'))


cat('--- #combinations with samples more than 0, 10, 100, 1000\n')
cat(paste0(sum(n_combn$n > 0), ' (', 100 * sum(n_combn$n > 0) / length(n_combn$n), '%)', '\n'))
cat(paste0(sum(n_combn$n > 10), ' (', 100 * sum(n_combn$n > 10) / length(n_combn$n), '%)', '\n'))
cat(paste0(sum(n_combn$n > 100), ' (', 100 * sum(n_combn$n > 100) / length(n_combn$n), '%)', '\n'))
cat(paste0(sum(n_combn$n > 1000), ' (', 100 * sum(n_combn$n > 1000) / length(n_combn$n), '%)', '\n'))


cat('--- #combinations of each prefecture with samples more than 0, 10, 100, 1000\n')
cat(paste0(sum(n_combnpref$n > 0), ' (', 100 * sum(n_combnpref$n > 0) / length(n_combnpref$n), '%)', '\n'))
cat(paste0(sum(n_combnpref$n > 10), ' (', 100 * sum(n_combnpref$n > 10) / length(n_combnpref$n), '%)', '\n'))
cat(paste0(sum(n_combnpref$n > 100), ' (', 100 * sum(n_combnpref$n > 100) / length(n_combnpref$n), '%)', '\n'))
cat(paste0(sum(n_combnpref$n > 1000), ' (', 100 * sum(n_combnpref$n > 1000) / length(n_combnpref$n), '%)', '\n'))



f_nitems <- data.frame(
    item = rep(c('crop', 'damage', 'crop x damage', 'crop x damage x pref'), each = 4),
    cate = rep(c('n<=10', '10<n<=100', '100<n<=1000', '1000<n'), 4),
    n = c(
        sum(0 < n_crops$n & n_crops$n <=10),
        sum(10 < n_crops$n & n_crops$n <=100),
        sum(100 < n_crops$n & n_crops$n <=1000),
        sum(1000 < n_crops$n),
        sum(0 < n_damages$n & n_damages$n <=10),
        sum(10 < n_damages$n & n_damages$n <=100),
        sum(100 < n_damages$n & n_damages$n <=1000),
        sum(1000 < n_damages$n),
        sum(0 < n_combn$n & n_combn$n <=10),
        sum(10 < n_combn$n & n_combn$n <=100),
        sum(100 < n_combn$n & n_combn$n <=1000),
        sum(1000 < n_combn$n),
        sum(0 < n_combnpref$n & n_combnpref$n <=10),
        sum(10 < n_combnpref$n & n_combnpref$n <=100),
        sum(100 < n_combnpref$n & n_combnpref$n <=1000),
        sum(1000 < n_combnpref$n)))
f_nitems$item <- factor(f_nitems$item, levels = c('crop', 'damage', 'crop x damage', 'crop x damage x pref'))
f_nitems$cate <- factor(f_nitems$cate, levels = rev(c('n<=10', '10<n<=100', '100<n<=1000', '1000<n')))
f_nitems <- ggplot(f_nitems, aes(x = item, y = n, fill = cate)) +
        geom_bar(stat = 'identity', position = 'fill') +
        scale_fill_nejm() +
        xlab('') +
        ylab('') 
png('trends/NITEMS_barplot.png', width = 2800, height = 1600, res = 600)
print(f_nitems)
dev.off()



# Distribution of DI
# ------------------------------------------------------------------------------


cat('--- values exactly equal to <0.1, =0.1, =1, =2, =3 \n')
cat(paste0(sum(DI$value < 0.1), ' (', 100 * sum(DI$value < 0.1) / length(DI$value), ')\n'))
cat(paste0(sum(DI$value == 0.1), ' (', 100 * sum(DI$value == 0.1) / length(DI$value), ')\n'))
cat(paste0(sum(DI$value == 1), ' (', 100 * sum(DI$value == 1) / length(DI$value), ')\n'))
cat(paste0(sum(DI$value == 2), ' (', 100 * sum(DI$value == 2) / length(DI$value), ')\n'))
cat(paste0(sum(DI$value == 3), ' (', 100 * sum(DI$value == 3) / length(DI$value), ')\n'))


fd_gamma <- fitdist(DI$value, 'gamma')
fd_lnorm <- fitdist(DI$value, 'lnorm')
fd_norm  <- fitdist(DI$value, 'norm')

summary(fd_gamma)
summary(fd_lnorm)
summary(fd_norm)

png('trends/DI_dist_fitting.png', width = 4000, height = 4600, res = 600)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
fitplt_name<- c('normal', 'log normal', 'gamma')
fitplt_col <- c('#D51317', '#EFD500', '#0094CD')
denscomp(list(fd_norm, fd_lnorm, fd_gamma), legendtext = fitplt_name, fitcol = fitplt_col, fitlwd = 2, fitlty =  1, ylim = c(0, 0.2))
qqcomp(list(fd_norm, fd_lnorm, fd_gamma), legendtext = fitplt_name, fitcol = fitplt_col, fitlwd =2)
cdfcomp(list(fd_norm, fd_lnorm, fd_gamma), legendtext = fitplt_name, fitcol = fitplt_col, fitlwd = 2, fitlty = 1)
ppcomp(list(fd_norm, fd_lnorm, fd_gamma), legendtext = fitplt_name, fitcol = fitplt_col, fitlwd = 2)
dev.off()



f_DIdist <- DI |>
    ggplot(aes(x = value)) +
    geom_histogram(binwidth = 0.2) +
    scale_x_continuous(trans = 'log10') +
    xlab('') +
    ylab('')
png('trends/DI_distribution.png', width = 2300, height = 1000, res = 600)
print(f_DIdist)
dev.off()


f_DIdist_ <- DI |>
    ggplot(aes(x = value)) +
    geom_histogram(binwidth = 0.2) +
    geom_vline(xintercept = log10(3)) +
    scale_x_continuous(trans = 'log10') +
    xlab('log10(DI)') +
    ylab('count')


table(DI$value <= 1) / length(DI$value) * 100
table(DI$value <= 3) / length(DI$value) * 100
table(DI$value <= 5) / length(DI$value) * 100
table(DI$value <= 10) / length(DI$value) * 100


# Survey terms
# ------------------------------------------------------------------------------


DI |>
    dplyr::mutate(combn_id = paste(crop, damage, prefecture, sep = '___'),
                  yr = str_sub(date, 1, 4),
                  month = str_sub(date, 6, 7)) |>
    dplyr::select(combn_id, crop, damage, prefecture, yr, month) |>
    dplyr::group_by(combn_id) |>
    dplyr::summarise(n_samples = n(), n_yr = n_distinct(yr), n_month = n_distinct(month), yr_from = min(yr), yr_to = max(yr)) |>
    dplyr::mutate(
        crop = str_split(combn_id, '___', simplify = TRUE)[, 1],
        damage = str_split(combn_id, '___', simplify = TRUE)[, 2],
        prefecture = str_split(combn_id, '___', simplify = TRUE)[, 3]
    ) |>
    dplyr::select(-combn_id) |>
    dplyr::arrange(desc(n_yr), desc(n_month), desc(n_samples)) |>
    write.table('trends/survey_terms.tsv', sep = '\t', row.names = FALSE, quote = FALSE)




n_yr <- DI |>
    dplyr::mutate(combn_id = paste0(crop, damage, prefecture),
                  yr = str_sub(date, 1, 4),
                  month = str_sub(date, 6, 7)) |>
    dplyr::select(combn_id, yr, month) |>
    dplyr::group_by(combn_id) |>
    dplyr::summarise(n_samples = n(), n_yr = n_distinct(yr), n_month = n_distinct(month))

table(n_yr$n_yr) 
table(n_yr$n_yr) / length(n_yr$n_yr) * 100

n_yr |>
    dplyr::filter(n_yr == 1) |>
    dplyr::select(n_samples) |>
    unlist() |>
    table()
    
100 - cumsum(table(n_yr$n_yr) / length(n_yr$n_yr) * 100) # check 6, 11, 21


n_yr |>
    dplyr::filter(n_yr == 25) |> # change to 5, 10, 15, 20, 25
    dplyr::select(n_samples) |>
    unlist() |>
    range()


plt_nmonth <- ggplot(n_yr, aes(x = as.factor(n_month), y = n_samples, color = as.factor(n_month))) +
    geom_jitter(alpha = 0.5, width = 0.35) +
    scale_color_manual(values = rep(c('#535c68', '#f0943b'), times = 14)) +
    theme(legend.position = 'none') +
    xlab('') +
    ylab('') 

plt_nmonth_bar <- n_yr |>
    mutate(n_month = as.factor(n_month)) |>
    group_by(n_month) |>
    summarise(n = n()) |>
    print(n = 100) |>
    ggplot(aes(x = n_month, y = n, fill = n_month)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = rep(c('#535c68', '#f0943b'), times = 14)) +
    theme(legend.position = 'none') +
    xlab('') +
    ylab('')


plt_nyear <- ggplot(n_yr, aes(x = as.factor(n_yr), y = n_samples, color = as.factor(n_yr))) +
    geom_jitter(alpha = 0.5, width = 0.35) +
    scale_color_manual(values = rep(c('#535c68', '#f0943b'), times = 14)) +
    theme(legend.position = 'none') +
    xlab('') +
    ylab('') 

plt_nyear_bar <- n_yr |>
    mutate(n_yr = as.factor(n_yr)) |>
    group_by(n_yr) |>
    summarise(n = n()) |>
    print(n = 100) |>
    ggplot(aes(x = n_yr, y = n, fill = n_yr)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = rep(c('#535c68', '#f0943b'), times = 14)) +
    theme(legend.position = 'none') +
    xlab('') +
    ylab('')


png('trends/n_months_scatter.png', width = 2000, height = 800, res = 600)
print(plt_nmonth)
dev.off()


png('trends/n_months_barplot.png', width = 2000, height = 800, res = 600)
print(plt_nmonth_bar)
dev.off()

png('trends/n_years_scatter.png', width = 3800, height = 1200, res = 600)
print(plt_nyear)
dev.off()

png('trends/n_years_barplot.png', width = 3800, height = 900, res = 600)
print(plt_nyear_bar)
dev.off()


table(n_yr$n_yr <= 5) / length(n_yr$n_yr) * 100
table(n_yr$n_yr > 5) / length(n_yr$n_yr) * 100
table(n_yr$n_yr > 10) / length(n_yr$n_yr) * 100
table(n_yr$n_yr > 20) / length(n_yr$n_yr) * 100


n_yr |>
    mutate(n_yr = as.factor(n_yr)) |>
    group_by(n_yr) |>
    summarise(n = n()) |>
    print(n = Inf)
    

n_yr |>
    mutate(n_month = as.factor(n_month)) |>
    group_by(n_month) |>
    summarise(n = n()) |>
    print(n = Inf)


n_month_tb <- table(n_yr$n_month)
cumsum(n_month_tb)/ sum(n_month_tb) * 100

plt_n_month_share <- n_yr |>
    mutate(n_month = factor(n_month, levels = as.character(12:1))) |>
    group_by(n_month) |>
    summarise(n = n()) |>
    mutate(x = 'x') |>
    ggplot(aes(x = x, y  = n, fill = n_month)) +
    geom_bar(stat = 'identity', position = 'fill') +
    scale_fill_manual(values = rev(get_cols(12))) +
    coord_flip() +
    theme(legend.position = 'none') +
    xlab('') +
    ylab('')
    
png('trends/survey_freq.png', width = 2000, height = 800, res = 500)
print(plt_n_month_share)
dev.off()




# ------------------------------------------------------------------------------
# Correlations of DI
# ------------------------------------------------------------------------------



if (!exists('cormat') || is.null(cormat)) {
    cormat <- NULL
    for (i in dimnames(DR)[[2]]) {
        cormat <- rbind(cormat, calc_corr(DR, item = i))
    }
    cormat <- cormat %>%
        filter(!is.na(cor)) %>%
        as_tibble()
}

cat('--- correlation between different crops \n')
cormat_0 <- cormat %>% filter(n_samples > 10)
cormat_1 <- cormat %>% filter(n_samples > 10, abs(cor) > 0.7)
cat(paste0(length(cormat_1$cor), ' (', 100 * sum(length(cormat_1$cor) / length(cormat$cor)), '%)\n'))
cormat_2 <- cormat %>% filter(n_samples > 10, 0.4 < abs(cor), abs(cor) <= 0.7)
cat(paste0(length(cormat_2$cor), ' (', 100 * sum(length(cormat_2$cor) / length(cormat$cor)), '%)\n'))
nn_ <- length(cormat$cor) - length(cormat_1$cor) - length(cormat_2$cor)
cat(paste0(nn_, ' (', 100 * nn_ / length(cormat$cor), '%)\n'))


cormat |>
    dplyr::mutate(crop_1 = str_split(crops, ' x ', simplify = TRUE)[, 1],
           crop_2 = str_split(crops, ' x ', simplify = TRUE)[, 2]) |>
    dplyr::select(item, crop_1, crop_2, cor, n_samples) |>
    dplyr::arrange(desc(n_samples), desc(cor)) |>
    write.table(file = 'trends/corcoef.tsv', sep = '\t', row.names = FALSE, quote = FALSE)

cormat |>
    dplyr::mutate(crop_1 = str_split(crops, ' x ', simplify = TRUE)[, 1],
                  crop_2 = str_split(crops, ' x ', simplify = TRUE)[, 2]) |>
    dplyr::select(item, crop_1, crop_2, cor, n_samples) |>
    dplyr::filter(n_samples > 10) |>
    dplyr::filter(cor > 0.7 | cor < -0.7) |>
    dplyr::arrange(desc(n_samples), desc(cor)) |>
    print(n = Inf)
    
    

cormat |>
    dplyr::mutate(crop_1 = str_split(crops, ' x ', simplify = TRUE)[, 1],
                  crop_2 = str_split(crops, ' x ', simplify = TRUE)[, 2]) |>
    dplyr::select(item, crop_1, crop_2, cor, n_samples) |>
    dplyr::filter(n_samples > 10) |>
    dplyr::filter(cor > 0.4 | cor < -0.4) |>
    dplyr::filter(cor < 0.7 | cor > -0.7) |>
    dplyr::arrange(desc(n_samples), desc(cor)) |>
    print(n = Inf)




f_cor <- ggplot(cormat, aes(x = n_samples, y = cor)) +
    geom_point() +
    scale_x_log10() +
    xlab('') +
    ylab('') +
    theme(legend.position = 'none')


png('trends/correlation_distribution.png', width = 2300, height = 1200, res = 600)
print(f_cor)
dev.off()



# ------------------------------------------------------------------------------
# ANOVA
# ------------------------------------------------------------------------------

if (!exists('pmat') || is.null(pmat)) {
    pmat <- NULL
    pmat <- calc_ANOVA(DR)
}
pmat_ <- pmat[!is.na(pmat$p), ] |>
    arrange(p)
pmat_$n_month <- sprintf('%02d', pmat_$n_month)
pmat_$n_month <- factor(pmat_$n_month, levels = c('01', sort(unique(pmat_$n_month))))



cat('--- ANOVA test result \n')
cat('Total: ', nrow(pmat), '\n')
cat('p-value is NA: ', sum(is.na(pmat$p)), '\n')
cat('q-value < 0.01: ', sum(pmat_$q < 0.01), '\n')
cat('q-value >= 0.01: ', sum(pmat_$q >= 0.01), '\n')

write.table(pmat_, file = 'trends/anova_results.tsv', sep = '\t', row.names = FALSE, quote = FALSE)

f_anova_sample <- ggplot(pmat_, aes(x = n_sample, y = q)) +
    geom_point() +
    scale_x_log10() +
    xlab('') +
    ylab('') +
    theme(legend.position = 'bottom')
f_anova_month <- ggplot(pmat_, aes(x = n_month, y = q)) +
    geom_jitter(alpha = 0.2, size = 1) +
    geom_violin(alpha = 0.1) +
    stat_summary(fun.data = data_summary_func,
                 geom = 'pointrange', col = '#333') +
    xlab('') +
    ylab('') +
    theme(legend.position = 'bottom')


    
png('trends/anova_results_vssample.png', width = 2300, height = 1200, res = 600)
print(f_anova_sample)
dev.off()

png('trends/anova_results_vsmonths.png', width = 2300, height = 1200, res = 600)
print(f_anova_month)
dev.off()



pmat_ <- pmat_ %>% arrange(desc(n_sample))
for (i in 1:5) {
    dr <- DR[pmat_$crop[i], pmat_$cdp[i], , pmat_$loc[i]]
    dr <- data.frame(date = names(dr), value = as.numeric(dr)) |>
        dplyr::mutate(month = substr(date, 6, 7)) |>
        dplyr::select(month, value) |>
        dplyr::filter(!is.na(value))
    dr_p <- ggplot(dr, aes(x = month, y = value, color = month, group = month)) +
        geom_violin() +
        geom_jitter(alpha = 0.3) +
        stat_summary(fun.data = data_summary_func,
                 geom = 'pointrange', col = '#333333') +
        scale_color_manual(values = get_cols(length(levels(pmat_$n_month)))) +
        theme(legend.position = 'none') +
        xlab('') +
        ylab('') 
    fname <- paste0('trends/monthlytrend_', pmat_$crop[i], '_', pmat_$cdp[i], '_', pmat_$loc[i], '.png')
    png(fname, width = 2300, height = 1200, res = 600)
    print(dr_p)
    dev.off()
}



# ------------------------------------------------------------------------------
# Analysis of RMSE
# ------------------------------------------------------------------------------


# Data Loading & Preprocessing
# ------------------------------------------------------------------------------
load('../data/MODELS.NA.RData')
rmsedf_ <- NULL
for (m in names(rmses)) {
    rmsedf_ <- bind_rows(rmsedf_, rmses[[m]])
}
rmsedf_ <- rmsedf_ |> mutate(combnid = paste0(crop, damage, prefecture))
di_ <- DI |>
    dplyr::mutate(combnid = paste0(crop, damage, prefecture),
                  yr = str_sub(date, 1, 4),
                  month = str_sub(date, 6, 7)) |>
    dplyr::group_by(combnid) |>
    dplyr::summarise(n_samples_ = n(), n_month = n_distinct(month), n_yr = n_distinct(yr))
rmsedf_ <- dplyr::full_join(rmsedf_, di_, by = 'combnid') |>
    dplyr::mutate(survey_term = case_when(
        n_yr <= 5 ~ 'STS',
        5 < n_yr & n_yr <= 10 ~ 'MTS',
        10 < n_yr ~ 'LTS',
        TRUE ~ 'NA'
        ))

pax_rmsedf <- rmsedf_ %>%
    filter(model == 'PA' | model == 'PAX')
rmsedf <- rmsedf_ %>%
    filter(model != 'PAX')

rmsedf$model <- factor(rmsedf$model,
                       levels = c('RAND', 'LOGRAND', 'PA', 'ARIMA', 'SARIMA', 'SARIMAX', 'GPR', 'RF'))
rmsedf$n_month <- factor(rmsedf$n_month,
                       levels = as.character(1:12))
#rmsedf$n_yr <- factor(rmsedf$n_yr,
 #                     levels = as.character(1:27))
                         


# Averages of RMSE
# ------------------------------------------------------------------------------

rmsedf |>
    dplyr::filter(!is.na(rmse)) |>
    dplyr::group_by(model, survey_term) |>
    dplyr::summarise(n_models = n()) |>
    print(n = Inf)

rmsedf |>
    dplyr::group_by(survey_term, model) |> 
    dplyr::summarise(mean = mean(rmse, na.rm = TRUE), sd = sd(rmse, na.rm = TRUE), median = median(rmse, na.rm = TRUE)) |> 
    print(n = Inf)

for (st in c('STS', 'MTS', 'LTS')) {
    rmsedf_st <- rmsedf[rmsedf$survey_term == st, ]
    rmse_boxplot <- ggplot(rmsedf_st, aes(x = model, y = log10(rmse), group = model, color = model)) +
        geom_jitter(alpha = 0.2, size = .1) +
        geom_violin(alpha = 0.1) +
        stat_summary(fun.data = data_summary_func,
                     geom = 'pointrange', col = rep('#333333', times = 8)) +
        ylim(-2.5, 2.5) +
        scale_colour_tableau() +
        xlab('') +
        ylab('') +
        theme(legend.position = 'none') 
    png(paste0('trends/rmse_boxplot-', st, '.png'), width = 2600, height = 1000, res = 600)
    print(rmse_boxplot)
    dev.off()
}


rmse_scatter_vssample <-  ggplot(rmsedf, aes(x = n_samples, y = log10(rmse), color = model)) +
    geom_point(alpha = 0.2) +
    stat_smooth(alpha = 0, linewidth = 0.5) +
    ylim(-1, 2) +
    scale_colour_tableau() +
    theme(legend.position = 'bottom')
rmse_scatter_vsmonths <-  ggplot(rmsedf, aes(x = n_month, y = log10(rmse), color = model)) +
    geom_jitter(alpha = 0.2, size = .1) +
    geom_violin(alpha = 0.1) +
    stat_summary(fun.data = data_summary_func,
                 geom = 'pointrange', col = rep('#333333', times = 88)) +
    ylim(-1, 2) +
    scale_colour_tableau() +
    theme(legend.position = 'none') +
    facet_grid(model ~ .)
rmse_scatter_vsyears <-  ggplot(rmsedf, aes(x = n_yr, y = log10(rmse), color = model)) +
    geom_jitter(alpha = 0.2, width = 0.1, size = 0.8) +
    stat_smooth(alpha = 0, linewidth = 1) +
    ylim(-1, 2) +
    scale_colour_tableau() +
    theme(legend.position = 'none') +
    facet_grid(model ~ .)

png('trends/rmse_scatter_vssample.png', width = 2700, height = 3000, res = 600)
print(rmse_scatter_vssample)
dev.off()
png('trends/rmse_scatter_vsmonth.png', width = 2700, height = 5000, res = 600)
print(rmse_scatter_vsmonths)
dev.off()
png('trends/rmse_scatter_vsyears.png', width = 2700, height = 5000, res = 600)
print(rmse_scatter_vsyears)
dev.off()




# t-test for RMSE
# ------------------------------------------------------------------------------
m_ <- matrix(NA, nrow = length(unique(rmsedf$model)), ncol = length(unique(rmsedf$model)))
colnames(m_) <- rownames(m_) <- c('RAND', 'LOGRAND', 'PA', 'ARIMA', 'SARIMA', 'SARIMAX', 'GPR', 'RF')
rmse_pmat <- list(STS = m_, MTS = m_, LTS = m_)
for (st in c('STS', 'MTS', 'LTS')) {
    rmsedf_st <- rmsedf[rmsedf$survey_term == st, ]
    rmsemat <- rmsedf_st |>
        dplyr::select(combnid, model, rmse) |>
        tidyr::pivot_wider(names_from = model, values_from = rmse)
    for (m1 in colnames(m_)) {
        for (m2 in rownames(m_)) {
            d1 <- log10(rmsemat[[m1]] + 0.001)
            d2 <- log10(rmsemat[[m2]] + 0.001)
            ff <- is.nan(d1) | is.nan(d2)
            d1 <- d1[!ff]
            d2 <- d2[!ff]
            #idx <- sample(1:length(d1), 30)
            rmse_pmat[[st]][m1, m2] <- t.test(d1, d2,
                        alternative = "two.sided", paired = TRUE)$p.value
        }
    }
    rmse_pmat[[st]][upper.tri(rmse_pmat[[st]], diag = TRUE)] <- NA
    rmse_pmat[[st]] <- rmse_pmat[[st]][, c(-2, -8)]
    rmse_pmat[[st]] <- rmse_pmat[[st]][-1,]
}


rmse_pmat_30s <- list(STS = m_, MTS = m_, LTS = m_)
set.seed(1)
for (st in c('STS', 'MTS', 'LTS')) {
    rmsedf_st <- rmsedf[rmsedf$survey_term == st, ]
    rmsemat <- rmsedf_st |>
        dplyr::select(combnid, model, rmse) |>
        tidyr::pivot_wider(names_from = model, values_from = rmse)
    for (m1 in colnames(m_)) {
        for (m2 in rownames(m_)) {
            d1 <- log10(rmsemat[[m1]] + 0.001)
            d2 <- log10(rmsemat[[m2]] + 0.001)
            ff <- is.nan(d1) | is.nan(d2)
            d1 <- d1[!ff]
            d2 <- d2[!ff]
            if (length(d1) > 200) {
                idx <- sample(1:length(d1), 200)
            } else {
                idx <- 1:length(d1)
            }
            rmse_pmat_30s[[st]][m1, m2] <- t.test(d1[idx], d2[idx],
                                              alternative = "two.sided", paired = TRUE)$p.value
        }
    }
    rmse_pmat_30s[[st]][upper.tri(rmse_pmat_30s[[st]], diag = TRUE)] <- NA
    rmse_pmat_30s[[st]] <- rmse_pmat_30s[[st]][, c(-2, -8)]
    rmse_pmat_30s[[st]] <- rmse_pmat_30s[[st]][-1,]
}





# Statistics of PA models
# ------------------------------------------------------------------------------

pa_n_table <- pax_rmsedf |>
    dplyr::filter(model == 'PA') |>
    dplyr::select(n_samples, rmse, n) |>
    dplyr::filter(!is.na(n) & !is.nan(rmse)) 

round(table(pa_n_table$n) / length(pa_n_table$n) * 100, 1)



pa_rmse_n_1 <- ggplot(pa_rmsedf, aes(x = n_samples, y = n, color = as.factor(n))) +
    geom_jitter(alpha = 0.4, width = 0, height = 0.3) +
    scale_colour_tableau('Classic Cyclic') +
    theme(legend.position = 'bottom')

pa_rmse_n_2 <- ggplot(pa_rmsedf, aes(x = n, y = log10(rmse), color = as.factor(n))) +
    geom_jitter(alpha = 0.4, width = 0.3, height = 0) +
    ylim(-2, 2) +
    scale_colour_tableau('Classic Cyclic') +
    theme(legend.position = 'bottom')


png('trends/pa_rmse_1.png', width = 1600, height = 2200, res = 600)
print(pa_rmse_n_1)
dev.off()
png('trends/pa_rmse_2.png', width = 1600, height = 2200, res = 600)
print(pa_rmse_n_2)
dev.off()




rmse_boxplot_pax <- ggplot(pax_rmsedf, aes(x = model, y = log10(rmse), group = model, color = model)) +
    geom_jitter(alpha = 0.2, size = .1) +
    geom_violin(alpha = 0.1) +
    geom_boxplot(width = .2, outlier.shape = 'n', alpha = 0.5, color = '#3f3f3f') +
    ylim(-2.5, 2.5) +
    scale_colour_tableau() +
    xlab('') +
    ylab('') +
    theme(legend.position = 'none') 
png('trends/rmse_boxplot_pax.png', width = 1200, height = 1800, res = 600)
print(rmse_boxplot_pax)
dev.off()


rmse_scatter_pax <-  ggplot(pax_rmsedf, aes(x = n_samples, y = log10(rmse), color = model)) +
    geom_point(alpha = 0.2) +
    stat_smooth(alpha = 0, linewidth = 0.5) +
    ylim(-1, 2) +
    scale_colour_tableau() +
    theme(legend.position = 'bottom')

png('trends/rmse_scatter_pax.png', width = 2700, height = 2300, res = 600)
print(rmse_scatter_pax)
dev.off()



#save(list = ls(), file = VIZ_RDATA)
