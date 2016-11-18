# Renaming ----------------------------------------------------------------

renaming_outcomes <- function(x) {
    x %>%
        gsub('^invHDL$', '1/HDL', .)
}

renaming_fats <- function(x) {
    x %>%
        gsub('.*(\\d\\d)(\\d)', '\\1:\\2', .) %>%
        gsub('n(\\d)$', 'n-\\1', .) %>%
        gsub('D(\\d\\d)$', 'D-\\1', .) %>%
        gsub('^pct_', '', .) %>%
        gsub('TotalTG', 'Total', .) %>%
        gsub('^TAG$', 'Clinical TAG', .)
}
