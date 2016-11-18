# Included here first since fetch_data is usually the first function that is
# called.
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Fetch data from the original source
#'
#' This function fetchs the main dataset, keeps variables relevant to
#' the analysis, restrict the sample size as needed, and lastly save
#' the new dataset as an `.RData` file.
#'
#' @return Saves the wrangled data into the data/ folder.
#' @export
#'
#' @examples
#' fetch_data()
#'
fetch_data <- function() {
    # Load the master dataset,
    ds.prep <- PROMISE::PROMISE_data %>%
        dplyr::filter(VN %in% c(1, 3, 6)) %>%
        ## Kick out Canoers
        dplyr::filter(is.na(Canoe)) %>%
        dplyr::tbl_df()

    print(paste0('Original dataset rows are ', dim(ds.prep)[1], ' and columns are ', dim(ds.prep)[2]))

    ##' Munge and wrangle the data into the final version.
    ds <- ds.prep %>%
        dplyr::select(
            SID, VN, TAG, HDL, Glucose0, Waist, Systolic, Diastolic, HOMA,
            dplyr::matches('^TotalTG'),
            tg140,
            tg160,
            tg141n7,
            tg161n7,
            tg180,
            tg200,
            tg201n9,
            tg181n9,
            tg181n7
        ) %>%
        dplyr::arrange(SID, VN) %>%
        dplyr::group_by(SID) %>%
        tidyr::fill(TotalTG, dplyr::matches('^tg\\d+')) %>%
        dplyr::group_by(VN) %>%
        dplyr::mutate(
            HOMA = as.numeric(scale(HOMA)),
            invHDL = as.numeric(scale(1/HDL)),
            Waist = as.numeric(scale(Waist)),
            SBP = as.numeric(scale(Systolic)),
            DBP = as.numeric(scale(Diastolic)),
            FPG = as.numeric(scale(Glucose0)),
            TG = as.numeric(scale(TAG))
        ) %>%
        dplyr::ungroup()

    ds <- ds %>%
        dplyr::full_join(ds %>%
                      dplyr::filter(VN == 1) %>%
                      dplyr::mutate_each(dplyr::funs((. / TotalTG) * 100), dplyr::matches('^tg\\d+')) %>%
                      dplyr::select(SID, dplyr::matches('^tg\\d+')) %>%
                      stats::setNames(paste0('pct_', names(.))) %>%
                      dplyr::rename(SID = pct_SID),
                  by = 'SID')

    ds <- ds %>%
        dplyr::mutate(
            VN = plyr::mapvalues(VN, c(1, 3, 6), c(0, 1, 2)),
            f.VN = factor(VN, c(0, 1, 2), c('yr0', 'yr3', 'yr6'))
        ) %>%
        dplyr::arrange(SID, VN) %>%
        dplyr::filter(!is.na(TotalTG))

    print(paste0('Working dataset rows are ', dim(ds)[1], ' and columns are ', dim(ds)[2]))

    # Final dataset object
    project_data <- ds

    # Save the dataset to the data/ folder.
    devtools::use_data(project_data, overwrite = TRUE)
    # Save the variable names as an internal dataset
    vars <- names(project_data)
    devtools::use_data(vars, internal = TRUE, overwrite = TRUE)
}
