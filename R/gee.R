# Functions for the GEE analysis
#
# Grab or combine data ----------------------------------------------------

#' Prepare the project data for analysis through GEE.
#'
#' @param data project data
#' @export
prep_gee_data <- function(data) {
    no_fattyacids <- data %>%
        dplyr::select(-dplyr::matches('pct_tg\\d+|^tg\\d+'), -TotalTG)

    scaled_variables <- data %>%
        dplyr::filter(VN == 0) %>%
        dplyr::select(SID, TotalTG, dplyr::matches('pct_tg\\d+|^tg\\d+')) %>%
        dplyr::mutate_each(dplyr::funs(as.numeric(scale(.))), -SID)

    dplyr::full_join(
            no_fattyacids,
            scaled_variables,
            by = 'SID'
        ) %>%
        dplyr::arrange(SID, VN)
}

# Analyze -----------------------------------------------------------------

#' Run GEE models on the prepared project data.
#'
#' @param data The project data
#' @param y outcomes (IS, BCF)
#' @param x predictors (TAGFA)
#' @param covariates to adjust for
#' @param rename_x Function to rename x variables
#' @param rename_y Function to rename y variables
#' @export
analyze_gee <- function(data = project_data,
                        y = outcomes,
                        x = list(
                            tg_pct = tg_pct
                        ),
                        covars = covariates,
                        rename_x = renaming_fats,
                        rename_y = renaming_outcomes) {

    data %>%
        prep_gee_data() %>%
        mason::design('gee') %>%
        mason::add_settings(family = stats::gaussian(),
                            corstr = 'ar1', cluster.id = 'SID') %>%
        mason::add_variables('yvars', y) %>%
        mason::add_variables('xvars', x[['tg_pct']]) %>%
        mason::add_variables('covariates', covars) %>%
        mason::construct() %>%
        mason::scrub() %>%
        mason::polish_filter("Xterm", 'term') %>%
        dplyr::mutate(unit = ifelse(grepl('pct', Xterms), 'mol%',
                                    ifelse(grepl('^tg\\d', Xterms), 'nmol/mL',
                                           'Totals'))) %>%
        #mason::polish_transform_estimates(function(x) (exp(x) - 1) * 100) %>%
        mason::polish_renaming(rename_x, 'Xterms') %>%
        mason::polish_renaming(rename_y, 'Yterms') %>%
        dplyr::mutate(
            order1 = substr(Xterms, nchar(Xterms), nchar(Xterms)),
            order1 = ifelse(order1 == 0, 10, order1),
            order1 = ifelse(order1 == 'l', 20, order1),
            order1 = ifelse(order1 == 'G', 30, order1),
            order1 = as.integer(order1)
        ) %>%
        mason::polish_adjust_pvalue(method = 'BH') %>%
        dplyr::rename(unadj.p.value = p.value, p.value = adj.p.value) %>%
        dplyr::arrange(desc(order1)) %>%
        dplyr::mutate(Xterms = factor(Xterms, unique(Xterms))) %>%
        dplyr::select(-order1)

}

# Plotting ----------------------------------------------------------------

#' Plot the GEE results in a Forest plot style.
#'
#' @param results Results data frame from the GEE analysis
#'
#' @export
plot_gee_main <- function(results = analyze_gee()) {
    results %>%
        dplyr::mutate(p.value = ifelse(p.value > 0.05, 1, 0.04)) %>%
        seer::view_main_effect(
            graph.options = 'dot.size',
            groups = 'unit~Yterms',
            legend.title = 'Significant\nvalues',
            xlab = 'Standard deviation (SD) difference with 95% CI in the\noutcomes for every SD increase in a fatty acid',
            ylab = 'Fatty acids'
            ) +
        ggplot2::facet_grid(~Yterms, scales = 'free') +
        graph_theme(ticks = FALSE, legend.pos = 'right') +
        ggplot2::theme(legend.margin = grid::unit(0, 'cm'))
}
