
#' @name get_cis_tscores
#'
#' @title Run analysis on Cognitive Impairment Screener (CIS) Scores
#'
#' @description get_cis_tscores() returns a set of t-scores for the Cognitive Impairment Screener (CIS) assessment
#'
#' @importFrom stats sd
#'
#' @section Development:
#' 2025-08-05: Added so that could be added to S2 scripts. JC
#'
#' @param df The data frame with p_respondent, age, and cis_pro
#'
#' @returns Additional column of CIS tscores
#'
#' @export
#'
#'

get_cis_tscores <- function(df = NULL) {

  cis_pred <- 6.4072045 + 0.2896971 * df$age -1.6339543 * df$p_respondent

  cis_low <- as.numeric((df$cis_pro - cis_pred) < 0)

  cis_sd_pred <- sqrt(9.969673 + 5.485572 * df$age + 26.371035 * df$p_respondent +
                        7.266461 * cis_low - 3.628262 * df$age * cis_low -
                        32.770611 * df$p_respondent * cis_low)

  res_adj <-
    dplyr::case_when(!(df$youth)  ~ 0.2337767,
              as.logical(df$youth)  ~ 0.1708991)
  sd_adj <-
    dplyr::case_when(!(df$youth)  ~ 0.9734727,
              as.logical(df$youth)  ~ 0.9802355)

  df$cis_study_tscores <- (((df$cis_pro - cis_pred) / cis_sd_pred) +
                                            res_adj) / (sd_adj) * 10 + 50

  print(
    df |>
      dplyr::group_by(gender, youth, p_respondent) |>
      dplyr::summarise(n = dplyr::n(),
                       mean = mean(cis_study_tscores, na.rm = T),
                       sd = stats::sd(cis_study_tscores, na.rm = T))
  )


  return(df)

}
