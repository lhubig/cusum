#' Risk-adjusted Performance Data
#'
#' Generated performance data of indicator: Ratio of observed to expected cases of severe stroke or death under open carotid stenosis surgery for Bavarian hospitals in the years 2016-2017.
#'
#' @format A data frame with 2000 rows and 4 variables:
#' \describe{
#'   \item{t}{Temporal sequence of observations}
#'   \item{y}{Patient outcome}
#'   \item{score}{Patient risk score}
#'   \item{year}{Year of treatment}
#' }
#' @source Decription of performance indicator (in German): \url{https://iqtig.org/downloads/auswertung/2016/10n2karot/QSKH_10n2-KAROT_2016_QIDB_V02_2017-04-26.pdf}
"racusum_example_data"

#' Non-Risk-adjusted Performance Data
#'
#' Generated performance data of indicator 54030: Preoperative stay over 24 hours for patients with proximal femur fracture for Bavarian hospitals in the years 2016-2017.
#' 
#' @format A data frame with 2000 rows and 3 variables:
#' \describe{
#'   \item{t}{Temporal sequence of observations}
#'   \item{y}{Patient outcome}
#'   \item{year}{Year of treatment}
#' }
#' @source Decription of performance indicator (in German): \url{https://iqtig.org/downloads/auswertung/2016/17n1hftfrak/QSKH_17n1-HUEFTFRAK_2016_QIDB_V02_2017-04-26.pdf}
"cusum_example_data"
