#' Effect of time to treatment on efficacy of rt-PA in treating acute stroke
#'
#' A dataset reconstructing reported outcomes after stroke
#' as reported in Lees et. al. (2010) which pooled the results
#' of several studies.
#'
#' @format A data frame with 3669 rows and 3 variables:
#' \describe{
#'   \item{time}{Time interval (minutes) between stroke onset and treatment}
#'   \item{treat}{Type of treatment received}
#'   \item{mRS}{Outcome at 3 months measured using the modified Rankin Scale}
#' }
#'
#' @references Lees, K. R., Bluhmki, E., Von Kummer, R., Brott, T. G.,
#' Toni, D., Grotta, J. C.,  Albers, G. W., Kaste, M., Marler, J. R.,
#' Hamilton, S. A., Tilley, B. C., Davis, S. M., Donnan, G. A., Hacke, W.
#'  (2010).
#' Time to treatment with intravenous alteplase and outcome in stroke:
#' an updated pooled analysis of ECASS, ATLANTIS, NINDS, and EPITHET trials.
#' \emph{The Lancet}, 375(9727), 1695-1703.
#'
"alteplase"

#' Effect of hydrocortisone on mortality and organ support in patients with severe COVID-19
#'
#' A dataset reconstructing reported outcomes in severe COVID-19 as reported in Angus et al. (2020)
#'
#' @format A data frame with 379 rows and 6 variables:
#' \describe{
#'   \item{group}{The treatment group in the trial. uc = Usual Care, shock = Shock-dependent hydrocortisone, fixed = Fixed-dose hydrocortisone}
#'   \item{deathTime}{Time of death, censored at 21 days}
#'   \item{dischargeTime}{Time of ICU discharge, censored at 21 days}
#'   \item{deathStatus}{If death was observed (0=no, 1=yes)}
#'   \item{dischargeStatus}{If ICU discharde was observed (0=no, 1=yes)}
#'   \item{score}{An ordinal score ranging from -1 to 21 covering death and organ support-free days }
#' }
#'
#' @references Angus, Derek C., et al. "Effect of hydrocortisone on mortality and organ support in
#' patients with severe COVID-19: the REMAP-CAP COVID-19 corticosteroid domain randomized
#' clinical trial." Jama 324.13 (2020): 1317-1329.
#'
"remapcap"
