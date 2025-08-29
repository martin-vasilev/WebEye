#' Summarize Effect Size, Credible Interval, and Bayes Factors from a brms Model
#'
#' This function extracts the posterior median, 95% credible interval (CrI),
#' the posterior probability for a directional hypothesis (P(b>0) or P(b<0)),
#' the associated evidence ratio (ER) for the directional hypothesis, and
#' the Savage-Dickey density ratio (BF10) for the hypothesis b = 0 vs b != 0
#' for a specified coefficient from a brmsfit object. Uses round(digits=10) for BF01.
#'
#' @param model A fitted brms model object (class 'brmsfit').
#' @param coefficient_name A string specifying the name of the fixed effect
#'   coefficient to summarize (e.g., "x", "conditiontreatment"). Must match
#'   the names in `rownames(fixef(model))`.
#'
#' @return A character string summarizing the results, including the median
#'   estimate (b), 95% CrI, directional posterior probability (P), directional
#'   evidence ratio (ER_dir), and the Savage-Dickey BF10. Results are formatted
#'   using LaTeX math delimiters. Handles Inf and near-zero edge cases for BFs.
#'
#' @details
#' The function calculates and formats the following values:
#' - **Posterior Median & 95% Credible Interval**: For values with a magnitude
#'   of 0.01 or greater, they are rounded to 2 decimal places. For values with
#'   a magnitude less than 0.01, they are formatted to show at least one
#'   significant digit to avoid displaying them as "0.00".
#' - **Directional Posterior Probability**: $P(b > 0)$ if the median is positive,
#'   or $P(b < 0)$ if the median is negative. Formatted using the same rules
#'   as the posterior median.
#' - **Directional Evidence Ratio (ER_dir)**: Evidence ratio for the directional
#'   hypothesis (e.g., H: b>0 vs H: b<0). Reported as '= value' (formatted
#'   as above), '> 1000' (for large or Inf values), or '< 0.001'.
#' - **Savage-Dickey BF10**: Bayes Factor comparing H1: b != 0 vs H0: b = 0.
#'   Calculated as 1 / BF01 from `brms::hypothesis(model, "coef = 0")`.
#'   `round(BF01, digits = 10)` is applied to handle floating point issues.
#'   BF10 is reported as '= value' (formatted as above), '> 1000', or '< 0.001'.
#'
#' **Important Note on BF10:** The Savage-Dickey BF10 calculation requires
#' an explicit, proper prior distribution (ideally centered at 0) to be specified for
#' the `coefficient_name` in the `brm()` model call (e.g., using the `prior`
#' argument). Using default flat priors or priors not defined at 0 will likely
#' result in `NA` or infinite values.
#'
#' @examples
#' \dontrun{
#' # Ensure brms is installed and loaded
#' # install.packages("brms")
#' library(brms)
#'
#' # --- Example 1: Model with a very small effect ---
#' set.seed(42)
#' dat_small <- data.frame(
#'   x = rnorm(150),
#'   y = rnorm(150, mean = 0.003 * x, sd = 1) # Very small true effect
#' )
#'
#' # Fit a model with an appropriate prior for BF calculation
#' prior_for_x_small <- prior(normal(0, 0.5), class = b, coef = x)
#' fit_small_effect <- brm(y ~ x, data = dat_small, prior = prior_for_x_small,
#'                         cores = 1, seed = 123, silent = 2, refresh = 0,
#'                         backend = "cmdstanr")
#'
#' # The corrected function will now display the small estimate properly
#' summary_str_small <- brms_effect_summary(fit_small_effect, "x")
#' print(summary_str_small)
#' # Example output might look like:
#' # $b = 0.0042$, 95% CrI $[-0.16, 0.17]$, $P(b > 0) = 0.52$, ER$_{b > 0} = 1.07$, BF$_{10} = 0.15$
#'
#' # --- Example 2: Model with appropriate prior for BF10 ---
#' set.seed(123)
#' dat_strong <- data.frame(
#'   x = rnorm(100, 0, 0.5),
#'   y = rnorm(100, mean = 5 * x, sd = 1)
#' )
#' prior_for_x <- prior(normal(0, 2), class = b, coef = x)
#' fit_strong_effect <- brm(y ~ x, data = dat_strong, prior = prior_for_x,
#'                          cores = 1, seed = 456, silent = 2, refresh = 0,
#'                          backend = "cmdstanr")
#' summary_str_strong <- brms_effect_summary(fit_strong_effect, "x")
#' print(summary_str_strong)
#' }
#' @export
#' @importFrom brms fixef hypothesis prior brm

brms_effect_summary <- function(model, coefficient_name) {
  # Check if brms is loaded
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package 'brms' is required but not installed.")
  }
  
  # --- NEW: Helper function for intelligent formatting ---
  # Formats numbers to 2 decimal places, but uses significant figures
  # for small numbers to avoid rounding to "0.00".
  format_value <- function(value) {
    if (is.na(value) || !is.numeric(value)) {
      return(as.character(value))
    }
    # For numbers with small magnitude (but not zero), show significant digits
    if (abs(value) < 0.01 && value != 0) {
      # formatC with 'g' and digits=2 prints in a nice format, e.g., 0.0034
      # signif() could also be used, but formatC is robust.
      return(formatC(value, format = "g", digits = 2))
    } else {
      # For numbers >= 0.01 or exactly 0, round to 2 decimal places
      return(format(round(value, 2), nsmall = 2))
    }
  }
  
  # Validate input model type
  if (!inherits(model, "brmsfit")) {
    stop("Model must be a brmsfit object.")
  }
  
  # Extract fixed effects
  fixed_effects <- tryCatch({
    brms::fixef(model)
  }, error = function(e) {
    stop("Could not extract fixed effects using brms::fixef. Error: ", e$message)
  })
  
  # Check if the coefficient exists
  if (!coefficient_name %in% rownames(fixed_effects)) {
    available_coefs <- paste(rownames(fixed_effects), collapse=", ")
    stop(paste0("Coefficient name '", coefficient_name,
                "' not found in the model's fixed effects. Available coefficients are: ",
                available_coefs))
  }
  
  # --- Extract Estimate and Credible Interval ---
  coef_estimate <- fixed_effects[coefficient_name, "Estimate"]
  cri_lower <- fixed_effects[coefficient_name, "Q2.5"]
  cri_upper <- fixed_effects[coefficient_name, "Q97.5"]
  
  # --- MODIFIED: Use the new helper function for formatting ---
  coef_estimate_fmt <- format_value(coef_estimate)
  cri_lower_fmt <- format_value(cri_lower)
  cri_upper_fmt <- format_value(cri_upper)
  
  # --- Directional Hypothesis Test (P(b>0) or P(b<0)) ---
  hypothesis_string <- "$P_{dir}$ = NA" # Default value
  evidence_ratio_string <- "NA"         # Default value
  
  direction_operator <- ifelse(coef_estimate >= 0, ">", "<")
  hypothesis_formula_dir <- paste0(coefficient_name, " ", direction_operator, " 0")
  
  hyp_result_directional <- tryCatch({
    brms::hypothesis(model, hypothesis_formula_dir)
  }, error = function(e) {
    warning(paste("Could not compute directional hypothesis (", hypothesis_formula_dir, ") for '",
                  coefficient_name, "': ", e$message, sep=""), call. = FALSE)
    return(NULL)
  })
  
  if (!is.null(hyp_result_directional) && inherits(hyp_result_directional, "brmshypothesis") &&
      !is.null(hyp_result_directional$hypothesis) &&
      all(c("Post.Prob", "Evid.Ratio") %in% names(hyp_result_directional$hypothesis)))
  {
    p_value <- hyp_result_directional$hypothesis$Post.Prob
    evidence_ratio <- hyp_result_directional$hypothesis$Evid.Ratio
    
    # --- MODIFIED: Use helper function for posterior probability ---
    p_value_fmt <- format_value(p_value)
    hypothesis_string <- paste0("$P(b ", direction_operator, " 0) = ", p_value_fmt, "$")
    
    if (is.na(evidence_ratio)) {
      evidence_ratio_string <- "NA"
    } else if (!is.finite(evidence_ratio)) {
      evidence_ratio_string <- if (evidence_ratio == Inf) "> 1000" else "NA"
    } else if (evidence_ratio > 1000) {
      evidence_ratio_string <- "> 1000"
    } else if (evidence_ratio < 0.001 && evidence_ratio >= 0) {
      evidence_ratio_string <- "< 0.001"
    } else if (evidence_ratio < 0) {
      warning(paste("Directional Evidence Ratio for '", coefficient_name, "' was negative. Reporting as NA."), call. = FALSE)
      evidence_ratio_string <- "NA"
    }
    else {
      # --- MODIFIED: Use helper function for evidence ratio ---
      evidence_ratio_string <- paste0("= ", format_value(evidence_ratio))
    }
  } else {
    warning(paste("Directional hypothesis result for '", coefficient_name, "' was NULL or had unexpected format."), call. = FALSE)
  }
  
  
  # --- Savage-Dickey Ratio for H0: b = 0 (BF10) ---
  bf10_string <- "NA" # Default to NA
  hypothesis_formula_sd <- paste0(coefficient_name, " = 0")
  
  hyp_result_sd <- tryCatch({
    brms::hypothesis(model, hypothesis_formula_sd)
  }, error = function(e) {
    warning(paste("Could not compute Savage-Dickey hypothesis (", hypothesis_formula_sd, ") for '",
                  coefficient_name, "': ", e$message,
                  "\n  -> This often happens if priors are not set explicitly, are improper, or not defined at 0.", sep=""), call. = FALSE)
    return(NULL)
  })
  
  if (!is.null(hyp_result_sd) && inherits(hyp_result_sd, "brmshypothesis") &&
      !is.null(hyp_result_sd$hypothesis) &&
      "Evid.Ratio" %in% names(hyp_result_sd$hypothesis))
  {
    bf01_sd_raw <- hyp_result_sd$hypothesis$Evid.Ratio
    
    if (is.na(bf01_sd_raw) || !is.finite(bf01_sd_raw)) {
      bf10_string <- "NA"
      warning(paste("Savage-Dickey BF01 for '", coefficient_name, "' was NA or non-finite. Check prior specification."), call. = FALSE)
    } else {
      bf01_sd_rounded <- round(bf01_sd_raw, digits = 10)
      
      if (bf01_sd_rounded < 0) {
        bf10_string <- "NA"
        warning(paste("Savage-Dickey BF01 for '", coefficient_name, "' was negative. Cannot calculate BF10."), call. = FALSE)
      } else if (bf01_sd_rounded == 0) {
        bf10_string <- "> 1000"
      } else {
        bf10_sd <- 1 / bf01_sd_rounded
        
        if (bf10_sd > 1000) {
          bf10_string <- "> 1000"
        } else if (bf10_sd < 0.001) {
          bf10_string <- "< 0.001"
        } else {
          # --- MODIFIED: Use helper function for BF10 ---
          bf10_string <- paste0("= ", format_value(bf10_sd))
        }
      }
    }
  } else {
    # Handle cases where the hypothesis object or its components are problematic
    if (!is.null(hyp_result_sd)) {
      warning(paste("Savage-Dickey hypothesis result for '", coefficient_name, "' had an unexpected format. Cannot calculate BF10."), call. = FALSE)
    }
    # Keep default bf10_string = "NA"
  }
  
  # --- Construct the final output string ---
  # Using a non-breaking space (~) in LaTeX for better spacing
  output_string <- paste0(
    "$b = ", coef_estimate_fmt, "$, ",
    "95% CrI $[", cri_lower_fmt, ",~", cri_upper_fmt, "]$, ",
    hypothesis_string, # Directional probability string (already includes $)
    ", $ER_{b ", direction_operator, " 0} ", evidence_ratio_string, "$, ",
    "$BF_{10} ", bf10_string, "$"
  )
  
  # A final gsub to clean up the string structure for a more readable output
  output_string <- gsub(", \\$,", ",", output_string)
  
  return(output_string)
}