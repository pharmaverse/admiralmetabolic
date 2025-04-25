#' Compute Metabolic Scores
#'
#' This function computes various metabolic scores based on the specified mode.
#'
#' @param mode A character string specifying the metabolic score to compute.
#'   Possible values are "HSI", "FLI", "NAFLDFS", "HOMAIR".
#' @param alt Alanine Aminotransferase (ALT) value (U/L).
#'   A numeric vector is expected. Required for "HSI" and "NAFLDFS" modes.
#' @param ast Aspartate Aminotransferase (AST) value (U/L).
#'   A numeric vector is expected. Required for "HSI" and "NAFLDFS" modes.
#' @param bmi Body Mass Index (BMI) value (kg/m2).
#'   A numeric vector is expected. Required for "HSI", "FLI", and "NAFLDFS" modes.
#' @param t2dm Type 2 Diabetes Mellitus (T2DM) status ("Y" or "N").
#'   A character vector is expected. Required for "HSI" and "NAFLDFS" modes.
#' @param sex Sex ("M" or "F").
#'   A character vector is expected. Required for "HSI" mode.
#' @param triglycerides Triglycerides value (mg/dL).
#'   A numeric vector is expected. Required for "FLI" mode.
#' @param ggt Gamma Glutamyl Transferase (GGT) value (U/L).
#'   A numeric vector is expected. Required for "FLI" mode.
#' @param waist_circumference Waist Circumference value (cm).
#'   A numeric vector is expected. Required for "FLI" mode.
#' @param age Age value (years).
#'   A numeric vector is expected. Required for "NAFLDFS" mode.
#' @param platelets Platelets value (x 10^9/L).
#'   A numeric vector is expected. Required for "NAFLDFS" mode.
#' @param albumin Albumin value (g/dL).
#'   A numeric vector is expected. Required for "NAFLDFS" mode.
#' @param insulin Fasting plasma insulin value (mIU/L).
#'   A numeric vector is expected. Required for "HOMA-IR" mode.
#' @param glucose Fasting plasma glucose value (mmol/L).
#'   A numeric vector is expected. Required for "HOMA-IR" mode.
#'
#' @return A numeric vector of computed metabolic scores.
#' @export
#'
#' @examples
#' # Compute HSI
#' compute_metabolic_score(
#'   mode = "HSI",
#'   alt = c(30, 45), ast = c(25, 40), bmi = c(28, 32), t2dm = c("N", "Y"), sex = c("M", "F")
#' )
#'
#' # Compute FLI
#' compute_metabolic_score(
#'   mode = "FLI",
#'   triglycerides = c(150, 200), bmi = c(28, 32), ggt = c(40, 55), waist_circumference = c(90, 105)
#' )
#'
#' # Compute NAFLDFS
#' compute_metabolic_score(
#'   mode = "NAFLDFS",
#'   age = c(50, 65), bmi = c(28, 32), t2dm = c("N", "Y"),
#'   ast = c(30, 45), alt = c(25, 40), platelets = c(200, 150), albumin = c(4, 3.5)
#' )
#'
#' # Compute HOMA-IR
#' compute_metabolic_score(
#'   mode = "HOMAIR",
#'   insulin = c(10, 15), glucose = c(5, 6)
#' )
compute_metabolic_score <- function(
  mode,
  alt = NA_real_, ast = NA_real_, bmi = NA_real_, t2dm = NA_character_, sex = NA_character_,
  triglycerides = NA_real_, ggt = NA_real_, waist_circumference = NA_real_,
  age = NA_real_, platelets = NA_real_, albumin = NA_real_,
  insulin = NA_real_, glucose = NA_real_
) {
  assert_character_scalar(mode, values = c("HSI", "FLI", "NAFLDFS", "HOMAIR"))

  score <- case_when(
    mode == "HSI" ~ {
      assert_numeric_vector(alt)
      assert_numeric_vector(ast)
      assert_numeric_vector(bmi)
      assert_character_vector(t2dm, values = c("Y", "N", NA_character_))
      assert_character_vector(sex, values = c("M", "F", NA_character_))
      case_when(
        !is.na(alt) & !is.na(ast) & !is.na(bmi) ~
          8 * (alt / ast) + bmi + if_else(t2dm == "Y", 2, 0) + if_else(sex == "F", 2, 0),
        TRUE ~ NA_real_
      )
    },
    mode == "FLI" ~ {
      assert_numeric_vector(triglycerides)
      assert_numeric_vector(bmi)
      assert_numeric_vector(ggt)
      assert_numeric_vector(waist_circumference)
      case_when(
        !is.na(triglycerides) & !is.na(bmi) & !is.na(ggt) & !is.na(waist_circumference) ~ {
          lambda <- 0.953 * log(triglycerides) + 0.139 * bmi + 0.718 * log(ggt) +
            0.053 * waist_circumference - 15.745
          (exp(lambda) / (1 + exp(lambda))) * 100
        },
        TRUE ~ NA_real_
      )
    },
    mode == "NAFLDFS" ~ {
      assert_numeric_vector(age)
      assert_numeric_vector(bmi)
      assert_character_vector(t2dm, values = c("Y", "N", NA_character_))
      assert_numeric_vector(ast)
      assert_numeric_vector(alt)
      assert_numeric_vector(platelets)
      assert_numeric_vector(albumin)
      case_when(
        !is.na(age) & !is.na(bmi) & !is.na(ast) & !is.na(alt) &
          !is.na(platelets) & !is.na(albumin) ~
          -1.675 + 0.037 * age + 0.094 * bmi + if_else(t2dm == "Y", 1.13, 0) +
            0.99 * (ast / alt) - 0.013 * platelets - 0.66 * albumin,
        TRUE ~ NA_real_
      )
    },
    mode == "HOMAIR" ~ {
      assert_numeric_vector(insulin)
      assert_numeric_vector(glucose)
      case_when(
        !is.na(insulin) & !is.na(glucose) ~ (insulin * glucose) / 22.5,
        TRUE ~ NA_real_
      )
    },
    TRUE ~ NA_real_ # Should not happen due to assert_character_scalar
  )

  return(score)
}
