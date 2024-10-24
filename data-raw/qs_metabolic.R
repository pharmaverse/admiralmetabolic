#' Dataset: qs_metabolic
#' Description: Create QS test SDTM dataset for metabolic studies,
#' containing COEQ data (Control of Eating Questionnaire)
#' Note: the University of Leeds are the copyright holders of the CoEQ and the test data included within {admiralmetabolic}
#' is for not-for-profit use only within `{admiralmetabolic}` and `pharmaverse`-related examples/documentation. Any persons
#' or companies wanting to use the CoEQ should request a license to do so from the following
#' [link](https://licensing.leeds.ac.uk/product/control-of-eating-questionnaire-coeq).

# Load libraries ----

library(tibble)
library(dplyr)
library(pharmaversesdtm)
library(purrr)

# Set seed for random data generation ----
set.seed(3.14159265)

# Read input data ----

vs_metabolic <- admiralmetabolic::vs_metabolic

# Set up questions, codes and categories ----
coeq_structure <- tibble::tribble(
  ~QSCAT, ~QSSCAT, ~QSTESTCD, ~QSTEST,
  "COEQ", "EATING", "COEQ01", "How hungry have you felt?",
  "COEQ", "EATING", "COEQ02", "How full have you felt?",
  "COEQ", "EATING", "COEQ03", "How strong was your desire to eat sweet foods?",
  "COEQ", "EATING", "COEQ04", "How strong was your desire to eat savoury foods?",
  "COEQ", "EMOTIONS", "COEQ05", "How happy have you felt?",
  "COEQ", "EMOTIONS", "COEQ06", "How anxious have you felt?",
  "COEQ", "EMOTIONS", "COEQ07", "How alert have you felt?",
  "COEQ", "EMOTIONS", "COEQ08", "How contented have you felt?",
  "COEQ", "GENERAL CRAVINGS", "COEQ09", "During the last 7 days how often have you had food cravings?",
  "COEQ", "GENERAL CRAVINGS", "COEQ10", "How strong have any food cravings been?",
  "COEQ", "GENERAL CRAVINGS", "COEQ11", "How difficult has it been to resist any food cravings?",
  "COEQ", "GENERAL CRAVINGS", "COEQ12", "How often have you eaten in response to food cravings?",
  "COEQ", "GENERAL CRAVINGS", "COEQ13", "Chocolate or chocolate flavoured foods",
  "COEQ", "SPECIFIC CRAVINGS", "COEQ14", "Other sweet foods (cakes, pastries, biscuits, etc)",
  "COEQ", "SPECIFIC CRAVING", "COEQ15", "Fruit or fruit juice",
  "COEQ", "SPECIFIC CRAVING", "COEQ16", "Dairy foods (cheese, yoghurts, milk, etc)",
  "COEQ", "SPECIFIC CRAVING", "COEQ17", "Starchy foods (bread, rice, pasta, etc)",
  "COEQ", "SPECIFIC CRAVING", "COEQ18", "Savoury foods (french fries, crisps, burgers, pizza, etc)",
  "COEQ", "CONTROL EATING", "COEQ19", "Generally, how difficult has it been to control your eating?",
  "COEQ", "CONTROL EATING", "COEQ20", "Which one food makes it most difficult for you to control eating?",
  "COEQ", "CONTROL EATING", "COEQ21", "How difficult has it been to resist eating this food during the last 7 days?",
)

# Use visit schedule and days from VS ----
visit_schedule <- vs_metabolic %>%
  select(STUDYID, USUBJID, VISIT, VISITNUM, VISITDY, VSDTC, VSDY) %>%
  filter(!grepl("AMBUL|RETRIEVAL", VISIT)) %>%
  rename(QSDTC = VSDTC, QSDY = VSDY) %>%
  distinct()

# Cross join to get questions at each visit ----
qs_metabolic_shell <- visit_schedule %>%
  dplyr::cross_join(coeq_structure)

# Simulate question answers ----
qs_metabolic_results <- qs_metabolic_shell %>%
  mutate(
    DOMAIN = "QS",
    QSORRES = floor(runif(1) * 10) + round(runif(1)),
    QSORRESU = "cm",
    QSSTRESU = "cm",
    QSSTRESN = QSORRES,
    QSSTRESC = as.character(QSORRES),
    QSBLFL = ifelse(VISIT == "BASELINE", "Y", NA_character_)
  )

# Order variables, sort and add sequence number ----
qs_metabolic_seq <- qs_metabolic_results %>%
  select(
    STUDYID, USUBJID, DOMAIN, VISIT, VISITNUM, VISITDY, QSBLFL, QSDTC, QSDY,
    QSCAT, QSSCAT, QSTEST, QSTESTCD, QSORRES, QSORRESU, QSSTRESC, QSSTRESN, QSSTRESU
  ) %>%
  arrange(STUDYID, USUBJID, VISITNUM, QSTESTCD) %>%
  group_by(USUBJID) %>%
  mutate(QSSEQ = row_number()) %>%
  ungroup()

# Add labels to variables that don't have them yet ----
attr(qs_metabolic_seq$DOMAIN, "label") <- "Domain Abbreviation"
attr(qs_metabolic_seq$QSBLFL, "label") <- "Baseline Flag"
attr(qs_metabolic_seq$QSCAT, "label") <- "Category for Questionnaire"
attr(qs_metabolic_seq$QSSCAT, "label") <- "Subcategory for Questionnaire"
attr(qs_metabolic_seq$QSTEST, "label") <- "Questionnaire Test Name"
attr(qs_metabolic_seq$QSTESTCD, "label") <- "Questionnaire Test Short Name"
attr(qs_metabolic_seq$QSORRES, "label") <- "Result or Finding in Original Units"
attr(qs_metabolic_seq$QSORRESU, "label") <- "Original Units"
attr(qs_metabolic_seq$QSSTRESC, "label") <- "Character Result/Finding in Std Format"
attr(qs_metabolic_seq$QSSTRESN, "label") <- "Numeric Result/Finding in Standard Units"
attr(qs_metabolic_seq$QSSTRESU, "label") <- "Standard Units"
attr(qs_metabolic_seq$QSSEQ, "label") <- "Sequence Number"

# Label QS dataset ----
attr(qs_metabolic_seq, "label") <- "Questionnaire"

# Final dataset ----
qs_metabolic <- qs_metabolic_seq

# Save dataset ----
usethis::use_data(qs_metabolic, overwrite = TRUE)
