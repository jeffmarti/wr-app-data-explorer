# ==============================================================================
# prepare_app_data.R
# ------------------------------------------------------------------------------
# Reads the newest WRTS export and writes the compact file the app reads.
# The parsing is the same as the app's old load_wr_excel() (Print Version sheet,
# report date from cell H4, WRIA names joined, rows without a priority date
# dropped), just moved here so it runs once a week instead of at app startup.
#
# Input : newest pipeline/raw/WRTS_applications_*.xlsx
# Output: data/wr_data.rds   list(data, report_date)
#
# Run from the project root:  source("pipeline/prepare_app_data.R")
# REQUIRED PACKAGES: dplyr, lubridate, readxl
# ==============================================================================

library(dplyr)
library(lubridate)
library(readxl)

# ==============================================================================
# SETTINGS
# ==============================================================================

RAW_DIR  <- "pipeline/raw"
OUT_FILE <- "data/wr_data.rds"

# Safety check: refuse to overwrite the app data if the export looks broken.
# (~4,000 active applications as of October 2026.)
MIN_ROWS <- 2000

# ==============================================================================
# WRIA LOOKUP
# ==============================================================================

wria_lookup <- tibble::tribble(
  ~WRIA, ~WRIA_NAME,
  1, "NOOKSACK", 2, "SAN JUAN", 3, "LOWER SKAGIT-SAMISH",
  4, "UPPER SKAGIT", 5, "STILLAGUAMISH", 6, "ISLAND",
  7, "SNOHOMISH", 8, "CEDAR-SAMMAMISH", 9, "DUWAMISH-GREEN",
  10, "PUYALLUP-WHITE", 11, "NISQUALLY", 12, "CHAMBERS-CLOVER",
  13, "DESCHUTES", 14, "KENNEDY-GOLDSBOROUGH", 15, "KITSAP",
  16, "SKOKOMISH-DOSEWALLIPS", 17, "QUILCENE-SNOW", 18, "ELWHA-DUNGENESS",
  19, "LYRE-HOKO", 20, "SOLEDUCK-HOH", 21, "QUEETS-QUINAULT",
  22, "LOWER CHEHALIS", 23, "UPPER CHEHALIS", 24, "WILLAPA",
  25, "GRAYS-ELOKOMAN", 26, "COWLITZ", 27, "LEWIS",
  28, "SALMON-WASHOUGAL", 29, "WIND-WHITE SALMON", 30, "KLICKITAT",
  31, "ROCK-GLADE", 32, "WALLA WALLA", 33, "LOWER SNAKE",
  34, "PALOUSE", 35, "MIDDLE SNAKE", 36, "ESQUATZEL COULEE",
  37, "LOWER YAKIMA", 38, "NACHES", 39, "UPPER YAKIMA",
  40, "ALKALI-SQUILCHUCK", 41, "LOWER CRAB", 42, "GRAND COULEE",
  43, "UPPER CRAB-WILSON", 44, "MOSES COULEE", 45, "WENATCHEE",
  46, "ENTIAT", 47, "CHELAN", 48, "METHOW", 49, "OKANOGAN",
  50, "FOSTER", 51, "NESPELEM", 52, "SANPOIL",
  53, "LOWER LAKE ROOSEVELT", 54, "LOWER SPOKANE", 55, "LITTLE SPOKANE",
  56, "HANGMAN", 57, "MIDDLE SPOKANE", 58, "MIDDLE LAKE ROOSEVELT",
  59, "COLVILLE", 60, "KETTLE", 61, "UPPER LAKE ROOSEVELT",
  62, "PEND OREILLE"
)

# ==============================================================================
# READ THE EXPORT
# ==============================================================================

load_wr_excel <- function(path) {

  peek <- suppressMessages(suppressWarnings(
    read_excel(path, sheet = "Print Version",
               col_names = FALSE, n_max = 8,
               .name_repair = "minimal")
  ))
  report_date_raw <- tryCatch(
    as.character(peek[[8]][4]),
    error = function(e) NA_character_
  )
  report_date <- as.Date(
    gsub("Report Generated on ", "", report_date_raw),
    format = "%m/%d/%Y"
  )
  if (is.na(report_date)) {
    warning("Could not read 'Report Generated on' date (got '", report_date_raw,
            "'); using today's date.", call. = FALSE)
    report_date <- Sys.Date()
  }

  data <- suppressWarnings(
    read_excel(path, sheet = "Print Version", skip = 8)
  ) %>%
    rename_with(trimws) %>%
    mutate(
      WRIA         = suppressWarnings(as.numeric(WRIA)),
      PriorityDate = mdy(`Priority Date Claim First Use`)
    ) %>%
    select(-`Priority Date Claim First Use`) %>%
    left_join(wria_lookup, by = "WRIA") %>%
    mutate(
      WRIA_LABEL   = paste0(WRIA, " - ", WRIA_NAME),
      County_upper = trimws(toupper(County)),
      WRIA_LABEL   = ifelse(is.na(WRIA), "Unassigned", WRIA_LABEL)
    ) %>%
    filter(!is.na(PriorityDate))

  list(data = data, report_date = report_date)
}

# ==============================================================================
# RUN
# ==============================================================================

raw_files <- sort(list.files(RAW_DIR, pattern = "^WRTS_applications_\\d{8}\\.xlsx$",
                             full.names = TRUE))
if (length(raw_files) == 0) stop("No WRTS_applications_*.xlsx found in ", RAW_DIR)
in_file <- tail(raw_files, 1)

result <- load_wr_excel(in_file)
d      <- result$data

needed <- c("Record Document Number", "WR Doc ID", "Person or Organization", "Status",
            "Phase", "Stage", "Purpose of Use", "Qi", "UOM", "Qa", "WRIA", "County",
            "TRS", "QQ", "Q", "First Source Name Listed", "Certificate Number")
missing <- setdiff(needed, names(d))
if (length(missing) > 0) stop("Export is missing columns: ", paste(missing, collapse = ", "))
if (nrow(d) < MIN_ROWS) {
  stop("Only ", nrow(d), " records with a priority date (expected at least ", MIN_ROWS,
       "). Not overwriting ", OUT_FILE, ".")
}

dir.create(dirname(OUT_FILE), showWarnings = FALSE, recursive = TRUE)
saveRDS(result, OUT_FILE)

cat("\n=================== APP DATA SUMMARY ===================\n")
cat("Input file          :", basename(in_file), "\n")
cat("Report generated    :", format(result$report_date, "%B %d, %Y"), "\n")
cat("Records (with date) :", format(nrow(d), big.mark = ","), "\n")
print(table(Phase = d$Phase))
cat("No WRIA assigned    :", sum(is.na(d$WRIA)), "\n")
cat("Saved               :", OUT_FILE, "(", round(file.size(OUT_FILE) / 1024), "KB )\n")
cat("========================================================\n")
