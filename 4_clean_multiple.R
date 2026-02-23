# ============================================================
#  DIAGNOSIS & DATA PREP: "Other" Responses
#  Run this once to add clean _other_text columns to your CSV,
#  then the Shiny app can display them properly.
# ============================================================
#
#  THE SITUATION (from data inspection):
#
#  There are TWO different structures for "Other":
#
#  TYPE A — select_ONE with Other  (3 variables):
#    Column:  varname_other  (free text, e.g. "Dispensary", "Masters degree")
#    Present: edu_aspire_level_other, health_seek_first_other,
#             econ_income_main_other, lead_initiative_most_impact_other,
#             lead_initiative_least_impact_other
#    Status:  ✅ Already works in Shiny app (make_other_freq_table finds it)
#
#  TYPE B — select_MULTIPLE with Other  (17 variables):
#    Column:  varname__other  (0/1 FLAG only — no free text column!)
#    The free text is EMBEDDED in the raw semicolon-delimited column,
#    as extra items beyond the official answer options.
#    e.g. sust_project_list = "School garden;Poultry keeping"
#         sust_project_list__other = 1
#    Status:  ❌ App can't show "other" text because no _other_text column exists
#
#  TYPE C — open text  (2 variables):
#    youth_volunteer_examples, youth_group_desc
#    Status:  ❌ Not shown anywhere yet (needs word-frequency or listing)
#
#  SOLUTION:
#  This script extracts the "other" text for TYPE B variables
#  by taking the raw semicolon column, removing official options,
#  and saving the remainder as  varname_other_text  columns.
#  After running this, save the result as clean_forshiny_v2.csv
#  and point the Shiny app at that file.
# ============================================================
# 
# library(tidyverse)

# clean <- read.csv("clean_forshiny.csv", check.names = FALSE, stringsAsFactors = FALSE)
# names(clean) <- ifelse(is.na(names(clean)) | names(clean) == "",
#                     paste0("col_", seq_along(names(clean))), names(clean))

# ── Official option lists (lowercase for matching) ────────────
official_opts <- list(
  edu_children_schoolage_not_attend_reason =
    c("cost","distance","illness","lack of interest"),
  sust_project_list =
    c("school garden","tree planting","livestock/animal rearing",
      "water harvesting","solar power","waste management"),
  sust_school_iga_list =
    c("farming","livestock keeping","school shop","craft making"),
  sust_support_need =
    c("financial support","training","materials/equipment","community involvement"),
  water_source_drinking_list =
    c("community well","borehole","river","rainwater","purchased bottled water"),
  water_treat_method =
    c("boil","use water filter","solar disinfection","water guard","reverse osmosis"),
  econ_training_provider =
    c("ngo","government"),
  econ_support_otherorg_type =
    c("training","inputs (seeds, livestock, tools)","cash","equipment"),
  econ_savings_location =
    c("mobile money","bank","sacco","savings group"),
  lead_leader_list =
    c("government","cbo / lc","church","none"),
  lead_collab_org_list =
    c("government agency","ngo","church","private sector"),
  lead_collab_area =
    c("education","health","water","economic development","discipleship"),
  lead_involved_list =
    c("education","health","water","economic development","discipleship"),
  involve_410_programs =
    c("access to clean water","livestock","child sponsorship","health programming",
      "farming programming","business startup programming","savings group"),
  youth_training_type =
    c("leadership","business","farming","discipleship","vocational skills"),
  youth_challenges_list =
    c("unemployment","drug abuse","lack of education","early marriage",
      "limited opportunities"),
  youth_skill_need =
    c("business and entrepreneurship","agriculture",
      "technical / vocational","leadership","ict / computer skills",
      # also match common variations
      "business and entreprenuership","technical / vocational",
      "ict / computer skills","ict/ computer skills")
)

# ── Extract "Other" free text from TYPE B variables ───────────
extract_other_text <- function(raw_val, official) {
  if (is.na(raw_val) || trimws(raw_val) == "" || trimws(raw_val) == "NA") return(NA_character_)
  parts <- trimws(strsplit(raw_val, ";")[[1]])
  parts <- parts[nchar(parts) > 0]
  # Keep only parts NOT in the official list (case-insensitive)
  others <- parts[!tolower(parts) %in% official]
  if (length(others) == 0) return(NA_character_)
  paste(others, collapse = "; ")
}

cat("Extracting 'other' text for select-multiple variables...\n")
for (var in names(official_opts)) {
  if (!var %in% names(clean)) {
    cat(sprintf("  SKIP (column not found): %s\n", var))
    next
  }
  flag_col <- paste0(var, "__other")
  new_col  <- paste0(var, "_other_text")
  
  clean[[new_col]] <- NA_character_
  
  # Only process rows where the __other flag = 1
  if (flag_col %in% names(clean)) {
    flag_rows <- !is.na(clean[[flag_col]]) & clean[[flag_col]] == 1
  } else {
    # No flag column — process all rows (extract any non-official text)
    flag_rows <- rep(TRUE, nrow(clean))
  }
  
  clean[[new_col]][flag_rows] <- sapply(
    clean[[var]][flag_rows],
    extract_other_text,
    official = official_opts[[var]]
  )
  
  n_extracted <- sum(!is.na(clean[[new_col]]))
  cat(sprintf("  %-45s → %d rows with 'other' text  (new col: %s)\n",
              var, n_extracted, new_col))
}

# ── Quick frequency check ─────────────────────────────────────
cat("\n=== TOP 'OTHER' RESPONSES BY VARIABLE ===\n")
for (var in names(official_opts)) {
  new_col <- paste0(var, "_other_text")
  if (!new_col %in% names(clean)) next
  vals <- clean[[new_col]][!is.na(clean[[new_col]]) & clean[[new_col]] != ""]
  if (!length(vals)) next
  # Split on ";" to get individual items
  items <- unlist(strsplit(vals, ";\\s*"))
  items <- trimws(items[nchar(trimws(items)) > 0])
  top <- sort(table(items), decreasing = TRUE)[1:min(5, length(unique(items)))]
  cat(sprintf("\n%s (%d responses):\n", var, length(vals)))
  for (nm in names(top)) cat(sprintf("  %dx %s\n", top[[nm]], nm))
}

# ── Save enriched CSV ─────────────────────────────────────────
write.csv(clean, "clean_for_app.csv", row.names = FALSE)
cat("\n✅ Saved: clean_for_app.csv\n")
# cat("   Point your Shiny app at this file instead of clean_forshiny.csv\n")

# # ── Summary of what the app will now be able to show ──────────
# cat("\n=== SUMMARY: WHAT EACH VARIABLE WILL SHOW IN THE APP ===\n")
# cat("
# TYPE A — select_one with Other (free text already in _other column):
#   edu_aspire_level           → _other col: edu_aspire_level_other         ✅
#   health_seek_first          → _other col: health_seek_first_other        ✅
#   econ_income_main           → _other col: econ_income_main_other         ✅
#   lead_initiative_most_impact  → lead_initiative_most_impact_other        ✅
#   lead_initiative_least_impact → lead_initiative_least_impact_other       ✅
# 
# TYPE B — select_multiple with Other (text EXTRACTED from raw column):
#   All 17 variables above    → new _other_text cols created               ✅
# 
# TYPE C — open text (shown as a word-list / frequency table):
#   youth_volunteer_examples   → shown as top responses table
#   youth_group_desc           → shown as listing by community
# ")

