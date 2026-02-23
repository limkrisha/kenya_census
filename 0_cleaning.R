pacman::p_load(dplyr, here, tidyverse, readr, readxl, janitor, gtsummary)

# source(here("3_code", "2_set_levels.R"))
# source(here("3_code", "3_recoding_other.R"))

##### Load raw data 
raw_new <- read_csv(here("data", "20260223 Kenya 2026 survey.csv"), show_col_types = FALSE)
raw_9 <- read_csv(here("data", "kenya_mutirithia.csv"), show_col_types = FALSE)

# Google Forms export the question as the var name
# So using setnames, rename the questions based on chosen varnames
var_map <- read_csv(here("data", "varname_map.csv"), show_col_types = FALSE)

# name vector: new_name = old_name
rename_vec <- setNames(var_map$question, var_map$varname)

raw_mapped <- raw_new %>%
  rename(any_of(rename_vec))        # renames only matches; ignores the rest safely

##### Merge with the 9 surveys from Mutirithia
raw <- rbind(raw_mapped, raw_9)




# Create village label variable 

df_clean <- raw %>%
  filter(consent == "Yes") %>%
  clean_names() %>%
  mutate(
    # normalize blanks
    across(starts_with("geo_"), ~ na_if(str_squish(as.character(.x)), ""))
  ) %>%
  # 1) Make a single village label column by coalescing the community-specific fields
  mutate(
    village_label = coalesce(
      geo_chembulet,
      geo_kahuria,
      geo_minyalala,
      geo_mugaa,
      geo_mutirithia,
      geo_ndibai,
      geo_ulu
    )
  ) 


df_clean <- df_clean %>%
  relocate(village_label, .after = geo_community)

# Clean enumerator names
df_clean <- df_clean %>%
  mutate(
    # basic cleanup first
    enumerator = str_trim(enumerator),
    enumerator = str_remove(enumerator, "\\.$"),   # remove trailing dot
    enumerator = str_to_title(enumerator),         # fix capitalization
    # manual corrections
    enumerator = case_when(
      enumerator %in% c("Edwin", "Edwin.") ~ "Edwin",
      enumerator %in% c("Felix", "Felix") ~ "Felix",
      enumerator %in% c("Fleix") ~ "Felix",
      enumerator %in% c("Hellen Kyalo", "Hellena Kyalo", "Hellena Kyalo","Hellen Kyalo") ~ "Hellen Kyalo",
      enumerator %in% c("Jotham","Jotham Mwebdwa","Jotham Mwendwa","Jotham Mwendwa") ~ "Jotham Mwendwa",
      enumerator %in% c("Vero", "Veronica") ~ "Veronica",
      TRUE ~ enumerator))


# Parse date
df_clean <- df_clean %>%
  mutate(
    survey_date = as.Date(substr(timestamp, 1, 10), format = "%Y/%m/%d")
  )
table(df_clean$survey_date)

# ---- 5) Pick Variables to Summarize ----
vars_to_summarize <- df_clean %>%
  select(-c(timestamp, enumerator, date, geo_chembulet, geo_kahuria, geo_minyalala, geo_mutirithia, geo_mugaa, geo_ndibai, geo_ulu, hh_name, hh_respondent_name)) %>% 
  names()

sel_cols <- df_clean %>%
  select(all_of(vars_to_summarize))

df_clean$sust_parent_involvement[df_clean$sust_parent_involvement == "Medium Involvement"] <- "Medium involvement"
df_clean$edu_aspire_level[df_clean$edu_aspire_level == "Technical School"] <- "Technical school"

write_csv(df_clean, here("data", paste0(format(Sys.Date(), "%Y%m%d"), "_kenya_selected_cols.csv")))

# write.csv(
#   sel_cols,
#   here("4_app", paste0(format(Sys.Date(), "%Y%m%d"), "_kenya_selected_cols.csv")),
#   row.names = FALSE
# )



