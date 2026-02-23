

# load dataset
clean <- read_csv(here("data", "20260223_kenya_selected_cols.csv"), show_col_types = F) %>% 
  select(-date)

source(here("code", "2_set_levels.R"))
source(here("code", "3_helper_functions.R"))

# invisible(
#   suppressMessages(
#     suppressWarnings(
#       source(here("code", "4_clean_multiple.R"))
#     )
#   )
# )
# 

capture.output(
  source(here("code", "4_clean_multiple.R"))
)

