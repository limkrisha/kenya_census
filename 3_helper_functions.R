standardize_sust_other <- function(x) {
  x_std <- x %>%
    str_to_lower() %>%                 # caps -> not an issue
    str_squish() %>%                   # remove double spaces
    str_replace_all("\\.+", ".") %>%   # multiple dots -> one dot
    str_replace_all("\\s*\\./\\s*", "/") %>%  # tidy " ./ " if it appears
    str_replace_all("\\s+", " ")       # final cleanup
  
  case_when(
    # Feeding/Food program -> one bucket
    str_detect(x_std, "feeding program|food program|food programs") ~ "Feeding/Food program",
    
    # Poultry keeping variants
    str_detect(x_std, "poultry") ~ "Poultry keeping",
    
    # Sheep rearing + bull fattening -> one bucket
    str_detect(x_std, "sheep rearing|bull fattening") ~ "Sheep/Bull rearing",
    
    # Hay/grass variants -> one bucket
    str_detect(x_std, "hay/grass|hay grass|hay farming|hay harvester|hay") ~ "Hay/grass (any)",
    
    # keep everything else (but re-title case nicely)
    TRUE ~ str_to_sentence(x_std)
  )
}



# ---------------------------------------
# Define sections
# ---------------------------------------
bg_vars <- c("hh_gender", "hh_size", "know_410","know_leadership")

school_vars <- c("edu_children_schoolage_any",
                 "edu_children_schoolage_num",
                 "edu_children_schoolage_attend")

school_vars2 <- c("edu_school_travel_time",
                 "edu_secondary_fee_pay", 
                 "edu_secondary_fee_pay_method",
                 "edu_aspire_level_clean",
                 "edu_aspire_likelihood")

sust_vars <- c("edu_primary_school_any",
               "sust_project_any",
               "sust_project_list",
               "sust_parent_involvement",
               "sust_school_iga_any",
               "sust_school_iga_list",
               "sust_school_facility_maint",
               "sust_support_need")

health_vars <- c("health_seek_first",
                 "health_travel_time",
                 "health_chp_visit_monthly",
                 "health_chp_econ_beneficiary",
                 "health_sick_3mo",
                 "health_sick_treat_seek",
                 "health_program_aware",
                 "health_program_list")

water_vars <- c("water_source_drinking_list",
                "water_source_410",
                "water_payment",
                "water_collect_location",
                "water_fetch_time",
                "water_shortage_12mo",
                "water_rain_harvest_any",
                "water_rain_harvest_list",
                "water_treat_any",
                "water_treat_method",
                "water_wise_worry",
                "water_wise_plan_change",
                "water_wise_no_handwash",
                "water_wise_less_drink")

econ_vars <- c("econ_income_main",
               "econ_income_daily_avg",
               "econ_training_any",
               "econ_training_when",
               "econ_training_provider_clean",
               "econ_support_otherorg",
               "econ_support_otherorg_type",
               "econ_group_member",
               "econ_group_savings_method",
               "econ_group_registered",
               "econ_savings_any",
               "econ_savings_added_12mo",
               "econ_savings_location")

food_vars <- c("food_worry",
               "food_less_meals",
               "food_sleep_hungry")

faith_vars <- c("faith_bible_own",
                "faith_bible_want",
                "faith_church_any",
                "faith_church_who",
                "faith_church_freq",
                "faith_disciple_self",
                "faith_rel_god",
                "faith_rel_pastor",
                "faith_pastor_trust")

lead_vars <- c("lead_leader_list",
               "lead_leader_trust",
               "lead_collab_any",
               "lead_collab_org_list",
               "lead_collab_area",
               "lead_collab_rate",
               "lead_influence_personal",
               "lead_ability_address_problem",
               "lead_involved_any",
               "lead_involved_list",
               "lead_initiative_most_impact",
               "lead_initiative_least_impact",
               "involve_410",
               "involve_410_programs")

youth_vars <- c("youth_group_any",
                "youth_group_desc",
                "youth_training_any",
                "youth_training_type",
                "youth_involve_rate",
                "youth_challenges_list",
                "youth_opportunity",
                "youth_skill_need",
                "youth_church_role",
                "youth_volunteer_any",
                "youth_volunteer_examples")

wb_vars <- c("wb_happy",
             "wb_hopeful")

# helper: keep only labels relevant to vars you are summarizing
subset_labels <- function(label_list, vars) {
  label_list[
    sapply(label_list, function(f) all.vars(f[[2]])[1] %in% vars)
  ]
}

# main helper: build tbl_summary for any "section" of variables
make_tbl_all <- function(df, vars, cont = NULL, by = NULL, overall_first = FALSE, labels = NULL) {
  vars <- intersect(vars, names(df))  # safety: drop missing vars
  
  if (is.null(labels)) labels <- subset_labels(var_labels, vars)
  
  tbl <- df %>%
    tbl_summary(
      by = if (is.null(by)) NULL else df[[by]],
      include = all_of(vars),
      type = if (is.null(cont)) NULL else as.list(stats::setNames(list("continuous"), cont)),
      statistic = list(
        all_categorical() ~ "{n} ({p}%)",
        all_continuous()  ~ "{mean} (SD {sd})"
      ),
      digits = all_continuous() ~ 1,
      percent = if (is.null(by)) NULL else "column",
      missing = "no",
      label = labels
    )
  
  if (!is.null(by)) {
    tbl <- tbl %>%
      add_overall(last = !overall_first) %>%
      modify_footnote(everything() ~ NA)
  }
  
  tbl
}
# 
# # make tbl_summary disaggregated by community (Overall first)
# make_tbl_by_comm <- function(df, vars, cont = NULL, label_list = var_labels) {
#   vars <- intersect(vars, names(df))                 # safety
#   lbls <- subset_labels(label_list, vars)
#   
#   type_list <- NULL
#   if (!is.null(cont)) {
#     cont <- intersect(cont, vars)
#     type_list <- as.list(stats::setNames(rep(list("continuous"), length(cont)), cont))
#   }
#   
#   df %>%
#     tbl_summary(
#       by = geo_community,
#       include = all_of(vars),
#       type = type_list,
#       statistic = list(
#         all_categorical() ~ "{n} ({p}%)",
#         all_continuous()  ~ "{mean} (SD {sd})"
#       ),
#       percent = "column",
#       digits = all_continuous() ~ 1,
#       missing = "no",
#       label = lbls
#     ) %>%
#     add_overall(last = FALSE) %>%       # Overall first
#     modify_footnote(everything() ~ NA)  # remove superscripts
# }
# 
# make_tbl_by_comm <- function(df, vars, cont = NULL, label_list = var_labels) {
#   
#   vars <- intersect(vars, names(df))
#   lbls <- subset_labels(label_list, vars)
#   
#   type_list <- NULL
#   if (!is.null(cont)) {
#     cont <- intersect(cont, vars)
#     type_list <- as.list(stats::setNames(
#       rep(list("continuous"), length(cont)), cont))
#   }
#   
#   df %>%
#     tbl_summary(
#       by = geo_community,
#       include = all_of(vars),
#       type = type_list,
#       statistic = list(
#         all_categorical() ~ "{n} ({p}%)",
#         all_continuous()  ~ "{mean} (SD {sd})"
#       ),
#       percent = "column",
#       digits = all_continuous() ~ 1,
#       missing = "ifany",
#       missing_text = "Missing",
#       label = lbls
#     ) %>%
#     add_overall(last = FALSE) %>%
#     modify_footnote(everything() ~ NA)
# }
# 
# subset_labels <- function(label_list, vars) {
#   label_list[
#     sapply(label_list, function(f) all.vars(f[[2]])[1] %in% vars)
#   ]
# }

make_tbl_by_comm <- function(df, vars, cont = NULL, label_list = var_labels) {
  
  vars <- intersect(vars, names(df))
  lbls <- subset_labels(label_list, vars)
  
  type_list <- list(all_dichotomous() ~ "categorical")  # <-- add this
  
  if (!is.null(cont)) {
    cont <- intersect(cont, vars)
    type_list <- c(
      type_list,
      as.list(stats::setNames(rep(list("continuous"), length(cont)), cont))
    )
  }
  
  df %>%
    tbl_summary(
      by = geo_community,
      include = all_of(vars),
      type = type_list,
      statistic = list(
        all_categorical() ~ "{n} ({p}%)",
        all_continuous()  ~ "{mean} (SD {sd})"
      ),
      percent = "column",
      digits = all_continuous() ~ 1,
      missing = "no",
    #  missing_text = "Missing",
      label = lbls
    ) %>%
    add_n(col_label = "**N responses**")    %>%
    add_overall(last = FALSE) %>%
    modify_footnote(everything() ~ NA) %>%
    bold_labels()
}

strip_label_prefix <- function(x) {
  # removes anything like "Something: " at the start
  stringr::str_remove(x, "^[^:]{1,60}:\\s*")
}

make_multi_tbl_by_comm <- function(df, prefix, label_list = var_labels) {
  vars <- names(df) %>% stringr::str_subset(paste0("^", prefix, "__"))
  
  # put "__other" at the bottom
  other_vars <- vars[stringr::str_detect(vars, "__other$")]
  vars_main  <- setdiff(vars, other_vars)
  vars <- c(vars_main, other_vars)
  
  lbls <- subset_labels(label_list, vars)
  
  df2 <- df %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(vars), ~ !is.na(.x)))
  
  df2 %>%
    gtsummary::tbl_summary(
      by = geo_community,
      include = dplyr::all_of(vars),
      statistic = gtsummary::all_categorical() ~ "{n} ({p}%)",
      percent = "column",
      missing = "no",
      label = lbls
    ) %>%
    gtsummary::add_overall(last = FALSE) %>%
    gtsummary::modify_footnote(gtsummary::everything() ~ NA) %>%
    gtsummary::modify_table_body(~ .x %>%
                                   dplyr::mutate(label = strip_label_prefix(label))
    )
}


# Other table formatting
library(dplyr)
library(gt)



make_other_table <- function(other_tables, var, title = "Other responses") {
  
  tbl <- other_tables[[var]] %>%
    filter(!is.na(other_text), other_text != "") %>%
    arrange(desc(n)) %>%
    rename(Response = other_text,
           Count = n)
  
  if (nrow(tbl) == 0) return(NULL)
  
  tbl %>%
    gt() %>%
    tab_header(
      title = title
    ) %>%
    fmt_number(columns = Count, decimals = 0) %>%
    cols_align("left", columns = Response) %>%
    cols_align("center", columns = Count)
}

