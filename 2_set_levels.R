# ================================
# 0) FIX LEVEL DEFINITIONS 
# ================================
yn_levels <- c("Yes", "No")
gender_levels <- c("Male", "Female", "Other")
yn_school_levels <- c("Yes", "No", "Not relevant - no child in secondary school")
yn_chp <- c("Yes", "No", "Not relevant to respondent (Not a CHP)")
yn_dk_levels <- c("Yes", "No", "I don't know")
yn_dk_levels_2 <- c("Yes", "No", "Don't know")
fee_levels <- c("Partially", "Fully")

educ_official <- c("Primary school", "Middle school","Secondary school", "University", "Technical school")
likelihood_levels <- c("Very unlikely","Somewhat unlikely","Somewhat likely","Very likely")
travel_time_levels <- c("Less than 30 minutes","30-60 minutes","1-2 hours","More than 2 hours", "Don't know")
health_travel_time_levels <- c("Less than 30 minutes","30 to 60 minutes","1 to 2 hours","More than 2 hours", "Don't know")


sust_involvement_levels <- c("No involvement","Low involvement","Medium involvement","High involvement")
maint_levels <- c("Very poorly maintained","Somewhat poorly maintained","Somewhat well maintained","Very well maintained")
shortage_levels <- c("Never","Occasionally","Often","Very often")

wise_levels <- c("Never (0 days)","Rarely (1-2 days)","Sometimes (3-10 days)","Often (11-20 days)","Always (more than 20 days)")
fies_levels <- c("Never","Seldom (once or twice in the last 4 weeks)","Sometimes (3-10x in the last 4 weeks)","Often (more than 10x in the last 4 weeks)","Always")

trust_levels <- c("Never","Seldom","Sometimes","Often","Always")
agree_levels <- c("Strongly disagree","Disagree","Agree","Strongly agree")

influence_levels <- c("A lot","Some","A little","None")
collab_levels <- c("Strong","Moderate","Weak","None")

church_freq_levels <- c("Weekly","Occasionally","Rarely","Never","Don't know")  # fixed curly apostrophe
water_purchased_levels <- c("Free", "Purchased", "Both")
water_location <- c("In own property", "Elsewhere")
water_time_levels <- c("Less than 30 minutes", "More than 30 minutes", "Household does not collect water", "Don't know")
water_safer_levels <- c("Yes", "No", "Not necessary - water is already safe to drink at source")

# FIXED: you had a broken vector with "<"
water_harvest_levels <- c("Roof gutter with tank", "Farm pond", "Water dam", "Buckets and jerricans")

income_levels <- c("Less than 299", "300-499", "500-799", "Over 800")
training_levels <- c("In the past 12 months", "More than 12 months ago")
rel_levels <- c("Growing stronger", "About the same", "Weaker than before", "Not interested / do not have relationship")
rel_church_levels <- c("Very good", "Good", "Fair", "Poor", "Not applicable (do not attend church / do not have a church leader)")

address_level <- c(
  "I can address it on my own",
  "I can help address it with others in my community",
  "I can help address it with support from outside the community",
  "I am not able to address it"
)

youth_level <- c("Yes", "No", "Not sure")
youth_involve_level <- c("No involvement", "Low involvement", "Moderate involvement", "High involvement")

# FIXED: you had <= which breaks R
fbo_support_level <- c("No support", "Limited support", "Moderate support", "Strong support")


# ================================
# 1) OFFICIAL LISTS (select-multi)
# ================================
school_skip_official <- c("Cost", "Distance", "Illness", "Lack of Interest")

sust_official <- c("School garden", "Tree planting", "Livestock/animal rearing", "Water harvesting", "Solar power", "Waste Management")
iga_official <- c("Farming", "Livestock keeping", "School shop", "Craft making")
support_needed_official <- c("Financial support", "Training", "Materials/Equipment", "Community involvement")

water_official <- c("Community well", "Borehole", "River", "Rainwater", "Purchased bottled water")
purify_levels_official <- c("Boil", "Use water filter", "Solar disinfection", "Water guard", "Reverse osmosis")

training_provider_official <- c("NGO", "Government")

income_official <- c(
  "Farming (crops and/or livestock)",
  "Own business / self-employment (non-farm)",
  "Salaried employment (formal or informal)",
  "Casual work",
  "Remittances from family and friends",
  "Pension or social assistance (e.g., government)"
)

support_received_official <- c("Training", "Inputs (seeds, livestock, tools)", "Cash", "Equipment")
savings_official <- c("Mobile money", "Bank", "SACCO", "Savings group")

church_att_official <- c("Adult men", "Adult women", "Children")

leaders_official <- c("Government", "CBO / LC", "Church", "None")

collab_official <- c("Government agency", "NGO", "Church", "Private sector")
collab_area_official <- c("Education", "Health", "Water", "Economic Development", "Discipleship")

initiative_area_official <- c(
  "Education", "Health", "Water", "Economic Development", "Discipleship",
  "None / Not sure",
  "Not relevant - no community initiatives"
)

treatment_levels_official <- c("Hospital", "Chemist", "Traditional healer", "None")

programs_official <- c("Access to clean water", "Livestock", "Child sponsorship", "Health programming", "Farming programming", "Business startup programming", "Savings group")
youth_prog_official <- c("Leadership", "Business", "Farming", "Discipleship", "Vocational skills")
youth_chall_official <- c("Unemployment", "Drug abuse", "Lack of education", "Early marriage", "Limited opportunities")
youth_skills_official <- c("Business and entrepreneurship", "Agriculture", "Technical / vocational", "Leadership", "ICT / computer skills")  # fixed TEchnical


# ==========================================
# 2) MAPPING TO THE LEVELS
# ==========================================

# ---- Select one (fixed levels) ----
spec_select_one <- list(
  hh_gender = gender_levels,
  know_410 = yn_levels,
  know_leadership = yn_levels,
  
  edu_children_schoolage_any = yn_levels,
  edu_children_schoolage_attend = yn_levels,
  edu_school_travel_time = travel_time_levels,
  edu_secondary_fee_pay = yn_school_levels,
  edu_secondary_fee_pay_method = fee_levels,
  edu_aspire_likelihood = likelihood_levels,
  edu_primary_school_any = yn_levels,
  
  sust_project_any = yn_dk_levels,
  sust_parent_involvement = sust_involvement_levels,
  sust_school_iga_any = yn_dk_levels_2,
  sust_school_facility_maint = maint_levels,
  
  health_travel_time = health_travel_time_levels,
  health_chp_visit_monthly = yn_dk_levels_2,
  health_chp_econ_beneficiary = yn_chp,
  health_sick_3mo = yn_dk_levels_2,
  health_sick_treat_seek = yn_levels,
  health_program_aware = yn_dk_levels_2,
  
  water_source_410 = yn_dk_levels_2,
  water_payment = water_purchased_levels,
  water_collect_location = water_location,
  water_fetch_time = water_time_levels,
  water_shortage_12mo = shortage_levels,
  water_rain_harvest_any = yn_levels,
  water_treat_any = water_safer_levels,
  
  water_wise_worry = wise_levels,
  water_wise_plan_change = wise_levels,
  water_wise_no_handwash = wise_levels,
  water_wise_less_drink = wise_levels,
  
  econ_income_daily_avg = income_levels,    
  econ_training_any = yn_levels,
  econ_training_when = training_levels,
  econ_support_otherorg = yn_levels,
  econ_group_member = yn_levels,
  econ_group_savings_method = yn_dk_levels_2,
  econ_group_registered = yn_dk_levels_2,
  econ_savings_any = yn_dk_levels_2,
  econ_savings_added_12mo = yn_levels,
  
  food_worry = fies_levels,
  food_less_meals = fies_levels,
  food_sleep_hungry = fies_levels,
  
  faith_bible_own = yn_levels,
  faith_bible_want = yn_levels,
  faith_church_any = yn_dk_levels,
  faith_church_freq = church_freq_levels,
  faith_disciple_self = yn_dk_levels,
  faith_rel_god = rel_levels,
  faith_rel_pastor = rel_church_levels,
  faith_pastor_trust = trust_levels,
  
  lead_leader_trust = trust_levels,
  lead_collab_any = yn_dk_levels,
  lead_collab_rate = collab_levels,
  lead_ability_address_problem = address_level,
  lead_involved_any = yn_levels,
  
  involve_410 = yn_levels,
  
  youth_group_any = youth_level,
  youth_training_any = yn_levels,
  youth_involve_rate = youth_involve_level,
  youth_opportunity = agree_levels,
  youth_church_role = fbo_support_level,
  youth_volunteer_any = yn_levels
)

# ---- Select one WITH Other ----
# you want: *_clean (factor with Other) + *_other (typed)
spec_select_one_other <- list(
  edu_aspire_level = educ_official,
  econ_income_main = income_official,
  lead_initiative_most_impact = initiative_area_official,
  lead_initiative_least_impact = initiative_area_official,
  health_seek_first = treatment_levels_official
)

# ---- Select multiple (no Other) ----
spec_select_multiple <- list(
  water_rain_harvest_list = water_harvest_levels,
  faith_church_who = church_att_official
)

# ---- Select multiple WITH Other ----
spec_select_multiple_other <- list(
  edu_children_schoolage_not_attend_reason = school_skip_official,
  sust_project_list = sust_official,
  sust_school_iga_list = iga_official,
  sust_support_need = support_needed_official,
  water_source_drinking_list = water_official,
  water_treat_method = purify_levels_official,
  econ_training_provider = training_provider_official, 
  econ_support_otherorg_type = support_received_official,
  econ_savings_location = savings_official,
  lead_leader_list = leaders_official,
  lead_collab_org_list = collab_official,
  lead_collab_area = collab_area_official,
  lead_involved_list = collab_area_official,
  involve_410_programs = programs_official,
  youth_training_type = youth_prog_official,
  youth_challenges_list = youth_chall_official,
  youth_skill_need = youth_skills_official
)

# ---- Numeric ----
spec_numeric <- c(
  "hh_size",
  "edu_children_schoolage_num",
  "wb_happy",
  "wb_hopeful"
)

# ---- Text ----
spec_text <- c(
  "health_program_list",
  "youth_group_desc",
  "youth_volunteer_examples"
)




# -----------------------------
# 2) Make dummy columns readable (0/1 -> No/Yes) for ALL select-multiple dummies
#    (anything with "__" in name is likely a dummy column from pivot_wider)
# -----------------------------
dummy_vars <- names(clean) %>%
  stringr::str_subset("__") %>%
  setdiff(c("timestamp"))  # keep id untouched

clean <- clean %>%
  mutate(across(all_of(dummy_vars), ~ factor(if_else(.x == 1, "Yes", "No"),
                                             levels = c("No", "Yes"))))



# ==========================================
# 3) APPLY THESE SPECS (simple)
# ==========================================
library(readr)

# 3A) Basic trimming for all character fields
clean <- clean %>% mutate(across(where(is.character), ~ str_trim(.x)))

# 3B) Numeric coercion (keeps non-numeric as NA)
for (v in spec_numeric) {
  if (v %in% names(clean)) {
    clean[[v]] <- parse_number(as.character(clean[[v]]))
  }
}

# 3C) Select-one factoring
for (v in names(spec_select_one)) {
  if (v %in% names(clean)) {
    clean[[v]] <- factor(as.character(clean[[v]]), levels = spec_select_one[[v]], ordered = TRUE)
  }
}

# 3D) Select-one with Other: create *_clean and *_other
# for (v in names(spec_select_one_other)) {
#   if (v %in% names(clean)) {
#     allowed <- spec_select_one_other[[v]]
#     x <- as.character(clean[[v]])
#     
#     clean[[paste0(v, "_clean")]] <- ifelse(x %in% allowed, x, "Other")
#     clean[[paste0(v, "_other")]] <- ifelse(x %in% allowed, NA_character_, x)
#     
#     clean[[paste0(v, "_clean")]] <- factor(
#       clean[[paste0(v, "_clean")]],
#       levels = c(allowed, "Other"),
#       ordered = TRUE
#     )
#   }
# }

for (v in names(spec_select_one_other)) {
  if (v %in% names(clean)) {
    allowed <- spec_select_one_other[[v]]
    x <- stringr::str_squish(as.character(clean[[v]]))
    x[x == ""] <- NA_character_
    
    clean_name  <- paste0(v, "_clean")
    other_name  <- paste0(v, "_other")
    
    # 1) Keep NA as NA (don’t convert to "Other")
    clean[[clean_name]] <- dplyr::case_when(
      is.na(x)        ~ NA_character_,
      x %in% allowed  ~ x,
      TRUE            ~ "Other"
    )
    
    # 2) Only store typed text when it truly is Other (not NA)
    clean[[other_name]] <- dplyr::case_when(
      is.na(x)       ~ NA_character_,
      x %in% allowed ~ NA_character_,
      TRUE           ~ x
    )
    
    clean[[clean_name]] <- factor(
      clean[[clean_name]],
      levels = c(allowed, "Other"),
      ordered = TRUE
    )
  }
}

# 3E) Select-multiple (no Other): split ; and create dummies
# NOTE: uses timestamp as join key; change if needed.
make_multi_dummies <- function(df, var, official = NULL, prefix = var) {
  if (!all(c("timestamp", var) %in% names(df))) return(df)
  
  long <- df %>%
    select(timestamp, !!sym(var)) %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
    separate_rows(!!sym(var), sep = ";") %>%
    mutate(choice = str_trim(as.character(.data[[var]])))
  
  if (!is.null(official)) {
    long <- long %>% filter(choice %in% official)
  }
  
  wide <- long %>%
    mutate(val = 1L) %>%
    distinct(timestamp, choice, .keep_all = TRUE) %>%
    select(timestamp, choice, val) %>%
    pivot_wider(names_from = choice, values_from = val, values_fill = 0) %>%
    rename_with(~ paste0(prefix, "__", janitor::make_clean_names(.x)), -timestamp)
  
  df %>% left_join(wide, by = "timestamp")
}

for (v in names(spec_select_multiple)) {
  clean <- make_multi_dummies(clean, v, official = spec_select_multiple[[v]], prefix = v)
}

# 3F) Select-multiple WITH Other: dummies + “Other text” frequency tables
# This creates dummies including "...__other" and prints a frequency table of other typed values.
make_multi_other <- function(df, var, official, prefix = var) {
  if (!all(c("timestamp", var) %in% names(df))) return(list(df = df, other_freq = NULL, other_long = NULL))
  
  long <- df %>%
    dplyr::select(timestamp, geo_community, !!rlang::sym(var)) %>%   # <-- add geo_community
    dplyr::filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
    tidyr::separate_rows(!!rlang::sym(var), sep = ";") %>%
    dplyr::mutate(raw = stringr::str_trim(as.character(.data[[var]]))) %>%
    dplyr::mutate(
      choice = dplyr::if_else(raw %in% official, raw, "Other"),
      other_text = dplyr::if_else(raw %in% official, NA_character_, raw)
    )
  
  other_long <- long %>%
    dplyr::filter(choice == "Other", !is.na(other_text), other_text != "") %>%
    dplyr::select(timestamp, geo_community, other_text) %>%
    dplyr::mutate(n = 1L)
  
  other_freq <- other_long %>%
    dplyr::count(other_text, sort = TRUE)
  
  wide <- long %>%
    dplyr::mutate(val = 1L) %>%
    dplyr::distinct(timestamp, choice, .keep_all = TRUE) %>%
    dplyr::select(timestamp, choice, val) %>%
    tidyr::pivot_wider(names_from = choice, values_from = val, values_fill = 0) %>%
    dplyr::rename_with(~ paste0(prefix, "__", janitor::make_clean_names(.x)), -timestamp)
  
  out <- df %>% dplyr::left_join(wide, by = "timestamp")
  list(df = out, other_freq = other_freq, other_long = other_long)
}

other_tables <- list()
other_tables_long <- list()

for (v in names(spec_select_multiple_other)) {
  res <- make_multi_other(clean, v, official = spec_select_multiple_other[[v]], prefix = v)
  clean <- res$df
  other_tables[[v]] <- res$other_freq
  other_tables_long[[v]] <- res$other_long
}

# ==========================================
# 4) RESULTS
# ==========================================
# - clean now has:
#   * factored single-selects
#   * *_clean + *_other for select-one-with-other
#   * dummy columns for select-multiple fields
# - other_tables contains the “Other typed text” frequency tables for each select-multiple-with-other variable

# Example: view "Other" values typed for sustainability projects:
# other_tables[["sust_project_list"]]




# --------------------------
# Var labels in Table
# --------------------------
var_labels <- list(
  hh_gender ~ "Gender",
  hh_size ~ "Household Size (include both adults and children)",
  know_410 ~ "Have you heard of 410 Bridge?",
  know_leadership ~ "Do you know about the CBO/Leadership Council?",
  edu_children_schoolage_any ~ "Do you have any school-aged children in your household?",
  edu_children_schoolage_num ~ "How many school-aged children are in your household?",
  edu_children_schoolage_attend ~ "Are all school-aged children attending school full-time?",
  edu_children_schoolage_not_attend_reason__cost ~ "Not attending reason: Cost",
  edu_children_schoolage_not_attend_reason__distance ~ "Not attending reason: Distance",
  edu_children_schoolage_not_attend_reason__illness ~ "Not attending reason: Illness",
  edu_children_schoolage_not_attend_reason__lack_of_interest ~ "Not attending reason: Lack of interest",
  edu_children_schoolage_not_attend_reason__other ~ "Not attending reason: Other",
  edu_school_travel_time ~ "Travel time to nearest school (one way)",
  edu_secondary_fee_pay ~ "Do you pay for secondary school fees?",
  edu_secondary_fee_pay_method ~ "How do you pay for secondary school fees?",
  edu_aspire_level_clean ~ "Education level you hope children can finish",
  edu_aspire_likelihood ~ "Likelihood children will reach that level",
  edu_primary_school_any ~ "Do you currently have any children attending primary school?",
  
  sust_project_any ~ "Ongoing sustainability projects in community/school?",
  sust_project_list__school_garden ~ "Sustainability project: School garden",
  sust_project_list__tree_planting ~ "Sustainability project: Tree planting",
  sust_project_list__livestock_animal_rearing ~ "Sustainability project: Livestock/animal rearing",
  sust_project_list__water_harvesting ~ "Sustainability project: Water harvesting",
  sust_project_list__solar_power ~ "Sustainability project: Solar power",
  sust_project_list__waste_management ~ "Sustainability project: Waste management",
  sust_project_list__other ~ "Sustainability project: Other",
  sust_parent_involvement ~ "Parent/community involvement in school sustainability projects",
  sust_school_iga_any ~ "Income-generating activities linked to school?",
  sust_school_iga_list__farming ~ "School IGA: Farming",
  sust_school_iga_list__livestock_keeping ~ "School IGA: Livestock keeping",
  sust_school_iga_list__school_shop ~ "School IGA: School shop",
  sust_school_iga_list__craft_making ~ "School IGA: Craft making",
  sust_school_iga_list__other ~ "School IGA: Other",
  sust_school_facility_maint ~ "How well school facilities are maintained",
  sust_support_need__financial_support ~ "Support needed: Financial support",
  sust_support_need__training ~ "Support needed: Training",
  sust_support_need__materials_equipment ~ "Support needed: Materials/Equipment",
  sust_support_need__community_involvement ~ "Support needed: Community involvement",
  sust_support_need__other ~ "Support needed: Other",
  
  health_seek_first ~ "Where do you seek treatment first?",
  health_travel_time ~ "Travel time to nearest health facility (one way)",
  health_chp_visit_monthly ~ "CHP visits monthly?",
  health_chp_econ_beneficiary ~ "If CHP, beneficiary of economic initiatives?",
  health_sick_3mo ~ "Anyone sick in past 3 months?",
  health_sick_treat_seek ~ "Did the sick member seek treatment?",
  health_program_aware ~ "Aware of health programs supported in community?",
  
  water_source_drinking_list__community_well ~ "Drinking water source: Community well",
  water_source_drinking_list__borehole ~ "Drinking water source: Borehole",
  water_source_drinking_list__river ~ "Drinking water source: River",
  water_source_drinking_list__rainwater ~ "Drinking water source: Rainwater",
  water_source_drinking_list__purchased_bottled_water ~ "Drinking water source: Purchased bottled water",
  water_source_drinking_list__other ~ "Drinking water source: Other",
  water_source_410 ~ "Any water source part of 410 Bridge project?",
  water_payment ~ "Is drinking water free, purchased, or both?",
  water_collect_location ~ "Where do you collect drinking water from?",
  water_fetch_time ~ "Time to fetch water (round trip)",
  water_shortage_12mo ~ "Water shortages in past 12 months",
  water_rain_harvest_any ~ "Harvest/store rainwater?",
  water_rain_harvest_list__roof_gutter_with_tank ~ "Rainwater method: Roof gutter with tank",
  water_rain_harvest_list__farm_pond ~ "Rainwater method: Farm pond",
  water_rain_harvest_list__water_dam ~ "Rainwater method: Water dam",
  water_rain_harvest_list__buckets_and_jerricans ~ "Rainwater method: Buckets and jerricans",
  water_treat_any ~ "Do anything to make water safer to drink?",
  water_treat_method__boil ~ "Water treatment: Boil",
  water_treat_method__use_water_filter ~ "Water treatment: Use water filter",
  water_treat_method__solar_disinfection ~ "Water treatment: Solar disinfection",
  water_treat_method__water_guard ~ "Water treatment: Water guard",
  water_treat_method__reverse_osmosis ~ "Water treatment: Reverse osmosis",
  water_treat_method__other ~ "Water treatment: Other",
  water_wise_worry ~ "Worried about enough water (last 4 weeks)",
  water_wise_plan_change ~ "Changed plans due to water (last 4 weeks)",
  water_wise_no_handwash ~ "Couldn’t wash hands due to water (last 4 weeks)",
  water_wise_less_drink ~ "Less drinking water than desired (last 4 weeks)",
  
  econ_income_main_clean ~ "Main source of income",
  econ_income_daily_avg ~ "Average daily income",
  econ_training_any ~ "Ever attended business/farmer training?",
  econ_training_when ~ "Most recent training timing",
  # econ_training_provider_clean ~ "Who provided training (clean)",
  econ_training_provider__ngo ~ "Training provider: NGO",
  econ_training_provider__government ~ "Training provider: Government",
  econ_training_provider__other ~ "Training provider: Other",
  econ_support_otherorg ~ "Received business/farming support from another org in past 12 months?",
  econ_support_otherorg_type__training ~ "Support received: Training",
  econ_support_otherorg_type__inputs_seeds_livestock_tools ~ "Support received: Inputs (seeds/livestock/tools)",
  econ_support_otherorg_type__cash ~ "Support received: Cash",
  econ_support_otherorg_type__equipment ~ "Support received: Equipment",
  econ_support_otherorg_type__other ~ "Support received: Other",
  econ_group_member ~ "Member of self-help/community group?",
  econ_group_savings_method ~ "Group practices savings group methodology?",
  econ_group_registered ~ "Group registered?",
  econ_savings_any ~ "Currently have savings?",
  econ_savings_added_12mo ~ "Added to savings in past 12 months?",
  econ_savings_location__mobile_money ~ "Savings location: Mobile money",
  econ_savings_location__bank ~ "Savings location: Bank",
  econ_savings_location__sacco ~ "Savings location: SACCO",
  econ_savings_location__savings_group ~ "Savings location: Savings group",
  econ_savings_location__other ~ "Savings location: Other",
  
  food_worry ~ "Food: Worried not enough food (last 4 weeks)",
  food_less_meals ~ "Food: Ate fewer meals (last 4 weeks)",
  food_sleep_hungry ~ "Food: Went to sleep hungry (last 4 weeks)",
  
  faith_bible_own ~ "Own a Bible?",
  faith_bible_want ~ "Would like to own a Bible?",
  faith_church_any ~ "Anyone in household attend church?",
  faith_church_who__adult_men ~ "Church attendance: Adult men",
  faith_church_who__adult_women ~ "Church attendance: Adult women",
  faith_church_who__children ~ "Church attendance: Children",
  faith_church_freq ~ "How often attend church?",
  faith_disciple_self ~ "Consider yourself a disciple of Jesus?",
  faith_rel_god ~ "Relationship with God (last few months)",
  faith_rel_pastor ~ "Relationship with pastor/church leader",
  faith_pastor_trust ~ "Pastors can be trusted?",
  
  lead_leader_list__government ~ "Main leaders: Government",
  lead_leader_list__cbo_lc ~ "Main leaders: CBO/Leadership Council",
  lead_leader_list__church ~ "Main leaders: Church",
  lead_leader_list__none ~ "Main leaders: None",
  lead_leader_list__other ~ "Main leaders: Other",
  lead_leader_trust ~ "Community leaders can be trusted?",
  lead_collab_any ~ "Collaborated with other organizations (past 3 years)?",
  lead_collab_org_list__government_agency ~ "Collaborated with: Government agency",
  lead_collab_org_list__ngo ~ "Collaborated with: NGO",
  lead_collab_org_list__church ~ "Collaborated with: Church",
  lead_collab_org_list__private_sector ~ "Collaborated with: Private sector",
  lead_collab_org_list__other ~ "Collaborated with: Other",
  lead_collab_area__education ~ "Collaboration area: Education",
  lead_collab_area__health ~ "Collaboration area: Health",
  lead_collab_area__water ~ "Collaboration area: Water",
  lead_collab_area__economic_development ~ "Collaboration area: Economic development",
  lead_collab_area__discipleship ~ "Collaboration area: Discipleship",
  lead_collab_area__other ~ "Collaboration area: Other",
  lead_collab_rate ~ "Rate collaboration",
  lead_influence_personal ~ "Personal influence to address community problems",
  lead_ability_address_problem ~ "Ability to address community problems",
  lead_involved_any ~ "Anyone involved in community initiatives/programs?",
  lead_involved_list__education ~ "Involved in initiatives: Education",
  lead_involved_list__health ~ "Involved in initiatives: Health",
  lead_involved_list__water ~ "Involved in initiatives: Water",
  lead_involved_list__economic_development ~ "Involved in initiatives: Economic development",
  lead_involved_list__discipleship ~ "Involved in initiatives: Discipleship",
  lead_involved_list__other ~ "Involved in initiatives: Other",
  lead_initiative_most_impact_clean ~ "Initiative with greatest positive impact",
  lead_initiative_least_impact_clean ~ "Initiative with least impact",
  
  involve_410 ~ "Any involvement with 410 Bridge programs?",
  involve_410_programs__access_to_clean_water ~ "410 programs involved in: Access to clean water",
  involve_410_programs__livestock ~ "410 programs involved in: Livestock",
  involve_410_programs__child_sponsorship ~ "410 programs involved in: Child sponsorship",
  involve_410_programs__health_programming ~ "410 programs involved in: Health programming",
  involve_410_programs__farming_programming ~ "410 programs involved in: Farming programming",
  involve_410_programs__business_startup_programming ~ "410 programs involved in: Business startup programming",
  involve_410_programs__savings_group ~ "410 programs involved in: Savings group",
  involve_410_programs__other ~ "410 programs involved in: Other",
  
  youth_group_any ~ "Active youth groups in community?",
  youth_training_any ~ "Anyone participated in youth training/mentorship (past year)?",
  youth_training_type__leadership ~ "Youth training: Leadership",
  youth_training_type__business ~ "Youth training: Business",
  youth_training_type__farming ~ "Youth training: Farming",
  youth_training_type__discipleship ~ "Youth training: Discipleship",
  youth_training_type__vocational_skills ~ "Youth training: Vocational skills",
  youth_training_type__other ~ "Youth training: Other",
  youth_involve_rate ~ "Youth involvement in decision-making",
  youth_challenges_list__unemployment ~ "Youth challenges: Unemployment",
  youth_challenges_list__drug_abuse ~ "Youth challenges: Drug abuse",
  youth_challenges_list__lack_of_education ~ "Youth challenges: Lack of education",
  youth_challenges_list__early_marriage ~ "Youth challenges: Early marriage",
  youth_challenges_list__limited_opportunities ~ "Youth challenges: Limited opportunities",
  youth_challenges_list__other ~ "Youth challenges: Other",
  youth_opportunity ~ "Youth have enough opportunities to contribute",
  youth_skill_need__business_and_entrepreneurship ~ "Youth skills needed: Business/entrepreneurship",
  youth_skill_need__agriculture ~ "Youth skills needed: Agriculture",
  youth_skill_need__technical_vocational ~ "Youth skills needed: Technical/vocational",
  youth_skill_need__leadership ~ "Youth skills needed: Leadership",
  youth_skill_need__ict_computer_skills ~ "Youth skills needed: ICT/computer skills",
  youth_skill_need__other ~ "Youth skills needed: Other",
  youth_church_role ~ "Role of church/FBOs in supporting youth development",
  youth_volunteer_any ~ "Youth participated in community service/volunteer (past 12 months)?",
  
  wb_happy ~ "Happiness (0–10)",
  wb_hopeful ~ "Hopefulness (0–10)"
)



# -----------------------------
# 3) Choose all variables to include in the summary
#    (exclude long free-text fields; we’ll show them separately)
# -----------------------------
text_vars <- intersect(
  c("health_program_list", "youth_group_desc", "youth_volunteer_examples", "edu_children_schoolage_not_attend_reason",
    "edu_aspire_level",
    "sust_project_list",
    "sust_school_iga_list",
    "sust_support_need",
    "water_source_drinking_list",
    "water_treat_method",
    "econ_income_main",
    "econ_training_provider",
    "econ_support_otherorg_type",
    "econ_savings_location",
    "lead_leader_list",
    "lead_collab_org_list",
    "lead_collab_area",
    "lead_involved_list",
    "lead_initiative_most_impact",
    "lead_initiative_least_impact",
    "involve_410_programs",
    "youth_training_type",
    "youth_challenges_list",
    "youth_skill_need"),
  names(clean)
)

include_vars <- setdiff(
  intersect(
    c(
      # core
      "hh_gender","hh_size","know_410","know_leadership",
      "edu_children_schoolage_any","edu_children_schoolage_num","edu_children_schoolage_attend",
      "edu_school_travel_time","edu_secondary_fee_pay","edu_secondary_fee_pay_method",
      "edu_aspire_level_clean","edu_aspire_likelihood","edu_primary_school_any",
      "sust_project_any","sust_parent_involvement","sust_school_iga_any","sust_school_facility_maint",
      "health_seek_first","health_travel_time","health_chp_visit_monthly","health_chp_econ_beneficiary",
      "health_sick_3mo","health_sick_treat_seek","health_program_aware",
      "water_source_410","water_payment","water_collect_location","water_fetch_time",
      "water_shortage_12mo","water_rain_harvest_any","water_treat_any",
      "water_wise_worry","water_wise_plan_change","water_wise_no_handwash","water_wise_less_drink",
      "econ_income_main_clean","econ_income_daily_avg","econ_training_any","econ_training_when",
      "econ_support_otherorg","econ_group_member","econ_group_savings_method","econ_group_registered",
      "econ_savings_any","econ_savings_added_12mo",
      "food_worry","food_less_meals","food_sleep_hungry",
      "faith_bible_own","faith_bible_want","faith_church_any","faith_church_freq",
      "faith_disciple_self","faith_rel_god","faith_rel_pastor","faith_pastor_trust",
      "lead_leader_trust","lead_collab_any","lead_collab_rate","lead_influence_personal",
      "lead_ability_address_problem","lead_involved_any",
      "lead_initiative_most_impact_clean","lead_initiative_least_impact_clean",
      "involve_410","youth_group_any","youth_training_any","youth_involve_rate",
      "youth_opportunity","youth_church_role","youth_volunteer_any",
      "wb_happy","wb_hopeful",
      # plus ALL dummy vars created from select-multiple
      dummy_vars
    ),
    names(clean)
  ),
  text_vars
)
