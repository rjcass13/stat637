library(dplyr)

dat_slc <- rio::import("slc_sample.xlsx")
dat_sf <- rio::import("sf_sample.xlsx")
# row bind the two datasets
dat <- dplyr::bind_rows(dat_slc, dat_sf)
# write.csv(dat, "mini_projects/proj_2/combined_data.csv", row.names = FALSE)


# Response Variable: (219) BLAKMOVE (group into would move, wouldn't move)
# 1) Would almost certainly move
# 2) Would probably move
# 3) Probably wouldn't move
# 4) Almost certainly wouldn't move
# Missing	53	
dat[["BLAKMOVE"]] <- ifelse(dat[["BLAKMOVE"]] == 1 | dat[["BLAKMOVE"]] == 2, 1, ifelse(dat[["BLAKMOVE"]] == 3 | dat[["BLAKMOVE"]] == 4, 0, NA))



## Reorganize covariates' answer choices to make it more continuous and interpretable: like can't decide, don't know, and uncertain to middle rather than the lowest or highest answer choice.

# (92): CURSCAIN: Black skin comes from curse of Cain
# 1) Definitely true
# 2) Probably true
# 3) Probably not true
# 4) Definitely not true
# 5) Don't know
# Missing	50	


# (95): PSTHNEG: God's will that priesthood is witheld
# 1) Definitely true
# 2) Probably true
# 3) Probably not true
# 4) Definitely not true
# 5) Don't know
# Missing	36	


# (110): DARWIN: Do you think we evolved from lower lifeforms
# 1) The theory is almost certainly true
# 2) The theory is probably true
# 3) The theory is probably false
# 4) The theory could not possibly be true
# 5) I don't really know or I haven't thought much about this before
# Missing	17


# (146): BLKDISCR: How serious of a sin is it to discriminate?
# 1) Very serious
# 2) Fairly serious
# 3) Not very serious
# 4) Scarcely matters at all
# 5) Can't decide
# Missing	57	


# (158): NAROWMND: How narrow minded do you think church members are?
# 1) Definitely agree
# 2) Agree somewhat
# 3) Disagree somewhat
# 4) Disagree strongly
# 5) Uncertain
# Missing	44	


# (163): CONSRVAT: Church is unduly conservative and doesn't change with times
# 1) Definitely agree
# 2) Agree somewhat
# 3) Disagree somewhat
# 4) Disagree strongly
# 5) Uncertain
# Missing	64	


# (165): SPKOUTSC: Leaders should speak out more against discrimination, war, etc.
# 1) Definitely agree
# 2) Agree somewhat
# 3) Disagree somewhat
# 4) Disagree strongly
# 5) Uncertain
# Missing	56


# (169): IGRSOCPB: True purpose of religion is to make social impact
# 1) Definitely agree
# 2) Agree somewhat
# 3) Disagree somewhat
# 4) Disagree strongly
# 5) Uncertain
# Missing	44

# Move choice 5 (don't know / can't decide / uncertain) to the midpoint:
# 1 -> 1, 2 -> 2, 5 -> 3, 3 -> 4, 4 -> 5 (NA stays NA)
mid_vars <- c("CURSCAIN", "PSTHNEG", "DARWIN", "BLKDISCR", "NAROWMND", "CONSRVAT", "SPKOUTSC", "IGRSOCPB")
for (v in mid_vars) {
  dat[[v]] <- c(1, 2, 4, 5, 3)[dat[[v]]]
}

# Keep only the response and the covariates listed above
keep_vars <- c("BLAKMOVE", "MISSION", "READSCRP", "PRVPRAYR", "GENDER", "AGE", "INCOME", "REGION", "CURSCAIN", "PSTHNEG", "DARWIN", "BLKDISCR", "NAROWMND", "CONSRVAT", "SPKOUTSC", "IGRSOCPB")
dat <- dat[, keep_vars]

dat <- dat %>% 
  mutate(across(c(BLAKMOVE, MISSION, READSCRP, PRVPRAYR, GENDER, REGION), as.factor))

# (6) MISSION: Did you serve
# 1) Yes, a full-time mission
# 2) Yes, a stake mission
# 3) Yes, both kinds of mission
# 4) No, no mission
# Missing	52	

dat$MISSION <- as.factor(ifelse(dat$MISSION == 1 | dat$MISSION == 3, 1, ifelse(dat$MISSION == 2 | dat$MISSION == 4, 0, NA)))

# (33): READSCRP: How often do you read scriptures (~10% missing)
# 1) Daily
# 2) Almost Every Day
# 3) At Least Once a Week
# 4) Once or Twice a Month
# 5) Several Times a Year
# 6) Rarely or Never
# Missing	135	


dat <- dat %>%
  mutate(READSCRP = factor(
    case_match(READSCRP,
      "1" ~ "Daily",
      "2" ~ "Almost Every Day",
      "3" ~ "At Least Once a Week",
      "4" ~ "Once or Twice a Month",
      "5" ~ "Several Times a Year",
      "6" ~ "Rarely or Never",
      .default = as.character(READSCRP) # Ensures matching data types
    ),
    levels = c(
      "Daily", 
      "Almost Every Day", 
      "At Least Once a Week", 
      "Once or Twice a Month", 
      "Several Times a Year", 
      "Rarely or Never"
    )
  ))

# (36): PRVPRAYR: How often you participate in private prayer
# 1) Daily
# 2) Almost Every Day
# 3) At Least Once a Week
# 4) Once or Twice a Month
# 5) Several Times a Year
# 6) Rarely or Never
# Missing	71	

dat <- dat %>%
  mutate(PRVPRAYR = factor(
    case_match(PRVPRAYR,
      "1" ~ "Daily",
      "2" ~ "Almost Every Day",
      "3" ~ "At Least Once a Week",
      "4" ~ "Once or Twice a Month",
      "5" ~ "Several Times a Year",
      "6" ~ "Rarely or Never",
      .default = as.character(PRVPRAYR) # Ensures matching data types
    ),
    levels = c(
      "Daily", 
      "Almost Every Day", 
      "At Least Once a Week", 
      "Once or Twice a Month", 
      "Several Times a Year", 
      "Rarely or Never"
    )
  ))

# (250) GENDER
# 1) Male
# 2) Female
# Missing	17	

dat <- dat %>%
  mutate(GENDER = factor(
    case_match(GENDER,
      "1" ~ "Male",
      "2" ~ "Female",
      .default = as.character(GENDER) # Ensures matching data types
    ),
    levels = c(
      "Male", 
      "Female"
    )
  ))

# (251): AGE
# 1) 20 years of age or less
# 2) Between 20 and 25
# 3) Between 26 and 30
# 4) Between 31 and 35
# 5) Between 36 and 40
# 6) Between 41 and 45
# 7) Between 46 and 50
# 8) Between 51 and 55
# 9) Between 56 and 60
# 10) 61 and over
# Missing	28	

# Transformed using centerpoint of each age
dat <- dat %>%
  mutate(AGE = 
    case_match(AGE,
      1 ~ 18,
      2 ~ 22.5,
      3 ~ 28.5,
      4 ~ 33.5,
      5 ~ 38.5,
      6 ~ 43.5,
      7 ~ 48.5,
      8 ~ 53.5,
      9 ~ 58.5,
      10 ~ 63.5,
      .default = AGE # Ensures matching data types
    )
  )

# (252): INCOME
# 1) $4,000 or less
# 2) $5,000
# 3) $6,000
# 4) $7,000
# 5) $8,000
# 6) $9,000
# 7) $10,000
# 8) $11,000 to $15,000
# 9) $15,000 to $20,000
# 10) $20,000 to $25,000
# 11) $35,000 to $50,000
# 12) More than $50,000
# Missing	80

# Transformed using centerpoint of each income bracket
dat <- dat %>%
  mutate(INCOME = 
    case_match(INCOME,
      1 ~ 4000,
      2 ~ 5000,
      3 ~ 6000,
      4 ~ 7000,
      5 ~ 8000,
      6 ~ 9000,
      7 ~ 10000,
      8 ~ 12500,
      9 ~ 17500,
      10 ~ 22500,
      11 ~ 42500,
      12 ~ 52500,
      .default = INCOME # Ensures matching data types
    )
  )


# (254): REGION
# 1) Utah
# 2) California
# 3) Idaho
# 4) Arizona
# 5) Other Western state
# 6) Elsewhere in the U.S. (specify):
# 7) Foreign country (specify):
# Missing	16

dat <- dat %>%
  mutate(REGION = factor(
    case_match(REGION,
      "1" ~ "Utah",
      "2" ~ "California",
      "3" ~ "Idaho",
      "4" ~ "Arizona",
      "5" ~ "Other Western state",
      "6" ~ "Elsewhere in U.S.",
      "7" ~ "Foreign Country",
      .default = as.character(REGION) # Ensures matching data types
    ),
    levels = c(
      "Utah",
      "California",
      "Idaho",
      "Arizona",
      "Other Western state",
      "Elsewhere in U.S.",
      "Foreign Country"
    )
  ))

dat <- na.omit(dat)

saveRDS(dat, file = "filt_and_fixed.rds")
