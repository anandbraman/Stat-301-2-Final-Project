library(tidyverse)
library(skimr)
library(janitor)
library(broom)
library(modelr)

set.seed(301)

hc <- read_csv("data/hhpub17.csv") %>% clean_names()

hc_full <- read_csv("data/hhpub17.csv") %>% clean_names()

# filtering out data where healthcare data is missing or not in the range that they specified

# variable to simplify classification -------------------------------------

# predicting all households that have fewer than all people insured.

hc <- hc %>%  mutate(hcov_recode = ifelse(hcov == 1, 0, 1),
                     now_hcov_recode = ifelse(now_hcov == 1, 0 , 1))


hc_full <- hc_full %>% mutate(hcov_recode = ifelse(hcov == 1, 0, 1),
                              now_hcov_recode = ifelse(now_hcov == 1, 0 , 1))

hc <- hc %>% filter(hcov != 0)

hc_full <- hc_full %>% filter(hcov != 0)

write_csv(hc_full, "data/asec_full.csv")


# Geographic Info ---------------------------------------------------------

# selecting cols that are usable ---------------------------------------------------

# some variables cannot be used because they are flags for certain things etc

hc <- hc %>% dplyr::select(-hrecord, -h_hhnum, -h_idnum, -h_seq, -hsup_wgt,
              -gediv, -gereg, -gestfips, -gtcbsa, -gtcbsast, -h_livqrt,
              -gtco, -gtcsa, -gtindvpc, -h_hhtype, -h_mis, -hhstatus,
              -hrhtype, -i_hunits, -h_month, -h_respnm, -h_telavl, -h_telhhd, -h_telint,
              -h_tenure, -h_typebc, -h_year, -h1livqrt, -h1telavl, -h1telhhd, -h1telint,
              -h1tenure, -i_chcareval, -i_hengas, -i_hengva, -i_hfdval, -i_hflunc, -i_hflunn,
              -i_hfoodm, -i_hfoodn, -i_hfoods, -i_hhotlu, -i_hhotno, -i_hloren, -i_hpubli,
              -i_propval, -thchcare_val, -thprop_val, -hh_hi_univ, -gestcen)

hc <- hc %>% select_if(is_bare_numeric)

skim(hc)



colnames(hc)





# universe = interviewed HH ----------------------------------------------------------

# looks like I have to clean more because a lot of people didn't report income
# dropping the actual value in favor of their metric hhinc

hc <- hc %>% select(-hearnval)

## ppl in household
hc %>% pull(h_numper) %>% tabyl()

# collapsing values between 8 and 16

hc <- hc %>% mutate(h_numper_recode = case_when(h_numper %in% c(8:16) == TRUE ~ 8,
                                                TRUE ~ as.double(h_numper)))

hc <- hc %>% select(-h_numper)

hc %>% ggplot(aes(x = hannval)) +
  geom_histogram()

hc %>% ggplot(aes(x = hchcare_val)) +
  geom_histogram()

hc %>% filter(hchcare_val == -1)

hc %>% tabyl(hchcare_yn)





write_csv(hc, "data/asec_clean.csv")

hc_reduced <- hc %>% select(-hannval, -hchcare_val, -hcspval, -hdisval,
                            -hdivval, -hdstval, -hedval, -hengval, -hfdval, -hflunch,
                            -hflunno, -hfoodmo, -hfoodno, -hfrval, -hhotno, -hintval,
                            -hoival, -hpawval, -hrntval, -hrnumwic, -hseval, -hssival, 
                            -hssval, -hsurval, -hucval, -hvetval)

write_csv(hc_reduced, "data/asec_reduced.csv")
