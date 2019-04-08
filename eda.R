hc_eda <- read_csv("data/asec_clean.csv") %>% clean_names()

hc_eda <- hc %>% sample_frac(.30)

hc_eda %>% 
  ggplot(aes(x = now_hcov_recode, fill = as.factor(now_hcov_recode))) +
  geom_bar() +
  xlab("") +
  scale_x_discrete() + labs(fill = "Health Care Coverage")

hc_eda %>% 
  ggplot(aes( x = hhinc)) +
  geom_histogram()

hc_eda %>% ggplot(aes(x = as.factor(now_hcov_recode), y = as.factor(hcov_recode))) +
  geom_bin2d() +
  labs(x = "Current Healthcare Coverage", y = "Healthcare Coverage Past Year")

