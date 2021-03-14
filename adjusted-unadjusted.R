# depends on objects created in index.Rmd

a <- zmr %>% 
  filter(rok %in% 2011:2019,
         vek_txt == "celkem") %>% 
  group_by(tyden) %>% 
  summarise(hodnota = mean(hodnota)) %>% 
  filter(tyden %in% 46:49)

unique(zmr$vek_txt)

b <- zmr_normd_with_sum %>% 
  filter(rok %in% 2011:2019,
         vek_txt == "celkem") %>% 
  group_by(tyden) %>% 
  summarise(normd = mean(normd)) %>% 
  filter(tyden %in% 46:49)

left_join(a, b) %>% 
  mutate(x = normd - hodnota)

mdl_data <- excess_data %>% 
  mutate(excess = umrti_excess + umrti_covid,
         covid = umrti_covid) %>% 
  rename(excess_noncovid = umrti_excess) %>% 
  mutate(excess_noncovid_share = excess_noncovid/excess,
         excess_noncovid_of_covid = excess_noncovid/covid)

mdl1 <- lm(excess_noncovid ~ covid, data = mdl_data)
summary(mdl1)
mdl2 <- lm(excess ~ covid, data = mdl_data)
summary(mdl2)

crd <- drop_na(mdl_data)
cor(crd$excess, crd$covid)


