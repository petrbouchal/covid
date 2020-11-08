kkk <- czso::czso_get_catalogue()

priciny0 <- czso::czso_get_table("130143")
skimr::skim(priciny)

priciny <- priciny0 %>% 
  filter(is.na(pohlavi_txt)) %>% 
  select(-starts_with("pohlavi")) %>% 
  filter(vuzemi_cis == "97") %>% 
  select(-starts_with("vuzemi"), -ends_with("_cis"), -ends_with("_kod")) 

priciny_all_3yr <- priciny %>% 
  group_by(ps0_txt, rok) %>% 
  summarise(hodnota = sum(hodnota)) %>% 
  group_by(ps0_txt) %>% 
  summarise(hodnota = mean(hodnota)) %>% 
  mutate(mesicne = hodnota/12) %>% 
  arrange(desc(mesicne))

priciny_resp_3yr <- priciny %>% 
  filter(ps0_txt == "Nemoci dýchací soustavy (J00 - J99)",
         rok > 2016) %>%
  group_by(ps_txt) %>% 
  summarise(hodnota = mean(hodnota)) %>% 
  mutate(mesicne = hodnota/12) %>% 
  arrange(desc(hodnota))

priciny_vnejsi_3yr <- priciny %>% 
  filter(ps0_txt == "Vnější příčiny nemocnosti a úmrtnosti (V01 - Y98)",
         rok > 2016) %>%
  group_by(ps_txt) %>% 
  summarise(hodnota = mean(hodnota)) %>% 
  mutate(mesicne = hodnota/12) %>% 
  arrange(desc(hodnota))

ps0_cis <- czso::czso_get_codelist("cis5120")
ps_cis <- czso::czso_get_codelist(5121)

ps_cis <- read_csv("http://apl.czso.cz/iSMS/cisexp.jsp?kodcis=5120&typdat=0&cisvaz=5121_1214&datpohl=08.11.2020&cisjaz=203&format=2&separator=%2C",
         locale = locale(encoding = "WINDOWS-1250"))
