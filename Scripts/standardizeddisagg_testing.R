library(gagglr)
library(tameDP)
library(tidyverse)
library(glue)
library(gt)
library(gtExtras)

msd_filepath<-return_latest(folderpath = si_path(),
                            pattern = "PSNU_IM")
df_msd<-read_psd(msd_filepath)


## Summarizes total numerator for list of indicators
df_num<-df_msd %>% filter(indicator %in% c("HTS_SELF", "PrEP_NEW", "GEND_GBV", "OVC_SERV"), 
                          standardizeddisaggregate=="Total Numerator") %>%
                          group_by(fiscal_year, indicator) %>%
                          summarise(across(targets, \(x) sum(x,na.rm = TRUE)), .groups = "drop") %>%
                          rename(total_numerator=targets) 
                          
## Lists potential Age/Sex disaggs for list of indicators
df_disagg<-df_msd %>% 
  filter(indicator %in% c("HTS_SELF", "PrEP_NEW", "GEND_GBV", "OVC_SERV"),
         source_name == "DATIM",
         str_detect(standardizeddisaggregate, "Age/Sex")) %>% 
  group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
  summarise(across(targets, \(x) sum(x,na.rm = TRUE)), .groups = "drop") %>%
  arrange(fiscal_year, indicator, standardizeddisaggregate) %>% 
  rename(age_sex_sum=targets) 


df_comp<-left_join(df_disagg,df_num,by=c("fiscal_year","indicator")) %>%
  mutate(difference=total_numerator-age_sex_sum)
   

#df_msd %>% 
#  filter(indicator %in% c("HTS_SELF", "PrEP_NEW", "GEND_GBV", "OVC_SERV")
#         ,source_name == "DATIM"
#         ,str_detect(standardizeddisaggregate, "Age/Sex")
#  ) %>% 
#  distinct(fiscal_year, indicator, standardizeddisaggregate) %>%
#  group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
#  arrange(fiscal_year, indicator, standardizeddisaggregate) %>% 
#  filter(row_number() == 1) %>% 
#  ungroup() %>% 
#  arrange(fiscal_year, indicator)

