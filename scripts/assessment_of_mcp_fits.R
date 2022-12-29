# mcp fit assessment metrics

library(here)
library(tidyverse)

# these are the assessments of whether the mcp fit is adequate to extract model parameters
df<-read_csv(here("mcp_assessment.csv"))

df<-df %>% 
  mutate(capture_state=ifelse(grepl("[7-9]L", ID)|
                                grepl("0H_2nd", ID), "AR",
                              ifelse(grepl("9H_2nd", ID)|
                                       grepl("2H_2nd", ID)|
                                       grepl("0N_2nd", ID)|
                                       grepl("6P", ID), "MN",
                                     ifelse(grepl("A", ID)|
                                              grepl("E", ID)|
                                              grepl("R", ID)|
                                              grepl("T",ID)|
                                              grepl("L", ID),"MN",
                                            ifelse(grepl("M", ID)|
                                                     grepl("N", ID), "OH",
                                                   ifelse(grepl("H", ID), "MB",
                                                          ifelse(grepl("C", ID), "IA",
                                                                 ifelse(grepl("P", ID), "WI",
                                                                        ifelse(grepl("J", ID)|
                                                                               grepl("K", ID), "MI",
                                                                        "flag")))))))))

table(df$mcp_fit_well, df$capture_state)
df$mcp_fit_well<-ifelse(df$mcp_fit_well=="Y", 1, 0)

df %>% 
  group_by(capture_state) %>% 
  summarise(fits_by_state=sum(mcp_fit_well==1)/n())


