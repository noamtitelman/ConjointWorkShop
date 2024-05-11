library(haven)
library(tidyverse)

d<-read_sav("C:/Users/ntite/Downloads/MY560+Conjoin+Workshop+LSE_May+11,+2024_02.19/MY560 Conjoin Workshop LSE_May 11, 2024_02.19.sav")
d<- data %>%
  select(starts_with("choice"), ends_with("pref") )%>%
  rownames_to_column(var="id")%>%
  rename("1_pref" = A1_pref, "2_pref" = A2_pref, "3_pref" = A3_pref, "4_pref" = A4_pref, "5_pref" = A5_pref)

# Reshape the sandwich attributes so each sandwich gets its own row .
d_sand <- d %>% select(id , starts_with ("choice"))
d_sand <- d_sand %>%
  gather (variable , value, - id ) %>%
  mutate (
    choiceNum = gsub ("[A-Za-z]|_.+" , "" , variable ) ,
    sandNum = gsub (".+(.$)" , "\\1" , variable ) ,
    attribute = gsub (".+_|.$" , "" , variable )
    ) %>%
  select ( -variable ) %>%
  spread ( attribute , value )

 # Reshape the respondent ’s preferences so each choice gets its own row .
d_pref <- d %>% select (id , ends_with ("pref" ))
d_pref <- d_pref %>%
  gather ( variable , preference , -id ) %>%
  mutate (
    choiceNum = gsub ("_pref" , "" , variable ) ,
    preference = as.numeric ( gsub ("Sandwich" , "" , preference ))
    ) %>%
  select ( -variable )

# Merge the attributes and preferences .
d_stack <- left_join( d_sand , d_pref )
d_stack <- d_stack %>%
  mutate (
    Y = as.numeric(sandNum == preference )
    )

# Check that you did not create any extra rows .
nrow( d_stack ) == ( nrow(d) * max(as.numeric(d_stack$sandNum ))* max(as.numeric(d_stack$choiceNum )))
