library(googlesheets4)
library(flextable)
library(tidyverse)

results=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1h0S27-69mZgoPEyer9InFWZ0rN2PRSdlvqT7xS7Ob6k/edit#gid=0",
                            sheet = "Results")
events=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1h0S27-69mZgoPEyer9InFWZ0rN2PRSdlvqT7xS7Ob6k/edit#gid=0",
                                 sheet = "Events")




results2=results %>% pivot_longer(cols=-c(Name), names_to="Event", values_to = "Score") %>% 
  filter(!is.na(Score)) %>% 
  merge(events %>% dplyr::select(Event, Scoring, Date), by="Event") %>% 
  group_by(Event) %>% 
  mutate(points=if_else(Scoring=="High", rank(Score, ties.method = "average"), rank(-Score, ties.method = "average"))) %>% 
  ungroup() %>% 
  dplyr::select(-Scoring, -Score)

resultswide=results2 %>% arrange(Date) %>% select(-Date) %>% pivot_wider(values_from = "points", names_from = "Event")

scoringevents=floor(nrow(events)*(2/3))

championship=results2 %>% 
  select(-Date, -Event) %>% 
  group_by(Name) %>% 
  slice_max(points, n=scoringevents, with_ties = F) %>% 
  summarise(Points=sum(points)) %>% 
  ungroup() %>% 
  mutate(Position=rank(-Points, ties.method = "min")) %>% 
  arrange(Position, Name)

c2=championship %>% merge(resultswide, by="Name") %>% 
  arrange(Position, Name) %>% 
  dplyr::select(-Position)


colours=results2 %>% 
  group_by(Name) %>% 
  slice_max(points, n=scoringevents, with_ties = F) %>% 
  mutate(scoring="#cceecc") %>% 
  merge(championship %>% select(Name, Position), by="Name") %>% 
  arrange(Date) %>% 
  select(-Date, -points) %>% 
  pivot_wider(values_from = "scoring", names_from = "Event") %>% 
  arrange(Position, Name) %>% 
  ungroup() %>% 
  select(-Name, -Position) %>% 
  as.matrix()


x=championship %>%
  arrange(Position, Name) %>% 
  mutate(col=case_when(Position==1~"#FFD700",
                       Position==2~"#C0C0C0",
                       Position==3~"#CD7F32"))
colour1=cbind(x$col, colours)



c2 %>% flextable() %>% 
  bg(j=2:(nrow(events)+2), bg=colour1)

sheet_write(c2, ss="https://docs.google.com/spreadsheets/d/1h0S27-69mZgoPEyer9InFWZ0rN2PRSdlvqT7xS7Ob6k/edit#gid=0",
            sheet="Scores")


