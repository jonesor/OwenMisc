#testing COMPADRE

x <- cdb_fetch("compadre", version = "6.23.3.0")
names(x)


x <- x %>% 
  mutate(SpeciesAuthor_tidy = gsub("_", " ", SpeciesAuthor, fixed = TRUE)) %>%  # replace first underscore with space
  mutate(SpeciesAuthor_tidy = gsub("[0-9]", "", SpeciesAuthor_tidy)) %>%       # remove numbers
  mutate(SpeciesAuthor_tidy = trimws(SpeciesAuthor_tidy))

table(x$SpeciesAuthor_tidy == x$SpeciesAccepted)

x %>% 
  cdb_metadata() %>% 
  select(SpeciesAuthor_tidy,SpeciesAccepted) %>% 
  slice(which(SpeciesAuthor_tidy != SpeciesAccepted)) %>% 
  unique()
