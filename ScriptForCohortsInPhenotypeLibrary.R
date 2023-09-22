remotes::install_github("OHDSI/PhenotypeLibrary")

fullPhenotypeLog <- PhenotypeLibrary::getPhenotypeLog()

# Analysis 1 - 
fullPhenotypeLog |> 
  dplyr::filter(createdDate > as.Date('2023-08-01')) |> 
  dplyr::arrange(as.Date('2023-08-01')) |> 
  View()


# Step 2 - forum post T and O combinations







# Analysis 3: this is the study to compute incidence of T&O + TwI & O

## HowOften cohorts ----
### Cohorts built by Patrick ----
builtByPatrick <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = hashTag, pattern = "#HowOften"))

### Cohorts that were used to subset in howOftenCohort ----
foundInLibraryIndication <-
  PhenotypeLibrary::getPhenotypeLog(c(770,
                                      765,
                                      71,
                                      1032,
                                      32,
                                      749,
                                      861,
                                      19,
                                      858,
                                      860,
                                      859,
                                      748))

### outcomes for use in HowOften study ----
foundInLibraryOutcomeDme <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#DME"))

foundInLibraryOutcomeAesi <- fullPhenotypeLog |> 
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#AESI")) ## add erica study + release

foundInLibraryOutcomeLegend <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#LEGEND"))

### there are duplicates of LEGEND cohorts hypertension







