remotes::install_github(repo = "OHDSI/PhenotypeLibrary", ref = "v3.22.1")


# Step 1: get all cohort definition in OHDSI PhenotypeLibrary ----
fullPhenotypeLog <- PhenotypeLibrary::getPhenotypeLog() |>
  dplyr::filter(stringr::str_detect(
    string = cohortName,
    pattern = stringr::fixed("[D]"),
    negate = TRUE
  ))


# Note: HowOften has three types of analysis
## Analysis 1: Use all cohorts in PL that met some criteria as outcome, and use a baseCohort as Target
## Analysis 2: Community proposals
## Analysis 3: Compare incidence of certain outcomes after exposure to drug with and without subset to drugs indications

# Step 2: Identify and flag the cohortIds of the cohorts we want to use in HowOften ----

subsetOfCohorts <- c()
## analysis 1 base cohort. The cohort is 1071
subsetOfCohorts$baseCohort <- fullPhenotypeLog |>
  dplyr::filter(cohortId %in% c(1071)) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonBaseCohort = 1)

## all cohorts that have been accepted to OHDSI PhenotypeLibrary after some review process
subsetOfCohorts$acceptedCohorts <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_length(string = addedVersion) > 0) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonAcceptToOhdsiPl = 1)

## Designated Medical Events
subsetOfCohorts$foundInLibraryOutcomeDme <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#DME")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonDme = 1)

## AESI cohorts built for covide anlaysis. These are mostly imported from the covid aesi studies
subsetOfCohorts$foundInLibraryOutcomeAesi <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#AESI")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonAesi = 1)

# LEGEND studies are imported from Legend Hypertension and LEGEND Diabetes. Unfortunately the cohorts may have duplicated.
subsetOfCohorts$foundInLibraryOutcomeLegend <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#LEGEND")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonLegend = 1)

# All cohorts that were submitted to the OHDSI PhenotypeLibrary on or after August 1st 2023
subsetOfCohorts$recentSubmission <- fullPhenotypeLog |>
  dplyr::filter(createdDate > as.Date('2023-08-01')) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonRecentlyPosted = 1)

# These are hand picked cohorts that were used by Patrick to subset drug exposure cohorts
subsetOfCohorts$libraryIndicationCohorts <- fullPhenotypeLog |>
  dplyr::filter(cohortId %in% c(770,
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
                                748)) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonAnalysis3Indication = 1)

# These are cohorts that Patrick built for use in Analysis 3
subsetOfCohorts$howOften <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = hashTag, pattern = "#HowOften")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonHowOftenAnalysis3 = 1)



allCohorts <- dplyr::bind_rows(subsetOfCohorts) |>
  dplyr::select(cohortId) |>
  dplyr::distinct() |>
  dplyr::left_join(subsetOfCohorts$baseCohort) |>
  dplyr::left_join(subsetOfCohorts$acceptedCohorts) |>
  dplyr::left_join(subsetOfCohorts$foundInLibraryOutcomeDme) |>
  dplyr::left_join(subsetOfCohorts$foundInLibraryOutcomeAesi) |>
  dplyr::left_join(subsetOfCohorts$foundInLibraryOutcomeLegend) |>
  dplyr::left_join(subsetOfCohorts$recentSubmission) |>
  dplyr::left_join(subsetOfCohorts$libraryIndicationCohorts) |>
  dplyr::left_join(subsetOfCohorts$howOften) |>
  tidyr::replace_na(
    replace = list(
      reasonBaseCohort = 0,
      reasonAcceptToOhdsiPl = 0,
      reasonDme = 0,
      reasonAesi = 0,
      reasonLegend = 0,
      reasonRecentlyPosted = 0,
      reasonAnalysis3Indication = 0,
      reasonHowOftenAnalysis3 = 0
    )
  )


# Step 3: Assign clean window ----
## All cohorts get a default clean window ----
allCohorts <- allCohorts |>
  dplyr::mutate(cleanWindow = 9999)

## Only event cohorts need custom clean window. ----
cohortsThatAreEventCohorts <- fullPhenotypeLog |>
  dplyr::filter(qualifyingLimitType == "All", primaryCriteriaLimit == "All") |>
  dplyr::pull(cohortId)

allCohorts <- allCohorts |>
  dplyr::mutate(eventCohorts = dplyr::if_else(
    condition = (cohortId %in% c(cohortsThatAreEventCohorts)),
    true = 1,
    false = 0
  )) |>
  dplyr::arrange(cohortId)

### We are using a combination of rule and heuristic to assign clean window.
allCohorts <- allCohorts |>
  dplyr::mutate(cleanWindowAssigned = 0) |>
  dplyr::mutate(
    cleanWindowAssigned = dplyr::if_else(
      condition = !as.logical(eventCohorts),
      true = 1,
      false = cleanWindowAssigned
    )
  )
allCohorts |>
  dplyr::group_by(cleanWindowAssigned, eventCohorts) |>
  dplyr::summarise(n = dplyr::n())

### Rule 1: use collapseEraPad  ----
### explore the distribution of collapseEraPad among event cohorts
allCohorts |>
  dplyr::filter(eventCohorts == 1) |>
  dplyr::inner_join(fullPhenotypeLog) |>
  dplyr::group_by(collapseEraPad) |>
  dplyr::select(collapseEraPad) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::arrange(collapseEraPad)

# Choice: If collapseEraPad >= 7, then we will use collapseEraPad
allCohorts <- allCohorts |>
  dplyr::inner_join(fullPhenotypeLog |>
                      dplyr::select(cohortId,
                                    collapseEraPad)) |>
  dplyr::mutate(
    cleanWindow = dplyr::if_else(
      condition = (eventCohorts == 1) &
        (cleanWindowAssigned == 0) & (collapseEraPad > 7),
      true = collapseEraPad,
      false = cleanWindow
    ),
    cleanWindowCollapseEraPad = dplyr::if_else(
      condition = (eventCohorts == 1) &
        (cleanWindowAssigned == 0) & (collapseEraPad > 7),
      true = 1,
      false = 0
    ),
    cleanWindowAssigned = dplyr::if_else(
      condition = (eventCohorts == 1) &
        (cleanWindowAssigned == 0) & (collapseEraPad > 7),
      true = 1,
      false = cleanWindowAssigned
    )
  )
allCohorts |>
  dplyr::group_by(cleanWindowAssigned, eventCohorts) |>
  dplyr::summarise(n = dplyr::n())

# rest is manual assignment
needManualAssignment <- allCohorts |>
  dplyr::filter(cleanWindowAssigned == 0) |>
  dplyr::inner_join(
    fullPhenotypeLog |>
      dplyr::select(
        cohortId,
        cohortName,
        exitPersistenceWindow,
        hasWashoutInText,
        useOfObservationPeriodInclusionRule,
        exitDateOffSetField,
        exitDateOffSet,
        exitStrategy,
        collapseEraPad
      )
  ) |>
  dplyr::relocate(cohortId,
                  cohortName) |>
  dplyr::arrange(cohortId)

readr::write_excel_csv(x = needManualAssignment,
                       file = "needManualAssignment.csv", na = "")
