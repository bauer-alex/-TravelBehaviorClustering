###### File containing functions needed for the paper:
# Understanding travel behaviour patterns and their dynamics 
###################################################################################

# Data preparation:
# Function to prepare tourist data for cluster analysis:
read_and_prepare_data <- function(dat) {
  
  ### 1. filter data by travel year and origin (only western germany)
  dat <- dat %>% filter(travel_year>= 1983)
  
  # Variable S_Herkunft: NA for travel_year < 1990, then "West" or "Ost"
  dat <- dat %>% filter(travel_year < 1990 | S_Herkunft == "West")
  
  # Remove people without a single trip:
  dat <- dat %>% filter(JS_Anzahl_URs != 0)
  
  ### 2. recode 'keine Angabe' and '-99' values to NA
  dat <- dat %>%
    dplyr::mutate(across(everything(), ~replace(., . == -99, NA)),
                  across(everything(), ~replace(., . == "keine Angabe", NA)))
  
  ### 3. Preparation of variables
  
  #### 3.1 Travel Frequency
  dat <- dat %>%
    mutate(
      RH_VJ = case_when(
        grepl("drei", RH_AnzUR_VJ_kat) ~ 3.3,
        grepl("Nein", RH_AnzUR_VJ_kat) ~ 0.0,
        grepl(" eine", RH_AnzUR_VJ_kat) ~ 1.0,
        grepl("zwei", RH_AnzUR_VJ_kat) ~ 2.0,
        TRUE ~ NA_real_ # "keine Angabe" to NA
      ),
      RH_2J = case_when(
        grepl("drei", RH_AnzUR_2J) ~ 3.3,
        grepl("Nein", RH_AnzUR_2J) ~ 0.0,
        grepl(" eine", RH_AnzUR_2J) ~ 1.0,
        grepl("zwei", RH_AnzUR_2J) ~ 2.0,
        TRUE ~ NA_real_ # "keine Angabe" to NA
      ),
      RH_3J = case_when(
        grepl("drei", RH_AnzUR_3J) ~ 3.3,
        grepl("Nein", RH_AnzUR_3J) ~ 0.0,
        grepl(" eine", RH_AnzUR_3J) ~ 1.0,
        grepl("zwei", RH_AnzUR_3J) ~ 2.0,
        TRUE ~ NA_real_ # "keine Angabe" to NA
      ))
  
    dat <- dat %>%
      mutate(RH_VJ = (RH_VJ-1)/2.3,
             RH_2J = RH_2J/3.3,
             RH_3J = RH_3J/3.3)
  
  #### 3.2 Destination
  ## 3.2.1 Major regrion
  Regionen <- read_excel("Data/Grossregionen.xlsx") %>% # Correct column names and "Fernreiseziele" 
    filter(!is.na(Grossregionen))
  dat$JS_UR1_Reiseziel <- dat$JS_HUR_Reiseziel
  vars_RZ <- paste0("JS_UR", 1:12, "_Reiseziel")
  dat_RZ <- dat %>%
    select(vars_RZ)
  
  for(i in vars_RZ){
    dat_RZ[, i] <- as.character(factor(unlist(dat[, i]), levels = Regionen$Code,
                                       labels = Regionen$Grossregionen))
  }
  
  dat$HUR_Reiseziel_Clustering <- dat_RZ$JS_UR1_Reiseziel
  dat$HUR_Reiseziel_Clustering_2 <- factor(dat$HUR_Reiseziel_Clustering, 
                                           levels = c("MITTELEUROPA", "NORDEUROPA", "SUEDOSTEUROPA", "SUEDEUROPA", 
                                                      "WESTEUROPA", "OSTEUROPA", "SONSTMITTELMEER", "FERNREISEZIELE"), labels = 1:8)
  
  for(j in unique(Regionen$Grossregionen)){
    dat_RZ[, j] <- rowSums(dat_RZ == j, na.rm = TRUE)
  }
  dat_RZ <- dat_RZ %>%
    mutate_at(unique(Regionen$Grossregionen), ~if_else(
      . > 1,
      1,
      .)) # Correct values above 1 to 1 
  dat_RZ <- dat_RZ %>%
    select(unique(Regionen$Grossregionen))
  colnames(dat_RZ) <- paste0("RZ_Region_", colnames(dat_RZ))
  
  dat <- cbind(dat, dat_RZ)
  
  
  ## 3.2.2 Popularity
  # Join german federal states into one destination
  # All destinations with just 2 characters are german federal states
  dat <- dat %>%
    mutate_at(vars_RZ, ~if_else(
      nchar(.) == 2,
      "DEU",
      .)) %>%
    mutate_at(vars_RZ, ~if_else(
      . %in% c("ITAN", "ITAT"),
      "ITA",
      .))
  
  # Popularity by year as fraction of destination of all travels in corresponding year 
  dat_Erschl <- dat %>% select(
    travel_year, all_of(vars_RZ))
  
  dat_Erschl <- dat_Erschl %>%
    gather("travel", "destination", 2:13) %>%
    filter(!is.na(destination)) %>%
    select(-travel)
  
  n_travels <- dat_Erschl %>%
    group_by(travel_year) %>%
    summarise(travels = n())
  
  dat_Erschl <- dat_Erschl %>%
    group_by(travel_year, destination) %>%
    summarise(n = n()) %>%
    left_join(n_travels) %>%
    mutate(Erschlossenheit = n/travels)
  
  
  for(i in vars_RZ){ # Append info to data
    dat_Erschl_temp <- dat_Erschl %>%
      select(travel_year, destination, Erschlossenheit)
    colnames(dat_Erschl_temp)[c(2, 3)] <- c(i, paste0(i, "_Erschlossenheit"))
    
    dat <- dat %>%
      left_join(dat_Erschl_temp)
  }
  
  # Get minimum of popularity (exotic destination) for each observation
  dat$Erschlossenheit <- apply(dat[, paste0("JS_UR", 1:12, "_Reiseziel_Erschlossenheit")], 1, min, na.rm = TRUE)
  
  # Divide by maximum to normalize values / force them to lie between [0, 1]
  dat <- dat %>%
      mutate(Erschlossenheit = Erschlossenheit/max(Erschlossenheit))
  
  #### 3.3 Activities
  # HUR only accessible unitl 1995; UR only accessible after 1995 (see Codebook)
  # Use HUR up to and including 1995, after, use UR
  vars_UA <- grep("UA_OB", colnames(dat), value = "TRUE")
  vars_UA <- str_replace(vars_UA, "HUR", "")
  vars_UA <- str_replace(vars_UA, "UR", "")
  vars_UA <- unique(vars_UA)
  
  for(i in vars_UA){
    
    var_clustering <- rep(NA, nrow(dat))
    var_clustering[dat$travel_year <= 1995] <- dat[dat$travel_year <= 1995, paste0(i, "HUR")]
    var_clustering[dat$travel_year > 1995] <- dat[dat$travel_year > 1995, paste0(i, "UR")]
    
    dat[, paste0(i, "clustering")] <- var_clustering
  }
  
  #### 3.4 Holiday type
  vars_UF <- grep("UF_Gemacht", colnames(dat), value = TRUE)
  dat <- dat %>%
    mutate_at(vars_UF, ~case_when(
      . == "Nein" ~ 0,
      . == "Ja" ~ 1,
      TRUE ~ NA_real_
    ))
  
  dat <- dat %>%
    mutate(UF_cluster_Gesund = rowSums(across(paste0("UF_Gemacht_", c(4, 2, 3, 31))), na.rm = TRUE),
           UF_cluster_Aktiv = rowSums(across(paste0("UF_Gemacht_", c(1, 18, 21, 22, 29, 30, 32, 36))), na.rm = TRUE),
           UF_cluster_Bildung = rowSums(across(paste0("UF_Gemacht_", c(7, 9, 23, 25))), na.rm = TRUE),
           UF_cluster_Schiff = rowSums(across(paste0("UF_Gemacht_", c(6, 12))), na.rm = TRUE),
           UF_cluster_Sight = rowSums(across(paste0("UF_Gemacht_", c(8, 10))), na.rm = TRUE),
           UF_cluster_Familie = rowSums(across(paste0("UF_Gemacht_", c(27, 34))), na.rm = TRUE),
           UF_cluster_Strand = rowSums(across(paste0("UF_Gemacht_", c(5, 24, 26, 28, 33, 35, 37))), na.rm = TRUE)
    )
  
  # Correct observations with only NA to NA (sum was zero)
  dat$UF_cluster_Gesund[rowSums(is.na(dat[,paste0("UF_Gemacht_", c(4, 2, 3, 31))])) == 4] <- NA_real_ 
  dat$UF_cluster_Aktiv[rowSums(is.na(dat[,paste0("UF_Gemacht_", c(1, 18, 21, 22, 29, 30, 32, 36))])) == 8] <- NA_real_ 
  dat$UF_cluster_Bildung[rowSums(is.na(dat[,paste0("UF_Gemacht_", c(7, 9, 23, 25))])) == 4] <- NA_real_ 
  dat$UF_cluster_Schiff[rowSums(is.na(dat[,paste0("UF_Gemacht_", c(6, 12))])) == 2] <- NA_real_ 
  dat$UF_cluster_Sight[rowSums(is.na(dat[,paste0("UF_Gemacht_", c(8, 10))])) == 2] <- NA_real_ 
  dat$UF_cluster_Familie[rowSums(is.na(dat[,paste0("UF_Gemacht_", c(27, 34))])) == 2] <- NA_real_ 
  dat$UF_cluster_Strand[rowSums(is.na(dat[,paste0("UF_Gemacht_", c(5, 24, 26, 28, 33, 35, 37))])) == 7] <- NA_real_ 
  
  vars_UF_cluster <- grep("UF_cluster", colnames(dat), value = "TRUE")
  
  dat <- dat %>%
    mutate_at(vars_UF_cluster, ~if_else(
      . > 1,
      1,
      .)) # Correct values above 1 to 1 
  
  
  ## 3.4.2 Organization
  # binary variable: 
  #   1: package tour
  #   0: self organized
  dat <- dat %>%
    mutate(
      Organisationsform_Pauschal = case_when(
        JS_HUR_Organisationsform == "Pauschalreise" ~ 1,
        JS_HUR_Organisationsform == "Selbst organisiert" ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  #### 3.5 Accomodation
  vars_Uk <- grep("JS_Unterkunft", colnames(dat), value = "TRUE")
  
  #### 3.6 Means of transport
  vars_Vk <- grep("JS_Verkehrsmittel", colnames(dat), value = "TRUE")
  
  #### 3.7 Travel companions
  
  # Household:
  # Variables have no 0 values; they are at laest 1
  # -> you yourself is included
  dat$JS_UR1_Reisebegleitung_HH <- dat$JS_HUR_Reisebegleitung_HH
  
  dat_HH <- dat[, paste0("JS_UR", 1:12, "_Reisebegleitung_HH")]
  dat_HH <- dat_HH %>%
    mutate_all(~case_when(
      . > 1 ~ 1,
      . <= 1 ~ 0,
      TRUE ~ NA_real_
    )) # Values now indicate if trip was made with a member of the household (value is 1) or not (value is 0). NA if trip was not made
  
  Anzahl_Reisen <- rowSums(!is.na(dat_HH))
  Anzahl_mitHH <- rowSums(dat_HH == 1, na.rm = TRUE)
  
  dat_HH$Anzahl_Reisen <- Anzahl_Reisen
  dat_HH$Anzahl_mitHH <- Anzahl_mitHH
  
  dat_HH <- dat_HH %>%
    mutate(Anteil = if_else(
      Anzahl_Reisen == 0,
      NA_real_,
      Anzahl_mitHH/Anzahl_Reisen
    ))
  
  dat$Begleitung_AnteilHH <- dat_HH$Anteil
  # At least one travel with household member
  dat$Begleitung_HH <- as.numeric(dat$Begleitung_AnteilHH != 0)  
  
  # non-household:
  dat$JS_UR1_Reisebegleitung_nichtHH <- dat$JS_HUR_Reisebegleitung_nichtHH
  
  dat_nichtHH <- dat[, paste0("JS_UR", 1:12, "_Reisebegleitung_nichtHH")]
  
  dat_nichtHH <- dat_nichtHH %>%
    mutate_all(~case_when(
      . > 0 ~ 1,
      . == 0 ~ 0,
      TRUE ~ NA_real_
    )) # Values now indicate if trip was made with a non-household member (value is 1) or not (value is 0). NA if trip was not made
  
  Anzahl_Reisen <- rowSums(!is.na(dat_nichtHH))
  Anzahl_nichtHH <- rowSums(dat_nichtHH == 1, na.rm = TRUE)
  
  dat_nichtHH$Anzahl_Reisen <- Anzahl_Reisen
  dat_nichtHH$Anzahl_nichtHH <- Anzahl_nichtHH
  
  dat_nichtHH <- dat_nichtHH %>%
    mutate(Anteil = if_else(
      Anzahl_Reisen == 0,
      NA_real_,
      Anzahl_nichtHH/Anzahl_Reisen
    ))
  
  dat$Begleitung_AnteilnichtHH <- dat_nichtHH$Anteil
  dat$Begleitung_nichtHH <- as.numeric(dat$Begleitung_AnteilnichtHH != 0) # At least one travel with non-household member 
  
  # Small child 
  dat$Begleitung_AnteilKind <- dat$JS_Anteil_Kleinkinder
  dat$Begleitung_Kind <- dat$JS_Reisebegleitung_mind1Kleinkind
  
  
  #### 3.8 Travel expenses
  ### 5. recode 'mehr als 5 Personen' in household size to 5.3 persons.
  ###    Reasoning:
  ###    5.3 is the average number of persons (overall, but also only regarding
  ###    persons with 14+ years of age) in households with 5+ persons for the
  ###    travel year 2000 (variables 's9a' and 's9b')
  dat <- dat %>% 
    mutate(S_Haushaltsgroesse        = as.numeric(substr(S_Haushaltsgroesse, 1, 1)),
           S_Haushaltsgroesse_14plus = as.numeric(substr(S_Haushaltsgroesse_14plus, 1, 1))) %>% 
    mutate(S_Haushaltsgroesse        = case_when(S_Haushaltsgroesse == 5 ~ 5.3,
                                                 TRUE                    ~ S_Haushaltsgroesse),
           S_Haushaltsgroesse_14plus = case_when(S_Haushaltsgroesse_14plus == 5 ~ 5.3,
                                                 TRUE                                  ~ S_Haushaltsgroesse_14plus))
  
  
  ### 6. Compute the number of children with <14 years of age in the household
  dat <- dat %>% 
    mutate(S_Haushaltsgroesse_Kinder = S_Haushaltsgroesse - S_Haushaltsgroesse_14plus)
  
  
  ### 7. compute the 'effective of number of fully paying persons' in the
  ###    household based on the OECD-modified equivalence scale.
  ###    The scale is used to account for synergy effects when multiple people
  ###    jointly run one household:
  ###    - the first adult (with 14+ years) counts as 1 person
  ###    - every further adult counts as 0.5 persons
  ###    - every child (<14 years) counts as 0.3 persons
  dat <- dat %>% 
    mutate(S_Haushaltsgroesse_equi = 1 + 0.5 * (S_Haushaltsgroesse_14plus - 1) +
             0.3 * S_Haushaltsgroesse_Kinder)
  
  ### 8. Optional information (not relevant)
  
  dat$JS_UR1_Reisebegleitung_HH <- dat$JS_HUR_Reisebegleitung_HH
  dat$JS_UR1_Ausgaben_gesamt <- dat$JS_HUR_Ausgaben_gesamt
  
  for(i in 1:12){
    
    var_name <- paste0("JS_UR", i, "_Reisebegleitung_HH")
    ### 9. compute the 'travel expenses per effective housemate that joined the trip':
    ###    1. The information how many people from the household jointly traveled
    ###       with the respondent is saved in variable 'JS_HUR_Reisebegleitung_HH'.
    ###    2. Problem:  The number of these people is sometimes larger than the
    ###                 number of actual housemates living in the household.
    ###       Solution: When this is the case we set the number of 'people from the
    ###                 household who jointly traveled with the respondent' to the
    ###                 number of housemates living in the household.
    dat <- dat %>% 
      mutate(!!var_name := case_when(eval(as.symbol(var_name)) > S_Haushaltsgroesse ~ S_Haushaltsgroesse,
                                     TRUE ~ eval(as.symbol(var_name))))
    
    ###    3. Compute the effective number of 'housemates the joined the trip':
    ###       - if the number of 'housemates that joined the trip' equals the
    ###         number of 'housemates in the household':
    ###         in this case the overall equivalence score of the household is used
    ###         as the effective number
    ###       - if the number of 'housemates that joined the trip' is smaller than
    ###         the number of 'housemates in the household':
    ###         in this case the effective number is calculated as
    ###         1 (reasoning: there's always at least one adult from the household) + k * mean_equi,
    ###         with 'k' the 'number of housemates that joined the trip minus one'
    ###         and 'mean_equi' the 'mean of the <overall equivalence score of the household minus 1>'
    ###         (i.e. when excluding one adult person).
    var_name_equi <- paste0("JS_UR", i, "_Reisebegleitung_HH_equi")
    dat <- dat %>% 
      mutate(!!var_name_equi := case_when(eval(as.symbol(var_name)) == S_Haushaltsgroesse ~ S_Haushaltsgroesse_equi,
                                          TRUE ~ 1 + (eval(as.symbol(var_name)) - 1) * ((S_Haushaltsgroesse_equi - 1) / (S_Haushaltsgroesse - 1))))
    
    # Note: Now, households of size 1 have NaN in JS_UR2_Reisebegleitung_HH_equi-variable if no travel
    
    ###    4. compute the person-adjusted travel expenses of the household
    var_name_Ausgaben <- paste0("JS_UR", i, "_Ausgaben_pP_equi")
    dat <- dat %>% 
      mutate(!!var_name_Ausgaben := eval(as.symbol(paste0("JS_UR", i, "_Ausgaben_gesamt"))) / eval(as.symbol(var_name_equi)))
    
    
  }
  
  # Compute median and max expenses
  dat$Ausgaben_median <- apply(dat[, paste0("JS_UR", 1:12, "_Ausgaben_pP_equi")], 1, median, na.rm = TRUE)
  dat$Ausgaben_max <- apply(dat[, paste0("JS_UR", 1:12, "_Ausgaben_pP_equi")], 1, max, na.rm = TRUE)
  dat$Ausgaben_max <- if_else(is.finite(dat$Ausgaben_max),
                              dat$Ausgaben_max, 
                              NA_real_)
  dat$Ausgaben_sum <- apply(dat[, paste0("JS_UR", 1:12, "_Ausgaben_pP_equi")], 1, sum, na.rm = TRUE)
  dat$Ausgaben_sum <- if_else(is.na(dat$Ausgaben_max),
                              NA_real_,
                              dat$Ausgaben_sum) # If all expenses are NA, sum is 0 which is corrected to NA
  
  
  # 95% Quantil sum: 4133.5
    dat <- dat %>%
      mutate(
        Ausgaben_sum = if_else(Ausgaben_sum > 4133.5,
                               4133.5,
                               Ausgaben_sum)
      ) %>%
      mutate(Ausgaben_sum = Ausgaben_sum/4133.5)
    
  
  #### 3.9 Duration
  dat$JS_UR1_Reisedauer <- dat$JS_HUR_Reisedauer
  dat_RD <- dat[, paste0("JS_UR", 1:12, "_Reisedauer")]
  
  for(i in 1:12){
    dat_RD[, paste0("JS_UR", i, "_Reisedauer")] <- as.numeric(as.character(factor(unlist(dat_RD[, paste0("JS_UR", i, "_Reisedauer")]), 
                                                                                  levels = levels(dat$JS_UR1_Reisedauer)[1:9],
                                                                                  labels = (0:8)/8))) # as.character because factor into numeric does not work as intended
  }
  
  # Maximum duration for each observation
  dat$Reisedauer <- apply(dat_RD[, paste0("JS_UR", 1:12, "_Reisedauer")], 1, max, na.rm = TRUE)
  dat$Reisedauer<- if_else(is.finite(dat$Reisedauer),
                           dat$Reisedauer, 
                           NA_real_)
  
  return(dat)
}




# Function to prepare data for further analyses:
# first: prepare variable lists and save them in global environment
# argument dat: final dataset for clustering
prepare_clustering_data <- function(dat) {

  ### select relevant variables
  vars_Uk <- grep("JS_Unterkunft", colnames(dat), value = "TRUE")
  vars_Vk <- grep("JS_Verkehrsmittel", colnames(dat), value = "TRUE")
  vars_UA <- intersect(grep("UA_OB", colnames(dat), value = "TRUE"),
                       grep("clustering", colnames(dat), value = "TRUE")) # Activities
  vars_Begleitung <- grep("Begl", colnames(dat), value = "TRUE")
  vars_UF_cluster <- grep("UF_cluster", colnames(dat), value = "TRUE")
  vars_RZ_Reg <- grep("RZ_Region", colnames(dat), value = "TRUE")
  
  dat_clustering <- dat %>%
    select(travel_year, 
           RH_VJ, RH_2J, RH_3J, # Frequency
           all_of(vars_RZ_Reg),
           HUR_Reiseziel_Clustering, # Destination HUR
           HUR_Reiseziel_Clustering_2, 
           Erschlossenheit,
           all_of(vars_UA), # Activity
           all_of(vars_UF_cluster), # Holiday type
           Organisationsform_Pauschal, # Holiday type
           all_of(vars_Uk), # Accomodation
           all_of(vars_Vk), # Means of transfport
           all_of(vars_Begleitung), # Travel companion
           Ausgaben_median , Ausgaben_max, # Expenses
           Ausgaben_sum,
           Reisedauer
    )
  
  return(dat_clustering)
}


# Function to load names of clustering variables into global environment
# (required for computation of distance matrix):
assign_clustering_variables <- function(clustering_data) {
  # Read distance matrix HUR Reiseziel
  dist_mat_RZ <- read_xlsx("Data/Dist_HUR_Reiseziel.xlsx")
  dist_mat_RZ <- as.matrix(dist_mat_RZ[, -1])
  
  # Get symmetric matrix
  dist_mat_RZ[is.na(dist_mat_RZ)] <- 0
  dist_mat_RZ <- dist_mat_RZ + t(dist_mat_RZ)
  
  
  var_RH_VJ <- grep("RH_VJ", colnames(clustering_data)) # Frequency
  var_RH_2J <- grep("RH_2J", colnames(clustering_data)) # Frequency
  var_RH_3J <- grep("RH_3J", colnames(clustering_data)) # Frequency
  vars_RZ_Reg <- grep("RZ_Region", colnames(clustering_data))
  vars_Erschl <- grep("Erschlossenheit", colnames(clustering_data))
  var_HUR <- grep("HUR_Reiseziel_Clustering_2", colnames(clustering_data))
  vars_UA <- grep("UA_OB", colnames(clustering_data)) # Activity
  vars_UA4 <- grep("4", grep("UA_OB", colnames(clustering_data), value = TRUE)) # Indices of vars_UA which contain variables of OB 4 
  vars_UA_other <- setdiff(1:length(vars_UA), vars_UA4) # Indices of remaining variables
  vars_UF <- grep("UF_cluster", colnames(clustering_data)) # Holiday type
  var_Orga <- grep("Organisationsform", colnames(clustering_data)) # Organization
  vars_Uk <- grep("Unterkunft", colnames(clustering_data)) # Accomodation
  vars_Vk <- grep("Verkehrsmittel", colnames(clustering_data)) # Means of transport
  vars_Beg <- grep("Begleitung", colnames(clustering_data)) # Companion
  vars_Ausg <- grep("Ausgaben_sum", colnames(clustering_data)) # Expenses (sum)
  vars_RD <- grep("Reisedauer", colnames(clustering_data)) 
  
  # Assign variables to global environment:
  assign('dist_mat_RZ', dist_mat_RZ, envir = .GlobalEnv)
  assign('var_RH_VJ', var_RH_VJ, envir = .GlobalEnv)
  assign('var_RH_2J', var_RH_2J, envir = .GlobalEnv)
  assign('var_RH_3J', var_RH_3J, envir = .GlobalEnv)
  assign('vars_RZ_Reg', vars_RZ_Reg, envir = .GlobalEnv)
  assign('vars_Erschl', vars_Erschl, envir = .GlobalEnv)
  assign('var_HUR', var_HUR, envir = .GlobalEnv)
  assign('vars_UA', vars_UA, envir = .GlobalEnv)
  assign('vars_UA4', vars_UA4, envir = .GlobalEnv)
  assign('vars_UA_other', vars_UA_other, envir = .GlobalEnv)
  assign('vars_UF', vars_UF, envir = .GlobalEnv)
  assign('var_Orga', var_Orga, envir = .GlobalEnv)
  assign('vars_Uk', vars_Uk, envir = .GlobalEnv)
  assign('vars_Vk', vars_Vk, envir = .GlobalEnv)
  assign('vars_Beg', vars_Beg, envir = .GlobalEnv)
  assign('vars_Ausg', vars_Ausg, envir = .GlobalEnv)
  assign('vars_RD', vars_RD, envir = .GlobalEnv)
}

dist_tourist <- function(x, y){
  
  suppressWarnings(x <- as.numeric(x))
  suppressWarnings(y <- as.numeric(y))
  
  # 1. Frequency
  dist_RH_VJ <- abs(x[var_RH_VJ]-y[var_RH_VJ])
  dist_RH_2J <- abs(x[var_RH_2J]-y[var_RH_2J])
  dist_RH_3J <- abs(x[var_RH_3J]-y[var_RH_3J])
  
  # 2. Destination
  # 2.1 Major region
  dist_RZ_Reg <- as.numeric(proxy::dist(list(as.numeric(x[vars_RZ_Reg]), as.numeric(y[vars_RZ_Reg])), method = "Jaccard"))
  # 2.2 Popularity
  dist_Erschl <- abs(x[vars_Erschl]-y[vars_Erschl])
  # 2.3 Destinaion HUR
  dist_HUR <- dist_mat_RZ[as.numeric(x[var_HUR]), as.numeric(y[var_HUR])]
  
  dist_RZ <- mean(as.numeric(c(dist_RZ_Reg, dist_Erschl, dist_HUR)), na.rm = TRUE)
  
  # 3. Activities
  # Category has 5 sub-categories
  # -> Mean of distances in OB4
  UA_vec_0 <- abs(x[vars_UA]-y[vars_UA]) # Vector containing all absolute distances, contains 5 values for OB4
  # Determine means based on up to 5 values (OB4); copy value from other categories
  UA_vec <- c(UA_vec_0[vars_UA_other], mean(UA_vec_0[vars_UA4], na.rm = TRUE))
  dist_UA <- mean(UA_vec, na.rm = TRUE)
  
  # Value of 1 is only attained if the discrepancy between all variables is maximal
  # Thus, values are unlikely to be 1
  # -> Normalize with max of 1000 observations (about 0.86, see following)
  dist_UA <- if_else(dist_UA > 0.86,
                     0.86,
                     dist_UA)
  dist_UA <- dist_UA/0.86
  
  
  
  # 4. Holiday type
  dist_UF_2 <- as.numeric(proxy::dist(list(as.numeric(x[vars_UF]), as.numeric(y[vars_UF])), method = "Jaccard"))
  # Two observations have 0s only -> correct NAs to mean of 1k observations (see code in 1_1)
  # Observations with NA only are not corrected
  dist_UF_2 <- if_else(is.na(dist_UF_2) & sum(c(as.numeric(x[vars_UF]), as.numeric(y[vars_UF])), na.rm = TRUE) == 0,
                       0.8,
                       dist_UF_2)
  
  
  # 4.2 Type of organization
  # Binary (No NAs)
  dist_Of <- abs(x[var_Orga] - y[var_Orga])
  
  
  
  # 5. Accomodation
  # Jaccard
  # No NAs
  dist_Uk <- as.numeric(proxy::dist(list(as.numeric(x[vars_Uk]), as.numeric(y[vars_Uk])), method = "Jaccard"))
  # Two observations have 0s only -> correct with mean of 1k observations
  dist_Uk <- if_else(is.na(dist_Uk),
                     0.65,
                     dist_Uk)
  
  # 6. Mean of transportation
  # Jaccard
  # No NAs
  dist_Vk <- as.numeric(proxy::dist(list(as.numeric(x[vars_Vk]), as.numeric(y[vars_Vk])), method = "Jaccard"))
  # Two observations have 0s only -> correct with mean of 1k observations
  dist_Vk <- if_else(is.na(dist_Vk),
                     0.68,
                     dist_Vk)
  
  # 7. Companion
  # Manhattan for all variables
  # all variables are betweeen 0 and 1
  Beg_vec <- abs(x[vars_Beg]-y[vars_Beg]) # Vectore containing all absolute distances 
  dist_Beg <- mean(Beg_vec, na.rm = TRUE)
  
  # 8. Expenses
  Ausg_vec <- abs(x[vars_Ausg]-y[vars_Ausg]) # Vectore containing all absolute distances
  dist_Ausg <- mean(Ausg_vec, na.rm = TRUE)
  
  # 9. Duration
  RD_vec <- abs(x[vars_RD]-y[vars_RD]) # Vectore containing all absolute distance
  dist_RD <- mean(RD_vec, na.rm = TRUE)
  
  dist_oberbereiche <- c(dist_RH_VJ, dist_RH_2J, dist_RH_3J, 
                         dist_RZ, dist_UA, 
                         dist_UF_2, dist_Of, 
                         dist_Uk, dist_Vk, dist_Beg, dist_Ausg, dist_RD)
  # Sum up with same weights for every (super-)category
  dist <- weighted.mean(dist_oberbereiche, 
                        w = c(1/3, 1/3, 1/3,
                              2, 1, # Destination has a weight of 2
                              1/2, 1/2,
                              1, 1, 1, 1, 1),
                        na.rm = TRUE)
  return(dist)
  
}


# Function to filter data so that every year appears with the same frequency
# Argument data_prepared: output of read_and_prepare_data
filter_travel_year <- function(data_prepared){
  
  set.seed(1234)
  n_year <- min(table(data_prepared$travel_year))
  
  result <- data.frame()
  for(i in unique(data_prepared$travel_year)){
    
    dat_year <- data_prepared %>%
      filter(travel_year == i)
    
    dat_sample <- dat_year[sample(1:nrow(dat_year), size = n_year, replace = FALSE),]
    
    result <- rbind(result, dat_sample)
    
  }
  
  return(result)
}

# Function to re-assign cluster labels:
reassign_cluster_labels <- function(analysis_data, years = "all") {
  
  # All travel years:
  if (years == "all") {
    analysis_data <- analysis_data %>%
      mutate(clustering = case_when(clustering_old == 1 ~ 3,
                                    clustering_old == 2 ~ 1,
                                    clustering_old == 3 ~ 4,
                                    clustering_old == 4 ~ 5,
                                    clustering_old == 5 ~ 2)) %>%
      mutate(clustering_fuzzy = case_when(clustering_fuzzy_old == "1" ~ "3",
                                          clustering_fuzzy_old == "2" ~ "1",
                                          clustering_fuzzy_old == "3" ~ "4",
                                          clustering_fuzzy_old == "4" ~ "5",
                                          clustering_fuzzy_old == "5" ~ "2",
                                          clustering_fuzzy_old == "1/2" ~ "1/3",
                                          clustering_fuzzy_old == "1/3" ~ "3/4",
                                          clustering_fuzzy_old == "1/4" ~ "3/5",
                                          clustering_fuzzy_old == "1/5" ~ "2/3",
                                          clustering_fuzzy_old == "2/3" ~ "1/4",
                                          clustering_fuzzy_old == "2/4" ~ "1/5",
                                          clustering_fuzzy_old == "2/5" ~ "1/2",
                                          clustering_fuzzy_old == "3/4" ~ "4/5",
                                          clustering_fuzzy_old == "3/5" ~ "2/4",
                                          clustering_fuzzy_old == "4/5" ~ "2/5")) %>%
      mutate(clustering_fuzzy_detailed = case_when(clustering_fuzzy_detailed_old == "1" ~ "3",
                                                   clustering_fuzzy_detailed_old == "1/2" ~ "3/1",
                                                   clustering_fuzzy_detailed_old == "1/3" ~ "3/4",
                                                   clustering_fuzzy_detailed_old == "1/4" ~ "3/5",
                                                   clustering_fuzzy_detailed_old == "1/5" ~ "3/2",
                                                   clustering_fuzzy_detailed_old == "2" ~ "1",
                                                   clustering_fuzzy_detailed_old == "2/1" ~ "1/3",
                                                   clustering_fuzzy_detailed_old == "2/3" ~ "1/4",
                                                   clustering_fuzzy_detailed_old == "2/4" ~ "1/5",
                                                   clustering_fuzzy_detailed_old == "2/5" ~ "1/2",
                                                   clustering_fuzzy_detailed_old == "3" ~ "4",
                                                   clustering_fuzzy_detailed_old == "3/1" ~ "4/3",
                                                   clustering_fuzzy_detailed_old == "3/2" ~ "4/1",
                                                   clustering_fuzzy_detailed_old == "3/4" ~ "4/5",
                                                   clustering_fuzzy_detailed_old == "3/5" ~ "4/2",
                                                   clustering_fuzzy_detailed_old == "4" ~ "5",
                                                   clustering_fuzzy_detailed_old == "4/1" ~ "5/3",
                                                   clustering_fuzzy_detailed_old == "4/2" ~ "5/1",
                                                   clustering_fuzzy_detailed_old == "4/3" ~ "5/4",
                                                   clustering_fuzzy_detailed_old == "4/5" ~ "5/2",
                                                   clustering_fuzzy_detailed_old == "5" ~ "2",
                                                   clustering_fuzzy_detailed_old == "5/1" ~ "2/3",
                                                   clustering_fuzzy_detailed_old == "5/2" ~ "2/1",
                                                   clustering_fuzzy_detailed_old == "5/3" ~ "2/4",
                                                   clustering_fuzzy_detailed_old == "5/4" ~ "2/5"))
  }
  
  # 1983 - 1990:
  if (years == "198390") {
    analysis_data <- analysis_data %>%
      mutate(clustering = case_when(clustering_old == 1 ~ 1,
                                    clustering_old == 2 ~ 3,
                                    clustering_old == 3 ~ 2,
                                    clustering_old == 4 ~ 4,
                                    clustering_old == 5 ~ 5)) %>%
      mutate(clustering_fuzzy = case_when(clustering_fuzzy_old == "1" ~ "1",
                                          clustering_fuzzy_old == "2" ~ "3",
                                          clustering_fuzzy_old == "3" ~ "2",
                                          clustering_fuzzy_old == "4" ~ "4",
                                          clustering_fuzzy_old == "5" ~ "5",
                                          clustering_fuzzy_old == "1/2" ~ "1/3",
                                          clustering_fuzzy_old == "1/3" ~ "1/2",
                                          clustering_fuzzy_old == "1/4" ~ "1/4",
                                          clustering_fuzzy_old == "1/5" ~ "1/5",
                                          clustering_fuzzy_old == "2/3" ~ "2/3",
                                          clustering_fuzzy_old == "2/4" ~ "3/4",
                                          clustering_fuzzy_old == "2/5" ~ "3/5",
                                          clustering_fuzzy_old == "3/4" ~ "2/4",
                                          clustering_fuzzy_old == "3/5" ~ "2/5",
                                          clustering_fuzzy_old == "4/5" ~ "4/5")) %>%
      mutate(clustering_fuzzy_detailed = case_when(clustering_fuzzy_detailed_old == "1" ~ "1",
                                                   clustering_fuzzy_detailed_old == "1/2" ~ "1/3",
                                                   clustering_fuzzy_detailed_old == "1/3" ~ "1/2",
                                                   clustering_fuzzy_detailed_old == "1/4" ~ "1/4",
                                                   clustering_fuzzy_detailed_old == "1/5" ~ "1/5",
                                                   clustering_fuzzy_detailed_old == "2" ~ "3",
                                                   clustering_fuzzy_detailed_old == "2/1" ~ "3/1",
                                                   clustering_fuzzy_detailed_old == "2/3" ~ "3/2",
                                                   clustering_fuzzy_detailed_old == "2/4" ~ "3/4",
                                                   clustering_fuzzy_detailed_old == "2/5" ~ "3/5",
                                                   clustering_fuzzy_detailed_old == "3" ~ "2",
                                                   clustering_fuzzy_detailed_old == "3/1" ~ "2/1",
                                                   clustering_fuzzy_detailed_old == "3/2" ~ "2/3",
                                                   clustering_fuzzy_detailed_old == "3/4" ~ "2/4",
                                                   clustering_fuzzy_detailed_old == "3/5" ~ "2/5",
                                                   clustering_fuzzy_detailed_old == "4" ~ "4",
                                                   clustering_fuzzy_detailed_old == "4/1" ~ "4/1",
                                                   clustering_fuzzy_detailed_old == "4/2" ~ "4/3",
                                                   clustering_fuzzy_detailed_old == "4/3" ~ "4/2",
                                                   clustering_fuzzy_detailed_old == "4/5" ~ "4/5",
                                                   clustering_fuzzy_detailed_old == "5" ~ "5",
                                                   clustering_fuzzy_detailed_old == "5/1" ~ "5/1",
                                                   clustering_fuzzy_detailed_old == "5/2" ~ "5/3",
                                                   clustering_fuzzy_detailed_old == "5/3" ~ "5/2",
                                                   clustering_fuzzy_detailed_old == "5/4" ~ "5/4"))
  }
  
  # 2011 - 2018:
  if (years == "201118") {
    analysis_data <- analysis_data %>%
      mutate(clustering = case_when(clustering_old == 1 ~ 5,
                                    clustering_old == 2 ~ 3,
                                    clustering_old == 3 ~ 1,
                                    clustering_old == 4 ~ 2,
                                    clustering_old == 5 ~ 4)) %>%
      mutate(clustering_fuzzy = case_when(clustering_fuzzy_old == "1" ~ "5",
                                          clustering_fuzzy_old == "2" ~ "3",
                                          clustering_fuzzy_old == "3" ~ "1",
                                          clustering_fuzzy_old == "4" ~ "2",
                                          clustering_fuzzy_old == "5" ~ "4",
                                          clustering_fuzzy_old == "1/2" ~ "3/5",
                                          clustering_fuzzy_old == "1/3" ~ "1/5",
                                          clustering_fuzzy_old == "1/4" ~ "2/5",
                                          clustering_fuzzy_old == "1/5" ~ "4/5",
                                          clustering_fuzzy_old == "2/3" ~ "1/3",
                                          clustering_fuzzy_old == "2/4" ~ "2/3",
                                          clustering_fuzzy_old == "2/5" ~ "3/4",
                                          clustering_fuzzy_old == "3/4" ~ "1/2",
                                          clustering_fuzzy_old == "3/5" ~ "1/4",
                                          clustering_fuzzy_old == "4/5" ~ "2/4")) %>%
      mutate(clustering_fuzzy_detailed = case_when(clustering_fuzzy_detailed_old == "1" ~ "5",
                                                   clustering_fuzzy_detailed_old == "1/2" ~ "5/3",
                                                   clustering_fuzzy_detailed_old == "1/3" ~ "5/1",
                                                   clustering_fuzzy_detailed_old == "1/4" ~ "5/2",
                                                   clustering_fuzzy_detailed_old == "1/5" ~ "5/4",
                                                   clustering_fuzzy_detailed_old == "2" ~ "3",
                                                   clustering_fuzzy_detailed_old == "2/1" ~ "3/5",
                                                   clustering_fuzzy_detailed_old == "2/3" ~ "3/1",
                                                   clustering_fuzzy_detailed_old == "2/4" ~ "3/2",
                                                   clustering_fuzzy_detailed_old == "2/5" ~ "3/4",
                                                   clustering_fuzzy_detailed_old == "3" ~ "1",
                                                   clustering_fuzzy_detailed_old == "3/1" ~ "1/5",
                                                   clustering_fuzzy_detailed_old == "3/2" ~ "1/3",
                                                   clustering_fuzzy_detailed_old == "3/4" ~ "1/2",
                                                   clustering_fuzzy_detailed_old == "3/5" ~ "1/4",
                                                   clustering_fuzzy_detailed_old == "4" ~ "2",
                                                   clustering_fuzzy_detailed_old == "4/1" ~ "2/5",
                                                   clustering_fuzzy_detailed_old == "4/2" ~ "2/3",
                                                   clustering_fuzzy_detailed_old == "4/3" ~ "2/1",
                                                   clustering_fuzzy_detailed_old == "4/5" ~ "2/4",
                                                   clustering_fuzzy_detailed_old == "5" ~ "4",
                                                   clustering_fuzzy_detailed_old == "5/1" ~ "4/5",
                                                   clustering_fuzzy_detailed_old == "5/2" ~ "4/3",
                                                   clustering_fuzzy_detailed_old == "5/3" ~ "4/1",
                                                   clustering_fuzzy_detailed_old == "5/4" ~ "4/2"))
  }
  
  return(analysis_data)
}


# Function to prepare data for statistical analysis:
prepare_analysis_data <- function(analysis_data, clustering_result,
                                  clustering_data = NULL, all_data = FALSE,
                                  membership_threshold = 0.5, years = "all") {
  
  # Select the observations used for clustering is all_data = FALSE:
  if (all_data == FALSE) {
    set.seed(1234) #use same seed as in clustering code!
    analysis_data <- filter_travel_year(analysis_data)
  }

  # Create cluster predictions if more data are used:
  if (all_data == TRUE) {
 
    clustering_result <- predict(object = clustering_result,
                                 newdata = clustering_data)
  }

  # Add information about cluster assignment to the data:

  # General assignment:
  analysis_data$clustering_old <- clustering_result$clustering

  # Core and fuzzy clusters:
  fuzzy_levels <- c("1", "2", "3", "4", "5", "1/2", "1/3", "1/4", "1/5",
                    "2/3", "2/4", "2/5", "3/4", "3/5", "4/5")
  fuzzy_levels_detailed <- c("1", "1/2", "1/3", "1/4", "1/5", "2", "2/1", "2/3",
                             "2/4", "2/5", "3", "3/1", "3/2", "3/4", "3/5", "4",
                             "4/1", "4/2", "4/3", "4/5", "5", "5/1", "5/2",
                             "5/3", "5/4")
  df_clustering_membScores <- clustering_result$membership_scores
  df_clustering_membScores <- df_clustering_membScores %>%
    mutate("largest_score_val" = apply(X = df_clustering_membScores, MARGIN = 1,
                                   FUN = max),
           "largest_score" = apply(X = df_clustering_membScores, MARGIN = 1,
                                       FUN = which.max),
           "secLargest_score_val" = apply(X = df_clustering_membScores, MARGIN = 1,
                                      FUN = nth, k = 2, descending = TRUE),
           "secLargest_score" = apply(X = df_clustering_membScores, MARGIN = 1,
                                       FUN = nth, k = 2, descending = TRUE,
                                       index.return = TRUE))
  cluster_combinations <- ifelse(df_clustering_membScores$largest_score <
                                   df_clustering_membScores$secLargest_score,
                                 paste(df_clustering_membScores$largest_score,
                                       df_clustering_membScores$secLargest_score,
                                       sep = "/"),
                                 paste(df_clustering_membScores$secLargest_score,
                                       df_clustering_membScores$largest_score,
                                       sep = "/"))
  cluster_combinations_detailed <- paste(df_clustering_membScores$largest_score,
                                         df_clustering_membScores$secLargest_score,
                                         sep = "/")
  df_clustering_membScores <- df_clustering_membScores %>%
    mutate(clustering_fuzzy = factor(if_else(largest_score_val >
                                                   membership_threshold,
                                                as.character(largest_score),
                                                cluster_combinations),
                                     levels = fuzzy_levels),
           clustering_fuzzy_detailed = factor(if_else(largest_score_val >
                                                       membership_threshold,
                                                     as.character(largest_score),
                                                     cluster_combinations_detailed),
                                             levels = fuzzy_levels_detailed))
  analysis_data$clustering_fuzzy_old <- df_clustering_membScores$clustering_fuzzy
  analysis_data$clustering_fuzzy_detailed_old <- df_clustering_membScores$clustering_fuzzy_detailed
  analysis_data$membership <- df_clustering_membScores$largest_score_val

  # analysis_data = readRDS("uni.rds")
  
  # Prepare variables for further analysis:
  analysis_data <- analysis_data %>%
    mutate(S_Haushaltsgroesse_factor = as.factor(S_Haushaltsgroesse),
           S_Haushaltsgroesse_14plus_factor = as.factor(S_Haushaltsgroesse_14plus),
           travel_year_factor = as.factor(travel_year)) %>%
    mutate_if(~(all(!is.na(unique(.))) && length(unique(.)) < 5),
              as.factor)
  colnames(analysis_data)[colnames(analysis_data) == "RZ_Region_SÜDOSTEUROPA"] <-
    "RZ_Region_SUEDOSTEUROPA"
  colnames(analysis_data)[colnames(analysis_data) == "RZ_Region_SÜDEUROPA"] <-
    "RZ_Region_SUEDEUROPA"
  
  # Change order of clusters:
  analysis_data <- analysis_data %>%
    reassign_cluster_labels(years = years) %>%
    mutate(clustering_fuzzy = factor(clustering_fuzzy, levels = fuzzy_levels)) %>%
    mutate(clustering_fuzzy_detailed = factor(clustering_fuzzy_detailed,
                                              levels = fuzzy_levels_detailed)) %>%
    dplyr::select(-clustering_old, -clustering_fuzzy_old,
                  -clustering_fuzzy_detailed_old)
  
  return(analysis_data)
}


################################################################################

# Visualizations:

## ggplot theme:
theme <- theme_minimal() +
  theme(text = element_text(size = 16), axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.key.width = unit(2, "lines"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        strip.text = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 16), legend.text.align = 0,
        strip.placement = "outside", strip.background = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)))

plot_destination_per_cluster <- function(data, type = "hard") {
  
  #Visualise
  cluster_variable <- sym(ifelse(type == "hard", "clustering", "clustering_fuzzy"))
  
  if (type == "fuzzy") {
    levels(data$clustering_fuzzy) <- c("1/1", "2/2", "3/3", "4/4", "5/5",
                                       "1/2*", "1/3*", "1/4*", "1/5*", "2/1*",
                                       "2/3*", "2/4*", "2/5*", "3/5*", "4/5*")
  }
  data$HUR_Reiseziel_Clustering <- factor(data$HUR_Reiseziel_Clustering,
                                  levels = c("MITTELEUROPA", "WESTEUROPA", "NORDEUROPA",
                                    "OSTEUROPA", "SUEDOSTEUROPA", "SUEDEUROPA",
                                    "SONSTMITTELMEER","FERNREISEZIELE"))
  levels(data$HUR_Reiseziel_Clustering) <- c("Central Europe", "Western Europe",
                                             "Northern Europe", "Eastern Europe",
                                             "SoutheasternEurope",
                                             "Southern Europe",
                                             "Further Mediterranean Region",
                                             "Long-Haul Destination")
  
  plot <- ggplot(data = subset(data, !is.na(HUR_Reiseziel_Clustering)),
                 aes(x = !!cluster_variable, fill = HUR_Reiseziel_Clustering)) +
    geom_bar(position = "fill") + theme +
    xlab("Cluster") +
    ylab("Relative frequency") +
   scale_fill_manual(values = rev(diverge_hcl(8, h = c(246, 70), c = 50, l = c(50, 90))) 
                      ) +
    guides(fill=guide_legend(title="Travel Destination"))
  return(plot)
}


starplot <- function(data) {
  
  library(igraph)
  
  # Generate frame for plot:
  star_plot <- make_full_graph(5)
  layout <- layout.circle(star_plot)[c(3, 2, 1, 5, 4), ]
  
  # Construct plot data:
  node_data <- data.frame("Cluster" = 1:5,
                          "Label" = paste0(scales::comma(as.numeric(table(data$clustering))), 
                                           "\n(Cluster ", 1:5, ")"),
                          "Frequency" = as.numeric(table(data$clustering)),
                          "Dim1" = layout[, 1], "Dim2" = layout[, 2])
  colors <- c("#332288", "#88CCEE", "#44AA99", "#bbbbbb", "#CC6677")
  
  edge_data <- get.data.frame(star_plot)
  edge_data$from.x <- node_data$Dim1[match(edge_data$from, node_data$Cluster)]  #  Match the from locations from the node data.frame we previously connected
  edge_data$from.y <- node_data$Dim2[match(edge_data$from, node_data$Cluster)]
  edge_data$to.x <- node_data$Dim1[match(edge_data$to, node_data$Cluster)]  #  Match the to locations from the node data.frame we previously connected
  edge_data$to.y <- node_data$Dim2[match(edge_data$to, node_data$Cluster)]
  edge_data$Weight <- as.numeric(table(analysis_data$clustering_fuzzy)[6:15])
  edge_data <- edge_data %>%
    mutate(pos.x = (from.x + to.x) / 2, pos.y = (from.y + to.y) / 2)
  
  # Generate plot:
  # browser()
  star_plot <- ggplot() +
    geom_segment(data=edge_data,aes(x=from.x,xend = to.x, y=from.y,yend = to.y,linewidth = Weight),colour="grey90") +
    geom_point(data = node_data,aes(x=Dim1,y=Dim2, size = Frequency,colour = Label)) +
    scale_color_manual(values = colors) +
    geom_point(data = node_data,aes(x=Dim1,y=Dim2, size = Frequency), colour = "black", pch = 21) +
    scale_size(range = c(30, 45)) +
    geom_text(data = node_data,aes(x = Dim1, y = Dim2,label = Label), colour = "white") +
    geom_text(data = edge_data,aes(x = pos.x, y = pos.y, label = paste(scales::comma(Weight), "\n(Clusters ", from, "/", to, ", ", to, "/", from, ")", sep = "")), colour = "black") +
    scale_x_continuous(expand = c(0, 0.2))+  # Expand the x limits 
    scale_y_continuous(expand = c(0, 0.2)) + theme + 
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  return(star_plot)
}

plot_cluster_over_time <- function(data, type = "hard", cluster) {

  # Visualise
  cluster_variable <- sym(ifelse(type == "hard", "clustering", "clustering_fuzzy_detailed"))
  
  # Rename levels of fuzzy clusters:
  if (type == "fuzzy") {
    levels(data$clustering_fuzzy_detailed) <-
      c("1/1", "1/2", "1/3", "1/4", "1/5", "2/2", "2/1", "2/3", "2/4", "2/5",
        "3/3", "3/1", "3/2", "3/4", "3/5", "4/4", "4/1", "4/2", "4/3", "4/5",
        "5/5", "5/1", "5/2", "5/3", "5/4")
  }
  
  # Filter observations if fuzzy clusters should be plotted:
  if (type == "fuzzy") {
    data <- data %>% filter(clustering == cluster)
  }

  # Visualization:
  colors_hard <- c("#332288", "#88CCEE", "#44AA99", "#bbbbbb", "#CC6677")
  values_hard <- colors_hard[c(cluster, c(1:5)[!c(1:5) %in% c(cluster)])]
  values_fuzzy <- list(c("Cluster 1/1" = "#332288", "Cluster 1/2" = "#654da5",
                         "Cluster 1/3" = "#927ac3", "Cluster 1/4" = "#beaae1",
                         "Cluster 1/5" = "#eadcff"),
                       c("Cluster 2/2" = "#88CCEE", "Cluster 1/2" = "#a9d9f2",
                         "Cluster 2/3" = "#c7e5f7", "Cluster 2/4" = "#e3f2fb",
                         "Cluster 2/5" = "#f1f9fd"),
                       c("Cluster 3/3" = "#44AA99", "Cluster 1/3" = "#79bfb2",
                         "Cluster 2/3" = "#a7d5cb", "Cluster 3/4" = "#d3eae5",
                         "Cluster 3/5" = "#e9f4f2"),
                       c("Cluster 4/4" = "#bbbbbb", "Cluster 1/4" = "#d0d0d0",
                         "Cluster 2/4" = "#e5e5e5", "Cluster 3/4" = "#f0f0f0",
                         "Cluster 4/5" = "#FAFAFA"),
                       c("Cluster 5/5" = "#CC6677", "Cluster 1/5" = "#dc8d97",
                         "Cluster 2/5" = "#eab2b9", "Cluster 3/5" = "#f6d9db",
                         "Cluster 4/5" = "#fbeced"))
  if (type == "hard") {
    values <- values_hard
    ref_level <- as.character(cluster)
  }
  if (type == "fuzzy") {
    values <- as.character(values_fuzzy[[cluster]])
    ref_level <- paste0(as.character(cluster), "/", as.character(cluster))
  }
  
  plot <- ggplot(data = data,
                 mapping = aes(x = travel_year, fill = relevel(as.factor(!!cluster_variable),
                                                               ref = ref_level))) +
    geom_bar(position = position_fill(reverse = TRUE)) + theme + theme(legend.position = "top") +
    xlab("Travel year") + ylab("Relative frequency") + labs(fill = "Cluster") +
    scale_fill_manual(values = values) +
    scale_x_continuous(breaks = seq(from = 1985, to = 2015, by = 5))
  return(plot)
}

fit_models <- function(data, type = "hard", cluster = NULL, drop_low_obs = TRUE) {

  # Prepare data for modelling:
  data_all <- data %>%
    mutate(period = travel_year, age = S_Alter,
           S_Geschlecht = droplevels(S_Geschlecht),
           S_Kinder_0_bis_5_binaer = droplevels(S_Kinder_0_bis_5_binaer),
           S_Bildung = droplevels(S_Bildung),
           S_Wohnortgroesse = droplevels(S_Wohnortgroesse),
           S_Haushaltsgroesse = as.factor(S_Haushaltsgroesse))
  
  data_all <- data_all %>%
    select(clustering, clustering_fuzzy, clustering_fuzzy_detailed,
           period, age, S_Geschlecht,
           S_Kinder_0_bis_5_binaer, S_Wohnortgroesse, S_Bildung,
           S_Einkommen_HH, S_Haushaltsgroesse)
  
  # Rename levels of fuzzy clusters:
  if (type == "fuzzy") {
    levels(data_all$clustering_fuzzy_detailed) <-
      c("1/1", "1/2", "1/3", "1/4", "1/5", "2/2", "2/1", "2/3", "2/4", "2/5",
        "3/3", "3/1", "3/2", "3/4", "3/5", "4/4", "4/1", "4/2", "4/3", "4/5",
        "5/5", "5/1", "5/2", "5/3", "5/4")
  }
  
  # Filter data if fuzzy clusters should be modelled:
  if (type == "fuzzy") {
    data_all <- data_all %>% filter(clustering == cluster) %>% droplevels()
  }
  
  # Exclude all categories with lower than 1000 observations:
  if (type == "fuzzy" & drop_low_obs == TRUE) {
    data_all <- data_all %>% group_by(clustering_fuzzy_detailed) %>%
      filter(n() >= 1000) %>% ungroup %>% droplevels()
  }
  
  # Focus variable:
  cluster_variable <- ifelse(type == "hard", "clustering", "clustering_fuzzy_detailed")
  
  # Create a dummy variable for each cluster
  data_all <- fastDummies::dummy_columns(data_all, cluster_variable)
  
  # Actual modelling for each cluster:
  models <- list()
  n_cluster <- length(table(data_all[, cluster_variable]))
  for(i in 1:n_cluster){
    
    print(i)
    data_all$cluster_i <- unlist(data_all[, paste0(cluster_variable, "_",
                                            names(table(data_all[, cluster_variable]))[i])])
    
    model_cluster_i <- bam(formula = cluster_i ~ te(period, age, k = c(10, 10),
                                                    bs = "ps") +
                             S_Geschlecht + S_Kinder_0_bis_5_binaer +
                             S_Wohnortgroesse +
                             S_Bildung + s(S_Einkommen_HH, bs = "ps", k = 10) +
                             S_Haushaltsgroesse,
                           family = binomial(link = "logit"), data = data_all)
    
    models <- append(models, list(mod = model_cluster_i))
  }
  names(models) <- paste0("Cluster ", names(table(data_all[, cluster_variable])))
  return(models)
} 

# Function in order to calculate AUC value of a model:
calculate_auc <- function(data, type = "hard", cluster = NULL, n_cluster = 5,
                          seed = 1234) {
  
  # Exclude all categories with lower than 1000 observations:
  if (type == "fuzzy") {
    data <- data %>% group_by(clustering_fuzzy_detailed) %>%
      filter(clustering == cluster) %>% filter(n() >= 1000) %>%
      ungroup %>% droplevels()
  }
  
  # Ten-fold cross validation
  set.seed(seed)
  folds <- cut(seq(1, nrow(data)), breaks = 10, labels = FALSE)
  prediction_list <- vector("list", n_cluster)
  label_list <- vector("list", n_cluster)
  
  for (i in 1:10) {
    print(i)
    testIndices <- which(folds == i,arr.ind = TRUE)
    data_test <- data[testIndices, ] %>%
      mutate(period = travel_year, age = S_Alter,
             S_Geschlecht = droplevels(S_Geschlecht),
             S_Kinder_0_bis_5_binaer = droplevels(S_Kinder_0_bis_5_binaer),
             S_Bildung = droplevels(S_Bildung),
             S_Wohnortgroesse = droplevels(S_Wohnortgroesse),
             S_Haushaltsgroesse = as.factor(S_Haushaltsgroesse))
    cluster_variable <- ifelse(type == "hard", "clustering", "clustering_fuzzy_detailed")
    data_test <- fastDummies::dummy_columns(data_test, cluster_variable)
    
    data_train <- data[-testIndices, ]
    
    # Estimation of the model:
    models <- fit_models(data = data_train, type = type, cluster = cluster,
                         drop_low_obs = FALSE)
    
    for (l in 1:n_cluster) {
      prediction_list[[l]][[i]] <- as.vector(predict(object = models[[l]], newdata = data_test))
      label_list[[l]][[i]] <- unlist(data_test[, paste0(cluster_variable, "_",
                                          names(table(data_test[, cluster_variable]))[l])])
    }
  }  
  
  # Calculate CV-AUC per cluster model
  auc_list <- list()
  for (l in 1:n_cluster) {
    auc_list[[l]] <- cvAUC(predictions = prediction_list[[l]],
                           labels = label_list[[l]])
  }
  names(auc_list) <- paste0("Cluster ", names(table(data_test[, cluster_variable])))
  return(auc_list)
}
  
plot_models_APC <- function(data, models, type = "hard", cluster = NULL) {
  theme <- theme + theme(legend.position = "top")
  theme_set(theme)
  data$age <- data$S_Alter
  data$period <- data$travel_year
  vlines_cohort <- list("cohort" = c(1946.5,1966.5,1982.5,1994.5))
  
  # Rename levels of fuzzy clusters:
  if (type == "fuzzy") {
    levels(data$clustering_fuzzy_detailed) <-
      c("1/1", "1/2", "1/3", "1/4", "1/5", "2/2", "2/1", "2/3", "2/4", "2/5",
        "3/3", "3/1", "3/2", "3/4", "3/5", "4/4", "4/1", "4/2", "4/3", "4/5",
        "5/5", "5/1", "5/2", "5/3", "5/4")
  }
  
  plot <- plot_jointMarginalAPCeffects4Tourist(model_list = models, dat = data,
                                               type = type, cluster = cluster,
                                               vlines_list = vlines_cohort)
  return(plot)
}

plot_jointMarginalAPCeffects4Tourist <- function (model_list, dat, vlines_list = NULL,
                                          ylab = NULL, ylim = NULL,
                                          type = "hard", cluster = NULL) 
{
  effect <- value <- NULL
  if (!is.null(names(model_list))) {
    model_labels <- names(model_list)
  }
  else {
    model_labels <- paste("model", 1:length(model_list))
  }
  datList_list <- lapply(model_list, function(x) {
    plot_marginalAPCeffects(x, dat, return_plotData = TRUE)
  })
  if (is.null(ylim)) {
    ylim <- lapply(datList_list, function(x) {
      dplyr::bind_rows(x)
    }) %>% dplyr::bind_rows() %>% pull(effect) %>% range()
  }
  used_logLink <- model_list[[1]]$family[[2]] %in% c("log", 
                                                     "logit")
  if (is.null(ylab)) {
    ylab <- ifelse(used_logLink, "Odds Ratio", "Effect")
  }
  gg_age <- gg_period <- gg_cohort <- ggplot()
  dat_age <- lapply(1:length(datList_list), function(i) {
    datList_list[[i]]$dat_age %>% mutate(type = model_labels[i])
  }) %>% dplyr::bind_rows() %>% mutate(type = factor(type, 
                                                     levels = model_labels))
  
  # Visualization:
  colors_hard <- c("#332288", "#88CCEE", "#44AA99", "#bbbbbb", "#CC6677")
  values_hard <- colors_hard[c(cluster, c(1:5)[!c(1:5) %in% c(cluster)])]
  values_fuzzy <- list(c("Cluster 1/1" = "#332288", "Cluster 1/2" = "#654da5",
                         "Cluster 1/3" = "#927ac3", "Cluster 1/4" = "#beaae1",
                         "Cluster 1/5" = "#eadcff"),
                       c("Cluster 2/2" = "#88CCEE", "Cluster 1/2" = "#a9d9f2",
                         "Cluster 2/3" = "#c7e5f7", "Cluster 2/4" = "#e3f2fb",
                         "Cluster 2/5" = "#f1f9fd"),
                       c("Cluster 3/3" = "#44AA99", "Cluster 1/3" = "#79bfb2",
                         "Cluster 2/3" = "#a7d5cb", "Cluster 3/4" = "#d3eae5",
                         "Cluster 3/5" = "#e9f4f2"),
                       c("Cluster 4/4" = "#bbbbbb", "Cluster 1/4" = "#d0d0d0",
                         "Cluster 2/4" = "#e5e5e5", "Cluster 3/4" = "#f0f0f0",
                         "Cluster 4/5" = "#FAFAFA"),
                       c("Cluster 5/5" = "#CC6677", "Cluster 1/5" = "#dc8d97",
                         "Cluster 2/5" = "#eab2b9", "Cluster 3/5" = "#f6d9db",
                         "Cluster 4/5" = "#fbeced"))
  print(type)
  if (type == "hard") {
    values <- values_hard
  }
  if (type == "fuzzy") {
    values <- values_fuzzy[[cluster]][names(values_fuzzy[[cluster]]) %in% names(models)]
  }
  
  if ("age" %in% names(vlines_list)) {
    gg_age <- gg_age + geom_vline(xintercept = vlines_list$age, 
                                  col = gray(0.5), lty = 2) +
      scale_color_manual(values = values)
  }
  gg_age <- gg_age + geom_hline(yintercept = ifelse(used_logLink, 
                                                    1, 0), col = gray(0.3), lty = 2) +
    geom_line(data = dat_age, 
                   aes(x = value, y = effect, col = type), lwd = 1) + xlab("Age") + 
    scale_y_continuous("Odds Ratio", trans = "log2", limits = c(0.2, 7),
                       breaks = 2^c(-2,-1,0,1, 2),
                       labels = c("0.25","0.5","1","2", "4")) +
    theme(legend.title = element_blank()) +
    scale_color_manual(values = values)
  dat_period <- lapply(1:length(datList_list), function(i) {
    datList_list[[i]]$dat_period %>% mutate(type = model_labels[i])
  }) %>% dplyr::bind_rows() %>% mutate(type = factor(type, 
                                                     levels = model_labels))
  if ("period" %in% names(vlines_list)) {
    gg_period <- gg_period + geom_vline(xintercept = vlines_list$period, 
                                        col = gray(0.5), lty = 2) +
      scale_color_manual(values = values)
  }
  gg_period <- gg_period + geom_hline(yintercept = ifelse(used_logLink, 
                                                          1, 0), col = gray(0.3), lty = 2) +
    geom_line(data = dat_period, 
                          aes(x = value, y = effect, col = type), lwd = 1) + xlab("Period") + 
    scale_y_continuous("Odds Ratio", trans = "log2", limits = c(0.2, 7),
                       breaks = 2^c(-2,-1,0,1, 2),
                       labels = c("0.25","0.5","1","2", "4")) +
    theme(legend.title = element_blank(), 
                                                                                       axis.title.y = element_blank(), axis.text.y = element_blank(), 
                                                                                       axis.ticks.y = element_blank()) +
    scale_color_manual(values = values)
  dat_cohort <- lapply(1:length(datList_list), function(i) {
    datList_list[[i]]$dat_cohort %>% mutate(type = model_labels[i])
  }) %>% dplyr::bind_rows() %>% mutate(type = factor(type, 
                                                     levels = model_labels))
  if ("cohort" %in% names(vlines_list)) {
    gg_cohort <- gg_cohort + geom_vline(xintercept = vlines_list$cohort, 
                                        col = gray(0.5), lty = 2) +
      scale_color_manual(values = values)
  }
  gg_cohort <- gg_cohort + geom_hline(yintercept = ifelse(used_logLink, 
                                                          1, 0), col = gray(0.3), lty = 2) +
    geom_line(data = dat_cohort, 
                aes(x = value, y = effect, col = type), lwd = 1) + xlab("Cohort") + 
    scale_y_continuous("Odds Ratio", trans = "log2", limits = c(0.2, 7),
                       breaks = 2^c(-2,-1,0,1, 2),
                       labels = c("0.25","0.5","1","2", "4")) +
    scale_x_continuous(limits = c(1939, 2001)) +
    theme(legend.title = element_blank(), 
                                                                                       axis.title.y = element_blank(), axis.text.y = element_blank(), 
                                                                                       axis.ticks.y = element_blank()) +
    scale_color_manual(values = values)
  if (length(model_list) == 1) {
    gg_age <- gg_age + scale_color_manual(values = gray(0.2))
    gg_period <- gg_period + scale_color_manual(values = gray(0.2))
    gg_cohort <- gg_cohort + scale_color_manual(values = gray(0.2))
  }
  ggpubr::ggarrange(plotlist = list(gg_age, gg_period, gg_cohort), 
                    legend = ifelse(length(model_list) == 1, "none", "bottom"), 
                    common.legend = TRUE, ncol = 3, widths = c(0.36, 0.32, 
                                                               0.32))
}

plot_income_effects <- function(models, type = "hard", cluster = NULL,
                                plot_ci = TRUE, select = 2, alpha = 0.05,
                                ylim = NULL, method_expTransform = "simple") {
  
  colors_hard <- c("#332288", "#88CCEE", "#44AA99", "#bbbbbb", "#CC6677")
  values_hard <- colors_hard[c(cluster, c(1:5)[!c(1:5) %in% c(cluster)])]
  values_fuzzy <- list(c("Cluster 1/1" = "#332288", "Cluster 1/2" = "#654da5",
                         "Cluster 1/3" = "#927ac3", "Cluster 1/4" = "#beaae1",
                         "Cluster 1/5" = "#eadcff"),
                       c("Cluster 2/2" = "#88CCEE", "Cluster 1/2" = "#a9d9f2",
                         "Cluster 2/3" = "#c7e5f7", "Cluster 2/4" = "#e3f2fb",
                         "Cluster 2/5" = "#f1f9fd"),
                       c("Cluster 3/3" = "#44AA99", "Cluster 1/3" = "#79bfb2",
                          "Cluster 2/3" = "#a7d5cb", "Cluster 3/4" = "#d3eae5",
                         "Cluster 3/5" = "#e9f4f2"),
                       c("Cluster 4/4" = "#bbbbbb", "Cluster 1/4" = "#d0d0d0",
                         "Cluster 2/4" = "#e5e5e5", "Cluster 3/4" = "#f0f0f0",
                         "Cluster 4/5" = "#FAFAFA"),
                       c("Cluster 5/5" = "#CC6677", "Cluster 1/5" = "#dc8d97",
                         "Cluster 2/5" = "#eab2b9", "Cluster 3/5" = "#f6d9db",
                         "Cluster 4/5" = "#fbeced"))
  if (type == "hard") {
    values <- values_hard
  }
  if (type == "fuzzy") {
    values <- values_fuzzy[[cluster]][names(values_fuzzy[[cluster]]) %in% names(models)]
  }
  
  plot_dat <- NULL
  for (l in seq_along(models)) {
    
    used_logLink <- (models[[l]]$family[[2]] %in% c("log","logit")) |
      grepl("Ordered Categorical", models[[l]]$family[[1]])
    ylab         <- ifelse(used_logLink, "Odds Ratio", "Effect")
    
    plotObject <- get_plotGAMobject(models[[l]])
    
    plotObject <- plotObject[[select]]
    plot_dat_new  <- data.frame(x  = plotObject$x,
                                y  = plotObject$fit,
                                se = plotObject$se / plotObject$se.mult) %>% 
      mutate(CI_lower = y - qnorm(1 - alpha/2)*se,
             CI_upper = y + qnorm(1 - alpha/2)*se)
    
    if (used_logLink) {
      
      # Transform the point estimates
      plot_dat_new <- plot_dat_new %>%
        mutate(y_exp = exp(y)) %>%
        select(-y) %>% 
        dplyr::rename(y = y_exp)
      
      # Transform the confidence intervals
      if (plot_ci) {
        
        if (method_expTransform == "simple") {
          
          plot_dat_new <- plot_dat_new %>%
            mutate(se_exp       = exp(se),
                   CI_lower_exp = exp(CI_lower),
                   CI_upper_exp = exp(CI_upper)) %>%
            select(-se, -CI_lower, -CI_upper) %>% 
            dplyr::rename(se = se_exp, CI_lower = CI_lower_exp, CI_upper = CI_upper_exp)
          
        } else { # Method_expTransform == "delta"
          
          plot_dat_new <- plot_dat_new %>%
            mutate(se_exp  = sqrt(se^2 * y^2)) %>%
            mutate(CI_lower_exp = y - qnorm(1 - alpha/2) * se_exp,
                   CI_upper_exp = y + qnorm(1 - alpha/2) * se_exp) %>% 
            select(-se, -CI_lower, -CI_upper) %>% 
            dplyr::rename(se = se_exp, CI_lower = CI_lower_exp, CI_upper = CI_upper_exp)
          
          # Correct negative CI_lower borders
          if (any(plot_dat_new$CI_lower < 0)) {
            warning("Note: After the delta method transformation some values of the
              lower confidence interval border resulted were negative. These
              values were set to 0.01")
            plot_dat_new$CI_lower[plot_dat_new$CI_lower < 0] <- 0.01
          }
        }
      }
    }
    
    # If 'ylim' is set and the CIs exceed it, trim them accordingly
    if (!is.null(ylim)) {
      plot_dat_new$CI_lower[plot_dat_new$CI_lower < ylim[1]] <- ylim[1]
      plot_dat_new$CI_upper[plot_dat_new$CI_upper > ylim[2]] <- ylim[2]
    }
    
    cluster_col <- rep(names(models)[[l]])
    plot_dat_new <- cbind(plot_dat_new, cluster_col)
    
    if (l == 1) {
      plot_dat <- plot_dat_new
    } else {
      plot_dat <- rbind(plot_dat, plot_dat_new)
    }
  }
  
  if (plot_ci) {
  ggplot(plot_dat, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = cluster_col), alpha = 0.1) +
    scale_fill_manual(values = values) +
    guides(fill= "none") +
    geom_line(aes(x = x, y = y, color = cluster_col), lwd = 1) +
    scale_color_manual(values = values) + 
    geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
      scale_x_continuous(limits = c(0, 6000)) +
      scale_y_continuous("Odds Ratio", trans = "log2", limits = c(0.2, 7),
                         breaks = 2^c(-2,-1,0,1, 2),
                         labels = c("0.25","0.5","1","2", "4")) + theme +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank()) +
    xlab("Income")
  } else {
    ggplot(plot_dat, aes(x = x, y = y)) +
      geom_line(aes(x = x, y = y, color = cluster_col), lwd = 1) +
      scale_color_manual(values = values) + scale_x_continuous(0, 6000)
      geom_hline(yintercept = ifelse(used_logLink, 1, 0), col = gray(0.3), lty = 2) +
        scale_x_continuous(limits = c(0, 6000)) +
        scale_y_continuous("Odds Ratio", trans = "log2", limits = c(0.2, 7),
                           breaks = 2^c(-2,-1,0,1, 2),
                           labels = c("0.25","0.5","1","2", "4")) + theme +
      theme(legend.position="bottom") +
      theme(legend.title=element_blank()) +
      xlab("Income")
  }
}

get_plotGAMobject <- function(model) {
  
  checkmate::assert_class(model, classes = "gam")
  
  
  # Idea: Save the plot in a temporal png file, which is deleted right afterwards.
  png("temp.png")
  # plot.gam returns all terms by default, select and rug are set only to decrease evaluation time
  plot.df <- mgcv::plot.gam(model, select = 1, rug = FALSE)
  dev.off()
  unlink("temp.png", recursive = TRUE)
  
  # Delete 'raw' elements as they are very large but not necessary for plotting the effects
  plot.df <- lapply(plot.df, function(x) {
    x$raw <- NULL
    x
  })
  
  return(invisible(plot.df))
}

plot_models_linear <- function(models, type = "hard", cluster = NULL) {
  
  linEffects_list <- lapply(models, plot_linearEffects,
                            return_plotData = TRUE, 
                            refCat = TRUE
                            )
  linEffects_data <- bind_rows(linEffects_list)
  linEffects_data$cluster <- rep(names(models), each = nrow(linEffects_list[[1]]))
  linEffects_data$cluster <- factor(linEffects_data$cluster, levels = names(models))
  linEffects_data$param <- factor(linEffects_data$param, levels = unique(linEffects_data$param)) # For order of x axis
  
  # Changing "parameter" names (to have English annotations in plot)
  
  levels(linEffects_data$param) <- c("Male", "Female", "No", "Yes",
                                     "<5.000", "5,000 to 49,999", "50.000 to 99.999",
                                     "100.000 bis 499.999", "500.000 and more",
                                     "Junior high school", "Secondary school",
                                     "High school", "University or college",
                                     "1", "2", "3", "4", ">4")
  
  
  linEffects_data <- linEffects_data %>%
    mutate(vargroup = as.character(vargroup)) %>% 
    mutate(vargroup = case_when(vargroup == "S_Geschlecht"            ~ "Gender",
                                vargroup == "S_Bildung"               ~ "Education",
                                vargroup == "S_Kinder_0_bis_5_binaer" ~ "Young children",
                                vargroup == "S_Haushaltsgroesse"      ~ "Household size",
                                vargroup == "S_Wohnortgroesse"        ~ "City size",
                                TRUE ~ vargroup)) %>% 
    mutate(vargroup = factor(vargroup, levels = c("Gender","Education","Household size",
                                                  "Young children","City size")))
  
  # Visualization:
  colors_hard <- c("#332288", "#88CCEE", "#44AA99", "#bbbbbb", "#CC6677")
  values_hard <- colors_hard[c(cluster, c(1:5)[!c(1:5) %in% c(cluster)])]
  values_fuzzy <- list(c("Cluster 1/1" = "#332288", "Cluster 1/2" = "#654da5",
                         "Cluster 1/3" = "#927ac3", "Cluster 1/4" = "#beaae1",
                         "Cluster 1/5" = "#eadcff"),
                       c("Cluster 2/2" = "#88CCEE", "Cluster 1/2" = "#a9d9f2",
                         "Cluster 2/3" = "#c7e5f7", "Cluster 2/4" = "#e3f2fb",
                         "Cluster 2/5" = "#f1f9fd"),
                       c("Cluster 3/3" = "#44AA99", "Cluster 1/3" = "#79bfb2",
                         "Cluster 2/3" = "#a7d5cb", "Cluster 3/4" = "#d3eae5",
                         "Cluster 3/5" = "#e9f4f2"),
                       c("Cluster 4/4" = "#bbbbbb", "Cluster 1/4" = "#d0d0d0",
                         "Cluster 2/4" = "#e5e5e5", "Cluster 3/4" = "#f0f0f0",
                         "Cluster 4/5" = "#FAFAFA"),
                       c("Cluster 5/5" = "#CC6677", "Cluster 1/5" = "#dc8d97",
                         "Cluster 2/5" = "#eab2b9", "Cluster 3/5" = "#f6d9db",
                         "Cluster 4/5" = "#fbeced"))
  if (type == "hard") {
    values <- values_hard
  }
  if (type == "fuzzy") {
    values <- values_fuzzy[[cluster]][names(values_fuzzy[[cluster]]) %in% names(models)]
  }
  plot_linEffects <- ggplot(linEffects_data, aes(x = param, y = coef_exp, col = cluster)) +
    geom_hline(yintercept = 1, col = gray(0.3), lty = 2) +
    geom_point(size = 2) +
    geom_pointrange(mapping = aes(ymin = CI_lower_exp, ymax = CI_upper_exp, col = cluster),
                    size = 1, fatten = 1) +
    scale_y_continuous("Odds Ratio", trans = "log2", limits = c(0.2, 7),
                       breaks = 2^c(-2,-1,0,1, 2),
                       labels = c("0.25","0.5","1","2", "4")) +
    scale_color_manual(values = values) +
    facet_grid(cluster ~ vargroup, scales = "free_x", space = "free_x",
               labeller = labeller(vargroup = label_wrap_gen(width = 12))) + theme + 
    theme(strip.background = element_rect(colour="white", fill="white")) + 
    theme(legend.position = "none",
            axis.title.x    = element_blank(),
          axis.text.x     = element_text(angle = 45, hjust = 1))
  return(plot_linEffects)
}

################################################################################

