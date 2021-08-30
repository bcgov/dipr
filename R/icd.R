# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


## Written by Craig Hutton (SDPR)

#' Assign a category to a vector of icd codes
#'
#' Vectorized function that assigns categories to icd codes. Thin wrappers are provided for
#' convenience for ICD-9 and ICD-10 codes respectively. Other than the `icd_version` argument,
#' each wrapper accepts the same arguments as the core function.
#'
#' @param icd_codes vector of icd codes
#' @param icd_version version of the icd code system
#' @param include_code_ranges include codes used to assign category in returned vector.
#' @param mortality default is FALSE. Set to TRUE if working with mortality data
#' @param print_defs_short print a short definition
#' @param print_defs_long print a long definition
#' @param export_defs write out a csv. Defaults to FALSE.
#'
#' @details
#'  - ICD-10 categories were defined according to the official 2019 WHO ICD-10 website: https://icd.who.int/browse10/2019/en
#'  - ICD-9 categories were defined according to the BC Ministry of Health's ICD-9 Diagnostic Code Descriptions
#'  website: https://www2.gov.bc.ca/gov/content/health/practitioner-professional-resources/msp/physicians/diagnostic-code-descriptions-icd-9
#' @export
#'
#' @author Craig Hutton (`craig.hutton@gov.bc.ca`)
#'
#' @examples
#'
#' ## ICD-9
#' dipr_icd9_categories("C19")
#'
#' ## ICD-10
#' dipr_icd10_categories("U07.1")


dipr_icd_categories <- function(icd_codes = NULL, icd_version = 10, include_code_ranges = FALSE, mortality = FALSE,
                           print_defs_short = FALSE, print_defs_long = FALSE, export_defs = FALSE) {

  if (is.null(icd_codes)) stop("Need to supply a vector of ICD codes", call. = FALSE)

  #currently, this function assigns ICD-9 or ICD-10 codes to 38 distinct categories based on the 1st 4 digits
  if(icd_version != 10 && icd_version != 9) {
    stop("only ICD-9 and ICD-10 codes are currently supported")
  }


  icd_codes <- clean_icd_codes(icd_codes)
  icd_defns <- make_icd_definitions(icd_version, mortality)

  icd_code_list <- icd_defns[["icd_code_list"]]
  icd_code_ranges <- icd_defns[["icd_code_ranges"]]


  categories <- assign_categories(icd_codes, icd_code_list, mortality, include_code_ranges)

  if(print_defs_short == TRUE) {
    code_ranges <- tibble::rownames_to_column(data.frame("ICD codes" = icd_code_ranges), "category")
    print(code_ranges, right = FALSE)
  }
  if(print_defs_long == TRUE) {
    print(icd_code_list)
  }
  if(export_defs == TRUE) {
    # readr::write_csv(icd_code_list, "icd code definitions - all codes.csv")
    readr::write_csv(tibble::rownames_to_column(data.frame("ICD codes" = icd_code_ranges), "category"), "icd code definitions - code ranges.csv")
  }

  categories
}


#' @describeIn dipr_icd_categories Thin wrapper around dipr_icd_categories for only ICD-9 codes
#' @param ... arguments passed to `dipr_icd_categories`
#' @export
dipr_icd9_categories <- function(...) {
  dipr_icd_categories(icd_version = 9, ...)
}

#' @describeIn dipr_icd_categories Thin wrapper around dipr_icd_categories for only ICD-10 codes
#' @param ... arguments passed to `dipr_icd_categories`

#' @export
dipr_icd10_categories <- function(...) {
  dipr_icd_categories(icd_version = 10, ...)
}


clean_icd_codes <- function(icd_codes) {
  icd_codes <- stringr::str_remove_all(icd_codes, "\\.") #remove the periods if that hasn't happened already

  #remove empty spaces
  icd_codes <- stringr::str_remove_all(icd_codes, "[:space:]")

  #extract the 1st 4 digits for codes that have more than 4 digits
  icd_codes <- stringr::str_sub(icd_codes, end = 4L)

  #pad codes that are missing a 4th digit with a zero
  stringr::str_pad(icd_codes, width = 4, side = "right", pad = "0")
}


make_icd_definitions <- function(icd_version, mortality) {
  # ICD definitions -------------------------------------------------------------
  icd_code_list <- list() #used for actual category assignment
  icd_code_ranges <- vector(mode = "character") #used for labelling purposes

  #note: section numbers align with the primary chapter numbers in the ICD-10

  #1. infections and parasitic diseases####
  if(icd_version == 10) {
    icd_code_list[["infectious_diseases"]] <- c(stringr::str_c("A", stringr::str_pad(c(0:999), width = 3, side = "left", pad = "0")),
                                                stringr::str_c("B", stringr::str_pad(c(0:999), width = 3, side = "left", pad = "0")))
    icd_code_ranges["infectious_diseases"] <- "A00-B99"

  } else if(icd_version == 9) {
    icd_code_list[["infectious_diseases"]] <- c(stringr::str_pad(c(10:1349, 1360:1398), width = 4, side = "left", pad = "0"),
                                                "45A0")
    icd_code_ranges["infectious_diseases"] <- "001-134, 136-139.8, 45A"
  }

  #2a. cancer####
  if(icd_version == 10) {
    icd_code_list[["malignant_neoplasms"]] <- stringr::str_c("C", stringr::str_pad(c(0:970), width = 3, side = "left", pad = "0"))
    icd_code_ranges["malignant_neoplasms"] <- "C00-C97"

  } else if(icd_version == 9) {
    icd_code_list[["malignant_neoplasms"]] <- as.character(c(1400:2089))
    icd_code_ranges["malignant_neoplasms"] <- "140-208"
  }

  #2b. non-cancerous growths####
  if(icd_version == 10) {
    icd_code_list[["benign_neoplasms"]] <- stringr::str_c("D", stringr::str_pad(c(0:489), width = 3, side = "left", pad = "0"))
    icd_code_ranges["benign_neoplasms"] <- "D00-D48"

  } else if(icd_version == 9) {
    icd_code_list[["benign_neoplasms"]] <- c(as.character(c(2100:2399)), "35A0")
    icd_code_ranges["benign_neoplasms"] <- "210-239, 35A"
  }

  #3. diseases of the blood and blood-forming organs####
  if(icd_version == 10) {
    icd_code_list[["blood_diseases"]] <- stringr::str_c("D", stringr::str_pad(c(500:899), width = 3, side = "left", pad = "0"))
    icd_code_ranges["blood_diseases"] <- "D50-D89"

  } else if(icd_version == 9) {
    icd_code_list[["blood_diseases"]] <- as.character(c(1350:1359, 2790:2890, 2894:2899))
    icd_code_ranges["blood_diseases"] <- "135, 276, 279-289.0, 289.4-289.9"
  }

  #4a. diabetes (types 1 and 2)####
  if(icd_version == 10) {
    icd_code_list[["diabetes"]] <- stringr::str_c("E", stringr::str_pad(c(100:149), width = 3, side = "left", pad = "0"))
    icd_code_ranges["diabetes"] <- "E10-E14"

  } else if(icd_version == 9){
    icd_code_list[["diabetes"]] <- as.character(c(2500:2509))
    icd_code_ranges["diabetes"] <- "250"
  }


  #4b. malnutrition####
  if(icd_version == 10) {
    icd_code_list[["malnutrition"]] <- stringr::str_c("E", stringr::str_pad(c(400:469, 500:569, 580:619, 630:649),
                                                                            width = 3, side = "left", pad = "0"))
    icd_code_ranges["malnutrition"] <- "E40-E64"

  } else if(icd_version == 9) {
    icd_code_list[["malnutrition"]] <- as.character(c(2600:2699))
    icd_code_ranges["malnutrition"] <- "260-269"
  }

  #4c. obesity####
  if(icd_version == 10) {
    icd_code_list[["obesity"]] <- stringr::str_c("E", stringr::str_pad(c(660:669), width = 3, side = "left", pad = "0"))
    icd_code_ranges["obesity"] <- "E66"

  } else if(icd_version == 9) {
    icd_code_list[["obesity"]] <- "2780"
    icd_code_ranges["obesity"] <- "278.0"
  }

  #4d. other endocrine, nutritional, or metabolic diseases####
  if(icd_version == 10) {
    icd_code_list[["other_endocrine_metabolic"]] <- stringr::str_c("E", stringr::str_pad(c(0:79, 150:169, 200:359, 650:659, 670:909),
                                                                                         width = 3, side = "left", pad = "0"))

    icd_code_ranges["other_endocrine_metabolic"] <- "E00-E07, E15, E16, E20-E35, E65, E67-E90"

  } else if(icd_version == 9) {
    icd_code_list[["other_endocrine_metabolic"]] <- as.character(c(2400:2469, 2510:2599, 2700:2779, 2781:2799))
    icd_code_ranges["other_endocrine_metabolic"] <- "240-246, 251-259, 270-277, 278.1-278"

  }

  #5a./6a. dementias & neurodegenerative diseases####
  if(icd_version == 10) {
    icd_code_list[["neurodegenerative_diseases"]] <- c(stringr::str_c("F",
                                                                      stringr::str_pad(c(0:49, 51), width = 3, side = "left", pad = "0")),
                                                       stringr::str_c("G",
                                                                      c(100:239, 250, 300:311, 318:329)))
    icd_code_ranges["neurodegenerative_diseases"] <- "F00-F04, F05.1, G10-G23, G25.0, G30-G31.1, G31.8-G32"

  } else if(icd_version == 9) {
    icd_code_list[["neurodegenerative_diseases"]] <- as.character(c(2900:2909, 2940:2941, 3300:3331, 3334, 3340:3379))
    icd_code_ranges["neurodegenerative_diseases"] <- "290, 294.0, 294.1, 330-333.1, 333.4, 334-337"
  }

  #5b. alcohol-related conditions####
  if(icd_version == 10) {
    icd_code_list[["alcohol_related"]] <- c("E244", #alcohol-induced pseudo-Cushing syndrome
                                            stringr::str_c("F", c(100:109)), #alcohol intoxication, psychoses, &/or dependence
                                            stringr::str_c("G", c(312, 621, 721)), #alcoholic neurological disorders
                                            "I426", #alcoholic cardiomyopathy
                                            stringr::str_c("K", c(292, 700:709, 860)), #alcoholic gastritis, pancreatitis & liver disease
                                            "O354", #maternal care for suspected damage to fetus due to alcohol
                                            "P043", #fetus/newborn affected by alcohol
                                            "Q860", #FASD
                                            "R780", #finding of excess alcohol in the blood
                                            stringr::str_c("X", c(450:459, 650:659)), #alcohol poisoning
                                            stringr::str_c("T", c(510:519)), #toxic effect of alcohol
                                            stringr::str_c("Y", c(150:159)) #alcohol poisoning (undetermined intent)
    )
    icd_code_ranges["alcohol_related"] <- "E24.4, F10, G31.2, G62.1, G72.1, I42.6, K29.2, K70, K86.0, O35.4, P04.3, Q86.0, R78.0, X45, X65, T51, Y15"

  } else if(icd_version == 9) {
    icd_code_list[["alcohol_related"]] <- c(as.character(c(2255, 2910:2919, 3030:3039, 3050, 3575,
                                                           4255, 5353, 5710:5713, 5771, 7607, 9800:9809)), "E860")
    icd_code_ranges["alcohol_related"] <- "225.5, 291, 303, 305.0, 357.5, 425.5, 535.3, 571.0-571.3, 577.1, 760.7, 980, E860"
  }

  #5c. opioid-related conditions####
  if(icd_version == 10) {
    icd_code_list[["opioid_related"]] <- c(stringr::str_c("F", c(110:119)), #opioid intoxication effects
                                           stringr::str_c("T", c(400:404, 406)), #poisoning by opiates
                                           stringr::str_c("X", c(420:429, 620:629)), #suicide by opioids and other narcotics
                                           stringr::str_c("Y", c(120:129)), #poisoning by opioids and other narcotics (undetermined intent)
                                           "Y450" #adverse effects of opioids under therapeutic use
    )
    icd_code_ranges["opioid_related"] <- "F11, T40.0-T40.4, T40.6, X42, X62, Y12, Y45.0"

  } else if(icd_version == 9) {
    icd_code_list[["opioid_related"]] <- c(as.character(c(3040, 3047, 3055)), #opioid abuse/dependence
                                           "9650", "E850") #poisoning by opiates
    icd_code_ranges["opioid_related"] <- "304.0, 304.7, 305.5, 965.0, E850"
  }

  #5d. drug-related conditions####
  if(icd_version == 10) {
    icd_code_list[["drug_related"]] <- c(stringr::str_c("F", c(120:199, #psychoactive-substance use (e.g. inebriation effects)
                                                               550:559)), #abuse of non-dependence producing substances (NSAIDs, vitamins, etc.)
                                         "G620", #drug-induced polyneuropathy
                                         "G720", #drug-induced myopathy
                                         stringr::str_c("X", c(400:419, 430:449,#accidental poisoning
                                                               600:619, 630:649, #suicide by drugs
                                                               850:859)), #assault using drugs/medications
                                         stringr::str_c("T", c(360:399, 405, 407:509)), #poisoning by drugs/medications (accidental).
                                         stringr::str_c("Y", c(100:119, 130:149, #poisoning by drugs (undetermined intent)
                                                               400:449, 451:599)) #adverse effects of drugs in therapeutic use
    )
    icd_code_ranges["drug_related"] <- "F12-F19, G62.0, G72.0, T36-T39, T40.5, T40.7-T50, X40, X41, X43, X44, X60, X61, X63, X64, X85, Y10, Y11, Y13, Y14, Y40-Y44, Y45.1-Y59"

  } else if(icd_version == 9) {
    icd_code_list[["drug_related"]] <- c(as.character(c(2920:2929, 3041:3046,
                                                        3048, 3049,
                                                        3051:3054, 3056:3059, 3576,
                                                        9600:9649, 9651:9799)),
                                         stringr::str_c("E", c(851:858, 861:869, 930:950))
    )
    icd_code_ranges["drug_related"] <- "292, 304.1-304.6, 304.8, 304.9, 305.1-305.4, 305.6-305.9, 357.6, 960-964, 965.1-979, E851-E858, E861-E869, E930-E950"
  }

  #5e. mental disorders with psychotic features (e.g. schizophrenia)####
  if(icd_version == 10) {
    icd_code_list[["psychoses"]] <- stringr::str_c("F", c(200:299)) #schizophrenia = F20
    icd_code_ranges["psychoses"] <- "F20-F29"

  } else if(icd_version == 9) {
    icd_code_list[["psychoses"]] <- as.character(c(2950:2959, 2970:2979, 2981:2989))
    icd_code_ranges["psychoses"] <- "295, 297, 298.1-298.9"
  }

  #5f. mood and anxiety disorders####
  if(icd_version == 10) {
    icd_code_list[["mood_anxiety_disorders"]] <- c(stringr::str_c("F", 300:399), #mood disorders (MDD = F32)
                                                   stringr::str_c("F", c(400:489)),#anxiety and other stress-related disorders (GAD = F411)
                                                   "F530") #post-partum depression
    icd_code_ranges["mood_anxiety_disorders"] <- "F30-F48, F53.0"

  } else if(icd_version == 9) {
    icd_code_list[["mood_anxiety_disorders"]] <- c(as.character(c(2960:2969, 2980, 3000:3009, 3060:3069, 3011, 3080:3099, 3110:3119)), "50B0")
    icd_code_ranges["mood_anxiety_disorders"] <- "296, 298.0, 300, 301.1, 306, 308, 309, 311, 50B"
  }

  #5g. other mental disorders####
  if(icd_version == 10) {
    icd_code_list[["other_mental_disorders"]] <- stringr::str_c("F", stringr::str_pad(c(50, 58:99, 450:459, 500:529, 531:549, 560:999),
                                                                                      width = 3, side = "left", pad = "0"))
    icd_code_ranges["other_mental_disorders"] <- "F05.0, F05.8-F09, F50-F52, F53.1-F54, F56-F99"

  } else if(icd_version == 9) {
    icd_code_list[["other_mental_disorders"]] <- c(as.character(c(2930:2939, 2948, 2949, 2990:2999,
                                                                  3010, 3012:3029, 3060:3079, 3100:3109,
                                                                  3120:3199)))
    icd_code_ranges["other_mental_disorders"] <- "293-293, 294.8, 294.9, 299, 301.0, 301.3-301.9, 302, 306, 307, 310, 312-319"
  }

  #6b. non-degenerative neurological diseases####
  if(icd_version == 10) {
    icd_code_list[["nondegen_neurological_diseases"]] <- stringr::str_c("G",
                                                                        stringr::str_pad(c(0:90, 240:269,
                                                                                           350:449, 460:619,
                                                                                           622:719, 722:999),
                                                                                         width = 3, side = "left", pad = "0"))
    icd_code_ranges["nondegen_neurological_diseases"] <- "G00-G09, G24-G26, G35-G44, G46-G61, G62.2-G71, G72.2-G99"

  } else if(icd_version == 9) {
    icd_code_list[["nondegen_neurological_diseases"]] <- as.character(c(3200:3269, 3332, 3333, 3335:3339, 3400:3574, 3577:3599))
    icd_code_ranges["nondegen_neurological_diseases"] <- "320-326, 333.2, 333.3, 333.5-333.9, 340-357.4, 357.7-359"
  }

  #7/8. eye and ear diseases####
  if(icd_version == 10) {
    icd_code_list[["eye_ear_diseases"]] <- stringr::str_c("H", stringr::str_pad(c(0:999),
                                                                                width = 3, side = "left", pad = "0"))
    icd_code_ranges["eye_ear_diseases"] <- "H00-H99"

  } else if(icd_version == 9) {
    icd_code_list[["eye_ear_diseases"]] <- c(as.character(c(3600:3899)),
                                             "06B0")
    icd_code_ranges["eye_ear_diseases"] <- "360-389, 06B"
  }

  #9a. hypertension-related diseases####
  if(icd_version == 10) {
    icd_code_list[["hypertension"]] <- stringr::str_c("I", c(100:159))
    icd_code_ranges["hypertension"] <- "I10-I15"

  } else if(icd_version == 9) {
    icd_code_list[["hypertension"]] <- as.character(c(4010:4059))
    icd_code_ranges["hypertension"] <- "401-405"
  }

  #9b. heart disease####
  if(icd_version == 10) {
    icd_code_list[["heart_disease"]] <- stringr::str_c("I",
                                                       stringr::str_pad(c(10:99, #rheumatic heart diseases (IOO was included based on an example publication)
                                                                          200:259, 271, 278, 279, 300:519), #ischemic heart conditions (e.g. heart attacks)
                                                                        width = 3, side = "left", pad = "0"))
    icd_code_ranges["heart_disease"] <- "I00-I09, I20-I25, I27.1, I27.8, I27.9, I30-I51"

  } else if(icd_version == 9) {
    icd_code_list[["heart_disease"]] <- as.character(c(3910:3920, 3930:3989, #rheumatic heart diseases
                                                       4100:4150, 4161, 4169, 4200:4299)) #ischemic heart conditions
    icd_code_ranges["heart_disease"] <- "391-392.0, 393-398, 410-415.0, 416.1, 416.9, 420-429"
  }

  #9c. cerebrovascular disease (e.g. stroke)####
  if(icd_version == 10) {
    icd_code_list[["cerebrovascular_disease"]] <- c(stringr::str_c("G", 450:459), #transient cerebral ischemic attacks
                                                    stringr::str_c("I", 600:699)) #other cerebrovascular diseases
    icd_code_ranges["cerebrovascular_disease"] <- "G45, I60-I69"

  } else if(icd_version == 9){
    icd_code_list[["cerebrovascular_disease"]] <- as.character(c(4300:4389))
    icd_code_ranges["cerebrovascular_disease"] <- "430-438"
  }

  #9d. other circulatory system diseases####
  if(icd_version == 10) {
    icd_code_list[["other_circulatory_diseases"]] <- stringr::str_c("I",
                                                                    stringr::str_pad(c(0:9, 260:270, 272, 280:289, 700:999),
                                                                                     width = 3, side = "left", pad = "0"))
    icd_code_ranges["other_circulatory_diseases"] <- "I00, I26, I27.0, I27.2, I28, I70-I99"

  } else if(icd_version == 9){
    icd_code_list[["other_circulatory_diseases"]] <- as.character(c(2891:2893, 3900:3909, 3929, 4150:4179, 4400:4599))
    icd_code_ranges["other_circulatory_diseases"] <- "289.1-289.3, 390, 392.9, 415-417, 440-459"
  }

  #10a. influenza and pneuomnia####
  if(icd_version == 10) {
    icd_code_list[["influenza_pneumonia"]] <- stringr::str_c("J",
                                                             stringr::str_pad(c(90:189),
                                                                              width = 3, side = "left", pad = "0"))
    icd_code_ranges["influenza_pneumonia"] <- "J09-J18"

  } else if(icd_version == 9) {
    icd_code_list[["influenza_pneumonia"]] <- as.character(4800:4878)
    icd_code_ranges["influenza_pneumonia"] <- "480-487"
  }

  #10b. chronic lower respiratory diseases####
  if(icd_version == 10) {
    icd_code_list[["chronic_lower_respiratory"]] <- stringr::str_c("J", 400:479)
    icd_code_ranges["chronic_lower_respiratory"] <- "J40-J47"

  } else if(icd_version == 9) {
    icd_code_list[["chronic_lower_respiratory"]] <- as.character(4900:4969)
    icd_code_ranges["chronic_lower_respiratory"] <- "490-496"
  }

  #10c. other respiratory diseases####
  if(icd_version == 10) {
    icd_code_list[["other_respiratory_diseases"]] <- c(stringr::str_c("J",
                                                                      stringr::str_pad(c(0:89, 190:399, 480:999),
                                                                                       width = 3, side = "left", pad = "0")), "U070")
    icd_code_ranges["other_respiratory_diseases"] <- "J00-J08, J19-J39, J48-J99, U07.0"

  } else if(icd_version == 9) {
    icd_code_list[["other_respiratory_diseases"]] <- as.character(c(4600:4789, 5000:5199))
    icd_code_ranges["other_respiratory_diseases"] <- "460-478, 500-519"
  }

  #11a. liver diseases####
  if(icd_version == 10) {
    icd_code_list[["liver_disease"]] <- stringr::str_c("K", c(710:779)) #except for alcohol-related (K70)
    icd_code_ranges["liver_disease"] <- "K71-K77"

  } else if(icd_version == 9) {
    icd_code_list[["liver_disease"]] <- as.character(c(5700:5709, 5714:5719, 5720:5739))
    icd_code_ranges["liver_disease"] <- "570, 571.4, 571.4-571.9, 572-573"
  }

  #11b. other digestive system diseases####
  #except for K292 (gastritis, alcohol-related) & K860 (pancreatitis, alcohol-related)
  if(icd_version == 10) {
    icd_code_list[["other_digestive_diseases"]] <- stringr::str_c("K",
                                                                  stringr::str_pad(c(0:291, 293:679, 800:859, 861:939),
                                                                                   width = 3, side = "left", pad = "0"))
    icd_code_ranges["other_digestive_diseases"] <- "K00-K29.1, K29.3-K67, K80-K85, K86.1-K93"

  } else if(icd_version == 9) {
    icd_code_list[["other_digestive_diseases"]] <- c(as.character(c(5200:5699, 5740:5799)), "36A0")
    icd_code_ranges["other_digestive_diseases"] <- "520-569, 574-579, 36A"
  }

  #12. diseases of the skin (cutaneuous tissue) and subcutaneous tissues####
  if(icd_version == 10) {
    icd_code_list[["cutaneous_diseases"]] <- stringr::str_c("L",
                                                            stringr::str_pad(c(0:999),
                                                                             width = 3, side = "left", pad = "0"))
    icd_code_ranges["cutaneous_diseases"] <- "L00-L99"

  } else if(icd_version == 9) {
    icd_code_list[["cutaneous_diseases"]] <- c(as.character(c(6800:7099)), "03B0")
    icd_code_ranges["cutaneous_diseases"] <- "680-709, 03B"
  }

  #13. diseases of the muscles, bones, and connective tissue####
  if(icd_version == 10) {
    icd_code_list[["musculoskeletal_diseases"]] <- stringr::str_c("M",
                                                                  stringr::str_pad(c(0:999),
                                                                                   width = 3, side = "left", pad = "0"))
    icd_code_ranges["musculoskeletal_diseases"] <- "M00-M99"

  } else if(icd_version == 9) {
    icd_code_list[["musculoskeletal_diseases"]] <- as.character(c(7100:7399))
    icd_code_ranges["musculoskeletal_diseases"] <- "710-739"

  }

  #14a. kidney disease####
  if(icd_version == 10) {
    icd_code_list[["kidney_disease"]] <- stringr::str_c("N",
                                                        stringr::str_pad(c(0:88, 120:133, 136, 140:200, 202, 250:299),
                                                                         width = 3, side = "left", pad = "0"))
    #I12, hypertensive kidney disease, is covered under hypertensive diseases
    #for chronic kidney disease only, change to codes: N01, N03, N07, N18, N19, N26, N27
    icd_code_ranges["kidney_disease"] <- "N00-N08, N12-N13.3, N13.6, N14-N19, N20.0, N20.2, N25-N29"

  } else if(icd_version == 9) {
    icd_code_list[["kidney_disease"]] <- as.character(c(5800:5920, 5930:5932, 5936))
    icd_code_ranges["kidney_disease"] <- "580-592.0, 593.0-593.2, 593.6"
  }

  #14b. other genitourinary diseases####
  if(icd_version == 10) {
    icd_code_list[["other_genitourinary_diseases"]] <- c(stringr::str_c("N", c(100:119, 134, 135, 137:139,
                                                                               201, 203:239,
                                                                               300:999))
    )
    icd_code_ranges["other_genitourinary_diseases"] <- "N10-N11, N13.4, N13.5, N13.7-N13.9, N20.1, N20.3-N23, N30-N99"

  } else if(icd_version == 9) {
    icd_code_list[["other_genitourinary_diseases"]] <- as.character(c(5921:5929, 5933:5935, 5937:6299))
    icd_code_ranges["other_genitourinary_diseases"] <- "592.1, 592.9, 593.3-593.5, 593.7-629"
  }

  #15 & 16. related to pregnancy, childbirth, and the perinatal period####
  if(icd_version == 10) {
    icd_code_list[["pregnancy_perinatal_related"]] <- c(stringr::str_c("O", stringr::str_pad(c(0:999),
                                                                                             width = 3, side = "left", pad = "0")),
                                                        stringr::str_c("P", stringr::str_pad(c(0:969),
                                                                                             width = 3, side = "left", pad = "0")))
    icd_code_ranges["pregnancy_perinatal_related"] <- "O00-P96"

  } else if(icd_version == 9) {
    icd_code_list[["pregnancy_perinatal_related"]] <- c(as.character(c(6300:6769, 7600:7606, 7608:7799)),
                                                        "06A0", "08A0", stringr::str_c(c(30:38), "B", 0))
    icd_code_ranges["pregnancy_perinatal_related"] <- "630-676, 760.0-760.6, 760.8-779, 06A, 08A, 30B-38B"
  }

  #17. congenital abnormalities####
  if(icd_version == 10) {
    icd_code_list[["congenital_abnormalities"]] <- stringr::str_c("Q", stringr::str_pad(c(0:999),
                                                                                        width = 3, side = "left", pad = "0"))
    icd_code_ranges["congenital_abnormalities"] <- "Q00-Q99"

  } else if(icd_version == 9) {
    icd_code_list[["congenital_abnormalities"]] <- c(as.character(c(7400:7599)), "08B0", "21B0")
    icd_code_ranges["congenital_abnormalities"] <- "740-759, 08B, 21B"
  }

  #18. conditions of unknown etiology####
  #from WHO guide: "practically all categories in the chapter could be
  #designated 'not otherwise specified', 'unknown etiology', or 'transient'"
  #used for cases where no specific diagnosis can be made or for signs/symptoms
  #whose causes can't be determined, the parient failed to return for followup
  #investigation, etc.
  if(icd_version == 10) {
    icd_code_list[["unknown_etiology"]] <- stringr::str_c("R", stringr::str_pad(c(0:999),
                                                                                width = 3, side = "left", pad = "0"))
    icd_code_ranges["unknown_etiology"] <- "R00-R99"

  } else if(icd_version == 9) {
    icd_code_list[["unknown_etiology"]] <- c(as.character(c(7800:7999)), "01A0", "02A0", "07A0", "12A0")
    icd_code_ranges["unknown_etiology"] <- "780-799, 01A, 02A, 07A, 12A"
  }

  #19/20a. injuries & accidents####
  if(icd_version == 10) {
    icd_code_list[["injuries_accidents"]] <- c(
      #injuries
      stringr::str_c("S", stringr::str_pad(c(0:999),
                                           width = 3, side = "left", pad = "0")),
      stringr::str_c("T", stringr::str_pad(c(0:99,
                                             #burns, frostbite, corrosion, and toxic chemical exposure, etc.
                                             100:359,
                                             520:659,
                                             790:799,
                                             900:959),
                                           width = 3, side = "left", pad = "0")),
      #accidents
      stringr::str_c("V", stringr::str_pad(c(0:999),
                                           width = 3, side = "left", pad = "0")),
      stringr::str_c("W", stringr::str_pad(c(0:999),
                                           width = 3, side = "left", pad = "0")),
      stringr::str_c("X", stringr::str_pad(c(0:399, 460:599),
                                           width = 3, side = "left", pad = "0")),
      stringr::str_c("Y", c(850:869, 880:889)))
    icd_code_ranges["injuries_accidents"] <- "S00-T35, T52-T65, T79, T90-T95, V01-X39, X46-X59, Y85, Y86, Y88"

  } else if(icd_version == 9) {
    icd_code_list[["injuries_accidents"]] <- c(as.character(c(8000:9089, 9100:9599, 9810:9949)),
                                               stringr::str_c("E", c(800:849, 880:929)),
                                               "10A0", "42A0", "43A0", "02B0", "22B0", "55B0", "60B0", "65B0", "66B0")
    icd_code_ranges["injuries_accidents"] <- "800-908, 910-959, 981-994, E800-E849, E880-E929, 10A, 42A, 02B, 22B, 55B, 60B, 65B, 66B"
  }


  #20b. suicide by means other than drug/alcohol overdose####
  if(icd_version == 10) {
    icd_code_list[["non_OD_suicide"]] <- c(stringr::str_c("X", c(660:849)), "Y870")
    icd_code_ranges["non_OD_suicide"] <- "X66-X84, Y87.0"

  } else if(icd_version == 9) {
    icd_code_list[["non_OD_suicide"]] <- stringr::str_c("E", c(951:959))
    icd_code_ranges["non_OD_suicide"] <- "E951-E959"
  }

  #20c. homicide####
  if(icd_version == 10) {
    icd_code_list[["homicide"]] <- c(stringr::str_c("X", 580:999),
                                     stringr::str_c("Y", stringr::str_pad(c(0:99, 871),
                                                                          width = 3, side = "left", pad = "0"))
    )
    icd_code_ranges["homicide"] <- "X58-Y09, Y87.1"

  } else if(icd_version == 9) {
    icd_code_list[["homicide"]] <- stringr::str_c("E", 960:969)
    icd_code_ranges["homicide"] <- "E960-E969"
  }

  #20d. other external causes of morbidity and mortality####
  if(icd_version == 10) {
    icd_code_list[["other_external_causes"]] <- c(stringr::str_c("T", c(660:789, 800:889, 980:983)),
                                                  stringr::str_c("Y",
                                                                 stringr::str_pad(c(160:449, 600:849, 872, 890:989),
                                                                                  width = 3, side = "left", pad = "0"))
    )
    icd_code_ranges["other_external_causes"] <- "T66-T78, T80-T88, T98, Y16-Y44, Y60-Y84, Y87.2, Y89-Y98"

  } else if(icd_version == 9) {
    icd_code_list[["other_external_causes"]] <- c(as.character(c(9090:9099, 9940:9999)),
                                                  stringr::str_c("E",
                                                                 stringr::str_pad(c(0:30, 870:879, 970:999),
                                                                                  width = 3, side = "left", pad = "0")),
                                                  "32A0")
    icd_code_ranges["other_external_causes"] <- "909, 994-999, E000-E030, E870-E879, E970-E999, 32A"
  }

  #21. other factors influencing health status and contact with health services####
  if(mortality == FALSE) {
    #this chapter should not be used for international comparisons or primary mortality coding

    # #21a
    # if(icd_version == 10) {
    #   icd_code_list[["psychosocioeconomic_circumstances"]] <- stringr::str_c("Z", 550:659)
    #   icd_code_ranges["psychosocioeconomic_circumstances"] <- "Z55-Z65"
    #
    # } else if(icd_version == 9) {
    #   icd_code_list[["psychosocioeconomic_circumstances"]] <- stringr::str_c("V", 600:629)
    #   icd_code_ranges["psychosocioeconomic_circumstances"] <- "V60-V62"
    # }
    # #21b
    # if(icd_version == 10){
    #   icd_code_list[["other_factors_influencing_health_status"]] <- stringr::str_c("Z",
    #                                                                                stringr::str_pad(c(0:549, 700:999),
    #                                                                                                 width = 3, side = "left", pad = "0"))
    #   icd_code_ranges["other_factors_influencing_health_status"] <- "Z00-Z54, Z70-Z99"
    #
    # } else if(icd_version == 9) {
    #   icd_code_list[["other_factors_influencing_health_status"]] <- stringr::str_c("V",
    #                                                                                stringr::str_pad(c(1:599, 630:919),
    #                                                                                                 width = 3, side = "left", pad = "0"))
    #   icd_code_ranges["other_factors_influencing_health_status"] <- "V01-V59, V63-V91"
    # }

    #just go with one category here for now due to conflicts between ICD-9 and ICD-10
    if(icd_version == 10){
      icd_code_list[["other_factors_influencing_health_status"]] <- stringr::str_c("Z",
                                                                                   stringr::str_pad(c(0:999),
                                                                                                    width = 3, side = "left", pad = "0"))
      icd_code_ranges["other_factors_influencing_health_status"] <- "Z00-Z99"

    } else if(icd_version == 9) {
      icd_code_list[["other_factors_influencing_health_status"]] <- c(stringr::str_c("V",
                                                                                     stringr::str_pad(c(1:829),
                                                                                                      width = 3, side = "left", pad = "0")),
                                                                      "04A0" ,"05A0", "31A0", "34A0", "44A0", "01B0",#MSP billing codes start here extra 0 added because of padding to fulfill 4-digit input requirement
                                                                      "10B0", "11B0", "12B0", "15B0", "16B0", "17B0", "18B0", "19B0",
                                                                      "20B0", "23B0", "01E0", "01F0", "01L0", "01X0")
      icd_code_ranges["other_factors_influencing_health_status"] <- "V01-V82, 04A, 05A, 31A, 34A, 44A, 01B, 10B-20B, 23B, 01E, 01F, 01L, 01X"
    }
  }

  #22. codes for special purposes (e.g. new diseases yet to be assigned to other categories)####
  # if(icd_version == 10) {
  #   icd_code_list[["SARS"]] <- "U049"
  # }
  #
  # if(icd_version == 10) {
  #   icd_code_list[["vaping_related_disorder"]] <- c("U070")
  # }

  if(icd_version == 10) {
    icd_code_list[["SARS_COVID19"]] <- c("U049", "U071", "U072")
    icd_code_ranges["SARS_COVID19"] <- "U049, U071, U072"
  } else if(icd_version == 9) {
    icd_code_list[["SARS_COVID19"]] <- c("C190")
    icd_code_ranges["SARS_COVID19"] <- "C190"
  }

  list("icd_code_list" = icd_code_list, "icd_code_ranges" = icd_code_ranges)

  #end of definitions ---------------------------------------------
}


assign_categories <- function(icd_codes, icd_code_list, mortality, include_code_ranges) {
  # disease category assignment ---------------------------------------------
  if(mortality == TRUE) {
    if(include_code_ranges == TRUE) {
      #note: upgrade to data.table::fcase() when data.table is updated to version 1.12.9 (version 1.12.8 is currently installed)
      #or perhaps using elucidate::translate()
      categories <- dplyr::case_when(icd_codes %in% icd_code_list$infectious_diseases ~
                                       paste0("infection [", icd_code_ranges["infectious_diseases"], "]"),

                                     icd_codes %in% icd_code_list$malignant_neoplasms ~
                                       paste0("malignant neoplasm [", icd_code_ranges["malignant_neoplasms"], "]"),

                                     icd_codes %in% icd_code_list$benign_neoplasms ~
                                       paste0("benign neoplasm [", icd_code_ranges["benign_neoplasms"], "]"),

                                     icd_codes %in% icd_code_list$blood_diseases ~
                                       paste0("blood disease [", icd_code_ranges["blood_diseases"], "]"),

                                     icd_codes %in% icd_code_list$diabetes ~
                                       paste0("diabetes [", icd_code_ranges["diabetes"], "]"),

                                     icd_codes %in% icd_code_list$malnutrition ~
                                       paste0("malnutrition [", icd_code_ranges["malnutrition"], "]"),

                                     icd_codes %in% icd_code_list$obesity ~
                                       paste0("obesity [", icd_code_ranges["obesity"], "]"),

                                     icd_codes %in% icd_code_list$other_endocrine_metabolic ~
                                       paste0("other endocrine or metabolic [", icd_code_ranges["other_endocrine_metabolic"], "]"),

                                     icd_codes %in% icd_code_list$neurodegenerative_diseases ~
                                       paste0("neurodegenerative disease [", icd_code_ranges["neurodegenerative_diseases"], "]"),

                                     icd_codes %in% icd_code_list$alcohol_related ~
                                       paste0("alcohol-related [", icd_code_ranges["alcohol_related"], "]"),

                                     icd_codes %in% icd_code_list$opioid_related ~
                                       paste0("opioid-related [", icd_code_ranges["opioid_related"], "]"),

                                     icd_codes %in% icd_code_list$drug_related ~
                                       paste0("drug-related [", icd_code_ranges["drug_related"], "]"),

                                     icd_codes %in% icd_code_list$psychoses ~
                                       paste0("psychosis [", icd_code_ranges["psychoses"], "]"),

                                     icd_codes %in% icd_code_list$mood_anxiety_disorders ~
                                       paste0("mood or anxiety disorder [", icd_code_ranges["mood_anxiety_disorders"], "]"),

                                     icd_codes %in% icd_code_list$other_mental_disorders ~
                                       paste0("other mental disorder [", icd_code_ranges["other_mental_disorders"], "]"),

                                     icd_codes %in% icd_code_list$nondegen_neurological_diseases ~
                                       paste0("other neurological disease [", icd_code_ranges["nondegen_neurological_diseases"], "]"),

                                     icd_codes %in% icd_code_list$eye_ear_diseases ~
                                       paste0("eye or ear disease [", icd_code_ranges["eye_ear_diseases"], "]"),

                                     icd_codes %in% icd_code_list$hypertension ~
                                       paste0("hypertension [", icd_code_ranges["hypertension"], "]"),

                                     icd_codes %in% icd_code_list$heart_disease ~
                                       paste0("heart disease [", icd_code_ranges["heart_disease"], "]"),

                                     icd_codes %in% icd_code_list$cerebrovascular_disease ~
                                       paste0("cerebrovascular disease [", icd_code_ranges["cerebrovascular_disease"], "]"),

                                     icd_codes %in% icd_code_list$other_circulatory_diseases ~
                                       paste0("other circulatory disease [", icd_code_ranges["other_circulatory_diseases"], "]"),

                                     icd_codes %in% icd_code_list$influenza_pneumonia ~
                                       paste0("influenza or pneumonia [", icd_code_ranges["influenza_pneumonia"], "]"),

                                     icd_codes %in% icd_code_list$chronic_lower_respiratory ~
                                       paste0("chronic lower respiratory disease [", icd_code_ranges["chronic_lower_respiratory"], "]"),

                                     icd_codes %in% icd_code_list$other_respiratory_diseases ~
                                       paste0("other respiratory disease [", icd_code_ranges["other_respiratory_diseases"], "]"),

                                     icd_codes %in% icd_code_list$liver_disease ~
                                       paste0("liver disease [", icd_code_ranges["liver_disease"], "]"),

                                     icd_codes %in% icd_code_list$other_digestive_diseases ~
                                       paste0("other digestive disease [", icd_code_ranges["other_digestive_diseases"], "]"),

                                     icd_codes %in% icd_code_list$cutaneous_diseases ~
                                       paste0("skin or sub-cut. disease [", icd_code_ranges["cutaneous_diseases"], "]"),

                                     icd_codes %in% icd_code_list$musculoskeletal_diseases ~
                                       paste0("musculoskeletal disease [", icd_code_ranges["musculoskeletal_diseases"], "]"),

                                     icd_codes %in% icd_code_list$kidney_disease ~
                                       paste0("kidney disease [", icd_code_ranges["kidney_disease"], "]"),

                                     icd_codes %in% icd_code_list$other_genitourinary_diseases ~
                                       paste0("other genitourinary disease [", icd_code_ranges["other_genitourinary_diseases"], "]"),

                                     icd_codes %in% icd_code_list$pregnancy_perinatal_related ~
                                       paste0("pregnancy- or perinatal-related [", icd_code_ranges["pregnancy_perinatal_related"], "]"),

                                     icd_codes %in% icd_code_list$congenital_abnormalities ~
                                       paste0("congenital abnormality [", icd_code_ranges["congenital_abnormalities"], "]"),

                                     icd_codes %in% icd_code_list$unknown_etiology ~
                                       paste0("disease of unknown etiology [", icd_code_ranges["unknown_etiology"], "]"),

                                     icd_codes %in% icd_code_list$injuries_accidents ~
                                       paste0("injury/accident [", icd_code_ranges["injuries_accidents"], "]"),

                                     icd_codes %in% icd_code_list$non_OD_suicide ~
                                       paste0("suicide (non-OD) [", icd_code_ranges["non_OD_suicide"], "]"),

                                     icd_codes %in% icd_code_list$homicide ~
                                       paste0("homicide [", icd_code_ranges["homicide"], "]"),

                                     icd_codes %in% icd_code_list$other_external_causes ~
                                       paste0("other external cause [", icd_code_ranges["other_external_causes"], "]"),

                                     icd_codes %in% icd_code_list$SARS_COVID19 ~
                                       paste0("SARS/COVID-19 [", icd_code_ranges["SARS_COVID19"], "]"),

                                     !is.na(icd_codes) ~ "code not recognized",
                                     TRUE ~ NA_character_
      )

    } else {
      #note: upgrade to data.table::fcase() when data.table is updated to version 1.12.9 (version 1.12.8 is currently installed)
      categories <- dplyr::case_when(icd_codes %in% icd_code_list$infectious_diseases ~ "infection",
                                     icd_codes %in% icd_code_list$malignant_neoplasms ~ "malignant neoplasm",
                                     icd_codes %in% icd_code_list$benign_neoplasms ~ "benign neoplasm",
                                     icd_codes %in% icd_code_list$blood_diseases ~ "blood disease",
                                     icd_codes %in% icd_code_list$diabetes ~ "diabetes",
                                     icd_codes %in% icd_code_list$malnutrition ~ "malnutrition",
                                     icd_codes %in% icd_code_list$obesity ~ "obesity",
                                     icd_codes %in% icd_code_list$other_endocrine_metabolic ~ "other endocrine or metabolic",
                                     icd_codes %in% icd_code_list$neurodegenerative_diseases ~ "neurodegenerative disease",
                                     icd_codes %in% icd_code_list$alcohol_related ~ "alcohol-related",
                                     icd_codes %in% icd_code_list$opioid_related ~ "opioid-related",
                                     icd_codes %in% icd_code_list$drug_related ~ "drug-related",
                                     icd_codes %in% icd_code_list$psychoses ~ "psychosis",
                                     icd_codes %in% icd_code_list$mood_anxiety_disorders ~ "mood or anxiety disorder",
                                     icd_codes %in% icd_code_list$other_mental_disorders ~ "other mental disorder",
                                     icd_codes %in% icd_code_list$nondegen_neurological_diseases ~ "other neurological disease",
                                     icd_codes %in% icd_code_list$eye_ear_diseases ~ "ear or eye disease",
                                     icd_codes %in% icd_code_list$hypertension ~ "hypertension",
                                     icd_codes %in% icd_code_list$heart_disease ~ "heart disease",
                                     icd_codes %in% icd_code_list$cerebrovascular_disease ~ "cerebrovascular disease",
                                     icd_codes %in% icd_code_list$other_circulatory_diseases ~ "other circulatory disease",
                                     icd_codes %in% icd_code_list$influenza_pneumonia ~ "influenza or pneumonia",
                                     icd_codes %in% icd_code_list$chronic_lower_respiratory ~ "chronic lower respiratory disease",
                                     icd_codes %in% icd_code_list$other_respiratory_diseases ~ "other respiratory disease",
                                     icd_codes %in% icd_code_list$liver_disease ~ "liver disease",
                                     icd_codes %in% icd_code_list$other_digestive_diseases ~ "other digestive disease",
                                     icd_codes %in% icd_code_list$cutaneous_diseases ~ "skin or sub-cut. disease",
                                     icd_codes %in% icd_code_list$musculoskeletal_diseases ~ "musculoskeletal disease",
                                     icd_codes %in% icd_code_list$kidney_disease ~ "kidney disease",
                                     icd_codes %in% icd_code_list$other_genitourinary_diseases ~ "other genitourinary disease",
                                     icd_codes %in% icd_code_list$pregnancy_perinatal_related ~ "pregnancy- or perinatal-related",
                                     icd_codes %in% icd_code_list$congenital_abnormalities ~ "congenital abnormality",
                                     icd_codes %in% icd_code_list$unknown_etiology ~ "disease of unknown etiology",
                                     icd_codes %in% icd_code_list$injuries_accidents ~ "injury/accident",
                                     icd_codes %in% icd_code_list$non_OD_suicide ~ "suicide (non-OD)",
                                     icd_codes %in% icd_code_list$homicide ~ "homicide",
                                     icd_codes %in% icd_code_list$other_external_causes ~ "other external cause",
                                     icd_codes %in% icd_code_list$SARS_COVID19 ~ "SARS/COVID-19",
                                     !is.na(icd_codes) ~ "code not recognized",
                                     TRUE ~ NA_character_
      )
    }
  } else if(mortality == FALSE) {
    if(include_code_ranges == TRUE) {
      #note: upgrade to data.table::fcase() when data.table is updated to version 1.12.9 (version 1.12.8 is currently installed)
      #or perhaps using elucidate::translate()
      categories <- dplyr::case_when(icd_codes %in% icd_code_list$infectious_diseases ~
                                       paste0("infection [", icd_code_ranges["infectious_diseases"], "]"),

                                     icd_codes %in% icd_code_list$malignant_neoplasms ~
                                       paste0("malignant neoplasm [", icd_code_ranges["malignant_neoplasms"], "]"),

                                     icd_codes %in% icd_code_list$benign_neoplasms ~
                                       paste0("benign neoplasm [", icd_code_ranges["benign_neoplasms"], "]"),

                                     icd_codes %in% icd_code_list$blood_diseases ~
                                       paste0("blood disease [", icd_code_ranges["blood_diseases"], "]"),

                                     icd_codes %in% icd_code_list$diabetes ~
                                       paste0("diabetes [", icd_code_ranges["diabetes"], "]"),

                                     icd_codes %in% icd_code_list$malnutrition ~
                                       paste0("malnutrition [", icd_code_ranges["malnutrition"], "]"),

                                     icd_codes %in% icd_code_list$obesity ~
                                       paste0("obesity [", icd_code_ranges["obesity"], "]"),

                                     icd_codes %in% icd_code_list$other_endocrine_metabolic ~
                                       paste0("other endocrine or metabolic [", icd_code_ranges["other_endocrine_metabolic"], "]"),

                                     icd_codes %in% icd_code_list$neurodegenerative_diseases ~
                                       paste0("neurodegenerative disease [", icd_code_ranges["neurodegenerative_diseases"], "]"),

                                     icd_codes %in% icd_code_list$alcohol_related ~
                                       paste0("alcohol-related [", icd_code_ranges["alcohol_related"], "]"),

                                     icd_codes %in% icd_code_list$opioid_related ~
                                       paste0("opioid-related [", icd_code_ranges["opioid_related"], "]"),

                                     icd_codes %in% icd_code_list$drug_related ~
                                       paste0("drug-related [", icd_code_ranges["drug_related"], "]"),

                                     icd_codes %in% icd_code_list$psychoses ~
                                       paste0("psychosis [", icd_code_ranges["psychoses"], "]"),

                                     icd_codes %in% icd_code_list$mood_anxiety_disorders ~
                                       paste0("mood or anxiety disorder [", icd_code_ranges["mood_anxiety_disorders"], "]"),

                                     icd_codes %in% icd_code_list$other_mental_disorders ~
                                       paste0("other mental disorder [", icd_code_ranges["other_mental_disorders"], "]"),

                                     icd_codes %in% icd_code_list$nondegen_neurological_diseases ~
                                       paste0("other neurological disease [", icd_code_ranges["nondegen_neurological_diseases"], "]"),

                                     icd_codes %in% icd_code_list$eye_ear_diseases ~
                                       paste0("eye or ear disease [", icd_code_ranges["eye_ear_diseases"], "]"),

                                     icd_codes %in% icd_code_list$hypertension ~
                                       paste0("hypertension [", icd_code_ranges["hypertension"], "]"),

                                     icd_codes %in% icd_code_list$heart_disease ~
                                       paste0("heart disease [", icd_code_ranges["heart_disease"], "]"),

                                     icd_codes %in% icd_code_list$cerebrovascular_disease ~
                                       paste0("cerebrovascular disease [", icd_code_ranges["cerebrovascular_disease"], "]"),

                                     icd_codes %in% icd_code_list$other_circulatory_diseases ~
                                       paste0("other circulatory disease [", icd_code_ranges["other_circulatory_diseases"], "]"),

                                     icd_codes %in% icd_code_list$influenza_pneumonia ~
                                       paste0("influenza or pneumonia [", icd_code_ranges["influenza_pneumonia"], "]"),

                                     icd_codes %in% icd_code_list$chronic_lower_respiratory ~
                                       paste0("chronic lower respiratory disease [", icd_code_ranges["chronic_lower_respiratory"], "]"),

                                     icd_codes %in% icd_code_list$other_respiratory_diseases ~
                                       paste0("other respiratory disease [", icd_code_ranges["other_respiratory_diseases"], "]"),

                                     icd_codes %in% icd_code_list$liver_disease ~
                                       paste0("liver disease [", icd_code_ranges["liver_disease"], "]"),

                                     icd_codes %in% icd_code_list$other_digestive_diseases ~
                                       paste0("other digestive disease [", icd_code_ranges["other_digestive_diseases"], "]"),

                                     icd_codes %in% icd_code_list$cutaneous_diseases ~
                                       paste0("skin or sub-cut. disease [", icd_code_ranges["cutaneous_diseases"], "]"),

                                     icd_codes %in% icd_code_list$musculoskeletal_diseases ~
                                       paste0("musculoskeletal disease [", icd_code_ranges["musculoskeletal_diseases"], "]"),

                                     icd_codes %in% icd_code_list$kidney_disease ~
                                       paste0("kidney disease [", icd_code_ranges["kidney_disease"], "]"),

                                     icd_codes %in% icd_code_list$other_genitourinary_diseases ~
                                       paste0("other genitourinary disease [", icd_code_ranges["other_genitourinary_diseases"], "]"),

                                     icd_codes %in% icd_code_list$pregnancy_perinatal_related ~
                                       paste0("pregnancy- or perinatal-related [", icd_code_ranges["pregnancy_perinatal_related"], "]"),

                                     icd_codes %in% icd_code_list$congenital_abnormalities ~
                                       paste0("congenital abnormality [", icd_code_ranges["congenital_abnormalities"], "]"),

                                     icd_codes %in% icd_code_list$unknown_etiology ~
                                       paste0("disease of unknown etiology [", icd_code_ranges["unknown_etiology"], "]"),

                                     icd_codes %in% icd_code_list$injuries_accidents ~
                                       paste0("injury/accident [", icd_code_ranges["injuries_accidents"], "]"),

                                     icd_codes %in% icd_code_list$non_OD_suicide ~
                                       paste0("suicide (non-OD) [", icd_code_ranges["non_OD_suicide"], "]"),

                                     icd_codes %in% icd_code_list$homicide ~
                                       paste0("homicide [", icd_code_ranges["homicide"], "]"),

                                     icd_codes %in% icd_code_list$other_external_causes ~
                                       paste0("other external cause [", icd_code_ranges["other_external_causes"], "]"),

                                     icd_codes %in% icd_code_list$psychosocioeconomic_circumstances ~
                                       paste0("psychosocioeconomic circumtances influencing health [",
                                              icd_code_ranges["psychosocioeconomic_circumstances"], "]"),

                                     icd_codes %in% icd_code_list$other_factors_influencing_health_status ~
                                       paste0("other factors influencing health status [",
                                              icd_code_ranges["other_factors_influencing_health_status"], "]"),

                                     icd_codes %in% icd_code_list$SARS_COVID19 ~
                                       paste0("SARS/COVID-19 [", icd_code_ranges["SARS_COVID19"], "]"),

                                     !is.na(icd_codes) ~ "code not recognized",

                                     TRUE ~ NA_character_
      )

    } else {
      #note: upgrade to data.table::fcase() when data.table is updated to version 1.12.9 (version 1.12.8 is currently installed)
      categories <- dplyr::case_when(icd_codes %in% icd_code_list$infectious_diseases ~ "infection",
                                     icd_codes %in% icd_code_list$malignant_neoplasms ~ "malignant neoplasm",
                                     icd_codes %in% icd_code_list$benign_neoplasms ~ "benign neoplasm",
                                     icd_codes %in% icd_code_list$blood_diseases ~ "blood disease",
                                     icd_codes %in% icd_code_list$diabetes ~ "diabetes",
                                     icd_codes %in% icd_code_list$malnutrition ~ "malnutrition",
                                     icd_codes %in% icd_code_list$obesity ~ "obesity",
                                     icd_codes %in% icd_code_list$other_endocrine_metabolic ~ "other endocrine or metabolic",
                                     icd_codes %in% icd_code_list$neurodegenerative_diseases ~ "neurodegenerative disease",
                                     icd_codes %in% icd_code_list$alcohol_related ~ "alcohol-related",
                                     icd_codes %in% icd_code_list$opioid_related ~ "opioid-related",
                                     icd_codes %in% icd_code_list$drug_related ~ "drug-related",
                                     icd_codes %in% icd_code_list$psychoses ~ "psychosis",
                                     icd_codes %in% icd_code_list$mood_anxiety_disorders ~ "mood or anxiety disorder",
                                     icd_codes %in% icd_code_list$other_mental_disorders ~ "other mental disorder",
                                     icd_codes %in% icd_code_list$nondegen_neurological_diseases ~ "other neurological disease",
                                     icd_codes %in% icd_code_list$eye_ear_diseases ~ "eye or ear disease",
                                     icd_codes %in% icd_code_list$hypertension ~ "hypertension",
                                     icd_codes %in% icd_code_list$heart_disease ~ "heart disease",
                                     icd_codes %in% icd_code_list$cerebrovascular_disease ~ "cerebrovascular disease",
                                     icd_codes %in% icd_code_list$other_circulatory_diseases ~ "other circulatory disease",
                                     icd_codes %in% icd_code_list$influenza_pneumonia ~ "influenza or pneumonia",
                                     icd_codes %in% icd_code_list$chronic_lower_respiratory ~ "chronic lower respiratory disease",
                                     icd_codes %in% icd_code_list$other_respiratory_diseases ~ "other respiratory disease",
                                     icd_codes %in% icd_code_list$liver_disease ~ "liver disease",
                                     icd_codes %in% icd_code_list$other_digestive_diseases ~ "other digestive disease",
                                     icd_codes %in% icd_code_list$cutaneous_diseases ~ "skin or sub-cut. disease",
                                     icd_codes %in% icd_code_list$musculoskeletal_diseases ~ "musculoskeletal disease",
                                     icd_codes %in% icd_code_list$kidney_disease ~ "kidney disease",
                                     icd_codes %in% icd_code_list$other_genitourinary_diseases ~ "other genitourinary disease",
                                     icd_codes %in% icd_code_list$pregnancy_perinatal_related ~ "pregnancy- or perinatal-related",
                                     icd_codes %in% icd_code_list$congenital_abnormalities ~ "congenital abnormality",
                                     icd_codes %in% icd_code_list$unknown_etiology ~ "disease of unknown etiology",
                                     icd_codes %in% icd_code_list$injuries_accidents ~ "injury/accident",
                                     icd_codes %in% icd_code_list$non_OD_suicide ~ "suicide (non-OD)",
                                     icd_codes %in% icd_code_list$homicide ~ "homicide",
                                     icd_codes %in% icd_code_list$other_external_causes ~ "other external cause",
                                     icd_codes %in% icd_code_list$psychosocioeconomic_circumstances ~ "psychosocioeconomic circumtances influencing health",
                                     icd_codes %in% icd_code_list$other_factors_influencing_health_status ~ "other factors influencing health status",
                                     icd_codes %in% icd_code_list$SARS_COVID19 ~ "SARS/COVID-19",
                                     !is.na(icd_codes) ~ "code not recognized",
                                     TRUE ~ NA_character_
      )
    }
  }
}
