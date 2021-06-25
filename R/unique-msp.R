#' Process msp data and return unique visits
#'
#' This method takes the raw msp input object (using `read_dat`) and
#' returns a condensed data.table that has one row per unique
#' visit to a doctor or hospital. This function requires data to be in
#' msp.c format with those column heading.
#'
#' @param msp_c A data.table object of msc.c status.
#' @param primary_icd Default FALSE. Set to TRUE is memory issues are occuring. This feature
#' returns only the primary ICD 9 code
#' @author Jenny Sutherland, Sam Albers
#'
#' @export

msp_unique <- function(msp_c, primary_icd = FALSE){

  if (!inherits(msp_c, "data.table")) {
    stop("The 'msp_c' object must of class 'data.table'")
  }

  # Create required flags
  msp_rules <- create_unique_flags(msp_c)

  rm(msp_c); gc()

  ffs_paid <- process_fss_records(msp_rules, primary_icd)

  encounter_raw <- process_encounter_records(msp_rules, primary_icd)

  msp_aro <-  bind_ffs_encounter(ffs_paid, encounter_raw)

  gc()

  return(msp_aro)
}



# Create FFS and Encounter flag

## Rule 1
create_unique_flags <- function(msp_raw) {


    ## Rule 1
    msp_raw[expl_cd1 == "RE" | expl_cd2 == "RE" | expl_cd3 == "RE", sub_type := "EC1"]
    msp_raw[servcode %in% c(65, 66, 67), sub_type := "EC2"]
    msp_raw[data.table::between(feeitem, 96700, 96899) |
              data.table::between(feeitem, 96002, 96093)
            & is.na(expl_cd1), sub_type := "EC3" ]
    msp_raw[sub_type %in% c("EC1", "EC2", "EC3"), record_type := "Encounter"]
    msp_raw[is.na(record_type), record_type := "FFS"]


    ## Rule 2
    cols_to_rowsum <- c("paidamt", "niaamt", "rrpamt", "dscamt", "pysychprm", "rvgamt", "rollamt", "prorattn", "fitmrol", "retro_amt")
    msp_raw[, expdamt := Reduce(`+`, .SD), .SDcol = cols_to_rowsum]

    msp_raw[expdamt == 0 & servunits == 0, refusal := 1]
    msp_raw[is.na(refusal), refusal := 0]


    return(msp_raw)

}


## Apply sum-group-by method

process_fss_records <- function(msp_rules, primary_icd) {

  if (primary_icd) {
    ffs_sum <- msp_rules[record_type == "FFS" &
                           refusal == 0, .(servunits = sum(servunits),
                                           expdamt = sum(expdamt)),
                         by = .(studyid, servdate, pracnum, feeitem,
                                servcode, clmtype, paynum, icd9, refusal
                         )]
  } else {
    ffs_sum <- msp_rules[record_type == "FFS" &
                           refusal == 0, .(servunits = sum(servunits),
                                           expdamt = sum(expdamt)),
                         by = .(studyid, servdate, pracnum, feeitem,
                                servcode, clmtype, paynum, icd9, icd9_1,
                                icd9_2, icd9_3, icd9_4, icd9_5, refusal
                         )]
  }






  ## Step 3: Define MSP claim types based on total servunits and expdamt.
  #****************************************************************************************************/
  #* ---        The rationale is to make sure our method behaves properly and it doesn't remove   --- */
  #* ---        too many records such as un-matched reversals.                                    --- */
  #* ---        We will only include Paid Claims record type into the final MSP file in step 4.   --- */
  #****************************************************************************************************/


  ffs_sum[servunits > 0, serv_flag := ">0"]
  ffs_sum[servunits == 0, serv_flag := "=0"]
  ffs_sum[is.na(serv_flag), serv_flag := "<0"]


  ffs_sum[expdamt > 0, exp_flag := ">0"]
  ffs_sum[expdamt == 0, exp_flag := "=0"]
  ffs_sum[is.na(exp_flag), exp_flag := "<0"]

  ffs_sum[serv_flag == '>0' & exp_flag %in% c("=0", ">0"), sub_type := 'Paid Claim']
  ffs_sum[serv_flag == '=0', sub_type := 'Reversed']
  ffs_sum[is.na(sub_type), sub_type := 'Unmatched']


  ## Step 4: Create FFS paid claim dataset;
  ## Create FFS Paid Claims dataset;

  ffs_paid <- ffs_sum[sub_type == 'Paid Claim', ]
  ffs_paid[, payment_status := 'FFS Paid']

  return(ffs_paid)
}

process_encounter_records <- function(msp_rules, primary_icd) {


  msp_rules <- msp_rules[record_type == 'Encounter']

  msp_rules[servunits > 0, serv_flag := ">0"]
  msp_rules[servunits == 0, serv_flag := "=0"]
  msp_rules[is.na(serv_flag), serv_flag := "<0"]

  msp_rules[expdamt > 0, exp_flag := ">0"]
  msp_rules[expdamt == 0, exp_flag := "=0"]
  msp_rules[is.na(exp_flag), exp_flag := "<0"]


  msp_rules[serv_flag == '<0', sub_type := 'Reversal']
  msp_rules[serv_flag != '<0', sub_type := 'Encounter']

  # encounter_rec <- msp_rules %>%
  #   filter(record_type == "Encounter") %>%
  #   mutate(serv_flag = case_when(servunits > 0 ~ ">0",
  #                                servunits == 0 ~ "=0",
  #                                TRUE ~ "<0")) %>%
  #   mutate(exp_flag = case_when(expdamt > 0 ~ ">0",
  #                               expdamt == 0 ~ "=0",
  #                               TRUE ~ "<0")) %>%
  #   mutate(sub_type = case_when(serv_flag == '<0' ~ 'Reversal',
  #                               TRUE ~ "Encounter"))



  ## Note .. in front of the cols_to_keep. This evaluates this variable.
  if (primary_icd) {
    cols_to_keep <- c("studyid", "servdate", "pracnum", "feeitem", "servcode", "clmtype", "paynum", "icd9", "servunits", "expdamt", "sub_type")
  } else {
    cols_to_keep <- c("studyid", "servdate", "pracnum", "feeitem", "servcode", "clmtype", "paynum", "icd9", "icd9_1", "icd9_2", "icd9_3",
                      "icd9_4", "icd9_5", "servunits", "expdamt", "sub_type")
  }

  ## Create encounter dataset
  encounter_rec <- msp_rules[sub_type == "Encounter", ..cols_to_keep]

  setnames(encounter_rec, "sub_type", "payment_status")

  return(encounter_rec)

  # encounter_rec %>%
  #   filter(sub_type == "Encounter") %>%
  #   rename(payment_status = sub_type) %>%
  #   select(studyid, servdate, pracnum, feeitem, servcode, clmtype, paynum, icd9, icd9_1, icd9_2, icd9_3,
  #          icd9_4, icd9_5, servunits, expdamt, payment_status)
}


bind_ffs_encounter <- function(ffs_paid, encounter_raw){

  d <- rbindlist(list(ffs_paid, encounter_raw), use.names = TRUE, fill = TRUE)

  d[clmtype %in% c('MA','MB','MH','MM','MN','MS','PB','PN') & servcode < 81, medical := 'Y']
  d[medical != 'Y', medical := 'N']

  return(d)
  # ffs_paid %>%
  #   bind_rows(encounter_raw) %>%
  #   mutate(medical = case_when(
  #     clmtype %in% c('MA','MB','MH','MM','MN','MS','PB','PN') & servcode < 81 ~ 'Y',
  #     TRUE ~ 'N'
  #   ))
}




