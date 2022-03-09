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
#' The dipr_icd_categories functions have been deprecated. Development on the hawkeye package
#' has superceded this functionality. In particular the *_lookup family of functions provide a
#' wider range of categories and icd codes.
#'
#'
#' @export
#'
#' @author Craig Hutton (`craig.hutton@gov.bc.ca`)
#'
dipr_icd_categories <- function() {

  .Deprecated(
    NULL,
    old = "The dipr_icd_categories functions have been deprecated. Development on the hawkeye package
       has superceded this functionality. In particular the *_lookup family of functions provide a
       wider range of categories and icd codes.
       dipr::install_sre_gitlab('hawkeye') to install that package
       "
  )

}

#' @rdname dipr_icd_categories
#' @export
dipr_icd9_categories <- dipr_icd_categories

#' @rdname dipr_icd_categories
#' @export
dipr_icd10_categories <- dipr_icd_categories
