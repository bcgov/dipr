# Copyright 2020 Province of British Columbia
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

.onLoad <-
  function(libname = find.package("tidyhydat"),
           pkgname = "tidyhydat") {
    # CRAN Note avoidance
    if (getRversion() >= "2.15.1")
      utils::globalVariables(
        # Vars used in Non-Standard Evaluations, declare here to avoid CRAN warnings
        ## This is getting ridiculous
        c("clmtype",
          "servcode",
          "medical",
          "expl_cd1",
          "expl_cd2",
          "expl_cd3",
          "sub_type",
          "feeitem",
          "record_type",
          "expdamt",
          "servunits",
          "refusal",
          "done",
          "colour_string",
          "serv_units",
          "exp_flag",
          "serv_flag",
          "icd9",
          "icd9_1",
          "icd9_2",
          "icd9_3",
          "icd9_4",
          "icd9_5",
          "..cols_to_keep",
          "name",
          "payment_status",
          "paynum",
          "pracnum",
          "servdate",
          "start",
          "studyid",
          "." # piping requires '.' at times
        )
      )
    invisible()
  }
