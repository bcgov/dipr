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

#' Set gitlab credentials from R
#'
#' Running this function raises the Windows Git Credential Manager to enter
#' the GitLat PAT. Accessing the GitLab PAT requires following these steps:
#' - Go to this website: [https://projectsc.popdata.bc.ca/](https://projectsc.popdata.bc.ca/).
#' - Scroll down and click on "Sign in with Pop Data SSO".
#' - Next go to this site: [https://projectsc.popdata.bc.ca/profile/personal_access_tokens](https://projectsc.popdata.bc.ca/profile/personal_access_tokens)
#' - Create a new Personal Access Token enabling each scope and typing in your username. This should create a new personal access token. Those are your "git credentials". Save those on your "U" drive along with your user name.
#' @inheritDotParams credentials::git_credential_ask save verbose
#' @export
set_gitlab_credentials <- function(...) {
  credentials::git_credential_ask(url = "https://projectsc.popdata.bc.ca", ...)
}

