# gitlab token - must have API access included in scope
token <- gitcreds::gitcreds_get("https://projectsc.popdata.bc.ca")$password

# get a release - note the use of the project ID (90) vs the name (dipr)
resp <- httr::GET("https://projectsc.popdata.bc.ca/api/v4/projects/90/releases/v1.1.0",
                  config = httr::add_headers(`Private-token` = token))

httr::content(resp)

# get a file - note the 'raw' endpoint to just get the file
resp <- httr::GET("https://projectsc.popdata.bc.ca/api/v4/projects/90/repository/files/DESCRIPTION/raw?ref=develop",
                  config = httr::add_headers(`Private-token` = token))

gitlab_version <- read.dcf(textConnection(httr::content(resp, as = "parsed")), fields = "Version")

installed_version <- read.dcf("U:/RLIBS/dipr/DESCRIPTION", fields = "Version")

package_version(gitlab_version) > package_version(installed_version)
