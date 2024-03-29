% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dat-to-arrow-formats.R
\name{dat_to_datasets}
\alias{dat_to_datasets}
\title{Convert dat.gz file to Apache Arrow Datasets using partitioned folder structures}
\usage{
dat_to_datasets(
  data_path,
  data_dict,
  chunk_size = 1e+06,
  path,
  partitioning,
  tz = "UTC",
  date_format = "\%AD",
  time_format = "\%AT",
  ...
)
}
\arguments{
\item{data_path}{A path or a vector of paths to a \code{.dat.gz} file. If supplying a vector of paths,
they must share a common data dictionary.}

\item{data_dict}{A data.frame with \code{start}, \code{stop} and \code{name} columns}

\item{chunk_size}{The number of rows to include in each chunk. The value of this
parameter you choose will depend on both the number of rows in the data you are
trying to process \emph{and} the RAM available. You can check the RAM available using
\code{memory.size(max = TRUE)}. The default for this values is currently 10 million.}

\item{path}{string path, URI, or \code{SubTreeFileSystem} referencing a directory
to write to (directory will be created if it does not exist)}

\item{partitioning}{\code{Partitioning} or a character vector of columns to
use as partition keys (to be written as path segments). Default is to
use the current \code{group_by()} columns.}

\item{tz}{what timezone should datetime fields use? Default UTC. This is recommended
to avoid timezone pain, but remember that the data is in UTC when doing analysis. See
OlsonNames() for list of available timezones.}

\item{date_format}{date format for columns where date format is not specified in \code{col_types}}

\item{time_format}{time format for columns where time format is not specified in \code{col_types}}

\item{...}{
  Arguments passed on to \code{\link[readr:read_fwf]{readr::read_fwf}}
  \describe{
    \item{\code{file}}{Either a path to a file, a connection, or literal data
(either a single string or a raw vector).

Files ending in \code{.gz}, \code{.bz2}, \code{.xz}, or \code{.zip} will
be automatically uncompressed. Files starting with \verb{http://},
\verb{https://}, \verb{ftp://}, or \verb{ftps://} will be automatically
downloaded. Remote gz files can also be automatically downloaded and
decompressed.

Literal data is most useful for examples and tests. To be recognised as
literal data, the input must be either wrapped with \code{I()}, be a string
containing at least one new line, or be a vector containing at least one
string with a new line.

Using a value of \code{\link[readr:clipboard]{clipboard()}} will read from the system clipboard.}
    \item{\code{col_positions}}{Column positions, as created by \code{\link[readr:read_fwf]{fwf_empty()}},
\code{\link[readr:read_fwf]{fwf_widths()}} or \code{\link[readr:read_fwf]{fwf_positions()}}. To read in only selected fields,
use \code{\link[readr:read_fwf]{fwf_positions()}}. If the width of the last column is variable (a
ragged fwf file), supply the last end position as NA.}
    \item{\code{col_types}}{One of \code{NULL}, a \code{\link[readr:cols]{cols()}} specification, or
a string. See \code{vignette("readr")} for more details.

If \code{NULL}, all column types will be imputed from \code{guess_max} rows
on the input interspersed throughout the file. This is convenient (and
fast), but not robust. If the imputation fails, you'll need to increase
the \code{guess_max} or supply the correct types yourself.

Column specifications created by \code{\link[=list]{list()}} or \code{\link[readr:cols]{cols()}} must contain
one column specification for each column. If you only want to read a
subset of the columns, use \code{\link[readr:cols]{cols_only()}}.

Alternatively, you can use a compact string representation where each
character represents one column:
\itemize{
\item c = character
\item i = integer
\item n = number
\item d = double
\item l = logical
\item f = factor
\item D = date
\item T = date time
\item t = time
\item ? = guess
\item _ or - = skip

By default, reading a file without a column specification will print a
message showing what \code{readr} guessed they were. To remove this message,
set \code{show_col_types = FALSE} or set `options(readr.show_col_types = FALSE).
}}
    \item{\code{col_select}}{Columns to include in the results. You can use the same
mini-language as \code{dplyr::select()} to refer to the columns by name. Use
\code{c()} or \code{list()} to use more than one selection expression. Although this
usage is less common, \code{col_select} also accepts a numeric column index. See
\code{\link[tidyselect:language]{?tidyselect::language}} for full details on the
selection language.}
    \item{\code{id}}{The name of a column in which to store the file path. This is
useful when reading multiple input files and there is data in the file
paths, such as the data collection date. If \code{NULL} (the default) no extra
column is created.}
    \item{\code{locale}}{The locale controls defaults that vary from place to place.
The default locale is US-centric (like R), but you can use
\code{\link[readr:locale]{locale()}} to create your own locale that controls things like
the default time zone, encoding, decimal mark, big mark, and day/month
names.}
    \item{\code{na}}{Character vector of strings to interpret as missing values. Set this
option to \code{character()} to indicate no missing values.}
    \item{\code{comment}}{A string used to identify comments. Any text after the
comment characters will be silently ignored.}
    \item{\code{trim_ws}}{Should leading and trailing whitespace (ASCII spaces and tabs) be trimmed from
each field before parsing it?}
    \item{\code{skip}}{Number of lines to skip before reading data.}
    \item{\code{n_max}}{Maximum number of lines to read.}
    \item{\code{guess_max}}{Maximum number of lines to use for guessing column types.
See \code{vignette("column-types", package = "readr")} for more details.}
    \item{\code{progress}}{Display a progress bar? By default it will only display
in an interactive session and not while knitting a document. The automatic
progress bar can be disabled by setting option \code{readr.show_progress} to
\code{FALSE}.}
    \item{\code{name_repair}}{Handling of column names. The default behaviour is to
ensure column names are \code{"unique"}. Various repair strategies are
supported:
\itemize{
\item \code{"minimal"}: No name repair or checks, beyond basic existence of names.
\item \code{"unique"} (default value): Make sure names are unique and not empty.
\item \code{"check_unique"}: no name repair, but check they are \code{unique}.
\item \code{"universal"}: Make the names \code{unique} and syntactic.
\item A function: apply custom name repair (e.g., \code{name_repair = make.names}
for names in the style of base R).
\item A purrr-style anonymous function, see \code{\link[rlang:as_function]{rlang::as_function()}}.
}

This argument is passed on as \code{repair} to \code{\link[vctrs:vec_as_names]{vctrs::vec_as_names()}}.
See there for more details on these terms and the strategies used
to enforce them.}
    \item{\code{num_threads}}{The number of processing threads to use for initial
parsing and lazy reading of data. If your data contains newlines within
fields the parser should automatically detect this and fall back to using
one thread only. However if you know your file has newlines within quoted
fields it is safest to set \code{num_threads = 1} explicitly.}
    \item{\code{show_col_types}}{If \code{FALSE}, do not show the guessed column types. If
\code{TRUE} always show the column types, even if they are supplied. If \code{NULL}
(the default) only show the column types if they are not explicitly supplied
by the \code{col_types} argument.}
    \item{\code{lazy}}{Read values lazily? By default the file is initially only
indexed and the values are read lazily when accessed. Lazy reading is
useful interactively, particularly if you are only interested in a subset
of the full dataset. \emph{Note}, if you later write to the same file you read
from you need to set \code{lazy = FALSE}. On Windows the file will be locked
and on other systems the memory map will become invalid.}
    \item{\code{skip_empty_rows}}{Should blank rows be ignored altogether? i.e. If this
option is \code{TRUE} then blank rows will not be represented at all.  If it is
\code{FALSE} then they will be represented by \code{NA} values in all the columns.}
  }}
}
\description{
Some large files in the SRE are too large to fit into memory. \code{dat_to_datasets}
reads files into memory in smaller chunks (controlled by the \code{chunk_size} argument)
and converts them into Arrow Datasets. All \code{...} argument are passed to \code{dipr::read_dat}
which is where column types can be specified.
}
\examples{
\dontrun{
data_dict_path <- dipr_example("starwars-dict.txt")
dict <- read.table(data_dict_path)
dat_path <- dipr_example("starwars-fwf.dat.gz")

## Create a partitioned datasets in the "bar" folder
dat_to_datasets(
    data_path = dat_path,
    data_dict = dict,
    path = "starwars_arrow",
    partitioning = "species",
    chunk_size = 2)
    }

}
