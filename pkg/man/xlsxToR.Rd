\name{xlsxToR}
\alias{xlsxToR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import Excel workbook
}
\description{
Import all or a selection of sheets of an Excel .docx XML workbook (ie Office 2007 or later format).
}
\usage{
xlsxToR(file, keep_sheets = NULL, header = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Filename (can include path if necessary) of a *.docx Excel workbook
}
  \item{keep_sheets}{
Either a character vector of the names of the sheets to import; or NULL in which case all are kept.
}
  \item{header}{
Logical; should the first row of each worksheet be treated as a header
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list, with an element object for each worksheet in the original Excel workbook.
}
\references{
http://housesofstones.com/blog/2013/06/20/quickly-read-excel-xlsx-worksheets-into-r-on-any-platform/#.Ue2XYJlzWTk
}
\author{
Schaun Wheeler, written June 2013 (used without permission, but he did post it on the web)
}
\note{
A reported issue, still to be investigated fully, is that this function may not work with spreadsheets with more than 26 columns.

The \code{xlsx} package provides a superior solution to reading and writing .xlsx files but in our environment only works in the 32 bit version of R (as it depends on Java, which is installed as 32 bit).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read.csv}}, \code{\link{xmlParse}}, \code{\link{odbcConnect}}
}
\examples{


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
\keyword{ database }
