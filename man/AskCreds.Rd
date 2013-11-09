\name{AskCreds}
\alias{AskCreds}
\title{
Ask for user credentials
}
\description{
A tiny utility function that provides a GUI to get a user's credentials for a database, so user id and password do not need to be included in a script file.
}
\usage{
AskCreds(Title = "User credentials for a database", startuid = "", returnValOnCancel = "ID_CANCEL")
}
\arguments{
\item{Title}{
Title of the dialogue box that asks for user credentials.  Can be used within a program so the user knows what credentials they are being asked for (eg for a particular database, the internet proxy, etc)
}

\item{startuid}{
Userid to provide to user as default.  Defaults to "".
}
  \item{returnValOnCancel}{
What value to return in the event user chooses to cancel.
}
}
\details{
This function was built because of a known issue with RStudio that prevents \code{odbcConnect()} functioning properly.  When the userid and password are not included in the call to \code{odbcConnect()} the database client is supposed to pop up a window asking for them; for some reason RStudio prevents this.  \code{AskCreds()} is a workaround function to allow similar functionality.
}
\value{
A list with two values
  \item{uid }{User id provided by the user, for subsequent use in odbcConnect()}
  \item{pwd }{Password provided by the user, for subsequent use in odbcConnect()}

}
\references{
\link{http://support.rstudio.org/help/discussions/problems/638-rodbc-window}
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}

\seealso{
\code{\link[RODBC]{odbcConnect}}

}
\examples{
  \dontrun{
    require(RODBC)
    creds <- AskCreds()
    TRED1 <- odbcConnect("TRED64", uid=creds$uid, pwd=creds$pwd)
    }
}

\keyword{ connection }

