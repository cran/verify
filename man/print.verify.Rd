\name{print.verify}
\alias{print.verify}

\title{ Print Verify Object }
\description{
  Prints an object of class \code{verify}.
}
\usage{
print.verify(x, short = FALSE, ...)
}

\arguments{
  \item{x}{ an object of class \code{verify}. }
  \item{short}{ logical value; if \code{TRUE}, then the character 
  strings of commands are not printed. }
  \item{\dots}{ additional arguments to \code{print.default} may be given. }
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  \code{x} (returned invisibly).

  A representation of the object is printed.
}
\references{ S Poetry, Patrick J. Burns, Section 3.6 }
\author{ Nick Efthymiou }
%\note{ ~~further notes~~ }

\seealso{ \code{\link{verify}} }

\examples{
verify(c("sin(1:9)", "2 * 2"))
}
\keyword{ programming }
\keyword{ print }
