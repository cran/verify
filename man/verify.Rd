\name{verify}
\alias{verify}
\alias{[.verify}
\alias{verify.default}
\alias{verify.verify}
\title{ Test Suite }
\description{
  \code{verify} is a generic function that
  creates or uses an object of class \code{"verify"} to check returned values
  of \R functions.
}
\usage{
verify(x, ...)
verify.default(x, data = list(), ...)
verify.verify(x, ...)
}
\arguments{
  \item{x}{ either a vector of character strings that are commands to 
	be tested, or an object of class \code{"verify"}. \cr
	If a character vector is given, it is often useful if it has names 
	that summarize the commands.
  }
  \item{data}{ a named list of data to use in the commands. }
  \item{\dots}{ generic arguments. }
}
\details{
  The commands may include the output from previous commands in the
  object.
  The result of any previous command is available as `Test.' plus the name
  of the test.
  The index number is used if there are no names.

  \code{verify} is valuable to insure that changes in the code, the \R version,
  the operating system or the hardware do not change (substantially) the
  results of the tested commands.

  There is a \code{print} method for the \code{"verify"} class of objects.
  There is also a method for \code{[}.
}
\value{
  an object of class \code{"verify"} that is a list of the results of the 
  commands.
  In addition to \code{class} and possibly \code{names}, it has the 
  following attributes:
  \item{data}{the input or default value of \code{data}.}
  \item{passed}{a logical vector, or a list that is the result 
	of \code{all.equal} on the new versus original results 
	for each command.}
  \item{random.seed}{the random seed needed to reproduce results that 
	depend on the random number generator.}
  \item{specifics}{a list providing the details of how and when the 
	object was created.}
}
\references{ S Poetry, Patrick J. Burns, Section 3.6, p. 52--58,
	\url{www.seanet.com/~pburns/Spoetry}
}
\author{ Nick Efthymiou }
\note{ If any of the commands require random numbers, then 
\code{.Random.seed} is created or changed in the current directory. }

\seealso{ \code{\link{print.verify}}, \code{\link{all.equal}} }

\examples{
\dontrun{
jjverif <- dget(file.path("tests", "jjverif"))
print(verify(jjverif),short=TRUE)
jjdig <- dget(file.path("tests", "jjdig"))
jjg <- dget(file.path("tests", "jjg"))
save(list = ls(pattern="jj"), 
     file=file.path("tests", "jjverifgraph.rda"), 
     ascii=TRUE, compress=TRUE)
load(file.path("tests", "jjverifgraph.rda"))
verify(jjverifgraph)
# Exclude two tests that use empty mathgraph()
print(verify(jjverifgraph[c(-4,-13)]), short = TRUE)
}
}
\keyword{ programming }
