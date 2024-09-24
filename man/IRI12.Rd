\docType{data}
\name{IRI12}
\alias{IRI12}
\title{Implementation Research Institute 2012 mentoring network}
\format{An igraph network object with 37 vertices and 170 edges}
\description{
  This data set contains a directed network of self-reported mentoring ties.
}
\details{
The data includes an igraph network object 'IRI12' consisting of 37 participants in the 2012
IRI training institute. Shortly after the institute, participants were asked to report
who they received any type of mentoring from in the past [month]. The network is
directed and ties are present or absent (unweighted). Attributes of the network members include:
their name (id), year, discipline, institution, and role.
}

\section{Node characteristics:}{
  \itemize{ \item \code{state} - State where LHD is located
    \item \code{popmil} - LHD jurisdiction population in millions
    \item \code{years} - Code for number of years the current LHD
      leader has been in their position:
        \itemize{ \item \emph{0} - 1-2 years
        \item \emph{1} - 3-5 years
        \item \emph{2} - 6-10 years
        \item \emph{3} - 11+ years
        }
    \item \code{hivscreen} - Binary code indicating whether the LHD does
          HIV screening (Y/N)
    \item \code{nutrition} - Binary code indicating whether the LHD does
          nutrition programming (Y/N)

      }
}

\source{The data were collected in 2012 by the Center for Public Health Systems Science, as part of an evaluation
of the Implementation Research Institute.}

\references{
  Luke, D. A., Baumann, A. A., Carothers, B. J., Landsverk, J., & Proctor, E. K. (2016). Forging a link between 
  mentoring and collaboration: A new training model for implementation science. \emph{Implementation Science, 11}, 137.
}
\keyword{datasets}

