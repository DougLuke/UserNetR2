\docType{data}
\name{IRI12}
\alias{IRI12}
\title{Implementation Research Institute 2012 mentoring network}
\format{An igraph network object with 38 vertices and 142 edges}
\description{
  This data set contains a directed network of self-reported mentoring ties.
}
\details{
The data includes an igraph network object 'IRI12' consisting of 37 participants in the 2012
IRI training institute. Shortly after the institute, participants were asked to report
who they received mentoring from or who they provided mentoring to in the past year. The network is
directed and ties are present or absent (unweighted). Attributes of the network members include:
their name (id), cohort year, discipline, institution, and role.
}

\section{Node characteristics:}{
  \itemize{ \item \code{name} - ID string
    \item \code{Year} - Cohort year for IRI fellows; 'Core' for faculty
    \item \code{Role2} - Whether the participant is a Fellow or Core teaching faculty
    \item \code{Discipline} - Primary discipline for terminal degree
    \item \code{Institution} - Institution where they work or are enrolled

      }
}

\source{The data were collected in 2012 by the Center for Public Health Systems 
Science (\url{https://cphss.wustl.edu/}), as part of an evaluation
of the Implementation Research Institute.}

\references{
  Luke, D. A., Baumann, A. A., Carothers, B. J., Landsverk, J., & Proctor, E. K. (2016). Forging a link between 
  mentoring and collaboration: A new training model for implementation science. \emph{Implementation Science, 11}, 137.
}
\keyword{datasets}

