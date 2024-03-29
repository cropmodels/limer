\name{CCE}

\alias{CCE_adjust_size}
\alias{CCE_adjust_depth}
\alias{CCE}

\title{
Calcium carbonate equivalents
}

\description{
Calcium carbonate equivalents. 

\code{CCE} returns a table with CCE values for different lime products  (\href{https://extension.psu.edu/soil-acidity-and-aglime}{source})

Adjust CCE based on the mesh sizes of the product. 
}


\usage{
CCE()

CCE_adjust_size(CCE, fine, medium)
}
 
\arguments{
  \item{CCE}{Calcium carbonate equivalent in \%}
  \item{fine}{fraction smaller than 60-mesh size}
  \item{medium}{fraction between between 8- and 60-mesh size}
}


\examples{
CCE()

CCE_adjust_size(100, 0.6, 0.4)
}