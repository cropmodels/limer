\name{CCE}

\alias{effective_CCE}
\alias{CCE}

\title{
Calcium carbonate equivalents
}

\description{
Calcium carbonate equivalents. \code{Effective_CCE} returns an ajusted CCE based on mesh sizes of the product. \code{CCE} returns a table with CCE values for different lime products.
}


\usage{
effective_CCE(CCE, fine, medium)
CCE()
}
 
\arguments{
  \item{CCE}{Calcium carbonate equivalent in \%}
  \item{fine}{fraction smaller than 60-mesh size}
  \item{medium}{fraction between between 8- and 60-mesh size}
}

