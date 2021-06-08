\name{CCE}

\alias{CCE_adjust_size}
\alias{CCE_adjust_depth}
\alias{CCE}

\title{
Calcium carbonate equivalents
}

\description{
Calcium carbonate equivalents. Adjuste CCE based on the depth of incorporaton in the soil or on the mesh sizes of the product. \code{CCE} returns a table with CCE values for different lime products.
}


\usage{
CCE_adjust_size(CCE, fine, medium)
CCE_adjust_depth(CCE, depth)
CCE()
}
 
\arguments{
  \item{CCE}{Calcium carbonate equivalent in \%}
  \item{fine}{fraction smaller than 60-mesh size}
  \item{medium}{fraction between between 8- and 60-mesh size}
  \item{depth}{depth of incorporaton in the soil (cm)}
}

