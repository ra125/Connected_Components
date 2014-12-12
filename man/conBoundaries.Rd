\name{conBoundaries}
\alias{conBoundaries}

\title{
Detects internal and external boundaries of connected components in a logical matrix
}
\description{
Detects internal and external boundaries of connected components in a logical matrix
}
\usage{
conBoundaries(x,N=4)
}
\arguments{
  \item{x}{
A logical matrix in which boundaries are to be detected
}
  \item{N}{
Number specifying whether to use 4 or 8 neighborhood connectivity (Can only be 4 or 8. Default is 4)
}
}

\note{
This function calls conLabel inside it
}

\examples{
x=matrix(c(1,1,1,0,0,0,0,0,1,1,1,0,1,1,0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,1,0,0,1,1,0,1,1,1,0,0,0,0,0),byrow=TRUE,nrow=8)

conBoundaries(x)
conBoundaries(x,4)
conBoundaries(x,8)
}