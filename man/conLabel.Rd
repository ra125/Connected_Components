\name{conLabel}
\alias{conLabel}

\title{
Assigns labels to connected components in a logical matrix
}
\description{
Assigns labels to connected components in a logical matrix
}
\usage{
conLabel(x,N=4)
}

\arguments{
  \item{x}{
A logical matrix whose connected components are to be detected
}
  \item{N}{
Number specifying whether to use 4 or 8 neighborhood connectivity (Can only be 4 or 8. Default is 4)
}
}
\value{
A matrix with labeled connected components
}

\examples{
x=matrix(c(1,1,1,0,0,0,0,0,1,1,1,0,1,1,0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,1,0,0,1,1,0,1,1,1,0,0,0,0,0),byrow=TRUE,nrow=8)

conLabel(x)
conLabel(x,4)
conLabel(x,8)
}

