% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum0.R
\encoding{UTF-8}
\name{sum0}
\alias{sum0}
\alias{sd0}
\alias{cor0}
\alias{cov0}
\alias{min0}
\alias{max0}
\alias{var0}
\alias{pmin0}
\alias{pmax0}
\alias{mean0}
\alias{csds0}
\alias{rsds0}
\alias{cvars0}
\alias{rvars0}
\alias{csums0}
\alias{rsums0}
\alias{cmeans0}
\alias{rmeans0}
\title{Compute stats ignoring \code{NA} values}
\usage{
sum0(...)

sd0(...)

cor0(x, y = NULL)

cov0(x, y = NULL)

min0(...)

max0(...)

var0(...)

pmin0(...)

pmax0(...)

mean0(...)

csds0(x)

rsds0(x)

cvars0(x)

rvars0(x)

csums0(x)

rsums0(x)

cmeans0(x)

rmeans0(x)
}
\arguments{
\item{...}{Scalars, vectors, or matrices. Reduced a single vector of atomic values for \code{sd0}, \code{min0}, \code{max0}, and \code{mean0}.}

\item{x}{A \link[=cmp_num_vec]{complete numeric vec} or a \link[=cmp_num_mat]{complete numeric matrix}.}

\item{y}{An optional complete numeric vec or a complete numeric matrix.}
}
\value{
\strong{A numeric vector}           \cr\cr \verb{cmeans0, csums0, cvars0, csds0} \cr \verb{rmeans0, rsums0, rvars0, rsds0} \cr \verb{mean0, sum0, var0, sd0} \cr \verb{pmin0, min0} \cr \verb{pmax0, max0}
\cr\cr  \strong{A numeric vector or matrix} \cr\cr \verb{cor0, cov0}
}
\description{
Simple wrappers for calculating statistics ignoring any \code{NA} values.
}
\details{
\tabular{ll}{  \code{cmeans0}   \tab Calls \code{colMeans(x, na.rm = T)}          \cr
  \code{rmeans0}   \tab Calls \code{rowMeans(x, na.rm = T)}          \cr
  \code{csums0}    \tab Calls \code{colSums(x, na.rm = T)}           \cr
  \code{rsums0}    \tab Calls \code{rowSums(x, na.rm = T)}           \cr
  \code{csds0}     \tab Calls \code{apply(x, 2, sd, na.rm = T)}      \cr
  \code{rsds0}     \tab Calls \code{apply(x, 1, sd, na.rm = T)}      \cr
  \code{pmin0}     \tab Calls \code{pmin(..., na.rm = T)}            \cr
  \code{pmax0}     \tab Calls \code{pmax(..., na.rm = T)}            \cr
  \code{mean0}     \tab Calls \code{mean(av(...), na.rm = T)}        \cr
  \code{min0}      \tab Calls \code{min(av(...), na.rm = T)}         \cr
  \code{max0}      \tab Calls \code{max(av(...), na.rm = T)}         \cr
  \code{sum0}      \tab Calls \code{sum(av(...), na.rm = T)}         \cr
  \code{var0}      \tab Calls \code{var(av(...), na.rm = T)}         \cr
  \code{cor0}      \tab Calls \code{cor(x, y, use = 'complete.obs')} \cr
  \code{cov0}      \tab Calls \code{cov(x, y, use = 'complete.obs')} \cr
  \code{sd0}       \tab Calls \code{sd(av(...), na.rm = T)}            }
}
\examples{
egVec1 <- sample(0:99, 10)
egVec2 <- sample(0:99, 20)
egVec3 <- sample(0:99, 20)
egMat1 <- sample(0:99, 100)

egVec1[sample(1:10, 1)] <- NA
egVec2[sample(1:20, 1)] <- NA
egVec3[sample(1:20, 2)] <- NA
egMat1[sample(1:100, 5)] <- NA
egMat1 <- matrix(mat1, nrow = 10)
rownames(egMat1) <- paste0("R", 1:10)
colnames(egMat1) <- paste0("C", 1:10)
egDtf1 <- as.data.frame(egMat1)
egX <- list(v1 = egVec1, v2 = egVec2, v3 = egVec3, m = egMat1, d = egDtf1)

egVec1
egVec2
egVec3
egMat1
egDtf1
egX

matrix(c(cmeans0(egMat1), cmeans0(egDtf1)), ncol = 2)
matrix(c(rmeans0(egMat1), rmeans0(egDtf1)), ncol = 2)
matrix(c(csums0( egMat1), csums0( egDtf1)), ncol = 2)
matrix(c(rsums0( egMat1), rsums0( egDtf1)), ncol = 2)
matrix(c(csds0(  egMat1), csds0(  egDtf1)), ncol = 2)
matrix(c(rsds0(  egMat1), rsds0(  egDtf1)), ncol = 2)

pmin0(egVec1, egVec2, egVec3)
pmax0(egVec2, egVec2, egVec3)

cor0(egVec2, egVec3)
cov0(egVec2, egVec3)

cor0(egMat1)
cov0(egMat1)

list(mean = mean0(egX), min = min0(egX), max = max0(egX),
      sum = sum0( egX), var = var0(egX), sd  = sd0( egX))
}
\seealso{
Other missingness: 
\code{\link{na}()}

Other extensions: 
\code{\link{N}()},
\code{\link{atomize}()},
\code{\link{case}()},
\code{\link{cb}()},
\code{\link{comparable}()},
\code{\link{compatible}()},
\code{\link{declare}},
\code{\link{dups}()},
\code{\link{failsafe}()},
\code{\link{flex_rbind}()},
\code{\link{fork}()},
\code{\link{is_unique}()},
\code{\link{naming}},
\code{\link{na}()},
\code{\link{paths}},
\code{\link{purge}()},
\code{\link{rd}()},
\code{\link{r}()},
\code{\link{sets}()},
\code{\link{swap}()},
\code{\link{value_exists}()}
}
\concept{extensions}
\concept{missingness}
