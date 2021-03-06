% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h2o_shap.R
\name{h2o.shap.varimp}
\alias{h2o.shap.varimp}
\title{h2o.shap.varimp}
\usage{
h2o.shap.varimp(shap_values)
}
\arguments{
\item{shap_values}{shap value matrix from \code{h2o.predict_contribution}}
}
\value{
data.table variable importance
}
\description{
calculate variable importance from shap value matrix
}
\examples{
data(shap_values_mtcars)
h2o.shap.varimp(shap_values_mtcars)
}
