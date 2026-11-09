\name{attribute_verification_status}
\alias{attribute_verification_status}
\title{Classify listed fields from independent verification}
\usage{
attribute_verification_status(
  audit, targets, independent_methods, id = "npi", year = "year"
)
}
\arguments{
\item{audit}{Reconciled \code{\link{attribute_nonresponse_audit}} object.
Supply a \code{verification_method} column on answers before reconciliation.}
\item{targets}{Data frame of unique source and attribute pairs. Every
sampled physician-year is evaluated for each pair.}
\item{independent_methods}{Character vector of methods accepted as
independent evidence by the study protocol.}
\item{id, year}{Identifier and year column names in the original sample.}
}
\value{
A list with \code{fields} (all targets with original sample columns),
\code{validity} (eligible fields with a listing status),
\code{exclusions}, and the original \code{items} for answer provenance.
Analyze each source and attribute separately to preserve the sampling unit.
}
\description{Classify listed fields from frozen claim answers.}
\details{
An independently confirmed error makes the field invalid. A field is valid
only when every listed value is independently confirmed correct. Missing,
untrusted, or mixed evidence remains unresolved. Fields with no listed value,
entirely inapplicable fields, and confirmed ineligible physicians are
explicitly excluded. A physician without contact attempts or answers has
status \code{not_attempted} for otherwise eligible fields.
}
\seealso{\code{\link{validity_estimates}},
\code{\link{validity_sensitivity_bounds}}}
