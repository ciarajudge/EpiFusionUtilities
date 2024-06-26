% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_epifusion_data.R
\name{prepare_epifusion_data}
\alias{prepare_epifusion_data}
\title{Prepare a tree (in S3 Phylo Object form) and case incidence (in data.frame form) for EpiFusion}
\usage{
prepare_epifusion_data(tree, case_incidence, start_date)
}
\arguments{
\item{tree}{phylogenetic tree (in S3 Phylo Object format) with branch lengths in terms of days}

\item{case_incidence}{a data frame with case incidence and their date of occurrence. The data frame should consist of two columns: 'Date', a \if{html}{\out{<Date>}} column with the observation dates, and 'Cases', a numeric column with reported cases on the corresponding day.}

\item{start_date}{the date, in \if{html}{\out{<date>}} format, that you want to start modelling the outbreak from. Should be some time before}
}
\description{
This function prepares a tree, given as an S3 phylo object, for EpiFusion analysis by adding 'time during outbreak' to the node and leaf labels. It also assigns 'time during outbreak' to the case incidence data making it easier to form the \if{html}{\out{<incidence>}} node in EpiFusion XML.
}
