% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_prepare_metadata.R
\name{construct_label}
\alias{construct_label}
\title{Construct variable label}
\usage{
construct_label(
  qnr_df,
  question_text_var = "question_text",
  variable_label_var = "variable_label"
)
}
\arguments{
\item{qnr_df}{Data frame. Questionnaire metadata.}

\item{question_text_var}{Character. Name of question text variable in `qnr_df`.}

\item{variable_label_var}{Character. Name of variable label in `qnr_df`.}
}
\value{
Data frame. Questionnaire metadata with `label` column.
}
\description{
If variable label present in Designer, use it. Otherwise, use question text.
}
