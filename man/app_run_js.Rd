% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\name{app_run_js}
\alias{app_run_js}
\title{Helper to execute js code in shinytest/shinytest2 browser}
\usage{
app_run_js(app, js, interval, timeout)
}
\arguments{
\item{app}{shinytest/shinytest2 application object (ShinyDriver/AppDriver)}

\item{js}{string, javascript code}

\item{interval}{see: \link[shinytest]{ShinyDriver} (method waitFor(), param checkInterval)
or \link[shinytest2]{AppDriver} (method wait_for_js(), param interval)}

\item{timeout}{see: \link[shinytest]{ShinyDriver} (method waitFor(), param timeout)
or \link[shinytest2]{AppDriver} (method wait_for_js(), param timeout)}
}
\value{
ToDo
}
\description{
Helper to execute js code in shinytest/shinytest2 browser
}
