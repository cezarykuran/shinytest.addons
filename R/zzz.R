#' Helper to execute js code in shinytest/shinytest2 browser
#'
#' @param app shinytest/shinytest2 application object (ShinyDriver/AppDriver)
#' @param js string, javascript code
#' @param interval
#' see: \link[shinytest]{ShinyDriver} (method waitFor(), param checkInterval)
#' or \link[shinytest2]{AppDriver} (method wait_for_js(), param interval)
#' @param timeout
#' see: \link[shinytest]{ShinyDriver} (method waitFor(), param timeout)
#' or \link[shinytest2]{AppDriver} (method wait_for_js(), param timeout)
#'
#' @return ToDo
app_run_js <- function(app, js, interval, timeout) {
  if ("ShinyDriver" %in% class(app)) {
    app$waitFor(expr = js, checkInterval = interval, timeout = timeout)
  }
  else if ("AppDriver" %in% class(app)) {
    app$wait_for_js(script = js, timeout = timeout, interval = interval)
  }
  else stop("Unknown class of `app`")
}
