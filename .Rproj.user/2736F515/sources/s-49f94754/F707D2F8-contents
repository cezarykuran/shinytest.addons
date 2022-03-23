#' Test plotly object
#'
#' @inherit app_run_js
#' @param id string, id of the plotly object
#' @param data list/data.frame, reference data
#' @export
testPlotly <- function(
    app,
    id,
    data,
    interval = 1000,
    timeout = 30000
  ) {

  stopifnot(
    inherits(app, "ShinyDriver", "AppDriver"),
    checkmate::test_string(id, min.chars = 1),
    is.list(data)
  )

  js <- paste0(
    "(function(){",
      "return '", toString(jsonlite::toJSON(data, auto_unbox = TRUE)), "'",
      "==",
      "JSON.stringify(",
        "document.getElementById('", id, "')",
          ".data",
          #".map(function(p){return {x:p.x,y:p.y,marker:p.marker}})",
          ".map(function(p){return {x:p.x,y:p.y}})",
      ")",
    "})()"
  )
  app_run_js(app, js, interval, timeout)
}

#' Test shiny::DT object
#'
#' @inherit app_run_js
#' @param id string, id of the DT object
#' @param row numeric vector, row number(s)
#' @param col numeric vector, columns number(s)
#' @param data matrix, reference data
#' @param regex flag, treat content of the reference data as regular expression
#' @export
testDT <- function(
    app,
    id,
    row,
    col,
    data,
    regex = FALSE,
    interval = 1000,
    timeout = 30000
  ) {

  stopifnot(
    inherits(app, "ShinyDriver", "AppDriver"),
    checkmate::test_string(id, min.chars = 1),
    checkmate::test_numeric(row, lower = 0, min.len = 1),
    checkmate::test_numeric(col, lower = 0, min.len = 1),
    is.matrix(data),
    length(row) == NROW(data),
    length(col) == NCOL(data),
    checkmate::test_flag(regex)
  )

  js <- paste0(
    "(function(){",
      "ref=", toString(jsonlite::toJSON(data, auto_unbox = TRUE)), ";",
      "ret=1;",
      "$('#", id, " table > tbody > tr')",
      ".filter(function(i) {return $.inArray(i,[", paste0(row, collapse = ","),"])!=-1})",
      ".each(function(r){",
        "$(this).children()",
            ".filter(function(i) {return $.inArray(i,[", paste0(col, collapse = ","),"])!=-1})",
            ".each(function(c){",
                "ret &= $(this).text()", ifelse(regex, ".match(ref[c][r]) !== null", "==ref[c][r]"), ";",
            "});",
      "});",
      "return ret",
    "})()"
  )
  #message(js)
  app_run_js(app, js, interval, timeout)
}
