library(shiny)
library(wdman)
library(RSelenium)
library(testthat)
library(XML)

context("output")

seleniumServer <- wdman::selenium(verbose=FALSE,
                                  port = 4444L)

driver <- remoteDriver(
  remoteServerAddr = 'localhost',
  port = 4444L,
  browserName = 'chrome'
)
driver$open(silent = TRUE)

appURL <- "http://127.0.0.1:3109"

test_that("output plot", {
  driver$navigate(appURL)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Plots")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Plot")]]$clickElement()
  
  control <- driver$findElement(using = "id", "plot")
  
  expect_true(unlist(control$isElementDisplayed()))
  expect_true(unlist(control$isElementEnabled()))
})

test_that("output histogram", {
  driver$navigate(appURL)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Plots")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Histogram")]]$clickElement()
  
  control <- driver$findElement(using = "id", "histogram")
  
  expect_true(unlist(control$isElementDisplayed()))
  expect_true(unlist(control$isElementEnabled()))
})

test_that("output data table", {
  driver$navigate(appURL)
  
  dt <- driver$findElements("xpath", "//th[@class='sorting']")
  dt_headers <- sapply(dt, function(x){x$getElementText()})
  
  # get random column
  headersRandom <- sample(seq_along(dt_headers[!dt_headers==""]), 1)
  
  # order of random column after 1st click
  dt[[headersRandom]]$clickElement()
  appSource <- driver$getPageSource()[[1]]
  appSource <- htmlParse(appSource)
  dttable <- readHTMLTable(appSource, stringsAsFactors = FALSE)
  appCol <- dttable$DataTables_Table_0[[headersRandom+1]]
  order1 <- is.unsorted(appCol)
  
  # order of random column after 2nd click
  dt[[headersRandom]]$clickElement()
  appSource <- driver$getPageSource()[[1]]
  appSource <- htmlParse(appSource)
  dttable <- readHTMLTable(appSource, stringsAsFactors = FALSE)
  appCol <- dttable$DataTables_Table_0[[headersRandom+1]]
  order2 <- is.unsorted(appCol)
  
  expect_false(order1 == order2)
})

test_that("output verbatim text", {
  driver$navigate(appURL)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Data")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Summary")]]$clickElement()
  
  control <- driver$findElement(using = "id", "summary")
  
  expect_true(unlist(control$isElementEnabled()))
})

driver$close()