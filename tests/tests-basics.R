library(shiny)
library(wdman)
library(RSelenium)
library(testthat)

context("basic")

seleniumServer <- wdman::selenium(verbose=FALSE,
                                  port = 4444L)

driver <- remoteDriver(
  remoteServerAddr = 'localhost',
  port = 4444L,
  browserName = 'chrome'
)
driver$open(silent = TRUE)

appURL <- "http://127.0.0.1:3109"

##### Connection test
test_that("app connection", {  
  driver$navigate(appURL)
  appTitle <- driver$getTitle()[[1]]
  expect_equal(appTitle, "Mauna Loa Atmospheric CO2 Concentration")  
})

##### Tabs test
test_that("tabs presence", {
  driver$navigate(appURL)
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  mainTabs <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))

  expect_equal(mainTabs[[1]], "Data")  
  expect_equal(mainTabs[[2]], "Plots")
})

test_that("secondary data tabs presence", {
  driver$navigate(appURL)
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  navMenu[[1]]$clickElement()

  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  secTabs <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  
  expect_equal(secTabs[[1]], "Data table")
  expect_equal(secTabs[[2]], "Summary")
})

test_that("secondary plot tabs presence", {
  driver$navigate(appURL)
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  navMenu[[2]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  secTabs <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  
  expect_equal(secTabs[[1]], "Plot")
  expect_equal(secTabs[[2]], "Histogram")
})

driver$close()