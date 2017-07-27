library(shiny)
library(wdman)
library(RSelenium)
library(testthat)
library(zoo)

context("controls")

seleniumServer <- wdman::selenium(verbose=FALSE,
                                  port = 4444L)

driver <- remoteDriver(
  remoteServerAddr = 'localhost',
  port = 4444L,
  browserName = 'chrome'
)
driver$open(silent = TRUE)

appURL <- "http://127.0.0.1:3109"




##### Checkbox tests #####
test_that("checkbox presence", {
  driver$navigate(appURL)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Plots")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Plot")]]$clickElement()
  
  control <- driver$findElement(using = "id", "comparison")

  expect_true(unlist(control$isElementDisplayed()))
  expect_true(unlist(control$isElementEnabled()))
})

test_that("checkbox selection test", {
  driver$navigate(appURL)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Plots")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Plot")]]$clickElement()
  
  control <- driver$findElement(using = "id", "comparison")

  expect_false(unlist(control$isElementSelected()))  
  control$clickElement() 
  expect_true(unlist(control$isElementSelected()))
  control$clickElement()
  expect_false(unlist(control$isElementSelected()))
})




##### Select tests #####
test_that("select presence", {
  driver$navigate(appURL)
  driver$setImplicitWaitTimeout(10000)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Data")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Summary")]]$clickElement()
  
  control <- driver$findElement(using = "xpath","//select[@id='selectInput_1']")

  expect_true(unlist(control$isElementEnabled()))
})

test_that("select input values", {  
  driver$navigate(appURL)
  driver$setImplicitWaitTimeout(2000)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Data")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Summary")]]$clickElement()
  
  control <- driver$findElement(using = "xpath","//select[@id='selectInput_1']")
  
  choices <- control$selectTag()
  
  expect_true(all(c("All",unique(format(as.yearmon(time(co2)),"%Y"))) %in% choices$text))
  expect_equal(which(choices$selected),1)
})

test_that("select change request", {
  driver$navigate(appURL)
  driver$setImplicitWaitTimeout(2000)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Data")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Summary")]]$clickElement()
  
  control <- driver$findElement(using = "xpath","//select[@id='selectInput_1']")
  
  choices <- control$selectTag()
  init_value <- choices$text[which(choices$selected)]
  
  change_idx <- sample(seq_along(choices$text)[-which(choices$selected)], 1)
  choices$elements[[change_idx]]$clickElement()
  choices <- control$selectTag()
  changed_value <- choices$text[which(choices$selected)]
  
  expect_false(init_value == changed_value)
})

##### Slider tests #####
test_that("slider presence", {
  driver$navigate(appURL)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Plots")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Histogram")]]$clickElement()
  
  control <- driver$findElement(using = "id", "dateRange")
  
  expect_true(unlist(control$isElementDisplayed()))
  expect_true(unlist(control$isElementEnabled()))
})

test_that("slider change request", {
  driver$navigate(appURL)
  
  navMenu <- driver$findElements(using = "xpath", "//a[@class = 'dropdown-toggle']")
  menuText <- unlist(lapply(navMenu, function(elem){elem$getElementText()}))
  navMenu[[which(menuText == "Plots")]]$clickElement()
  
  navElem <- driver$findElements(using = "xpath", "//a[@data-toggle = 'tab']")
  elemText <- unlist(lapply(navElem, function(elem){elem$getElementText()}))
  navElem[[which(elemText == "Histogram")]]$clickElement()
  
  control <- driver$findElement(using = "id", "dateRange")
  
  # slider init values
  sliderMin <- as.numeric(control$getElementAttribute("data-from")[[1]])
  sliderMax <- as.numeric(control$getElementAttribute("data-to")[[1]])
  sliderValue <- as.numeric(strsplit(control$getElementAttribute("value")[[1]],";")[[1]])
  sliderStep <- as.numeric(control$getElementAttribute("data-step"))

  # slider bar and pointers
  slider <- driver$findElement("class", "irs-bar")
  fromButton <- driver$findElement("xpath", "//span[@class='irs-slider from']")
  toButton <- driver$findElement("xpath", "//span[@class='irs-slider to']")
  
  sliderDim <- slider$getElementSize()
  
  newValues <- seq(from = sliderMin, to = sliderMax, by = sliderStep)
  newValues <- sort(sample(newValues, 2))
  
  pxToMoveSldr <- round(sliderDim$width * (newValues - sliderValue)/(sliderMax - sliderMin))
  
  for(x in pxToMoveSldr){
    if(x == min(pxToMoveSldr)){
      driver$mouseMoveToLocation(webElement = toButton)
      driver$buttondown()
      driver$mouseMoveToLocation(x = as.integer(x), y = -1L)
      driver$buttonup()
    } else {
      driver$mouseMoveToLocation(webElement = fromButton)
      driver$buttondown()
      driver$mouseMoveToLocation(x = as.integer(x), y = -1L)
      driver$buttonup()
    }
  }

  # slider output values
  Sys.sleep(2)
  control <- driver$findElement(using = "id", "dateRange")

  sliderValueOutput <- as.numeric(strsplit(control$getElementAttribute("value")[[1]],";")[[1]])

  expect_false(all(sliderValue == sliderValueOutput))
})

driver$close()