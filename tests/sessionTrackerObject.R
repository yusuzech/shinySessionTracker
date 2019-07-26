context("Test sessionTracker object")
# library(shinySessionTracker)

test_that("object initialize correctly",{
    expect_error(sessionTracker$new(),NA)
    # only character as input
    expect_error(sessionTracker$new(sessionID = 1))
    expect_error(sessionTracker$new(sessionID = "1"),NA)
    expect_error(sessionTracker$new(sessionID = TRUE))
    
})

test_that("file add methods working as intended",{
    mytracker <- sessionTracker$new()
    mytracker$addEventAuto("a")
    mytracker$addEventManual("b",type = "button",info = "test")
    eventTable <- mytracker$getSessionInfo()[[1]]$detail
    expect_match(eventTable[[1]][[2]],"auto",fixed = TRUE)
    expect_match(eventTable[[2]][[2]],"manual",fixed = TRUE)
    expect_match(eventTable[[2]][[3]],"button",fixed = TRUE)
    expect_match(eventTable[[2]][[4]],"test",fixed = TRUE)
    expect_equal(length(eventTable),2)
})


test_that("file name and path validation all working",{
    mytracker <- sessionTracker$new()
    # file don't save with inccorect extension
    expect_error(mytracker$saveSessionInfo(fname = "fdf.abc"))
    expect_error(mytracker$saveSessionInfo(fname = "fdf"))
    # file saved with correct file name
    expect_error(mytracker$saveSessionInfo(saveParams = list(folder_path = tempdir())),NA)
    expect_error(mytracker$saveSessionInfo(fname = "test.rds",
                                           saveParams = list(folder_path = tempdir())),NA)
})

