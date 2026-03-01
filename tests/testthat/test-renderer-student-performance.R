acontext("student performance dashboard")

## suppress disk writes from dashboard source file
animint2dir <- function(...) invisible(NULL)
fwrite <- function(...) invisible(NULL)
source("student-dashboard-data.R")

info <- animint2HTML(complete_dashboard)

test_that("mainTrends has 8 tallrect elements one per semester", {
  nodes <- getNodeSet(
    info$html,
    '//g[@class="geom1_tallrect_mainTrends"]//rect'
  )
  expect_equal(length(nodes), 8)
})

## 2. mainTrends has line geom elements for student grade progression
test_that("mainTrends contains line geom elements", {
  nodes <- getNodeSet(
    info$html,
    '//g[@class="geom2_line_mainTrends"]//path'
  )
  expect_true(length(nodes) > 0,
    info = paste("found", length(nodes), "line path elements"))
})

## 3. mainTrends has point geom elements
test_that("mainTrends contains point geom elements", {
  nodes <- getNodeSet(
    info$html,
    '//g[@class="geom3_point_mainTrends"]//circle'
  )
  expect_true(length(nodes) > 0,
    info = paste("found", length(nodes), "point circle elements"))
})

## 4. studyAnalysis has background point elements (all semesters)
test_that("studyAnalysis contains background circle elements", {
  nodes <- getNodeSet(
    info$html,
    '//g[@class="geom4_point_studyAnalysis"]//circle'
  )
  expect_true(length(nodes) > 0,
    info = paste("found", length(nodes), "background circles"))
})

## 5. studyAnalysis has showSelected point elements for Sem1
test_that("studyAnalysis shows circles for selected semester", {
  nodes <- getNodeSet(
    info$html,
    '//g[@class="geom5_point_studyAnalysis"]//circle'
  )
  expect_true(length(nodes) > 0,
    info = paste("found", length(nodes), "semester-filtered circles"))
})

## 6. subjectComparison has bar elements for 6 subjects
test_that("subjectComparison contains bar rect elements", {
  nodes <- getNodeSet(
    info$html,
    '//g[@class="geom7_bar_subjectComparison"]//rect'
  )
  expect_true(length(nodes) > 0,
    info = paste("found", length(nodes), "bar rect elements"))
})

## 7. topPerformers has bar elements for top 15 students
test_that("topPerformers contains bar rect elements", {
  nodes <- getNodeSet(
    info$html,
    '//g[@class="geom9_bar_topPerformers"]//rect'
  )
  expect_true(length(nodes) > 0,
    info = paste("found", length(nodes), "topPerformers bar elements"))
})

## 8. semester selector widget is present in the page
test_that("semester selector is present in the page", {
  inputs <- getNodeSet(info$html, "//select|//input[@type='radio']")
  expect_true(length(inputs) > 0,
    info = "expected semester selector widget in HTML")
})
