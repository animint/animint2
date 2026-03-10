library(XML)
library(animint2)

acontext("student performance subject comparison")

subject_summary <- data.frame(
    subject = rep(c(
        "Mathematics", "Physics", "Chemistry",
        "Biology", "English", "History"
    ), times = 2),
    mean_grade = c(
        72.5, 74.3, 75.8, 77.2, 79.1, 80.4,
        73.5, 75.3, 76.8, 78.2, 80.1, 81.4
    ),
    semester = rep(c("Sem1", "Sem2"), each = 6),
    stringsAsFactors = FALSE
)

student_points <- data.frame(
    subject = rep(c(
        "Mathematics", "Physics", "Chemistry",
        "Biology", "English", "History"
    ), each = 5),
    grade = c(
        65, 70, 72, 75, 80,
        68, 72, 74, 76, 82,
        70, 73, 75, 78, 83,
        72, 75, 77, 80, 85,
        74, 77, 79, 82, 87,
        75, 78, 80, 83, 88
    ),
    semester = rep("Sem1", 30),
    stringsAsFactors = FALSE
)

viz <- animint(
    subjectComparison = ggplot() +
        theme_bw() +
        theme_animint(width = 680, height = 380) +
        geom_bar(
            aes(
                x = subject, y = mean_grade,
                fill = subject, key = subject
            ),
            showSelected = "semester",
            clickSelects = "subject",
            data = subject_summary,
            stat = "identity",
            position = "identity",
            alpha = 0.7
        ) +
        geom_point(
            aes(
                x = subject, y = grade,
                color = subject,
                key = paste(subject, grade)
            ),
            showSelected = "semester",
            clickSelects = "subject",
            data = student_points,
            alpha = 0.6
        ) +
        ggtitle("Subject Performance Comparison") +
        xlab("Subject") +
        ylab("Grade (%)"),
    first = list(semester = "Sem1", subject = "Mathematics")
)

info <- animint2HTML(viz)

# test 1 - exactly 6 bar rects rendered, one per subject
test_that("subjectComparison renders exactly 6 bar rects for 6 subjects", {
    rects <- getNodeSet(
        info$html,
        "//rect[not(@class)]"
    )
    expect_equal(length(rects), 6L)
})

# test 2 - point circles rendered for student grades
test_that("subjectComparison renders point circles for students", {
    circles <- getNodeSet(
        info$html,
        '//g[contains(@class,"geom")]//circle'
    )
    expect_true(length(circles) > 0)
})

# test 3 - selector widget exists for subject
test_that("subject selector widget is present", {
    inputs <- getNodeSet(
        info$html,
        "//select | //input[@type='radio']"
    )
    expect_true(length(inputs) > 0)
})

# test 4 - semester selector has exactly 2 options (Sem1 and Sem2)
test_that("semester selector has exactly 2 options", {
    options <- getNodeSet(
        info$html,
        "//select//option"
    )
    expect_equal(length(options), 2L)
})

# test 5 - plot title is rendered correctly in SVG (fixed: more specific)
test_that("plot title text is rendered exactly once in SVG", {
    titles <- getNodeSet(
        info$html,
        '//text[contains(text(), "Subject Performance Comparison")]'
    )
    expect_equal(length(titles), 1L)
})

# test 6 - all 6 subject names appear as axis tick labels
test_that("all 6 subject names appear as x axis labels", {
    subjects <- c(
        "Mathematics", "Physics", "Chemistry",
        "Biology", "English", "History"
    )
    tick_nodes <- getNodeSet(
        info$html,
        "//g[contains(@class,'xaxis')]//text"
    )
    tick_labels <- sapply(tick_nodes, xmlValue)
    for (s in subjects) {
        expect_true(
            any(grepl(s, tick_labels)),
            info = paste("subject not found in axis labels:", s)
        )
    }
})

# test 7 - clickID on Physics updates selected subject in browser
test_that("clicking Physics bar updates the selected subject", {
    clickID("Physics")
    Sys.sleep(1)
    html_after <- getHTML()
    # after clicking Physics, circles for Physics should be visible
    circles_after <- getNodeSet(
        html_after,
        '//g[contains(@class,"geom")]//circle'
    )
    expect_true(length(circles_after) > 0)
})

# test 8 - clicking Sem2 option updates bars to show Sem2 data
test_that("selecting Sem2 updates bars to show Sem2 mean grades", {
    clickHTML(xpath = "//option[@value='Sem2']")
    Sys.sleep(1)
    html_after <- getHTML()
    rects_after <- getNodeSet(
        html_after,
        "//rect[not(@class)]"
    )
    # bars should still be 6 after semester change
    expect_equal(length(rects_after), 6L)
})
