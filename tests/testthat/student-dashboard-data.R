# student performance dashboard using animint2
# generates synthetic data and builds 5 linked interactive plots

library(animint2)
library(data.table)

set.seed(2024)

# basic setup
n_students <- 50
students <- paste0("S", sprintf("%03d", 1:n_students))
subjects <- c("Mathematics", "Physics", "Chemistry", "Biology", "English", "History")
semesters <- paste0("Sem", 1:8)
years <- c(rep("Year 1", 2), rep("Year 2", 2), rep("Year 3", 2), rep("Year 4", 2))

# one row per student per subject per semester
student_data <- data.table(expand.grid(
  student_id = students,
  subject = subjects,
  semester = semesters,
  stringsAsFactors = FALSE
))

student_data[, year := rep(years, each = 1)[as.numeric(gsub("Sem", "", semester))]]
student_data[, sem_num := as.numeric(gsub("Sem", "", semester))]

# grades go up slightly each semester, adjusted by student ability and subject difficulty
student_data[, `:=`(
  base_grade = 60 + sem_num * 2,
  student_ability = rnorm(n_students, mean = 0, sd = 10)[match(student_id, students)],
  subject_difficulty = c(Mathematics = -5, Physics = -3, Chemistry = -2, 
                        Biology = 0, English = 2, History = 3)[subject]
)]

student_data[, grade := base_grade + student_ability + subject_difficulty + rnorm(.N, 0, 5)]
student_data[grade > 100, grade := 100]
student_data[grade < 30, grade := 30 + runif(.N, 0, 10)]
student_data[, grade := round(grade, 1)]

# other metrics, loosely correlated with grade
student_data[, `:=`(
  study_hours = round(10 + (grade - 60) / 3 + rnorm(.N, 0, 3)),
  attendance = round(70 + (grade - 60) / 2 + rnorm(.N, 0, 5)),
  assignments_completed = round(runif(.N, 5, 20)),
  quiz_score = round(grade + rnorm(.N, 0, 8))
)]

# clamp everything to valid ranges
student_data[study_hours < 2, study_hours := 2]
student_data[study_hours > 40, study_hours := 40]
student_data[attendance < 60, attendance := 60]
student_data[attendance > 100, attendance := 100]
student_data[quiz_score > 100, quiz_score := 100]
student_data[quiz_score < 20, quiz_score := 20]

student_data[, performance_category := cut(
  grade,
  breaks = c(0, 60, 75, 85, 100),
  labels = c("Needs Improvement", "Satisfactory", "Good", "Excellent"),
  include.lowest = TRUE
)]

student_data[, gender := sample(c("Male", "Female"), .N, replace = TRUE)]

semesters_data <- unique(student_data[, .(semester, sem_num)])
subjects_data <- unique(student_data[, .(subject)])

# plot 1 - grade trends over all semesters, click a student to highlight their lines
grade_trends <- ggplot() +
  theme_bw() +
  theme(
    legend.position = "right",
    text = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  theme_animint(
    width = 1400,
    height = 420,
    colspan = 2,
    last_in_row = TRUE
  ) +
  geom_tallrect(
    aes(xmin = sem_num - 0.5, xmax = sem_num + 0.5),
    clickSelects = "semester",
    data = semesters_data,
    alpha = 0.2
  ) +
  geom_line(
    aes(x = sem_num, y = grade,
        group = interaction(student_id, subject),
        color = subject),
    clickSelects = "student_id",
    data = student_data,
    size = 0.8,
    alpha = 0.1
  ) +
  geom_point(
    aes(
      x = sem_num, y = grade,
      color = subject, size = study_hours,
      key = paste(student_id, subject, semester),
      tooltip = paste0(
        "Student: ", student_id, "\n",
        "Subject: ", subject, "\n",
        "Semester: ", semester, "\n",
        "Grade: ", grade, "%\n",
        "Study Hours: ", study_hours, "\n",
        "Attendance: ", attendance, "%"
      )
    ),
    showSelected = "student_id",
    clickSelects = "subject",
    data = student_data,
    alpha = 0.7
  ) +
  scale_size_continuous(range = c(3, 10), name = "Study Hours/Week") +
  scale_x_continuous(breaks = 1:8, labels = paste0("Sem", 1:8)) +
  ggtitle("Student Grade Progression Over Semesters") +
  xlab("Semester") +
  ylab("Grade (%)")
# plot 2 - how study hours relate to grades, point size is attendance
study_scatter <- ggplot() +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  theme_animint(width = 680, height = 380) +
  geom_point(
    aes(x = study_hours, y = grade, color = subject),
    data = student_data,
    alpha = 0.08,
    size = 1
  ) +
  geom_point(
    aes(
      x = study_hours, y = grade,
      color = subject, size = attendance,
      key = paste(student_id, subject, semester),
      tooltip = paste0(
        "Student: ", student_id, "\n",
        "Subject: ", subject, "\n",
        "Grade: ", grade, "%\n",
        "Study Hours: ", study_hours, "\n",
        "Attendance: ", attendance, "%"
      )
    ),
    showSelected = "semester",
    clickSelects = c("student_id", "subject"),
    data = student_data,
    alpha = 0.6
  ) +
  geom_smooth(
    aes(x = study_hours, y = grade, color = subject),
    showSelected = "semester",
    data = student_data,
    method = "lm",
    se = FALSE
  ) +
  ggtitle("Study Hours vs Grade") +
  xlab("Weekly Study Hours") +
  ylab("Grade (%)")

# aggregate stats per subject per semester for the bar chart
subject_summary <- student_data[, .(
  median_grade = median(grade),
  mean_grade = mean(grade),
  min_grade = min(grade),
  max_grade = max(grade),
  count = .N
), by = .(semester, subject, sem_num)]

# plot 3 - subject-wise comparison, bars are mean with jittered individual points on top
subject_comparison <- ggplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none"
  ) +
  theme_animint(
    width = 680,
    height = 380,
    last_in_row = TRUE
  ) +
  geom_bar(
    aes(x = subject, y = mean_grade, fill = subject, key = subject),
    showSelected = "semester",
    clickSelects = "subject",
    data = subject_summary,
    stat = "identity",
    alpha = 0.7
  ) +
  geom_point(
    aes(x = subject, y = grade, color = subject),
    showSelected = "semester",
    clickSelects = "subject",
    data = student_data,
    alpha = 0.25,
    position = position_jitter(width = 0.2)
  ) +
  ggtitle("Subject Performance Comparison") +
  xlab("Subject") +
  ylab("Grade (%)")

# per-student averages across all subjects
student_avg <- student_data[, .(
  avg_grade = mean(grade),
  total_study_hours = sum(study_hours),
  avg_attendance = mean(attendance)
), by = .(student_id, semester, sem_num)]

# grab top 15 each semester
top_students <- student_avg[, .SD[order(-avg_grade)][1:15], by = semester]

# plot 4 - horizontal bar chart of top students, colored by avg grade
ranking_plot <- ggplot() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  theme_animint(width = 680, height = 380) +
  geom_bar(
    aes(
      x = reorder(student_id, avg_grade),
      y = avg_grade,
      fill = avg_grade,
      key = student_id
    ),
    showSelected = "semester",
    clickSelects = "student_id",
    data = top_students,
    stat = "identity"
  ) +
  coord_flip() +
  ggtitle("Top 15 Students") +
  xlab("Student ID") +
  ylab("Average Grade (%)")

# bin attendance into ranges and get mean grade per bin
attendance_bins <- student_data[, .(
  avg_grade = mean(grade),
  count = .N
), by = .(attendance_bin = cut(attendance, breaks = seq(60, 100, by = 10)), 
         semester, subject)]

# plot 5 - does higher attendance actually help, this shows it pretty clearly
attendance_plot <- ggplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  theme_animint(
    width = 680,
    height = 380,
    last_in_row = TRUE
  ) +
  geom_bar(
    aes(
      x = attendance_bin,
      y = avg_grade,
      fill = subject,
      key = paste(attendance_bin, subject)
    ),
    showSelected = "semester",
    clickSelects = "subject",
    data = attendance_bins[!is.na(attendance_bin)],
    stat = "identity",
    position = "dodge"
  ) +
  ggtitle("Attendance Impact on Grades") +
  xlab("Attendance Range (%)") +
  ylab("Average Grade (%)")

# wire everything together into one dashboard
complete_dashboard <- animint(
  title = "Student Performance Analytics Dashboard",
  source = "https://github.com/AviraL0013/student-performance-code",
  
  mainTrends = grade_trends,
  studyAnalysis = study_scatter,
  subjectComparison = subject_comparison,
  topPerformers = ranking_plot,
  attendanceImpact = attendance_plot,
  
  time = list(variable = "semester", ms = 3000),
  duration = list(semester = 1000, student_id = 500),
  
  first = list(
    semester = "Sem1",
    student_id = "S001",
    subject = "Mathematics"
  ),
  
  selector.types = list(
    student_id = "single",
    subject = "multiple",
    semester = "single"
  )
)

# render and save
output_dir <- "Student-Performance-Dashboard-Clean"
if(dir.exists(output_dir)) {
  unlink(output_dir, recursive = TRUE)
}

animint2dir(complete_dashboard, output_dir)
fwrite(student_data, file.path(output_dir, "student_data.csv"))

cat("Done. Open with: servr::httd('", output_dir, "')\n", sep = "")