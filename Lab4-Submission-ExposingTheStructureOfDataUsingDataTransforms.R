#Load dataset
library(readr)
student_performance_dataset <-
  readr::read_csv(
    "data/StudentPerformanceDataset.csv", # nolint
    col_types =
      readr::cols(
        class_group =
          readr::col_factor(levels = c("A", "B", "C")),
        gender = readr::col_factor(levels = c("1", "0")),
        YOB = readr::col_date(format = "%Y"),
        regret_choosing_bi =
          readr::col_factor(levels = c("1", "0")),
        drop_bi_now =
          readr::col_factor(levels = c("1", "0")),
        motivator =
          readr::col_factor(levels = c("1", "0")),
        read_content_before_lecture =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        anticipate_test_questions =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        answer_rhetorical_questions =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        find_terms_I_do_not_know =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        copy_new_terms_in_reading_notebook =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        take_quizzes_and_use_results =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        reorganise_course_outline =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        write_down_important_points =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        space_out_revision =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        studying_in_study_group =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        schedule_appointments =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        goal_oriented =
          readr::col_factor(levels =
                              c("1", "0")),
        spaced_repetition =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        testing_and_active_recall =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        interleaving =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        categorizing =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        retrospective_timetable =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        cornell_notes =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        sq3r = readr::col_factor(levels =
                                   c("1", "2", "3", "4")),
        commute = readr::col_factor(levels =
                                      c("1", "2",
                                        "3", "4")),
        study_time = readr::col_factor(levels =
                                         c("1", "2",
                                           "3", "4")),
        repeats_since_Y1 = readr::col_integer(),
        paid_tuition = readr::col_factor(levels =
                                           c("0", "1")),
        free_tuition = readr::col_factor(levels =
                                           c("0", "1")),
        extra_curricular = readr::col_factor(levels =
                                               c("0", "1")),
        sports_extra_curricular =
          readr::col_factor(levels = c("0", "1")),
        exercise_per_week = readr::col_factor(levels =
                                                c("0", "1",
                                                  "2",
                                                  "3")),
        meditate = readr::col_factor(levels =
                                       c("0", "1",
                                         "2", "3")),
        pray = readr::col_factor(levels =
                                   c("0", "1",
                                     "2", "3")),
        internet = readr::col_factor(levels =
                                       c("0", "1")),
        laptop = readr::col_factor(levels = c("0", "1")),
        family_relationships =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        friendships = readr::col_factor(levels =
                                          c("1", "2", "3",
                                            "4", "5")),
        romantic_relationships =
          readr::col_factor(levels =
                              c("0", "1", "2", "3", "4")),
        spiritual_wellnes =
          readr::col_factor(levels = c("1", "2", "3",
                                       "4", "5")),
        financial_wellness =
          readr::col_factor(levels = c("1", "2", "3",
                                       "4", "5")),
        health = readr::col_factor(levels = c("1", "2",
                                              "3", "4",
                                              "5")),
        day_out = readr::col_factor(levels = c("0", "1",
                                               "2", "3")),
        night_out = readr::col_factor(levels = c("0",
                                                 "1", "2",
                                                 "3")),
        alcohol_or_narcotics =
          readr::col_factor(levels = c("0", "1", "2", "3")),
        mentor = readr::col_factor(levels = c("0", "1")),
        mentor_meetings = readr::col_factor(levels =
                                              c("0", "1",
                                                "2", "3")),
        `Attendance Waiver Granted: 1 = Yes, 0 = No` =
          readr::col_factor(levels = c("0", "1")),
        GRADE = readr::col_factor(levels =
                                    c("A", "B", "C", "D",
                                      "E"))),
    locale = readr::locale())

# Determine which columns are numeric
data_types <- sapply(student_performance_dataset, class)
column_numbers <- 1:ncol(student_performance_dataset)

# Create a data frame to display the results
column_info <- data.frame(ColumnNumber = column_numbers, DataType = data_types)

# Print the column information
print(column_info)

# Scale Data Transform ----
### The Scale Basic Transform on the Boston Housing Dataset ----
# BEFORE
summary(student_performance_dataset)
hist(student_performance_dataset[, 81], main = names(student_performance_dataset)[81])
hist(student_performance_dataset[, 2], main = names(student_performance_dataset)[2])
hist(student_performance_dataset[, 3], main = names(student_performance_dataset)[3])
hist(student_performance_dataset[, 5], main = names(student_performance_dataset)[5])
hist(student_performance_dataset[, 6], main = names(student_performance_dataset)[6])
hist(student_performance_dataset[, 7], main = names(student_performance_dataset)[7])
hist(student_performance_dataset[, 8], main = names(student_performance_dataset)[8])
hist(student_performance_dataset[, 9], main = names(student_performance_dataset)[9])
hist(student_performance_dataset[, 10], main = names(student_performance_dataset)[10])
hist(student_performance_dataset[, 11], main = names(student_performance_dataset)[11])
hist(student_performance_dataset[, 12], main = names(student_performance_dataset)[12])
hist(student_performance_dataset[, 13], main = names(student_performance_dataset)[13])
hist(student_performance_dataset[, 14], main = names(student_performance_dataset)[14])

model_of_the_transform <- preProcess(student_performance_dataset, method = c("scale"))
print(model_of_the_transform)
student_perfomance_dataset_scale_transform <- predict(model_of_the_transform,
                                          student_performance_dataset)
# AFTER
summary(boston_housing_scale_transform)
hist(boston_housing_scale_transform[, 1],
     main = names(boston_housing_scale_transform)[1])
hist(boston_housing_scale_transform[, 2],
     main = names(boston_housing_scale_transform)[2])
hist(boston_housing_scale_transform[, 3],
     main = names(boston_housing_scale_transform)[3])
hist(boston_housing_scale_transform[, 5],
     main = names(boston_housing_scale_transform)[5])
hist(boston_housing_scale_transform[, 6],
     main = names(boston_housing_scale_transform)[6])
hist(boston_housing_scale_transform[, 7],
     main = names(boston_housing_scale_transform)[7])
hist(boston_housing_scale_transform[, 8],
     main = names(boston_housing_scale_transform)[8])
hist(boston_housing_scale_transform[, 9],
     main = names(boston_housing_scale_transform)[9])
hist(boston_housing_scale_transform[, 10],
     main = names(boston_housing_scale_transform)[10])
hist(boston_housing_scale_transform[, 11],
     main = names(boston_housing_scale_transform)[11])
hist(boston_housing_scale_transform[, 12],
     main = names(boston_housing_scale_transform)[12])
hist(boston_housing_scale_transform[, 13],
     main = names(boston_housing_scale_transform)[13])
hist(boston_housing_scale_transform[, 14],
     main = names(boston_housing_scale_transform)[14])