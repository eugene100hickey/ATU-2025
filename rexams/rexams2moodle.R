library(exams)
# elearn_exam <- c("rexams/penguins.Rmd",
#                  "rexams/diamonds.Rmd",
#                  "rexams/diamonds.Rmd",
#                  "rexams/penguins-class.Rmd")

elearn_exam <- c(
  "rexams/week-01/diamonds.Rmd",
  # "rexams/week-1/gettysburg.Rmd",
  # "rexams/week-1/jane-austen.Rmd",
  # "rexams/week-1/package-author.Rmd",
  # "rexams/week-1/penguins-class.Rmd",
  # "rexams/week-1/penguins-complete.Rmd",
  # "rexams/week-1/penguins-na.Rmd",
  # "rexams/week-1/penguins-table.Rmd",
  # "rexams/week-1/penguins.Rmd",
  "rexams/week-01/titanic-proportions-table.Rmd")

elearn_exam <- c(
  "rexams/week-2/ABA-group.Rmd",
  "rexams/week-2/ABA-join.Rmd",
  "rexams/week-2/ABA-pivot-long.Rmd",
  "rexams/week-2/dplyr-verbs.Rmd",
  "rexams/week-2/falling-mutate.Rmd",
  "rexams/week-2/gapminder-group.Rmd",
  "rexams/week-2/lubridate.Rmd",
  "rexams/week-2/murders-arrange.Rmd",
  "rexams/week-2/US-areas-join.Rmd",
  "rexams/week-2/us-contagious-filter.Rmd"
  )

elearn_exam <- c(
  # "rexams/week-02/excel.Rmd",
  # #"rexams/week-02/genomics.Rmd",
  # "rexams/week-02/imdb.Rmd",
  # "rexams/week-02/kaggle-univ-csv.Rmd",
  # "rexams/week-02/methane-csv.Rmd",
   #"rexams/week-02/noaa.Rmd"
  # "rexams/week-02/selector-gadget.Rmd",
   "rexams/week-02/googlesheets.Rmd"
  #"rexams/week-02/genomics.Rmd",
  #"rexams/week-02/pdf.Rmd"
)

elearn_exam <- c(
 "rexams/week-04/hypo-test.Rmd"
 # "rexams/week-04/paired-t-test.Rmd",
 # "rexams/week-04/binomial-probs.Rmd",
 # "rexams/week-04/conf-int.Rmd",
 # "rexams/week-04/normal-probs.Rmd"
 # "rexams/week-04/normal-quant.Rmd",
 # "rexams/week-04/binomial-quant.Rmd",
 # "rexams/week-04/normal-probs-1.Rmd",
 # "rexams/week-04/correct-command.Rmd",
 # "rexams/week-04/soybean-conf-int.Rmd"
)
 elearn_exam <- "rexams/week-02/noaa.Rmd"


set.seed(42)
exams2moodle(elearn_exam, n = 10, name = "rexams22")

