library(dplyr)
input <- readr::read_delim("input01.txt", delim = "   ", col_names = FALSE)
input |>
  mutate(across(X1:X2, sort)) |>
  mutate(diff = (abs(X1 - X2))) |>
  summarise(res = sum(diff))

cntX2 <- table(input$X2)
idx <- match(as.numeric(names(cntX2)))

sum(input$X1 * cntX2[idx], na.rm = TRUE)
