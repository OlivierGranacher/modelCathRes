my_summarise4 <- function(data, expr) {
  data %>% summarise(
    "{{expr}}" := mean({{ expr }})
  )
}
