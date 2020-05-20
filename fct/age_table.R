age_table = structure(list(idade_lower = c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L), 
                           idade_upper = c(9L, 19L, 29L, 39L, 49L, 59L, 69L, 79L, 100L), 
                           ID = c("age_1", "age_2", "age_3", "age_4", "age_5", "age_6", "age_7", "age_8", "age_9"), 
                           faixas = c("0 a 9", "10 a 19", "20 a 29", "30 a 39", 
                                      "40 a 49", "50 a 59", "60 a 69", "70 a 79", "80+")),
                      class = "data.frame", row.names = c(NA, -9L))

classifyAgeFast = function(x){
  laply(x, function(age) paste0("age_", sum(age >= age_table$idade_lower)))
}
