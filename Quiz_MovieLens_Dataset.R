# Quiz: MovieLens Dataset

# Question 5
edx %>% filter(str_detect(genres,"Drama")) %>% nrow() 
edx %>% filter(str_detect(genres,"Comedy")) %>% nrow()
edx %>% filter(str_detect(genres,"Thriller")) %>% nrow()
edx %>% filter(str_detect(genres,"Romance")) %>% nrow()