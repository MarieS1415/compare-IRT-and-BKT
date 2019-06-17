library(rstudioapi)
library(mirt)
library(dplyr)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#fix random
set.seed(153)

#read data
#we make a strong hypothesis here by using this data: that all the 100 questions deal with the same concept
first_data <- read.csv(file="data/2015_100_skill_builders_main_problems.csv", header=TRUE, sep=",")
#if correct different from 0 or 1, put back to these values
first_data=first_data%>%mutate(correct=1*(correct>0.5))

student_ids=unique(first_data$user_id)
student_ids=sapply(student_ids, function(x) toString(x))
factor(student_ids)
question_ids=unique(first_data$sequence_id)

# construct the data frame the library can work with
data_irt=data.frame()
data_irt <- first_data %>%
  group_by(sequence_id, user_id) %>%
  arrange(log_id, .by_group = TRUE) %>%
  filter((row_number() == 1)) %>% 
  reshape2::acast(., user_id ~ sequence_id, value.var="correct")

# en moyenne, les apprenants répondent à combien de questions ?
nb_of_answers_per_stud=apply(data_irt, 1, function(x) sum(x,na.rm=TRUE))
print("in average, students answer to:")
print(mean(nb_of_answers_per_stud))

nb_of_answers_per_qu=apply(data_irt, 2, function(x) sum(x,na.rm=TRUE))
print("in average, a question is answered:")
print(mean(nb_of_answers_per_qu))

#instantiate error
error=list()

#prepare cross validation
student_cv=list()
k=5
nb_students_cv=floor(length(student_ids)/k)
for (i in 1:(k-1)){
  cv_index=sample(1:length(student_ids), nb_students_cv)
  student_cv[[i]]=student_ids[cv_index]
  student_ids=student_ids[-cv_index]
}
student_cv[[k]]=student_ids

#do five times the following
for (i in 1:5) {
  #select 80% of students for training, the others for testing
  students_train=unlist(student_cv[-i])
  students_test=unlist(student_cv[i])
  
  #train irt with these students
  data_irt_train=data_irt[students_train,]
  
  model_irt = mirt(data = data_irt_train, itemtype = '2PL', model = 1)
  coefficients = coef(model_irt, IRTpars = TRUE)

  data_irt_test=first_data %>% filter(user_id %in% students_test)
  
  answer_complete=data_irt_test %>%
    group_by(sequence_id, user_id) %>%
    arrange(log_id, .by_group = TRUE) %>%
    filter((row_number() == 1))
  
  log_complete=data_irt_test %>%
    group_by(sequence_id, user_id) %>%
    arrange(log_id, .by_group = TRUE) %>%
    filter((row_number() == 1))
  
  #apply irt to the 20% of students remaining
  #for each answer, store the result (=error), and the number of the question (first one, seconde one...)
  for (student_test in students_test){
    data_irt_test_current=data_irt_test %>% filter(user_id==student_test)
    questions_test=unique(data_irt_test_current$sequence_id)
    questions_test=lapply(questions_test, toString)
    for (j in 1:length(questions_test)){
      if (j==1){
        #for the first question, we do not have any information about the student:
        #we make the hypothesis that s/he has the mean ability, 0
        theta=0
      } else{
        #find log_id of the question we want to estimate
        find_log <- log_complete %>%
          filter(user_id==student_test) %>%
          filter(sequence_id==questions_test[j]) %>%
          reshape2::acast(., user_id ~ sequence_id, value.var="log_id")
        
        #construct response pattern
        complete_pattern=data.frame(rep(NA, length(question_ids)))
        row.names(complete_pattern)=question_ids
        pattern=data_irt_test %>%
          filter(user_id==student_test) %>%
          filter(log_id<find_log[1,1]) %>%
          group_by(sequence_id, user_id) %>%
          filter((row_number() == 1)) %>%
          arrange(desc(log_id), .by_group = TRUE) %>%
          reshape2::acast(., user_id ~ sequence_id, value.var="correct")
        pattern=t(pattern)
        complete_pattern[row.names(pattern),1]=pattern
        
        #compute the ability theta given the known response pattern
        scores=fscores(model_irt, response.pattern = t(complete_pattern))
        theta=scores[1,"F1"]
      }
      
      #compute expectation for the next item given the estimated mastery
      expected=expected.item(extract.item(model_irt, toString(questions_test[j])), c(theta))
      
      #compare expectation with reality
      answer=answer_complete %>%
        filter(user_id==student_test) %>%
        filter(sequence_id==questions_test[j]) %>%
        reshape2::acast(., user_id ~ sequence_id, value.var="correct")

      cur_error=abs(expected-answer[1,1])
      error[[toString(j)]]=append(error[[toString(j)]], cur_error)
    }
  }  
}

mean_error=lapply(error, function(x){mean(x, na.rm = TRUE)})
plot(unlist(mean_error))
