#Устанавливаем библиотеки. Закоментил, потому что уже установил

#install.packages('poLCA')
#install.packages('tidyverse')
#install.packages('pwr')
#install.packages('ez')
#install.packages('emmeans')

#Подгружаем библиотеки
library(poLCA)
library(tidyverse)
library(pwr)
library(ez)
library(emmeans)

#Загружаем чистый файл
gam_add <- read_csv("gam_clean.csv")
#Поставим тему для граффиков
theme_set(theme_bw())


##LCA(Latent Class Analysis) ----- 


#Давайте посмотрим, как наши ребята делятся по отношению к игре


#Для этой процедуры мы решили использовать LCA. 
#Понадобится небольшая предобработка
#Вопросы были по 5-тибальной ликертовской шкале. 
#Интерпретировать будет тяжеловата
#Щедрым жестом перегоним в трёхбальную
gam_add %>% mutate(across(
  rel01:rel13, function(x) ifelse(x == 3, 2, ifelse(x > 3, 3, 1)))
  ) -> gam_add#Аналоги лямбда функции в питоне


#Ну, а теперь собственно будем использовать сам метод


#Запишем формулу для LCA
f <- cbind(rel01, rel02, rel03, rel04, rel05, rel06, rel07,
           rel08, rel09, rel10, rel11, rel12, rel13)~1


#Прогоним мето, используя разное количество предполагаемых классов: от 2 до 13
gam_add_relation2 <- poLCA (f, gam_add, nclass = 2, maxiter = 50000, 
                            graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation3 <- poLCA (f, gam_add, nclass = 3, maxiter = 50000, 
                            graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation4 <- poLCA (f, gam_add, nclass = 4, maxiter = 50000, 
                            graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation5 <- poLCA (f, gam_add, nclass = 5, maxiter = 50000, 
                            graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation6 <- poLCA (f, gam_add, nclass = 6, maxiter = 50000, 
                            graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation7 <- poLCA (f, gam_add, nclass = 7, maxiter = 50000, 
                            graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation8 <- poLCA (f, gam_add, nclass = 8, maxiter = 50000, 
                            graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation9 <- poLCA (f, gam_add, nclass = 9, maxiter = 50000, 
                            graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation10 <- poLCA (f, gam_add, nclass = 10, maxiter = 50000, 
                             graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation11 <- poLCA (f, gam_add, nclass = 11, maxiter = 50000, 
                             graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation12 <- poLCA (f, gam_add, nclass = 12, maxiter = 50000, 
                             graphs = FALSE, nrep =  10, verbose = TRUE)
gam_add_relation13 <- poLCA (f, gam_add, nclass = 13, maxiter = 50000, 
                             graphs = FALSE, nrep =  10, verbose = TRUE)


#Создадим тибл с итоговыми показателями качества модели
tibble(LC_num = c(2:13), 
       AIC = c(gam_add_relation2$aic, gam_add_relation3$aic, 
               gam_add_relation4$aic, gam_add_relation5$aic,
               gam_add_relation6$aic, gam_add_relation7$aic, 
               gam_add_relation8$aic, gam_add_relation9$aic, 
               gam_add_relation10$aic, gam_add_relation11$aic, 
               gam_add_relation12$aic, gam_add_relation13$aic), 
       BIC = c(gam_add_relation2$bic, gam_add_relation3$bic, 
               gam_add_relation4$bic, gam_add_relation5$bic, 
               gam_add_relation6$bic, gam_add_relation7$bic, 
               gam_add_relation8$bic, gam_add_relation9$bic, 
               gam_add_relation10$bic, gam_add_relation11$bic, 
               gam_add_relation12$bic, gam_add_relation13$bic)
)-> gam_add_LCA


#Сохраним, чтобы потом не считать модельки заново 
write_csv(gam_add_LCA, 'gam_add_LCA_res.csv')


#Добавим класс в общий датасет
gam_add %>% mutate(class = gam_add_relation7$predclass) -> gam_add


##LCA. ГРАФИК ----- 


#Построим графики и выберем модель
#Но для начала превратим long-формат
gam_add_LCA_long <- pivot_longer(gam_add_LCA, cols = c('AIC','BIC'))


ggplot(gam_add_LCA_long, aes(as_factor(LC_num), value, 
                             shape = name, group = name)) + 
  geom_point() +
  geom_line() +
  labs(x = "Количество классов", y = "AIC/BIC",
       title = "Выберем модельку", shape = 'Информационные\nкритерии')


#Ну. Выбрали модельку. 7 классов. Давайте на неё смотреть
gam_add_relation7 <- poLCA (f, gam_add, nclass = 7, maxiter = 50000, 
                            graphs = TRUE, nrep =  10, verbose = TRUE)


##LCA. ПЕРЕВОД В 4-ЗНАЧНУЮ СИСТЕМУ ДЛЯ ПРОСТОТЫ ИНТЕРПЕРТАЦИИ ----- 


#Сделаем табличку, где напротив каждого класса вероятность того 
#или иного значения

#Пустой тибл
prob <- tibble(class = c(1:7), rel01 = 0, rel02 = 0, rel03 = 0, rel04 = 0, 
               rel05 = 0, rel06 = 0, rel07 = 0,rel08 = 0, rel09 = 0, rel10 = 0, 
               rel11 = 0, rel12 = 0, rel13 = 0)

#Пройдёмся по всем вопросам и создадим столбец с самыми вероятными 
#значениями ответа на вопрос для всех классов              
for (i in 1:13) {
  #Для каждого вопроса создали тибл, где столбцы отвечают за уровень
  
  #превращаем в вектор список вероятность для одного вопроса
  a = unlist(gam_add_relation7$probs[i], recursive = TRUE, use.names = FALSE) 
  a_t = tibble(low = c(a[1:7]),
               med = c(a[8:14]),
               high = c(a[15:21]))
  a_t %>% mutate(a = ifelse(low > 0.6, 1, 
                            ifelse(med > 0.6, 2, 
                                   ifelse(high > 0.6, 3, 0)))) -> a_t
  prob[1+i] <- a_t$a
}

#Сохраним
write_csv(prob, 'class_most_prob_answ.csv')