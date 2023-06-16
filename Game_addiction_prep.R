#Устанавливаем библиотеки. Закоментил, потому что уже установил

#install.packages('tidyverse')

#Подгружаем библиотеки
library(tidyverse)

#ЧИСТКА ДАННЫХ

#Читаем таблицу
gam <- read_csv2("data-tables/gameadddiction.csv")
#Создаём вектор с новыми именами
new_names <- c('time','ID','sex','age', 'max_rating', 'current_rating', 
               'hours_in_game', 'g_w', 'pas01', 'pas02', 'pas03', 
               'pas04', 'pas05', 'pas06', 'pas07', 'pas08', 'pas09', 'rel01', 
               'rel02', 'rel03', 'rel04', 'rel05', 'rel06', 'rel07', 'rel08', 
               'rel09', 'rel10', 'rel11', 'rel12', 'rel13', 'heroes_3')


#Переименовываем все столбцы по-человечески
colnames(gam) <- new_names
#Проверяем, что вышло
str(gam)
View(gam)
#Уберём точно ненужные колонки
gam %>% select(-time) -> gam 


#Уберём строки с пропущенными значениями
gam %>% drop_na() -> gam
#Потеряли 1 строку
#Проверили на всякий случай
gam %>% is.na() %>% summary()


#В столбцах age - hours_in_game должны быть только цифры, 
#выкинем всё остальное (данных много, можем себе позволить)
gam %>% filter((!is.na(as.numeric(age)) & 
                  !is.na(as.numeric(max_rating)) &
                  !is.na(as.numeric(current_rating)) & 
                  !is.na(as.numeric(hours_in_game)))) -> gam
str(gam)


#Займёмся перегонко в хорошие форматы данных, которые есть
#sex в фактор переведём
gam %>% mutate(sex = as.factor(sex)) -> gam
#age, max_rating, current_rating, hours_in_game в числа (потому что числа)
gam %>% mutate(age = as.numeric(age),
               max_rating = as.numeric(max_rating),
               current_rating = as.numeric(current_rating),
               hours_in_game = as.numeric(hours_in_game),) -> gam


#Почистим шутников и ограничим возраст 6-100, 
#рейтинг ограничим 11000 (самый большой в мире),
#часов в игре 17000 (самое большое количество часов в мире),
#уберём тех, у кого текущий рейтинг больше максимального
gam %>% filter(age <101 &
                 age > 5 &
                 max_rating < 11000 &
                 max_rating >= current_rating &
                 hours_in_game < 17000) -> gam

#g_w переведём в фактор
gam %>% mutate(g_w = factor(g_w,
                            levels = c('1-5 игр (по выходным чисто пару каток)', 
                                       '6-10 игр (пару дней в неделю стабильно-дота)', 
                                       '11-20 игр (почти каждый день по несколько игр)', 
                                       '20-30 игр (каждый день по несколько игр)', 
                                       'Больше 30 игр (по много пабликов каждый день)', 
                                       'Больше 50 игр (хочу быть как ILTW, тренируюсь каждый день нон-стоп)'), 
                            ordered = FALSE)) -> gam


#g_w укоротим названия
gam$g_w %>% recode('1-5 игр (по выходным чисто пару каток)' = 
                     '1-5 игр', 
                   '6-10 игр (пару дней в неделю стабильно-дота)' = 
                     '6-10 игр', 
                   '11-20 игр (почти каждый день по несколько игр)' = 
                     '11-20 игр', 
                   '20-30 игр (каждый день по несколько игр)' = 
                     '20-30 игр', 
                   'Больше 30 игр (по много пабликов каждый день)' = 
                     'Больше 30 игр', 
                   'Больше 50 игр (хочу быть как ILTW, тренируюсь каждый день нон-стоп)' = 
                     'Больше 50 игр') -> gam$g_w



#Создадим столбец a_g
gam %>% mutate(a_g = case_when(age < 18 ~ '17 лет или младше',
                                     age < 21 ~ '18-20 лет',
                                     age < 30 ~ '21-29 лет',
                                     age < 40 ~ '30-39 лет',
                                     age < 50 ~ '40-49 лет',
                                     age < 60 ~ '50-59 лет',
                                     age > 59 ~ '60 лет или старше')) -> gam


#a_g переведём в фактор
gam %>% mutate(a_g = factor(a_g,
                                  levels = c('17 лет или младше', '18-20 лет', 
                                             '21-29 лет', '30-39 лет', '40-49 лет', 
                                             '50-59 лет', '60 лет или старше'), 
                                  ordered = FALSE)) -> gam


#Создадим столбец с итоговым баллом вовлечённости (зависимости)
gam %>% mutate(pas_total = rowSums(across(c(pas01:pas09)))) -> gam


#Сохраняем почищенный файл
write_csv2(gam, 'data-tables/gam_clean.csv')
