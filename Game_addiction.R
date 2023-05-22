library(tidyverse)
library(pwr)

#ЧИСТКА ДАННЫХ


#Читаем таблицу
gam <- read_csv2("gameadddiction.csv")
#Создаём вектор с новыми именами
new_names <- c('time','ID','sex','age', 'max_rating', 'current_rating', 'hours_in_game', 'games_week', 
               'pas01', 'pas02', 'pas03', 'pas04', 'pas05', 'pas06', 'pas07', 'pas08', 'pas09', 
               'rel01', 'rel02', 'rel03', 'rel04', 'rel05', 'rel06', 'rel07', 'rel08', 'rel09', 'rel10', 
               'rel11', 'rel12', 'rel13', 'heroes_3')

#Переименовываем все столбцы по-человечески
colnames(gam) <- new_names
#Проверяем, что вышло
str(gam)
View(gam)
#Уберём точно ненужные колонки
gam %>% select(-c(time, heroes_3)) -> gam 

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

#games_week переведём в фактор
gam %>% mutate(games_week = factor(games_week,
                                  levels = c('1-5 игр (по выходным чисто пару каток)', '6-10 игр (пару дней в неделю стабильно-дота)', '11-20 игр (почти каждый день по несколько игр)', '20-30 игр (каждый день по несколько игр)', 'Больше 30 игр (по много пабликов каждый день)', 'Больше 50 игр (хочу быть как ILTW, тренируюсь каждый день нон-стоп)'), 
                                  ordered = TRUE)) -> gam
#Создадим столбец age_group
gam %>% mutate(age_group = case_when(age < 18 ~ '17 лет или младше',
                                     age < 21 ~ '18-20 лет',
                                     age < 30 ~ '21-29 лет',
                                     age < 40 ~ '30-39 лет',
                                     age < 50 ~ '40-49 лет',
                                     age < 60 ~ '50-59 лет',
                                     age > 59 ~ '60 лет или старше')) -> gam
#age_group переведём в фактор
gam %>% mutate(age_group = factor(age_group,
                                   levels = c('17 лет или младше', '18-20 лет', '21-29 лет', '30-39 лет', '40-49 лет', '50-59 лет', '60 лет или старше'), 
                                   ordered = TRUE)) -> gam
#Создадим столбец с итоговым баллом вовлечённости (зависимости)
gam %>% mutate(pas_total = rowSums(across(c(pas01:pas09)))) -> gam

#Сохраняем почищенный файл
write_csv(gam, 'gam_clean.csv')


#ОПИСАТЕЛЬНЫЕ СТАТИСТИКИ


#Статистики по ЗП (уровень вовлечённости)
gam %>% summarise(mean = mean(pas_total),
                  median = median(pas_total),
                  sd = sd(pas_total),
                  min = min(pas_total),
                  max = max(pas_total),
                  range = max(pas_total) - min(pas_total),
                  IQR = stats::IQR(pas_total),
                  skewness = datawizard::skewness(pas_total)$Skewness,
                  kurtosis = datawizard::kurtosis(pas_total)$Kurtosis) %>% 
  write_csv('Статистики по ЗП (уровень вовлечённости).csv')
#График
gam %>% ggplot(aes(x=pas_total)) +
  geom_histogram(aes(y=..density..)) + 
  geom_density(aes(y=..density..)) +
  geom_vline(xintercept=mean(gam$pas_total), size=1.0, color="red") +
  geom_vline(xintercept=median(gam$pas_total), size=1.0, color="blue") +
  geom_text(aes(x=mean(gam$pas_total)+7, , color="red", label=paste0("Среднее"), y=0.090)) +
  geom_text(aes(x=mean(gam$pas_total)+7, , color="blue", label=paste0("Медиана"), y=0.080)) +
  theme(legend.position = "none") + 
  labs(x = "Уровень вовлечённости", y = "Плотность вероятности",
       title = "Вовлечённость на выборке")

#Статистики по НП (возрастные группы)
#По фактору чёт не очень понятно, ограничимся граффиком и ассиметрией с эксцессом
gam %>% summarise(skewness = datawizard::skewness(age_group)$Skewness,
                  kurtosis = datawizard::kurtosis(age_group)$Kurtosis) %>%
  write_csv('Статистики по НП (Возрастные группы).csv')
#График
gam %>% ggplot(aes(x=age_group)) +
  geom_bar() + 
  theme_bw() +
  labs(x = "Возрастная группа", y = "Количество респондентов",
       title = "Распределение по возрастам")

#Статистики по НП (количество игр в неделю)
#По фактору чёт не очень понятно, ограничимся граффиком и ассиметрией с эксцессом
gam %>% summarise(skewness = datawizard::skewness(games_week)$Skewness,
                  kurtosis = datawizard::kurtosis(games_week)$Kurtosis) %>%
  write_csv('Статистики по НП (количество игр в неделю).csv')
#График
gam %>% ggplot(aes(x=games_week)) +
  geom_bar() + 
  theme_bw() +
  scale_x_discrete(labels=c("1-5", "6-10", "11-20", "20-30", "> 30", "> 50")) +
  labs(x = "Количество игр в неделю", y = "Количество респондентов",
       title = "Количество игр в неделю на выборке")

#Статистики по НП (рейтинг - текущий)
#Текущий рейтинг выбрали, потому что тестивароние проводится сейчас, 
cor.test(gam$max_rating,gam$current_rating)
#а корреляция очень значима и велика (0.93)
gam %>% summarise(mean = mean(current_rating),
                  median = median(current_rating),
                  sd = sd(current_rating),
                  min = min(current_rating),
                  max = max(current_rating),
                  range = max(current_rating) - min(current_rating),
                  IQR = stats::IQR(current_rating),
                  skewness = datawizard::skewness(current_rating)$Skewness,
                  kurtosis = datawizard::kurtosis(current_rating)$Kurtosis) %>% 
  write_csv('Статистики по НП (рейтинг - текущий).csv')
#График
gam %>% ggplot(aes(x=current_rating)) +
  geom_histogram(aes(y=..density..)) + 
  geom_density(aes(y=..density..)) +
  geom_vline(xintercept=mean(gam$current_rating), size=1.0, color="red") +
  geom_vline(xintercept=median(gam$current_rating), size=1.0, color="blue") +
  geom_text(aes(x=mean(gam$current_rating)+1600, , color="red", label="Среднее", y=0.00030)) +
  geom_text(aes(x=mean(gam$current_rating)+1600, , color="blue", label="Медиана", y=0.00027)) +
  theme(legend.position = "none") + 
  labs(x = "Текущий рейтинг", y = "Плотность вероятности",
       title = "Текущий рейтинг на выборке")

#Искомый график
gam %>% ggplot(aes(x=current_rating, y = pas_total, color = games_week)) +
  scale_color_manual(values = colorspace::rainbow_hcl(length(unique(diamonds$color))),
                     labels = c("1-5", "6-10", "11-20", "20-30", "> 30", "> 50")) +
  ylim(9, 45) +
  xlim(0, 8000) +
  geom_point(alpha = 0.2)+
  geom_smooth(method = 'lm')+
  facet_wrap( ~ age_group) + 
  labs(x = "Текущий рейтинг", y = "Уровень вовлечённоси", title = "График искомых закономерностей") 


#ОБРАБОТКА ДАННЫХ
#ЛИНЕЙНАЯ РЕГРЕССИЯ


#График выше показал, что возрстные группые 40+ представлены очень слабо, поэтому мы их из анализа выкинем
gam %>% filter(age_group %in% c('17 лет или младше', '18-20 лет', '21-29 лет', '30-39 лет')) -> gam
#Начнём с малого. Посмотрим предикторы отдельно

#Сначала рейтинг
model1 <- lm(pas_total ~ current_rating, gam)
summary(model1)
plot(model1)
#Модель слабовата, но стастически значима
#Однако при очень большой выборке мы замечаем даже слабые эффекты, предсказывает 0.06% дисперсии

#Теперь посмотрим возрастную группу
model2 <- lm(pas_total ~ age_group, gam)
summary(model2)
plot(model2)
#Здесь модель уже имеет большую значимость, но предсказывает очень маленькую дисперсию (0.8% дисперсии)
#Значимы предиктором является принадлежность к первой и второй возрастным группам

#Теперь посмотрим количество игр в неделю
model3 <- lm(pas_total ~ games_week, gam)
summary(model3)
plot(model3)
#Эта модель уже лучше, она предсказывает 15.8% дисперсии, кайф! (начало)
#Значимые предикторы - это принадлженость к тем, кто играет 1-5, 6-10 и 11-20


#Теперь посмотрим игры в неделю вместе с возрастной группой
model4 <- lm(pas_total ~ games_week + age_group, gam)
summary(model4)
plot(model4)
#Модель не лучше предыдущей, и значимы те-же предикторы, объясняет 16.0% дисперсии 
#Проверим вздутость (если предикторы коррелируют между собой сильно)
car::vif(model4)
#Её нет

#Теперь посмотрим все три
model5 <- lm(pas_total ~ games_week + age_group + current_rating, gam)
summary(model5)
#Добавление рейтинга модель не улучшило(

#Теперь посмотрим игры в неделю, возрастную группу и их взаимодействие
model6 <- lm(pas_total ~ games_week + age_group + games_week*age_group, gam)
summary(model6)
#Модель оказалась хуже чем модель без взаимодействия... Объясняем 15.8% дисперсии
#Проверим вздутость
car::vif(model6)
#Её нет, но задатки появляются
plot(model6)


#А теперь добавим в к ним рейтинг
model7 <- lm(pas_total ~ games_week + age_group + current_rating + games_week*age_group, gam)
summary(model7)
#Модель плохая, предикторы лишние, гипотеза не подтверждается, мы плачем((((
#Дисперсия 15.9%





#Может быть был косяк с тем, что мы перевели шкалу отношений в ординаруную?
#А давайте оставим возраст, как шкалу отношений и посмотрим
model8 <- lm(pas_total ~ age, gam)
summary(model8)
#Модель объясняет 0.7% дисперсии. Ну тоже плохо (((

#Теперь посмотрим игры в неделю, возраст и их взаимодействие
model9 <- lm(pas_total ~ games_week + age + games_week*age, gam)
summary(model9)
#Модель объясняет 16% дисперсии и плохо интерпретируется

#Нет, не был

View(model1)
#Посчитаем мощность для подтвердившихся гипотез
#Возрастная группа
pwr.f2.test(u = 3, v = 5717, sig.level = 0.01, f2 = 0.008/(1 - 0.008))
#Игр в неделю
pwr.f2.test(u = 5, v = 5715, sig.level = 0.01, f2 = 0.158/(1 - 0.158))



