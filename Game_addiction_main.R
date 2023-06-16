#Устанавливаем библиотеки. Закоментил, потому что уже установил

#install.packages('tidyverse')
#install.packages('pwr')
#install.packages('emmeans')
#install.packages("rempsyc")
#install.packages("(broom")

#Подгружаем библиотеки
library(tidyverse)
library(pwr)
library(emmeans)
library(rempsyc)
library(broom)
library(flextable)

#Загружаем чистый файл
gam_main <- read_csv2("data-tables/gam_clean.csv")
#Поставим тему для граффиков
theme_set(theme_bw())

View(gam_main)

##ОПИСАТЕЛЬНЫЕ СТАТИСТИКИ ----- 


#Статистики по ЗП (уровень вовлечённости)
gam_main %>% summarise(mean = mean(pas_total),
                  median = median(pas_total),
                  sd = sd(pas_total),
                  min = min(pas_total),
                  max = max(pas_total),
                  range = max(pas_total) - min(pas_total),
                  IQR = stats::IQR(pas_total),
                  skewness = datawizard::skewness(pas_total)$Skewness,
                  kurtosis = datawizard::kurtosis(pas_total)$Kurtosis) %>% 
  write_csv('data-tables/Статистики по ЗП (уровень вовлечённости).csv')
#График
gam_main %>% ggplot(aes(x=pas_total)) +
  geom_histogram(aes(y=..density..)) + 
  geom_density(aes(y=..density..)) +
  geom_vline(xintercept=mean(gam_main$pas_total), size=1.0, color="red") +
  geom_vline(xintercept=median(gam_main$pas_total), size=1.0, color="blue") +
  geom_text(aes(x=mean(pas_total)+7, color="red", label=paste0("Среднее"), y=0.090)) +
  geom_text(aes(x=mean(pas_total)+7, color="blue", label=paste0("Медиана"), y=0.080)) +
  theme(legend.position = "none") + 
  labs(x = "Уровень вовлечённости", y = "Плотность вероятности",
       title = "Вовлечённость на выборке")
ggsave('images/Вовлечённость на выборке.png')

#Статистики по НП (возрастные группы)
#По фактору чёт не очень понятно, ограничимся граффиком и ассиметрией с эксцессом
gam_main %>% summarise(skewness = datawizard::skewness(a_g)$Skewness,
                  kurtosis = datawizard::kurtosis(a_g)$Kurtosis) %>%
  write_csv('data-tables/Статистики по НП (Возрастные группы).csv')
#График
gam_main %>% ggplot(aes(x=a_g)) +
  geom_bar() + 
  theme_bw() +
  labs(x = "Возрастная группа", y = "Количество респондентов",
       title = "Вовлечённость на выборке")
ggsave('images/Вовлечённость на выборке.png')

#Статистики по НП (количество игр в неделю)
#По фактору чёт не очень понятно, ограничимся граффиком и ассиметрией с эксцессом
gam_main %>% summarise(skewness = datawizard::skewness(g_w)$Skewness,
                  kurtosis = datawizard::kurtosis(g_w)$Kurtosis) %>%
  write_csv('data-tables/Статистики по НП (количество игр в неделю).csv')
#График
gam_main %>% ggplot(aes(x=g_w)) +
  geom_bar() + 
  theme_bw() +
  scale_x_discrete(labels=c("1-5", "6-10", "11-20", "20-30", "> 30", "> 50")) +
  labs(x = "Количество игр в неделю", y = "Количество респондентов",
       title = "Количество игр в неделю на выборке")
ggsave('images/Количество игр в неделю на выборке.png')

#Статистики по НП (рейтинг - текущий)
#Текущий рейтинг выбрали, потому что тестивароние проводится сейчас, 
cor.test(gam_main$max_rating,gam_main$current_rating)
#а корреляция очень значима и велика (0.93)
gam_main %>% summarise(mean = mean(current_rating),
                  median = median(current_rating),
                  sd = sd(current_rating),
                  min = min(current_rating),
                  max = max(current_rating),
                  range = max(current_rating) - min(current_rating),
                  IQR = stats::IQR(current_rating),
                  skewness = datawizard::skewness(current_rating)$Skewness,
                  kurtosis = datawizard::kurtosis(current_rating)$Kurtosis) %>% 
  write_csv('data-tables/Статистики по НП (рейтинг - текущий).csv')
#График
gam_main %>% ggplot(aes(x=current_rating)) +
  geom_histogram(aes(y=..density..)) + 
  geom_density(aes(y=..density..)) +
  geom_vline(xintercept=mean(gam_main$current_rating), size=1.0, color="red") +
  geom_vline(xintercept=median(gam_main$current_rating), size=1.0, color="blue") +
  geom_text(aes(x=mean(current_rating)+1600, color="red", label="Среднее", y=0.00030)) +
  geom_text(aes(x=mean(current_rating)+1600, color="blue", label="Медиана", y=0.00027)) +
  theme(legend.position = "none") + 
  labs(x = "Текущий рейтинг", y = "Плотность вероятности",
       title = "Текущий рейтинг на выборке")
ggsave('images/Текущий рейтинг на выборке.png')

#Искомый график
gam_main %>% ggplot(aes(x=current_rating, y = pas_total, color = g_w)) +
  scale_color_manual(values = colorspace::rainbow_hcl(length(unique(gam_main$g_w))),
                     labels = c("1-5", "6-10", "11-20", "20-30", "> 30", "> 50")) +
  ylim(9, 45) +
  xlim(0, 8000) +
  geom_point(alpha = 0.2)+
  geom_smooth(method = 'lm')+
  facet_wrap( ~ a_g) + 
  labs(x = "Текущий рейтинг", y = "Уровень вовлечённоси", title = "График искомых закономерностей") 
ggsave('images/График искомых закономерностей.png')

##ЛИНЕЙНАЯ РЕГРЕССИЯ ----- 


#График в Финале прошлого раздела показал, 
#что возрстные группые 40+ представлены очень слабо, 
#поэтому мы их из анализа выкинем
gam_main %>% 
  filter(a_g %in% 
           c('17 лет или младше', '18-20 лет', '21-29 лет', '30-39 лет')) -> gam_main
#Начнём с малого. Посмотрим предикторы отдельно


#Сначала рейтинг
model1 <- lm(pas_total ~ current_rating, gam_main)
summary(model1)
#Проверим допущения
#plot(model1)
#Модель слабовата, но стастически значима
#Однако при очень большой выборке мы замечаем даже слабые эффекты, 
#предсказывает 0.06% дисперсии


#Теперь посмотрим возрастную группу
model2 <- lm(pas_total ~ a_g, gam_main)
summary(model2)
#Сравним группы между собой
emmeans(model2, pairwise ~ a_g)
#Проверим допущения
#plot(model2)
#Здесь модель уже имеет большую значимость, но предсказывает очень маленькую дисперсию (0.8% дисперсии)
#Значимы предиктором является принадлежность к первой и второй возрастным группам


#Теперь посмотрим количество игр в неделю
model3 <- lm(pas_total ~ g_w, gam_main)
summary(model3)
#Сравним группы между собой
emmeans(model3, pairwise ~ g_w)
#Проверим допущения
#plot(model3)
#Эта модель уже лучше, она предсказывает 15.8% дисперсии, кайф! (начало)
#Значимые предикторы - это принадлженость к тем, кто играет 1-5, 6-10 и 11-20


#Теперь посмотрим игры в неделю вместе с возрастной группой
model4 <- lm(pas_total ~ g_w + a_g, gam_main)
summary(model4)
#Проверим допущения
#plot(model4)
#Модель не лучше предыдущей, и значимы те-же предикторы, объясняет 16.0% дисперсии 
#Проверим вздутость (если предикторы коррелируют между собой сильно)
car::vif(model4)
#Её нет


#Теперь посмотрим все три
model5 <- lm(pas_total ~ g_w + a_g + current_rating, gam_main)
summary(model5)
#Добавление рейтинга модель не улучшило(


#Теперь посмотрим игры в неделю, возрастную группу и их взаимодействие
model6 <- lm(pas_total ~ g_w + a_g + g_w*a_g, gam_main)
summary(model6)
#Модель оказалась хуже чем модель без взаимодействия... Объясняем 15.8% дисперсии
#Проверим допущения
#plot(model6)


#А теперь добавим в к ним рейтинг
model7 <- lm(pas_total ~ g_w + a_g + current_rating + g_w*a_g, gam_main)
summary(model7)
#Модель плохая, предикторы лишние, гипотеза не подтверждается, мы плачем((((
#Дисперсия 15.9%





#Может быть был косяк с тем, что мы перевели шкалу отношений в ординаруную?
#А давайте оставим возраст, как шкалу отношений и посмотрим
model8 <- lm(pas_total ~ age, gam_main)
summary(model8)
#Модель объясняет 0.7% дисперсии. Ну тоже плохо (((


#Теперь посмотрим игры в неделю, возраст и их взаимодействие
model9 <- lm(pas_total ~ g_w + age + g_w*age, gam_main)
summary(model9)
#Модель объясняет 16% дисперсии и плохо интерпретируется

#Нет, не был


##МОЩНОСТЬ ----- 


#Посчитаем мощность для подтвердившихся гипотез
#Возрастная группа
pwr.f2.test(u = 3, v = 5717, sig.level = 0.01, f2 = 0.008/(1 - 0.008))
#Игр в неделю
pwr.f2.test(u = 5, v = 5715, sig.level = 0.01, f2 = 0.158/(1 - 0.158))


##ВИЗУАЛИЗАЯ ----- 


#Давайте построим графики для гипотез, почему бы и нет собственно


#Гипотеза о рейтинге
gam_main %>% ggplot(aes(x=current_rating, y = pas_total)) +
  ylim(9, 45) +
  xlim(0, 8000) +
  geom_point(alpha = 0.2)+
  geom_smooth(method = 'lm')+
  labs(x = "Текущий рейтинг", y = "Уровень вовлечённоси", title = "Показатель вовлечённости от рейтинга") 
ggsave('images/Показатель вовлечённости от рейтинга.png')

#Гипотеза о возрастных группах
ggplot(gam_main,
       aes(a_g, pas_total)) +
  stat_summary(fun = mean, geom = 'point') +
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') +
  labs(x = "Возрастная группа", y = "Показатель вовлечённости",
       title = "Показатель вовлечённости среди разных возрастных групп")
ggsave('images/Показатель вовлечённости среди разных возрастных групп.png')

#Гипотеза об играх в неделю
ggplot(gam_main,
       aes(g_w, pas_total)) +
  stat_summary(fun = mean, geom = 'point') +
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') +
  labs(x = "Игр в неделю", y = "Показатель вовлечённости",
       title = "Показатель вовлечённости среди от игр в неделю") +
  scale_x_discrete(labels=c("1-5", "6-10", "11-20", "20-30", "> 30", "> 50"))
ggsave('images/Показатель вовлечённости среди от игр в неделю.png')

#Гипотеза о взаимодействии
ggplot(gam_main,
       aes(g_w, pas_total, color = a_g,
           group = interaction(g_w, a_g))) +
  stat_summary(fun = mean, geom = 'point', position = position_dodge(0.5)) +
  stat_summary(fun.data = mean_cl_boot,position = position_dodge(0.5), geom = 'errorbar') +
  labs(x = "Игр в неделю", y = "Показатель вовлечённости", color = "Возрастная группа",
       title = "Показатель вовлечённости среди игроков от игр в неделю и возрастной группы") +
  scale_x_discrete(labels=c("1-5", "6-10", "11-20", "20-30", "> 30", "> 50")) +
  scale_color_manual(values = c('green', 'red', 'blue', 'orange'))
ggsave('images/Показатель вовлечённости среди игроков от игр в неделю и возрастной группы.png')

##Таблички ----- 


#Ну. Раз академическая преза, то давайте оформлять таблички по APA
#Для регрессий


#Табличку, где модели сравнены я сделал руками (сдался, да)


#Для первой гипотезы:
tabl1 <- tidy(model2, conf.int = TRUE)
save_as_docx(nice_table(tabl1, broom = "lm", 
                        note = "* p < .05, ** p < .01, *** p < .001"), 
             path = "apa-tables/hyp1.docx")
#Для второй гипотезы:
tabl2 <- tidy(model3, conf.int = TRUE)
save_as_docx(nice_table(tabl2, broom = "lm", 
                        note = "* p < .05, ** p < .01, *** p < .001"), 
             path = "apa-tables/hyp2.docx")
#Для третьей гипотезы:
tabl3 <- tidy(model1, conf.int = TRUE)
save_as_docx(nice_table(tabl3, broom = "lm", 
                        note = "* p < .05, ** p < .01, *** p < .001"), 
             path = "apa-tables/hyp3.docx")
#Для четвёртой гипотезы:
tabl4 <- tidy(model4, conf.int = TRUE)
save_as_docx(nice_table(tabl4, broom = "lm", 
                        note = "* p < .05, ** p < .01, *** p < .001"), 
             path = "apa-tables/hyp4_1.docx")
tabl5 <- tidy(model6, conf.int = TRUE)
save_as_docx(nice_table(tabl5, broom = "lm", 
                        note = "* p < .05, ** p < .01, *** p < .001"), 
             path = "apa-tables/hyp4_2.docx")


#Для попарных сравнений
tabl6 <- as.data.frame(emmeans(model2, pairwise ~ a_g)$contrasts)
names(tabl6) <- c("Contrast", "Estimate", "SE", "Df", "t", "p")
tabl7 <- as.data.frame(emmeans(model3, pairwise ~ g_w)$contrasts)
names(tabl7) <- c("Contrast", "Estimate", "SE", "Df", "t", "p")


tabl6 <- tidy(tabl6)
tabl7 <- tidy(tabl7)


save_as_docx(nice_table(tabl6, 
                        note = c("* p < .05, ** p < .01, *** p < .001", 
                                 "P value adjustment: Tukey method for comparing a family of 4 estimates ")), 
             path = "apa-tables/comp1.docx")
save_as_docx(nice_table(tabl7, 
                        note = c("* p < .05, ** p < .01, *** p < .001", 
                                 "P value adjustment: Tukey method for comparing a family of 6 estimates ")), 
             path = "apa-tables/comp2.docx")