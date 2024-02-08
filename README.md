# Игровая зависимость

## Контекст и цель
Датасет по игровой зависимости собрал один из ютуберов, в прошлом психолог. Датасет крупный, цепляет где-то 1% ГС. Однако проблема в том, что данные грязные, не были формализованы ответы на многие вопросы + одна из самых ценных частей (опросник) была составлена интуитивно, без научной опробации.
Был выдвинут ряд исследовательских гипотез:

### Основные гипотезы
1.  У подростков (13-17 лет) и молодых взрослых (18-22) наблюдается повышение баллов по опроснику
2.  У игроков, играющих больше игр в неделю, наблюдается повышение баллов по опроснику
3.  У игроков с высоким актуальным рейтингом наблюдается повышение баллов по опроснику
4.  В возрастных группах 18+ связь между количеством игр в неделю и вовлеченностью усиливается

### Дополнительные гипотезы
1.  На основании ответов на вопросы об отношении к игре можно кластеризовать игроков
2.  Эти класстеры будут различаться по возрасту, вовлечённости в игру, часам в игре, рейтингу

### Дополнительные задачи:
1. Сравнить результаты анализа, полученные в парадигме байесовской и фреквентистской статистики
2. Автоматизировать оформление результатов по стандарту APA

## Стек
### R
-   library(tidyverse) --- пайплайны и тиблы
-   library(pwr) --- анализ мощности
-   library(emmeans) --- попарные сравнения
-   library(rempsyc) --- вывод таблиц
-   library(broom) --- вывод таблиц
-   library(flextable) --- вывод таблиц
-   library(apaTables) --- вывод таблиц
-   library(rempsyc) --- вывод таблиц
-   library(poLCA) --- кластеризация
-   library(ez) --- фреквентистская анова
-   library(BayesFactor) --- байесовская анова

## Этапы работы
1. Чистка данных, работа с пропусками/выбросами/невозможными значениями, редактирование форматов (Game_addiction_prep)
2. Изучения распредления переменных, эксплаторный факторный анализ, визуализация (Game_addiciton_main)
3. Проверка допущений, проверка гипотез с помощью линейных моделей и попарных сравнений, визуализация (Game_addiciton_main)
4. Подсчёт полученной мощности (Game_addiciton_main)
5. Оформление результатов по APA (Game_addiciton_main)
6. Перевод itemов в трёхбальные шкалы, LCA для классификации по отношению к игре, визуализации (Game_addiciton_main)
7. Проверка дополнительный гипотез с помощью байесовской и фреквентистской статистики, сравнение результатов, визуализации (Game_addiciton_main)

## Результат

1. Из основных гипотез первые две подтвердились, вторые две нет
2. Получить осмысленные класстеры удалось
3. Различия по возрасту и другим параметрам значимы - "агро-школьники" это не миф
4. На таком крупном датасете использование байесовской и фреквентистской статистики привело к одинаковым выводам, однако интерпретация байесовского варианта более наглядна
