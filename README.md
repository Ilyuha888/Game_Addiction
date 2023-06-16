# Game Addiction

## Предисловие

### В общих словах

Этот проект был выполнен мной в рамках учебной дисциплины. Нам предоставлили данные и дали свободу работы с ними. Я выбрал датасет по игровой зависимости. Его собрал один из ютуберов, в прошлом психолог. Датасет крупный. Однако проблема в том, что он был грязноват + одна из самых ценных частей (опросник) была составлена интуитивно, без научной опробации

### Основные гипотезы

1.  У подростков (13-17 лет) и молодых взрослых (18-22) наблюдается повышение баллов по опроснику
2.  У игроков, играющих больше игр в неделю, наблюдается повышение баллов по опроснику
3.  У игроков с высоким актуальным рейтингом наблюдается повышение баллов по опроснику
4.  В возрастных группах 18+ связь между количеством игр в неделю и вовлеченностью усиливается

### Дополнительные гипотезы

1.  На основании ответов на вопросы об отношении к игре можно кластеризовать игроков
2.  Эти класстеры будут различаться по возрасту, вовлечённости в игру, часам в игре, рейтингу

### Используемые библиотеки

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

## Game_addiction_prep

### Общее

Это файл, где я почистил данные: привёл к нужным форматам, удалил строки с пропусками и некорретно заполненными пунктами (подробнее в комментариях), удалено было 15% данных. Терпимо, часть данных можно было бы спасти, но я не посчитал это оправданным

### Легенда

-   time --- Отметка времени

-   ID --- Id

-   sex --- Пол (М - 2, Ж - 1)

-   age --- Возраст

-   max_rating --- Максимальный рейтинг в Доте

-   current_rating --- Рейтинг в доте на данный момент

-   hours_in_game --- Количество часов в игре (хотя бы примерное)

-   g_w --- Какое количество игр в среднем ты играешь в неделю?

-   pas01 --- У меня ухудшается настроение, если я долго не играю в Доту 2

-   pas02 --- Почти все свое свободное время я трачу на Доту 2

-   pas03 --- Я часто играю больше, чем нужно, так как не могу остановиться (регаю новую игру)

-   pas04 --- Когда я играю в Доту я полностью над ней сосредоточен (не играю "фо фан")

-   pas05 --- С каждой неделей/месяцем я провожу в Доте все больше времени, чем раньше

-   pas06 --- Я часто регаю некст игру несмотря на дискомфорт (я могу играть голодный, не выспавшийся, в плохом самочувствии и т.д.)

-   pas07 --- Я не рассказываю семье/друзьям о количестве часов, которое я провожу в Доте

-   pas08 --- Дота позволяет мне забыть о стрессе и жизненных проблемах ирл

-   pas09 --- Из-за количества часов, проводимых в игре я хуже учусь/работаю/выполняю иные задачи

-   rel01 --- Каждую игру в доте я играю на победу

-   rel02 --- Я часто играю в доту фо фан - мне интересен сам процесс игры (пробую разные сборки, новых героев, связки и тд)

-   rel03 --- Если мне откровенно руинят игру тиммейты, я могу заруинить им в ответ

-   rel04 --- Я могу ливнуть из игры/встать в амулет, если что-то идет не так

-   rel05 --- У меня есть конкретная цель - апнуть птсы (например: взять 6к mmr или медаль властелина/дивайна/титана)

-   rel06 --- Я играю в доту для того, чтобы побеждать

-   rel07 --- В доте слишком многое зависит не от меня и моей игры

-   rel08 --- Если я проигрываю, то это чаще всего связано с плохой игрой тиммейтов

-   rel09 --- Если бы у меня была сильная команда в каждой игре, я бы почти не проигрывал

-   rel10 --- Мой возможный птс намного больше, чем мой актуальный

-   rel11 --- Я долгое время сижу на одном и том же рейтинге без видимого прогресса

-   rel12 --- У меня часто пропадает желание играть, когда мои тиммейты играют плохо

-   rel13 --- MMR - это всего лишь цифра, она не показывает реальный скилл игрока

-   heroes_3 --- Напиши трех героев, на которых у тебя сыграно больше всего игр, количество и винрейт на этих персонажах (например: Пудж, 152 игры, 56%):

-   a_g --- Возрастная группа (я перешёл к порядковой шкале, потому что гипотезы были связаны с влиянием возрастной группы именно)

-   pas_total --- итоговый бал по шкале вовлечённости

## Game_addiciton_main

### В двух словах

В этом файле я тестирую основные гипотезы + делаю обзор данных. Основные гипотезы проверяю линейными реггрессиями + потом смотрю попарное сравнение

### Эксплаторный анализ

Смотрю распредления переменных зависимых и независимых. Строю графики красивые

### Анализ основных гипотез

Пробрую разные линейные модельки, проверяю допущения, строю графики, провожу попарные сравнения. Потом ещё оформляю таблички по Apa. Считаю мощность итоговую

### Итоги

Первые две гипотезы подтвердились, вторые две нет

## Game_addiction_additional

### В двух словах

Разбил на классы, использовал классы, чтобы посмотреть отличаются ли они по возрасту (гипотеза об "агро-школьниках") и по некоторым игровым особенностям

### Кластеризация

Использовал LCA. Перевёл ответы от шкалы в 5 баллов в 3-ёхбальную, чтобы потом проще было интерпретировать

### Байесовская анова

Решил потренироваться в Байесовской статистике + сравнил с результатами фреквентисткой ановы

## images

Все графики, которые создаются в ходе работы

## data-tables

Все csvшки (начальная и порождённые)

## apa-tables

Таблицы с итогами статистических тестов, оформелнные по APA
