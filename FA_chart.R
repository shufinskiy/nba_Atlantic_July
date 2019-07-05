library(data.table)
library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)

## Загрузка данных
fa <- fread("./data/FA_PIPM.csv")
pipm <- fread("./data/2018-19 PIPM & Multi-Year PIPM - 2018-19 PIPM.csv")
color <- fread("./data/Team_color.csv")

## Удаление лишних столбцов из таблицы pipm
pipm <- pipm[, .(Player, PIPM)]

## Объединение таблиц и изменение значений NA на 0
table <- merge(fa, pipm, by = "Player", all.x = T)
table[is.na(table)] <- 0

## Сворачивание таблицы из "long" в "wild", объединение столбцов и удаление элементов NA
test <- dcast(table, Team ~ Player, value.var = "Player")
test <- test[, .(Team, Player = do.call(paste, c(.SD, sep = ", "))), .SDcols = 2:45][
  , Player := str_remove_all(Player, "(NA, )|(, NA)")]

## Суммирование стоимости контрактов и показателя PIPM по командам
table1 <- table[, .(Money = sum(Money),
                    PIPM = sum(PIPM)), by = Team]

## Объединение таблиц и сортировка 
table1 <- merge(table1, test, by = "Team")
table1 <- table1[order(Money, decreasing = T)]

## Создание символьного вектора с названием команд с порядке убывания их затрат
factor <- table1[order(Money, decreasing = T)][,Team]

## Объединение таблицы с данными и таблицы с цветом, сортировка по убыванию
table_color <- merge(table1, color, by.x = "Team", by.y = "TEAM_ABBREVIATION")
table_color <- table_color[, .(Team, col1, col2)]
table_color <- merge(table_color, table1)
table_color <- table_color[order(Money, decreasing = T)]

## Изменение типа данных столбца Team на фактор.
table2 <- table[, Team := factor(Team, levels = factor)]

## Создание графика
gg <- ggplot(table2, aes(x = Team, y = Money)) +
  geom_bar(stat = "identity", aes(fill = Player), color = "black") +
  scale_fill_viridis_d() +
  theme_tufte()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 

## Создание темы для таблицы. Добавление цвета заливки и текста
theme <- ttheme_minimal(
  core = list(bg_params = list(fill = table_color$col1, col = "white"),
              fg_params = list(col = table_color$col2, cex = 0.8))
)

## Создание таблицы
gtable <- tableGrob(table1, theme = theme)

## Объединение графика и таблицы, добавление заголовка и подписи.
output <- grid.arrange(gg, gtable, nrow = 1, 
                       top = textGrob("Free Agency 2019, Day 1. Only UFA and RFA.\nAmount of monetary obligations and PIPM in last season by teams",
                                      hjust = 0.5),
                       bottom = textGrob("Sources: @wojespn, @ShamsCharania; Telegram: @NBAatlantic, Twitter: @vshufinskiy"))

## Созранение объекта
ggsave("FA.jpeg", output, width = 12, height = 8, unit = "in")
