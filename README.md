
## Проекты, созданные для Телеграм-канала NBAatlantic в июне-июле

### Анализ количества наград у бывших Rookie of the Year
Анализ "успешности" карьеры бывших новичков года в плане наград в дальнейшем. Выбраны три их вида: поездка на All-Star, попадание в All-NBA команды и титул MVP.

```r
library(data.table)
library(tidyverse)
library(ggthemes)
dt <- fread("./data/ROY.csv")
```

Точное количество наград я заменяю категориальной переменной к тремя значениями: None, Single, Multi. Замену првоодится с помощью функции ```replase_func```.

```r
dt1 <- copy(dt)
dt1 <- dt1[, Player := str_remove(Player, "\\\\[:alnum:]{1,}")]
```

|Season	|Player|	All_Star|	All_NBA|	MVP|
|---|---|---|---|---|
|2017-18	|Ben Simmons01|	1|	0|	0|
|2016-17	|Malcolm Brogdon01|	0|	0|	0|
|2015-16	|Karl-Anthony Towns01|	2|	1|	0|
|2014-15	|Andrew Wiggins01|	0|	0|	0|
|2013-14	|Michael Carter-Williams01|	0|	0|	0|
|2012-13	|Damian Lillard01|	4|	4|	0|
|2011-12	|Kyrie Irving01|	6|	2|	0|
|2010-11	|Blake Griffin01|	6|	5|	0|
|2009-10	|Tyreke Evans01|	0|	0|	0|
|2008-09	|Derrick Rose01|	3|	1|	1|

```r
## Функция для замены числовых значений категориальными
replase_func <- function(x){
  if_else(x == 0, "None",
          if_else(x == 1, "Single",
                  if_else(x > 1, "Multi", "NA")))
}
cols <- colnames(dt1[, 3:5])
dt1 <- dt1[, (cols) := lapply(.SD, replase_func), .SDcols = cols]
```

|Season|	Player|	All_Star|	All_NBA|	MVP|
|---|---|---|---|---|
|2017-18|	Ben Simmons|	Single|	None|	None|
|2016-17|	Malcolm Brogdon|	None|	None|	None|
|2015-16|	Karl-Anthony Towns|	Multi|	Single|	None|
|2014-15|	Andrew Wiggins|	None|	None|	None|
|2013-14|	Michael Carter-Williams|	None|	None|	None|

Изменение вида данных с помощью функции ```melt```
```r
dt1 <- melt(dt1, measure.vars = cols, variable.name = "Awards", value.name = "Count")
dt1 <- dt1[, Count := factor(Count, levels = c("Multi", "Single", "None"))]
```

|Season|	Player|	Awards|	Count|
|---|---|---|---|
|2017-18|	Ben Simmons|	All_Star|	Single|
|2016-17|	Malcolm Brogdon|	All_Star|	None|
|2015-16|	Karl-Anthony Towns|	All_Star|	Multi|
|2014-15|	Andrew Wiggins|	All_Star|	None|
|2013-14|	Michael Carter-Williams|	All_Star|	None|

Построение графика

```r
ggplot(dt1, aes(x = Season, y = Awards, fill = Count)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis_d(option = "viridis") +
  coord_equal() +
  theme_tufte() +
  labs(title = "The number of awards to former ROY",
       subtitle = "* 1994-1995 Jason Kidd, 1999-00 Elton Brand",
       caption = "Sourse: basketball-reference\n Telegram: @NBAatlantic, twitter: @vshufinskiy") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(size = 8),
        axis.ticks = element_blank(),
        legend.title = element_blank())
```
![](ROY.jpeg)
### Анализ стоимости и продуктивности игроков, подписанных в первый день рынка свободных агентов.

```r
library(data.table)
library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)

fa <- fread("F:/NBA_20191705/Excel/BBindex/FA_PIPM.csv")
pipm <- fread("C:/Users/1170201i3/Desktop/2018-19 PIPM & Multi-Year PIPM - 2018-19 PIPM.csv")
color <- fread("F:/NBA_20191705/Excel/Team_color.csv")
```

```r
## Удаление лишних столбцов из таблицы pipm
pipm <- pipm[, .(Player, PIPM)]
## Объединение таблиц и изменение значений NA на 0
table <- merge(fa, pipm, by = "Player", all.x = T)
table[is.na(table)] <- 0
```

Сворачивание таблицы из "long" в "wild", объединение столбцов и удаление элементов NA.

```r
test <- dcast(table, Team ~ Player, value.var = "Player")
test <- test[, .(Team, Player = do.call(paste, c(.SD, sep = ", "))), .SDcols = 2:45][
  , Player := str_remove_all(Player, "(NA, )|(, NA)")]
```

|Team|	Player|
|---|---|
|BKN|	DeAndre Jordan, Garrett Temple, Kevin Durant, Kyrie Irving|
|BOS|	Enes Kanter, Kemba Walker|
|CHA|	NA|
|CHI|	NA|
|DAL|	JJ Barea, Kristaps Porzingis, Maxi Kleber|

Суммирование стоимости контрактов и показателя PIPM по командам и объединение с таблицей test.
```r
table1 <- table[, .(Money = sum(Money),
                    PIPM = sum(PIPM)), by = Team]
table1 <- merge(table1, test, by = "Team")
table1 <- table1[order(Money, decreasing = T)]
```

|Team|	Money|	PIPM|	Player|
|---|---|---|---|
|BKN|	356.0|	7.95|	DeAndre Jordan, Garrett Temple, Kevin Durant, Kyrie Irving|
|GSW|	322.0|	0.44|	D'Angelo Russell, Kevon Looney, Klay Thompson|
|PHI|	300.8|	1.02|	Al Horford, Kyle O'Quinn|
|MIL|	270.6|	2.37|	Brook Lopez, George Hill, Khris Middleton|
|DAL|	227.4|	-0.63|	JJ Barea, Kristaps Porzingis, Maxi Kleber|

```r
## Создание символьного вектора с названием команд с порядке убывания их затрат
factor <- table1[order(Money, decreasing = T)][,Team]
## Объединение таблицы с данными и таблицы с цветом, сортировка по убыванию
table_color <- merge(table1, color, by.x = "Team", by.y = "TEAM_ABBREVIATION")
table_color <- table_color[, .(Team, col1, col2)]
table_color <- merge(table_color, table1)
table_color <- table_color[order(Money, decreasing = T)]
## Изменение типа данных столбца Team на фактор.
table2 <- table[, Team := factor(Team, levels = factor)]
```
Создание графика и таблицы и их объединение с помощью функции ```grid.arrange```

```r
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
```

Конечный график
![](FA.jpeg)

### Анализ обмена Нового Орлеана и Атланты.
### Сравение показателей WS и VORP для пиков, участвовавших в обмене за первые 7 лет карьеры в НБА.

Получаем суммы показателей WS и VORP в первые 7 лет после драфта для игроков, задрафтованных в период с 1976 по 2011 год с помощью созданной функции ```seven_first_years```. Код функции находится в файле `seven_first_years.R`

```r
library(tidyverse)
library(data.table)
library(ggthemes)
library(gridExtra)
```

```r
source("seven_first_years.R")
```

Подсчёт суммы показателей для 8, 17 и 35 пиков вместе. Сравнение их с показателями 4 пика.

```r
dt <- table[Pk == 4 | Pk == 8 | Pk == 17 | Pk == 35][
  , Pk := as.character(Pk)][Pk == "8" | Pk == "17" | Pk == "35", Pk := "sum(8, 17, 35)"][
    , .(WS_ch = sum(WS),
               VORP_ch = sum(VORP)), by =.(Pk, Year)]

dt1 <- dcast(dt, Year ~ Pk, value.var = c("WS_ch", "VORP_ch"))
dt2 <- dt1[, .(Log = WS_ch_4 >`WS_ch_sum(8, 17, 35)`)][, .(.N), by = Log]
dt3 <- dt1[, .(Log = VORP_ch_4 >`VORP_ch_sum(8, 17, 35)`)][, .(.N), by = Log]
```

table

|Player| Year| Rd| Pk|  Tm|   WS| VORP|
|---|---|---|---|---|---|---|
|Adrian Dantley| 1976|  1|  6| BUF| 68.7| 25.2|
|Al Fleming| 1976|  2| 30| PHO|  0.0| -0.1|
|Alex English| 1976|  2| 23| MIL| 45.6| 19.2|
|Armond Hill| 1976|  1|  9| ATL| 11.2| -0.9|
|Barnes Hauptfuhrer| 1976|  3| 44| HOU|  0.0|  0.0|

dt

|Pk| Year| WS_ch| VORP_ch|
|---|---|---|---|
|sum(8, 17, 35)| 1976|  79.1|    28.4|
|4| 1976|  15.1|     1.1|
|sum(8, 17, 35)| 1977|  77.5|    19.0|
|4| 1977|  42.6    16.8|
|sum(8, 17, 35)| 1978|   9.8|    -2.6|
|4| 1978|  31.8|    23.6|

dt2

|Log|  N|
|---|---|
|FALSE| 22|
|TRUE| 14|

Подсчёт средних показателей для каждого пика

```{r}
table_mean <- table[Pk == 4 | Pk == 8 | Pk == 17 | Pk == 35][
  ,.(WS_mean = round(mean(WS), digits = 2),
     VORP_mean = round(mean(VORP), digits = 2)) , by = Pk][order(Pk)]
```

Создание графиков 4 пиков с показателями WS и VORP для каждого выбора.

```{r, eval=FALSE}
cclist <- lapply(c(4, 8, 17, 35), function(pick){
  mean <- table[Pk == pick][,.(WS_mean = round(mean(WS), digits = 2),
                              VORP_mean = round(mean(VORP), digits = 2)), by = Pk][order(Pk)]
  ggplot(table[Pk == pick], aes(x = Year, y = WS)) +
    geom_point(aes(color = "WS")) +
    geom_segment(aes(xend = Year), yend=0, linetype = "dotted") +
    expand_limits(y=0) +
    geom_point(aes(x = Year, y = VORP,  color = "VORP")) +
    scale_colour_manual(name = 'Value', 
                        guide = 'legend',
                        values = c('WS' = 'green',
                                   'VORP' = 'red'), 
                        labels = c('VORP',
                                   'WS')) +
    geom_text(aes(x = Year, y = WS, label = WS), nudge_y = 2, size  = 2) +
    geom_text(aes(x = Year, y = VORP, label = VORP), nudge_y = -2,
              size  = 2, color = "red") +
    theme_tufte() +
    scale_x_continuous(breaks = seq(1976, 2011)) +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste0("The values of WS and VORP for ", pick, "th pick of the NBA draft\nin first 7 years of his career."),
         subtitle = paste0("Average WS for the ", pick, "th pick - ", mean[[2]],
                           "\nAverage VORP for the ", pick, "th pick - ", mean[[3]]),
         caption = "Sourse: basketball-reference\n Telegram: @NBAatlantic, twitter: @vshufinskiy")
})
cclist[["ncol"]] <- 1
  
output <- do.call(grid.arrange, cclist)
```

Результат
![](draft_plot.jpeg)

Мои работы можно посмотреть в Телеграм-канале [NBAatlantic](https://t.me/nbaatlantic)
