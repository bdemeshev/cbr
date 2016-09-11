[![Travis-CI Build Status](https://travis-ci.org/bdemeshev/cbr.svg?branch=master)](https://travis-ci.org/bdemeshev/cbr)

# cbr
Пакет `cbr` предназначен для скачивания временных рядов с сайта Центробанка России [cbr.ru](http://cbr.ru/). Пакет можно установить командами:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/cbr")
```

Для новичков в R: Пакеты устанавливаются один раз, и каждый раз для скачанивания рядов выполнять эти команды совершенно ненужно :)


Пример простого использования. Скачаем котировки бурундийского франка!
```r
library("cbr")
df <- cbr_currency(currency = 'R01120', 
  from = '1993-01-05', to = '2013-01-09')
```

Цены на металлы:
```r
df <- cbr_metal(from = '2012-01-01', to = '2013-01-09')
```

Свопы:
```r
df <- cbr_swap(from = '2012-01-01', to = '2013-01-09')
```


- получения динамики котировок драгоценных металлов `cbr_metal(from = , to = )`
- получения динамики ставок межбанковского рынка `cbr_credit_rates(from = , to = )`
- получения динамики ставок "валютный своп" `cbr_swap(from = , to = )`
- получения динамики отпускных цен Банка России на инвестиционные монеты `cbr_coins(from = , to = )`

## Веб-сервис получения информации по рынку ценных бумаг

Посмотрим на вспомогательный датафрейм, в котором содержится информация о всех переменных рынка ценных бумаг с которыми работает пакет `cbr`:
```r 
data("cbr_requests")
cbr_requests
```


Давайте посмотрим на данные сессии прямого РЕПО по фиксированной ставке (RepoSessionXML):
```r
repo <- cbr_security_info("RepoSessionXML")
head(repo)
```

Есть очень короткое введение в пакет [в виде виньетки](http://htmlpreview.github.io/?https://github.com/bdemeshev/cbr/blob/master/inst/doc/cbr_intro.html).

тудушки:

- [ ] табличка с кодами валют
- [ ] другие XML с ЦБ
- [ ] описание переменных в cbr_requests
- [ ] стандартизировать body XML запросов
- [ ] убрать зависимость от plyr? один вызов
- [ ] XML функции стандартизировать под одну
- [ ] преобразование типов переменных!!!!
- [ ] встроенные набора данных для демо-целей


# English translation cbr

The R-package `cbr` is designed for downloading data from [cbr.ru](http://cbr.ru/). The package may be installed with two commands:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/cbr")
```


Let's downloads exchange rate for burundi frank :)
```r
library("cbr")
df <- cbr_currency(currency = 'R01120', 
  from = '1993-01-05', to = '2013-01-09')
```

Metal prices:
```r
df <- cbr_metal(from = '2012-01-01', to = '2013-01-09')
```

Swaps:
```r
df <- cbr_swap(from = '2012-01-01', to = '2013-01-09')
```

