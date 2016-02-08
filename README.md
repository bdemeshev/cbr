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
