[![Travis-CI Build Status](https://travis-ci.org/bdemeshev/cbr.svg?branch=master)](https://travis-ci.org/bdemeshev/cbr)

# cbr
Пакет `cbr` предназначен для скачивания временных рядов с сайта Центробанка России [cbr.ru](http://cbr.ru/). Пакет можно установить командами:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/cbr")
```
Может потребоваться ручная установка пакета `SSOAP`:
```r
install.packages('SSOAP', repos = 'http://www.omegahat.org/R',   dependencies = TRUE, type = 'source')
```
Если ссылка не работает, попробуйте версию 0.9:
```r
install_github("bdemeshev/SSOAP")
```
Возможно также потребуется пакет `XMLSchema`:
```r
install_github("omegahat/XMLSchema")
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


# cbr
The R-package `cbr` is designed for downloading data from [cbr.ru](http://cbr.ru/). The package may be installed with two commands:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/cbr")
```
Probably one should manually install `SSOAP` R-package:
```r
install.packages('SSOAP', repos = 'http://www.omegahat.org/R',   dependencies = TRUE, type = 'source')
```
If the link is not working, try
```r
install_github("bdemeshev/SSOAP")
```
Also install `XMLSchema`:
```r
install_github("omegahat/XMLSchema")
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

