### download direct from the ONS price quotes data:

source("data-raw/code/03_load_packages.R")

# 320206 - Hand rolling tobacco pack 30gm

id <- 320206

#################
### 2019 ########

files <- c("https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesseptember2019/upload-pricequotes201909.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesoctober2019/upload-pricequotes201910.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesnovember2019/upload-pricequotes201911.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesdecember2019/upload-pricequotes201912v1.csv")

for (i in 1:length(files)){

  m <- i + 8
  y <- 2019

  temp <- tempfile()
  url <- files[i]
  temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

  prices <- read.csv(temp)

  setDT(prices)

  prices <- prices[ITEM_ID == id,]

  prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

  prices[, year := y]
  prices[, month := m]

  if (i == 1){
    ave_price_hrt_30g <- copy(prices)
  } else {
    ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))
  }


}


#################
### 2020 ########

y <- 2020

files <- c("https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricesquotesjanuary2020/upload-pricequotes202001.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricesquotesfebruary2020/upload-pricequotes202002.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricesquotesmarch2020/upload-pricequotes202003.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricesquotesapril2020/upload-202004pricequotes.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricesquotesmay2020/upload-202005pricequotes.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjune2020/upload-pricequotes202006.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricesquotesjuly2020/upload-pricequotes202007.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesaugust2020/upload-pricequotes202008.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesseptember2020/upload-pricequotes202009.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesoctober2020/upload-pricequotes202010.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesnovember2020/upload-pricequotes202011.csv",
           "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesdecember2020/upload-pricequotes202012.csv")


### JAN

temp <- tempfile()
url <- files[1]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 1]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### FEB

temp <- tempfile()
url <- files[2]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 2]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAR

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricesquotesmarch2020/upload-pricequotes202003.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 3]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### APR

temp <- tempfile()
url <- files[4]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 4]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAY

temp <- tempfile()
url <- files[5]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 5]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### JUNE

temp <- tempfile()
url <- files[6]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 6]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### JULY

temp <- tempfile()
url <- files[7]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 7]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### AUG

temp <- tempfile()
url <- files[8]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 8]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### SEPT

temp <- tempfile()
url <- files[9]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 9]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### OCT

temp <- tempfile()
url <- files[10]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 10]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### NOV

temp <- tempfile()
url <- files[11]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 11]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### DEC

temp <- tempfile()
url <- files[12]
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2020]
prices[, month := 12]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))



#################
### 2021 ########

y <- 2021

### JAN

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjanuary2021/upload-pricequotes202101.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 1]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### FEB

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesfebruary2021/upload-pricequotes202102.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 2]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAR

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesmarch2021/upload-pricequotes202103.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 3]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### APR

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesapril2021/upload-pricequotes202104.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 4]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAY

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesmay2021/upload-pricequotes2021051.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 5]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### JUNE

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjune2021/upload-pricequotes202106.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 6]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### JULY

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjuly2021/upload-pricequotes202107.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 7]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### AUG

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesaugust2021/upload-pricequotes202108.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 8]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### SEPT

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesseptember2021/upload-pricequotes202109.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 9]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### OCT

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesoctober2021/upload-pricequotes202110.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 10]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### NOV

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesnovember2021/upload-pricequotes202111.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 11]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### DEC

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesdecember2021/upload-pricequotes202112.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2021]
prices[, month := 12]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))



#################
### 2022 ########

y <- 2022

### JAN

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjanuary2022/upload-pricequotes202201.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 1]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### FEB

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesfebruary2022/upload-pricequotes202202.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 2]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAR

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesmarch2022/upload-pricequotes202203.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 3]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### APR

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesapril2022/upload-pricequotes202204.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 4]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAY

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesmay2022/upload-pricequotes202205.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 5]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### JUNE

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjune2022/upload-pricequotes202206.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 6]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### JULY

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjuly2022/upload-pricequotes202207.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 7]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### AUG

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesaugust2022/upload-pricequotes202208.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 8]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### SEPT

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesseptember2022/upload-pricequotes202209.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 9]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### OCT

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesoctober2022/upload-pricequotes202210.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 10]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### NOV

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesnovember2022/upload-pricequotes202211.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 11]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### DEC

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesdecember2022/upload-pricequotes202212.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2022]
prices[, month := 12]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))


#################
### 2023 ########

y <- 2023

### JAN

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjanuary2023/upload-pricequotes202301.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 1]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### FEB

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesfebruary2023/upload-pricequotes202302.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 2]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAR

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesmarch2023/upload-pricequotes202303.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 3]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### APR

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesapril2023/upload-pricequotes202304.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 4]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAY

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesmay2023/upload-pricequotes202305.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 5]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### JUNE

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjune2023/upload-pricequotes202306.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 6]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### JULY

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjuly2023/upload-pricequotes202307.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 7]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### AUG

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesaugust2023/upload-pricequotes202308.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 8]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### SEPT

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesseptember2023/upload-pricequotes202309.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 9]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### OCT

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesoctober2023/upload-pricequotes202310.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 10]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### NOV

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesnovember2023/upload-pricequotes202311.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 11]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### DEC

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesdecember2023/upload-pricequotes202312.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2023]
prices[, month := 12]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))


#################
### 2024 ########

y <- 2024

### JAN

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjanuary2024/upload-pricequotes202401.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2024]
prices[, month := 1]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### FEB

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesfebruary2024/upload-pricequotes202402.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2024]
prices[, month := 2]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAR

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesmarch2024/upload-pricequotes202403.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2024]
prices[, month := 3]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### APR

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesapril2024/upload-pricequotes202404.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2024]
prices[, month := 4]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

### MAY

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesmay2024/upload-pricequotes202405.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

prices <- read.csv(temp)

setDT(prices)

prices <- prices[ITEM_ID == id,]

prices <- prices[, .(price_hrt_30g = weighted.mean(PRICE, w = SHOP_WEIGHT))]

prices[, year := 2024]
prices[, month := 5]

ave_price_hrt_30g <- rbindlist(list(ave_price_hrt_30g, prices))

ave_price_hrt_30g <- ave_price_hrt_30g[, .(price_hrt_30g_1 = mean(price_hrt_30g)), by = "year"]

write.csv(ave_price_hrt_30g,"data-raw/data/Tobacco_Price_HRT_30g.csv")

####################################################
###### STAPM HRT PRICE DATA ########################

### Use CPI inflation series CPI Index 02.2.0.3 Other tobacco products

hrt_price_cpi <- read_xls(path = "data-raw/data/Tobacco_Price_HRT_CPI_Index.xls",
                          range = "A9:B17",
                          col_names = F) %>% setDT
setnames(hrt_price_cpi, names(hrt_price_cpi), c("year","cpi"))

index_2018 <- 1 + ((as.numeric(hrt_price_cpi[year == 2018, "cpi"]) - as.numeric(hrt_price_cpi[year == 2017, "cpi"]))/as.numeric(hrt_price_cpi[year == 2018, "cpi"]))
index_2019 <- 1 + ((as.numeric(hrt_price_cpi[year == 2019, "cpi"]) - as.numeric(hrt_price_cpi[year == 2018, "cpi"]))/as.numeric(hrt_price_cpi[year == 2019, "cpi"]))
index_2020 <- 1 + ((as.numeric(hrt_price_cpi[year == 2020, "cpi"]) - as.numeric(hrt_price_cpi[year == 2019, "cpi"]))/as.numeric(hrt_price_cpi[year == 2020, "cpi"]))

index_2021 <- 1 + ((as.numeric(hrt_price_cpi[year == 2021, "cpi"]) - as.numeric(hrt_price_cpi[year == 2020, "cpi"]))/as.numeric(hrt_price_cpi[year == 2020, "cpi"]))
index_2022 <- 1 + ((as.numeric(hrt_price_cpi[year == 2022, "cpi"]) - as.numeric(hrt_price_cpi[year == 2021, "cpi"]))/as.numeric(hrt_price_cpi[year == 2021, "cpi"]))
index_2023 <- 1 + ((as.numeric(hrt_price_cpi[year == 2023, "cpi"]) - as.numeric(hrt_price_cpi[year == 2022, "cpi"]))/as.numeric(hrt_price_cpi[year == 2022, "cpi"]))

#### (v) STAPM TAX-sim 2.5.0
####
#### STAPM modelling control arm B price for 2017, then uprated using the cigarette CPI
#### The mean price of HRT in the STAPM model is
#### 0.163891207067166 per stick (0.5g) = 9.83 per 30g pack

ave_price_hrt_30g <- data.table(year = 2017:2023,
                                price_hrt_30g_2 = c(9.83,
                                                    9.83*index_2018,
                                                    9.83*index_2018*index_2019,
                                                    9.83*index_2018*index_2019*index_2020,
                                                    9.83*index_2018*index_2019*index_2020*index_2021,
                                                    9.83*index_2018*index_2019*index_2020*index_2021*index_2022,
                                                    9.83*index_2018*index_2019*index_2020*index_2021*index_2022*index_2023))


write.csv(ave_price_hrt_30g,"data-raw/data/Tobacco_Price_HRT_STAPM_30g.csv")




