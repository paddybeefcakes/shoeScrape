#------------------#
# R Options Config #
#------------------#

options(
  'pkgType' = 'mac.binary.el-capitan',
  'install.packages.check.source' = 'no',
  'install.packages.compile.from.source' = 'never'
)

#----------------#
# Load Lobraries #
#----------------#

library(RSelenium)
library(rvest)
library(tidyverse)
library(furrr)

#-----------------#
# Future Exe Plan #
#-----------------#

plan(list(tweak(multiprocess, workers = 4L), tweak(multiprocess, workers = 12L)))

#----------------------#
# Set Script Arguments #
#----------------------#

# datesList <- c('2018', '2017', '2016', '2015', '2014')
brandsList <- c('adidas', 'retro-jordans', 'nike', 'other-sneakers')

datesList <- c('2018')
# brandsList <- c('adidas')

basePage <- 'http://stockx.com'

# Make all combinations of years and brands.
combos <- expand.grid(brands = brandsList, dates = datesList, stringsAsFactors = FALSE)

# Build combos into named list for pmap
pmapArgs <- list(brands = combos[,'brands'], dates = combos[,'dates'])


#------------------#
# Execute Scrapper #
#------------------#

results <- #future_pmap(
  pmap(
  pmapArgs,
  safely({
  function(brands, dates){

  # Start Chrome for webscraping.
  remDr <- RSelenium::rsDriver(browser = 'chrome') # if needed

  remDr$client$navigate(sprintf("https://stockx.com/%s?years=%s", brands, dates))
  Sys.sleep(5)

  print(glue::glue("Brand: {brands}"))
  print(glue::glue("Date: {dates}"))

  loadMoreBtn <- try({
    remDr$client$findElement(using = 'css selector', ".browse-load-more > button")
  })

  numClick <- 0

  print("Clicking load more")
  loopTry <- loadMoreBtn$clickElement()

  while(class(loopTry) == "NULL"){
    print("Clicking load more")
    loopTry <- try({loadMoreBtn$clickElement()})
    Sys.sleep(2)
  }

  shoeURLS <- read_html(remDr$client$getPageSource()[[1]]) %>%
    html_nodes(".browse-grid:not(.loading) > a") %>%
    html_attr('href') %>%
    paste0(basePage, .)

  print(glue::glue("length: {length(shoeURLS)} \nYear: {dates} \nBrand: {brands}"))

  #----------------------------#
  # Start Anchor Tag List Loop #
  #----------------------------#

  shoeData <-
    future_map(
    # map(
      shoeURLS,
      safely({
        function(x){

          print(x)

          shoePage <- read_html(x)

          lastPrice <- shoePage %>%
            html_node(".last-sale > .sale-value") %>%
            html_text() %>%
            substr(2,nchar(.)) %>%
            as.numeric()

          ticker <- shoePage %>%
            html_nodes(".header-stat") %>%
            .[2] %>%
            html_text() %>%
            substr(9, nchar(.))

          shoeName <- shoePage %>%
            html_nodes('.name') %>%
            html_text()

          tradeVolmn <- shoePage %>%
            html_nodes('.last-sale-block > div > .sale-size > .size-container > .bid-ask-sizes') %>%
            html_text()

          shoeDesc <- shoePage %>%
            html_node(".product-description > p") %>%
            html_text()

          # hiLoPrice <- shoePage %>%
          #   html_nodes('.ft-high-low-col > .value-container > span') %>%
          #   html_text()

          # str_locate(test[1], "\\$")

          tradeRange <- shoePage %>%
            html_nodes('.ds-range-col.market-up') %>%
            .[[2]] %>%
            html_text()

          shoeVol <- shoePage %>%
            html_node("li.volatility-col.market-down > div > span") %>%
            html_text() %>%
            substr(1, nchar(.) - 1) %>%
            as.numeric()

          pricePrem <- shoePage %>%
            html_nodes('div.gauge-value') %>%
            html_text() %>%
            .[2] %>%
            substr(1, nchar(.) -1) %>%
            as.numeric()

          numberSales <- shoePage %>%
            html_nodes('div.gauge-value') %>%
            html_text() %>%
            .[1] %>%warnings
            as.numeric()

          shoeSize <- shoePage %>%
            html_nodes('.select-options > ul > li > .inset > .title') %>%
            html_text()

          sizePrice <- shoePage %>%
            html_nodes('.select-options > ul > li > .inset > .subtitle') %>%
            html_text() %>%
            substr(2, nchar(.))


          sizePriceTBL <- list(tibble(shoeSize = shoeSize,
                                      sizePrice = sizePrice))

          tibble(date = dates,
                 brand = brands,
                 lastPrice = lastPrice,
                 ticker   = ticker,
                 shoeName = shoeName,
                 tradeVolmn = tradeVolmn,
                 shoeDesc = shoeDesc,
                 tradeRange = tradeRange,
                 shoeVol = shoeVol,
                 pricePrem = pricePrem,
                 # numberSales = numberSales,
                 sizePriceTBL = sizePriceTBL
                 )
        }

      })

    ) %>%

    map_df(function(x){x$result})

}})) %>%
  map(function(x){x$result}) %>%
  set_names(paste0(pmapArgs[['brands']], '-' , pmapArgs[['dates']])) 

# results2 <- map(results,function(x){x[!duplicated(x),]})

saveRDS(results2, 'showData.Rds')

# data %>% reduce(function(x, y){bind_rows(x, y)}) %>% select(-sizePriceTBL)

write.csv(results2 %>% reduce(function(x, y){bind_rows(x, y)}) %>% select(-sizePriceTBL, -numberSales), 'shoe.csv')
