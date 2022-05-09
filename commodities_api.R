library(httr)
library(jsonlite)


updateAPI <- function(key = NULL, commodities = NULL) {
  
  if(is.null(key))
    key <- "2d0ppa2gl3wy607t7w5wn1699y3g4ycf1cas0u4b5q8h69x1730xxv9k3jhk"
  else
    key = key
  
  if(is.null(commodities))
    commodities <- c("AFN,ALL,DZD,ALU,AOA,ARS,AMD,AWG,AUD,AZN,BSD,BHD,BDT,BBD,BYN,BYR,BZD,BMD,BTC,BCH,BOB,
                 BAM,BWP,BRL,BRENTOIL,GBP,BND,BGN,BIF,KHR,CAD,CVE,ADA,KYD,XAF,XPF,LINK,CLP,CNY,CNH,LCO,
                 COFFEE,COP,KMF,CDF,XCU,CORN,CRC,COTTON,HRK,CPO,CUC,CZK,DKK,DJF,DOP,XCD,EGP,ERN,EEK,ETHANOL,
                 ETH,ETB,EUR,FKP,FJD,GMD,GEL,GHS,GIP,XAU,GTQ,GGP,GYD,HTG,HNL,HKD,HUF,ISK,INR,IDR,IQD,IRD,IMP,
                 ILS,JMD,JPY,JEP,JOD,KZT,KES,KWD,KGS,LAK,LVL,LBP,LSL,LRD,LYD,LTC,LTL,LUMBER,MOP,MKD,MGA,MWK,MYR,
                     MVR,MTL,MRO,MUR,MXN,MDL,MNT,MAD,MZN,MMK,NAD,NG,NPR,ANG,TWD,NZD,NIO,NI,NGN,NOK,OMR,PKR,XPD,
                     PAB,PGK,PYG,PEN,PHP,XPT,PLN,QAR,XRH,RICE,XRP,,XRP2,RON,RUBBER,RUB,RUTH,RWF,SHP,SVC,WST,
                     STD,SAR,RSD,SCR,SLL,XAG,SGD,SBD,SOS,ZAR,KRW,SSP,SOYBEAN,XDR,LKR,XLM,SUGAR,SRD,SZL,SEK,
                     CHF,TJS,TZS,THB,TIN,TOP,TTD,TND,TRY,TMT,UGX,UAH,CLF,UNI,AED,USD,UYU,UZS,VUV,VEF,VES,
                     VND,XOF,WHEAT,WTIOIL,YER,ZMK,ZMW,ZWL,ZNC")
  else
    commodities = commodities
  
  getreq <- GET(paste0("https://commodities-api.com/api/latest?access_key=",
                       key,
                       "&base=USD"))
  
  
  #convert raw unicode to JSON and
  list <- rawToChar(getreq$content) |> 
    fromJSON()
  
  return(list)
}


