#' @importFrom abdiv shannon
#' @import dplyr
#' @import forecast
#' @import ggplot2
#' @importFrom kableExtra kbl
#' @importFrom kableExtra kable_material
#' @importFrom kableExtra column_spec
#' @importFrom KScorrect LcKS
#' @import lubridate
#' @importFrom lubridate isoweek
#' @importFrom stringr str_detect
#' @importFrom TSA McLeod.Li.test
#' @importFrom tidyr replace_na

require(dplyr)
require(ggplot2)
require(forecast)
require(tidyr)
require(TSA)
require(abdiv)
require(lubridate)
require(stringr)
require(KScorrect)
require(kableExtra)

#' Extracting values from forecast objects using OrniStatsData for use in ggplot2
#' 
#' @param fcast Object of type `forecast` from which the values are extracted
#' @return Data frame containing the point forecast, 50 and 80 percent confidence interval values
#' @export 

forecastExtract <- function(fcast){
  dflo <- data.frame(fcast$lower)
  dfhi <- data.frame(fcast$upper)
  dfcastn <- data.frame(fcast$mean, dflo$X50., dfhi$X50.,dflo$X80., dfhi$X80.)
  names(dfcastn)<- c('forecast','lo50', 'hi50', 'lo80', 'hi80')
  return(dfcastn)
}

#'Generate table of records of species in OrniStats data.
#'
#'@param qualifer Single or list of quoted 5-letter abbreviation(s) for the species whose records are being tabulated. To check which qualifiers you need, please consult the list in the GitHub repository for this package.
#'@param path File path to save the table. Default value is NA where tables are not saved.
#'@return The OrniStats table for the species inputted.
#'@export

recordTable <- function(qualifier, path = NA) {
  
  data("OrniStatsRecords", package="ornistats")
  
  subData = data.frame(subset(OrniStatsRecords, Qualifier %in% qualifier & "Year.of.Discovery" != 2022))
  table = subData[, c("Record.No.", "Recording.Area", "Site", "Date.of.Discovery", "Date.of.Departure",
                    "Stay.Length", "Age", "Sex")]
  table = table[order(table$Record.No., decreasing=T), ]
  output = table %>%
    kbl(align = 'c', col.names = c("Record No.", "Recording Area", "Site", "Discovery", "Departure",
                                   "Length of Stay (days)", "Age", "Sex"), row.names = FALSE,
        full_width = T) %>%
    kable_material(c("striped","bordered", "condensed", "responsive")) %>%
    column_spec(1, bold=T)
  
  show(output)
  
  var <- readline(prompt = "Save table (T/F)? ")
  var = as.logical(var)
    
  if(!is.na(path)) {
    if (isTRUE(var)) {
      save_kable(output, file = !!glue("{path}/{qualifier} Records Table"))
      message("Table saved")
    } else if (isFALSE(var)) {
      message("Function halted, table not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    if (isTRUE(var)) {
      stop("Path must be provided to save graph")
    }
  }
}

#' 10-year forecasts (ETS or ARIMA) for yearly frequencies of bird species in OrniStats data.
#' 
#' @param qualifer Single or list of quoted 5-letter abbreviation(s) for the species whose records are being analysed. To find out the qualifier for a species, please consult the list in the GitHub repository for this package.
#' @param path File path to save the figure. Default value is NA where graphs are not saved.
#' @return ggplot figure with historical annual frequencies, 10-year point forecast of chosen type in readline, and 50 and 80 percent confidence interval values.
#' @export

yearlyForecast <- function(qualifier, path=NA) {
  data("OrniStatsRecords", package="ornistats")
  
  subData = data.frame(subset(OrniStatsRecords, Qualifier %in% qualifier & 
                              !("Year.of.Discovery" %in% c(2022, 2023)) 
                              & Subset %in% c(NA, "A")))
  
  yearData = subData %>%
    group_by(Year.of.Discovery) %>%
    rename(discovery_year = Year.of.Discovery)%>%
    summarise(record_count = n_distinct(Record.No.))
  
  years = data.frame(discovery_year = 1950:2021)
  yearData = left_join(years, yearData, by = "discovery_year") %>%
    replace_na(list(record_count = 0))
  
  plot = ggplot(yearData, aes(as.numeric(discovery_year), record_count))+
    geom_bar(stat="identity",fill="red", color="black")+
    geom_smooth(se=FALSE, method="loess", color="black", linewidth=1.5)+
    theme_classic()+
    labs(x="Year", y="Number of records")+
    scale_y_continuous(expand = expansion(mult = c(0, .1)))+
    scale_x_continuous(expand = c(0,0))
  
  show(plot)
  
  if(!is.na(path)) {
    var <- readline(prompt = "Save figure (T/F)? ")
    var = as.logical(var)
    if (isTRUE(var)) {
      ggsave(plot, file = glue("{path}/{qualifier} Trend Plot.jpg"))
      message("Graph saved")
    } else if (isFALSE(var)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var <- readline(prompt = "Press enter to continue. ")
  }
  
  series = yearData$record_count
  train_data = series[1:60]
  test_data = series[61:72]
  
  etsFit = ets(train_data)
  show(checkresiduals(etsFit))
  
  var2 <- readline(prompt="Valid ETS residuals (T/F)? ")
  var2 = as.logical(var2)
  
  if (isFALSE(var2)) {
    message("Warning: Residuals indicate lack of fit. Printing accuracy statistics.")
  } else if (is.na(var2)) {
    stop("Require boolean input as prompt")
  }
  
  a2 = etsFit %>%
    forecast(h = 72-60) %>%
    accuracy(test_data)
  
  arimaFit = auto.arima(train_data)
  checkARCH = McLeod.Li.test(arimaFit, train_data)
  show(checkARCH)
  checkVariance = readline(prompt = "Evidence of heteroscedasticity (T/F)? ")
  checkVariance = as.logical(checkVariance)
  
  if (is.na(checkVariance)) {
    stop("Require boolean input as prompt")
  } else if (isTRUE(checkVariance)) {
    lambda <- BoxCox.lambda(series)
    series <- BoxCox(series, lambda)
    train_data = series[1:60]
    test_data = series[61:72]
    arimaFit = auto.arima(train_data)
  }
  
  a3 = arimaFit %>%
    forecast(h = 72-60) %>%
    accuracy(test_data)
  
  print(a2[, "RMSE"])
  print(a3[, "RMSE"])
  
  var3 <- readline(prompt="Which model are you selecting (ETS/ARIMA)? ")
  var3 = toString(var3)
                  
  if (var3 == "ETS") {
    forecast = forecast(etsFit, h=10, level = c(50,80))
    plotData = forecastExtract(forecast)
    plotData$date <- seq(2022,2031,1)
    plotData[plotData < 0] = 0
    
    forecastPlot <- ggplot(plotData, aes(as.numeric(date), forecast))+
      geom_line(data=yearData,aes(x=as.numeric(discovery_year),y=as.numeric(record_count)),color="red",linewidth=1.5)+
      geom_ribbon(aes(ymin=lo80, ymax=hi80,group=1), alpha=.50, fill='pink')+
      geom_ribbon(aes(ymin=lo50, ymax=hi50,group=1), alpha=.25, fill="#AA336A")+
      geom_line(aes(group=1), color='red', linewidth = 1.5)+
      theme_classic()+
      theme(legend.position = 'none')+
      labs(x="Year", y="Number of Records")+
      scale_y_continuous(expand = expansion(mult = c(0, .1)))+
      scale_x_continuous(expand = c(0,0))
    
    show(forecastPlot)
    if (isFALSE(var3)){
      message("Warning! Autocorrelation detected in the residuals. Proceed with caution.")
    }
    
    if(!is.na(path)) {
      var5 <- readline(prompt = "Save figure (T/F)? ")
      var5 = as.logical(var5)
      if (isTRUE(var5)) {
        ggsave(forecastPlot, file = glue("{path}/{qualifier} Forecast Plot.jpg"))
        message("Graph saved")
      } else if (isFALSE(var5)) {
        message("Function halted, graph not saved")
      } else {
        stop("Require boolean input as prompt")
      }
    } else {
      var5 <- readline(prompt = "Press enter to continue. ")
    }
  } else if (var3 == "ARIMA") {
      forecast = forecast(arimaFit, h=10, level = c(50,80))
      plotData = forecastExtract(forecast)
      if (isTRUE(checkVariance)) {
        fvar <- structure(list())
        for (i in colnames(plotData)) {
          plotData[[i]] <- InvBoxCox(plotData[[i]], lambda=lambda, biasadj = TRUE)
        }
      }
      plotData$date <- seq(2022,2031,1)
      plotData[plotData < 0] = 0
      
      forecastPlot <- ggplot(plotData, aes(as.numeric(date), forecast))+
        geom_line(data=yearData,aes(x=as.numeric(discovery_year),y=as.numeric(record_count)),color="red",linewidth=1.5)+
        geom_ribbon(aes(ymin=lo80, ymax=hi80,group=1), alpha=.50, fill='pink')+
        geom_ribbon(aes(ymin=lo50, ymax=hi50,group=1), alpha=.25, fill="#AA336A")+
        geom_line(aes(group=1), color='red', linewidth = 1.5)+
        theme_classic()+
        theme(legend.position = 'none')+
        labs(x="Year", y="Number of Records")+
        scale_y_continuous(expand = expansion(mult = c(0, .1)))+
        scale_x_continuous(expand = c(0,0))
      
    show(forecastPlot)
      
    if(!is.na(path)) {
      var5 <- readline(prompt = "Save figure (T/F)? ")
      var5 = as.logical(var5)
      if (isTRUE(var5)) {
        ggsave(forecastPlot, file = glue("{path}/{qualifier} Forecast Plot.jpg"))
        message("Graph saved")
      } else if (isFALSE(var5)) {
        message("Function halted, graph not saved")
      } else {
        stop("Require boolean input as prompt")
      }
    }
  }
}

#' Generate summary statistics for species or species groups in OrniStats data.
#' 
#' @param qualifier Single or list of quoted 5-letter abbreviation(s) for the species whose records are being analysed. To find out the qualifier for a species, please consult the list in the GitHub repository for this package.
#' @return Output data frame containing measures of variability, heterogeneity, fidelity and mortality for given species or species groups.
#' @export

speciesStats <- function(qualifier) {
  data("OrniStatsRecords", package="ornistats")
  data("OrniStatsLocations", package="ornistats")
  force(OrniStatsLocations)
  
  subData = data.frame(subset(OrniStatsRecords, Qualifier %in% qualifier & 
                                !("Year.of.Discovery" %in% c(2022,2023))))
  subData$record_unique = paste(subData$Record.No., subData$Subset)
  subData$Date.of.Discovery = as.Date(subData$Date.of.Discovery, format="%d/%m/%Y")
  subData$isoweek = isoweek(subData$Date.of.Discovery)
  
  Record.Count = n_distinct(subData$record_unique)
  Individual.Count = nrow(subData[subData$Subset %in% c("A",NA), ])
  Dead.Count = nrow(subData[!(subData$Death.Score %in% c(NA,5)), ])
  
  areaData = subData %>%
    group_by(Recording.Area) %>%
    summarise(count = n_distinct(record_unique))
  
  weeklyData = subData %>%
    subset(Subset %in% c(NA, 'A') & Approx.Date != 1) %>%
    group_by(isoweek) %>%
    summarise(count = n_distinct(Record.No.))
  
  print(weeklyData)
  
  yearData = subData %>%
    subset(Subset %in% c(NA, 'A')) %>%
    group_by(Year.of.Discovery) %>%
    rename(discovery_year = Year.of.Discovery)%>%
    summarise(record_count = n_distinct(Record.No.))
  years = data.frame(discovery_year = 1950:2021)
  yearData = left_join(years, yearData, by = "discovery_year") %>%
    replace_na(list(record_count = 0))
  
  homerangeData <- subData %>%
    mutate(Polygon = if_else(str_detect(Site, "/"), 1, 0))
  
  uniquehomeRanges <- homerangeData %>%
    filter(Polygon == 1) %>%
    distinct()
  
  relocationsData <- homerangeData %>%
    mutate(Site = ifelse(Polygon == 1, paste0("Home Range ", match(homerangeData$Site, uniquehomeRanges$Site)), Site)) %>%
    group_by(Record.No.) %>%
    summarise(numRelocations = n_distinct(Site) - 1)
  
  returnsList = c()
  
  for (num in 1:max(homerangeData$Record.No.)) {
    recordsSep <- subset(homerangeData, Record.No. == num)
    siteList <- c()
    returnCounter <- 0
    baseYear <- recordsSep$Year.of.Discovery[1]
    sites <- NULL
  
    for (i in 1:nrow(recordsSep)){
      year <- recordsSep$Year.of.Discovery[i]
      subsetValue <- recordsSep$Subset[i]
      site <- recordsSep$Site[i]
      
      if (length(subsetValue) > 0 && !is.na(subsetValue)) {
        if (grepl("/", site)) {
          sites <- unlist(strsplit(site, "/"))
        }
        
        for (loc in sites) {
          if (loc %in% siteList) {
            if (year != baseYear) {
              returnCounter <- returnCounter + 1
              baseYear <- year
              break
            }
          } else {
            siteList <- c(siteList, loc)
          }
        }
      }
    }
    returnsList[num] <- returnCounter
  }
  
  returnsData = data.frame(1:max(homerangeData$Record.No.), returnsList)
  
  mortalityRate = Dead.Count/Individual.Count
  HL = shannon(areaData$count)
  HT = shannon(weeklyData$count)
  Variability = sd(yearData$record_count)/mean(yearData$record_count)
  Irruptivity = shannon(yearData$record_count)
  CR = (length(relocationsData$numRelocations)/Individual.Count) * mean(relocationsData$numRelocations)
  CF = (length(returnsData$returnsList)/Individual.Count) * mean(returnsData$returnsList)
  
  Measurements = c("Mortality Rate", "HL", "HT", "Variability", "Irruptivity", "CR", "CF")
  Statistics = c(mortalityRate, HL, HT, Variability, Irruptivity, CR, CF)
  
  output = data.frame(Measurements, Statistics)
  print(output)
}

#' Generate the graphs for length of stay, arrival date and arrival location of species in OrniStats data.
#' 
#' @param qualifer Single or list of quoted 5-letter abbreviation(s) for the species whose records are being analysed. To find out the qualifier for a species, please consult the list in the GitHub repository for this package.
#' @param path File path to save the figure. Default value is `NA` where graphs are not saved.
#' @return ggplot figures summarising length of stay, arrival date and arrival location trends for the inputted species
#' @export

speciesPlots <- function(qualifier, path = NA) {
  data("OrniStatsRecords", package="ornistats")
  data("OrniStatsLocations", package="ornistats")
  
  subData = data.frame(subset(OrniStatsRecords, Qualifier %in% qualifier & Year.of.Discovery != 2023))
  OrniStatsLocations <- OrniStatsLocations %>%
    rename(Site = site)
  combinedData = left_join(subData, OrniStatsLocations, by = 'Site')
  combinedData$Date.of.Discovery <- as.Date(combinedData$Date.of.Discovery, format = "%d/%m/%Y")
  
  message("*** LENGTH OF STAY PLOTS ***")
  stayData <- subset(combinedData, "Approx.Date" != 1)
  
  stayCounts <- stayData %>%
    group_by(Stay.Length) %>%
    summarise(Frequency = n_distinct(Helper))
  stays = data.frame(Stay.Length = seq(1, max(stayCounts$Stay.Length), by=1))
  stayCounts = left_join(stays, stayCounts, by = 'Stay.Length') %>%
    replace_na(list(Frequency = 0))
  
  stayPlotGeneral <- ggplot(stayCounts, aes(Stay.Length, Frequency))+
    geom_bar(stat="identity", position="dodge", color="black", fill="red")+
    geom_smooth(se=FALSE, method="loess", color="black", linewidth=1.5)+
    theme_classic()+
    labs(x="Length of stay", y="Number of records")+
    scale_y_continuous(expand = expansion(mult = c(0, .1)))+
    scale_x_continuous(expand = c(0,0))
  
  show(stayPlotGeneral)
  
  if(!is.na(path)) {
    var <- readline(prompt = "Save figure (T/F)? ")
    var = as.logical(var)
    if (isTRUE(var)) {
      ggsave(plot, file = glue("{path}/{qualifier} Record Stays.jpg"))
      message("Graph saved")
    } else if (isFALSE(var)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var <- readline(prompt = "Press enter to continue. ")
  }
  
  staysList <- ifelse(stayCounts$Frequency == 0, 0.0001, stayCounts$Frequency)
  expDistTest <- LcKS(staysList, "pexp")
  print(paste("D value: ", expDistTest$D.obs, ", p-value: ", expDistTest$p.value))
  
  if (expDistTest$p.value <= 0.05) {
    times <- stayCounts$Stay.Length
    model <- lm(log(staysList) ~ times)
    decay_rate <- coef(model)[2]
    print(paste("Decay rate: ", abs(decay_rate)))
  } else {
    message("Data does not follow exponential decay distribution")
  }
  
  stayPlotYearly <- ggplot(stayData, aes(Year.of.Discovery, Stay.Length))+
    geom_point(color = "red", size=2)+
    geom_smooth(se=FALSE, method="loess", color="black", linewidth=1.5)+
    theme_classic()+
    labs(x="Year of Discovery", y="Length of Stay")
  
  show(stayPlotYearly)
  
  if(!is.na(path)) {
    var2 <- readline(prompt = "Save figure (T/F)? ")
    var2 = as.logical(var2)
    if (isTRUE(var2)) {
      ggsave(plot, file = glue("{path}/{qualifier} Record Stays Over Time.jpg"))
      message("Graph saved")
    } else if (isFALSE(var2)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var2 <- readline(prompt = "Press enter to continue. ")
  }
  
  stayPlotLat <- ggplot(stayData, aes(lat, Stay.Length))+
    geom_point(color = "red", size=2)+
    geom_smooth(se=FALSE, method="loess", color="black", linewidth=1.5)+
    theme_classic()+
    labs(x="Record Latitude", y="Length of Stay")
  
  show(stayPlotLat)
  
  if(!is.na(path)) {
    var3 <- readline(prompt = "Save figure (T/F)? ")
    var3 = as.logical(var3)
    if (isTRUE(var3)) {
      ggsave(plot, file = glue("{path}/{qualifier} Record Stays By Latitude.jpg"))
      message("Graph saved")
    } else if (isFALSE(var3)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var3 <- readline(prompt = "Press enter to continue. ")
  }
  
  stayPlotLong <- ggplot(stayData, aes(long, Stay.Length))+
    geom_point(color = "red", size=2)+
    geom_smooth(se=FALSE, method="loess", color="black", linewidth=1.5)+
    theme_classic()+
    labs(x="Record Longitude", y="Length of Stay")
  
  show(stayPlotLong)
  
  if(!is.na(path)) {
    var4 <- readline(prompt = "Save figure (T/F)? ")
    var4 = as.logical(var4)
    if (isTRUE(var4)) {
      ggsave(plot, file = glue("{path}/{qualifier} Record Stays By Longitude.jpg"))
      message("Graph saved")
    } else if (isFALSE(var4)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var4 <- readline(prompt = "Press enter to continue. ")
  }
  
  seasonalCombined <- combinedData
  seasonalCombined$Date.of.Discovery <- update(seasonalCombined$Date.of.Discovery, year=2023)
  seasonalCombined <- subset(seasonalCombined, "Approx.Date" != 1)
  
  stayPlotDate <- ggplot(seasonalCombined, aes(Date.of.Discovery, Stay.Length))+
    geom_point(color = "red", size=2)+
    theme_classic()+
    labs(x="Record Date", y="Length of Stay")+
    scale_x_date(date_labels = "%b")
  
  show(stayPlotDate)
  
  if(!is.na(path)) {
    var5 <- readline(prompt = "Save figure (T/F)? ")
    var5 = as.logical(var5)
    if (isTRUE(var5)) {
      ggsave(plot, file = glue("{path}/{qualifier} Record Stays By Season.jpg"))
      message("Graph saved")
    } else if (isFALSE(var5)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var5 <- readline(prompt = "Press enter to continue. ")
  }
  
  message("*** ARRIVAL DATE PLOTS ***")
  
  combinedData$yearday <- yday(combinedData$Date.of.Discovery)
  arrivalData <- subset(combinedData, !(is.na(Date.of.Discovery)))
  
  arrivalPlotYearly <- ggplot(arrivalData, aes(Year.of.Discovery, yday(Date.of.Discovery)))+
    geom_point(color = "red", size=2)+
    theme_classic()+
    labs(x="Year", y="Day of Year of Discovery")
  
  show(arrivalPlotYearly)
  
  if(!is.na(path)) {
    var6 <- readline(prompt = "Save figure (T/F)? ")
    var6 = as.logical(var6)
    if (isTRUE(var6)) {
      ggsave(plot, file = glue("{path}/{qualifier} Arrival Date over Time.jpg"))
      message("Graph saved")
    } else if (isFALSE(var6)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var6 <- readline(prompt = "Press enter to continue. ")
  }
  
  arrivalPlotLat <- ggplot(arrivalData, aes(yearday, lat))+
    geom_point(color = "red", size=2)+
    geom_smooth(se=FALSE, method="loess", color="black", linewidth=1.5)+
    theme_classic()+
    labs(x="Yearday", y="Record Latitude")
  
  show(arrivalPlotLat)
  
  if(!is.na(path)) {
    var7 <- readline(prompt = "Save figure (T/F)? ")
    var7 = as.logical(var7)
    if (isTRUE(var7)) {
      ggsave(plot, file = glue("{path}/{qualifier} Arrival Date By Latitude.jpg"))
      message("Graph saved")
    } else if (isFALSE(var7)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var7 <- readline(prompt = "Press enter to continue. ")
  }
  
  arrivalPlotLong <- ggplot(arrivalData, aes(yearday, abs(long)))+
    geom_point(color = "red", size=2)+
    geom_smooth(se=FALSE, method="loess", color="black", linewidth=1.5)+
    theme_classic()+
    labs(x="Yearday", y="Degrees west of Greenwich Meridian")
  
  show(arrivalPlotLong)
  
  if(!is.na(path)) {
    var8 <- readline(prompt = "Save figure (T/F)? ")
    var8 = as.logical(var8)
    if (isTRUE(var8)) {
      ggsave(plot, file = glue("{path}/{qualifier} Arrival Date By Longitude.jpg"))
      message("Graph saved")
    } else if (isFALSE(var8)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var8 <- readline(prompt = "Press enter to continue. ")
  }
  
  message("*** ARRIVAL LOCATION PLOTS ***")
  
  latPlotYearly <- ggplot(combinedData, aes(Year.of.Discovery, lat))+
    geom_point(color = "red", size=2)+
    geom_smooth(se=FALSE, method="loess", color="black", linewidth=1.5)+
    theme_classic()+
    labs(x="Year", y="Record Latitude")
  
  show(latPlotYearly)
  
  if(!is.na(path)) {
    var9 <- readline(prompt = "Save figure (T/F)? ")
    var9 = as.logical(var9)
    if (isTRUE(var9)) {
      ggsave(plot, file = glue("{path}/{qualifier} Latitude by Year.jpg"))
      message("Graph saved")
    } else if (isFALSE(var9)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    var9 <- readline(prompt = "Press enter to continue. ")
  }
  
  longPlotYearly <- ggplot(combinedData, aes(Year.of.Discovery, long))+
    geom_point(color = "red", size=2)+
    geom_smooth(se=FALSE, method="loess", color="black", linewidth=1.5)+
    theme_classic()+
    labs(x="Year", y="Record Longitude")
  
  show(longPlotYearly)
  
  if(!is.na(path)) {
    varX <- readline(prompt = "Save figure (T/F)? ")
    varX = as.logical(varX)
    if (isTRUE(varX)) {
      ggsave(plot, file = glue("{path}/{qualifier} Longitude by Year.jpg"))
      message("Graph saved")
    } else if (isFALSE(varX)) {
      message("Function halted, graph not saved")
    } else {
      stop("Require boolean input as prompt")
    }
  } else {
    varX <- readline(prompt = "Press enter to continue. ")
  }
}

#' Conduct simple regression analysis to compare associations between variables in the OrniStats data.
#' 
#' @param qualifier Single or list of quoted 5-letter abbreviation(s) for the species whose records are being analysed. To find out the qualifier for a species, please consult the list in the GitHub repository for this package.
#' @param x,y Variables being analysed by regression. The inputs should correspond to the name of the variable column in the `OrniStatsData` or `OrniStatsLocations` objects.
#' @param max_degree Maximum degree polynomial at which regression analysis is conducted. If program is running slowly, set this value to 5.
#' @param approx_dates Remove records where the date of arrival or departure is approximate. Defaults to `FALSE`, but should be set to `TRUE` if either `x` or `y` is the date of arrival or departure at below an annual level.
#' @return Summary table of the model selected in the readline.
#' @export

speciesRegression <- function(qualifier, x, y, max_degree = 10, approx_dates = FALSE) {
  data("OrniStatsRecords", package="ornistats")
  data("OrniStatsLocations", package="ornistats")
  
  subData = data.frame(subset(OrniStatsRecords, Qualifier %in% qualifier & 
                                "Year.of.Discovery" != 2023))
  OrniStatsLocations <- OrniStatsLocations %>%
    rename(Site = site)
  combinedData = left_join(subData, OrniStatsLocations, by = 'Site')
  combinedData$Date.of.Discovery <- as.Date(combinedData$Date.of.Discovery, format = "%d/%m/%Y")
  combinedData$yearday <- yday(combinedData$Date.of.Discovery)
  
  if (isTRUE(approx_dates)) {
    combinedData <- combinedData[Approx.Date != 1]
  }
  
  combinedData <- combinedData %>%
    mutate(across(c(x,y), ~ coalesce(.,0)))

  set.seed(101)
  train.data = combinedData[1:60, ]
  print(train.data)
  test.data = combinedData[61:73, ]
  rmse_list <- c()
  r2_list <- c()
  degs = c(1:max_degree)
  y_unquote = noquote(y)
  
  for (deg in degs) {
    model_formula <- as.formula(paste(y, "~ poly(", x, ",", deg, ", raw=TRUE)"))
    model <- lm(data = train.data, formula = model_formula)
    predictions <- model %>% predict(test.data)
    RMSE_val <- sqrt(mean(model$residuals^2))
    rmse_list <- c(rmse_list, RMSE_val)
    R2_val <- summary(model)$r.squared
    r2_list <- c(r2_list, R2_val)
  }

  plotFrame <- data.frame(degs, rmse_list, r2_list)
  accuracyPlot <- ggplot(plotFrame, aes(degs, rmse_list))+
    geom_line(color="blue")+
    geom_point(color="black")+
    theme_classic()
  
  show(accuracyPlot)
  
  var <- readline("Which degree polynomial will you be selecting? ")
  var = as.numeric(var)
  
  final_model_formula <- as.formula(paste(y, "~ poly(", x, ",", var, ", raw=TRUE)"))
  final_model <- lm(data = train.data, formula = final_model_formula)
  show(summary(final_model))
}