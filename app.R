############################################################
# STOCK CHARTER
############################################################

library(shiny)
library(quantmod)
library(xts)
library(TTR)
library(highcharter)
library(lubridate)

# IMPORTANT:
# jsonlite is called explicitly with jsonlite::fromJSON().
# Do NOT attach library(jsonlite), because jsonlite::validate()
# can conflict with shiny::validate().

options(scipen = 999)

gc()


############################################################
# GENERAL HELPERS
############################################################

format_price <- function(x) {
  
  if (
    is.null(x) ||
    length(x) == 0 ||
    is.na(x[1]) ||
    !is.finite(x[1])
  ) {
    return("N/A")
  }
  
  paste0(
    "$",
    formatC(
      as.numeric(x[1]),
      format = "f",
      digits = 2
    )
  )
}


format_number <- function(x, digits = 2) {
  
  if (
    is.null(x) ||
    length(x) == 0 ||
    is.na(x[1]) ||
    !is.finite(x[1])
  ) {
    return("N/A")
  }
  
  formatC(
    as.numeric(x[1]),
    format = "f",
    digits = digits
  )
}


timestamp_to_ms <- function(x) {
  
  as.numeric(
    as.POSIXct(
      x,
      tz = "UTC"
    )
  ) * 1000
}


############################################################
# COMPANY NAME LOOKUP
############################################################

get_company_name <- function(symbol) {
  
  symbol <- toupper(trimws(symbol))
  
  url <- paste0(
    "https://query1.finance.yahoo.com/v1/finance/search?q=",
    URLencode(symbol, reserved = TRUE),
    "&quotesCount=10&newsCount=0"
  )
  
  result <- tryCatch(
    jsonlite::fromJSON(url),
    error = function(e) {
      NULL
    }
  )
  
  if (
    is.null(result) ||
    is.null(result$quotes) ||
    NROW(result$quotes) == 0 ||
    !"symbol" %in% names(result$quotes)
  ) {
    return(symbol)
  }
  
  quotes <- result$quotes
  
  exact_match <- which(
    toupper(quotes$symbol) == symbol
  )
  
  if (length(exact_match) == 0) {
    return(symbol)
  }
  
  i <- exact_match[1]
  
  company_name <- NA_character_
  
  if ("longname" %in% names(quotes)) {
    company_name <- quotes$longname[i]
  }
  
  if (
    length(company_name) == 0 ||
    is.na(company_name) ||
    company_name == ""
  ) {
    
    if ("shortname" %in% names(quotes)) {
      company_name <- quotes$shortname[i]
    }
  }
  
  if (
    length(company_name) == 0 ||
    is.na(company_name) ||
    company_name == ""
  ) {
    company_name <- symbol
  }
  
  as.character(company_name)
}


############################################################
# DAILY YAHOO DATA
############################################################

download_yahoo_daily <- function(
    symbol,
    start_date,
    end_date
) {
  
  tryCatch(
    
    suppressWarnings(
      
      quantmod::getSymbols(
        Symbols = symbol,
        src = "yahoo",
        from = start_date,
        to = end_date + days(1),
        periodicity = "daily",
        auto.assign = FALSE,
        warnings = FALSE
      )
    ),
    
    error = function(e) {
      NULL
    }
  )
}


############################################################
# HOURLY YAHOO DATA
############################################################

download_yahoo_hourly <- function(symbol) {
  
  encoded_symbol <- URLencode(
    symbol,
    reserved = TRUE
  )
  
  url <- paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/",
    encoded_symbol,
    "?range=1mo",
    "&interval=1h",
    "&includePrePost=false",
    "&events=div%2Csplits"
  )
  
  response <- tryCatch(
    jsonlite::fromJSON(
      url,
      simplifyVector = FALSE
    ),
    error = function(e) {
      NULL
    }
  )
  
  if (
    is.null(response) ||
    is.null(response$chart) ||
    is.null(response$chart$result) ||
    length(response$chart$result) == 0
  ) {
    return(NULL)
  }
  
  result <- response$chart$result[[1]]
  
  if (
    is.null(result$timestamp) ||
    length(result$timestamp) == 0 ||
    is.null(result$indicators$quote) ||
    length(result$indicators$quote) == 0
  ) {
    return(NULL)
  }
  
  quote <- result$indicators$quote[[1]]
  
  safe_numeric <- function(x) {
    
    vapply(
      x,
      function(z) {
        
        if (
          is.null(z) ||
          length(z) == 0
        ) {
          return(NA_real_)
        }
        
        as.numeric(z[1])
      },
      numeric(1)
    )
  }
  
  timestamps <- safe_numeric(result$timestamp)
  opens      <- safe_numeric(quote$open)
  highs      <- safe_numeric(quote$high)
  lows       <- safe_numeric(quote$low)
  closes     <- safe_numeric(quote$close)
  volumes    <- safe_numeric(quote$volume)
  
  n <- min(
    length(timestamps),
    length(opens),
    length(highs),
    length(lows),
    length(closes),
    length(volumes)
  )
  
  if (n == 0) {
    return(NULL)
  }
  
  timestamps <- timestamps[seq_len(n)]
  opens      <- opens[seq_len(n)]
  highs      <- highs[seq_len(n)]
  lows       <- lows[seq_len(n)]
  closes     <- closes[seq_len(n)]
  volumes    <- volumes[seq_len(n)]
  
  exchange_tz <- "America/New_York"
  
  if (
    !is.null(result$meta$exchangeTimezoneName) &&
    length(result$meta$exchangeTimezoneName) > 0
  ) {
    exchange_tz <- result$meta$exchangeTimezoneName
  }
  
  datetime_utc <- as.POSIXct(
    timestamps,
    origin = "1970-01-01",
    tz = "UTC"
  )
  
  datetime_local <- lubridate::with_tz(
    datetime_utc,
    tzone = exchange_tz
  )
  
  intraday_df <- data.frame(
    Open = opens,
    High = highs,
    Low = lows,
    Close = closes,
    Volume = volumes,
    Adjusted = closes
  )
  
  good_rows <-
    is.finite(intraday_df$Open) &
    is.finite(intraday_df$High) &
    is.finite(intraday_df$Low) &
    is.finite(intraday_df$Close)
  
  intraday_df <- intraday_df[
    good_rows,
    ,
    drop = FALSE
  ]
  
  datetime_local <- datetime_local[
    good_rows
  ]
  
  if (NROW(intraday_df) == 0) {
    return(NULL)
  }
  
  hourly_xts <- xts::xts(
    intraday_df,
    order.by = datetime_local
  )
  
  colnames(hourly_xts) <- c(
    "Open",
    "High",
    "Low",
    "Close",
    "Volume",
    "Adjusted"
  )
  
  hourly_xts
}


############################################################
# LAST N TRADING DAYS
############################################################

select_last_trading_days <- function(
    intraday_data,
    trading_days
) {
  
  if (
    is.null(intraday_data) ||
    NROW(intraday_data) == 0
  ) {
    return(NULL)
  }
  
  tz_used <- xts::tzone(intraday_data)
  
  if (
    is.null(tz_used) ||
    length(tz_used) == 0 ||
    identical(tz_used, "")
  ) {
    tz_used <- "America/New_York"
  }
  
  session_dates <- as.Date(
    index(intraday_data),
    tz = tz_used
  )
  
  available_days <- unique(
    session_dates
  )
  
  if (length(available_days) == 0) {
    return(NULL)
  }
  
  selected_days <- tail(
    available_days,
    min(
      trading_days,
      length(available_days)
    )
  )
  
  keep <- session_dates %in% selected_days
  
  intraday_data[
    keep,
  ]
}


############################################################
# WEEKLY AGGREGATION
############################################################

aggregate_weekly_ohlcv <- function(stock_data) {
  
  if (
    is.null(stock_data) ||
    NROW(stock_data) == 0
  ) {
    return(NULL)
  }
  
  ep <- xts::endpoints(
    stock_data,
    on = "weeks"
  )
  
  if (length(ep) < 2) {
    return(NULL)
  }
  
  starts <- head(ep, -1) + 1
  ends   <- tail(ep, -1)
  
  valid <- starts <= ends
  
  starts <- starts[valid]
  ends   <- ends[valid]
  
  if (length(starts) == 0) {
    return(NULL)
  }
  
  weekly_matrix <- matrix(
    NA_real_,
    nrow = length(starts),
    ncol = 6
  )
  
  for (i in seq_along(starts)) {
    
    block <- stock_data[
      starts[i]:ends[i],
    ]
    
    weekly_matrix[i, 1] <- as.numeric(
      Op(block)[1]
    )
    
    weekly_matrix[i, 2] <- max(
      as.numeric(Hi(block)),
      na.rm = TRUE
    )
    
    weekly_matrix[i, 3] <- min(
      as.numeric(Lo(block)),
      na.rm = TRUE
    )
    
    weekly_matrix[i, 4] <- as.numeric(
      tail(
        Cl(block),
        1
      )
    )
    
    volume_values <- as.numeric(
      Vo(block)
    )
    
    weekly_matrix[i, 5] <- sum(
      volume_values,
      na.rm = TRUE
    )
    
    adjusted_data <- tryCatch(
      Ad(block),
      error = function(e) {
        NULL
      }
    )
    
    if (!is.null(adjusted_data)) {
      
      weekly_matrix[i, 6] <- as.numeric(
        tail(
          adjusted_data,
          1
        )
      )
      
    } else {
      
      weekly_matrix[i, 6] <- weekly_matrix[i, 4]
    }
  }
  
  weekly_xts <- xts::xts(
    weekly_matrix,
    order.by = as.Date(
      index(stock_data)[ends]
    )
  )
  
  colnames(weekly_xts) <- c(
    "Open",
    "High",
    "Low",
    "Close",
    "Volume",
    "Adjusted"
  )
  
  weekly_xts
}


############################################################
# FIBONACCI
############################################################

calculate_fibonacci <- function(stock_data) {
  
  if (
    is.null(stock_data) ||
    NROW(stock_data) < 2
  ) {
    return(NULL)
  }
  
  high_values <- as.numeric(
    Hi(stock_data)
  )
  
  low_values <- as.numeric(
    Lo(stock_data)
  )
  
  if (
    all(is.na(high_values)) ||
    all(is.na(low_values))
  ) {
    return(NULL)
  }
  
  high_price <- max(
    high_values,
    na.rm = TRUE
  )
  
  low_price <- min(
    low_values,
    na.rm = TRUE
  )
  
  if (
    !is.finite(high_price) ||
    !is.finite(low_price) ||
    high_price <= low_price
  ) {
    return(NULL)
  }
  
  high_position <- which.max(
    high_values
  )
  
  low_position <- which.min(
    low_values
  )
  
  price_range <- high_price - low_price
  
  ratios <- c(
    0.236,
    0.382,
    0.500,
    0.618,
    0.786
  )
  
  labels <- c(
    "23.6%",
    "38.2%",
    "50.0%",
    "61.8%",
    "78.6%"
  )
  
  descriptions <- c(
    "Shallow retracement",
    "Moderate pullback",
    "Equilibrium",
    "Golden retracement",
    "Deep retracement"
  )
  
  if (low_position < high_position) {
    
    swing_direction <- "Uptrend"
    
    levels <- high_price -
      price_range * ratios
    
  } else {
    
    swing_direction <- "Downtrend"
    
    levels <- low_price +
      price_range * ratios
  }
  
  data.frame(
    ratio = ratios,
    label = labels,
    description = descriptions,
    price = levels,
    swing_direction = swing_direction,
    swing_low = low_price,
    swing_high = high_price,
    swing_low_date = index(stock_data)[low_position],
    swing_high_date = index(stock_data)[high_position]
  )
}


############################################################
# PIVOT / SWING SUPPORT & RESISTANCE
#
# PRIORITY:
# 1. Confirmed local price pivot
# 2. Timeframe swing extreme
# 3. Fibonacci fallback
#
# Fibonacci also acts as confirmation of price structure.
############################################################

calculate_structure_levels <- function(
    stock_data,
    current_price,
    pivot_span = 2,
    fib_tolerance = 0.02
) {
  
  empty_result <- list(
    support = NA_real_,
    resistance = NA_real_,
    support_source = "No established support",
    resistance_source = "No established resistance",
    support_fib_confirmed = FALSE,
    resistance_fib_confirmed = FALSE,
    support_fib_level = NA_real_,
    resistance_fib_level = NA_real_
  )
  
  if (
    is.null(stock_data) ||
    NROW(stock_data) < (pivot_span * 2 + 1) ||
    !is.finite(current_price)
  ) {
    return(empty_result)
  }
  
  highs <- as.numeric(
    Hi(stock_data)
  )
  
  lows <- as.numeric(
    Lo(stock_data)
  )
  
  n <- length(lows)
  
  pivot_lows <- numeric(0)
  pivot_highs <- numeric(0)
  
  first_pivot <- pivot_span + 1
  last_pivot  <- n - pivot_span
  
  if (first_pivot <= last_pivot) {
    
    for (i in first_pivot:last_pivot) {
      
      low_window <- lows[
        (i - pivot_span):(i + pivot_span)
      ]
      
      high_window <- highs[
        (i - pivot_span):(i + pivot_span)
      ]
      
      if (
        is.finite(lows[i]) &&
        lows[i] <= min(
          low_window,
          na.rm = TRUE
        )
      ) {
        pivot_lows <- c(
          pivot_lows,
          lows[i]
        )
      }
      
      if (
        is.finite(highs[i]) &&
        highs[i] >= max(
          high_window,
          na.rm = TRUE
        )
      ) {
        pivot_highs <- c(
          pivot_highs,
          highs[i]
        )
      }
    }
  }
  
  support_candidates <- pivot_lows[
    pivot_lows < current_price
  ]
  
  resistance_candidates <- pivot_highs[
    pivot_highs > current_price
  ]
  
  swing_low <- min(
    lows,
    na.rm = TRUE
  )
  
  swing_high <- max(
    highs,
    na.rm = TRUE
  )
  
  fib <- calculate_fibonacci(
    stock_data
  )
  
  fib_support <- NA_real_
  fib_resistance <- NA_real_
  
  if (!is.null(fib)) {
    
    fib_below <- fib$price[
      fib$price < current_price
    ]
    
    fib_above <- fib$price[
      fib$price > current_price
    ]
    
    if (length(fib_below) > 0) {
      
      fib_support <- max(
        fib_below,
        na.rm = TRUE
      )
    }
    
    if (length(fib_above) > 0) {
      
      fib_resistance <- min(
        fib_above,
        na.rm = TRUE
      )
    }
  }
  
  ##########################################################
  # SUPPORT
  ##########################################################
  
  support <- NA_real_
  support_source <- "No established support"
  
  if (length(support_candidates) > 0) {
    
    support <- max(
      support_candidates,
      na.rm = TRUE
    )
    
    support_source <- "Local pivot support"
    
  } else if (
    is.finite(swing_low) &&
    swing_low < current_price
  ) {
    
    support <- swing_low
    support_source <- "Timeframe swing low"
    
  } else if (is.finite(fib_support)) {
    
    support <- fib_support
    support_source <- "Fibonacci fallback"
  }
  
  ##########################################################
  # RESISTANCE
  ##########################################################
  
  resistance <- NA_real_
  resistance_source <- "No established resistance"
  
  if (length(resistance_candidates) > 0) {
    
    resistance <- min(
      resistance_candidates,
      na.rm = TRUE
    )
    
    resistance_source <- "Local pivot resistance"
    
  } else if (
    is.finite(swing_high) &&
    swing_high > current_price
  ) {
    
    resistance <- swing_high
    resistance_source <- "Timeframe swing high"
    
  } else if (is.finite(fib_resistance)) {
    
    resistance <- fib_resistance
    resistance_source <- "Fibonacci fallback"
  }
  
  ##########################################################
  # FIBONACCI CONFIRMATION
  ##########################################################
  
  support_confirmed <- FALSE
  resistance_confirmed <- FALSE
  
  support_confirming_level <- NA_real_
  resistance_confirming_level <- NA_real_
  
  if (
    !is.null(fib) &&
    is.finite(support) &&
    support != 0
  ) {
    
    support_distance <- abs(
      fib$price - support
    ) / abs(support)
    
    closest_index <- which.min(
      support_distance
    )
    
    if (
      length(closest_index) > 0 &&
      is.finite(
        support_distance[closest_index]
      ) &&
      support_distance[closest_index] <= fib_tolerance
    ) {
      
      support_confirmed <- TRUE
      
      support_confirming_level <- fib$price[
        closest_index
      ]
    }
  }
  
  if (
    !is.null(fib) &&
    is.finite(resistance) &&
    resistance != 0
  ) {
    
    resistance_distance <- abs(
      fib$price - resistance
    ) / abs(resistance)
    
    closest_index <- which.min(
      resistance_distance
    )
    
    if (
      length(closest_index) > 0 &&
      is.finite(
        resistance_distance[closest_index]
      ) &&
      resistance_distance[closest_index] <= fib_tolerance
    ) {
      
      resistance_confirmed <- TRUE
      
      resistance_confirming_level <- fib$price[
        closest_index
      ]
    }
  }
  
  list(
    support = support,
    resistance = resistance,
    support_source = support_source,
    resistance_source = resistance_source,
    support_fib_confirmed = support_confirmed,
    resistance_fib_confirmed = resistance_confirmed,
    support_fib_level = support_confirming_level,
    resistance_fib_level = resistance_confirming_level
  )
}


############################################################
# TRUE HIGH / TRUE LOW
############################################################

calculate_true_high <- function(
    highs,
    closes
) {
  
  n <- length(highs)
  
  result <- rep(
    NA_real_,
    n
  )
  
  if (n == 0) {
    return(result)
  }
  
  result[1] <- highs[1]
  
  if (n >= 2) {
    
    for (i in 2:n) {
      
      result[i] <- max(
        highs[i],
        closes[i - 1],
        na.rm = TRUE
      )
    }
  }
  
  result
}


calculate_true_low <- function(
    lows,
    closes
) {
  
  n <- length(lows)
  
  result <- rep(
    NA_real_,
    n
  )
  
  if (n == 0) {
    return(result)
  }
  
  result[1] <- lows[1]
  
  if (n >= 2) {
    
    for (i in 2:n) {
      
      result[i] <- min(
        lows[i],
        closes[i - 1],
        na.rm = TRUE
      )
    }
  }
  
  result
}


############################################################
# TD SEQUENTIAL
#
# Research implementation using publicly described concepts.
#
# SETUP:
# Buy  = Close < Close four bars earlier
# Sell = Close > Close four bars earlier
#
# Nine consecutive qualifying bars complete Setup.
#
# SETUP PERFECTION:
# Buy: bar 8 or 9 low <= lows of bars 6 and 7
# Sell: bar 8 or 9 high >= highs of bars 6 and 7
#
# COUNTDOWN:
# Buy  = Close <= Low two bars earlier
# Sell = Close >= High two bars earlier
#
# Countdown bars are not required to be consecutive.
#
# 13 QUALIFICATION:
# Buy candidate 13 low <= close of Countdown bar 8
# Sell candidate 13 high >= close of Countdown bar 8
#
# Opposite completed Setup cancels an incomplete Countdown.
#
# Same-direction recycling below is conservative and should
# still be considered a research approximation rather than
# an exact reproduction of proprietary DeMARK software.
############################################################

calculate_td_sequential <- function(stock_data) {
  
  if (
    is.null(stock_data) ||
    NROW(stock_data) == 0
  ) {
    return(NULL)
  }
  
  closes <- as.numeric(
    Cl(stock_data)
  )
  
  highs <- as.numeric(
    Hi(stock_data)
  )
  
  lows <- as.numeric(
    Lo(stock_data)
  )
  
  dates <- index(
    stock_data
  )
  
  n <- length(
    closes
  )
  
  true_high <- calculate_true_high(
    highs,
    closes
  )
  
  true_low <- calculate_true_low(
    lows,
    closes
  )
  
  ##########################################################
  # OUTPUT
  ##########################################################
  
  buy_setup_run <- rep(0L, n)
  sell_setup_run <- rep(0L, n)
  
  buy_setup9 <- rep(FALSE, n)
  sell_setup9 <- rep(FALSE, n)
  
  buy_perfected <- rep(FALSE, n)
  sell_perfected <- rep(FALSE, n)
  
  buy_countdown <- rep(NA_integer_, n)
  sell_countdown <- rep(NA_integer_, n)
  
  buy13 <- rep(FALSE, n)
  sell13 <- rep(FALSE, n)
  
  buy13_deferred <- rep(FALSE, n)
  sell13_deferred <- rep(FALSE, n)
  
  buy_active_series <- rep(FALSE, n)
  sell_active_series <- rep(FALSE, n)
  
  buy_count_state <- rep(0L, n)
  sell_count_state <- rep(0L, n)
  
  ##########################################################
  # STATE
  ##########################################################
  
  buy_cd_active <- FALSE
  sell_cd_active <- FALSE
  
  buy_cd_count <- 0L
  sell_cd_count <- 0L
  
  buy_cd_indices <- integer(0)
  sell_cd_indices <- integer(0)
  
  buy_cd_bar8_close <- NA_real_
  sell_cd_bar8_close <- NA_real_
  
  prior_buy_range <- NA_real_
  prior_sell_range <- NA_real_
  
  ##########################################################
  # RESET HELPERS
  ##########################################################
  
  clear_buy_countdown <- function() {
    
    if (length(buy_cd_indices) > 0) {
      
      buy_countdown[
        buy_cd_indices
      ] <<- NA_integer_
      
      buy13_deferred[
        buy_cd_indices
      ] <<- FALSE
    }
    
    buy_cd_active <<- FALSE
    buy_cd_count <<- 0L
    buy_cd_indices <<- integer(0)
    buy_cd_bar8_close <<- NA_real_
  }
  
  
  clear_sell_countdown <- function() {
    
    if (length(sell_cd_indices) > 0) {
      
      sell_countdown[
        sell_cd_indices
      ] <<- NA_integer_
      
      sell13_deferred[
        sell_cd_indices
      ] <<- FALSE
    }
    
    sell_cd_active <<- FALSE
    sell_cd_count <<- 0L
    sell_cd_indices <<- integer(0)
    sell_cd_bar8_close <<- NA_real_
  }
  
  
  ##########################################################
  # MAIN LOOP
  ##########################################################
  
  if (n >= 5) {
    
    for (i in 5:n) {
      
      ######################################################
      # BUY SETUP RUN
      ######################################################
      
      if (
        !is.na(closes[i]) &&
        !is.na(closes[i - 4]) &&
        closes[i] < closes[i - 4]
      ) {
        
        previous_buy <- buy_setup_run[
          i - 1
        ]
        
        buy_setup_run[i] <- previous_buy + 1L
        
      } else {
        
        buy_setup_run[i] <- 0L
      }
      
      ######################################################
      # SELL SETUP RUN
      ######################################################
      
      if (
        !is.na(closes[i]) &&
        !is.na(closes[i - 4]) &&
        closes[i] > closes[i - 4]
      ) {
        
        previous_sell <- sell_setup_run[
          i - 1
        ]
        
        sell_setup_run[i] <- previous_sell + 1L
        
      } else {
        
        sell_setup_run[i] <- 0L
      }
      
      ######################################################
      # BUY SETUP 9
      ######################################################
      
      if (buy_setup_run[i] == 9L) {
        
        buy_setup9[i] <- TRUE
        
        setup_indices <- (i - 8):i
        
        setup_range <- max(
          true_high[setup_indices],
          na.rm = TRUE
        ) - min(
          true_low[setup_indices],
          na.rm = TRUE
        )
        
        # Opposite Setup cancels active Sell Countdown.
        if (sell_cd_active) {
          clear_sell_countdown()
        }
        
        # Conservative same-direction recycle.
        recycle_buy <- FALSE
        
        if (
          buy_cd_active &&
          is.finite(prior_buy_range) &&
          prior_buy_range > 0 &&
          is.finite(setup_range)
        ) {
          
          range_ratio <- setup_range /
            prior_buy_range
          
          if (
            range_ratio >= 1 &&
            range_ratio <= 2
          ) {
            recycle_buy <- TRUE
          }
        }
        
        if (recycle_buy) {
          clear_buy_countdown()
        }
        
        if (!buy_cd_active) {
          
          buy_cd_active <- TRUE
          buy_cd_count <- 0L
          buy_cd_indices <- integer(0)
          buy_cd_bar8_close <- NA_real_
        }
        
        prior_buy_range <- setup_range
        
        # Perfection at Setup completion.
        reference_low <- min(
          lows[i - 3],
          lows[i - 2],
          na.rm = TRUE
        )
        
        if (
          (
            is.finite(lows[i - 1]) &&
            lows[i - 1] <= reference_low
          ) ||
          (
            is.finite(lows[i]) &&
            lows[i] <= reference_low
          )
        ) {
          buy_perfected[i] <- TRUE
        }
      }
      
      ######################################################
      # SELL SETUP 9
      ######################################################
      
      if (sell_setup_run[i] == 9L) {
        
        sell_setup9[i] <- TRUE
        
        setup_indices <- (i - 8):i
        
        setup_range <- max(
          true_high[setup_indices],
          na.rm = TRUE
        ) - min(
          true_low[setup_indices],
          na.rm = TRUE
        )
        
        if (buy_cd_active) {
          clear_buy_countdown()
        }
        
        recycle_sell <- FALSE
        
        if (
          sell_cd_active &&
          is.finite(prior_sell_range) &&
          prior_sell_range > 0 &&
          is.finite(setup_range)
        ) {
          
          range_ratio <- setup_range /
            prior_sell_range
          
          if (
            range_ratio >= 1 &&
            range_ratio <= 2
          ) {
            recycle_sell <- TRUE
          }
        }
        
        if (recycle_sell) {
          clear_sell_countdown()
        }
        
        if (!sell_cd_active) {
          
          sell_cd_active <- TRUE
          sell_cd_count <- 0L
          sell_cd_indices <- integer(0)
          sell_cd_bar8_close <- NA_real_
        }
        
        prior_sell_range <- setup_range
        
        reference_high <- max(
          highs[i - 3],
          highs[i - 2],
          na.rm = TRUE
        )
        
        if (
          (
            is.finite(highs[i - 1]) &&
            highs[i - 1] >= reference_high
          ) ||
          (
            is.finite(highs[i]) &&
            highs[i] >= reference_high
          )
        ) {
          sell_perfected[i] <- TRUE
        }
      }
      
      ######################################################
      # EXTENDED SETUP RESTART
      ######################################################
      
      if (
        buy_setup_run[i] == 22L &&
        buy_cd_active
      ) {
        
        clear_buy_countdown()
        
        buy_cd_active <- TRUE
      }
      
      if (
        sell_setup_run[i] == 22L &&
        sell_cd_active
      ) {
        
        clear_sell_countdown()
        
        sell_cd_active <- TRUE
      }
      
      ######################################################
      # BUY COUNTDOWN
      ######################################################
      
      if (
        buy_cd_active &&
        i >= 3 &&
        !is.na(closes[i]) &&
        !is.na(lows[i - 2]) &&
        closes[i] <= lows[i - 2]
      ) {
        
        if (buy_cd_count < 12L) {
          
          buy_cd_count <- buy_cd_count + 1L
          
          buy_countdown[i] <- buy_cd_count
          
          buy_cd_indices <- c(
            buy_cd_indices,
            i
          )
          
          if (buy_cd_count == 8L) {
            buy_cd_bar8_close <- closes[i]
          }
          
        } else {
          
          qualifies_13 <-
            is.finite(buy_cd_bar8_close) &&
            is.finite(lows[i]) &&
            lows[i] <= buy_cd_bar8_close
          
          if (qualifies_13) {
            
            buy_countdown[i] <- 13L
            buy13[i] <- TRUE
            
            buy_cd_active <- FALSE
            buy_cd_count <- 13L
            
          } else {
            
            buy13_deferred[i] <- TRUE
          }
        }
      }
      
      ######################################################
      # SELL COUNTDOWN
      ######################################################
      
      if (
        sell_cd_active &&
        i >= 3 &&
        !is.na(closes[i]) &&
        !is.na(highs[i - 2]) &&
        closes[i] >= highs[i - 2]
      ) {
        
        if (sell_cd_count < 12L) {
          
          sell_cd_count <- sell_cd_count + 1L
          
          sell_countdown[i] <- sell_cd_count
          
          sell_cd_indices <- c(
            sell_cd_indices,
            i
          )
          
          if (sell_cd_count == 8L) {
            sell_cd_bar8_close <- closes[i]
          }
          
        } else {
          
          qualifies_13 <-
            is.finite(sell_cd_bar8_close) &&
            is.finite(highs[i]) &&
            highs[i] >= sell_cd_bar8_close
          
          if (qualifies_13) {
            
            sell_countdown[i] <- 13L
            sell13[i] <- TRUE
            
            sell_cd_active <- FALSE
            sell_cd_count <- 13L
            
          } else {
            
            sell13_deferred[i] <- TRUE
          }
        }
      }
      
      ######################################################
      # CURRENT STATE
      ######################################################
      
      buy_active_series[i] <- buy_cd_active
      sell_active_series[i] <- sell_cd_active
      
      if (buy_cd_active) {
        buy_count_state[i] <- buy_cd_count
      }
      
      if (sell_cd_active) {
        sell_count_state[i] <- sell_cd_count
      }
    }
  }
  
  data.frame(
    Date = dates,
    Close = closes,
    High = highs,
    Low = lows,
    BuySetup = pmin(buy_setup_run, 9L),
    SellSetup = pmin(sell_setup_run, 9L),
    BuySetupRun = buy_setup_run,
    SellSetupRun = sell_setup_run,
    BuySetup9 = buy_setup9,
    SellSetup9 = sell_setup9,
    BuyPerfected = buy_perfected,
    SellPerfected = sell_perfected,
    BuyCountdown = buy_countdown,
    SellCountdown = sell_countdown,
    Buy13 = buy13,
    Sell13 = sell13,
    Buy13Deferred = buy13_deferred,
    Sell13Deferred = sell13_deferred,
    BuyCountdownActive = buy_active_series,
    SellCountdownActive = sell_active_series,
    BuyCountdownState = buy_count_state,
    SellCountdownState = sell_count_state
  )
}


############################################################
# BETA
############################################################

calculate_beta <- function(
    stock_data,
    benchmark_data
) {
  
  if (
    is.null(stock_data) ||
    is.null(benchmark_data) ||
    NROW(stock_data) < 30 ||
    NROW(benchmark_data) < 30
  ) {
    return(NA_real_)
  }
  
  stock_returns <- dailyReturn(
    Ad(stock_data),
    type = "log"
  )
  
  market_returns <- dailyReturn(
    Ad(benchmark_data),
    type = "log"
  )
  
  combined <- na.omit(
    merge(
      stock_returns,
      market_returns
    )
  )
  
  if (NROW(combined) < 30) {
    return(NA_real_)
  }
  
  stock_r <- as.numeric(
    combined[, 1]
  )
  
  market_r <- as.numeric(
    combined[, 2]
  )
  
  market_variance <- var(
    market_r,
    na.rm = TRUE
  )
  
  if (
    !is.finite(market_variance) ||
    market_variance == 0
  ) {
    return(NA_real_)
  }
  
  as.numeric(
    cov(
      stock_r,
      market_r,
      use = "complete.obs"
    ) /
      market_variance
  )
}


############################################################
# VOLATILITY
############################################################

calculate_volatility <- function(stock_data) {
  
  if (
    is.null(stock_data) ||
    NROW(stock_data) < 20
  ) {
    return(NA_real_)
  }
  
  returns <- dailyReturn(
    Ad(stock_data),
    type = "log"
  )
  
  returns <- na.omit(
    returns
  )
  
  if (NROW(returns) < 20) {
    return(NA_real_)
  }
  
  as.numeric(
    sd(
      as.numeric(returns),
      na.rm = TRUE
    ) *
      sqrt(252)
  )
}


############################################################
# INTERPRETATIONS
############################################################

interpret_beta <- function(beta) {
  
  if (
    is.na(beta) ||
    !is.finite(beta)
  ) {
    return("Unavailable")
  }
  
  if (beta < 0) {
    return("Inverse market sensitivity")
  }
  
  if (beta < 0.80) {
    return("Lower market sensitivity")
  }
  
  if (beta <= 1.20) {
    return("Near-market sensitivity")
  }
  
  if (beta <= 1.50) {
    return("Above-market sensitivity")
  }
  
  "High market sensitivity"
}


interpret_volatility <- function(volatility) {
  
  if (
    is.na(volatility) ||
    !is.finite(volatility)
  ) {
    return("Unavailable")
  }
  
  pct <- volatility * 100
  
  if (pct < 20) {
    return("Relatively low")
  }
  
  if (pct < 30) {
    return("Moderate")
  }
  
  if (pct < 45) {
    return("Elevated")
  }
  
  if (pct < 60) {
    return("High")
  }
  
  "Very high"
}


############################################################
# UI
############################################################

ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      
      HTML("

      body {
        background-color: #1e1e1e;
        color: white;
      }

      .well {
        background-color: #252525;
        border-color: #444444;
      }

      .control-label {
        color: white;
        font-weight: 500;
      }

      .radio,
      .checkbox {
        color: white;
      }

      .form-control {
        background-color: white;
        color: #222222;
      }

      h2, h3, h4 {
        color: white;
      }

      .btn-primary {
        width: 100%;
      }

      .section-title {
        font-weight: bold;
        margin-top: 7px;
        margin-bottom: 8px;
      }

      .loaded-ticker {
        margin-top: 8px;
        padding: 6px 8px;
        background-color: #333333;
        border-radius: 4px;
        color: #dddddd;
        font-size: 12px;
      }

      .chart-key {
        font-size: 11px;
        color: #bbbbbb;
        margin-top: 4px;
        margin-bottom: 7px;
      }

      .key-support {
        color: #00E676;
        margin-right: 14px;
      }

      .key-resistance {
        color: #FF5252;
        margin-right: 14px;
      }

      .key-td9 {
        color: #FFD54F;
        font-weight: bold;
        margin-right: 14px;
      }

      .key-td13 {
        color: #00E5FF;
        font-weight: bold;
        margin-right: 14px;
      }

      .key-deferred {
        color: #80DEEA;
        margin-right: 14px;
      }

      .analysis-card {
        background-color: #252525;
        border: 1px solid #444444;
        border-radius: 7px;
        padding: 11px;
        margin-bottom: 10px;
        min-height: 112px;
      }

      .analysis-label {
        color: #aaaaaa;
        font-size: 11px;
        text-transform: uppercase;
      }

      .analysis-value {
        color: white;
        font-size: 18px;
        font-weight: bold;
        margin-top: 4px;
      }

      .analysis-note {
        color: #cccccc;
        font-size: 11px;
        margin-top: 4px;
      }

      .fib-card {
        background-color: #292929;
        border: 1px solid #484848;
        border-radius: 7px;
        padding: 12px;
        margin-bottom: 10px;
      }

      .fib-percent {
        color: #42A5F5;
        font-size: 16px;
        font-weight: bold;
      }

      .fib-price {
        color: white;
        font-size: 17px;
        font-weight: bold;
      }

      .fib-description {
        color: #bbbbbb;
        font-size: 11px;
      }

      .research-disclaimer {
        background-color: #202020;
        border: 1px solid #414141;
        border-radius: 6px;
        padding: 10px 12px;
        margin-top: 15px;
        color: #bdbdbd;
        font-size: 11px;
        line-height: 1.45;
      }

      ")
    )
  ),
  
  titlePanel(
    "Stock Charter"
  ),
  
  sidebarLayout(
    
    ########################################################
    # SIDEBAR
    ########################################################
    
    sidebarPanel(
      
      textInput(
        "stockSymbol",
        "Enter Stock Symbol:",
        value = "AAPL",
        placeholder = "AAPL, F, NFLX, NVDA"
      ),
      
      actionButton(
        "loadTicker",
        "Load Ticker",
        class = "btn-primary"
      ),
      
      uiOutput(
        "loadedTickerStatus"
      ),
      
      hr(),
      
      tags$div(
        class = "section-title",
        "Chart Interval"
      ),
      
      radioButtons(
        "chartInterval",
        NULL,
        choices = c(
          "Hourly" = "hourly",
          "Daily" = "daily",
          "Weekly" = "weekly"
        ),
        selected = "daily",
        inline = TRUE
      ),
      
      uiOutput(
        "periodControls"
      ),
      
      hr(),
      
      uiOutput(
        "movingAverageControls"
      ),
      
      hr(),
      
      tags$div(
        class = "section-title",
        "Optional Views"
      ),
      
      checkboxInput(
        "showTD",
        "Show TD 9 / 13",
        TRUE
      ),
      
      checkboxInput(
        "showCountdown",
        "Show TD Countdown Progress",
        TRUE
      ),
      
      checkboxInput(
        "showNavigator",
        "Show Chart Navigator",
        TRUE
      ),
      
      checkboxInput(
        "showRSI",
        "Show RSI Chart",
        TRUE
      ),
      
      checkboxInput(
        "showFib3M",
        "Show 3-Month Retracement Details",
        FALSE
      ),
      
      checkboxInput(
        "showFib26W",
        "Show 26-Week Retracement Details",
        FALSE
      ),
      
      hr(),
      
      downloadButton(
        "downloadData",
        "Download Data"
      )
    ),
    
    ########################################################
    # MAIN
    ########################################################
    
    mainPanel(
      
      wellPanel(
        
        tags$div(
          
          style = "
            background-color:#BFD7EA;
            padding:7px;
            border-radius:5px;
            color:#333333;
            font-size:12px;
            margin-bottom:8px;
          ",
          
          paste(
            "Market and intraday data are provided for research purposes",
            "and may be delayed. Displayed prices should not be interpreted",
            "as real-time executable quotations."
          )
        ),
        
        tags$div(
          
          class = "chart-key",
          
          tags$span(
            class = "key-support",
            "S = Support"
          ),
          
          tags$span(
            class = "key-resistance",
            "R = Resistance"
          ),
          
          tags$span(
            class = "key-td9",
            "9 = TD Setup"
          ),
          
          tags$span(
            class = "key-td13",
            "13 = TD Countdown"
          ),
          
          tags$span(
            class = "key-deferred",
            "+ = Deferred 13"
          )
        ),
        
        highchartOutput(
          "stockChart",
          height = "62vh"
        ),
        
        conditionalPanel(
          condition = "input.showRSI == true",
          
          highchartOutput(
            "rsiChart",
            height = "14vh"
          )
        ),
        
        tags$hr(),
        
        uiOutput(
          "technicalSnapshot"
        ),
        
        conditionalPanel(
          condition = "input.showFib3M == true",
          
          tags$hr(),
          
          uiOutput(
            "fib3MDetails"
          )
        ),
        
        conditionalPanel(
          condition = "input.showFib26W == true",
          
          tags$hr(),
          
          uiOutput(
            "fib26WDetails"
          )
        ),
        
        tags$div(
          
          class = "research-disclaimer",
          
          tags$strong(
            "Research and Data Disclaimer: "
          ),
          
          paste(
            "Stock Charter is an informational and research tool only.",
            "It does not provide investment, financial, tax, legal or trading",
            "advice and does not constitute a recommendation or solicitation",
            "to buy, sell or hold any security. Market and intraday data may",
            "be delayed, incomplete, adjusted or differ from executable",
            "quotations. Technical indicators, Fibonacci levels, pivot and",
            "swing support/resistance estimates, moving averages, RSI, beta",
            "and historical volatility are mechanically derived from historical",
            "observations and may not predict future performance. The TD",
            "Sequential logic is a research implementation based on publicly",
            "described methodology and should not be interpreted as a licensed",
            "or exact reproduction of proprietary DeMARK Indicators. Users",
            "should independently verify market data and conduct their own",
            "research before making investment decisions."
          )
        ),
        
        verbatimTextOutput(
          "errorMsg"
        )
      )
    )
  )
)


############################################################
# SERVER
############################################################

server <- function(
    input,
    output,
    session
) {
  
  ##########################################################
  # AUTHORITATIVE SYMBOL
  ##########################################################
  
  loaded_symbol <- reactiveVal(
    "AAPL"
  )
  
  observeEvent(
    input$loadTicker,
    {
      
      symbol <- toupper(
        trimws(
          input$stockSymbol
        )
      )
      
      if (nchar(symbol) == 0) {
        return()
      }
      
      loaded_symbol(
        symbol
      )
      
      updateTextInput(
        session,
        "stockSymbol",
        value = symbol
      )
    },
    ignoreInit = TRUE
  )
  
  
  output$loadedTickerStatus <- renderUI({
    
    tags$div(
      class = "loaded-ticker",
      paste(
        "Loaded ticker:",
        loaded_symbol()
      )
    )
  })
  
  
  ##########################################################
  # PERIOD CONTROLS
  ##########################################################
  
  output$periodControls <- renderUI({
    
    if (
      identical(
        input$chartInterval,
        "hourly"
      )
    ) {
      
      radioButtons(
        "intradayDays",
        "Hourly Period:",
        choices = c(
          "5 Days (1 Week)" = 5,
          "10 Days (2 Weeks)" = 10
        ),
        selected = 5
      )
      
    } else if (
      identical(
        input$chartInterval,
        "weekly"
      )
    ) {
      
      radioButtons(
        "reportPeriod",
        "Reporting Period:",
        choices = c(
          "3 Months" = "3M",
          "6 Months" = "6M",
          "Year to Date" = "YTD",
          "1 Year" = "1Y",
          "2 Years" = "2Y",
          "5 Years" = "5Y"
        ),
        selected = "1Y"
      )
      
    } else {
      
      radioButtons(
        "reportPeriod",
        "Reporting Period:",
        choices = c(
          "1 Month" = "1M",
          "3 Months" = "3M",
          "6 Months" = "6M",
          "Year to Date" = "YTD",
          "1 Year" = "1Y",
          "2 Years" = "2Y",
          "5 Years" = "5Y"
        ),
        selected = "1Y"
      )
    }
  })
  
  
  ##########################################################
  # MOVING AVERAGES
  ##########################################################
  
  output$movingAverageControls <- renderUI({
    
    period_word <- switch(
      input$chartInterval,
      "hourly" = "Hour",
      "weekly" = "Week",
      "Day"
    )
    
    controls <- list(
      
      tags$div(
        class = "section-title",
        "Technical Indicators"
      ),
      
      checkboxInput(
        "ma9",
        paste0(
          "9-",
          period_word,
          " Moving Average"
        ),
        FALSE
      ),
      
      checkboxInput(
        "ma20",
        paste0(
          "20-",
          period_word,
          " Moving Average"
        ),
        FALSE
      ),
      
      checkboxInput(
        "ma50",
        paste0(
          "50-",
          period_word,
          " Moving Average"
        ),
        FALSE
      )
    )
    
    if (
      !identical(
        input$chartInterval,
        "hourly"
      )
    ) {
      
      controls <- append(
        controls,
        list(
          checkboxInput(
            "ma200",
            paste0(
              "200-",
              period_word,
              " Moving Average"
            ),
            FALSE
          )
        )
      )
    }
    
    do.call(
      tagList,
      controls
    )
  })
  
  
  ##########################################################
  # REPORT DATES
  ##########################################################
  
  reporting_dates <- reactive({
    
    req(
      input$reportPeriod
    )
    
    end_date <- Sys.Date()
    
    start_date <- switch(
      
      input$reportPeriod,
      
      "1M" =
        end_date %m-%
        months(1),
      
      "3M" =
        end_date %m-%
        months(3),
      
      "6M" =
        end_date %m-%
        months(6),
      
      "YTD" =
        floor_date(
          end_date,
          "year"
        ),
      
      "1Y" =
        end_date %m-%
        years(1),
      
      "2Y" =
        end_date %m-%
        years(2),
      
      "5Y" =
        end_date %m-%
        years(5),
      
      end_date %m-%
        years(1)
    )
    
    list(
      start = as.Date(start_date),
      end = as.Date(end_date)
    )
  })
  
  
  ##########################################################
  # COMPANY NAME
  ##########################################################
  
  company_name <- reactive({
    
    get_company_name(
      loaded_symbol()
    )
  })
  
  
  ##########################################################
  # DAILY MASTER
  ##########################################################
  
  daily_master <- reactive({
    
    symbol <- loaded_symbol()
    
    data <- download_yahoo_daily(
      symbol = symbol,
      start_date = Sys.Date() - years(7),
      end_date = Sys.Date()
    )
    
    shiny::validate(
      
      shiny::need(
        !is.null(data),
        paste0(
          "Unable to retrieve daily market data for ",
          symbol,
          ". Verify the ticker symbol."
        )
      ),
      
      shiny::need(
        !is.null(data) &&
          NROW(data) > 0,
        paste0(
          "No daily observations were returned for ",
          symbol,
          "."
        )
      )
    )
    
    data
  })
  
  
  ##########################################################
  # HOURLY MASTER
  ##########################################################
  
  hourly_master <- reactive({
    
    symbol <- loaded_symbol()
    
    data <- download_yahoo_hourly(
      symbol
    )
    
    shiny::validate(
      
      shiny::need(
        !is.null(data) &&
          NROW(data) > 0,
        paste0(
          "Hourly data are currently unavailable for ",
          symbol,
          "."
        )
      )
    )
    
    data
  })
  
  
  ##########################################################
  # WEEKLY MASTER
  ##########################################################
  
  weekly_master <- reactive({
    
    data <- aggregate_weekly_ohlcv(
      daily_master()
    )
    
    shiny::validate(
      
      shiny::need(
        !is.null(data) &&
          NROW(data) > 0,
        "Weekly data could not be constructed."
      )
    )
    
    data
  })
  
  
  ##########################################################
  # CURRENT INTERVAL MASTER
  ##########################################################
  
  interval_master <- reactive({
    
    if (
      identical(
        input$chartInterval,
        "hourly"
      )
    ) {
      return(hourly_master())
    }
    
    if (
      identical(
        input$chartInterval,
        "weekly"
      )
    ) {
      return(weekly_master())
    }
    
    daily_master()
  })
  
  
  ##########################################################
  # DISPLAY DATA
  ##########################################################
  
  chart_data <- reactive({
    
    if (
      identical(
        input$chartInterval,
        "hourly"
      )
    ) {
      
      req(
        input$intradayDays
      )
      
      data <- select_last_trading_days(
        hourly_master(),
        trading_days = as.integer(
          input$intradayDays
        )
      )
      
      shiny::validate(
        
        shiny::need(
          !is.null(data) &&
            NROW(data) > 0,
          "No hourly observations are available for the selected period."
        )
      )
      
      return(data)
    }
    
    dates <- reporting_dates()
    
    if (
      identical(
        input$chartInterval,
        "weekly"
      )
    ) {
      
      source_data <- weekly_master()
      
    } else {
      
      source_data <- daily_master()
    }
    
    data <- source_data[
      paste0(
        dates$start,
        "/",
        dates$end
      )
    ]
    
    shiny::validate(
      
      shiny::need(
        !is.null(data) &&
          NROW(data) > 0,
        "No observations are available for the selected reporting period."
      )
    )
    
    data
  })
  
  
  ##########################################################
  # LABEL HELPERS
  ##########################################################
  
  interval_label <- reactive({
    
    switch(
      input$chartInterval,
      "hourly" = "Hourly",
      "weekly" = "Weekly",
      "Daily"
    )
  })
  
  
  period_unit <- reactive({
    
    switch(
      input$chartInterval,
      "hourly" = "hour",
      "weekly" = "week",
      "day"
    )
  })
  
  
  ##########################################################
  # INTERVAL ANALYSIS
  ##########################################################
  
  interval_analysis <- reactive({
    
    source_data <- interval_master()
    display_data <- chart_data()
    
    current_price <- as.numeric(
      tail(
        Cl(display_data),
        1
      )
    )
    
    ########################################################
    # RSI
    ########################################################
    
    rsi_full <- TTR::RSI(
      Cl(source_data),
      n = 14
    )
    
    rsi_values <- na.omit(
      rsi_full
    )
    
    current_rsi <- if (
      NROW(rsi_values) > 0
    ) {
      
      as.numeric(
        tail(
          rsi_values,
          1
        )
      )
      
    } else {
      
      NA_real_
    }
    
    rsi_status <- if (
      !is.finite(current_rsi)
    ) {
      
      "Unavailable"
      
    } else if (
      current_rsi >= 70
    ) {
      
      "Overbought"
      
    } else if (
      current_rsi <= 30
    ) {
      
      "Oversold"
      
    } else if (
      current_rsi >= 55
    ) {
      
      "Positive Momentum"
      
    } else if (
      current_rsi <= 45
    ) {
      
      "Weak Momentum"
      
    } else {
      
      "Neutral"
    }
    
    ########################################################
    # MOVING AVERAGES
    ########################################################
    
    ma9 <- TTR::SMA(
      Cl(source_data),
      n = 9
    )
    
    ma20 <- TTR::SMA(
      Cl(source_data),
      n = 20
    )
    
    current_ma9 <- as.numeric(
      tail(
        na.omit(ma9),
        1
      )
    )
    
    current_ma20 <- as.numeric(
      tail(
        na.omit(ma20),
        1
      )
    )
    
    if (NROW(source_data) >= 50) {
      
      ma50 <- TTR::SMA(
        Cl(source_data),
        n = 50
      )
      
      current_ma50 <- as.numeric(
        tail(
          na.omit(ma50),
          1
        )
      )
      
    } else {
      
      ma50 <- NULL
      current_ma50 <- NA_real_
    }
    
    ########################################################
    # TREND
    ########################################################
    
    if (is.finite(current_ma50)) {
      
      if (
        current_price > current_ma20 &&
        current_ma20 > current_ma50
      ) {
        
        trend <- "Bullish"
        
      } else if (
        current_price < current_ma20 &&
        current_ma20 < current_ma50
      ) {
        
        trend <- "Bearish"
        
      } else {
        
        trend <- "Mixed / Consolidating"
      }
      
      trend_note <- paste0(
        "MA20: ",
        format_price(current_ma20),
        " | MA50: ",
        format_price(current_ma50)
      )
      
    } else {
      
      if (
        current_price > current_ma9 &&
        current_ma9 > current_ma20
      ) {
        
        trend <- "Bullish"
        
      } else if (
        current_price < current_ma9 &&
        current_ma9 < current_ma20
      ) {
        
        trend <- "Bearish"
        
      } else {
        
        trend <- "Mixed / Consolidating"
      }
      
      trend_note <- paste0(
        "MA9: ",
        format_price(current_ma9),
        " | MA20: ",
        format_price(current_ma20)
      )
    }
    
    ########################################################
    # VOLUME
    ########################################################
    
    current_volume <- as.numeric(
      tail(
        Vo(display_data),
        1
      )
    )
    
    recent_volume <- as.numeric(
      tail(
        Vo(source_data),
        20
      )
    )
    
    average_volume <- mean(
      recent_volume,
      na.rm = TRUE
    )
    
    if (
      is.finite(average_volume) &&
      average_volume > 0
    ) {
      
      volume_ratio <- current_volume /
        average_volume
      
    } else {
      
      volume_ratio <- NA_real_
    }
    
    if (!is.finite(volume_ratio)) {
      
      volume_status <- "Unavailable"
      
    } else if (
      volume_ratio >= 1.20
    ) {
      
      volume_status <- "Above Average"
      
    } else if (
      volume_ratio <= 0.80
    ) {
      
      volume_status <- "Below Average"
      
    } else {
      
      volume_status <- "Normal"
    }
    
    ########################################################
    # TD
    ########################################################
    
    td <- calculate_td_sequential(
      source_data
    )
    
    last_row <- NROW(td)
    
    buy_active <- isTRUE(
      td$BuyCountdownActive[last_row]
    )
    
    sell_active <- isTRUE(
      td$SellCountdownActive[last_row]
    )
    
    buy_count <- td$BuyCountdownState[
      last_row
    ]
    
    sell_count <- td$SellCountdownState[
      last_row
    ]
    
    current_buy_setup <- td$BuySetup[
      last_row
    ]
    
    current_sell_setup <- td$SellSetup[
      last_row
    ]
    
    if (
      buy_active &&
      buy_count > 0
    ) {
      
      td_status <- paste0(
        "Buy Countdown ",
        buy_count,
        " of 13"
      )
      
    } else if (
      sell_active &&
      sell_count > 0
    ) {
      
      td_status <- paste0(
        "Sell Countdown ",
        sell_count,
        " of 13"
      )
      
    } else if (
      current_buy_setup > 0 &&
      current_buy_setup < 9
    ) {
      
      td_status <- paste0(
        "Buy Setup ",
        current_buy_setup,
        " of 9"
      )
      
    } else if (
      current_sell_setup > 0 &&
      current_sell_setup < 9
    ) {
      
      td_status <- paste0(
        "Sell Setup ",
        current_sell_setup,
        " of 9"
      )
      
    } else {
      
      td_status <- "No Active Sequence"
    }
    
    ########################################################
    # MOST RECENT COMPLETED SETUP
    ########################################################
    
    completed_setups <- c(
      which(td$BuySetup9),
      which(td$SellSetup9)
    )
    
    setup_quality <- "No recent completed setup"
    
    if (length(completed_setups) > 0) {
      
      latest_setup <- max(
        completed_setups
      )
      
      if (
        isTRUE(
          td$BuySetup9[
            latest_setup
          ]
        )
      ) {
        
        if (
          isTRUE(
            td$BuyPerfected[
              latest_setup
            ]
          )
        ) {
          
          setup_quality <- "Recent Buy 9 perfected"
          
        } else {
          
          setup_quality <- "Recent Buy 9 completed"
        }
        
      } else {
        
        if (
          isTRUE(
            td$SellPerfected[
              latest_setup
            ]
          )
        ) {
          
          setup_quality <- "Recent Sell 9 perfected"
          
        } else {
          
          setup_quality <- "Recent Sell 9 completed"
        }
      }
    }
    
    ########################################################
    # SUPPORT / RESISTANCE
    ########################################################
    
    pivot_span <- switch(
      input$chartInterval,
      "hourly" = 2,
      "weekly" = 2,
      3
    )
    
    sr <- calculate_structure_levels(
      stock_data = display_data,
      current_price = current_price,
      pivot_span = pivot_span,
      fib_tolerance = 0.02
    )
    
    list(
      current_price = current_price,
      rsi = current_rsi,
      rsi_status = rsi_status,
      current_ma9 = current_ma9,
      current_ma20 = current_ma20,
      current_ma50 = current_ma50,
      trend = trend,
      trend_note = trend_note,
      volume_ratio = volume_ratio,
      volume_status = volume_status,
      td = td,
      td_status = td_status,
      setup_quality = setup_quality,
      buy_countdown_active = buy_active,
      sell_countdown_active = sell_active,
      buy_countdown = buy_count,
      sell_countdown = sell_count,
      support = sr$support,
      resistance = sr$resistance,
      support_source = sr$support_source,
      resistance_source = sr$resistance_source,
      support_fib_confirmed = sr$support_fib_confirmed,
      resistance_fib_confirmed = sr$resistance_fib_confirmed,
      support_fib_level = sr$support_fib_level,
      resistance_fib_level = sr$resistance_fib_level
    )
  })
  
  
  ##########################################################
  # LONG-TERM METRICS
  ##########################################################
  
  long_term_analysis <- reactive({
    
    data <- daily_master()
    
    risk_start <- Sys.Date() -
      years(2)
    
    stock_risk <- data[
      paste0(
        risk_start,
        "/"
      )
    ]
    
    spy <- download_yahoo_daily(
      symbol = "SPY",
      start_date = risk_start,
      end_date = Sys.Date()
    )
    
    beta <- calculate_beta(
      stock_risk,
      spy
    )
    
    volatility <- calculate_volatility(
      stock_risk
    )
    
    data_3m <- data[
      paste0(
        Sys.Date() - months(3),
        "/"
      )
    ]
    
    fib_3m <- calculate_fibonacci(
      data_3m
    )
    
    data_26w <- data[
      paste0(
        Sys.Date() - weeks(26),
        "/"
      )
    ]
    
    fib_26w <- calculate_fibonacci(
      data_26w
    )
    
    list(
      beta = beta,
      beta_status = interpret_beta(beta),
      volatility = volatility,
      volatility_status = interpret_volatility(volatility),
      fib_3m = fib_3m,
      fib_26w = fib_26w
    )
  })
  
  
  ##########################################################
  # DOWNLOAD
  ##########################################################
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      paste0(
        loaded_symbol(),
        "_",
        input$chartInterval,
        "_Stock_Data_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      data <- chart_data()
      
      export_data <- data.frame(
        Timestamp = index(data),
        coredata(data)
      )
      
      write.csv(
        export_data,
        file,
        row.names = FALSE
      )
    }
  )
  
  
  ##########################################################
  # THEME
  ##########################################################
  
  custom_theme <- hc_theme_merge(
    
    hc_theme_darkunica(),
    
    hc_theme(
      
      chart = list(
        backgroundColor = "#1e1e1e",
        style = list(
          fontFamily = "Arial, sans-serif"
        )
      ),
      
      title = list(
        style = list(
          color = "#ffffff",
          fontSize = "19px"
        )
      ),
      
      subtitle = list(
        style = list(
          color = "#cccccc",
          fontSize = "12px"
        )
      )
    )
  )
  
  
  ##########################################################
  # MAIN CHART
  ##########################################################
  
  output$stockChart <- renderHighchart({
    
    data <- chart_data()
    analysis <- interval_analysis()
    source_data <- interval_master()
    
    symbol <- loaded_symbol()
    company <- company_name()
    interval <- interval_label()
    
    ########################################################
    # SUBTITLE
    ########################################################
    
    if (
      identical(
        input$chartInterval,
        "hourly"
      )
    ) {
      
      day_count <- as.integer(
        input$intradayDays
      )
      
      subtitle_text <- paste0(
        "Hourly | 60-minute candles | ",
        day_count,
        " trading days",
        ifelse(
          day_count == 5,
          " (1 week)",
          " (2 weeks)"
        )
      )
      
    } else {
      
      actual_start <- as.Date(
        index(data)[1]
      )
      
      actual_end <- as.Date(
        tail(
          index(data),
          1
        )
      )
      
      subtitle_text <- paste(
        interval,
        "|",
        format(
          actual_start,
          "%B %d, %Y"
        ),
        "through",
        format(
          actual_end,
          "%B %d, %Y"
        )
      )
    }
    
    ########################################################
    # SUPPORT / RESISTANCE
    ########################################################
    
    price_lines <- list()
    
    if (is.finite(analysis$support)) {
      
      price_lines <- append(
        price_lines,
        list(
          list(
            value = analysis$support,
            color = "#00E676",
            dashStyle = "Dash",
            width = 2,
            zIndex = 5,
            label = list(
              text = "S",
              align = "left",
              x = 6,
              style = list(
                color = "#00E676",
                fontSize = "10px",
                fontWeight = "normal"
              )
            )
          )
        )
      )
    }
    
    if (is.finite(analysis$resistance)) {
      
      price_lines <- append(
        price_lines,
        list(
          list(
            value = analysis$resistance,
            color = "#FF5252",
            dashStyle = "Dash",
            width = 2,
            zIndex = 5,
            label = list(
              text = "R",
              align = "right",
              x = -6,
              style = list(
                color = "#FF5252",
                fontSize = "10px",
                fontWeight = "normal"
              )
            )
          )
        )
      )
    }
    
    ########################################################
    # BASE CHART
    ########################################################
    
    hc <- highchart(
      type = "stock"
    ) |>
      
      hc_title(
        text = paste0(
          company,
          " (",
          symbol,
          ")"
        )
      ) |>
      
      hc_subtitle(
        text = subtitle_text
      ) |>
      
      hc_add_series(
        OHLC(data),
        type = "candlestick",
        yAxis = 0,
        name = symbol,
        showInLegend = FALSE
      ) |>
      
      hc_yAxis_multiples(
        
        list(
          title = list(
            text = "Price"
          ),
          height = "75%",
          lineWidth = 1,
          resize = list(
            enabled = TRUE
          ),
          plotLines = price_lines
        ),
        
        list(
          title = list(
            text = "Volume"
          ),
          top = "78%",
          height = "22%",
          offset = 0,
          lineWidth = 1
        )
      ) |>
      
      hc_add_series(
        Vo(data),
        yAxis = 1,
        type = "column",
        name = "Volume",
        color = "#00C853",
        showInLegend = FALSE
      ) |>
      
      hc_tooltip(
        valueDecimals = 2
      ) |>
      
      hc_rangeSelector(
        enabled = FALSE
      ) |>
      
      hc_navigator(
        enabled = isTRUE(
          input$showNavigator
        )
      ) |>
      
      hc_scrollbar(
        enabled = isTRUE(
          input$showNavigator
        )
      ) |>
      
      hc_add_theme(
        custom_theme
      )
    
    ########################################################
    # MOVING AVERAGE HELPER
    ########################################################
    
    add_ma <- function(
    chart,
    n
    ) {
      
      if (NROW(source_data) < n) {
        return(chart)
      }
      
      ma <- TTR::SMA(
        Cl(source_data),
        n = n
      )
      
      ma_display <- ma[
        index(data)
      ]
      
      unit_label <- switch(
        input$chartInterval,
        "hourly" = "Hour",
        "weekly" = "Week",
        "Day"
      )
      
      chart |>
        
        hc_add_series(
          ma_display,
          name = paste0(
            n,
            "-",
            unit_label,
            " MA"
          ),
          type = "line",
          yAxis = 0,
          lineWidth = 1.5
        )
    }
    
    if (isTRUE(input$ma9)) {
      hc <- add_ma(
        hc,
        9
      )
    }
    
    if (isTRUE(input$ma20)) {
      hc <- add_ma(
        hc,
        20
      )
    }
    
    if (isTRUE(input$ma50)) {
      hc <- add_ma(
        hc,
        50
      )
    }
    
    if (
      !identical(
        input$chartInterval,
        "hourly"
      ) &&
      isTRUE(input$ma200)
    ) {
      
      hc <- add_ma(
        hc,
        200
      )
    }
    
    ########################################################
    # TD NUMBERS
    ########################################################
    
    if (isTRUE(input$showTD)) {
      
      td <- analysis$td
      
      first_time <- min(
        index(data)
      )
      
      last_time <- max(
        index(data)
      )
      
      td_display <- td[
        td$Date >= first_time &
          td$Date <= last_time,
      ]
      
      add_td_label <- function(
    chart,
    rows,
    label_text,
    y_variable,
    y_offset,
    label_color,
    font_size = "12px",
    font_weight = "bold"
      ) {
        
        if (
          is.null(rows) ||
          NROW(rows) == 0
        ) {
          return(chart)
        }
        
        points <- data.frame(
          x = timestamp_to_ms(
            rows$Date
          ),
          y = rows[[y_variable]]
        )
        
        chart |>
          
          hc_add_series(
            data = list_parse2(points),
            type = "scatter",
            yAxis = 0,
            showInLegend = FALSE,
            enableMouseTracking = FALSE,
            marker = list(
              enabled = FALSE
            ),
            dataLabels = list(
              enabled = TRUE,
              format = label_text,
              y = y_offset,
              crop = FALSE,
              overflow = "allow",
              style = list(
                color = label_color,
                fontSize = font_size,
                fontWeight = font_weight,
                textOutline = "2px #111111"
              )
            )
          )
      }
      
      ######################################################
      # COMPLETED 9
      ######################################################
      
      hc <- add_td_label(
        hc,
        td_display[
          td_display$BuySetup9,
        ],
        "9",
        "Low",
        18,
        "#FFD54F"
      )
      
      hc <- add_td_label(
        hc,
        td_display[
          td_display$SellSetup9,
        ],
        "9",
        "High",
        -13,
        "#FFD54F"
      )
      
      ######################################################
      # COMPLETED 13
      ######################################################
      
      hc <- add_td_label(
        hc,
        td_display[
          td_display$Buy13,
        ],
        "13",
        "Low",
        20,
        "#00E5FF"
      )
      
      hc <- add_td_label(
        hc,
        td_display[
          td_display$Sell13,
        ],
        "13",
        "High",
        -15,
        "#00E5FF"
      )
      
      ######################################################
      # DEFERRED 13
      ######################################################
      
      hc <- add_td_label(
        hc,
        td_display[
          td_display$Buy13Deferred,
        ],
        "+",
        "Low",
        16,
        "#80DEEA",
        "10px",
        "bold"
      )
      
      hc <- add_td_label(
        hc,
        td_display[
          td_display$Sell13Deferred,
        ],
        "+",
        "High",
        -11,
        "#80DEEA",
        "10px",
        "bold"
      )
      
      ######################################################
      # COUNTDOWN PROGRESS 8-12
      ######################################################
      
      if (isTRUE(input$showCountdown)) {
        
        buy_progress <- td_display[
          !is.na(td_display$BuyCountdown) &
            td_display$BuyCountdown >= 8 &
            td_display$BuyCountdown <= 12,
        ]
        
        if (NROW(buy_progress) > 0) {
          
          points <- lapply(
            seq_len(
              NROW(buy_progress)
            ),
            function(i) {
              
              list(
                x = timestamp_to_ms(
                  buy_progress$Date[i]
                ),
                y = buy_progress$Low[i],
                custom = list(
                  label = as.character(
                    buy_progress$BuyCountdown[i]
                  )
                )
              )
            }
          )
          
          hc <- hc |>
            
            hc_add_series(
              data = points,
              type = "scatter",
              yAxis = 0,
              showInLegend = FALSE,
              enableMouseTracking = FALSE,
              marker = list(
                enabled = FALSE
              ),
              dataLabels = list(
                enabled = TRUE,
                format = "{point.custom.label}",
                y = 10,
                style = list(
                  color = "#A5D6A7",
                  fontSize = "8px",
                  fontWeight = "normal",
                  textOutline = "1px #1e1e1e"
                )
              )
            )
        }
        
        sell_progress <- td_display[
          !is.na(td_display$SellCountdown) &
            td_display$SellCountdown >= 8 &
            td_display$SellCountdown <= 12,
        ]
        
        if (NROW(sell_progress) > 0) {
          
          points <- lapply(
            seq_len(
              NROW(sell_progress)
            ),
            function(i) {
              
              list(
                x = timestamp_to_ms(
                  sell_progress$Date[i]
                ),
                y = sell_progress$High[i],
                custom = list(
                  label = as.character(
                    sell_progress$SellCountdown[i]
                  )
                )
              )
            }
          )
          
          hc <- hc |>
            
            hc_add_series(
              data = points,
              type = "scatter",
              yAxis = 0,
              showInLegend = FALSE,
              enableMouseTracking = FALSE,
              marker = list(
                enabled = FALSE
              ),
              dataLabels = list(
                enabled = TRUE,
                format = "{point.custom.label}",
                y = -9,
                style = list(
                  color = "#EF9A9A",
                  fontSize = "8px",
                  fontWeight = "normal",
                  textOutline = "1px #1e1e1e"
                )
              )
            )
        }
      }
    }
    
    hc
  })
  
  
  ##########################################################
  # RSI
  ##########################################################
  
  output$rsiChart <- renderHighchart({
    
    source_data <- interval_master()
    display_data <- chart_data()
    
    rsi_full <- TTR::RSI(
      Cl(source_data),
      n = 14
    )
    
    rsi_display <- rsi_full[
      index(display_data)
    ]
    
    unit <- switch(
      input$chartInterval,
      "hourly" = "hourly",
      "weekly" = "weekly",
      "daily"
    )
    
    highchart(
      type = "stock"
    ) |>
      
      hc_title(
        text = "RSI",
        margin = 2,
        style = list(
          fontSize = "12px",
          fontWeight = "normal"
        )
      ) |>
      
      hc_subtitle(
        text = paste0(
          "14-period | ",
          unit
        ),
        style = list(
          fontSize = "9px",
          color = "#999999"
        )
      ) |>
      
      hc_chart(
        marginTop = 31,
        marginBottom = 24
      ) |>
      
      hc_add_series(
        rsi_display,
        type = "line",
        color = "#42A5F5",
        lineWidth = 1.5,
        showInLegend = FALSE
      ) |>
      
      hc_yAxis(
        min = 0,
        max = 100,
        tickPositions = c(
          0,
          30,
          50,
          70,
          100
        ),
        opposite = TRUE,
        title = list(
          text = NULL
        ),
        plotLines = list(
          
          list(
            value = 30,
            color = "#00E676",
            dashStyle = "ShortDash",
            width = 1
          ),
          
          list(
            value = 70,
            color = "#FF5252",
            dashStyle = "ShortDash",
            width = 1
          )
        )
      ) |>
      
      hc_rangeSelector(
        enabled = FALSE
      ) |>
      
      hc_navigator(
        enabled = FALSE
      ) |>
      
      hc_scrollbar(
        enabled = FALSE
      ) |>
      
      hc_legend(
        enabled = FALSE
      ) |>
      
      hc_add_theme(
        custom_theme
      )
  })
  
  
  ##########################################################
  # TECHNICAL SNAPSHOT
  ##########################################################
  
  output$technicalSnapshot <- renderUI({
    
    a <- interval_analysis()
    lt <- long_term_analysis()
    
    unit <- period_unit()
    
    ########################################################
    # VALUES
    ########################################################
    
    if (is.finite(a$volume_ratio)) {
      
      volume_text <- paste0(
        format_number(
          a$volume_ratio,
          2
        ),
        "x"
      )
      
    } else {
      
      volume_text <- "N/A"
    }
    
    if (is.finite(lt$beta)) {
      
      beta_text <- format_number(
        lt$beta,
        2
      )
      
    } else {
      
      beta_text <- "N/A"
    }
    
    if (is.finite(lt$volatility)) {
      
      volatility_text <- paste0(
        format_number(
          lt$volatility * 100,
          1
        ),
        "%"
      )
      
    } else {
      
      volatility_text <- "N/A"
    }
    
    ########################################################
    # TD NOTE
    ########################################################
    
    td_note <- a$setup_quality
    
    if (
      a$buy_countdown_active &&
      a$buy_countdown > 0
    ) {
      
      td_note <- paste0(
        "Buy ",
        a$buy_countdown,
        "/13 active"
      )
    }
    
    if (
      a$sell_countdown_active &&
      a$sell_countdown > 0
    ) {
      
      td_note <- paste0(
        "Sell ",
        a$sell_countdown,
        "/13 active"
      )
    }
    
    ########################################################
    # SUPPORT NOTE
    ########################################################
    
    support_note <- a$support_source
    
    if (
      isTRUE(
        a$support_fib_confirmed
      )
    ) {
      
      support_note <- paste0(
        support_note,
        " | Fib confirmed near ",
        format_price(
          a$support_fib_level
        )
      )
    }
    
    ########################################################
    # RESISTANCE NOTE
    ########################################################
    
    resistance_note <- a$resistance_source
    
    if (
      isTRUE(
        a$resistance_fib_confirmed
      )
    ) {
      
      resistance_note <- paste0(
        resistance_note,
        " | Fib confirmed near ",
        format_price(
          a$resistance_fib_level
        )
      )
    }
    
    ########################################################
    # UI
    ########################################################
    
    tagList(
      
      tags$h3(
        paste0(
          loaded_symbol(),
          " Technical Snapshot"
        )
      ),
      
      fluidRow(
        
        column(
          2,
          tags$div(
            class = "analysis-card",
            
            tags$div(
              class = "analysis-label",
              "Current Price"
            ),
            
            tags$div(
              class = "analysis-value",
              format_price(
                a$current_price
              )
            ),
            
            tags$div(
              class = "analysis-note",
              interval_label()
            )
          )
        ),
        
        column(
          2,
          tags$div(
            class = "analysis-card",
            
            tags$div(
              class = "analysis-label",
              "Trend"
            ),
            
            tags$div(
              class = "analysis-value",
              a$trend
            ),
            
            tags$div(
              class = "analysis-note",
              a$trend_note
            )
          )
        ),
        
        column(
          2,
          tags$div(
            class = "analysis-card",
            
            tags$div(
              class = "analysis-label",
              "RSI"
            ),
            
            tags$div(
              class = "analysis-value",
              format_number(
                a$rsi,
                1
              )
            ),
            
            tags$div(
              class = "analysis-note",
              paste0(
                a$rsi_status,
                " | 14-",
                unit
              )
            )
          )
        ),
        
        column(
          2,
          tags$div(
            class = "analysis-card",
            
            tags$div(
              class = "analysis-label",
              "Volume"
            ),
            
            tags$div(
              class = "analysis-value",
              volume_text
            ),
            
            tags$div(
              class = "analysis-note",
              a$volume_status
            )
          )
        ),
        
        column(
          2,
          tags$div(
            class = "analysis-card",
            
            tags$div(
              class = "analysis-label",
              "Beta"
            ),
            
            tags$div(
              class = "analysis-value",
              beta_text
            ),
            
            tags$div(
              class = "analysis-note",
              lt$beta_status
            ),
            
            tags$div(
              class = "analysis-note",
              "vs SPY | 2-year daily"
            )
          )
        ),
        
        column(
          2,
          tags$div(
            class = "analysis-card",
            
            tags$div(
              class = "analysis-label",
              "Volatility"
            ),
            
            tags$div(
              class = "analysis-value",
              volatility_text
            ),
            
            tags$div(
              class = "analysis-note",
              lt$volatility_status
            ),
            
            tags$div(
              class = "analysis-note",
              "Annualized | 2-year daily"
            )
          )
        )
      ),
      
      fluidRow(
        
        column(
          4,
          tags$div(
            class = "analysis-card",
            
            tags$div(
              class = "analysis-label",
              paste(
                interval_label(),
                "TD Sequential"
              )
            ),
            
            tags$div(
              class = "analysis-value",
              a$td_status
            ),
            
            tags$div(
              class = "analysis-note",
              td_note
            )
          )
        ),
        
        column(
          4,
          tags$div(
            class = "analysis-card",
            
            tags$div(
              class = "analysis-label",
              "Support"
            ),
            
            tags$div(
              class = "analysis-value",
              style = "color:#00E676;",
              format_price(
                a$support
              )
            ),
            
            tags$div(
              class = "analysis-note",
              support_note
            )
          )
        ),
        
        column(
          4,
          tags$div(
            class = "analysis-card",
            
            tags$div(
              class = "analysis-label",
              "Resistance"
            ),
            
            tags$div(
              class = "analysis-value",
              style = "color:#FF5252;",
              format_price(
                a$resistance
              )
            ),
            
            tags$div(
              class = "analysis-note",
              resistance_note
            )
          )
        )
      )
    )
  })
  
  
  ##########################################################
  # FIBONACCI CARD HELPER
  ##########################################################
  
  render_fib_cards <- function(fib) {
    
    if (is.null(fib)) {
      
      return(
        tags$div(
          "Fibonacci data unavailable."
        )
      )
    }
    
    cards <- lapply(
      seq_len(
        NROW(fib)
      ),
      function(i) {
        
        level <- fib[i, ]
        
        column(
          ifelse(
            i <= 3,
            4,
            6
          ),
          
          tags$div(
            class = "fib-card",
            
            tags$div(
              class = "fib-percent",
              paste(
                level$label,
                "Level"
              )
            ),
            
            tags$div(
              class = "fib-price",
              format_price(
                level$price
              )
            ),
            
            tags$div(
              class = "fib-description",
              level$description
            )
          )
        )
      }
    )
    
    do.call(
      fluidRow,
      cards
    )
  }
  
  
  ##########################################################
  # 3-MONTH FIB
  ##########################################################
  
  output$fib3MDetails <- renderUI({
    
    fib <- long_term_analysis()$fib_3m
    
    if (is.null(fib)) {
      
      return(
        tags$div(
          "3-month Fibonacci data unavailable."
        )
      )
    }
    
    tagList(
      
      tags$h3(
        "3-Month Fibonacci Retracement"
      ),
      
      tags$div(
        class = "analysis-note",
        paste0(
          "Swing: ",
          unique(
            fib$swing_direction
          )[1],
          " | Low: ",
          format_price(
            unique(
              fib$swing_low
            )[1]
          ),
          " | High: ",
          format_price(
            unique(
              fib$swing_high
            )[1]
          )
        )
      ),
      
      br(),
      
      render_fib_cards(
        fib
      )
    )
  })
  
  
  ##########################################################
  # 26-WEEK FIB
  ##########################################################
  
  output$fib26WDetails <- renderUI({
    
    fib <- long_term_analysis()$fib_26w
    
    if (is.null(fib)) {
      
      return(
        tags$div(
          "26-week Fibonacci data unavailable."
        )
      )
    }
    
    tagList(
      
      tags$h3(
        "26-Week Fibonacci Retracement"
      ),
      
      tags$div(
        class = "analysis-note",
        paste0(
          "Swing: ",
          unique(
            fib$swing_direction
          )[1],
          " | Low: ",
          format_price(
            unique(
              fib$swing_low
            )[1]
          ),
          " | High: ",
          format_price(
            unique(
              fib$swing_high
            )[1]
          )
        )
      ),
      
      br(),
      
      render_fib_cards(
        fib
      )
    )
  })
  
  
  ##########################################################
  # STATUS MESSAGE
  ##########################################################
  
  output$errorMsg <- renderText({
    
    data <- chart_data()
    
    if (NROW(data) < 5) {
      
      return(
        paste(
          "Limited market observations are available",
          "for the selected timeframe."
        )
      )
    }
    
    ""
  })
}


############################################################
# RUN APPLICATION
############################################################

shinyApp(
  ui = ui,
  server = server
)