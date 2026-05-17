library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)


library(readxl)

##########################returns##############################################################
returns_values <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/returns_values.xlsx")


returns_values <- returns_values %>% select(-1)
View(returns_values)

#convert to year.month format
date_cols <- seq(1, ncol(returns_values), by = 2)

returns_values[date_cols] <- lapply(returns_values[date_cols], function(x) {
  x_numeric <- as.numeric(x)
  as.Date(x_numeric, origin = "1899-12-30")
})


returns_values[date_cols] <- lapply(returns_values[date_cols], function(x) {
  format(x, "%Y-%m")
})



# next steps- convert to wide format
date_cols <- seq(1, ncol(returns_values), by = 2)
val_cols  <- seq(2, ncol(returns_values), by = 2)

long_returns_values <- map2_dfr(date_cols, val_cols, function(dcol, vcol) {
  tibble(
    Date   = returns_values[[dcol]],
    Ticker = names(returns_values)[vcol],
    returns    = as.numeric(returns_values[[vcol]])
  )
}) 


long_returns_values <- long_returns_values %>%
  tidyr::drop_na()


View(long_returns_values)


##########################MARKETCAP##############################################################
market_cap_CUR_values <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/market_cap_CUR_values.xlsx")
View(market_cap_CUR_values)

market_cap_CUR_values <- market_cap_CUR_values %>% select(-1)

#convert to year.month format
date_cols <- seq(1, ncol(market_cap_CUR_values), by = 2)

market_cap_CUR_values[date_cols] <- lapply(market_cap_CUR_values[date_cols], function(x) {
  x_numeric <- as.numeric(x)
  as.Date(x_numeric, origin = "1899-12-30")
})

market_cap_CUR_values[date_cols] <- lapply(market_cap_CUR_values[date_cols], function(x) {
  format(x, "%Y-%m")
})


# next steps- convert to wide format
date_cols <- seq(1, ncol(market_cap_CUR_values), by = 2)
val_cols  <- seq(2, ncol(market_cap_CUR_values), by = 2)

long_market_cap_values <- map2_dfr(date_cols, val_cols, function(dcol, vcol) {
  tibble(
    Date   = market_cap_CUR_values[[dcol]],
    Ticker = names(market_cap_CUR_values)[vcol],
    mark_cap    = as.numeric(market_cap_CUR_values[[vcol]])
  )
}) 

long_market_cap_values <- long_market_cap_values %>%
  tidyr::drop_na()

View(long_market_cap_values)





##########################operating_profitability_values##############################################################

operating_profitability_values <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/operating_profitability_values.xlsx")

operating_profitability_values <- operating_profitability_values %>% select(-1)

View(operating_profitability_values)

#convert to year.month format
date_cols <- seq(1, ncol(operating_profitability_values), by = 2)

operating_profitability_values[date_cols] <- lapply(operating_profitability_values[date_cols], function(x) {
  x_numeric <- as.numeric(x)
  as.Date(x_numeric, origin = "1899-12-30")
})

operating_profitability_values[date_cols] <- lapply(operating_profitability_values[date_cols], function(x) {
  format(x, "%Y-%m")
})


# next steps- convert to wide format
date_cols <- seq(1, ncol(operating_profitability_values), by = 2)
val_cols  <- seq(2, ncol(operating_profitability_values), by = 2)

operating_profitability_values <- map2_dfr(date_cols, val_cols, function(dcol, vcol) {
  tibble(
    Date   = operating_profitability_values[[dcol]],
    Ticker = names(operating_profitability_values)[vcol],
    op_prof    = as.numeric(operating_profitability_values[[vcol]])
  )
}) 

operating_profitability_values <- operating_profitability_values %>%
  tidyr::drop_na()

View(operating_profitability_values)


#######achieve annual profit numbers
library(dplyr)
library(lubridate)

op_long <- operating_profitability_values %>%
  mutate(
    Date = ymd(paste0(Date, "-01")),   # if Date is "YYYY-MM"
    year = year(Date)
  ) %>%
  group_by(Ticker, year) %>%
  summarise(op_prof = sum(op_prof, na.rm = TRUE),
            #n_obs = sum(!is.na(op_prof)),
            .groups = "drop")

View(op_long)

#####merge and impute OP PROFIT######

df <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/df2.xlsx")

View(df)
library(lubridate)

df$Date <- format(df$Date, "%Y-%m")
df <- df %>%
  mutate(year = as.numeric(substr(Date, 1, 4)))
View(df)


library(dplyr)

op_long <- df %>%
  left_join(op_long, by = c("year", "Ticker"))

View(op_long)

#identify problematic tickers (only NAs)

na_op_prof <- op_long %>%
  group_by(Ticker) %>%
  summarise(non_na_count = sum(!is.na(op_prof)),
            .groups = "drop") %>%
  filter(non_na_count == 0) %>%
  pull(Ticker)

na_op_prof

#remove problematic tickers

rm_op_prof <- c(
  "1088123D SW Equity",
  "2601855D SW Equity",
  "ARAO SW Equity",
  "BACP SW Equity",
  "CSM SW Equity",
  "EDI SW Equity",
  "EDIN SW Equity",
  "GCZ SW Equity",
  "GNT SW Equity",
  "GOT SW Equity",
  "HAB SW Equity",
  "IFTN SW Equity",
  "IHSN SW Equity",
  "KAMM SW Equity",
  "KONF SW Equity",
  "LIMN SW Equity",
  "MIRN SW Equity",
  "OMN SW Equity",
  "OMNP SW Equity",
  "ORI SW Equity",
  "RIV SW Equity",
  "SYMS SW Equity",
  "TAGN SW Equity"
)


op_long <- op_long %>%
  filter(!Ticker %in% rm_op_prof)


View(op_long)





##########################Total_assets_values##############################################################

Total_assets_values <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/Total_assets_values.xlsx")

Total_assets_values <- Total_assets_values %>% select(-1)

View(Total_assets_values)

#convert to year.month format
date_cols <- seq(1, ncol(Total_assets_values), by = 2)

Total_assets_values[date_cols] <- lapply(Total_assets_values[date_cols], function(x) {
  x_numeric <- as.numeric(x)
  as.Date(x_numeric, origin = "1899-12-30")
})

Total_assets_values[date_cols] <- lapply(Total_assets_values[date_cols], function(x) {
  format(x, "%Y-%m")
})


# next steps- convert to wide format
date_cols <- seq(1, ncol(Total_assets_values), by = 2)
val_cols  <- seq(2, ncol(Total_assets_values), by = 2)

Total_assets_values <- map2_dfr(date_cols, val_cols, function(dcol, vcol) {
  tibble(
    Date   = Total_assets_values[[dcol]],
    Ticker = names(Total_assets_values)[vcol],
    total_assets    = as.numeric(Total_assets_values[[vcol]])
  )
}) 

Total_assets_values <- Total_assets_values %>%
  tidyr::drop_na()


#IMPUTE: Under the column "value imputed" add the following entries corresponding to the respective dates under "Date" column

library(dplyr)

library(dplyr)


new_rows <- tibble(
  Date = c(
    "2008-12",
    
    "2009-03", "2009-06", "2009-09", "2009-12",
    
    "2010-03", "2010-06", "2010-09", "2010-12",
    
    "2011-03", "2011-06", "2011-09", "2011-12",
    
    "2012-03", "2012-06", "2012-09", "2012-12"
  ),
  Ticker = "SNBN SW Equity",
  total_assets = c(
    214322.6,
    
    238229.7, 242246.9, 205800.9, 207263.8,
    
    211480.5, 305083.5, 288067.1, 273574.6,
    
    280505.9, 262515.5, 385732.6, 350808.9,
    
    344333.4, 439353.1, 508545.8, 506159.6
  )
)
Total_assets_values <- Total_assets_values %>%
  bind_rows(new_rows) %>%
  # if a row already exists for same Date/Ticker, keep the non-NA / newest
  arrange(Ticker, Date) %>%
  group_by(Ticker, Date) %>%
  summarise(
    total_assets = dplyr::coalesce(max(total_assets, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )
View(Total_assets_values)


#####merge and impute Total_assets######

df <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/df2.xlsx")

library(lubridate)

df$Date <- format(df$Date, "%Y-%m")
View(df)

library(dplyr)

Total_assets_values <- df %>%
  left_join(Total_assets_values, by = c("Date", "Ticker"))

View(Total_assets_values)

#identify problematic tickers (only NAs)

na_total_assets <- Total_assets_values %>%
  group_by(Ticker) %>%
  summarise(non_na_count = sum(!is.na(total_assets)),
            .groups = "drop") %>%
  filter(non_na_count == 0) %>%
  pull(Ticker)

na_total_assets

#remove problematic tickers

rm_total_assets <- c(
  "1088123D SW Equity",
  "2601855D SW Equity",
  "ARAO SW Equity",
  "BACP SW Equity",
  "CSM SW Equity",
  "EDI SW Equity",
  "EDIN SW Equity",
  "GCZ SW Equity",
  "GNT SW Equity",
  "GOT SW Equity",
  "HAB SW Equity",
  "IFTN SW Equity",
  "IHSN SW Equity",
  "KAMM SW Equity",
  "KONF SW Equity",
  "LIMN SW Equity",
  "MIRN SW Equity",
  "MOVE SW Equity",
  "OMN SW Equity",
  "OMNP SW Equity",
  "ORI SW Equity",
  "RIV SW Equity",
  "SYMS SW Equity",
  "TAGN SW Equity"
)


Total_assets_values <- Total_assets_values %>%
  filter(!Ticker %in% rm_total_assets)


View(Total_assets_values)

#IMPUTE/forward fill
library(dplyr)
library(zoo)


#####merged

Total_assets_values <- Total_assets_values %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    # Identify first non-NA index
    first_valid = match(TRUE, !is.na(total_assets)),
    
    # Forward fill
    value_ffill = na.locf(total_assets, na.rm = FALSE),
    
    # Count months since last observed value
    last_obs_date = if_else(!is.na(total_assets), Date, NA_character_),
    last_obs_date = na.locf(last_obs_date, na.rm = FALSE),
    months_since_obs = as.numeric(as.yearmon(Date) - as.yearmon(last_obs_date)) * 12,
    
    # Apply rules
    value_imputed = case_when(
      row_number() < first_valid ~ NA_real_,                    
      months_since_obs <= 12 ~ value_ffill,                     
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

View(Total_assets_values)

Total_assets_values <- Total_assets_values %>%
  select(Date, Ticker, value_imputed)

View(Total_assets_values)


##########################VALUE##############################################################


value_values <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/value_values.xlsx")


value_values <- value_values %>% select(-1)

View(value_values)


#convert to year.month format
date_cols <- seq(1, ncol(value_values), by = 2)

value_values[date_cols] <- lapply(value_values[date_cols], function(x) {
  x_numeric <- as.numeric(x)
  as.Date(x_numeric, origin = "1899-12-30")
})

value_values[date_cols] <- lapply(value_values[date_cols], function(x) {
  format(x, "%Y-%m")
})


# next steps- convert to wide format
date_cols <- seq(1, ncol(value_values), by = 2)
val_cols  <- seq(2, ncol(value_values), by = 2)

Value_Values <- map2_dfr(date_cols, val_cols, function(dcol, vcol) {
  tibble(
    Date   = value_values[[dcol]],
    Ticker = names(value_values)[vcol],
    value_values    = as.numeric(value_values[[vcol]])
  )
}) 

View(Value_Values)

##deal with NAs
Value_Values <- Value_Values %>%
  tidyr::drop_na()

View(Value_Values)

#####merge######

df <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/df2.xlsx")

library(lubridate)

df$Date <- format(df$Date, "%Y-%m")
View(df)

library(dplyr)

merged_df_total_equity <- df %>%
  left_join(Value_Values, by = c("Date", "Ticker"))



#identify problematic tickers (only NAs)

tickers_only_na <- merged_df_total_equity %>%
  group_by(Ticker) %>%
  summarise(non_na_count = sum(!is.na(value_values)),
            .groups = "drop") %>%
  filter(non_na_count == 0) %>%
  pull(Ticker)

tickers_only_na
#remove problematic tickers

tickers_to_remove <- c(
  "1088123D SW Equity",
  "1583423D SW Equity",
  "2601855D SW Equity",
  "77639Q SW Equity",
  "AEVS SW Equity",
  "ARAO SW Equity",
  "BACP SW Equity",
  "CSM SW Equity",
  "EDI SW Equity",
  "EDIN SW Equity",
  "GCZ SW Equity",
  "GNT SW Equity",
  "GOT SW Equity",
  "HAB SW Equity",
  "IFTN SW Equity",
  "IHSN SW Equity",
  "KAMM SW Equity",
  "KONF SW Equity",
  "LIMN SW Equity",
  "MIRN SW Equity",
  "OMN SW Equity",
  "OMNP SW Equity",
  "ORI SW Equity",
  "RIV SW Equity",
  "SYMS SW Equity",
  "TAGN SW Equity"
)


merged_df_total_equity <- merged_df_total_equity %>%
  filter(!Ticker %in% tickers_to_remove)


View(merged_df_total_equity)

###IMPUTATION###########################################################################
library(dplyr)
library(zoo)


#####merged

merged_df_total_equity <- merged_df_total_equity %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    # Identify first non-NA index
    first_valid = match(TRUE, !is.na(value_values)),
    
    # Forward fill
    value_ffill = na.locf(value_values, na.rm = FALSE),
    
    # Count months since last observed value
    last_obs_date = if_else(!is.na(value_values), Date, NA_character_),
    last_obs_date = na.locf(last_obs_date, na.rm = FALSE),
    months_since_obs = as.numeric(as.yearmon(Date) - as.yearmon(last_obs_date)) * 12,
    
    # Apply rules
    value_imputed = case_when(
      row_number() < first_valid ~ NA_real_,                    
      months_since_obs <= 12 ~ value_ffill,                    
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

View(merged_df_total_equity)

merged_df_total_equity <- merged_df_total_equity %>%
  select(Date, Ticker, value_imputed)

View(merged_df_total_equity)

#add back the company names we initially removed to do the imputation. By doing a left join between the df and "merged_df_total_equity"
library(dplyr)
merged_df_total_equity <- df %>%
  left_join(merged_df_total_equity, by = c("Date", "Ticker"))

View(merged_df_total_equity)




####################FINAL MERGED DATASET##################################################
library(dplyr)

final_merged_df <- merged_df_total_equity %>%
  
  left_join(long_market_cap_values,
            by = c("Date", "Ticker")) %>%
  
  left_join(op_long,
            by = c("Date", "Ticker")) %>%
  
  left_join(long_returns_values,
            by = c("Date", "Ticker")) %>%
  
  left_join(Total_assets_values,
            by = c("Date", "Ticker"))

View(final_merged_df)

final_merged_df <- final_merged_df %>%
  rename(
    equity     = value_imputed.x,
    tot_assets = value_imputed.y
  )

final_merged_df <- final_merged_df %>%
  filter(!(Ticker == "GRKP SW Equity" & Date < "2006-04")) %>%
  filter(!(Ticker == "SVBZPN SW Equity"))


View(final_merged_df)
final_merged_df <- final_merged_df %>%
  group_by(Ticker) %>%
  filter(!all(is.na(returns))) %>%
  ungroup()

#rm year column
final_merged_df <- final_merged_df %>%
  select(-year)


View(final_merged_df)

####################transform total return index level to monthly returns 


final_merged_DF <- final_merged_df %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    ret = (returns / lag(returns)) - 1
  ) %>%
  ungroup()

#check the new ret
final_merged_DF %>%
  arrange(Ticker) %>%
  select(Date, Ticker, returns, ret) %>%
  print(n = 500)


View(final_merged_DF)


############################data checks################

# no duplicates
final_merged_df %>%
  count(Date, Ticker) %>%
  filter(n > 1)
# remove pure NAs
library(dplyr)

final_merged_df <- final_merged_df %>%
  filter(
    !(is.na(equity) &
        is.na(mark_cap) &
        is.na(op_prof) &
        is.na(returns) &
        is.na(tot_assets))
  )

View(final_merged_df)

saveRDS(final_merged_df, "final_merged_df")
final_merged_df <- readRDS("final_merged_df")

saveRDS(final_merged_DF, "final_merged_DF")
final_merged_DF <- readRDS("final_merged_DF")
final_merged_DF

# count of full entries
final_merged_DF %>%
  summarise(
    n_complete = sum(
      !is.na(equity) &
        !is.na(mark_cap) &
        !is.na(op_prof) &
        !is.na(returns) &
        !is.na(tot_assets)
    )
  )

# Reasonable cross-sectional size per month
library(dplyr)

final_merged_df %>%
  group_by(Date) %>%
  summarise(
    n_value   = sum(!is.na(equity)),
    n_mcap    = sum(!is.na(mark_cap)),
    n_prof    = sum(!is.na(op_prof)),
    n_ret     = sum(!is.na(returns)),
    n_assets  = sum(!is.na(tot_assets)),
    .groups = "drop"
  ) %>%
  arrange(Date) %>%
  print(n = 313)

write_xlsx(final_merged_df, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/final_merged_df.xlsx")

write_xlsx(final_merged_DF, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/final_merged_DF.xlsx")
#plot data###############
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

monthly_counts <- final_merged_DF %>%
  group_by(Date) %>%
  summarise(
    equity  = sum(!is.na(equity)),
    mcap    = sum(!is.na(mark_cap)),
    prof    = sum(!is.na(op_prof)),
    ret     = sum(!is.na(ret)),   
    assets  = sum(!is.na(tot_assets)),
    .groups = "drop"
  )


monthly_counts$Date <- ym(monthly_counts$Date)


monthly_counts_long <- monthly_counts %>%
  pivot_longer(
    cols = -Date,
    names_to = "variable",
    values_to = "count"
  )


ggplot(monthly_counts_long, aes(x = Date, y = count, color = variable)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Non-NA Observations per Month Across Variables",
    x = "Date",
    y = "Number of Tickers"
  ) +
  theme_minimal()

ggplot(monthly_counts_long, aes(x = Date, y = count, color = variable)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Non-NA Observations per Month Across Variables",
    x = "Date",
    y = "Number of Tickers"
  ) +
  theme_minimal()


#plot sums over time

monthly_totals <- final_merged_DF %>%
  group_by(Date) %>%
  summarise(
    sum_equity   = sum(equity, na.rm = TRUE),
    sum_assets   = sum(tot_assets, na.rm = TRUE),
    sum_mcap     = sum(mark_cap, na.rm = TRUE),
    avg_return   = mean(returns, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Date) %>%
  mutate(Date = as.Date(paste0(Date, "-01")))


ggplot(monthly_totals, aes(x = Date, y = avg_return)) +
  geom_line(linewidth = 1) +
  labs(
    title = "returns",
    x = "Date",
    y = "returns"
  ) +
  theme_minimal()

#checking data breaks
final_merged_DF
monthly_totals <- final_merged_DF %>%
  group_by(Date) %>%
  summarise(
    sum_equity = sum(equity, na.rm = TRUE),
    sum_assets = sum(tot_assets, na.rm = TRUE),
    sum_mcap   = sum(mark_cap, na.rm = TRUE),
    avg_return = mean(returns, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Date) %>%
  mutate(Date = as.Date(paste0(Date, "-01")))

library(tidyr)

monthly_totals  %>%
  print (n=313)

monthly_totals_long <- monthly_totals %>%
  pivot_longer(
    cols = -Date,
    names_to = "variable",
    values_to = "value"
  )

#table

monthly_totals_long_df <- monthly_totals_long %>%
  filter(variable == "sum_mcap")

monthly_totals_long_df  %>%
  print (n=313)

#plot################################################################



library(ggplot2)
library(dplyr)

monthly_totals_long %>%
  filter(!is.na(value), !is.na(Date)) %>%
  ggplot(aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Aggregate Variables Over Time",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal()

####count

#mrkt cap
final_merged_df %>%
  group_by(Date) %>%
  summarise(
    sum_mcap = sum(mark_cap, na.rm = TRUE),
    .groups = "drop"
  )  %>%
  print(n=313)

final_merged_df %>%
  filter(Date == "1999-12",
         Ticker == "GRKP SW Equity")

monthly_totals <- final_merged_df %>%
  group_by(Date) %>%
  summarise(
    n_firms      = sum(!is.na(returns)),
    sum_equity   = sum(equity, na.rm = TRUE),
    sum_assets   = sum(tot_assets, na.rm = TRUE),
    sum_mcap     = sum(mark_cap, na.rm = TRUE),
    #avg_return   = mean(returns, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange (Date)
print(n = 50) 
class(final_merged_df$Date)



# check why return is so much lower
final_merged_df %>%
  filter(is.na(returns)) %>%
  print(n = 8133)

final_merged_df %>%
  filter(is.na(returns)) %>%
  summarise(n_unique_tickers = n_distinct(Ticker))

final_merged_df %>%
  filter(is.na(returns)) %>%
  count(Ticker, sort = TRUE) %>%
  print (n = 134)

final_merged_df %>%
  filter(is.na(returns) & !is.na(mark_cap)) %>%
  arrange(Ticker, Date)


count non NA observations from "long_returns_values" for the date 2025-01, 2025-06, and 2025-12


long_returns_values %>%
  filter(Date %in% c("2005-01", "2005-06", "2005-12")) %>%
  group_by(Date) %>%
  summarise(
    n_non_na_returns = sum(!is.na(returns)),
    .groups = "drop"
  )

#check data break
final_merged_DF %>%
  group_by(Date) %>%
  summarise(
    n_assets = sum(!is.na(tot_assets)),
    sum_tot_assets = sum(tot_assets, na.rm = TRUE),
    med_tot_assets = median(tot_assets, na.rm = TRUE),
    .groups="drop"
  ) %>%
  filter(Date >= "2000-12", Date <= "2001-06") %>%
  print(n=50)

#check missing companies during break- totassets
big_firms <- final_merged_df %>%
  filter(Date == "2001-02") %>%
  arrange(desc(tot_assets)) %>%
  select(Ticker, tot_assets) %>%
  head(10)

big_firms

big_firms_apr <- final_merged_df %>%
  filter(Date == "2001-03") %>%
  arrange(desc(tot_assets)) %>%
  select(Ticker, tot_assets) %>%
  head(10)

big_firms_apr

GRKP cause of break!! sol: remove data from before the break

#check missing companies during break- total assets  

#check missing companies during break- markcap
big_firms <- final_merged_DF %>%
  filter(Date == "2012-11") %>%
  arrange(desc(tot_assets)) %>%
  select(Ticker, tot_assets) %>%
  head(10)

big_firms

big_firms_apr <- final_merged_DF %>%
  filter(Date == "2012-12") %>%
  arrange(desc(tot_assets)) %>%
  select(Ticker, tot_assets) %>%
  head(10)

big_firms_apr

#check data break
final_merged_DF %>%
  group_by(Date) %>%
  summarise(
    n_assets = sum(!is.na(tot_assets)),
    sum_assets = sum(tot_assets, na.rm = TRUE),
    med_assets = median(tot_assets, na.rm = TRUE),
    .groups="drop"
  ) %>%
  #filter(Date >= "2008-11", Date <= "2008-12") %>%
  print(n=313)

#identify jump and fall
new_assets_2009 <- final_merged_df %>%
  filter(Date %in% c("2008-11", "2008-12")) %>%
  select(Date, Ticker, tot_assets) %>%
  tidyr::pivot_wider(names_from = Date, values_from = tot_assets) %>%
  mutate(appeared = is.na(`2009-02`) & !is.na(`2009-03`)) %>%
  filter(appeared == TRUE) %>%
  arrange(desc(`2009-03`))

new_assets_2009 %>% head(20)

# SNBN SW Equity present in 2009-03 but not 2009-02, present in 2010-03 but not 2010-04. enters and disapears within a year
disappeared_2010 <- final_merged_df %>%
  filter(Date %in% c("2010-03", "2010-04")) %>%
  select(Date, Ticker, tot_assets) %>%
  tidyr::pivot_wider(names_from = Date, values_from = tot_assets) %>%
  mutate(disappeared = !is.na(`2010-03`) & is.na(`2010-04`)) %>%
  filter(disappeared == TRUE) %>%
  arrange(desc(`2010-03`))

disappeared_2010 %>% head(20)

big_firms <- final_merged_DF %>%
  filter(Date == "2013-02") %>%
  arrange(desc(tot_assets)) %>%
  select(Ticker, tot_assets) %>%
  print (n=10)
big_firms

big_firms_mar <- final_merged_DF %>%
  filter(Date == "2013-03") %>%
  arrange(desc(tot_assets)) %>%
  select(Ticker, tot_assets) %>%
  print (n=10)

big_firms_mar

# asset change

library(dplyr)
library(tidyr)

library(dplyr)
library(tidyr)

asset_change <- final_merged_df %>%
  filter(Date %in% c("2008-11", "2008-12")) %>%
  select(Ticker, Date, tot_assets) %>%
  pivot_wider(names_from = Date, values_from = tot_assets) %>%
  mutate(
    `2008-02` = replace_na(`2008-02`, 0),
    `2008-03` = replace_na(`2008-03`, 0),
    change = `2008-03` - `2008-02`
  ) %>%
  arrange(desc(change))

print(asset_change, n = 20)

asset_change

remove SVBZPN SW Equity

####################transform total return index level to monthly returns 
final_merged_DF <- final_merged_df %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    ret = (returns / lag(returns)) - 1
  ) %>%
  ungroup()

#check the new ret
final_merged_DF %>%
  arrange(Ticker) %>%
  select(Date, Ticker, returns, ret) %>%
  print(n = 500)


View(final_merged_DF)


##############cross sectionality#######################################################
library(dplyr)
library(lubridate)

library(dplyr)

final_merged_DF

cs_counts <- final_merged_DF %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    tot_assets_lag1 = lag(tot_assets, 1),
    mcap_lag1       = lag(mark_cap, 1)   # needed for MKT weights
  ) %>%
  ungroup() %>%
  group_by(Date) %>%
  summarise(
    # Return usable (current month)
    n_ret = sum(!is.na(ret)),
    
    # MKT factor usable inputs: need ret_t and mcap_{t-1}
    n_mkt_inputs = sum(!is.na(ret) & !is.na(mcap_lag1)),
    
    # Size sorting universe (ME available)
    n_size_inputs = sum(!is.na(mark_cap)),
    
    # Value sorting universe (inputs to compute BM: equity / mark_cap)
    n_value_inputs = sum(!is.na(equity) & !is.na(mark_cap)),
    
    # Profitability sorting universe (inputs to compute OP/BE: op_prof / equity)
    n_prof_inputs  = sum(!is.na(op_prof) & !is.na(equity)),
    
    # Investment sorting universe (inputs to compute asset growth: A_t and A_{t-1})
    n_inv_inputs   = sum(!is.na(tot_assets) & !is.na(tot_assets_lag1)),
    
    # Fully usable for ALL factors + returns (raw-input version)
    n_all_inputs_and_ret = sum(
      !is.na(ret) &
        !is.na(mark_cap) &
        !is.na(equity) &
        !is.na(op_prof) &
        !is.na(tot_assets) &
        !is.na(tot_assets_lag1) &
        !is.na(mcap_lag1)        # include this if you want “also market-factor ready”
    ),
    
    .groups = "drop"
  ) %>%
  arrange(Date)

View(cs_counts)



write_xlsx(cs_counts, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/cs_counts_1.xlsx")


summary(cs_counts)


######FIGURE 1###################

install.packages("dplyr")
install.packages("lubridate")
library(dplyr)
library(ggplot2)
library(lubridate)

final_merged_df <- readRDS("final_merged_df")
final_merged_df

company_counts <- final_merged_df %>%
  filter(!is.na(returns)) %>%
  group_by(Date) %>%
  summarise(n_companies = n_distinct(Ticker), .groups = "drop") %>%
  mutate(Date = as.Date(paste0(Date, "-01")))


p <- ggplot(company_counts, aes(x = Date, y = n_companies)) +
  geom_line(linewidth = 0.5, color = "black") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  expand_limits(y = 0) +
  labs(
    x = "Date",
    y = "Number of Companies"
  ) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.length.x = unit(0.15, "cm"),
    axis.ticks.length.y = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p

######FIGURE 2###################
library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)
library(grid)

library(ggplot2)
library(dplyr)
library(grid)

#try 4- Novarits
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)




#try 6

View(final_merged_DF)

p <- final_merged_DF %>%
  filter(Ticker == "NOVN SW Equity") %>%
  mutate(
    Date = as.Date(paste0(Date, "-01")),
    tot_assets = tot_assets / 1e3,
    equity     = equity / 1e3,
    mark_cap   = mark_cap / 1e3,
    op_prof    = op_prof / 1e3
  ) %>%
  select(Date, equity, mark_cap, op_prof, returns, tot_assets) %>%
  pivot_longer(
    cols = -Date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    variable = factor(
      variable,
      levels = c("returns", "tot_assets", "equity", "mark_cap", "op_prof"),
      labels = c(
        "Panel A- Total Return Index",
        "Panel B- Total Assets (Bn CHF)",
        "Panel C- Book Equity (Bn CHF)",
        "Panel D- Market Cap (Bn CHF)",
        "Panel E- Operating Profit (Bn CHF)"
      )
    )
  ) %>%
  ggplot(aes(x = Date, y = value)) +
  geom_line(linewidth = 0.5, color = "black") +
  facet_wrap(
    ~ variable,
    ncol = 2,
    scales = "free_y",
    axes = "all",
    axis.labels = "all"
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4)
  ) +
  labs(
    x = "Date",
    y = NULL
  ) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 14, color = "black"),
    strip.background = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.length.x = unit(0.15, "cm"),
    axis.ticks.length.y = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p

#factor construction
df <- readRDS("final_merged_DF")

View(df)

install.packages("writexl")
library(writexl)
write_xlsx(df, "df_clean.xlsx")


df2 <- readRDS("final_merged_df")
df2
View(df)
library(dplyr)
library(tidyr)

prev_distinct <- function(x) {
  # a "new run" begins whenever:
  #  - current value is non-NA AND
  #  - either previous was NA OR value differs from previous
  changed <- !is.na(x) & (is.na(dplyr::lag(x)) | x != dplyr::lag(x))
  
  run <- cumsum(replace_na(changed, FALSE))  # run id increments on each change
  
  # first value of each run (the level at the start of that run)
  run_val <- tapply(x, run, function(v) v[which(!is.na(v))[1]])
  
  # previous run's level
  prev_run_val <- dplyr::lag(as.numeric(run_val))
  
  # map previous-run value back to each row
  out <- prev_run_val[match(run, as.integer(names(run_val)))]
  
  # if a row is NA, keep tminus1 as NA (optional, but usually sensible)
  out[is.na(x)] <- NA_real_
  
  out
}

library(dplyr)

prev_distinct <- function(x) {
  # a "new run" begins whenever:
  #  - current value is non-NA AND
  #  - either previous was NA OR value differs from previous
  changed <- !is.na(x) & (is.na(dplyr::lag(x)) | x != dplyr::lag(x))
  
  run <- cumsum(replace_na(changed, FALSE))  # run id increments on each change
  
  # first value of each run (the level at the start of that run)
  run_val <- tapply(x, run, function(v) v[which(!is.na(v))[1]])
  
  # previous run's level
  prev_run_val <- dplyr::lag(as.numeric(run_val))
  
  # map previous-run value back to each row
  out <- prev_run_val[match(run, as.integer(names(run_val)))]
  
  # if a row is NA, keep tminus1 as NA (optional, but usually sensible)
  out[is.na(x)] <- NA_real_
  
  out
}

df <- df %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    equity_tminus1     = prev_distinct(equity),
    mark_cap_tminus1 = lag(mark_cap),
    op_prof_tminus1    = prev_distinct(op_prof),
    tot_assets_tminus1 = prev_distinct(tot_assets)
  ) %>%
  ungroup()

df <- df %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    tot_assets_tminus2 = prev_distinct(tot_assets_tminus1)
  ) %>%
  ungroup()

df

View(df)

#construct factors
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)

# df has: Date (YYYY-MM), Ticker, ret (or monthly return), and the tminus columns:
# equity_tminus1, mark_cap_tminus1, op_prof_tminus1, tot_assets_tminus1, tot_assets_tminus2

df_factors_june <- df %>%
  mutate(
    # make sure Date is a proper monthly date
    Date_m = as.Date(as.yearmon(Date, "%Y-%m")),
    year   = year(Date_m),
    month  = month(Date_m),
    
    # construct characteristics (as-of / lagged)
    BM  = equity_tminus1 / mark_cap_tminus1,
    OP  = op_prof_tminus1 / equity_tminus1,
    INV = (tot_assets_tminus1 - tot_assets_tminus2) / tot_assets_tminus2
  ) %>%
  # keep only June observations (the sorting month)
  filter(month == 6) %>%
  select(Date = Date_m, year, Ticker, ret, BM, OP, INV,
         equity_tminus1, mark_cap_tminus1, op_prof_tminus1,
         tot_assets_tminus1, tot_assets_tminus2)

df_factors_june
View(df_factors_june)

#check cross sectionality
library(dplyr)

cs_counts <- df_factors_june %>%
  #filter(month(Date) == 6) %>%   # keep June only
  group_by(year) %>%
  summarise(
    n_mark_cap = sum(!is.na(mark_cap_tminus1)),
    n_BM   = sum(!is.na(BM)),
    n_OP   = sum(!is.na(OP)),
    n_INV  = sum(!is.na(INV))
  ) %>%
  arrange(year)

View(cs_counts)

####################################################################################################################################

library(dplyr)

df_factors_june <- df_factors_june %>%
  group_by(year) %>%
  mutate(
    
    # 2 groups for Size
    size_median = median(mark_cap_tminus1, na.rm = TRUE),
    SIZE_port = ifelse(mark_cap_tminus1 <= size_median, "Small", "Big"),
    
    # 3 groups for BM: Low / Neutral / High
    BM_30 = quantile(BM, 0.3, na.rm = TRUE),
    BM_70 = quantile(BM, 0.7, na.rm = TRUE),
    BM_port = case_when(
      is.na(BM) ~ NA_character_,
      BM <= BM_30 ~ "Low",
      BM <= BM_70 ~ "Neutral",
      BM >  BM_70 ~ "High"
    ),
    
    # 3 groups for OP: Weak / Neutral / Robust
    OP_30 = quantile(OP, 0.3, na.rm = TRUE),
    OP_70 = quantile(OP, 0.7, na.rm = TRUE),
    OP_port = case_when(
      is.na(OP) ~ NA_character_,
      OP <= OP_30 ~ "Weak",
      OP <= OP_70 ~ "Neutral",
      OP >  OP_70 ~ "Robust"
    ),
    
    # 3 groups for INV: Aggressive / Neutral / Conservative
    # Low INV = Conservative, High INV = Aggressive
    INV_30 = quantile(INV, 0.3, na.rm = TRUE),
    INV_70 = quantile(INV, 0.7, na.rm = TRUE),
    INV_port = case_when(
      is.na(INV) ~ NA_character_,
      INV <= INV_30 ~ "Conservative",
      INV <= INV_70 ~ "Neutral",
      INV >  INV_70 ~ "Aggressive"
    )
    
  ) %>%
  ungroup() %>%
  select(-size_median, -BM_30, -BM_70, -OP_30, -OP_70, -INV_30, -INV_70)

df_factors_june <- df_factors_june %>%
  mutate(
    BM_portfolio  = paste(SIZE_port, BM_port),
    OP_portfolio  = paste(SIZE_port, OP_port),
    INV_portfolio = paste(SIZE_port, INV_port)
  )

df_factors_june <- df_factors_june %>%
  mutate(
    BM_portfolio  = ifelse(is.na(SIZE_port) | is.na(BM_port),  NA, BM_portfolio),
    OP_portfolio  = ifelse(is.na(SIZE_port) | is.na(OP_port),  NA, OP_portfolio),
    INV_portfolio = ifelse(is.na(SIZE_port) | is.na(INV_port), NA, INV_portfolio)
  )
df_factors_june

View(df_factors_june)

#check cross sectionality
cs_counts_1 <- df_factors_june %>%
  #filter(month(Date) == 6) %>%   # keep June only
  group_by(year) %>%
  summarise(
    n_size = sum(!is.na(SIZE_port)),
    n_BM   = sum(!is.na(BM_portfolio)),
    n_OP   = sum(!is.na(OP_portfolio)),
    n_INV  = sum(!is.na(INV_portfolio))
  ) %>%
  arrange(year)

View(cs_counts_1)
summary(cs_counts_1)

##merge the datasets together

library(dplyr)
library(lubridate)
df

df <- df %>%
  mutate(Date = ymd(paste0(Date, "-01")))

df <- df %>%
  mutate(
    year = year(Date),
    month = month(Date),
    sort_year = ifelse(month >= 7, year, year - 1)
  ) 

df
View(df)

####merge df and df_factors_june
#rename col name year to sort_year from df_factors_june

df_factors_june <- df_factors_june %>%
  rename(sort_year = year)

###merge df (left data set) with df_factors_june by ticker and sort_year
library(dplyr)

df_merged_factors <- df %>%
  left_join(
    df_factors_june,
    by = c("Ticker", "sort_year")
  )

View(df_merged_factors)
colnames

df_merged_factors <- df_merged_factors %>%
  select(Date = Date.x, Ticker, mark_cap_tminus1 = mark_cap_tminus1.x, ret = ret.x, SIZE_port, BM_portfolio, OP_portfolio, INV_portfolio)

View(df_merged_factors)

library(writexl)

write_xlsx(df_merged_factors, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/df_factors_june.xlsx")


#-----------------------------#
# 1) BM portfolios -> HML, SMB_BM
#-----------------------------#

BM_returns <- df_merged_factors %>%
  filter(!is.na(BM_portfolio), !is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date, BM_portfolio) %>%
  summarise(
    port_ret = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  ) %>%
  print(n=1854)

View(BM_returns)

write_xlsx(BM_returns, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/BM_returns.xlsx")

BM_returns_wide <- BM_returns %>%
  select(Date, BM_portfolio, port_ret) %>%
  pivot_wider(
    names_from = BM_portfolio,
    values_from = port_ret
  ) %>%
  mutate(
    HML = 0.5 * (`Small High` + `Big High`) -
      0.5 * (`Small Low`  + `Big Low`),
    
    SMB_BM = (1/3) * (`Small Low` + `Small Neutral` + `Small High`) -
      (1/3) * (`Big Low`  + `Big Neutral`  + `Big High`),
    
    SMALL_BM = (`Small High`+ `Small Low` + `Small Neutral`),
    
    BIG_BM = (`Big Low`  + `Big Neutral`  + `Big High`),
    
    HIGH_BM = (`Small High`+`Big High`),
    
    LOW_BM = (`Small Low` + `Big Low`)
  )

BM_returns_wide
View(BM_returns_wide)

write_xlsx(BM_returns_wide, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/BM_returns_wide.xlsx")
#-----------------------------#
# 2) OP portfolios -> RMW, SMB_OP
#-----------------------------#

OP_returns <- df_merged_factors %>%
  filter(!is.na(OP_portfolio), !is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date, OP_portfolio) %>%
  summarise(
    port_ret = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  )

OP_returns_wide <- OP_returns %>%
  select(Date, OP_portfolio, port_ret) %>%
  pivot_wider(
    names_from = OP_portfolio,
    values_from = port_ret
  ) %>%
  mutate(
    RMW = 0.5 * (`Small Robust` + `Big Robust`) -
      0.5 * (`Small Weak`  + `Big Weak`),
    
    SMB_OP = (1/3) * (`Small Weak` + `Small Neutral` + `Small Robust`) -
      (1/3) * (`Big Weak`  + `Big Neutral`  + `Big Robust`),
    
    SMALL_OP = (`Small Weak`+ `Small Neutral`+`Small Robust`),
    
    BIG_OP = (`Big Weak`  + `Big Neutral`  + `Big Robust`),
    
    ROBUST_OP = (`Small Robust` + `Big Robust`),
    
    WEAK_OP = (`Small Weak` + `Big Weak`)
  )

OP_returns_wide %>%
  filter(if_any(everything(), is.na))

write_xlsx(OP_returns_wide, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/OP_returns_wide.xlsx")

#-----------------------------#
# 3) INV portfolios -> CMA, SMB_INV
#-----------------------------#

INV_returns <- df_merged_factors %>%
  filter(!is.na(INV_portfolio), !is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date, INV_portfolio) %>%
  summarise(
    port_ret = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  )

View(INV_returns)

INV_returns_wide <- INV_returns %>%
  select(Date, INV_portfolio, port_ret) %>%
  pivot_wider(
    names_from = INV_portfolio,
    values_from = port_ret
  ) %>%
  mutate(
    CMA = 0.5 * (`Small Conservative` + `Big Conservative`) -
      0.5 * (`Small Aggressive`   + `Big Aggressive`),
    
    SMB_INV = (1/3) * (`Small Conservative` + `Small Aggressive` + `Small Neutral`) -
      (1/3) * (`Big Aggressive`  + `Big Conservative` + `Big Neutral`),
    
    SMALL_INV = (`Small Aggressive`+ `Small Conservative` + `Small Neutral`),
    
    BIG_INV = (`Big Aggressive`  + `Big Conservative` + `Big Neutral`),
    
    CONSERVATIVE_INV = (`Small Conservative` + `Big Conservative`),
    
    AGGRESSIVE_INV = (`Small Aggressive` + `Big Aggressive`)
  )

write_xlsx(INV_returns_wide, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/INV_returns_wide.xlsx")
View(INV_returns_wide)

#-----------------------------#
# 4) Merge all factor pieces
#-----------------------------#

factors_vw <- BM_returns_wide %>%
  select(Date, HML, SMB_BM) %>%
  left_join(
    OP_returns_wide %>% select(Date, RMW, SMB_OP),
    by = "Date"
  ) %>%
  left_join(
    INV_returns_wide %>% select(Date, CMA, SMB_INV),
    by = "Date"
  ) %>%
  filter(Date >= as.Date("2001-07-01")) %>%
  mutate(
    SMB = (SMB_BM + SMB_OP + SMB_INV) / 3
  ) %>%
  arrange(Date)

factors_vw %>%
  print (n=306)

View(factors_vw)
library(dplyr)


write_xlsx(factors_vw, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/factors_vw.xlsx")

factors_vw %>%
  summarise(across(-Date, mean, na.rm = TRUE))

#Calculate Rm

df_clean <- df_merged_factors %>%
  filter(!is.na(ret), !is.na(mark_cap_tminus1))
df_clean
Rm <- df_clean %>%
  group_by(Date) %>%
  summarise(
    Rm = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE)
  )

Rm %>%
  print(n=319)



factors_final <- Rm %>%
  left_join(factors_vw, by = c("Date"))
factors_final
View(factors_final)

write_xlsx(factors_final, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/factors_final.xlsx")

#plot
library(ggplot2)
ggplot(factors_final, aes(x = Date, y = Rm)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Average market returns",
    x = "Date",
    y = "Returns"
  ) +
  theme_minimal()


###Average returns 2x3####


View(colMeans(dplyr::select(factors_final, where(is.numeric)), na.rm = TRUE))

avg_returns <- BM_returns_wide %>%
  select(Date, HML, SMB_BM, SMALL_BM, BIG_BM, HIGH_BM, LOW_BM) %>%
  left_join(
    OP_returns_wide %>% select(Date, RMW, SMB_OP, SMALL_OP, BIG_OP, ROBUST_OP, WEAK_OP),
    by = "Date"
  ) %>%
  left_join(
    INV_returns_wide %>% select(Date, CMA, SMB_INV, SMALL_INV, BIG_INV, CONSERVATIVE_INV, AGGRESSIVE_INV),
    by = "Date"
  ) %>%
  filter(Date >= as.Date("2001-07-01")) %>%
  mutate(
    SMB = ((SMB_BM + SMB_OP + SMB_INV) / 3),
    SMALL = ((SMALL_BM + SMALL_OP + SMALL_INV)/3),
    BIG = ((BIG_BM + BIG_OP + BIG_INV)/3)
  ) %>%
  arrange(Date)

avg_returns %>%
  print (n=306)

write_xlsx(avg_returns, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/avg_returns.xlsx")

(colMeans(dplyr::select(avg_returns, where(is.numeric)), na.rm = TRUE))


######################################################################################################################################


#####retrieve external factors##########################################
library(readxl)
CPI <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/CPI.xlsx")


Long_term_interest_rate <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/Long_term_interest_rate.xlsx")
View(Long_term_interest_rate)

LT_Real_IR <- Long_term_interest_rate %>%
  full_join(CPI, by = c("TIME_PERIOD"))

LT_Real_IR <- LT_Real_IR %>%
  filter(TIME_PERIOD >= "1999-01")
View(LT_Real_IR)

#get rid of growth rates
LT_Real_IR <- LT_Real_IR %>%
  mutate(
    CPI = as.numeric(CPI),
    LT_IR = as.numeric(LT_IR)
  ) %>%
  filter(CPI >= 4)

LT_Real_IR <- LT_Real_IR %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    nominal = LT_IR / 100,
    inflation_yoy = CPI / lag(CPI, 12) - 1,
    real_IR = (1 + nominal) / (1 + inflation_yoy) - 1
  )

LT_Real_IR_clean <- LT_Real_IR %>%
  rename(Date = TIME_PERIOD) %>%
  mutate(Date = as.Date(paste0(Date, "-01"))) %>%
  select(Date, real_IR)

View(LT_Real_IR_clean)

#####SARON-Risk-free IR
library(readxl)
library(dplyr)
library(lubridate)
install.packages("writexl")
library(writexl)

SARON <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/SARON .xlsx")


View(SARON)

rf_monthly <- SARON %>%
  mutate(
    Date = as.Date(Date),
    saron = as.numeric(gsub(",", ".", SARON))
  ) %>%
  group_by(year = year(Date), month = month(Date)) %>%
  summarise(
    Rf = prod(1 + (saron/100)*(1/360), na.rm = TRUE) - 1
  )

rf_monthly <- rf_monthly %>%
  mutate(
    Date = as.Date(paste(year, month, "01", sep = "-"))
  )

rf_monthly <- rf_monthly %>%
  select(Date, Rf)

View(rf_monthly)

write_xlsx(rf_monthly, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/rf_monthly.xlsx")

rf_monthly 
mean(rf_monthly$Rf, na.rm = TRUE)


KOF <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/KOF.xlsx")
REER <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/REER.xlsx")
CHF_USD <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/CHF-USD.xlsx")

# Standardize REER
REER <- REER %>%
  mutate(Date = as.Date(paste0(Date, "-01")))  # remove if already YYYY-MM-DD
# adjust column name to match your file

# Standardize KOF
KOF <- KOF %>%
  mutate(Date = as.Date(paste0(Date, "-01")))  # adjust column name to match your file

#Standardize CHF-USD
CHF_USD <- CHF_USD %>%
  mutate(Date = as.Date(paste0(Date, "-01")))

# Full join
external_factors <- LT_Real_IR_clean %>%
  full_join(KOF, by = "Date") %>%
  full_join(REER, by = "Date") %>%
  full_join(CHF_USD, by = "Date") %>%
  arrange(Date)

View(external_factors)

##### Merge with FF5 and compute lags + dummies ##########################################


external_factors <- external_factors %>%
  mutate(
    # Lag everything by 1 month
    IR_lag     = lag(real_IR),
    S_CH_lag   = lag(KOF_CH),
    S_EU_lag   = lag(KOF_EU),
    REER_lag   = lag(REER_Change),
    CHFUSD_lag = lag(`CHF-USD Change`)
  )

FF5

colnames(external_factors)
FF5_garch <- FF5 %>%
  select(-real_IR) %>%
  left_join(external_factors, by = "Date") %>%
  arrange(Date)

FF5_garch <- FF5_garch %>%
  select(-real_IR, -KOF_CH, -KOF_EU, -REER, -REER_Change, -`CHF-USD`, -`CHF-USD Change`)


View(FF5_garch)

colnames(FF5_garch)

# Median splits for levels: High / Low
FF5_garch <- FF5_garch %>%
  mutate(
    # Interest rate
    IR_H = ifelse(IR_lag >= median(IR_lag, na.rm = TRUE), 1, 0),
    IR_L = 1 - IR_H,
    
    # Sentiment Swiss (only valid from 2007+)
    S_CH_H = ifelse(S_CH_lag >= median(S_CH_lag, na.rm = TRUE), 1, 0),
    S_CH_L = 1 - S_CH_H,
    
    # Sentiment EU
    S_EU_H = ifelse(S_EU_lag >= median(S_EU_lag, na.rm = TRUE), 1, 0),
    S_EU_L = 1 - S_EU_H
  )
View(FF5_garch)
# Positive / Negative splits for changes: Appreciation / Depreciation
FF5_garch <- FF5_garch %>%
  mutate(
    # REER: positive = CHF appreciated
    REER_pos = ifelse(REER_lag > 0, 1, 0),
    REER_neg = 1 - REER_pos,
    
    # CHF-USD: check direction first (see sanity check below)
    CHFUSD_pos = ifelse(CHFUSD_lag > 0, 1, 0),
    CHFUSD_neg = 1 - CHFUSD_pos
  )

View(FF5_garch)

write_xlsx(FF5_garch, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/FF5_garch.xlsx")

# Check direction: Jan 2015 SNB shock = CHF appreciated massively
FF5_garch %>%
  filter(Date >= as.Date("2015-01-01"), Date <= as.Date("2015-03-01")) %>%
  select(Date, REER_lag, CHFUSD_lag, REER_pos, CHFUSD_pos)

########## PHASE 2: REGIME DUMMY REGRESSIONS ##########################################################

########## PHASE 2: REGIME DUMMY REGRESSIONS ##########################################################

library(dplyr)
library(tidyr)
library(moments)
library(tseries)

sig_code <- function(p) {
  cut(p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", ""),
      right = TRUE)
}

# ---- Helper function: run FF5 regression on a subset and extract results ---- #
run_ff5_regression <- function(data, excess_cols, label) {
  
  models <- lapply(excess_cols, function(dep_var) {
    lm(
      as.formula(paste(dep_var, "~ Mkt_excess + SMB + HML + RMW + CMA")),
      data = data
    )
  })
  names(models) <- excess_cols
  
  results <- lapply(names(models), function(name) {
    model <- models[[name]]
    smry  <- summary(model)
    coefs <- smry$coefficients
    fstat <- smry$fstatistic
    
    data.frame(
      regime    = label,
      portfolio = name,
      alpha     = coefs["(Intercept)", "Estimate"],
      sig_alpha = sig_code(coefs["(Intercept)", "Pr(>|t|)"]),
      beta_mkt  = coefs["Mkt_excess", "Estimate"],
      sig_mkt   = sig_code(coefs["Mkt_excess", "Pr(>|t|)"]),
      beta_smb  = coefs["SMB", "Estimate"],
      sig_smb   = sig_code(coefs["SMB", "Pr(>|t|)"]),
      beta_hml  = coefs["HML", "Estimate"],
      sig_hml   = sig_code(coefs["HML", "Pr(>|t|)"]),
      beta_rmw  = coefs["RMW", "Estimate"],
      sig_rmw   = sig_code(coefs["RMW", "Pr(>|t|)"]),
      beta_cma  = coefs["CMA", "Estimate"],
      sig_cma   = sig_code(coefs["CMA", "Pr(>|t|)"]),
      t_alpha   = coefs["(Intercept)", "t value"],
      t_mkt     = coefs["Mkt_excess", "t value"],
      t_smb     = coefs["SMB", "t value"],
      t_hml     = coefs["HML", "t value"],
      t_rmw     = coefs["RMW", "t value"],
      t_cma     = coefs["CMA", "t value"],
      p_alpha   = coefs["(Intercept)", "Pr(>|t|)"],
      p_mkt     = coefs["Mkt_excess", "Pr(>|t|)"],
      p_smb     = coefs["SMB", "Pr(>|t|)"],
      p_hml     = coefs["HML", "Pr(>|t|)"],
      p_rmw     = coefs["RMW", "Pr(>|t|)"],
      p_cma     = coefs["CMA", "Pr(>|t|)"],
      r2        = smry$r.squared,
      adj_r2    = smry$adj.r.squared,
      resid_se  = smry$sigma,
      f_stat    = unname(fstat[1]),
      df_num    = unname(fstat[2]),
      df_den    = unname(fstat[3]),
      model_p   = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    )
  }) %>%
    dplyr::bind_rows()
  
  return(results)
}

# ---- Helper function: Panel A (summary stats) per regime ---- #
run_panel_A <- function(data, label) {
  data %>%
    select(Mkt_excess, SMB, HML, RMW, CMA) %>%
    pivot_longer(everything(), names_to = "Factor", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(Factor) %>%
    summarise(
      Mean     = mean(value),
      Std_Dev  = sd(value),
      Sharpe   = mean(value) / sd(value) * sqrt(12),
      Skewness = skewness(value),
      Kurtosis = kurtosis(value),
      Min      = min(value),
      Max      = max(value),
      JB_pval  = jarque.bera.test(value)$p.value,
      .groups  = "drop"
    ) %>%
    mutate(
      JB_sig = case_when(
        JB_pval < 0.01 ~ "***",
        JB_pval < 0.05 ~ "**",
        JB_pval < 0.10 ~ "*",
        TRUE ~ ""
      ),
      Factor = factor(Factor, levels = c("Mkt_excess", "SMB", "HML", "RMW", "CMA")),
      regime = label
    ) %>%
    arrange(Factor)
}

# ---- Helper function: Panel B (correlation) per regime ---- #
run_panel_B <- function(data, label) {
  cor_mat <- data %>%
    select(Mkt_excess, SMB, HML, RMW, CMA) %>%
    cor(use = "complete.obs") %>%
    round(4)
  
  cor_mat <- as.data.frame(cor_mat)
  cor_mat$Factor <- rownames(cor_mat)
  cor_mat$regime <- label
  return(cor_mat)
}

# ---- Helper function: Eq 4-5 dummy regression (no intercept) ---- #
run_eq45 <- function(data, high_var, low_var, label) {
  factor_names <- c("Mkt_excess", "SMB", "HML", "RMW", "CMA")
  
  results <- lapply(factor_names, function(fac) {
    model <- lm(as.formula(paste(fac, "~ 0 +", high_var, "+", low_var)), data = data)
    smry <- summary(model)
    coefs <- smry$coefficients
    data.frame(
      regime_var = label,
      factor     = fac,
      a_H        = coefs[high_var, "Estimate"],
      t_H        = coefs[high_var, "t value"],
      p_H        = coefs[high_var, "Pr(>|t|)"],
      sig_H      = sig_code(coefs[high_var, "Pr(>|t|)"]),
      a_L        = coefs[low_var, "Estimate"],
      t_L        = coefs[low_var, "t value"],
      p_L        = coefs[low_var, "Pr(>|t|)"],
      sig_L      = sig_code(coefs[low_var, "Pr(>|t|)"]),
      diff       = coefs[high_var, "Estimate"] - coefs[low_var, "Estimate"]
    )
  }) %>% bind_rows()
  
  return(results)
}

########## EQUATIONS 4-5: FACTOR RETURNS ON REGIME DUMMIES ############################################

# Interest rate regimes
eq4_results <- run_eq45(FF5_garch, "IR_H", "IR_L", "Interest Rate")
View(eq4_results)

# Sentiment regimes (Swiss KOF)
eq5_results <- run_eq45(FF5_garch, "S_CH_H", "S_CH_L", "Sentiment CH")
View(eq5_results)

# Safe-haven regimes (your extension)
eq_SH_results <- run_eq45(FF5_garch, "REER_pos", "REER_neg", "Safe-Haven REER")
View(eq_SH_results)

# All Eq 4-5 results in one table
eq45_all <- bind_rows(eq4_results, eq5_results, eq_SH_results)
View(eq45_all)

########## 2a. INTEREST RATE REGIMES — SUBSAMPLE REGRESSIONS ##########################################

# Merge regime dummies into test portfolio dataframes
FF5_test_3x3_BM_regime <- FF5_test_3x3 %>%
  left_join(FF5_garch %>% select(Date, IR_H, IR_L, S_CH_H, S_CH_L, S_EU_H, S_EU_L, 
                                 REER_pos, REER_neg, CHFUSD_pos, CHFUSD_neg), by = "Date")

FF5_test_3x3_OP_regime <- FF5_test_3x3_OP %>%
  left_join(FF5_garch %>% select(Date, IR_H, IR_L, S_CH_H, S_CH_L, S_EU_H, S_EU_L, 
                                 REER_pos, REER_neg, CHFUSD_pos, CHFUSD_neg), by = "Date")

FF5_test_3x3_INV_regime <- FF5_test_3x3_INV %>%
  left_join(FF5_garch %>% select(Date, IR_H, IR_L, S_CH_H, S_CH_L, S_EU_H, S_EU_L, 
                                 REER_pos, REER_neg, CHFUSD_pos, CHFUSD_neg), by = "Date")

# Get excess column names for each sort
excess_cols_BM  <- names(FF5_test_3x3)[grepl("_excess$", names(FF5_test_3x3))]
excess_cols_BM  <- setdiff(excess_cols_BM, "Mkt_excess")

excess_cols_OP  <- names(FF5_test_3x3_OP)[grepl("_excess$", names(FF5_test_3x3_OP))]
excess_cols_OP  <- setdiff(excess_cols_OP, "Mkt_excess")

excess_cols_INV <- names(FF5_test_3x3_INV)[grepl("_excess$", names(FF5_test_3x3_INV))]
excess_cols_INV <- setdiff(excess_cols_INV, "Mkt_excess")

# --- Interest Rate: High vs Low --- #
IR_high_BM  <- FF5_test_3x3_BM_regime  %>% filter(IR_H == 1)
IR_low_BM   <- FF5_test_3x3_BM_regime  %>% filter(IR_L == 1)
IR_high_OP  <- FF5_test_3x3_OP_regime  %>% filter(IR_H == 1)
IR_low_OP   <- FF5_test_3x3_OP_regime  %>% filter(IR_L == 1)
IR_high_INV <- FF5_test_3x3_INV_regime %>% filter(IR_H == 1)
IR_low_INV  <- FF5_test_3x3_INV_regime %>% filter(IR_L == 1)

# Regressions: Size x BM
results_IR_BM <- bind_rows(
  run_ff5_regression(IR_high_BM, excess_cols_BM, "IR_High"),
  run_ff5_regression(IR_low_BM,  excess_cols_BM, "IR_Low")
)
View(results_IR_BM)

# Regressions: Size x OP
results_IR_OP <- bind_rows(
  run_ff5_regression(IR_high_OP, excess_cols_OP, "IR_High"),
  run_ff5_regression(IR_low_OP,  excess_cols_OP, "IR_Low")
)
View(results_IR_OP)

# Regressions: Size x INV
results_IR_INV <- bind_rows(
  run_ff5_regression(IR_high_INV, excess_cols_INV, "IR_High"),
  run_ff5_regression(IR_low_INV,  excess_cols_INV, "IR_Low")
)
View(results_IR_INV)

# Panel A & B: Interest Rate regimes
panel_A_IR <- bind_rows(
  run_panel_A(FF5_garch %>% filter(IR_H == 1), "IR_High"),
  run_panel_A(FF5_garch %>% filter(IR_L == 1), "IR_Low")
)
View(panel_A_IR)

panel_B_IR_high <- run_panel_B(FF5_garch %>% filter(IR_H == 1), "IR_High")
panel_B_IR_low  <- run_panel_B(FF5_garch %>% filter(IR_L == 1), "IR_Low")
View(panel_B_IR_high)
View(panel_B_IR_low)

########## 2b. SENTIMENT REGIMES (Swiss KOF, 2007+) ##################################################

S_high_BM  <- FF5_test_3x3_BM_regime  %>% filter(S_CH_H == 1)
S_low_BM   <- FF5_test_3x3_BM_regime  %>% filter(S_CH_L == 1)
S_high_OP  <- FF5_test_3x3_OP_regime  %>% filter(S_CH_H == 1)
S_low_OP   <- FF5_test_3x3_OP_regime  %>% filter(S_CH_L == 1)
S_high_INV <- FF5_test_3x3_INV_regime %>% filter(S_CH_H == 1)
S_low_INV  <- FF5_test_3x3_INV_regime %>% filter(S_CH_L == 1)

results_S_BM <- bind_rows(
  run_ff5_regression(S_high_BM, excess_cols_BM, "S_CH_High"),
  run_ff5_regression(S_low_BM,  excess_cols_BM, "S_CH_Low")
)
View(results_S_BM)

results_S_OP <- bind_rows(
  run_ff5_regression(S_high_OP, excess_cols_OP, "S_CH_High"),
  run_ff5_regression(S_low_OP,  excess_cols_OP, "S_CH_Low")
)
View(results_S_OP)

results_S_INV <- bind_rows(
  run_ff5_regression(S_high_INV, excess_cols_INV, "S_CH_High"),
  run_ff5_regression(S_low_INV,  excess_cols_INV, "S_CH_Low")
)
View(results_S_INV)

panel_A_S <- bind_rows(
  run_panel_A(FF5_garch %>% filter(S_CH_H == 1), "S_CH_High"),
  run_panel_A(FF5_garch %>% filter(S_CH_L == 1), "S_CH_Low")
)
View(panel_A_S)

panel_B_S_high <- run_panel_B(FF5_garch %>% filter(S_CH_H == 1), "S_CH_High")
panel_B_S_low  <- run_panel_B(FF5_garch %>% filter(S_CH_L == 1), "S_CH_Low")
View(panel_B_S_high)
View(panel_B_S_low)



########## 2c. SAFE-HAVEN REGIMES (REER) ##############################################################
FF5_test_3x3_BM_regime
SH_pos_BM  <- FF5_test_3x3_BM_regime  %>% filter(REER_pos == 1)
SH_neg_BM  <- FF5_test_3x3_BM_regime  %>% filter(REER_neg == 1)
SH_pos_OP  <- FF5_test_3x3_OP_regime  %>% filter(REER_pos == 1)
SH_neg_OP  <- FF5_test_3x3_OP_regime  %>% filter(REER_neg == 1)
SH_pos_INV <- FF5_test_3x3_INV_regime %>% filter(REER_pos == 1)
SH_neg_INV <- FF5_test_3x3_INV_regime %>% filter(REER_neg == 1)

results_SH_BM <- bind_rows(
  run_ff5_regression(SH_pos_BM, excess_cols_BM, "REER_Appreciation"),
  run_ff5_regression(SH_neg_BM, excess_cols_BM, "REER_Depreciation")
)
View(results_SH_BM)

results_SH_OP <- bind_rows(
  run_ff5_regression(SH_pos_OP, excess_cols_OP, "REER_Appreciation"),
  run_ff5_regression(SH_neg_OP, excess_cols_OP, "REER_Depreciation")
)
View(results_SH_OP)

results_SH_INV <- bind_rows(
  run_ff5_regression(SH_pos_INV, excess_cols_INV, "REER_Appreciation"),
  run_ff5_regression(SH_neg_INV, excess_cols_INV, "REER_Depreciation")
)
View(results_SH_INV)

panel_A_SH <- bind_rows(
  run_panel_A(FF5_garch %>% filter(REER_pos == 1), "REER_Appreciation"),
  run_panel_A(FF5_garch %>% filter(REER_neg == 1), "REER_Depreciation")
)
View(panel_A_SH)

panel_B_SH_pos <- run_panel_B(FF5_garch %>% filter(REER_pos == 1), "REER_Appreciation")
panel_B_SH_neg <- run_panel_B(FF5_garch %>% filter(REER_neg == 1), "REER_Depreciation")
View(panel_B_SH_pos)
View(panel_B_SH_neg)


########## COMPARISON SUMMARY #########################################################################

comparison <- bind_rows(
  results_IR_BM  %>% group_by(regime) %>% summarise(sort = "Size x BM",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_IR_OP  %>% group_by(regime) %>% summarise(sort = "Size x OP",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_IR_INV %>% group_by(regime) %>% summarise(sort = "Size x INV", avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_S_BM   %>% group_by(regime) %>% summarise(sort = "Size x BM",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_S_OP   %>% group_by(regime) %>% summarise(sort = "Size x OP",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_S_INV  %>% group_by(regime) %>% summarise(sort = "Size x INV", avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_SH_BM  %>% group_by(regime) %>% summarise(sort = "Size x BM",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_SH_OP  %>% group_by(regime) %>% summarise(sort = "Size x OP",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_SH_INV %>% group_by(regime) %>% summarise(sort = "Size x INV", avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop")
)
View(comparison)

########## ROBUSTNESS CHECK 1: Safe-Haven using CHF-USD instead of REER ##############################

# Merge regime dummies (already done if using FF5_test_3x3_XX_regime dataframes)

# Eq 4-5: Factor returns on CHF-USD dummies
eq_CHFUSD_results <- run_eq45(FF5_garch, "CHFUSD_pos", "CHFUSD_neg", "Safe-Haven CHF-USD")
View(eq_CHFUSD_results)

# Subsample regressions
CHFUSD_pos_BM  <- FF5_test_3x3_BM_regime  %>% filter(CHFUSD_pos == 1)
CHFUSD_neg_BM  <- FF5_test_3x3_BM_regime  %>% filter(CHFUSD_neg == 1)
CHFUSD_pos_OP  <- FF5_test_3x3_OP_regime  %>% filter(CHFUSD_pos == 1)
CHFUSD_neg_OP  <- FF5_test_3x3_OP_regime  %>% filter(CHFUSD_neg == 1)
CHFUSD_pos_INV <- FF5_test_3x3_INV_regime %>% filter(CHFUSD_pos == 1)
CHFUSD_neg_INV <- FF5_test_3x3_INV_regime %>% filter(CHFUSD_neg == 1)

results_CHFUSD_BM <- bind_rows(
  run_ff5_regression(CHFUSD_pos_BM, excess_cols_BM, "CHFUSD_Appreciation"),
  run_ff5_regression(CHFUSD_neg_BM, excess_cols_BM, "CHFUSD_Depreciation")
)
View(results_CHFUSD_BM)

results_CHFUSD_OP <- bind_rows(
  run_ff5_regression(CHFUSD_pos_OP, excess_cols_OP, "CHFUSD_Appreciation"),
  run_ff5_regression(CHFUSD_neg_OP, excess_cols_OP, "CHFUSD_Depreciation")
)
View(results_CHFUSD_OP)

results_CHFUSD_INV <- bind_rows(
  run_ff5_regression(CHFUSD_pos_INV, excess_cols_INV, "CHFUSD_Appreciation"),
  run_ff5_regression(CHFUSD_neg_INV, excess_cols_INV, "CHFUSD_Depreciation")
)
View(results_CHFUSD_INV)

# Panel A & B
panel_A_CHFUSD <- bind_rows(
  run_panel_A(FF5_garch %>% filter(CHFUSD_pos == 1), "CHFUSD_Appreciation"),
  run_panel_A(FF5_garch %>% filter(CHFUSD_neg == 1), "CHFUSD_Depreciation")
)
View(panel_A_CHFUSD)

panel_B_CHFUSD_pos <- run_panel_B(FF5_garch %>% filter(CHFUSD_pos == 1), "CHFUSD_Appreciation")
panel_B_CHFUSD_neg <- run_panel_B(FF5_garch %>% filter(CHFUSD_neg == 1), "CHFUSD_Depreciation")
View(panel_B_CHFUSD_pos)
View(panel_B_CHFUSD_neg)

########## ROBUSTNESS CHECK 2: Sentiment using EU KOF instead of Swiss KOF ###########################

# Eq 4-5: Factor returns on EU sentiment dummies
eq_S_EU_results <- run_eq45(FF5_garch, "S_EU_H", "S_EU_L", "Sentiment EU")
View(eq_S_EU_results)

# Subsample regressions
S_EU_high_BM  <- FF5_test_3x3_BM_regime  %>% filter(S_EU_H == 1)
S_EU_low_BM   <- FF5_test_3x3_BM_regime  %>% filter(S_EU_L == 1)
S_EU_high_OP  <- FF5_test_3x3_OP_regime  %>% filter(S_EU_H == 1)
S_EU_low_OP   <- FF5_test_3x3_OP_regime  %>% filter(S_EU_L == 1)
S_EU_high_INV <- FF5_test_3x3_INV_regime %>% filter(S_EU_H == 1)
S_EU_low_INV  <- FF5_test_3x3_INV_regime %>% filter(S_EU_L == 1)

results_S_EU_BM <- bind_rows(
  run_ff5_regression(S_EU_high_BM, excess_cols_BM, "S_EU_High"),
  run_ff5_regression(S_EU_low_BM,  excess_cols_BM, "S_EU_Low")
)
View(results_S_EU_BM)

results_S_EU_OP <- bind_rows(
  run_ff5_regression(S_EU_high_OP, excess_cols_OP, "S_EU_High"),
  run_ff5_regression(S_EU_low_OP,  excess_cols_OP, "S_EU_Low")
)
View(results_S_EU_OP)

results_S_EU_INV <- bind_rows(
  run_ff5_regression(S_EU_high_INV, excess_cols_INV, "S_EU_High"),
  run_ff5_regression(S_EU_low_INV,  excess_cols_INV, "S_EU_Low")
)
View(results_S_EU_INV)

# Panel A & B
panel_A_S_EU <- bind_rows(
  run_panel_A(FF5_garch %>% filter(S_EU_H == 1), "S_EU_High"),
  run_panel_A(FF5_garch %>% filter(S_EU_L == 1), "S_EU_Low")
)
View(panel_A_S_EU)

panel_B_S_EU_high <- run_panel_B(FF5_garch %>% filter(S_EU_H == 1), "S_EU_High")
panel_B_S_EU_low  <- run_panel_B(FF5_garch %>% filter(S_EU_L == 1), "S_EU_Low")
View(panel_B_S_EU_high)
View(panel_B_S_EU_low)

########## ROBUSTNESS COMPARISON SUMMARY ##############################################################

robustness_comparison <- bind_rows(
  # CHF-USD robustness
  results_CHFUSD_BM  %>% group_by(regime) %>% summarise(sort = "Size x BM",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_CHFUSD_OP  %>% group_by(regime) %>% summarise(sort = "Size x OP",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_CHFUSD_INV %>% group_by(regime) %>% summarise(sort = "Size x INV", avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  # EU sentiment robustness
  results_S_EU_BM  %>% group_by(regime) %>% summarise(sort = "Size x BM",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_S_EU_OP  %>% group_by(regime) %>% summarise(sort = "Size x OP",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_S_EU_INV %>% group_by(regime) %>% summarise(sort = "Size x INV", avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop")
)
View(robustness_comparison)

# Compare main vs robustness Eq 4-5 results side by side
eq45_robustness <- bind_rows(
  eq_SH_results,       # Main: REER
  eq_CHFUSD_results,   # Robustness: CHF-USD
  eq5_results,         # Main: Swiss KOF
  eq_S_EU_results      # Robustness: EU KOF
)
View(eq45_robustness)

#comparing robustness checks
# Do the portfolio alphas agree across specifications?
# Safe-haven#############
alpha_compare_SH <- results_SH_BM %>%
  filter(regime == "REER_Appreciation") %>%
  select(portfolio, alpha_main = alpha) %>%
  left_join(
    results_CHFUSD_BM %>%
      filter(regime == "CHFUSD_Appreciation") %>%
      select(portfolio, alpha_robust = alpha),
    by = "portfolio"
  )

cor(alpha_compare_SH$alpha_main, alpha_compare_SH$alpha_robust, use = "complete.obs")

alpha_compare_SH_dep <- results_SH_BM %>%
  filter(regime == "REER_Depreciation") %>%
  select(portfolio, alpha_main = alpha) %>%
  left_join(
    results_CHFUSD_BM %>%
      filter(regime == "CHFUSD_Depreciation") %>%
      select(portfolio, alpha_robust = alpha),
    by = "portfolio"
  )

cor(alpha_compare_SH_dep$alpha_main, alpha_compare_SH_dep$alpha_robust, use = "complete.obs")

# sentiment#############
# Sentiment: Swiss KOF vs EU KOF
alpha_compare_S_high <- results_S_BM %>%
  filter(regime == "S_CH_High") %>%
  select(portfolio, alpha_main = alpha) %>%
  left_join(
    results_S_EU_BM %>%
      filter(regime == "S_EU_High") %>%
      select(portfolio, alpha_robust = alpha),
    by = "portfolio"
  )
cor(alpha_compare_S_high$alpha_main, alpha_compare_S_high$alpha_robust, use = "complete.obs")

alpha_compare_S_low <- results_S_BM %>%
  filter(regime == "S_CH_Low") %>%
  select(portfolio, alpha_main = alpha) %>%
  left_join(
    results_S_EU_BM %>%
      filter(regime == "S_EU_Low") %>%
      select(portfolio, alpha_robust = alpha),
    by = "portfolio"
  )
cor(alpha_compare_S_low$alpha_main, alpha_compare_S_low$alpha_robust, use = "complete.obs")


##################combination####################################################
# How many observations per combined regime?
FF5_garch %>%
  filter(!is.na(S_CH_H)) %>%
  mutate(
    combined = case_when(
      S_CH_L == 1 & REER_neg == 1 ~ "Low_S + Depreciation",
      S_CH_L == 1 & REER_pos == 1 ~ "Low_S + Appreciation",
      S_CH_H == 1 & REER_neg == 1 ~ "High_S + Depreciation",
      S_CH_H == 1 & REER_pos == 1 ~ "High_S + Appreciation"
    )
  ) %>%
  count(combined)

#perform the combined cycle
# Combined regime dummies
FF5_garch <- FF5_garch %>%
  mutate(
    SL_RN = ifelse(S_CH_L == 1 & REER_neg == 1, 1, 0),  # Low sentiment + Depreciation
    SL_RP = ifelse(S_CH_L == 1 & REER_pos == 1, 1, 0),  # Low sentiment + Appreciation
    SH_RN = ifelse(S_CH_H == 1 & REER_neg == 1, 1, 0),  # High sentiment + Depreciation
    SH_RP = ifelse(S_CH_H == 1 & REER_pos == 1, 1, 0)   # High sentiment + Appreciation
  )

# Eq 4-5 style: factor returns by combined regime
factor_names <- c("Mkt_excess", "SMB", "HML", "RMW", "CMA")

FF5_garch <- FF5_garch %>%
  mutate(
    SL_RN = ifelse(S_CH_L == 1 & REER_neg == 1, 1, 0),
    SL_RP = ifelse(S_CH_L == 1 & REER_pos == 1, 1, 0),
    SH_RN = ifelse(S_CH_H == 1 & REER_neg == 1, 1, 0),
    SH_RP = ifelse(S_CH_H == 1 & REER_pos == 1, 1, 0)
  )

# Check counts
table(FF5_garch$SL_RN)
table(FF5_garch$SL_RP)
table(FF5_garch$SH_RN)
table(FF5_garch$SH_RP)


eq_combined <- lapply(factor_names, function(fac) {
  model <- lm(as.formula(paste(fac, "~ 0 + SL_RN + SL_RP + SH_RN + SH_RP")),
              data = FF5_garch %>% filter(!is.na(S_CH_H)))
  smry <- summary(model)
  coefs <- smry$coefficients
  data.frame(
    factor = fac,
    SL_RN  = coefs["SL_RN", "Estimate"],
    t_SL_RN = coefs["SL_RN", "t value"],
    sig_SL_RN = sig_code(coefs["SL_RN", "Pr(>|t|)"]),
    SL_RP  = coefs["SL_RP", "Estimate"],
    t_SL_RP = coefs["SL_RP", "t value"],
    sig_SL_RP = sig_code(coefs["SL_RP", "Pr(>|t|)"]),
    SH_RN  = coefs["SH_RN", "Estimate"],
    t_SH_RN = coefs["SH_RN", "t value"],
    sig_SH_RN = sig_code(coefs["SH_RN", "Pr(>|t|)"]),
    SH_RP  = coefs["SH_RP", "Estimate"],
    t_SH_RP = coefs["SH_RP", "t value"],
    sig_SH_RP = sig_code(coefs["SH_RP", "Pr(>|t|)"])
  )
}) %>% bind_rows()
View(eq_combined)

########## COMBINED REGIME: Panel A, Panel B, and Regressions ########################################

# Panel A
panel_A_combined <- bind_rows(
  run_panel_A(FF5_garch %>% filter(SL_RN == 1), "SL_RN"),
  run_panel_A(FF5_garch %>% filter(SL_RP == 1), "SL_RP"),
  run_panel_A(FF5_garch %>% filter(SH_RN == 1), "SH_RN"),
  run_panel_A(FF5_garch %>% filter(SH_RP == 1), "SH_RP")
)
View(panel_A_combined)

# Panel B
panel_B_SL_RN <- run_panel_B(FF5_garch %>% filter(SL_RN == 1), "SL_RN")
panel_B_SL_RP <- run_panel_B(FF5_garch %>% filter(SL_RP == 1), "SL_RP")
panel_B_SH_RN <- run_panel_B(FF5_garch %>% filter(SH_RN == 1), "SH_RN")
panel_B_SH_RP <- run_panel_B(FF5_garch %>% filter(SH_RP == 1), "SH_RP")
View(panel_B_SL_RN)
View(panel_B_SL_RP)
View(panel_B_SH_RN)
View(panel_B_SH_RP)

# Merge regime dummies into test portfolios (if not already there)
FF5_test_3x3_BM_regime <- FF5_test_3x3_BM_regime %>%
  left_join(FF5_garch %>% select(Date, SL_RN, SL_RP, SH_RN, SH_RP), by = "Date")

FF5_test_3x3_OP_regime <- FF5_test_3x3_OP_regime %>%
  left_join(FF5_garch %>% select(Date, SL_RN, SL_RP, SH_RN, SH_RP), by = "Date")

FF5_test_3x3_INV_regime <- FF5_test_3x3_INV_regime %>%
  left_join(FF5_garch %>% select(Date, SL_RN, SL_RP, SH_RN, SH_RP), by = "Date")

# Regressions: Size x BM
results_combined_BM <- bind_rows(
  run_ff5_regression(FF5_test_3x3_BM_regime %>% filter(SL_RN == 1), excess_cols_BM, "SL_RN"),
  run_ff5_regression(FF5_test_3x3_BM_regime %>% filter(SL_RP == 1), excess_cols_BM, "SL_RP"),
  run_ff5_regression(FF5_test_3x3_BM_regime %>% filter(SH_RN == 1), excess_cols_BM, "SH_RN"),
  run_ff5_regression(FF5_test_3x3_BM_regime %>% filter(SH_RP == 1), excess_cols_BM, "SH_RP")
)
View(results_combined_BM)

# Regressions: Size x OP
results_combined_OP <- bind_rows(
  run_ff5_regression(FF5_test_3x3_OP_regime %>% filter(SL_RN == 1), excess_cols_OP, "SL_RN"),
  run_ff5_regression(FF5_test_3x3_OP_regime %>% filter(SL_RP == 1), excess_cols_OP, "SL_RP"),
  run_ff5_regression(FF5_test_3x3_OP_regime %>% filter(SH_RN == 1), excess_cols_OP, "SH_RN"),
  run_ff5_regression(FF5_test_3x3_OP_regime %>% filter(SH_RP == 1), excess_cols_OP, "SH_RP")
)
View(results_combined_OP)

# Regressions: Size x INV
results_combined_INV <- bind_rows(
  run_ff5_regression(FF5_test_3x3_INV_regime %>% filter(SL_RN == 1), excess_cols_INV, "SL_RN"),
  run_ff5_regression(FF5_test_3x3_INV_regime %>% filter(SL_RP == 1), excess_cols_INV, "SL_RP"),
  run_ff5_regression(FF5_test_3x3_INV_regime %>% filter(SH_RN == 1), excess_cols_INV, "SH_RN"),
  run_ff5_regression(FF5_test_3x3_INV_regime %>% filter(SH_RP == 1), excess_cols_INV, "SH_RP")
)
View(results_combined_INV)

# Comparison summary
comparison_combined <- bind_rows(
  results_combined_BM  %>% group_by(regime) %>% summarise(sort = "Size x BM",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_combined_OP  %>% group_by(regime) %>% summarise(sort = "Size x OP",  avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop"),
  results_combined_INV %>% group_by(regime) %>% summarise(sort = "Size x INV", avg_R2 = mean(r2), avg_abs_alpha = mean(abs(alpha)), .groups = "drop")
)
View(comparison_combined)


####summary table#################################################################################
########## MASTER COMPARISON TABLE ##########################################################

# Helper function to summarise one set of results
summarise_results <- function(data, spec_name) {
  data %>%
    summarise(
      spec = spec_name,
      avg_R2 = mean(r2, na.rm = TRUE),
      avg_abs_alpha = mean(abs(alpha), na.rm = TRUE),
      n_sig_alpha_5pct = sum(p_alpha < 0.05, na.rm = TRUE),
      n_sig_alpha_10pct = sum(p_alpha < 0.10, na.rm = TRUE),
      n_portfolios = n(),
      avg_df = mean(df_den, na.rm = TRUE)
    )
}

master_comparison <- bind_rows(
  # Baseline (average across 3 sorts)
  bind_rows(ff3x3_results, ff5_results_OP, ff5_results_INV) %>%
    summarise_results("Baseline"),
  
  # IR regimes
  bind_rows(results_IR_BM, results_IR_OP, results_IR_INV) %>%
    filter(regime == "IR_High") %>%
    summarise_results("IR_High"),
  
  bind_rows(results_IR_BM, results_IR_OP, results_IR_INV) %>%
    filter(regime == "IR_Low") %>%
    summarise_results("IR_Low"),
  
  # Sentiment CH regimes
  bind_rows(results_S_BM, results_S_OP, results_S_INV) %>%
    filter(regime == "S_CH_High") %>%
    summarise_results("S_CH_High"),
  
  bind_rows(results_S_BM, results_S_OP, results_S_INV) %>%
    filter(regime == "S_CH_Low") %>%
    summarise_results("S_CH_Low"),
  
  # Safe-haven REER regimes
  bind_rows(results_SH_BM, results_SH_OP, results_SH_INV) %>%
    filter(regime == "REER_Appreciation") %>%
    summarise_results("REER_Appreciation"),
  
  bind_rows(results_SH_BM, results_SH_OP, results_SH_INV) %>%
    filter(regime == "REER_Depreciation") %>%
    summarise_results("REER_Depreciation"),
  
  # Combined regimes
  bind_rows(results_combined_BM, results_combined_OP, results_combined_INV) %>%
    filter(regime == "SL_RN") %>%
    summarise_results("SL_RN"),
  
  bind_rows(results_combined_BM, results_combined_OP, results_combined_INV) %>%
    filter(regime == "SL_RP") %>%
    summarise_results("SL_RP"),
  
  bind_rows(results_combined_BM, results_combined_OP, results_combined_INV) %>%
    filter(regime == "SH_RN") %>%
    summarise_results("SH_RN"),
  
  bind_rows(results_combined_BM, results_combined_OP, results_combined_INV) %>%
    filter(regime == "SH_RP") %>%
    summarise_results("SH_RP"),
  
  # Robustness: Sentiment EU
  bind_rows(results_S_EU_BM, results_S_EU_OP, results_S_EU_INV) %>%
    filter(regime == "S_EU_High") %>%
    summarise_results("S_EU_High (robustness)"),
  
  bind_rows(results_S_EU_BM, results_S_EU_OP, results_S_EU_INV) %>%
    filter(regime == "S_EU_Low") %>%
    summarise_results("S_EU_Low (robustness)"),
  
  # Robustness: CHF-USD
  bind_rows(results_CHFUSD_BM, results_CHFUSD_OP, results_CHFUSD_INV) %>%
    filter(regime == "CHFUSD_Appreciation") %>%
    summarise_results("CHFUSD_Appreciation (robustness)"),
  
  bind_rows(results_CHFUSD_BM, results_CHFUSD_OP, results_CHFUSD_INV) %>%
    filter(regime == "CHFUSD_Depreciation") %>%
    summarise_results("CHFUSD_Depreciation (robustness)")
)

# Add percentage of significant alphas for easier reading
master_comparison <- master_comparison %>%
  mutate(
    pct_sig_5pct  = paste0(n_sig_alpha_5pct, "/", n_portfolios, 
                           " (", round(n_sig_alpha_5pct / n_portfolios * 100), "%)"),
    pct_sig_10pct = paste0(n_sig_alpha_10pct, "/", n_portfolios,
                           " (", round(n_sig_alpha_10pct / n_portfolios * 100), "%)")
  )

View(master_comparison)


#################Equation 8
# Equation 8: Market return on regime dummies + five factors
# ---- Helper function: extract Eq 8 results in standard format ---- #
extract_eq8 <- function(model, label) {
  smry  <- summary(model)
  coefs <- smry$coefficients
  fstat <- smry$fstatistic
  
  data.frame(
    model     = label,
    variable  = rownames(coefs),
    estimate  = coefs[, "Estimate"],
    t_value   = coefs[, "t value"],
    p_value   = coefs[, "Pr(>|t|)"],
    sig       = sig_code(coefs[, "Pr(>|t|)"]),
    r2        = smry$r.squared,
    adj_r2    = smry$adj.r.squared,
    resid_se  = smry$sigma,
    f_stat    = unname(fstat[1]),
    df_num    = unname(fstat[2]),
    df_den    = unname(fstat[3]),
    model_p   = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  )
}

########## EQUATION 8: INDIVIDUAL REGIME VARIABLES ####################################################

# No regimes (baseline)
eq8_baseline <- lm(Mkt_excess ~ SMB + HML + RMW + CMA, data = FF5_garch)
eq8_baseline_results <- extract_eq8(eq8_baseline, "Eq8_Baseline")
View(eq8_baseline_results)


# Interest rate only
eq8_IR <- lm(Mkt_excess ~ IR_H + SMB + HML + RMW + CMA, data = FF5_garch)
eq8_IR_results <- extract_eq8(eq8_IR, "Eq8_IR")
View(eq8_IR_results)

# Sentiment only
eq8_S <- lm(Mkt_excess ~ S_CH_H + SMB + HML + RMW + CMA, data = FF5_garch)
eq8_S_results <- extract_eq8(eq8_S, "Eq8_Sentiment_CH")
View(eq8_S_results)

# Safe-haven only
eq8_REER <- lm(Mkt_excess ~ REER_pos + SMB + HML + RMW + CMA, data = FF5_garch)
eq8_REER_results <- extract_eq8(eq8_REER, "Eq8_REER")
View(eq8_REER_results)

########## EQUATION 8: COMBINATION ##################################################################

# Sentiment + REER
eq8_S_REER <- lm(Mkt_excess ~ S_CH_H + REER_pos + SMB + HML + RMW + CMA, data = FF5_garch)
eq8_S_REER_results <- extract_eq8(eq8_S_REER, "Eq8_Sentiment_x_REER")
View(eq8_S_REER_results)



########## COMBINE ALL EQ 8 RESULTS ##################################################################

eq8_combined <- bind_rows(
  eq8_baseline_results,
  eq8_IR_results,
  eq8_S_results,
  eq8_REER_results,
  #eq8_IR_S_results,
  eq8_S_REER_results,
  #eq8_IR_REER_results,
  #eq8_all_results
)
View(eq8_combined)




#####################robustness check- megacap#######################################
########## ROBUSTNESS CHECK: SMB excluding mega-caps ##################################################

# Top companies by average market cap weight
mega_cap_check <- df_merged_factors %>%
  filter(!is.na(mark_cap_tminus1)) %>%
  group_by(Date) %>%
  mutate(
    total_mkt_cap = sum(mark_cap_tminus1),
    weight = mark_cap_tminus1 / total_mkt_cap * 100
  ) %>%
  ungroup() %>%
  group_by(Ticker) %>%
  summarise(
    avg_weight = mean(weight, na.rm = TRUE),
    max_weight = max(weight, na.rm = TRUE),
    n_months = n()
  ) %>%
  arrange(desc(avg_weight))

View(mega_cap_check)

# Top 10
head(mega_cap_check, 10)


# Step 2: Check their market cap weight over time
mega_caps <- c("NESN SW Equity", "NOVN SW Equity", "ROP SW Equity", "UBSG SW Equity")  # adjust to match your tickers

mega_cap_weight <- df_merged_factors %>%
  filter(!is.na(mark_cap_tminus1)) %>%
  group_by(Date) %>%
  summarise(
    total_mkt_cap = sum(mark_cap_tminus1),
    mega_cap = sum(mark_cap_tminus1[Ticker %in% mega_caps]),
    mega_pct = mega_cap / total_mkt_cap * 100
  )

# Plot concentration over time
library(ggplot2)
ggplot(mega_cap_weight, aes(x = Date, y = mega_pct)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Market Cap Share of Nestlé, Novartis Roche, & UBS",
    y = "% of Total Market Cap",
    x = "Date"
  ) +
  theme_minimal()

# Average concentration
mean(mega_cap_weight$mega_pct, na.rm = TRUE)

# Step 3: Reconstruct factors WITHOUT mega-caps
df_merged_factors_no_mega <- df_merged_factors %>%
  filter(!Ticker %in% mega_caps)

# Recalculate market return without mega-caps
Rm_no_mega <- df_merged_factors_no_mega %>%
  filter(!is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date) %>%
  summarise(
    Rm_no_mega = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE)
  )

# Recalculate BM portfolio returns without mega-caps
BM_returns_no_mega <- df_merged_factors_no_mega %>%
  filter(!is.na(BM_portfolio), !is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date, BM_portfolio) %>%
  summarise(
    port_ret = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  )

BM_returns_wide_no_mega <- BM_returns_no_mega %>%
  select(Date, BM_portfolio, port_ret) %>%
  pivot_wider(names_from = BM_portfolio, values_from = port_ret) %>%
  mutate(
    HML_no_mega = 0.5 * (`Small High` + `Big High`) - 0.5 * (`Small Low` + `Big Low`),
    SMB_BM_no_mega = (1/3) * (`Small Low` + `Small Neutral` + `Small High`) -
      (1/3) * (`Big Low` + `Big Neutral` + `Big High`)
  )

# Recalculate OP portfolio returns without mega-caps
OP_returns_no_mega <- df_merged_factors_no_mega %>%
  filter(!is.na(OP_portfolio), !is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date, OP_portfolio) %>%
  summarise(
    port_ret = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  )

OP_returns_wide_no_mega <- OP_returns_no_mega %>%
  select(Date, OP_portfolio, port_ret) %>%
  pivot_wider(names_from = OP_portfolio, values_from = port_ret) %>%
  mutate(
    RMW_no_mega = 0.5 * (`Small Robust` + `Big Robust`) - 0.5 * (`Small Weak` + `Big Weak`),
    SMB_OP_no_mega = (1/3) * (`Small Weak` + `Small Neutral` + `Small Robust`) -
      (1/3) * (`Big Weak` + `Big Neutral` + `Big Robust`)
  )

# Recalculate INV portfolio returns without mega-caps
INV_returns_no_mega <- df_merged_factors_no_mega %>%
  filter(!is.na(INV_portfolio), !is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date, INV_portfolio) %>%
  summarise(
    port_ret = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  )

INV_returns_wide_no_mega <- INV_returns_no_mega %>%
  select(Date, INV_portfolio, port_ret) %>%
  pivot_wider(names_from = INV_portfolio, values_from = port_ret) %>%
  mutate(
    CMA_no_mega = 0.5 * (`Small Conservative` + `Big Conservative`) -
      0.5 * (`Small Aggressive` + `Big Aggressive`),
    SMB_INV_no_mega = (1/3) * (`Small Conservative` + `Small Neutral` + `Small Aggressive`) -
      (1/3) * (`Big Conservative` + `Big Neutral` + `Big Aggressive`)
  )

# Step 4: Merge into one factor dataset
factors_no_mega <- BM_returns_wide_no_mega %>%
  select(Date, HML_no_mega, SMB_BM_no_mega) %>%
  left_join(
    OP_returns_wide_no_mega %>% select(Date, RMW_no_mega, SMB_OP_no_mega),
    by = "Date"
  ) %>%
  left_join(
    INV_returns_wide_no_mega %>% select(Date, CMA_no_mega, SMB_INV_no_mega),
    by = "Date"
  ) %>%
  left_join(Rm_no_mega, by = "Date") %>%
  left_join(rf_monthly, by = "Date") %>%
  mutate(
    SMB_no_mega = (SMB_BM_no_mega + SMB_OP_no_mega + SMB_INV_no_mega) / 3,
    Mkt_excess_no_mega = Rm_no_mega - Rf
  ) %>%
  filter(Date >= as.Date("2001-07-01"))

# Step 5: Compare factor means — WITH vs WITHOUT mega-caps
comparison_smb <- data.frame(
  Factor = c("MKT", "SMB", "HML", "RMW", "CMA"),
  With_mega = c(
    mean(FF5$Mkt_excess, na.rm = TRUE),
    mean(FF5$SMB, na.rm = TRUE),
    mean(FF5$HML, na.rm = TRUE),
    mean(FF5$RMW, na.rm = TRUE),
    mean(FF5$CMA, na.rm = TRUE)
  ),
  Without_mega = c(
    mean(factors_no_mega$Mkt_excess_no_mega, na.rm = TRUE),
    mean(factors_no_mega$SMB_no_mega, na.rm = TRUE),
    mean(factors_no_mega$HML_no_mega, na.rm = TRUE),
    mean(factors_no_mega$RMW_no_mega, na.rm = TRUE),
    mean(factors_no_mega$CMA_no_mega, na.rm = TRUE)
  )
)
comparison_smb$difference <- comparison_smb$Without_mega - comparison_smb$With_mega
View(comparison_smb)

# Step 6: Time series comparison of SMB
smb_compare <- FF5 %>%
  select(Date, SMB_full = SMB) %>%
  left_join(
    factors_no_mega %>% select(Date, SMB_no_mega),
    by = "Date"
  )

cor(smb_compare$SMB_full, smb_compare$SMB_no_mega, use = "complete.obs")

ggplot(smb_compare, aes(x = Date)) +
  geom_line(aes(y = SMB_full, color = "With mega-caps")) +
  geom_line(aes(y = SMB_no_mega, color = "Without mega-caps")) +
  labs(
    title = "SMB Factor: With vs Without Megacaps",
    y = "Monthly SMB Return",
    x = "Date",
    color = ""
  ) +
  theme_minimal()

# Step 5b: Panel A — WITHOUT mega-caps
panel_A_no_mega <- factors_no_mega %>%
  select(Mkt_excess_no_mega, SMB_no_mega, HML_no_mega, RMW_no_mega, CMA_no_mega) %>%
  rename(
    Mkt_excess = Mkt_excess_no_mega,
    SMB = SMB_no_mega,
    HML = HML_no_mega,
    RMW = RMW_no_mega,
    CMA = CMA_no_mega
  ) %>%
  pivot_longer(everything(), names_to = "Factor", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(Factor) %>%
  summarise(
    Mean     = mean(value),
    Std_Dev  = sd(value),
    Sharpe   = mean(value) / sd(value) * sqrt(12),
    Skewness = skewness(value),
    Kurtosis = kurtosis(value),
    Min      = min(value),
    Max      = max(value),
    JB_pval  = jarque.bera.test(value)$p.value,
    .groups  = "drop"
  ) %>%
  mutate(
    JB_sig = case_when(
      JB_pval < 0.01 ~ "***",
      JB_pval < 0.05 ~ "**",
      JB_pval < 0.10 ~ "*",
      TRUE ~ ""
    ),
    Factor = factor(Factor, levels = c("Mkt_excess", "SMB", "HML", "RMW", "CMA")),
    sample = "Without mega-caps"
  ) %>%
  arrange(Factor)

View(panel_A_no_mega)

# Step 5c: Panel B — WITHOUT mega-caps
panel_B_no_mega <- factors_no_mega %>%
  select(Mkt_excess_no_mega, SMB_no_mega, HML_no_mega, RMW_no_mega, CMA_no_mega) %>%
  rename(
    Mkt_excess = Mkt_excess_no_mega,
    SMB = SMB_no_mega,
    HML = HML_no_mega,
    RMW = RMW_no_mega,
    CMA = CMA_no_mega
  ) %>%
  cor(use = "complete.obs") %>%
  round(4)

View(panel_B_no_mega)

# Step 5d: Side-by-side Panel A comparison
panel_A_full <- panel_A %>%
  mutate(sample = "Full sample")

panel_A_comparison <- bind_rows(panel_A_full, panel_A_no_mega)
View(panel_A_comparison)



###########################putting the model together#################################################################

library(dplyr)

factors_final

FF5 <- factors_final %>%
  left_join(rf_monthly, by = "Date") %>%
  left_join(external_factors, by = "Date") 

FF5 <- FF5 %>%
  select(Date, Rm, Rf, HML, RMW, CMA, SMB) %>%
  mutate(
    Mkt_excess = Rm - Rf
  )





#3x3 portfolios- size and OP:##############################################################################

library(dplyr)
library(tidyr)

View(df)


df_large_OP <- df 

df_large_OP <- df_large_OP %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    tot_assets_tminus2 = prev_distinct(tot_assets_tminus1)
  ) %>%
  ungroup()

View(df_large_OP)

#construct factors
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)

# df has: Date (YYYY-MM), Ticker, ret (or monthly return), and the tminus columns:
# equity_tminus1, mark_cap_tminus1, op_prof_tminus1, tot_assets_tminus1, tot_assets_tminus2

df_factors_june_OP <- df_large_OP %>%
  mutate(
    # make sure Date is a proper monthly date
    Date_m = as.Date(as.yearmon(Date, "%Y-%m")),
    year   = year(Date_m),
    month  = month(Date_m),
    
    # construct characteristics (as-of / lagged)
    BM  = equity_tminus1 / mark_cap_tminus1,
    OP  = op_prof_tminus1 / equity_tminus1,
    INV = (tot_assets_tminus1 - tot_assets_tminus2) / tot_assets_tminus2
  ) %>%
  # keep only June observations (the sorting month)
  filter(month == 6) %>%
  select(Date = Date_m, year, Ticker, ret, BM, OP, INV,
         equity_tminus1, mark_cap_tminus1, op_prof_tminus1,
         tot_assets_tminus1, tot_assets_tminus2)

View(df_factors_june_OP)

#check cross sectionality
library(dplyr)

cs_counts <- df_factors_june_OP %>%
  #filter(month(Date) == 6) %>%   # keep June only
  group_by(year) %>%
  summarise(
    n_mark_cap = sum(!is.na(mark_cap_tminus1)),
    n_BM   = sum(!is.na(BM)),
    n_OP   = sum(!is.na(OP)),
    n_INV  = sum(!is.na(INV))
  ) %>%
  arrange(year)

View(cs_counts)

####################################################################################################################################

library(dplyr)


df_factors_june_OP <- df_factors_june_OP %>%
  mutate(
    BM = as.numeric(BM),
    mark_cap_tminus1 = as.numeric(mark_cap_tminus1)
  ) %>%
  group_by(year) %>%
  mutate(
    SIZE_30 = quantile(mark_cap_tminus1, 0.3, na.rm = TRUE),
    SIZE_70 = quantile(mark_cap_tminus1, 0.7, na.rm = TRUE),
    
    SIZE_port_3 = case_when(
      is.na(mark_cap_tminus1) ~ NA_character_,
      mark_cap_tminus1 <= SIZE_30 ~ "Small",
      mark_cap_tminus1 <= SIZE_70 ~ "Neutral",
      TRUE ~ "Big"
    ),
    
    OP_30 = quantile(OP, 0.3, na.rm = TRUE),
    OP_70 = quantile(OP, 0.7, na.rm = TRUE),
    
    OP_port_3 = case_when(
      is.na(OP) ~ NA_character_,
      OP <= OP_30 ~ "Weak",
      OP <= OP_70 ~ "Neutral",
      TRUE ~ "Robust"
    ),
    
    OP_3x3_portfolio = ifelse(
      is.na(SIZE_port_3) | is.na(OP_port_3),
      NA,
      paste(SIZE_port_3, OP_port_3, sep = "_")
    )
    
  ) %>%
  ungroup() %>%
  select(Date, year, Ticker, OP_3x3_portfolio) %>%
  filter(!is.na(OP_3x3_portfolio))

View(df_factors_june_OP)

#df_factors_june_OP <- df_factors_june_OP %>%
#mutate(
#BM_portfolio  = ifelse(is.na(SIZE_port) | is.na(BM_port),  NA, BM_portfolio),
#OP_portfolio  = ifelse(is.na(SIZE_port) | is.na(OP_port),  NA, OP_portfolio),
#INV_portfolio = ifelse(is.na(SIZE_port) | is.na(INV_port), NA, INV_portfolio)
#)


df_factors_june_OP <- df_factors_june_OP %>%
  
  select(Date, year, Ticker, OP_3x3_portfolio)


df_factors_june_OP <- df_factors_june_OP %>%
  filter(
    !is.na(OP_3x3_portfolio),
    !grepl("NA", OP_3x3_portfolio)
  )

# EXTRA LINE
df_factors_june_OP <- df_factors_june_OP %>%
  filter(OP_3x3_portfolio != "")
View(df_factors_june_OP)

##merge the datasets together

library(dplyr)
library(lubridate)


df_large_OP <- df_large_OP %>%
  mutate(Date = as.Date(as.yearmon(Date, "%Y-%m"))) %>%
  mutate(
    year = year(Date),
    month = month(Date),
    sort_year = ifelse(month >= 7, year, year - 1)
  )


View(df_large_OP)

####merge df_large_OP and df_factors_june_OP
#rename col name year to sort_year from df_factors_june_OP

df_factors_june_OP <- df_factors_june_OP %>%
  rename(sort_year = year)

df_factors_june_OP


###merge df (left data set) with df_factors_june_OP by ticker and sort_year
library(dplyr)

df_merged_factors_OP <- df_large_OP %>%
  left_join(
    df_factors_june_OP,
    by = c("Ticker", "sort_year")
  )

View(df_merged_factors_OP)
colnames

df_merged_factors_OP <- df_merged_factors_OP %>%
  select(Date = Date.x, Ticker, mark_cap_tminus1, ret, OP_3x3_portfolio)

View(df_merged_factors_OP)


##sanity check of portfolio:
library(dplyr)



write_xlsx(portfolio_counts_OP, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/portfolio_counts_OP.xlsx")


portfolio_counts_OP <- df_merged_factors_OP %>%
  filter(!is.na(OP_3x3_portfolio), !is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date, OP_3x3_portfolio) %>%
  summarise(
    n_firms = n(),
    .groups = "drop"
  ) %>%
  arrange(Date, OP_3x3_portfolio)

View(portfolio_counts_OP)

library(ggplot2)



portfolio_counts_OP %>%
  group_by(OP_3x3_portfolio) %>%
  summarise(avg_firms = mean(n_firms, na.rm = TRUE)) %>%
  ggplot(aes(x = OP_3x3_portfolio, y = avg_firms)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Number of Firms per Portfolio",
    x = "Portfolio",
    y = "Average Number of Firms"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)
library(lubridate)

portfolio_yearly_OP <- portfolio_counts_OP %>%
  mutate(year = year(Date)) %>%
  group_by(year, OP_3x3_portfolio) %>%
  summarise(
    avg_firms = mean(n_firms, na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)

ggplot(portfolio_yearly_OP, aes(x = OP_3x3_portfolio, y = avg_firms)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year) +
  labs(
    title = "Yearly Average Number of Firms per Portfolio",
    x = "Portfolio",
    y = "Average Number of Firms"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_clean_OP <- df_merged_factors_OP %>%
  filter(
    !is.na(OP_3x3_portfolio),
    !is.na(ret),
    !is.na(mark_cap_tminus1)
  )

OP_3x3_returns <- df_clean_OP %>%
  group_by(Date, OP_3x3_portfolio) %>%
  summarise(
    port_ret = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  )

library(tidyr)

OP_3x3_wide <- OP_3x3_returns %>%
  select(Date, OP_3x3_portfolio, port_ret) %>%
  pivot_wider(
    names_from = OP_3x3_portfolio,
    values_from = port_ret
  ) %>%
  arrange(Date)

View(OP_3x3_wide)

FF5_test_3x3_OP <- FF5 %>%
  left_join(OP_3x3_wide, by = "Date")

View(FF5_test_3x3_OP)

portfolio_cols <- names(OP_3x3_wide)[names(OP_3x3_wide) != "Date"]

FF5_test_3x3_OP <- FF5_test_3x3_OP %>%
  mutate(
    across(
      all_of(portfolio_cols),
      ~ .x - Rf,
      .names = "{.col}_excess"
    )
  )

View(FF5_test_3x3_OP)

#######calculate avg_excess_returns

avg_excess_returns_3x3_OP <- FF5_test_3x3_OP %>%
  select(ends_with("_excess")) %>%
  summarise(
    across(everything(), ~ mean(.x, na.rm = TRUE))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "portfolio",
    values_to = "avg_excess_return"
  ) %>%
  mutate(
    portfolio = gsub("_excess$", "", portfolio)
  ) %>%
  arrange(desc(avg_excess_return))

View(avg_excess_returns_3x3_OP)


########run regression on 3x3- size and OP###################################################################################################################
library(dplyr)

names(OP_3x3_wide) <- names(OP_3x3_wide) %>%
  gsub(" ", "_", .)
names(FF5_test_3x3_OP) <- names(FF5_test_3x3_OP) %>%
  gsub(" ", "_", .)
excess_cols <- names(FF5_test_3x3_OP)[grepl("_excess$", names(FF5_test_3x3_OP))]
excess_cols <- setdiff(excess_cols, "Mkt_excess")

ff5_models <- lapply(excess_cols, function(dep_var) {
  lm(
    as.formula(paste(dep_var, "~ Mkt_excess + SMB + HML + RMW + CMA")),
    data = FF5_test_3x3_OP
  )
})

names(ff5_models) <- excess_cols

names(ff5_models)
summary(ff5_models[["Small_Weak_excess"]])


#complete results###############
sig_code <- function(p) {
  cut(p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", ""),
      right = TRUE)
}

ff5_results_OP <- lapply(names(ff5_models), function(name) {
  
  model <- ff5_models[[name]]
  smry  <- summary(model)
  coefs <- smry$coefficients
  fstat <- smry$fstatistic
  
  data.frame(
    portfolio = name,
    
    # coefficients
    alpha     = coefs["(Intercept)", "Estimate"],
    beta_mkt  = coefs["Mkt_excess", "Estimate"],
    beta_smb  = coefs["SMB", "Estimate"],
    beta_hml  = coefs["HML", "Estimate"],
    beta_rmw  = coefs["RMW", "Estimate"],
    beta_cma  = coefs["CMA", "Estimate"],
    
    # t-statistics
    t_alpha   = coefs["(Intercept)", "t value"],
    t_mkt     = coefs["Mkt_excess", "t value"],
    t_smb     = coefs["SMB", "t value"],
    t_hml     = coefs["HML", "t value"],
    t_rmw     = coefs["RMW", "t value"],
    t_cma     = coefs["CMA", "t value"],
    
    # p-values
    p_alpha   = coefs["(Intercept)", "Pr(>|t|)"],
    p_mkt     = coefs["Mkt_excess", "Pr(>|t|)"],
    p_smb     = coefs["SMB", "Pr(>|t|)"],
    p_hml     = coefs["HML", "Pr(>|t|)"],
    p_rmw     = coefs["RMW", "Pr(>|t|)"],
    p_cma     = coefs["CMA", "Pr(>|t|)"],
    
    # significance codes
    sig_alpha = sig_code(coefs["(Intercept)", "Pr(>|t|)"]),
    sig_mkt   = sig_code(coefs["Mkt_excess", "Pr(>|t|)"]),
    sig_smb   = sig_code(coefs["SMB", "Pr(>|t|)"]),
    sig_hml   = sig_code(coefs["HML", "Pr(>|t|)"]),
    sig_rmw   = sig_code(coefs["RMW", "Pr(>|t|)"]),
    sig_cma   = sig_code(coefs["CMA", "Pr(>|t|)"]),
    
    # model fit
    r2        = smry$r.squared,
    adj_r2    = smry$adj.r.squared,
    
    # residual standard error
    resid_se  = smry$sigma,
    
    # F-statistic, degrees of freedom, and overall p-value
    f_stat    = unname(fstat[1]),
    df_num    = unname(fstat[2]),
    df_den    = unname(fstat[3]),
    model_p   = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  )
}) %>%
  dplyr::bind_rows()

View(ff5_results_OP)


mean(ff5_results_OP$r2, na.rm = TRUE)

print(summary(ff5_models[[1]]))






###############################3x3 portfolio- size and BM###################################################################################################################################################

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)

# -----------------------------#
# 1) Lagged variables
# -----------------------------#

df_3x3 <- df

df_3x3 <- df_3x3 %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    tot_assets_tminus2 = prev_distinct(tot_assets_tminus1)
  ) %>%
  ungroup()

# -----------------------------#
# 2) June sorting dataset
# -----------------------------#

df_factors_june_3x3 <- df_3x3 %>%
  mutate(
    Date_m = as.Date(as.yearmon(Date, "%Y-%m")),
    year   = year(Date_m),
    month  = month(Date_m),
    
    BM  = equity_tminus1 / mark_cap_tminus1,
    OP  = op_prof_tminus1 / equity_tminus1,
    INV = (tot_assets_tminus1 - tot_assets_tminus2) / tot_assets_tminus2
  ) %>%
  filter(month == 6) %>%
  select(
    Date = Date_m, year, Ticker, ret, BM, OP, INV,
    equity_tminus1, mark_cap_tminus1
  )

# -----------------------------#
# 3) Assign 3×3 portfolios
# -----------------------------#

df_factors_june_3x3 <- df_factors_june_3x3 %>%
  mutate(
    BM = as.numeric(BM),
    mark_cap_tminus1 = as.numeric(mark_cap_tminus1)
  ) %>%
  group_by(year) %>%
  mutate(
    SIZE_30 = quantile(mark_cap_tminus1, 0.3, na.rm = TRUE),
    SIZE_70 = quantile(mark_cap_tminus1, 0.7, na.rm = TRUE),
    
    SIZE_port_3 = case_when(
      is.na(mark_cap_tminus1) ~ NA_character_,
      mark_cap_tminus1 <= SIZE_30 ~ "Small",
      mark_cap_tminus1 <= SIZE_70 ~ "Neutral",
      TRUE ~ "Big"
    ),
    
    BM_30 = quantile(BM, 0.3, na.rm = TRUE),
    BM_70 = quantile(BM, 0.7, na.rm = TRUE),
    
    BM_port_3 = case_when(
      is.na(BM) ~ NA_character_,
      BM <= BM_30 ~ "Low",
      BM <= BM_70 ~ "Neutral",
      TRUE ~ "High"
    ),
    
    BM_3x3_portfolio = ifelse(
      is.na(SIZE_port_3) | is.na(BM_port_3),
      NA,
      paste(SIZE_port_3, BM_port_3, sep = "_")
    )
    
  ) %>%
  ungroup() %>%
  select(Date, year, Ticker, BM_3x3_portfolio) %>%
  filter(!is.na(BM_3x3_portfolio))

# -----------------------------#
# 4) Merge portfolios to monthly data
# -----------------------------#

df_3x3 <- df_3x3 %>%
  mutate(
    Date = as.Date(as.yearmon(Date, "%Y-%m")),
    year = year(Date),
    month = month(Date),
    sort_year = ifelse(month >= 7, year, year - 1)
  )

df_factors_june_3x3 <- df_factors_june_3x3 %>%
  rename(sort_year = year)


df_merged_factors_3x3 <- df_3x3 %>%
  left_join(
    df_factors_june_3x3,
    by = c("Ticker", "sort_year")
  ) %>%
  select(Date=Date.x, Ticker, mark_cap_tminus1, ret, BM_3x3_portfolio)

View(df_merged_factors_3x3)

# -----------------------------#
# 5) Sanity check: firms per portfolio
# -----------------------------#

portfolio_counts_3x3 <- df_merged_factors_3x3 %>%
  filter(!is.na(BM_3x3_portfolio), !is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date, BM_3x3_portfolio) %>%
  summarise(n_firms = n(), .groups = "drop")

write_xlsx(portfolio_counts_3x3, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/portfolio_counts_3x3.xlsx")


View(portfolio_counts_3x3)

# Average firms per portfolio
portfolio_counts_3x3 %>%
  group_by(BM_3x3_portfolio) %>%
  summarise(avg_firms = mean(n_firms, na.rm = TRUE)) %>%
  ggplot(aes(x = BM_3x3_portfolio, y = avg_firms)) +
  geom_bar(stat = "identity") +
  theme_minimal()

portfolio_counts_3x3 %>%
  group_by(BM_3x3_portfolio) %>%
  summarise(avg_firms = mean(n_firms, na.rm = TRUE)) %>%
  ggplot(aes(x = BM_3x3_portfolio, y = avg_firms)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Number of Firms per 3x3 B/M Portfolio",
    x = "B/M Portfolio",
    y = "Average Number of Firms"
  ) +
  theme_minimal()

# Yearly averages
portfolio_yearly_3x3 <- portfolio_counts_3x3 %>%
  mutate(year = year(Date)) %>%
  group_by(year, BM_3x3_portfolio) %>%
  summarise(avg_firms = mean(n_firms, na.rm = TRUE), .groups = "drop")

ggplot(portfolio_yearly_3x3, aes(x = BM_3x3_portfolio, y = avg_firms)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year) +
  theme_minimal()

# -----------------------------#
# 6) Compute portfolio returns
# -----------------------------#

df_clean_3x3 <- df_merged_factors_3x3 %>%
  filter(
    !is.na(BM_3x3_portfolio),
    !is.na(ret),
    !is.na(mark_cap_tminus1)
  )

BM_3x3_returns <- df_clean_3x3 %>%
  group_by(Date, BM_3x3_portfolio) %>%
  summarise(
    port_ret = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE),
    .groups = "drop"
  )

BM_3x3_wide <- BM_3x3_returns %>%
  pivot_wider(
    names_from = BM_3x3_portfolio,
    values_from = port_ret
  ) %>%
  arrange(Date)

names(BM_3x3_wide) <- gsub(" ", "_", names(BM_3x3_wide))

# -----------------------------#
# 7) Merge with FF5 factors
# -----------------------------#

FF5_test_3x3 <- FF5 %>%
  left_join(BM_3x3_wide, by = "Date")

names(FF5_test_3x3) <- gsub(" ", "_", names(FF5_test_3x3))

# -----------------------------#
# 8) Create excess returns
# -----------------------------#

portfolio_cols <- names(BM_3x3_wide)[names(BM_3x3_wide) != "Date"]

FF5_test_3x3 <- FF5_test_3x3 %>%
  mutate(
    across(
      all_of(portfolio_cols),
      ~ .x - Rf,
      .names = "{.col}_excess"
    )
  )

###calculate average excess returns
avg_excess_returns_3x3 <- FF5_test_3x3 %>%
  select(ends_with("_excess")) %>%
  summarise(
    across(everything(), ~ mean(.x, na.rm = TRUE))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "portfolio",
    values_to = "avg_excess_return"
  ) %>%
  mutate(
    portfolio = gsub("_excess$", "", portfolio)
  ) %>%
  arrange(desc(avg_excess_return))

View(avg_excess_returns_3x3)

# -----------------------------#
# 9) Run regressions
# -----------------------------#

excess_cols <- names(FF5_test_3x3)[grepl("_excess$", names(FF5_test_3x3))]
excess_cols <- setdiff(excess_cols, "Mkt_excess")

ff3x3_models <- lapply(excess_cols, function(dep_var) {
  lm(
    as.formula(paste(dep_var, "~ Mkt_excess + SMB + HML + RMW + CMA")),
    data = FF5_test_3x3
  )
})

names(ff3x3_models) <- excess_cols

# -----------------------------#
# 10) Collect results
# -----------------------------#


###another try

sig_code <- function(p) {
  cut(p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", ""),
      right = TRUE)
}

ff3x3_results <- lapply(names(ff3x3_models), function(name) {
  
  model <- ff3x3_models[[name]]
  smry  <- summary(model)
  coefs <- smry$coefficients
  fstat <- smry$fstatistic
  
  data.frame(
    portfolio = name,
    
    # coefficients
    alpha     = coefs["(Intercept)", "Estimate"],
    beta_mkt  = coefs["Mkt_excess", "Estimate"],
    beta_smb  = coefs["SMB", "Estimate"],
    beta_hml  = coefs["HML", "Estimate"],
    beta_rmw  = coefs["RMW", "Estimate"],
    beta_cma  = coefs["CMA", "Estimate"],
    
    # t-statistics
    t_alpha   = coefs["(Intercept)", "t value"],
    t_mkt     = coefs["Mkt_excess", "t value"],
    t_smb     = coefs["SMB", "t value"],
    t_hml     = coefs["HML", "t value"],
    t_rmw     = coefs["RMW", "t value"],
    t_cma     = coefs["CMA", "t value"],
    
    # p-values
    p_alpha   = coefs["(Intercept)", "Pr(>|t|)"],
    p_mkt     = coefs["Mkt_excess", "Pr(>|t|)"],
    p_smb     = coefs["SMB", "Pr(>|t|)"],
    p_hml     = coefs["HML", "Pr(>|t|)"],
    p_rmw     = coefs["RMW", "Pr(>|t|)"],
    p_cma     = coefs["CMA", "Pr(>|t|)"],
    
    # significance codes
    sig_alpha = sig_code(coefs["(Intercept)", "Pr(>|t|)"]),
    sig_mkt   = sig_code(coefs["Mkt_excess", "Pr(>|t|)"]),
    sig_smb   = sig_code(coefs["SMB", "Pr(>|t|)"]),
    sig_hml   = sig_code(coefs["HML", "Pr(>|t|)"]),
    sig_rmw   = sig_code(coefs["RMW", "Pr(>|t|)"]),
    sig_cma   = sig_code(coefs["CMA", "Pr(>|t|)"]),
    
    # model fit
    r2        = smry$r.squared,
    adj_r2    = smry$adj.r.squared,
    
    # residual standard error
    resid_se  = smry$sigma,
    
    # F-statistic, degrees of freedom, and overall p-value
    f_stat    = unname(fstat[1]),
    df_num    = unname(fstat[2]),
    df_den    = unname(fstat[3]),
    model_p   = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  )
}) %>%
  dplyr::bind_rows()

View(ff3x3_results)




###############################3x3 portfolio- size and INV###################################################################################################################################################
#3x3 portfolios - Size and INV:##############################################################################
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

df_large_INV <- df

df_large_INV <- df_large_INV %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    tot_assets_tminus2 = prev_distinct(tot_assets_tminus1)
  ) %>%
  ungroup()

# Construct factors
df_factors_june_INV <- df_large_INV %>%
  mutate(
    Date_m = as.Date(as.yearmon(Date, "%Y-%m")),
    year   = year(Date_m),
    month  = month(Date_m),
    
    BM  = equity_tminus1 / mark_cap_tminus1,
    OP  = op_prof_tminus1 / equity_tminus1,
    INV = (tot_assets_tminus1 - tot_assets_tminus2) / tot_assets_tminus2
  ) %>%
  filter(month == 6) %>%
  select(Date = Date_m, year, Ticker, ret, BM, OP, INV,
         equity_tminus1, mark_cap_tminus1, op_prof_tminus1,
         tot_assets_tminus1, tot_assets_tminus2)

# Check cross-sectionality
cs_counts_INV <- df_factors_june_INV %>%
  group_by(year) %>%
  summarise(
    n_mark_cap = sum(!is.na(mark_cap_tminus1)),
    n_BM   = sum(!is.na(BM)),
    n_OP   = sum(!is.na(OP)),
    n_INV  = sum(!is.na(INV))
  ) %>%
  arrange(year)
View(cs_counts_INV)

####################################################################################################################################
# 3x3 Sort: Size terciles x INV terciles
####################################################################################################################################
df_factors_june_INV <- df_factors_june_INV %>%
  mutate(
    INV = as.numeric(INV),
    mark_cap_tminus1 = as.numeric(mark_cap_tminus1)
  ) %>%
  group_by(year) %>%
  mutate(
    SIZE_30 = quantile(mark_cap_tminus1, 0.3, na.rm = TRUE),
    SIZE_70 = quantile(mark_cap_tminus1, 0.7, na.rm = TRUE),
    
    SIZE_port_3 = case_when(
      is.na(mark_cap_tminus1) ~ NA_character_,
      mark_cap_tminus1 <= SIZE_30 ~ "Small",
      mark_cap_tminus1 <= SIZE_70 ~ "Neutral",
      TRUE ~ "Big"
    ),
    
    INV_30 = quantile(INV, 0.3, na.rm = TRUE),
    INV_70 = quantile(INV, 0.7, na.rm = TRUE),
    
    INV_port_3 = case_when(
      is.na(INV) ~ NA_character_,
      INV <= INV_30 ~ "Conservative",
      INV <= INV_70 ~ "Neutral",
      TRUE ~ "Aggressive"
    ),
    
    INV_3x3_portfolio = ifelse(
      is.na(SIZE_port_3) | is.na(INV_port_3),
      NA,
      paste(SIZE_port_3, INV_port_3, sep = "_")
    )
    
  ) %>%
  ungroup() %>%
  filter(!is.na(INV_3x3_portfolio)) %>%
  select(Date, year, Ticker, INV_3x3_portfolio)

View(df_factors_june_INV)

##merge the datasets together
df_large_INV <- df_large_INV %>%
  mutate(
    year = year(Date),
    month = month(Date),
    sort_year = ifelse(month >= 7, year, year - 1)
  )

# Rename year to sort_year for merge
df_factors_june_INV <- df_factors_june_INV %>%
  rename(sort_year = year)

# Merge df_large_INV with df_factors_june_INV by Ticker and sort_year
df_merged_factors_INV <- df_large_INV %>%
  left_join(
    df_factors_june_INV,
    by = c("Ticker", "sort_year")
  )

df_merged_factors_INV <- df_merged_factors_INV %>%
  select(Date = Date.x, Ticker, mark_cap_tminus1, ret, INV_3x3_portfolio)

## Sanity check: portfolio counts


portfolio_counts_INV <- df_merged_factors_INV %>%
  filter(!is.na(INV_3x3_portfolio), !is.na(ret), !is.na(mark_cap_tminus1)) %>%
  group_by(Date, INV_3x3_portfolio) %>%
  summarise(
    n_firms = n(),
    .groups = "drop"
  ) %>%
  arrange(Date, INV_3x3_portfolio)
View(portfolio_counts_INV)


write_xlsx(portfolio_counts_INV, "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/portfolio_counts_INV.xlsx")

library(ggplot2)
portfolio_counts_INV %>%
  group_by(INV_3x3_portfolio) %>%
  summarise(avg_firms = mean(n_firms, na.rm = TRUE)) %>%
  ggplot(aes(x = INV_3x3_portfolio, y = avg_firms)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Number of Firms per Portfolio (Size x INV)",
    x = "Portfolio",
    y = "Average Number of Firms"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

portfolio_yearly_INV <- portfolio_counts_INV %>%
  mutate(year = year(Date)) %>%
  group_by(year, INV_3x3_portfolio) %>%
  summarise(
    avg_firms = mean(n_firms, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(portfolio_yearly_INV, aes(x = INV_3x3_portfolio, y = avg_firms)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year) +
  labs(
    title = "Yearly Average Number of Firms per Portfolio (Size x INV)",
    x = "Portfolio",
    y = "Average Number of Firms"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compute value-weighted portfolio returns
df_clean_INV <- df_merged_factors_INV %>%
  filter(
    !is.na(INV_3x3_portfolio),
    !is.na(ret),
    !is.na(mark_cap_tminus1)
  )

INV_3x3_returns <- df_clean_INV %>%
  group_by(Date, INV_3x3_portfolio) %>%
  summarise(
    port_ret = weighted.mean(ret, mark_cap_tminus1, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  )

INV_3x3_wide <- INV_3x3_returns %>%
  select(Date, INV_3x3_portfolio, port_ret) %>%
  pivot_wider(
    names_from = INV_3x3_portfolio,
    values_from = port_ret
  ) %>%
  arrange(Date)
View(INV_3x3_wide)

# Merge with FF5 factors and compute excess returns
FF5_test_3x3_INV <- FF5 %>%
  left_join(INV_3x3_wide, by = "Date")

portfolio_cols_INV <- names(INV_3x3_wide)[names(INV_3x3_wide) != "Date"]

FF5_test_3x3_INV <- FF5_test_3x3_INV %>%
  mutate(
    across(
      all_of(portfolio_cols_INV),
      ~ .x - Rf,
      .names = "{.col}_excess"
    )
  )
View(FF5_test_3x3_INV)

# Average excess returns
avg_excess_returns_3x3_INV <- FF5_test_3x3_INV %>%
  select(ends_with("_excess")) %>%
  summarise(
    across(everything(), ~ mean(.x, na.rm = TRUE))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "portfolio",
    values_to = "avg_excess_return"
  ) %>%
  mutate(
    portfolio = gsub("_excess$", "", portfolio)
  ) %>%
  arrange(desc(avg_excess_return))
View(avg_excess_returns_3x3_INV)

########run regression on 3x3 - Size and INV###################################################################################################################
names(INV_3x3_wide) <- names(INV_3x3_wide) %>%
  gsub(" ", "_", .)
names(FF5_test_3x3_INV) <- names(FF5_test_3x3_INV) %>%
  gsub(" ", "_", .)

excess_cols_INV <- names(FF5_test_3x3_INV)[grepl("_excess$", names(FF5_test_3x3_INV))]
excess_cols_INV <- setdiff(excess_cols_INV, "Mkt_excess")

ff5_models_INV <- lapply(excess_cols_INV, function(dep_var) {
  lm(
    as.formula(paste(dep_var, "~ Mkt_excess + SMB + HML + RMW + CMA")),
    data = FF5_test_3x3_INV
  )
})
names(ff5_models_INV) <- excess_cols_INV

# Check one model
summary(ff5_models_INV[["Small_Conservative_excess"]])

# Complete results
sig_code <- function(p) {
  cut(p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", ""),
      right = TRUE)
}

ff5_results_INV <- lapply(names(ff5_models_INV), function(name) {
  
  model <- ff5_models_INV[[name]]
  smry  <- summary(model)
  coefs <- smry$coefficients
  fstat <- smry$fstatistic
  
  data.frame(
    portfolio = name,
    
    # coefficients
    alpha     = coefs["(Intercept)", "Estimate"],
    beta_mkt  = coefs["Mkt_excess", "Estimate"],
    beta_smb  = coefs["SMB", "Estimate"],
    beta_hml  = coefs["HML", "Estimate"],
    beta_rmw  = coefs["RMW", "Estimate"],
    beta_cma  = coefs["CMA", "Estimate"],
    
    # significance codes
    sig_alpha = sig_code(coefs["(Intercept)", "Pr(>|t|)"]),
    sig_mkt   = sig_code(coefs["Mkt_excess", "Pr(>|t|)"]),
    sig_smb   = sig_code(coefs["SMB", "Pr(>|t|)"]),
    sig_hml   = sig_code(coefs["HML", "Pr(>|t|)"]),
    sig_rmw   = sig_code(coefs["RMW", "Pr(>|t|)"]),
    sig_cma   = sig_code(coefs["CMA", "Pr(>|t|)"]),
    
    # t-statistics
    t_alpha   = coefs["(Intercept)", "t value"],
    t_mkt     = coefs["Mkt_excess", "t value"],
    t_smb     = coefs["SMB", "t value"],
    t_hml     = coefs["HML", "t value"],
    t_rmw     = coefs["RMW", "t value"],
    t_cma     = coefs["CMA", "t value"],
    
    # p-values
    p_alpha   = coefs["(Intercept)", "Pr(>|t|)"],
    p_mkt     = coefs["Mkt_excess", "Pr(>|t|)"],
    p_smb     = coefs["SMB", "Pr(>|t|)"],
    p_hml     = coefs["HML", "Pr(>|t|)"],
    p_rmw     = coefs["RMW", "Pr(>|t|)"],
    p_cma     = coefs["CMA", "Pr(>|t|)"],
    
    # model fit
    r2        = smry$r.squared,
    adj_r2    = smry$adj.r.squared,
    resid_se  = smry$sigma,
    
    # F-statistic
    f_stat    = unname(fstat[1]),
    df_num    = unname(fstat[2]),
    df_den    = unname(fstat[3]),
    model_p   = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  )
}) %>%
  dplyr::bind_rows()

View(ff5_results_INV)
mean(ff5_results_INV$r2, na.rm = TRUE)


####################descriptive statistics############################################################################
install.packages("moments")
install.packages("tseries")
library(dplyr)
library(tidyr)
library(moments)  # for skewness, kurtosis
library(tseries)  # for jarque.bera.test

# Panel A: Summary Statistics
panel_A <- FF5 %>%
  select(Mkt_excess, SMB, HML, RMW, CMA) %>%
  pivot_longer(everything(), names_to = "Factor", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(Factor) %>%
  summarise(
    Mean     = mean(value),
    Std_Dev  = sd(value),
    Sharpe   = mean(value) / sd(value) * sqrt(12),
    Skewness = skewness(value),
    Kurtosis = kurtosis(value),
    Min      = min(value),
    Max      = max(value),
    JB_pval  = jarque.bera.test(value)$p.value
  ) %>%
  mutate(
    JB_sig = case_when(
      JB_pval < 0.01 ~ "***",
      JB_pval < 0.05 ~ "**",
      JB_pval < 0.10 ~ "*",
      TRUE ~ ""
    ),
    Factor = factor(Factor, levels = c("Mkt_excess", "SMB", "HML", "RMW", "CMA"))
  ) %>%
  arrange(Factor)

View(panel_A)

# Panel B: Correlation Matrix
panel_B <- FF5 %>%
  select(Mkt_excess, SMB, HML, RMW, CMA) %>%
  cor(use = "complete.obs") %>%
  round(4)

View(panel_B)

# Helper function: average across all 27 portfolios per regime
summarise_regime <- function(results_BM, results_OP, results_INV) {
  bind_rows(results_BM, results_OP, results_INV) %>%
    group_by(regime) %>%
    summarise(
      avg_alpha    = mean(alpha, na.rm = TRUE),
      avg_abs_alpha = mean(abs(alpha), na.rm = TRUE),
      avg_beta_mkt = mean(beta_mkt, na.rm = TRUE),
      avg_beta_smb = mean(beta_smb, na.rm = TRUE),
      avg_beta_hml = mean(beta_hml, na.rm = TRUE),
      avg_beta_rmw = mean(beta_rmw, na.rm = TRUE),
      avg_beta_cma = mean(beta_cma, na.rm = TRUE),
      avg_R2       = mean(r2, na.rm = TRUE),
      avg_adj_R2   = mean(adj_r2, na.rm = TRUE),
      n_sig_alpha_5pct  = sum(p_alpha < 0.05, na.rm = TRUE),
      n_sig_alpha_10pct = sum(p_alpha < 0.10, na.rm = TRUE),
      n_portfolios = n(),
      avg_df       = mean(df_den, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_sig_5pct  = paste0(n_sig_alpha_5pct, "/", n_portfolios, " (", round(n_sig_alpha_5pct / n_portfolios * 100), "%)"),
      pct_sig_10pct = paste0(n_sig_alpha_10pct, "/", n_portfolios, " (", round(n_sig_alpha_10pct / n_portfolios * 100), "%)")
    )
}

# Baseline
baseline_summary <- bind_rows(ff3x3_results, ff5_results_OP, ff5_results_INV) %>%
  mutate(regime = "Baseline") %>%
  group_by(regime) %>%
  summarise(
    avg_alpha    = mean(alpha, na.rm = TRUE),
    avg_abs_alpha = mean(abs(alpha), na.rm = TRUE),
    avg_beta_mkt = mean(beta_mkt, na.rm = TRUE),
    avg_beta_smb = mean(beta_smb, na.rm = TRUE),
    avg_beta_hml = mean(beta_hml, na.rm = TRUE),
    avg_beta_rmw = mean(beta_rmw, na.rm = TRUE),
    avg_beta_cma = mean(beta_cma, na.rm = TRUE),
    avg_R2       = mean(r2, na.rm = TRUE),
    avg_adj_R2   = mean(adj_r2, na.rm = TRUE),
    n_sig_alpha_5pct  = sum(p_alpha < 0.05, na.rm = TRUE),
    n_sig_alpha_10pct = sum(p_alpha < 0.10, na.rm = TRUE),
    n_portfolios = n(),
    avg_df       = mean(df_den, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_sig_5pct  = paste0(n_sig_alpha_5pct, "/", n_portfolios, " (", round(n_sig_alpha_5pct / n_portfolios * 100), "%)"),
    pct_sig_10pct = paste0(n_sig_alpha_10pct, "/", n_portfolios, " (", round(n_sig_alpha_10pct / n_portfolios * 100), "%)")
  )

# All regime summaries
regime_summary <- bind_rows(
  baseline_summary,
  summarise_regime(results_IR_BM, results_IR_OP, results_IR_INV),
  summarise_regime(results_S_BM, results_S_OP, results_S_INV),
  summarise_regime(results_SH_BM, results_SH_OP, results_SH_INV),
  summarise_regime(results_combined_BM, results_combined_OP, results_combined_INV),
  # Robustness
  summarise_regime(results_CHFUSD_BM, results_CHFUSD_OP, results_CHFUSD_INV),
  summarise_regime(results_S_EU_BM, results_S_EU_OP, results_S_EU_INV)
)

View(regime_summary)

# Master Panel A: all regimes in one table
master_panel_A <- bind_rows(
  panel_A %>% mutate(regime = "Baseline"),
  panel_A_IR,
  panel_A_S,
  panel_A_SH,
  panel_A_combined,
  # Robustness
  panel_A_CHFUSD %>% mutate(regime = paste0(regime, " (robustness)")),
  panel_A_S_EU %>% mutate(regime = paste0(regime, " (robustness)"))
)
View(master_panel_A)

# Master Panel B: all regimes in one table
master_panel_B <- bind_rows(
  panel_B %>% as.data.frame() %>% mutate(Factor = rownames(panel_B), regime = "Baseline"),
  panel_B_IR_high,
  panel_B_IR_low,
  panel_B_S_high,
  panel_B_S_low,
  panel_B_SH_pos,
  panel_B_SH_neg,
  panel_B_SL_RN,
  panel_B_SL_RP,
  panel_B_SH_RN,
  panel_B_SH_RP,
  # Robustness
  panel_B_CHFUSD_pos %>% mutate(regime = paste0(regime, " (robustness)")),
  panel_B_CHFUSD_neg %>% mutate(regime = paste0(regime, " (robustness)")),
  panel_B_S_EU_high %>% mutate(regime = paste0(regime, " (robustness)")),
  panel_B_S_EU_low %>% mutate(regime = paste0(regime, " (robustness)"))
)
View(master_panel_B)

# Updated summarise_regime function with factor significance#####################################
summarise_regime <- function(results_BM, results_OP, results_INV) {
  bind_rows(results_BM, results_OP, results_INV) %>%
    group_by(regime) %>%
    summarise(
      avg_alpha    = mean(alpha, na.rm = TRUE),
      avg_abs_alpha = mean(abs(alpha), na.rm = TRUE),
      avg_beta_mkt = mean(beta_mkt, na.rm = TRUE),
      avg_beta_smb = mean(beta_smb, na.rm = TRUE),
      avg_beta_hml = mean(beta_hml, na.rm = TRUE),
      avg_beta_rmw = mean(beta_rmw, na.rm = TRUE),
      avg_beta_cma = mean(beta_cma, na.rm = TRUE),
      avg_p_mkt    = mean(p_mkt, na.rm = TRUE),
      avg_p_smb    = mean(p_smb, na.rm = TRUE),
      avg_p_hml    = mean(p_hml, na.rm = TRUE),
      avg_p_rmw    = mean(p_rmw, na.rm = TRUE),
      avg_p_cma    = mean(p_cma, na.rm = TRUE),
      avg_R2       = mean(r2, na.rm = TRUE),
      avg_adj_R2   = mean(adj_r2, na.rm = TRUE),
      n_sig_alpha_5pct  = sum(p_alpha < 0.05, na.rm = TRUE),
      n_sig_alpha_10pct = sum(p_alpha < 0.10, na.rm = TRUE),
      n_portfolios = n(),
      avg_df       = mean(df_den, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      sig_mkt = sig_code(avg_p_mkt),
      sig_smb = sig_code(avg_p_smb),
      sig_hml = sig_code(avg_p_hml),
      sig_rmw = sig_code(avg_p_rmw),
      sig_cma = sig_code(avg_p_cma),
      pct_sig_5pct  = paste0(n_sig_alpha_5pct, "/", n_portfolios, " (", round(n_sig_alpha_5pct / n_portfolios * 100), "%)"),
      pct_sig_10pct = paste0(n_sig_alpha_10pct, "/", n_portfolios, " (", round(n_sig_alpha_10pct / n_portfolios * 100), "%)")
    )
}

# Baseline (same logic, just without group_by regime)
baseline_summary <- bind_rows(ff3x3_results, ff5_results_OP, ff5_results_INV) %>%
  mutate(regime = "Baseline") %>%
  group_by(regime) %>%
  summarise(
    avg_alpha    = mean(alpha, na.rm = TRUE),
    avg_abs_alpha = mean(abs(alpha), na.rm = TRUE),
    avg_beta_mkt = mean(beta_mkt, na.rm = TRUE),
    avg_beta_smb = mean(beta_smb, na.rm = TRUE),
    avg_beta_hml = mean(beta_hml, na.rm = TRUE),
    avg_beta_rmw = mean(beta_rmw, na.rm = TRUE),
    avg_beta_cma = mean(beta_cma, na.rm = TRUE),
    avg_p_mkt    = mean(p_mkt, na.rm = TRUE),
    avg_p_smb    = mean(p_smb, na.rm = TRUE),
    avg_p_hml    = mean(p_hml, na.rm = TRUE),
    avg_p_rmw    = mean(p_rmw, na.rm = TRUE),
    avg_p_cma    = mean(p_cma, na.rm = TRUE),
    avg_R2       = mean(r2, na.rm = TRUE),
    avg_adj_R2   = mean(adj_r2, na.rm = TRUE),
    n_sig_alpha_5pct  = sum(p_alpha < 0.05, na.rm = TRUE),
    n_sig_alpha_10pct = sum(p_alpha < 0.10, na.rm = TRUE),
    n_portfolios = n(),
    avg_df       = mean(df_den, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sig_mkt = sig_code(avg_p_mkt),
    sig_smb = sig_code(avg_p_smb),
    sig_hml = sig_code(avg_p_hml),
    sig_rmw = sig_code(avg_p_rmw),
    sig_cma = sig_code(avg_p_cma),
    pct_sig_5pct  = paste0(n_sig_alpha_5pct, "/", n_portfolios, " (", round(n_sig_alpha_5pct / n_portfolios * 100), "%)"),
    pct_sig_10pct = paste0(n_sig_alpha_10pct, "/", n_portfolios, " (", round(n_sig_alpha_10pct / n_portfolios * 100), "%)")
  )

# Build master table
regime_summary <- bind_rows(
  baseline_summary,
  summarise_regime(results_IR_BM, results_IR_OP, results_IR_INV),
  summarise_regime(results_S_BM, results_S_OP, results_S_INV),
  summarise_regime(results_SH_BM, results_SH_OP, results_SH_INV),
  summarise_regime(results_combined_BM, results_combined_OP, results_combined_INV),
  summarise_regime(results_CHFUSD_BM, results_CHFUSD_OP, results_CHFUSD_INV),
  summarise_regime(results_S_EU_BM, results_S_EU_OP, results_S_EU_INV)
)

View(regime_summary)



####saving#######################################################################

base_path <- "Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/RDS"
saveRDS(df,                paste0(base_path, "01_df_with_lags.rds"))
saveRDS(df_merged_factors, paste0(base_path, "02_df_merged_factors.rds"))
saveRDS(factors_final,     paste0(base_path, "03_factors_final.rds"))
saveRDS(FF5,               paste0(base_path, "04_FF5.rds"))
saveRDS(FF5_port,          paste0(base_path, "05_FF5_port.rds"))
saveRDS(ff5_results,       paste0(base_path, "06_ff5_results_5x5_BM.rds"))
saveRDS(ff5_results_OP,    paste0(base_path, "07_ff5_results_3x3_OP.rds"))
saveRDS(ff3x3_results,     paste0(base_path, "08_ff3x3_results_BM.rds"))
saveRDS(ff5_results_INV,   paste0(base_path, "09_ff5_results_3x3_INV.rds"))
saveRDS(panel_A,           paste0(base_path, "10_panel_A.rds"))
saveRDS(panel_B,           paste0(base_path, "11_panel_B.rds"))
saveRDS(external_factors,  paste0(base_path, "12_external_factors.rds"))

# 3x3 OP
saveRDS(df_merged_factors_OP, paste0(base_path, "13_df_merged_factors_OP.rds"))
saveRDS(OP_3x3_wide,          paste0(base_path, "14_OP_3x3_wide.rds"))
saveRDS(FF5_test_3x3_OP,      paste0(base_path, "15_FF5_test_3x3_OP.rds"))
saveRDS(avg_excess_returns_3x3_OP, paste0(base_path, "16_avg_excess_returns_3x3_OP.rds"))

# 3x3 BM
saveRDS(df_merged_factors_3x3, paste0(base_path, "17_df_merged_factors_3x3.rds"))
saveRDS(BM_3x3_wide,           paste0(base_path, "18_BM_3x3_wide.rds"))  # your naming
saveRDS(FF5_test_3x3,          paste0(base_path, "19_FF5_test_3x3_BM.rds"))
saveRDS(avg_excess_returns_3x3, paste0(base_path, "20_avg_excess_returns_3x3_BM.rds"))

# 3x3 INV
saveRDS(df_merged_factors_INV, paste0(base_path, "21_df_merged_factors_INV.rds"))
saveRDS(INV_3x3_wide,          paste0(base_path, "22_INV_3x3_wide.rds"))
saveRDS(FF5_test_3x3_INV,      paste0(base_path, "23_FF5_test_3x3_INV.rds"))
saveRDS(avg_excess_returns_3x3_INV, paste0(base_path, "24_avg_excess_returns_3x3_INV.rds"))


df              <- readRDS(paste0(base_path, "01_df_with_lags.rds"))
df_merged_factors <- readRDS(paste0(base_path, "02_df_merged_factors.rds"))
factors_final   <- readRDS(paste0(base_path, "03_factors_final.rds"))
FF5             <- readRDS(paste0(base_path, "04_FF5.rds"))
FF5_port        <- readRDS(paste0(base_path, "05_FF5_port.rds"))
ff5_results     <- readRDS(paste0(base_path, "06_ff5_results_5x5_BM.rds"))
ff5_results_OP  <- readRDS(paste0(base_path, "07_ff5_results_3x3_OP.rds"))
ff3x3_results   <- readRDS(paste0(base_path, "08_ff3x3_results_BM.rds"))
ff5_results_INV <- readRDS(paste0(base_path, "09_ff5_results_3x3_INV.rds"))
panel_A         <- readRDS(paste0(base_path, "10_panel_A.rds"))
panel_B         <- readRDS(paste0(base_path, "11_panel_B.rds"))
external_factors         <- readRDS(paste0(base_path, "12_external_factors.rds"))
# 3x3 OP
df_merged_factors_OP <- readRDS(paste0(base_path, "13_df_merged_factors_OP.rds"))
OP_3x3_wide          <- readRDS(paste0(base_path, "14_OP_3x3_wide.rds"))
FF5_test_3x3_OP      <- readRDS(paste0(base_path, "15_FF5_test_3x3_OP.rds"))
avg_excess_returns_3x3_OP <- readRDS(paste0(base_path, "16_avg_excess_returns_3x3_OP.rds"))

# 3x3 BM
df_merged_factors_3x3 <- readRDS(paste0(base_path, "17_df_merged_factors_3x3.rds"))
OP_3x3_wide           <- readRDS(paste0(base_path, "18_BM_3x3_wide.rds"))
FF5_test_3x3          <- readRDS(paste0(base_path, "19_FF5_test_3x3_BM.rds"))
avg_excess_returns_3x3 <- readRDS(paste0(base_path, "20_avg_excess_returns_3x3_BM.rds"))

# 3x3 INV
df_merged_factors_INV <- readRDS(paste0(base_path, "21_df_merged_factors_INV.rds"))
INV_3x3_wide          <- readRDS(paste0(base_path, "22_INV_3x3_wide.rds"))
FF5_test_3x3_INV      <- readRDS(paste0(base_path, "23_FF5_test_3x3_INV.rds"))
avg_excess_returns_3x3_INV <- readRDS(paste0(base_path, "24_avg_excess_returns_3x3_INV.rds"))


#########################figure 3-Rf######################################################
library(ggplot2)
library(dplyr)
library(grid)

rf_monthly

p <- rf_monthly %>%
  mutate(Date = as.Date(paste0(Date, "-01"))) %>%
  filter(!is.na(Date), !is.na(Rf)) %>%
  ggplot(aes(x = Date, y = Rf)) +
  geom_line(linewidth = 0.5, color = "black") +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4)
  ) +
  labs(
    x = "Date",
    y = "Rf (%)"
  ) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.length.x = unit(0.15, "cm"),
    axis.ticks.length.y = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p

#try 2
p <- rf_monthly %>%
  mutate(
    Date = as.Date(paste0(Date, "-01")),
    Rf = Rf * 100   # <-- convert to %
  ) %>%
  filter(!is.na(Date), !is.na(Rf)) %>%
  ggplot(aes(x = Date, y = Rf)) +
  geom_line(linewidth = 0.5, color = "black") +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4)
  ) +
  labs(
    x = "Date",
    y = "Rf (%)"
  ) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.length.x = unit(0.15, "cm"),
    axis.ticks.length.y = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p

######################figure 4- KOF and EU###################################

p <- external_factors %>%
  mutate(Date = as.Date(paste0(Date, "-01"))) %>%
  select(Date, KOF_CH, KOF_EU) %>%
  pivot_longer(
    cols = c(KOF_CH, KOF_EU),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(Date), !is.na(value)) %>%
  mutate(
    variable = factor(
      variable,
      levels = c("KOF_CH", "KOF_EU"),
      labels = c("Switzerland Sentiment Index", "EU Sentiment Index")
    )
  ) %>%
  ggplot(aes(x = Date, y = value, linetype = variable)) +
  geom_line(linewidth = 0.6, color = "black") +
  scale_linetype_manual(
    values = c("solid", "dashed")
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4)
  ) +
  labs(
    x = "Date",
    y = "Sentiment Index",
    linetype = NULL
  ) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p

#try 2

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

p <- external_factors %>%
  mutate(
    Date = as.Date(paste0(Date, "-01")),
    KOF_EU = ifelse(Date < as.Date("1999-01-01"), NA, KOF_EU)
  ) %>%
  select(Date, KOF_CH, KOF_EU) %>%
  pivot_longer(
    cols = c(KOF_CH, KOF_EU),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(Date), !is.na(value)) %>%
  mutate(
    variable = factor(
      variable,
      levels = c("KOF_CH", "KOF_EU"),
      labels = c("Switzerland Sentiment Index", "EU Sentiment Index")
    )
  ) %>%
  ggplot(aes(x = Date, y = value, linetype = variable)) +
  geom_line(linewidth = 0.6, color = "black") +
  scale_linetype_manual(
    values = c("solid", "dashed")
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4)
  ) +
  labs(
    x = "Date",
    y = "Sentiment Index",
    linetype = NULL
  ) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p

#############figure 5- factors box plot#################



library(dplyr)
library(tidyr)
library(ggplot2)


factors_box <- FF5 %>%
  select(Mkt_excess, SMB, HML, RMW, CMA) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Factor",
    values_to = "Return"
  ) %>%
  filter(!is.na(Return)) %>%
  mutate(
    Factor = factor(
      Factor,
      levels = c("Mkt_excess", "SMB", "HML", "RMW", "CMA"),
      labels = c("MKT", "SMB", "HML", "RMW", "CMA")
    )
  )

factors_box

p <- ggplot(factors_box, aes(x = Factor, y = Return)) +
  geom_boxplot(
    fill = "white",
    color = "black",
    linewidth = 0.4,
    outlier.size = 1,
    outlier.shape = 1
  ) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  scale_y_continuous(
    #labels = scales::percent_format(accuracy = 1),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    x = "Factor",
    y = "Monthly Return (%)"
  ) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey90", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

p

#############figure 6- cross sectionality#################

library(readxl)
cs_counts <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/cs_counts_1.xlsx")
cs_counts
library(ggplot2)
library(dplyr)
library(tidyr)

cs_counts %>%
  filter(!is.na(Date)) %>%
  mutate(Date = as.Date(paste0(Date, "-01"))) %>%
  pivot_longer(
    cols = -Date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = Date, y = value, linetype = variable)) +
  geom_line(color = "black", linewidth = 0.4) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "solid"),
    labels = c(
      "n_size_inputs" = "Size_inputs",
      "n_value_inputs" = "B/M_inputs",
      "n_prof_inputs" = "OP_inputs",
      "n_inv_inputs" = "INV_inputs",
      "n_ret" = "MKT_inputs"
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "Date",
    y = "Number of Companies",
    linetype = NULL
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position.inside = c(0.75, 0.75),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key = element_blank()
  )

#try 2
library(ggplot2)
library(dplyr)
library(tidyr)

cs_counts %>%
  filter(!is.na(Date)) %>%
  mutate(Date = as.Date(paste0(Date, "-01"))) %>%
  pivot_longer(
    cols = -Date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(variable %in% c(
    "n_ret",
    "n_size_inputs",
    "n_value_inputs",
    "n_prof_inputs",
    "n_inv_inputs"
  )) %>%
  mutate(variable = factor(
    variable,
    levels = c(
      "n_ret",            # MKT
      "n_size_inputs",    # Size
      "n_value_inputs",   # B/M
      "n_prof_inputs",    # OP
      "n_inv_inputs"      # INV
    )
  )) %>%
  ggplot(aes(x = Date, y = value, linetype = variable)) +
  geom_line(color = "black", linewidth = 0.4) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash", "longdash"),
    labels = c(
      "n_ret" = "MKT",
      "n_size_inputs" = "Market cap",
      "n_value_inputs" = "BM",
      "n_prof_inputs" = "OP",
      "n_inv_inputs" = "INV"
    )
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x = "Date",
    y = "Number of Companies",
    linetype = NULL
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position.inside = c(0.75, 0.75),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key = element_blank()
  )


#####figure 7- Average number of firms################################################
library(ggplot2)
library(dplyr)
install.packages("scales")
library(scales)

portfolio_counts_OP

library(dplyr)

portfolio_counts_OP <- portfolio_counts_OP %>%
  mutate(OP_3x3_portfolio = recode(OP_3x3_portfolio,
                                   "Big_Weak" = "S3 BM1",
                                   "Big_Neutral" = "S3 BM2",
                                   "Big_Robust" = "S3 BM3",
                                   "Neutral_Weak" = "S2 BM1",
                                   "Neutral_Neutral" = "S2 BM2",
                                   "Neutral_Robust" = "S2 BM3",
                                   "Small_Weak" = "S1 BM1",
                                   "Small_Neutral" = "S1 BM2",
                                   "Small_Robust" = "S1 BM3"
  ))

portfolio_counts_OP %>%
  group_by(OP_3x3_portfolio) %>%
  summarise(avg_firms = mean(n_firms, na.rm = TRUE)) %>%
  ggplot(aes(x = OP_3x3_portfolio, y = avg_firms)) +
  geom_bar(stat = "identity", fill = "grey80", color = "white", linewidth = 0.3) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(
    x = "3x3 Portfolio",
    y = "Average Number of Firms"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#7.2 is in factor construction R file because it uses 5x5 sort

df_OP_BM <- df_merged_factors_OP %>%
  left_join(df_merged_factors_3x3, by = c("Date", "Ticker")) %>%
  filter(
    grepl("Robust", OP_3x3_portfolio, ignore.case = TRUE),
    grepl("Low", BM_3x3_portfolio, ignore.case = TRUE)
  )

View(df_OP_BM)

df_merged_factors_OP %>%
  filter(grepl("Robust", OP_3x3_portfolio, ignore.case = TRUE)) %>%
  nrow()



###########figure 8

library(readxl)
final_graph <- read_excel("Desktop/Desktop - MacBook Air (276)/HSG St Gallen/semester 7/thesis/finalgraph.xlsx")
final_graph

install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(scales)

library(ggplot2)
library(grid)

remove.packages("gtable")
install.packages("gtable")

remove.packages("ggplot2")
install.packages("ggplot2")

library(ggplot2)
library(grid)

library(ggplot2)
library(grid)

final_graph %>%
  ggplot(aes(x = Date)) +
  geom_line(
    aes(y = `portfolio return`, linetype = "portfolio return"),
    linewidth = 0.6,
    color = "black"
  ) +
  geom_line(
    aes(y = `MKT return`, linetype = "MKT return"),
    linewidth = 0.6,
    color = "black"
  ) +
  scale_linetype_manual(
    values = c(
      "portfolio return" = "solid",
      "MKT return" = "dashed"
    )
  ) +
  labs(
    x = "Date",
    y = "Return",
    linetype = NULL
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.25,
      linetype = "dotted"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.78, 0.88),
    legend.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 0.25
    ),
    legend.text = element_text(size = 10)
  )


#try 2
library(ggplot2)
library(grid)

final_graph %>%
  ggplot(aes(x = Date)) +
  geom_line(
    aes(y = `portfolio return`, linetype = "portfolio return"),
    linewidth = 0.6,
    color = "black",
    na.rm = TRUE
  ) +
  geom_line(
    aes(y = `MKT return`, linetype = "MKT return"),
    linewidth = 0.6,
    color = "black",
    na.rm = TRUE
  ) +
  scale_linetype_manual(
    values = c(
      "portfolio return" = "solid",
      "MKT return" = "dashed"
    ),
    labels = c(
      "portfolio return" = "Portfolio",
      "MKT return" = "SIX Market"
    )
  ) +
  labs(
    x = "Date",
    y = "Return (CHF)",
    linetype = NULL
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.25,
      linetype = "dotted"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.78, 0.88),
    legend.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 0.25
    ),
    legend.key = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 10, color = "black")
  )

###try 3

####extra- PANEL A individual 27 regressions baseline model###################################

library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(showtext)

font_add("CMU Serif", regular = "cmunrm.ttf")
showtext_auto()

df <- read_delim(
  "portfolio;alpha;adj_r2
Big_High_excess;-0,001256688;0,8022996
Big_Low_excess;0,000390524;0,9479677
Big_Neutral_excess;-0,000689103;0,7755974
Big_Neutral_excess;0,002374772;0,7747088
Big_Robust_excess;-0,000135404;0,9385661
Big_Weak_excess;-0,002794607;0,8112745
Big_Aggressive_excess;-0,00086515;0,8372883
Big_Conservative_excess;0,001365858;0,7683406
Big_Neutral_excess;-0,000258011;0,8614047
Neutral_High_excess;0,002010567;0,7567888
Neutral_Low_excess;-0,002001598;0,7321382
Neutral_Neutral_excess;0,000688617;0,8246912
Neutral_Neutral_excess;0,001657403;0,8191054
Neutral_Robust_excess;0,000252719;0,7732866
Neutral_Weak_excess;-0,000683804;0,744865
Neutral_Aggressive_excess;0,000869085;0,7227508
Neutral_Conservative_excess;-0,001566246;0,7691514
Neutral_Neutral_excess;0,00133543;0,7842908
Small_High_excess;0,000234385;0,7080987
Small_Neutral_excess;-0,000338045;0,5748535
Small_Low_excess;-0,002602761;0,3416756
Small_Neutral_excess;0,003396725;0,6056151
Small_Robust_excess;-0,000498589;0,3119045
Small_Weak_excess;-0,002973658;0,6009668
Small_Conservative_excess;-0,002494019;0,590121
Small_Neutral_excess;5,86471E-05;0,5247024
Small_Aggressive_excess;-0,002290311;0,5124081",
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
) %>%
  mutate(
    portfolio_id = make.unique(portfolio),
    portfolio_label = str_replace_all(portfolio, "_excess", ""),
    portfolio_label = str_replace_all(portfolio_label, "_", " "),
    portfolio_id = factor(portfolio_id, levels = portfolio_id)
  )

journal_theme <- theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.35),
    axis.ticks = element_line(color = "black", linewidth = 0.35),
    axis.ticks.length = unit(-0.12, "cm"),
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_line(
      color = "grey80",
      linewidth = 0.25,
      linetype = "dotted"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_blank(),
    plot.margin = margin(8, 8, 8, 8)
  )

alpha_plot <- ggplot(df, aes(x = portfolio_id, y = alpha)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.35) +
  geom_col(
    fill = "white",
    color = "black",
    linewidth = 0.35,
    width = 0.65
  ) +
  scale_x_discrete(labels = df$portfolio_label) +
  scale_y_continuous(
    name = expression(alpha),
    labels = scales::label_number(accuracy = 0.001),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(x = NULL) +
  journal_theme

adj_r2_plot <- ggplot(df, aes(x = portfolio_id, y = adj_r2)) +
  geom_col(
    fill = "white",
    color = "black",
    linewidth = 0.35,
    width = 0.65
  ) +
  scale_x_discrete(labels = df$portfolio_label) +
  scale_y_continuous(
    name = expression("Adjusted " * R^2),
    labels = scales::label_number(accuracy = 0.01),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(x = NULL) +
  journal_theme

alpha_plot
adj_r2_plot

#####try 2
library(ggplot2)
library(dplyr)
library(readr)
install.packages("readr")
library(stringr)
library(ggrepel)
library(scales)

df <- read_delim(
  "portfolio;alpha;adj_r2;sort
Big_High_excess;-0,001256688;0,8022996;BM
Big_Low_excess;0,000390524;0,9479677;BM
Big_Neutral_excess;-0,000689103;0,7755974;BM
Big_Neutral_excess;0,002374772;0,7747088;OP
Big_Robust_excess;-0,000135404;0,9385661;OP
Big_Weak_excess;-0,002794607;0,8112745;OP
Big_Aggressive_excess;-0,00086515;0,8372883;INV
Big_Conservative_excess;0,001365858;0,7683406;INV
Big_Neutral_excess;-0,000258011;0,8614047;INV
Neutral_High_excess;0,002010567;0,7567888;BM
Neutral_Low_excess;-0,002001598;0,7321382;BM
Neutral_Neutral_excess;0,000688617;0,8246912;BM
Neutral_Neutral_excess;0,001657403;0,8191054;OP
Neutral_Robust_excess;0,000252719;0,7732866;OP
Neutral_Weak_excess;-0,000683804;0,744865;OP
Neutral_Aggressive_excess;0,000869085;0,7227508;INV
Neutral_Conservative_excess;-0,001566246;0,7691514;INV
Neutral_Neutral_excess;0,00133543;0,7842908;INV
Small_High_excess;0,000234385;0,7080987;BM
Small_Neutral_excess;-0,000338045;0,5748535;BM
Small_Low_excess;-0,002602761;0,3416756;BM
Small_Neutral_excess;0,003396725;0,6056151;OP
Small_Robust_excess;-0,000498589;0,3119045;OP
Small_Weak_excess;-0,002973658;0,6009668;OP
Small_Conservative_excess;-0,002494019;0,590121;INV
Small_Neutral_excess;5,86471E-05;0,5247024;INV
Small_Aggressive_excess;-0,002290311;0,5124081;INV",
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
) %>%
  mutate(
    size = str_extract(portfolio, "^(Big|Neutral|Small)"),
    raw_char = str_remove(portfolio, "^(Big|Neutral|Small)_"),
    raw_char = str_remove(raw_char, "_excess$"),
    characteristic = case_when(
      sort == "BM"  & raw_char == "High"         ~ "High BM",
      sort == "BM"  & raw_char == "Low"          ~ "Low BM",
      sort == "BM"  & raw_char == "Neutral"      ~ "Neutral BM",
      sort == "OP"  & raw_char == "Robust"       ~ "Robust OP",
      sort == "OP"  & raw_char == "Weak"         ~ "Weak OP",
      sort == "OP"  & raw_char == "Neutral"      ~ "Neutral OP",
      sort == "INV" & raw_char == "Aggressive"   ~ "Aggressive INV",
      sort == "INV" & raw_char == "Conservative" ~ "Conservative INV",
      sort == "INV" & raw_char == "Neutral"      ~ "Neutral INV",
      TRUE ~ raw_char
    ),
    label = paste(size, characteristic),
    adj_r2_pct = 100 * adj_r2
  )

mean_alpha <- mean(df$alpha)
mean_adj_r2_pct <- mean(df$adj_r2_pct)

journal_theme <- theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", color = "black"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.35),
    axis.ticks = element_line(color = "black", linewidth = 0.35),
    axis.ticks.length = unit(-0.12, "cm"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.25, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_blank(),
    plot.margin = margin(12, 38, 12, 12)
  )

alpha_r2_plot <- ggplot(df, aes(x = adj_r2_pct, y = alpha)) +
  geom_hline(yintercept = mean_alpha, color = "black", linewidth = 0.35, linetype = "dotted") +
  geom_vline(xintercept = mean_adj_r2_pct, color = "black", linewidth = 0.35, linetype = "dotted") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.30) +
  geom_point(shape = 1, size = 2.2, stroke = 0.35, color = "black") +
  geom_text_repel(
    aes(label = label),
    family = "Times New Roman",
    size = 3.2,
    color = "black",
    segment.color = "black",
    segment.linewidth = 0.20,
    box.padding = 0.45,
    point.padding = 0.30,
    force = 2.5,
    force_pull = 0.35,
    max.overlaps = Inf,
    min.segment.length = 0,
    seed = 123
  ) +
  scale_x_continuous(
    name = expression("Adjusted " * R^2 * " (%)"),
    labels = label_percent(scale = 1, accuracy = 1),
    limits = c(25, 100),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  scale_y_continuous(
    name = expression(alpha * " (%)"),
    labels = label_number(accuracy = 0.001),
    expand = expansion(mult = c(0.10, 0.10))
  ) +
  coord_cartesian(clip = "off") +
  journal_theme

alpha_r2_plot

###make prettier
alpha_r2_plot2 <- ggplot(df, aes(x = adj_r2_pct, y = alpha)) +
  geom_hline(yintercept = mean_alpha, color = "black", linewidth = 0.35, linetype = "dotted") +
  geom_vline(xintercept = mean_adj_r2_pct, color = "black", linewidth = 0.35, linetype = "dotted") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.30) +
  geom_point(
    shape = 1,
    size = 2.0,
    stroke = 0.35,
    color = "black"
  ) +
  geom_text_repel(
    aes(label = label),
    family = "Times New Roman",
    size = 2.8,
    color = "black",
    segment.color = "grey35",
    segment.linewidth = 0.18,
    segment.alpha = 0.70,
    box.padding = 0.55,
    point.padding = 0.35,
    force = 8,
    force_pull = 0.15,
    max.time = 2,
    max.iter = 10000,
    max.overlaps = Inf,
    min.segment.length = 0,
    seed = 123
  ) +
  scale_x_continuous(
    name = expression("Adjusted " * R^2 * " (%)"),
    labels = scales::label_percent(scale = 1, accuracy = 1),
    breaks = seq(30, 100, 10),
    limits = c(25, 105),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  scale_y_continuous(
    name = expression(alpha * " (%)"),
    labels = scales::label_number(accuracy = 0.001),
    breaks = seq(-0.003, 0.004, 0.001),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  coord_cartesian(clip = "off") +
  journal_theme +
  theme(
    aspect.ratio = 0.62,
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.margin = margin(12, 46, 12, 14)
  )

alpha_r2_plot2


############################try 3###################################################################################################################################################
# ---- Load libraries ----
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(ggrepel)
library(scales)

# ---- Load and prepare data ----
df <- read_delim(
  "portfolio;alpha;adj_r2;sort
Big_High_excess;-0,001256688;0,8022996;BM
Big_Low_excess;0,000390524;0,9479677;BM
Big_Neutral_excess;-0,000689103;0,7755974;BM
Big_Neutral_excess;0,002374772;0,7747088;OP
Big_Robust_excess;-0,000135404;0,9385661;OP
Big_Weak_excess;-0,002794607;0,8112745;OP
Big_Aggressive_excess;-0,00086515;0,8372883;INV
Big_Conservative_excess;0,001365858;0,7683406;INV
Big_Neutral_excess;-0,000258011;0,8614047;INV
Neutral_High_excess;0,002010567;0,7567888;BM
Neutral_Low_excess;-0,002001598;0,7321382;BM
Neutral_Neutral_excess;0,000688617;0,8246912;BM
Neutral_Neutral_excess;0,001657403;0,8191054;OP
Neutral_Robust_excess;0,000252719;0,7732866;OP
Neutral_Weak_excess;-0,000683804;0,744865;OP
Neutral_Aggressive_excess;0,000869085;0,7227508;INV
Neutral_Conservative_excess;-0,001566246;0,7691514;INV
Neutral_Neutral_excess;0,00133543;0,7842908;INV
Small_High_excess;0,000234385;0,7080987;BM
Small_Neutral_excess;-0,000338045;0,5748535;BM
Small_Low_excess;-0,002602761;0,3416756;BM
Small_Neutral_excess;0,003396725;0,6056151;OP
Small_Robust_excess;-0,000498589;0,3119045;OP
Small_Weak_excess;-0,002973658;0,6009668;OP
Small_Conservative_excess;-0,002494019;0,590121;INV
Small_Neutral_excess;5,86471E-05;0,5247024;INV
Small_Aggressive_excess;-0,002290311;0,5124081;INV",
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
) %>%
  mutate(
    size = str_extract(portfolio, "^(Big|Neutral|Small)"),
    raw_char = str_remove(portfolio, "^(Big|Neutral|Small)_"),
    raw_char = str_remove(raw_char, "_excess$"),
    characteristic = case_when(
      sort == "BM"  & raw_char == "High"         ~ "High BM",
      sort == "BM"  & raw_char == "Low"          ~ "Low BM",
      sort == "BM"  & raw_char == "Neutral"      ~ "Neutral BM",
      sort == "OP"  & raw_char == "Robust"       ~ "Robust OP",
      sort == "OP"  & raw_char == "Weak"         ~ "Weak OP",
      sort == "OP"  & raw_char == "Neutral"      ~ "Neutral OP",
      sort == "INV" & raw_char == "Aggressive"   ~ "Aggressive INV",
      sort == "INV" & raw_char == "Conservative" ~ "Conservative INV",
      sort == "INV" & raw_char == "Neutral"      ~ "Neutral INV",
      TRUE ~ raw_char
    ),
    label       = paste(size, characteristic),
    adj_r2_pct  = 100 * adj_r2,
    alpha_pct   = 100 * alpha   # <-- KEY FIX: convert α from decimal to %
  )

# ---- Reference lines: cross-portfolio averages ----
mean_alpha_pct  <- mean(df$alpha_pct)
mean_adj_r2_pct <- mean(df$adj_r2_pct)

# ---- Journal theme ----
journal_theme <- theme_classic(base_family = "Times New Roman", base_size = 14) +
  theme(
    text              = element_text(family = "Times New Roman", color = "black"),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    panel.border      = element_blank(),
    axis.line         = element_line(color = "black", linewidth = 0.35),
    axis.ticks        = element_line(color = "black", linewidth = 0.35),
    axis.ticks.length = unit(-0.12, "cm"),
    axis.text         = element_text(size = 14, color = "black"),
    axis.title        = element_text(size = 14, color = "black"),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.25, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none",
    plot.title         = element_blank(),
    plot.margin        = margin(12, 46, 12, 14)
  )

# ---- Plot ----
alpha_r2_plot <- ggplot(df, aes(x = adj_r2_pct, y = alpha_pct)) +
  geom_hline(yintercept = mean_alpha_pct,  color = "black", linewidth = 0.35, linetype = "dotted") +
  geom_vline(xintercept = mean_adj_r2_pct, color = "black", linewidth = 0.35, linetype = "dotted") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.30) +
  geom_point(
    shape  = 1,
    size   = 2.0,
    stroke = 0.35,
    color  = "black"
  ) +
  geom_text_repel(
    aes(label = label),
    family             = "Times New Roman",
    size               = 3.2,
    color              = "black",
    segment.color      = "grey35",
    segment.linewidth  = 0.18,
    segment.alpha      = 0.70,
    box.padding        = 0.55,
    point.padding      = 0.35,
    force              = 8,
    force_pull         = 0.15,
    max.time           = 2,
    max.iter           = 10000,
    max.overlaps       = Inf,
    min.segment.length = 0,
    seed               = 123
  ) +
  scale_x_continuous(
    name   = expression("Adjusted " * R^2 * " (%)"),
    labels = label_number(accuracy = 1),
    breaks = seq(30, 100, 10),
    limits = c(25, 105),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  scale_y_continuous(
    name   = expression(alpha * " (%)"),
    labels = label_number(accuracy = 0.01),
    breaks = seq(-0.30, 0.40, 0.10),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  coord_cartesian(clip = "off") +
  journal_theme +
  theme(aspect.ratio = 0.62)

alpha_r2_plot

####correlation matrix

library(ggplot2)
library(dplyr)
library(tidyr)

# Construct the correlation data frame
corr_data <- read.table(text = "
Pair Baseline IR_H IR_L S_H S_L SH_Pos SH_Neg
MKT-SMB -0.346 -0.406 -0.280 -0.401 -0.311 -0.287 -0.412
MKT-HML 0.084 0.138 0.022 -0.023 0.159 0.147 0.003
MKT-RMW -0.170 -0.360 0.102 0.037 -0.058 -0.153 -0.189
MKT-CMA 0.114 0.183 0.024 -0.030 0.126 0.009 0.231
SMB-HML -0.060 -0.089 -0.025 -0.111 -0.119 0.005 -0.145
SMB-RMW 0.083 0.192 -0.063 -0.022 0.081 0.011 0.146
SMB-CMA 0.065 -0.034 0.170 0.055 0.123 0.195 -0.064
HML-RMW -0.322 -0.441 -0.168 -0.383 -0.257 -0.277 -0.377
HML-CMA 0.009 0.035 -0.018 0.082 0.035 -0.042 0.063
RMW-CMA -0.209 -0.312 -0.057 -0.119 -0.069 -0.060 -0.342
", header = TRUE, check.names = FALSE)

# Pivot to long format
corr_long <- corr_data %>%
  pivot_longer(
    cols = -Pair,
    names_to = "Regime",
    values_to = "Correlation"
  ) %>%
  mutate(
    Pair = factor(Pair, levels = corr_data$Pair),
    Regime = factor(
      Regime,
      levels = c("Baseline", "IR_H", "IR_L", "S_H", "S_L", "SH_Pos", "SH_Neg")
    )
  )

# Plot
p <- ggplot(corr_long, aes(x = Regime, y = Pair, fill = Correlation)) +
  geom_tile(color = "black", linewidth = 0.3) +
  geom_text(
    aes(label = sprintf("%.3f", Correlation)),
    family = "Times New Roman",
    size = 4,
    color = "black"
  ) +
  scale_fill_gradient2(
    low      = "#4575b4",   # blue (negative)
    mid      = "white",     # white (zero)
    high     = "#d73027",   # red (positive)
    midpoint = 0,
    limits   = c(-0.5, 0.5),
    breaks   = c(-0.4, -0.2, 0, 0.2, 0.4),
    name     = "Correlation"
  ) +
  scale_y_discrete(limits = rev) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    text             = element_text(color = "black"),
    axis.text        = element_text(size = 14, color = "black"),
    axis.title       = element_text(size = 14, color = "black"),
    axis.line        = element_blank(),
    axis.ticks       = element_blank(),
    legend.title     = element_text(size = 12, color = "black"),
    legend.text      = element_text(size = 11, color = "black"),
    legend.position  = "right",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid       = element_blank()
  )

p

#try 2
library(ggplot2)
library(dplyr)
library(tidyr)

# Construct the correlation data frame
corr_data <- read.table(text = "
Pair Baseline IR_H IR_L S_H S_L SH_Pos SH_Neg
MKT-SMB -0.346 -0.406 -0.280 -0.401 -0.311 -0.287 -0.412
MKT-HML 0.084 0.138 0.022 -0.023 0.159 0.147 0.003
MKT-RMW -0.170 -0.360 0.102 0.037 -0.058 -0.153 -0.189
MKT-CMA 0.114 0.183 0.024 -0.030 0.126 0.009 0.231
SMB-HML -0.060 -0.089 -0.025 -0.111 -0.119 0.005 -0.145
SMB-RMW 0.083 0.192 -0.063 -0.022 0.081 0.011 0.146
SMB-CMA 0.065 -0.034 0.170 0.055 0.123 0.195 -0.064
HML-RMW -0.322 -0.441 -0.168 -0.383 -0.257 -0.277 -0.377
HML-CMA 0.009 0.035 -0.018 0.082 0.035 -0.042 0.063
RMW-CMA -0.209 -0.312 -0.057 -0.119 -0.069 -0.060 -0.342
", header = TRUE, check.names = FALSE)

# Pivot to long format and add absolute correlation for shading
corr_long <- corr_data %>%
  pivot_longer(
    cols = -Pair,
    names_to = "Regime",
    values_to = "Correlation"
  ) %>%
  mutate(
    Abs_Correlation = abs(Correlation),
    Pair = factor(Pair, levels = corr_data$Pair),
    Regime = factor(
      Regime,
      levels = c("Baseline", "IR_H", "IR_L", "S_H", "S_L", "SH_Pos", "SH_Neg")
    ),
    text_color = ifelse(Abs_Correlation > 0.25, "white", "black")
  )

# Plot
q <- ggplot(corr_long, aes(x = Regime, y = Pair, fill = Abs_Correlation)) +
  geom_tile(color = "black", linewidth = 0.3) +
  geom_text(
    aes(label = sprintf("%.3f", Correlation), color = text_color),
    family = "Times New Roman",
    size = 4
  ) +
  scale_fill_gradient(
    low    = "white",
    high   = "black",
    limits = c(0, 0.5),
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
    name   = "|Correlation|"
  ) +
  scale_color_identity() +
  scale_y_discrete(limits = rev) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    text             = element_text(color = "black"),
    axis.text        = element_text(size = 14, color = "black"),
    axis.title       = element_text(size = 14, color = "black"),
    axis.line        = element_blank(),
    axis.ticks       = element_blank(),
    legend.title     = element_text(size = 12, color = "black"),
    legend.text      = element_text(size = 11, color = "black"),
    legend.position  = "right",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid       = element_blank()
  )

q






