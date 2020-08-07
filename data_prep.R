library(tidyr)
library(dplyr)
library(stringi)
library(lubridate)
library(fpp2)

setwd('C:/Users/rknit/Documents/RProjects/COVID-Shiny-Report/')

# Pull Census population estimates and create FIPS key

#keep_cols <- c('STATE','COUNTY','STNAME','CTYNAME','POPESTIMATE2019')
#df_pop <- read.csv('https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv')[,keep_cols]
#write.csv(df_pop, 'data/census_pop_est.csv', row.names=FALSE)

df_pop <- read.csv('data/census_pop_est.csv')

df_pop <- df_pop %>% 
  mutate(STATE = ifelse(nchar(STATE)==1, paste("0",STATE,sep=""), STATE),
         COUNTY = ifelse(nchar(COUNTY)==1, paste("00",COUNTY,sep=""),
                         ifelse(nchar(COUNTY)==2, paste("0",COUNTY,sep=""),
                                COUNTY)),
         FIPS = paste(STATE,COUNTY,sep=""))


# Pull daily confirmed cases by county and prep dataset
# - Update FIPS column to pop_df format
# - Pivot the dataset to long form
# - Calc new variables for risk assessment
# - Add forecast for every county
# - Join with population data

df_county <- read.csv('https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv?raw=true')

df_county <- df_county %>%
  select(FIPS, Province_State, Lat, Long_, Combined_Key, starts_with('X')) %>%
  mutate(FIPS = ifelse(nchar(FIPS) < 5, 
                       stri_pad(FIPS,width=5,side='left',pad='0'),
                       FIPS)) %>%
  pivot_longer(c('FIPS','Province_State'),cols=starts_with('X'), names_to='rpt_dt0', values_to='confirmed') %>%
  mutate(rpt_dt = mdy(substr(rpt_dt0,2,nchar(rpt_dt0)))) %>%
  arrange(FIPS, rpt_dt) %>%
  mutate(new_confirmed = ifelse(FIPS == lag(FIPS,n=1) & !is.na(lag(FIPS,n=1)),
                                confirmed - lag(confirmed,n=1,default=0),
                                0)) %>%
  select(-confirmed,-rpt_dt0) %>%
  inner_join(df_pop, by='FIPS')

lst_rpt_dt <- max(df_county$rpt_dt)

f_range <- data.frame(rpt_dt = seq(from=lst_rpt_dt+1, 
                                   to=lst_rpt_dt+20, 
                                   by=1))

# Forecast every county out 20 days to be used to calculate active cases
for (cnty in levels(df_county$Combined_Key)){

  indiv_county <- df_county %>%
    filter(Combined_Key == cnty)
  
  if (nrow(indiv_county) == 0){next}
  
  ts_new <- ts(indiv_county$new_confirmed)
  
  m <- ets(ts_new)
  
  f <- forecast(m, h = 20)$mean
  
  f_df <- data.frame(rpt_dt = seq(from=max(df_county$rpt_dt)+1, 
                                  to=max(df_county$rpt_dt)+20, 
                                  by=1),
                     new_confirmed = f)
  
  f_df <- f_df %>%
    left_join(distinct(select(indiv_county,-rpt_dt,-new_confirmed,)), 
              by = character())
  
  tryCatch({f_df_all <- union(f_df_all, f_df)},
           error = function(err) {f_df_all <<- f_df})
}

df_county <- union(df_county,f_df_all) %>%
  arrange(FIPS, rpt_dt)

# Create new cummulative column
df_county <- df_county %>%
  group_by(FIPS) %>%
  mutate(cum_confirmed = cumsum(new_confirmed)) %>%
  ungroup() %>%
  mutate(forecast = (rpt_dt > lst_rpt_dt))


# Pull daily COVID data at state level
# - Check the most recent date in the dataset
# - Try to pull the next day if available
# - Append the new data and save it

df_state <- read.csv('data/COVID_state.csv')

df_state$rpt_dt <- as.Date(df_state$rpt_dt)

rpt_dt <- max(as.Date(df_state$rpt_dt)) + 1

while (rpt_dt < today()){
  tryCatch({
    df_state0 <- read.csv(paste('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports_us/',
                                stri_pad(as.character(month(rpt_dt)),width=2,side='left',pad='0'),'-',
                                stri_pad(as.character(day(rpt_dt)),width=2,side='left',pad='0'),'-',
                                year(rpt_dt),
                                '.csv', sep=''))
    df_state0 <- mutate(df_state0, rpt_dt = rpt_dt)
    
    df_state <- union(df_state, df_state0)
    write.csv(df_state, 'data/COVID_state.csv', row.names=FALSE)
    print(paste('Data pulled for',rpt_dt))
    }, 
    error = function(err) {print(err)},
    warning = function(war) {print(war)}
  )
  
  rpt_dt <- max(as.Date(df_state$rpt_dt)) + 1
}

df_pop2 <- df_pop %>%
  filter(COUNTY == '000') %>%
  select(STNAME,POPESTIMATE2019)

df_state_ <- df_state %>%
  left_join(df_pop2, by=c('Province_State'='STNAME')) %>%
  arrange(Province_State,rpt_dt) %>%
  mutate(tested_7d = People_Tested - ifelse(Province_State == lag(Province_State,n=7) & !is.na(lag(Province_State,n=7)),
                             lag(People_Tested,n=7,default=0),
                             0),
         confirmed_7d = Confirmed - ifelse(Province_State == lag(Province_State,n=7) & !is.na(lag(Province_State,n=7)),
                               lag(Confirmed,n=7,default=0),
                               0),
         positive_rate = confirmed_7d/tested_7d,
         tested_rate_per_100k = tested_7d/POPESTIMATE2019*100000)

# Add State data to County data

df <- df_state %>%
  select(Province_State, People_Tested, Testing_Rate, Confirmed, Active, Incident_Rate, rpt_dt) %>%
  right_join(df_county, by = c('Province_State','rpt_dt')) %>%
  arrange(FIPS, rpt_dt) %>%
  mutate(est_active_per_100k = (ifelse(lead(Combined_Key,n=8) == Combined_Key,
                                       lead(cum_confirmed,n=8), NaN) -
                                    coalesce(ifelse(lag(Combined_Key,n=10) == Combined_Key,
                                                    lag(cum_confirmed,n=10,default=0),0),
                                             0))/POPESTIMATE2019*100000/0.6,
         est_active_chg_7d = est_active_per_100k - 
                              coalesce(ifelse(lag(Combined_Key,n=7) == Combined_Key,
                                     lag(est_active_per_100k,n=7,default=0),0),0)) %>%
  select(rpt_dt, Combined_Key, Province_State, Lat, Long_, POPESTIMATE2019, 
         cum_confirmed, est_active_per_100k, est_active_chg_7d, forecast)

write.csv(df, 'data/COVID_data.csv', row.names=FALSE)
