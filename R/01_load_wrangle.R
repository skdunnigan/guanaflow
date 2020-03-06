# ----01 read in data files----

# read in all files in this folder beginning with guanaflow
files1 <- list.files(here::here('data', '2020', 'limited.csv'),
                     pattern = "guanaflow*", full.names = TRUE)

# convert that list into a dataframe
# merge all the data together by rows and clean up the column names
flow_dat <- sapply(files1, readr::read_csv, simplify = FALSE) %>%
  dplyr::bind_rows(.id = "id") %>%
  janitor::clean_names()


# ----02 inspect data frame ----

head(flow_dat)
tail(flow_dat)
str(flow_dat)
names(flow_dat)
dplyr::glimpse(flow_dat)

# ----03 adjust date and time----
flow_dat <- flow_dat %>%
  dplyr::mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"),
                date = lubridate::date(datetime),
                year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate::day(date),
                hour = lubridate::hour(datetime))

flow_dat %>%
  select(-id, -contains("depth"), -contains("flow")) %>%
  tidyr::pivot_longer(cols = 2:4, names_to = "culvert", values_to = "velocity") %>%
  tidyr::separate(culvert, into = c("name", "culvert"),
                  sep = "_") %>%
  select(-name) %>%
  group_by(date, hour, culvert) %>%
  summarise(velocity_mean = -1 * mean(velocity, na.rm = TRUE)) %>%
  mutate(date_hour = date + hour) %>%
  ggplot() +
  geom_col(aes(x = date_hour, y = velocity_mean, fill = culvert),
           position = "dodge") +
  geom_hline(yintercept = 0) +
  theme_cowplot()
  