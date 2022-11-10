if(!require(devtools)) install.packages('devtools')
library(tidyverse)
library(lubridate)
library(sp)
library(sf)
library(ggplot2)
library(ggspatial)
library(pander)
library(xts)
library(purrr)
library(furrr)

my_cols<-cols(
  deployid = col_character(),
  x = col_double(),
   y = col_double(),
  date_time = col_datetime("%Y-%m-%d %H:%M:%S")
)



tbl_locs <- file.path('Your directory', "....csv") %>%
  purrr::map(read_csv,col_types = my_cols) %>% 
  dplyr::bind_rows()


sf_locs <- sf::st_as_sf(tbl_locs, coords = c("x","y")) %>% 
  sf::st_set_crs('+proj=utm +zone=12 ellps=WGS84')

sf_lines <- sf_locs %>% 
  dplyr::arrange(deployid, date_time) %>% 
  sf::st_geometry() %>% 
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs$deployid))) %>% 
  sf::st_cast("MULTILINESTRING") %>% 
  sf::st_sf(deployid = as.factor(unique(sf_locs$deployid)))

ggplot() + 
  
  layer_spatial(sf_lines, size = 0.75,aes(color = deployid)) +
  scale_x_continuous(expand = expand_scale(mult = c(.6, .6))) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")

tbl_locs %>% dplyr::group_by(deployid) %>% dplyr::select(deployid,date_time) %>% 
  dplyr::do(head(.,1))


make_unique <- function(x) {
  xts::make.time.unique(x$date_time,eps = 10)
}





tbl_locs <- tbl_locs %>% 
  dplyr::arrange(deployid,date_time) %>% 
  dplyr::group_by(deployid) %>% tidyr::nest() %>% 
  dplyr::mutate(unique_time = purrr::map(data, make_unique)) %>% 
  tidyr::unnest() %>% 
  dplyr::select(-date_time) %>% rename(date_time = unique_time)





future::plan(multiprocess)


tbl_locs <- tbl_locs %>% 
  dplyr::arrange(deployid, date_time) %>% 
  dplyr::group_by(deployid) %>% 
  tidyr::nest() 


tbl_locs %>% dplyr::summarise(n = n())


tbl_locs <- tbl_locs %>% 
  tidyr::unnest()  %>% 
  dplyr::arrange(deployid,date_time)


sf_locs <- sf::st_as_sf(tbl_locs, coords = c("x","y")) %>% 
  sf::st_set_crs('+proj=utm +zone=12 ellps=WGS84')

#sf_locs <- sf::st_transform(sf_locs, "+proj=utm +zone=12 ellps=WGS84")


sf::st_geometry(sf_locs)

future::plan(multisession)
sf_locs <- sf_locs %>% 
  dplyr::group_by(deployid) %>% dplyr::arrange(date_time) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(data = furrr::future_map(data,sf::st_as_sf))

sf_locs <- sf_locs %>% 
  dplyr::mutate(
    fixpar = rep(
      list(c(NA,NA)),
      nrow(.)
    )
  )


fit_crawl <- function(d, fixpar) {
  
  prior <- function(p) {
    dnorm(p[2], -4, 2, log = TRUE)
  } 
  
  fit <- crawl::crwMLE(
    mov.model =  ~ 1,
    err.model = NULL,
    if (any(colnames(d) == "activity")) {
      activity <- ~ I(activity)
    } else {activity <- NULL},
    fixPar = fixpar,
    data = d,
    method = "Nelder-Mead",
    Time.name = "date_time",
    prior = prior,
    attempts = 8,
    control = list(
      trace = 0
    ),
    initialSANN = list(
      maxit = 1500,
      trace = 0
    )
  )
  fit
}



tbl_locs_fit <- sf_locs %>% 
  dplyr::mutate(fit = furrr::future_pmap(list(d = data,fixpar = fixpar),
                                         fit_crawl),
                params = map(fit, crawl::tidy_crwFit))

panderOptions('knitr.auto.asis', FALSE)
tbl_locs_fit$params %>% 
  walk(pander::pander,caption = "crwMLE fit parameters")
a<-tbl_locs_fit$params[[1]]

if(a$estimate[4]>0){


tbl_locs_fit <- tbl_locs_fit %>% 
  dplyr::mutate(predict = furrr::future_map(fit,
                                            crawl::crwPredict,
                                            predTime = '1 min')) 

tbl_locs_fit <- tbl_locs_fit %>% 
  dplyr::mutate(sf_points = purrr::map(predict,
                                       crawl::crw_as_sf,
                                       ftype = "POINT",
                                       locType = "p"),
                sf_line = purrr::map(predict,
                                     crawl::crw_as_sf,
                                     ftype = "LINESTRING",
                                     locType = "p")
  )

sf_pred_lines <- do.call(rbind,tbl_locs_fit$sf_line) %>% 
  mutate(id = tbl_locs_fit$deployid) 


sf_pred_points <- tbl_locs_fit %>% tidyr::unnest(sf_points) %>% 
  mutate(geometry = sf::st_sfc(geometry)) %>% 
  sf::st_as_sf() %>% sf::st_set_crs('+proj=utm +zone=12 ellps=WGS84')



  dir.create(paste0(" your output directory",i))
  print(i)

  
  save(sf_pred_points,file = paste0(".....RData"))
}

