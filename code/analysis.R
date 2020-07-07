#################################################################
##               Analysis code for Obaidi et al                ##
#################################################################

#### load packages ####
xfun::pkg_attach2(
  "here",
  "tidyverse",
  "foreign",
  "magrittr",
  "metafor",
  "psych",
  "remotes"
)

#### define helper functions ####

remotes::install_github("ljcolling/ODPhelper", upgrade = "never")
library(ODPHelper)

rd <- purrr::partial(sprintf, fmt = "%.2f")
rp2 <- function(.x) {
  if_else(.x < 0.01, "< .01",
    stringr::str_replace(
      string = rd(.x),
      pattern = "0.",
      replacement = "."
    )
  )
}

tidy_cor <- function(x) x %>% broom::tidy() %>% glue::glue_data("{rp2(estimate)}{if_else(p.value < 0.05,'*','')}")

#### define env to hold output #####

obaidi <- new.env()

#### replicate the analysis ####

# list the data files

list.files(path = here::here("data"), pattern = "*.sav", full.names = T) %>%
  map(function(x)
    foreign::read.spss(x,
                       use.missings = T,
                       use.value.labels = T) %>%
      as_tibble() %>%
      magrittr::set_colnames(tolower(names(.)))
  ) -> dats

# fix var name
dats[[6]] %>% rename(native = birth_place1pak2out) -> dats[[6]]

dats <- map(dats, ~mutate(., native = as.numeric(native)))

#### generate table 1 ####

y_vars <- rep("native", 5)
x_vars <- c("grd", "musid", "injust", "anger", "viointen")

d = list()
for(i in 1:6){
  d = c(d,rep(list(dats[[i]]),5))
}

pmap_chr(list(dat = d, x_var = rep(x_vars,6), y_var = rep(y_vars,6)),
          function(dat, x_var, y_var)
            cor.test(as.numeric(dat[[x_var]]),as.numeric(dat[[y_var]])) %>% tidy_cor()) %>%
  matrix(nrow = 5) -> table1_1

cors = list()
for(d in 1 : 6){
  for(i in 1 : length(x_vars)){
    cors = c(cors,cor.test(as.numeric(dats[[d]][[x_vars[i]]]),as.numeric(dats[[d]][[y_vars[i]]])) %>% tidy_cor())

  }
}


y_vars <- rep("grd", 4)
x_vars <- c("musid", "injust", "anger", "viointen")

d = list()
for(i in 1:6){
  d = c(d,rep(list(dats[[i]]),4))
}

pmap_chr(list(dat = d, x_var = rep(x_vars,6), y_var = rep(y_vars,6)),
         function(dat, x_var, y_var)
           cor.test(as.numeric(dat[[x_var]]),as.numeric(dat[[y_var]])) %>% tidy_cor()) %>%
  matrix(nrow = 4) -> table1_2

#### Generate table 2 values

do_mediation <- function(X, Y, M){
  map(1:6, function(x) {
  set.seed(75)
  psych::mediate(x = X, y = Y, m = M, dats[[x]] %>% select(all_of(c(X,Y,M))) %>% drop_na(), n.iter = 5000, plot = FALSE) %>% .[["boot"]]}) %>%
    map_df(
      function(x)
        x %>%
          .[c("mean", "sd", "ci")] %>%
          unlist() %>%
          .[c("mean1", "sd1", "ci1", "ci2")] %>%
          as.list() %>%
          as_tibble() %>%
          set_colnames(c("mean", "sd", "ll", "ul")))
}


list(do_mediation(X = "native",Y = "musid", M = "grd"),
do_mediation(X = "native", Y = "injust", M = "grd"),
do_mediation(X = "native", Y = "anger" , M = "grd"),
do_mediation(X = "native", Y = "viointen", M = "grd")) -> table2_data

table2_data %>%
  map_df(function(x)
    metafor::rma(yi = x$mean, sei = x$sd, method = "DL") %>%
      .[c("b", "se", "zval", "ci.lb", "ci.ub", "pval")] %>%
      as_tibble() %>% mutate(b = as.numeric(b))
  ) -> table_3


table1_1 %>% as_tibble(.name_repair = make.names) %>% set_colnames(glue::glue("Study {1:6}")) %>% add_column(Variable = c("Group-based relative depravation","Muslim identification","Perceived injustice","Group-based anger","Violent behavioral intentions"), .before = 1) -> table1_1_text

table1_2 %>% as_tibble(.name_repair = make.names) %>% set_colnames(glue::glue("Study {1:6}")) %>% add_column(Variable = c("Muslim identification","Perceived injustice","Group-based anger","Violent behavioral intentions"), .before = 1) -> table1_2_text

rbind(table1_1_text,table1_2_text) -> obaidi$table_1

names = c("Muslim\nidentification","Perceived\n injustice","Group-based\n anger","Violent\nbehavioral\n intentions")

pmap_df(list(name = names,x = table2_data),
        function(name,x)
  x %>% glue::glue_data("{round(mean,2)} ({round(sd,2)})\nL{round(ll,2)}, {round(ul,2)}R") %>% as_tibble() %>% add_column(Study = 1:6, .before = 1) %>% add_column(Variable = name)) %>%
  pivot_wider(id_cols = "Study", names_from = "Variable" ) %>% arrange(Study) -> obaidi$table_2


table_3 %>% add_column(var = names, .before = 1)  %>% mutate(ci = glue::glue("L{round(ci.lb,2)}, {round(ci.ub,2)}R")) %>%
  mutate(pval = rp2(pval)) %>%
  select(var,b,se,ci,zval,pval) %>% set_colnames(c("Dependent variable","Weighted\nmean\neffect","$SE$","95% CI","$z$","$p$")) -> obaidi$table_3


# Write out session and package information
obaidi$session_info <- desc_session()
saveRDS(obaidi, here::here("./made/obaidi.Rdata"))
