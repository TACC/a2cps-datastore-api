FROM rocker/tidyverse:latest

RUN apt-get update &&\
   apt-get install build-essential libcurl4-gnutls-dev libssl-dev libxml2-dev libgit2-dev libxt-dev libcairo2-dev libssh2-1-dev -y &&\
   mkdir -p /var/lib/shiny-server/bookmarks/shiny

RUN R -e "install.packages('devtools')"

RUN R -e "install.packages(c('shiny', 'shinydashboard','DT','dplyr','ggplot2','gridExtra','shinythemes','parsedate','remotes', 'tidyverse','httr','jsonlite','scales','forcats','ggplot2 ','DT','plotly','ggthemses','Hmisc','kableExtra','here','hablar','lubridate','janitor','UpSetR'), repos='http://cran.rstudio.com/')"

RUN R -e "install.packages(c('ggthemes'), repos='http://cran.rstudio.com/')"
COPY src /root/app
COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site
RUN chmod -R 755 /root/app
RUN chmod -R 755 /usr/local/lib/R/etc

EXPOSE 8050
CMD ["R", "-e", "shiny::runApp('/root/app')"]