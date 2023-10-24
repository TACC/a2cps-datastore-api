FROM rocker/tidyverse:latest
ARG UID
ARG GID

RUN apt update && \
    apt install -y sudo && \
    addgroup --gid $GID appuser && \
    adduser --uid $UID --gid $GID --disabled-password --gecos "" appuser && \
    echo 'appuser ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers


RUN apt-get update &&\
   apt-get install build-essential libcurl4-gnutls-dev libssl-dev libxml2-dev libgit2-dev libxt-dev libcairo2-dev libssh2-1-dev -y &&\
   mkdir -p /var/lib/shiny-server/bookmarks/shiny

RUN R -e "install.packages('devtools')"

RUN R -e "install.packages(c('shiny', 'shinydashboard','DT','dplyr','ggplot2','gridExtra','shinythemes','parsedate','remotes', 'tidyverse','httr','jsonlite','scales','forcats','ggplot2 ','DT','plotly','ggthemses','Hmisc','kableExtra','here','hablar','lubridate','janitor','UpSetR'), repos='http://cran.rstudio.com/')"

RUN R -e "install.packages(c('ggthemes'), repos='http://cran.rstudio.com/')"

# Set the app user as the default user
RUN chmod -R 777 /usr/local/lib/R/etc
COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site
RUN chmod -R 777 /usr/local/lib/R/etc/Rprofile.site
RUN chown -R appuser:appuser /usr/local/lib/R

USER appuser
WORKDIR /home/appuser/app

COPY --chown=appuser:appuser src /home/appuser/app
RUN chmod -R 755 /home/appuser/app
RUN echo "local(options(shiny.port = 8050, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

EXPOSE 8050
CMD ["R", "-e", "shiny::runApp('/home/appuser/app')"]