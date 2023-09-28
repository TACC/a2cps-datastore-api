FROM rocker/r-base:latest
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*
RUN install.r \
    shiny \
    httr \
    jsonlite \
    scales \
    forcats \
    ggplot2 \
    DT \
    shinythemes \
    plotly \
    ggthemses \
    Hmisc \
    kableExtra \
    here \
    hablar \
    lubridate \
    janitor \
    UpSetR
RUN echo "local(options(shiny.port = 8050, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/app
RUN chown app:app -R /home/app
ADD src /home/app/
USER app
EXPOSE 8050
CMD ["R", "-e", "shiny::runApp('/home/app/src')"]