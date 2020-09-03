FROM rocker/r-ver:4.0.2
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libpq-dev libssl-dev libxml2-dev pandoc pandoc-citeproc zlib1g-dev pkg-config && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "1.5")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "0.4.7")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.7.0")'
RUN Rscript -e 'remotes::install_version("foreach",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("here",upgrade="never", version = "0.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("roxygen2",upgrade="never", version = "7.1.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.7.9")'
RUN Rscript -e 'remotes::install_version("rotor",upgrade="never", version = "0.2.4")'
RUN Rscript -e 'remotes::install_version("lgr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("doParallel",upgrade="never", version = "1.0.15")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("rapportools",upgrade="never", version = "1.0")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.13.0")'
RUN Rscript -e 'remotes::install_version("devtools",upgrade="never", version = "2.3.1")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("ipify",upgrade="never", version = "0.2.0")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("RSocrata",upgrade="never", version = "1.7.10-6")'
RUN mkdir /build_zone
RUN mkdir /build_zone/logs
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
# RUN rm -rf /build_zone
# EXPOSE 80
# CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');tripgen::run_app()"
