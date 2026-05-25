FROM rocker/shiny-verse:4.2.2

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libglpk-dev \
    libuv1-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install LaTeX for PDF certificate generation
RUN apt-get update && apt-get install -y \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    texlive-xetex \
    && rm -rf /var/lib/apt/lists/*

#Install Packages
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinyBS", \
              "bs4Dash", \
              "fresh", \
              "DT", \
              "pool", \
              "dplyr", \
              "shinyjs", \
              "glue", \
              "spsComps", \
              "rutifier", \
              "shinyvalidate", \
              "igraph", \
              "readxl", \
              "janitor", \
              "highcharter", \
              "tidyr", \
              "stringr", \
              "shinyalert", \
              "shinydisconnect", \
              "sendmailR", \
              "DBI", \
              "remotes", \
              "rmarkdown", \
              "kableExtra", \
              "knitr", \
              "config" \
            ), \
            repos="http://cran.rstudio.com", \
            dependencies = TRUE \
          )'
RUN R -e 'install.packages("devtools")'
RUN R -e 'install.packages("shinycssloaders", repos="http://cran.rstudio.com", dependencies = TRUE)'
RUN R -e 'install.packages("lubridate", repos="http://cran.rstudio.com", dependencies = TRUE)'
RUN R -e 'install.packages("openxlsx", repos="http://cran.rstudio.com", dependencies = TRUE)'
RUN R -e 'install.packages("bslib", repos="http://cran.rstudio.com", dependencies = TRUE)'
RUN R -e 'install.packages("rvest", repos="http://cran.rstudio.com", dependencies = TRUE)'
RUN R -e 'install.packages("qrcode", repos="http://cran.rstudio.com", dependencies = TRUE)'
RUN R -e 'install.packages("RMySQL", repos="http://cran.rstudio.com", dependencies = TRUE)'
RUN R -e 'install.packages("base64enc", repos="http://cran.rstudio.com", dependencies = TRUE)'
RUN R -e 'install.packages("dotenv", repos="http://cran.rstudio.com")'
RUN R -e 'install.packages("fs", repos="http://cran.rstudio.com", dependencies = TRUE)'
RUN R -e 'install.packages("gt", repos="http://cran.rstudio.com")'
RUN R -e 'install.packages("dbplyr", repos="http://cran.rstudio.com")'
RUN R -e 'install.packages("waiter", repos="http://cran.rstudio.com")'
RUN R -e 'require(devtools)'

RUN R -e 'devtools::install_version("auth0", version = "0.2.1", dependencies = TRUE, repos = "http://cran.rstudio.com")'

#Copy App
COPY ./app/ /srv/shiny-server/

# COPY .Renviron /usr/local/lib/R/etc/.myRenviron
# RUN cat /usr/local/lib/R/etc/.myRenviron >> /usr/local/lib/R/etc/Renviron

COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838

#allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

#run App
#CMD ["/usr/bin/shiny-server"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host='0.0.0.0', port=3838)"]