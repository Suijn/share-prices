FROM rocker/shiny as base

WORKDIR app

FROM base as build_system_dependencies
RUN apt-get update
RUN apt-get install -y libcurl4-openssl-dev
RUN apt-get install -y libssl-dev
RUN apt-get install -y libxml2 libxml2-dev

FROM build_system_dependencies as install_shiny_server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'rmarkdown'))" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

FROM install_shiny_server as copy_necessary_files
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /usr/bin/shiny-server.sh

FROM copy_necessary_files as install_project_dependencies
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'tidyverse', 'dplyr', 'tidyquant', 'quantmod', 'crypto2', 'plotly', 'gridExtra'))"

FROM install_project_dependencies as project
COPY R /srv/shiny-server/R
EXPOSE 80
CMD ["/usr/bin/shiny-server.sh"]
