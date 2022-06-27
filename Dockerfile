FROM r-base as base

WORKDIR app

FROM base as build_app_dependecies
COPY renv.lock renv.lock
RUN apt-get update
RUN apt-get install libcurl4-openssl-dev
RUN apt-get install libssl-dev
RUN R -e "install.packages('curl')"
RUN R -e "install.packages('renv')"
RUN R -e "renv::activate()"
RUN R -e "renv::restore()"

ENTRYPOINT ["echo", "hello"]