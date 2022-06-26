FROM r-base as base

WORKDIR app

FROM base as build_app_dependecies
COPY renv.lock renv.lock
RUN R -e "install.packages('renv')"
RUN R -e "renv::activate()"
RUN R -e "renv::restore()"

ENTRYPOINT ["echo", "hello"]