FROM r-base as base

WORKDIR app

FROM base as build_app_dependecies
COPY renv.lock renv.lock
RUN R -e "install.packages('renv')"
RUN R -e "renv::activate()"

ENTRYPOINT ["echo", "hello"]