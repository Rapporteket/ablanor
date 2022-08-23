FROM rocker/r-base:4.1.2

LABEL maintainer "Are Edvardsen <are.edvardsen@helse-nord.no>"
LABEL no.mongr.cd.enable="true"

WORKDIR /app/R

## add package tarball
# hadolint ignore=DL3010
COPY *.tar.gz .

## install package, clean up and make sure sufficient latex tools exists in base image
RUN R CMD INSTALL --clean ./*.tar.gz \
    && rm ./*.tar.gz

EXPOSE 3008

CMD ["R", "-e", "options(shiny.port=3008,shiny.host='0.0.0.0'); ablanor::run_app()"]
