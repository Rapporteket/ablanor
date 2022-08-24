FROM rapporteket/base-r:4.2.1

LABEL maintainer "Are Edvardsen <are.edvardsen@helse-nord.no>"
LABEL no.rapporteket.cd.enable="true"

WORKDIR /app/R

# hadolint ignore=DL3010
COPY *.tar.gz .

RUN R -e "remotes::install_local(list.files(pattern = \"*.tar.gz\"))"
    && rm ./*.tar.gz

EXPOSE 3008

CMD ["R", "-e", "options(shiny.port = 3008,shiny.host = \"0.0.0.0\"); ablanor::run_app()"]
