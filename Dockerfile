FROM rapporteket/base-r:main

LABEL maintainer="Arnfinn Hykkerud Steindal <arnfinn.hykkerud.steindal@helse-nord.no>"
LABEL no.rapporteket.cd.enable="true"

WORKDIR /app/R

COPY *.tar.gz .

RUN R -e "remotes::install_local(list.files(pattern = \"*.tar.gz\"))" \
    && rm ./*.tar.gz \
    && R -e "remotes::install_github(\"Rapporteket/rapbase\", ref = \"simple_app\")"

EXPOSE 3838

RUN adduser --uid "1000" --disabled-password rapporteket && \
    chown -R 1000:1000 /app/R && \
    chmod -R 755 /app/R
USER rapporteket

CMD ["R", "-e", "options(shiny.port = 3838, shiny.host = \"0.0.0.0\"); rygg::kjorRyggApp()"]
