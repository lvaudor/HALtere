FROM rocker/r2u:jammy

LABEL org.opencontainers.image.authors="Lise Vaudor <lise.vaudor@ens-lyon.fr>, Samuel Dunesme <samuel.dunesme@ens-lyon.fr>"
LABEL org.opencontainers.image.source="https://github.com/lvaudor/HALtere"

RUN locale-gen fr_FR.UTF-8

RUN Rscript -e 'install.packages("shiny")'

RUN Rscript -e 'install.packages("ggraph")'
RUN Rscript -e 'install.packages("httr")'
RUN Rscript -e 'install.packages("jsonlite")'
RUN Rscript -e 'install.packages("plotly")'
RUN Rscript -e 'install.packages("tidygraph")'
RUN Rscript -e 'install.packages("tidytext")'
RUN Rscript -e 'install.packages("glue")'
RUN Rscript -e 'install.packages("tidyr")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("DT")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("remotes")'

RUN R -e 'remotes::install_github("lvaudor/mixr")'
RUN R -e 'remotes::install_github("lvaudor/HALtere_pack")'

RUN mkdir /app
ADD . /app
WORKDIR /app

EXPOSE 3840

RUN groupadd -g 1010 app && useradd -c 'app' -u 1010 -g 1010 -m -d /home/app -s /sbin/nologin app
USER app

CMD  ["R", "-e", "shiny::runApp('.', port=3840, host='0.0.0.0')"]