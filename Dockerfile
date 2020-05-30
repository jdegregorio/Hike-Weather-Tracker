FROM rocker/r-ver:3.6.1

RUN apt-get -y update

RUN apt-get -y install libcurl4-openssl-dev

COPY ./packrat/packrat.lock packrat/

RUN Rscript -e 'install.packages("packrat")'

RUN Rscript -e 'packrat::restore()'