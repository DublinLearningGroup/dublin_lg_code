FROM rocker/verse:3.6.3


RUN apt-get update \
  && apt-get -y upgrade \
  && apt-get install -y --no-install-recommends \
    byobu \
    htop \
  && apt-get clean \
  && install2.r --error \
    furrr \
    broom \
    knitr
