FROM rocker/shiny:latest

RUN R -e "install.packages(c( \
  'shinydashboard', 'ggplot2', 'corrplot', \
  'DT', 'caret', 'rpart', 'glmnet', 'naivebayes', 'scales' \
), repos='https://cran.rstudio.com/', Ncpus=2)"

COPY . /srv/shiny-server/app/

RUN chown -R shiny:shiny /srv/shiny-server/app

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
