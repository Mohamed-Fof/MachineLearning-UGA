FROM rocker/shiny:latest

RUN R -e "install.packages(c( \
  'shinydashboard', 'ggplot2', 'corrplot', \
  'DT', 'caret', 'rpart', 'glmnet', 'naivebayes', 'scales' \
), repos='https://cran.rstudio.com/', Ncpus=2)"

COPY . /srv/shiny-server/app/

# Remplace la config shiny-server par la nôtre (port 10000 pour Render)
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN chown -R shiny:shiny /srv/shiny-server/app

EXPOSE 10000

CMD ["/usr/bin/shiny-server"]
