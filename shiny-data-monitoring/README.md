# Shiny app implementation of an example datastore

## Details

1. Dockerfile packages the R app with shiny framework
2. The shiny app integrates with https://github.com/a2cps/Data-Monitoring-reports


## How to run

1. docker buildx build --platform linux/amd64 . -t taccdash/shiny-briha-demo:latest
2. docker push taccdash/shiny-briha-demo:latest
3. Ensure the endpoint is setup in Core-Deployment
4. burnup-latest