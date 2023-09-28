# Shiny app implementation of an example datastore

## Details

1. Dockerfile packages the R app with shiny framework
2. The shiny app integrates with portal authentication and provides necessary session data to datastore to fetch api data.
3. The app uses imaging end point.


## How to run

1. docker buildx build --platform linux/amd64 . -t taccdash/shiny-demo:latest
2. docker push taccdash/shiny-demo:latest
3. Ensure the endpoint is setup in Core-Deployment
4. burnup