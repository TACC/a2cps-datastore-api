# Shiny app implementation of an example datastore

## Details

1. Dockerfile packages the R app with shiny framework
2. The shiny app integrates with https://github.com/a2cps/Data-Monitoring-reports
3. The CSV files are in storage and mounted via Persistent Volume Claim.


## How to run

1. docker buildx build --platform linux/amd64 --build-arg UID=1027 --build-arg GID=1031 . -t taccdash/shiny-briha-demo:latest
2. docker push taccdash/shiny-briha-demo:latest
3. Ensure the endpoint is setup in Core-Deployment
4. a2cps VM: /home/a2cps-dev/shiny-app-briha-demo/burndown
5. a2cps VM: wait for pods to burn down.
6. a2cps VM: /home/a2cps-dev/shiny-app-briha-demo/burnup-latest