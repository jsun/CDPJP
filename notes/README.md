# Data Analysis

To build models, run the following scripts on terminal.


```bash
Rscript run_modeling.R NA ARIMA
Rscript run_modeling.R NA SARIMA
Rscript run_modeling.R NA SARIMAX
Rscript run_modeling.R NA GPR
Rscript run_modeling.R NA RF
Rscript run_modeling.R NA PA
Rscript run_modeling.R NA RAND

Rscript run_modeling.R 1 ARIMA
Rscript run_modeling.R 1 SARIMA
Rscript run_modeling.R 1 SARIMAX
Rscript run_modeling.R 1 GPR
Rscript run_modeling.R 1 RF
Rscript run_modeling.R 1 PA
Rscript run_modeling.R 1 RAND

Rscript run_modeling.R 3 ARIMA
Rscript run_modeling.R 3 SARIMA
Rscript run_modeling.R 3 SARIMAX
Rscript run_modeling.R 3 GPR
Rscript run_modeling.R 3 RF
Rscript run_modeling.R 3 PA
Rscript run_modeling.R 3 RAND

Rscript run_modeling.R 5 ARIMA
Rscript run_modeling.R 5 SARIMA
Rscript run_modeling.R 5 SARIMAX
Rscript run_modeling.R 5 GPR
Rscript run_modeling.R 5 RF
Rscript run_modeling.R 5 PA
Rscript run_modeling.R 5 RAND
```


To calculate metrics and figures representing performance of models, run the following script on terminal.


```
Rscript data_vis.R
```


# Annoations

A dictionary to convert crop names, crop diseases, and crop pests from Japanese to English is stored in `ennames.csv` in this directly.


