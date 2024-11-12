# disease and pest outbreak data analysis


## Database

### Weather Data

Check https://www.data.jma.go.jp/obd/stats/etrn/index.php to get the location ID
and save them to `mst/weather_station.mst.tsv`.
Then, use the following Python script to download weather data according to the location ID.

```
python create_db_weather.py ./mst ./weather_html_data ../data/db.sqlite3
```

### Outbreak Data

Download the raw data and insert them to SQLite.

```
Rscript --vanilla create_db_damagerecords.R ./damage_records_data ./mst ../data/db.sqlite3
```


## Data Analysis

Check Rmarkdown in `notes` directory.


