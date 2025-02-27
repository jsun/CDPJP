# Data Preparation

## Weather Data

Check https://www.data.jma.go.jp/obd/stats/etrn/index.php to get the location ID
and save them to `mst/weather_station.mst.tsv`.
Then, use the following Python script to download weather data according to the location ID,
and save the weather data into a local database (db.sqlite3).

```
python create_db_weather.py ./mst ./weather_html_data ../data/db.sqlite3
```

## MAFF CDP Data

Manually download the raw data and 
run the following script to save the data into the local database (db.sqlite3).


```
Rscript --vanilla create_db_damagerecords.R ./damage_records_data ./mst ../data/db.sqlite3
```

## Notes

Raw data is not published in Zenodo. Instead, the local database (db.sqlite3) is published.



