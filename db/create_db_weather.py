import os
import sys
import re
import time
import datetime
import tqdm
import random
import glob
import gzip
import requests
import bs4
import sqlite3



def load_locations(fpath: str) -> list:
    locations = []
    url_ptn = re.compile('prec_no=([0-9]+)&block_no=([0-9]+)')
    with open(fpath, 'r') as infh:
        for line in infh:
            pref_name, block_name, url = line.replace('\n', '').split('\t')
            pref_code = block_code = None
            m = url_ptn.search(url)
            if m:
                pref_code = m.group(1)
                block_code = m.group(2)
            locations.append({
                'pref_name': pref_name,
                'block_name': block_name,
                'pref_code': str(pref_code),
                'block_code': str(block_code)
            })
    return locations
    


def download_html(pref_code: str, block_code: str, year: str, output_fpath: str) -> None:
    url = 'https://www.data.jma.go.jp/obd/stats/etrn/view/monthly_s1.php?prec_no={}&block_no={}&year={}&month=&day=&view='.format(pref_code, block_code, year)
    http_data = requests.get(url)
    with gzip.open(output_fpath, 'wt') as outfh:
        outfh.write(http_data.text)



def download_weather_data(mst_dpath: str, output_dpath: str) -> None:
    locations = load_locations(os.path.join(mst_dpath, 'weather_station.mst.tsv'))
    
    this_year = datetime.date.today().year
    for year in tqdm.tqdm(range(1995, this_year + 1), desc='downloading'):
        for loc in tqdm.tqdm(locations, leave=False):
            output_fpath = os.path.join(output_dpath,
                '{}-{}-{}.html.gz'.format(loc['pref_name'], loc['block_name'], year))
            if not os.path.exists(output_fpath):
                download_html(loc['pref_code'], loc['block_code'], year, output_fpath)
                time.sleep(random.randint(0, 10))
    


def format_cells(x: list) -> list:
    for i in range(len(x)):
        x[i] = x[i].replace(' )', '').replace(' ]', '')
        x[i] = x[i].replace('#', '').replace('///', '').replace('×', '').replace(b'\xc3\x83\xc2\x97'.decode(), '')
        x[i] = x[i].replace(' ', '')
        x[i] = 'NULL' if x[i] == '' else x[i]
    return x



def parse_html(fpath: str) -> list:
    prec, block, year = os.path.splitext(os.path.basename(fpath))[0].split('-')
    year = year.replace('.gz', '').replace('.html', '')
    records = []
    with gzip.open(fpath, 'rt') as infh:
        html_text = infh.read()

    html = bs4.BeautifulSoup(html_text, 'html.parser')
    table = html.select_one('#tablefix1')
    
    for tr in table.select('tr'):
        td_cells = []
        for td in tr.select('td'):
            td_cells.append(td.text)
        if (len(td_cells) > 0) and (td_cells[3] != '' and td_cells[7] != ''):
            td_cells = [block, '{}-{}-01'.format(year, td_cells[0].zfill(2)),
                        td_cells[3], td_cells[7],
                        td_cells[10], td_cells[11], td_cells[12], td_cells[13],
                        td_cells[19]]
            td_cells = format_cells(td_cells)
            records.append(td_cells)
    return records



def create_table_prefecture(cur: object, mst_dpath: str) -> None:
    
    cur.execute('CREATE TABLE IF NOT EXISTS prefectures (name TEXT PRIMARY KEY, jis_code TEXT)') 
    cur.execute('BEGIN TRANSACTION insert_prefs')
    with open(os.path.join(mst_dpath, 'prefectures.mst.tsv'), 'r') as infh:
        line = infh.readline()
        for line in infh:
            pref_id, pref_name = line.replace('\n', '').split('\t')
            cur.execute('INSERT INTO prefectures VALUES("{}", "{}")'.format(pref_name, pref_id))
    cur.execute('COMMIT TRANSACTION insert_prefs')




def create_table_location(cur: object, mst_dpath: str):
    locations = load_locations(os.path.join(mst_dpath, 'weather_station.mst.tsv'))
    
    loc2pref = {}
    for location in locations:
        loc2pref[location['block_name']] = location['pref_name']
    
    cur.executescript('''
        CREATE TABLE observatories(
            name TEXT PRIMARY KEY,
            prefecture TEXT NOT NULL,
            FOREIGN KEY (prefecture) REFERENCES prefectures(name)
        );
    ''')
    cur.execute('BEGIN TRANSACTION insert_obs')
    for loc_name, pref_name in loc2pref.items():
        cur.execute('INSERT INTO observatories VALUES("{}", "{}")'.format(loc_name, pref_name))
    cur.execute('COMMIT TRANSACTION insert_obs')
    



def create_db(mst_dpath: str, html_dpath: str, db_name: str) -> None:
    
    con = sqlite3.connect(db_name)
    cur = con.cursor()
    cur.execute('PRAGMA foreign_keys = ON')
    
    create_table_prefecture(cur, mst_dpath)
    create_table_location(cur, mst_dpath)
    
    cur.executescript('''
        CREATE TABLE weather (
            observatory TEXT NOT NULL,
            date TEXT NOT NULL,
            precipitation_ttl REAL,
            temperature_avg REAL,
            temperature_max REAL,
            temperature_min REAL,
            humidity_avg REAL,
            humidity_min REAL,
            sunshineduration_ttl REAL,
            FOREIGN KEY (observatory) REFERENCES observatories(name)
        );
    ''')
    cur.execute('BEGIN TRANSACTION insert_weather')
    
    try:
        for fpath in tqdm.tqdm(glob.glob(os.path.join(html_dpath, '*.html.gz')),
                               desc='parsing data'):
            records = parse_html(fpath)
            for record in records:
                sql_code = 'INSERT INTO weather VALUES("{}", "{}", {}, {}, {}, {}, "{}", "{}", {})'.format(*record)
                cur.execute('INSERT INTO weather VALUES("{}", "{}", {}, {}, {}, {}, {}, {}, {})'.format(*record))
        cur.execute('COMMIT TRANSACTION insert_weather')
    except KeyboardInterrupt:
        cur.execute('ROLLBACK TRANSACTION insert_weather')
        print('Interrupt by Keybord signal.')
    except Exception as e:
        cur.execute('ROLLBACK TRANSACTION insert_weather')
        print(fpath, sql_code, e)
    
    con.close()




def main(mst_dpath: str, html_dpath: str, db_dpath: str) -> None:
    download_weather_data(mst_dpath, html_dpath)
    create_db(mst_dpath, html_dpath, db_dpath)


if __name__ == '__main__':
    mst_dpath = sys.argv[1]
    html_dpath = sys.argv[2]
    db_dpath = sys.argv[3]
    main(mst_dpath, html_dpath, db_dpath)


