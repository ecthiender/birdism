#!/usr/bin/env python3
import sys
import psycopg2

try:
    csv_path = sys.argv[1]
except IndexError:
    print('ERROR: Taxonomy CSV file path required.')
    sys.exit(1)

conn = psycopg2.connect("host=localhost dbname=bih user=postgres")
cur = conn.cursor()
with open(csv_path, 'r') as fp:
    next(fp)  # Skip the header row.
    cur.copy_from(fp, 'taxonomy', sep=',')

conn.commit()
