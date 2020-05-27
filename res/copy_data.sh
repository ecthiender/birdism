#!/bin/bash
psql -h localhost -U postgres -d bih < res/copy_data.sql < ws/ebird_taxonomy_2020_05_26.csv
