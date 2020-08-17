#!/bin/bash

taxonomy_file="$1"
if [[ -z "$taxonomy_file" ]]; then
  echo "ERROR: Taxonomy CSV file path required."
  exit 1
fi

sql_cmd="COPY taxonomy(scientific_name, common_name, species_code, category, taxonomy_order, common_name_code, scientific_name_code, banding_codes, order_name, family_common_name, family_scientific_name, report_as, extinct, extinct_year) FROM STDIN DELIMITER ',' CSV HEADER;"
psql -h localhost -U postgres -d bih -c "$sql_cmd" < "$taxonomy_file"
