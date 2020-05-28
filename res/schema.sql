CREATE TABLE IF NOT EXISTS taxonomy (
  id SERIAL PRIMARY KEY,
  scientific_name TEXT,
  common_name TEXT,
  species_code TEXT,
  category TEXT,
  taxonomy_order NUMERIC,
  common_name_code TEXT,
  scientific_name_code TEXT,
  banding_codes TEXT,
  order_name TEXT,
  family_common_name TEXT,
  family_scientific_name TEXT,
  report_as TEXT,
  extinct BOOLEAN,
  extinct_year INTEGER
);

CREATE TABLE IF NOT EXISTS region (
  region_code TEXT,
  region_name TEXT
);
