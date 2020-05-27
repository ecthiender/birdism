COPY taxonomy(scientific_name, common_name, species_code, category, taxonomy_order, common_name_code, scientific_name_code, banding_codes, order_name, family_common_name, family_scientific_name, report_as, extinct, extinct_year)
FROM STDIN DELIMITER ',' CSV HEADER;
