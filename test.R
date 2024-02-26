result <- calculate_emissions(
    area = building_ex$GEBF,
    floors = building_ex$GASTW,
    year = building_ex$GBAUJ,
    utilisation_key = building_ex$utilisation_key,
    climate_code = building_ex$climate_code,
    energy_carrier = building_ex$energy_carrier
  )

print(result)