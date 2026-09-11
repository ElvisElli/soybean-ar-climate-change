#!/usr/bin/env python3
"""
Create realistic daily temperature and photoperiod data for soybean trial locations.

This script generates climate-realistic data based on regional norms.
For actual NASA POWER data, run the R script with nasapower or apsimx packages.
"""

import csv
from datetime import datetime, timedelta
import math
import random

def calculate_photoperiod(doy, latitude_deg):
    """Calculate photoperiod (hours) for a given DOY and latitude (Spencer 1971)."""
    latitude_rad = math.radians(latitude_deg)
    b = (doy - 1) * 2 * math.pi / 365
    decl = (0.006918 - 0.399912 * math.cos(b) + 0.070257 * math.sin(b) -
            0.006758 * math.cos(2*b) + 0.000907 * math.sin(2*b) -
            0.00002 * math.cos(3*b) + 0.00029 * math.sin(3*b))
    cos_h = -math.tan(latitude_rad) * math.tan(decl)
    cos_h = max(-1, min(1, cos_h))
    h = math.acos(cos_h)
    return 24 * h / math.pi

# Regional climate parameters (based on NOAA data)
locations = {
    "Fayetteville, AR": {
        "lat": 36.0726,
        "lon": -94.1729,
        "seasonal_temp_mean": 24.8,  # May-Aug average
        "seasonal_temp_amplitude": 8.5,  # ±amplitude from mean
        "noise_std": 2.5  # daily variability
    },
    "Columbia, MO": {
        "lat": 38.2526,
        "lon": -92.2734,
        "seasonal_temp_mean": 23.2,  # May-Aug average (cooler than AR)
        "seasonal_temp_amplitude": 9.0,
        "noise_std": 2.8
    }
}

sowing_dates = {
    "Early (Apr 15)": {"date": "2023-04-15", "doy": 105},
    "Late (Jun 15)": {"date": "2023-06-15", "doy": 166}
}

all_data = []
random.seed(42)

print("Generating realistic daily temperature and photoperiod data...")
print("=" * 70)

for loc_name, loc_info in locations.items():
    for sow_name, sow_info in sowing_dates.items():
        print(f"\n{loc_name} - {sow_name}")

        start_dt = datetime.strptime(sow_info["date"], "%Y-%m-%d")
        end_dt = start_dt + timedelta(days=129)

        scenario_name = f"{loc_name} - {sow_name}"

        for day_offset in range(130):  # 130 days for 130-day crop cycle
            current_dt = start_dt + timedelta(days=day_offset)
            doy = current_dt.timetuple().tm_yday

            # Temperature model: seasonal curve + daily noise
            # Simple sine curve from April to September
            season_phase = (doy - 105) / 180 * math.pi  # 0 at DOY 105, pi at ~290
            if season_phase < 0:
                season_phase = 0
            elif season_phase > math.pi:
                season_phase = math.pi

            seasonal_mean = (loc_info["seasonal_temp_mean"] +
                           loc_info["seasonal_temp_amplitude"] *
                           math.sin(season_phase))
            daily_noise = random.gauss(0, loc_info["noise_std"])
            temp_mean = seasonal_mean + daily_noise

            # Diurnal temperature range: ~8-10°C for growing season
            diurnal_range = 9.0
            temp_min = temp_mean - diurnal_range / 2
            temp_max = temp_mean + diurnal_range / 2

            # Calculate photoperiod
            pp = calculate_photoperiod(doy, loc_info["lat"])

            all_data.append({
                "location": loc_name,
                "latitude": loc_info["lat"],
                "sowing_date": sow_name,
                "sowing_doy": sow_info["doy"],
                "calendar_date": current_dt.strftime("%Y-%m-%d"),
                "calendar_doy": doy,
                "day_in_season": day_offset + 1,
                "temp_mean": round(temp_mean, 1),
                "temp_min": round(temp_min, 1),
                "temp_max": round(temp_max, 1),
                "photoperiod_hours": round(pp, 2),
                "scenario": scenario_name
            })

        print(f"  ✓ Generated 130 days of data")

# Save to CSV
output_file = "temperature_and_photoperiod_data.csv"
with open(output_file, 'w', newline='') as f:
    writer = csv.DictWriter(f, fieldnames=all_data[0].keys())
    writer.writeheader()
    writer.writerows(all_data)

print("\n" + "=" * 70)
print(f"✓ Data saved to: {output_file}")
print(f"✓ Total records: {len(all_data)}")

# Summary statistics
print(f"\nTemperature range across all scenarios:")
temps = [float(d["temp_mean"]) for d in all_data]
print(f"  Min: {min(temps):.1f}°C")
print(f"  Max: {max(temps):.1f}°C")
print(f"  Mean: {sum(temps)/len(temps):.1f}°C")

print(f"\nPhotoperiod range across all scenarios:")
pps = [float(d["photoperiod_hours"]) for d in all_data]
print(f"  Min: {min(pps):.1f} hours")
print(f"  Max: {max(pps):.1f} hours")

# By scenario
scenarios = {}
for d in all_data:
    if d["scenario"] not in scenarios:
        scenarios[d["scenario"]] = {"temps": [], "pps": []}
    scenarios[d["scenario"]]["temps"].append(float(d["temp_mean"]))
    scenarios[d["scenario"]]["pps"].append(float(d["photoperiod_hours"]))

print(f"\nTemperature summary by scenario:")
for scenario in sorted(scenarios.keys()):
    temps = scenarios[scenario]["temps"]
    print(f"  {scenario}:")
    print(f"    Mean: {sum(temps)/len(temps):.1f}°C (range: {min(temps):.1f}–{max(temps):.1f}°C)")

print(f"\nPhotoperiod summary by scenario:")
for scenario in sorted(scenarios.keys()):
    pps = scenarios[scenario]["pps"]
    print(f"  {scenario}:")
    print(f"    Range: {min(pps):.1f}–{max(pps):.1f} hours")

print("\n" + "=" * 70)
print("NOTE: This is climate-realistic simulated data for visualization.")
print("      For actual NASA POWER data, use the R script:")
print("      rscript fetch_nasa_power_real_data.R")
