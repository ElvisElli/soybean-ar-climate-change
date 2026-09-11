#!/usr/bin/env python3
"""
Fetch NASA POWER daily temperature data for proposed soybean trial locations
and sowing dates.

Uses only standard library (no pandas required).
"""

import urllib.request
import json
from datetime import datetime, timedelta
import csv
import math

# NASA POWER API endpoint (using point-based API)
API_URL = "https://power.larc.nasa.gov/api/v2/daily"

# Location coordinates
locations = {
    "Fayetteville_AR": {
        "lat": 36.0726,
        "lon": -94.1729,
        "name": "Fayetteville, AR"
    },
    "Columbia_MO": {
        "lat": 38.2526,
        "lon": -92.2734,
        "name": "Columbia, MO"
    }
}

# Sowing dates
sowing_dates = {
    "Early_Apr15": {"date": "2023-04-15", "doy": 105, "name": "Early (Apr 15)"},
    "Late_Jun15": {"date": "2023-06-15", "doy": 166, "name": "Late (Jun 15)"}
}

def calculate_photoperiod(doy, latitude_deg):
    """
    Calculate photoperiod (hours) for a given day of year and latitude.
    Based on Spencer (1971) solar declination calculation.
    """
    latitude_rad = math.radians(latitude_deg)

    # Solar declination (Spencer, 1971)
    b = (doy - 1) * 2 * math.pi / 365
    decl = (0.006918 - 0.399912 * math.cos(b) + 0.070257 * math.sin(b) -
            0.006758 * math.cos(2*b) + 0.000907 * math.sin(2*b) -
            0.00002 * math.cos(3*b) + 0.00029 * math.sin(3*b))

    # Hour angle at sunrise/sunset
    cos_h = -math.tan(latitude_rad) * math.tan(decl)
    cos_h = max(-1, min(1, cos_h))  # constrain to [-1, 1]
    h = math.acos(cos_h)

    # Photoperiod in hours
    pp = 24 * h / math.pi
    return pp

all_data = []

print("Fetching NASA POWER data...")
print("=" * 70)

for loc_key, loc_info in locations.items():
    for sow_key, sow_info in sowing_dates.items():

        # Calculate start and end dates (115-day growing season)
        start_date = sow_info["date"]
        start_dt = datetime.strptime(start_date, "%Y-%m-%d")
        end_dt = start_dt + timedelta(days=115)
        end_date = end_dt.strftime("%Y-%m-%d")

        print(f"\n{loc_info['name']} - {sow_info['name']}")
        print(f"  Lat/Lon: {loc_info['lat']:.4f}, {loc_info['lon']:.4f}")
        print(f"  Date range: {start_date} to {end_date}")

        # Build API URL (using point-based daily API)
        params_str = (
            f"?start={start_date.replace('-', '')}"
            f"&end={end_date.replace('-', '')}"
            f"&latitude={loc_info['lat']}"
            f"&longitude={loc_info['lon']}"
            f"&parameters=T2M,T2MN,T2MX"
            f"&format=json"
        )
        url = f"{API_URL}{params_str}"

        try:
            with urllib.request.urlopen(url, timeout=30) as response:
                data = json.loads(response.read().decode())

            if "properties" in data and "parameter" in data["properties"]:
                temp_data = data["properties"]["parameter"]
                temp_mean_data = temp_data.get("T2M", {})
                temp_min_data = temp_data.get("T2MN", {})
                temp_max_data = temp_data.get("T2MX", {})

                days_after_sowing = 1
                for date_str in sorted(temp_mean_data.keys()):
                    try:
                        year = int(date_str[:4])
                        month = int(date_str[4:6])
                        day = int(date_str[6:8])
                        date_obj = datetime(year, month, day)
                        doy = date_obj.timetuple().tm_yday

                        t_mean = temp_mean_data.get(date_str)
                        t_min = temp_min_data.get(date_str)
                        t_max = temp_max_data.get(date_str)

                        if t_mean is not None:
                            # Calculate photoperiod for this location and day
                            pp = calculate_photoperiod(doy, loc_info["lat"])

                            all_data.append({
                                "location": loc_info["name"],
                                "latitude": loc_info["lat"],
                                "sowing_date": sow_info["name"],
                                "sowing_doy": sow_info["doy"],
                                "calendar_date": date_obj.strftime("%Y-%m-%d"),
                                "calendar_doy": doy,
                                "day_in_season": days_after_sowing,
                                "temp_mean": t_mean,
                                "temp_min": t_min,
                                "temp_max": t_max,
                                "photoperiod_hours": round(pp, 2),
                                "scenario": f"{loc_info['name']} - {sow_info['name']}"
                            })
                            days_after_sowing += 1
                    except (ValueError, KeyError):
                        continue

                print(f"  ✓ Retrieved {days_after_sowing - 1} days of data")
            else:
                print(f"  ✗ No temperature data in response")

        except Exception as e:
            print(f"  ✗ Error: {e}")

# Save to CSV
if all_data:
    output_file = "nasa_power_temperature_data.csv"

    with open(output_file, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=all_data[0].keys())
        writer.writeheader()
        writer.writerows(all_data)

    print("\n" + "=" * 70)
    print(f"✓ Data saved to: {output_file}")
    print(f"✓ Total records: {len(all_data)}")

    # Summary statistics
    print(f"\nTemperature range across all scenarios:")
    temps = [d["temp_mean"] for d in all_data if d["temp_mean"] is not None]
    if temps:
        print(f"  Mean: {min(temps):.1f}°C to {max(temps):.1f}°C")
        print(f"  Avg:  {sum(temps)/len(temps):.1f}°C")

    # By scenario
    scenarios = {}
    for d in all_data:
        if d["scenario"] not in scenarios:
            scenarios[d["scenario"]] = []
        if d["temp_mean"] is not None:
            scenarios[d["scenario"]].append(d["temp_mean"])

    print(f"\nTemperature summary by scenario:")
    for scenario, temps in sorted(scenarios.items()):
        if temps:
            print(f"  {scenario}:")
            print(f"    Mean: {sum(temps)/len(temps):.1f}°C (range: {min(temps):.1f}–{max(temps):.1f}°C)")
else:
    print("\n✗ No data retrieved.")

print("\n" + "=" * 70)
