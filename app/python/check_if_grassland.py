"""
Module Name: check_if_grassland.py
Description: Functions for checking if coordinates are grassland according to given TIF land cover map.

Developed in the BioDT project by Thomas Banitz (UFZ) with contributions by Franziska Taubert (UFZ),
Tuomas Rossi (CSC) and Taimur Haider Khan (UFZ).

Copyright (C) 2024
- Helmholtz Centre for Environmental Research GmbH - UFZ, Germany
- CSC - IT Center for Science Ltd., Finland

Licensed under the EUPL, Version 1.2 or - as soon they will be approved
by the European Commission - subsequent versions of the EUPL (the "Licence").
You may not use this work except in compliance with the Licence.

You may obtain a copy of the Licence at:
https://joinup.ec.europa.eu/software/page/eupl

This project has received funding from the European Union's Horizon Europe Research and Innovation
Programme under grant agreement No 101057437 (BioDT project, https://doi.org/10.3030/101057437).
The authors acknowledge the EuroHPC Joint Undertaking and CSC - IT Center for Science Ltd., Finland
for awarding this project access to the EuroHPC supercomputer LUMI, hosted by CSC - IT Center for
Science Ltd., Finland and the LUMI consortium through a EuroHPC Development Access call.

Data sources:
    # Land cover maps and classifications used:

    "EUR_eunis_habitat":
    - Eunis EEA habitat types (version 2012).
      https://eunis.eea.europa.eu/habitats-code-browser.jsp
    - Only for DEIMS Sites: Get all habitat types of a site, check if any of them is grassland.

    "EUR_hrl_grassland":
    - European Union's Copernicus Land Monitoring Service information (2020).
      High Resolution Layer (HRL) Grassland 2018 raster, Europe.
      https://doi.org/10.2909/60639d5b-9164-4135-ae93-fb4132bb6d83
    - REST API:
      https://sdi.eea.europa.eu/catalogue/copernicus/eng/catalog.search#/metadata/60639d5b-9164-4135-ae93-fb4132bb6d83

    "EUR_Pflugmacher":
    - Pflugmacher, D., Rabe, A., Peters, M., Hostert, P. (2018).
      Pan-European land cover map of 2015 based on Landsat and LUCAS data.
      PANGAEA, https://doi.org/10.1594/PANGAEA.896282

    "GER_Preidl":
    - Preidl, S., Lange, M., Doktor, Daniel (2020).
      Land cover classification map of Germany's agricultural area based on Sentinel-2A data from 2016.
      PANGAEA, https://doi.org/10.1594/PANGAEA.910837

    "GER_Schwieder":
    - Schwieder, M., Tetteh, G.O., Blickensdörfer, L., Gocht, A., Erasmi, S. (2024).
      Agricultural land use (raster): National-scale crop type maps for Germany from combined time series of
      Sentinel-1, Sentinel-2 and Landsat data (2017 to 2021).
      Zenodo, https://zenodo.org/records/10640528

    "GER_Lange":
    - German ATKIS digital landscape model 2015
      Bundesamt für Kartographie und Geodäsie, 2015.
      Digitales Basis-Landschaftsmodell (AAA-Modellierung).
      GeoBasis-DE. Geodaten der deutschen Landesvermessung.
    - derived via land use maps by Lange et al. (2022), https://data.mendeley.com/datasets/m9rrv26dvf/1


    # Further candidate maps, not implemented (yet):

    ESA WorldCover 10 m 2021 v200:
    - https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v100#citations
    - https://zenodo.org/records/7254221

    CORINE Land Cover 2018 (vector/raster 100 m), Europe
    - https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/digitale-landschaftsmodelle/corine-land-cover-5-ha-stand-2018-clc5-2018.html
    - https://land.copernicus.eu/en/products/corine-land-cover
    - REST API:
      https://image.discomap.eea.europa.eu/arcgis/rest/services/Corine/CLC2018_WM/MapServer


    # Predecessors of Schwieder 2024, not implemented:

    "GER_Griffiths":
    - Griffiths, P., Nendel, C., Hostert, P. (2018).
      National-scale crop- and land-cover map of Germany (2016) based on imagery acquired by Sentinel-2A MSI and Landsat-8 OLI.
      PANGAEA, https://doi.pangaea.de/10.1594/PANGAEA.893195

    "GER_Blickensdörfer":
    - Blickensdörfer, L., Schwieder, M., Pflugmacher, D., Nendel, C., Erasmi, S., Hostert, P. (2021).
      National-scale crop type maps for Germany from combined time series of Sentinel-1, Sentinel-2 and Landsat 8 data (2017, 2018 and 2019).
      Zenodo, https://zenodo.org/records/5153047)
"""

import argparse
import copy
import warnings
import xml.etree.ElementTree as ET
from datetime import datetime, timezone
from pathlib import Path

import deims
import pandas as pd
import requests
import utils as ut


def get_map_specs(map_key):
    """
    Set up map file names and specifications based on provided map key.

    Args:
        map_key (str): Identifier of the map to be used.

    Returns:
        dict: Dictionary containing the following keys:
            'file_stem': Stem of the file names.
            'map_ext': File extension for the map.
            'leg_ext': File extension for the legend (may extend the stem).
            'folder': Folder name for local files or files on opendap server.
            'subfolder': Subfolder name for local files or files on opendap server.
            'url_folder': URL to folder containing the files.
    """
    folder = "landCoverMaps"
    url_opendap = "http://opendap.biodt.eu/grasslands-pdt/"

    if map_key == "GER_Preidl":
        map_specs = {
            "file_stem": "preidl-etal-RSE-2020_land-cover-classification-germany-2016",
            "map_ext": ".tif",
            "leg_ext": ".tif.aux.xml",
            "folder": folder,
            "subfolder": map_key,
            "url_folder": f"{url_opendap}{folder}/{map_key}/",
        }
    elif map_key == "EUR_Pflugmacher":
        map_specs = {
            "file_stem": "europe_landcover_2015_RSE-Full3",
            "map_ext": ".tif",
            "leg_ext": "_legend.xlsx",
            "folder": folder,
            "subfolder": map_key,
            "url_folder": f"{url_opendap}{folder}/{map_key}/",  # "https://hs.pangaea.de/Maps/EuropeLandcover/" not working anymore,
        }
    elif map_key.startswith("GER_Schwieder_"):
        map_year = map_key.split("_")[-1]  # get year from map key

        if map_year in ["2017", "2018", "2019", "2020", "2021"]:
            map_specs = {
                "file_stem": "CTM_GER_",
                "map_ext": map_year + "_rst_v202_COG.tif",
                "leg_ext": "LegendEN_rst_v202.xlsx",
                "folder": folder,
                "subfolder": "GER_Schwieder",
                "url_folder": "https://zenodo.org/records/10640528/files/",
            }
        else:
            warnings.warn(f"Land cover map for key '{map_key}' not found!")
            return None
    elif map_key.startswith("GER_Lange_"):
        map_year = map_key.split("_")[-1]  # get year from map key

        if map_year in ["2017", "2018"]:
            map_ext = (
                "98d7c7ab-0a8f-4c2f-a78f-6c1739ee9354/file_downloaded"
                if map_year == 2017
                else "d871429a-b2a6-4592-b3e5-4650462a9ac3/file_downloaded"
            )
            map_specs = {
                "file_stem": "",
                "map_ext": map_ext,
                "leg_ext": "GER_Lange_Legend.xlsx",
                "folder": folder,
                "subfolder": "GER_Lange",
                "url_folder": "https://data.mendeley.com/public-files/datasets/m9rrv26dvf/files/",
            }
        else:
            warnings.warn(f"Land cover map for key '{map_key}' not found!")
            return None
    else:
        warnings.warn(f"Land cover map for key '{map_key}' not found!")
        return None

    return map_specs


def get_map_and_legend(map_key, *, cache=None):
    """
    Check if TIF file and categories file are avaible, read categories from file.

    Parameters:
        map_key (str): Identifier of the map to be used.
        cache (Path): Path for local map directory (optional).

    Returns:
        tuple: Tuple containing the following values:
            str: Full path or URL of TIF file.
            dict: Mapping of category indices to category names.
    """
    # Get map specifications and map file name
    map_specs = get_map_specs(map_key)
    file_name = f"{map_specs['file_stem']}{map_specs['map_ext']}"
    map_found = False

    if cache is not None:
        # Get map from local file
        map_file = Path(cache) / map_specs["subfolder"] / file_name

        if map_file.is_file():
            print(f"Land cover map found. Using '{map_file}'.")
            map_found = True
        else:
            print(f"Land cover map file '{map_file}' not found!")
            print("Trying to access via URL ...")

    if not map_found:
        # Get map directly from URL, no download
        map_file = f"{map_specs['url_folder']}{file_name}"

        if ut.check_url(map_file):
            print(f"Land cover map found. Using '{map_file}'.")
        else:
            raise FileNotFoundError(f"Land cover map file '{map_file}' not found!")

    # Get categories, files are very small and can be downloaded if not exisiting
    file_name = f"{map_specs['file_stem']}{map_specs['leg_ext']}"

    if cache is None:
        leg_file = (
            ut.get_package_root()
            / map_specs["folder"]
            / map_specs["subfolder"]
            / file_name
        )
    else:
        leg_file = Path(cache) / map_specs["subfolder"] / file_name

    if not leg_file.is_file():
        # Get categories file from opendap server
        print(f"Land cover categories file '{leg_file}' not found!")
        print("Trying to download from URL ...")
        ut.download_file_opendap(
            file_name,
            f"{map_specs['folder']}/{map_specs['subfolder']}",
            leg_file.parent,
        )

    if leg_file.is_file():
        # Read categories from file
        print(f"Land cover categories found. Using '{leg_file}'.")
        category_mapping = create_category_mapping(leg_file)
    else:
        raise FileNotFoundError(f"Land cover categories file '{file_name}' not found!")

    return map_file, category_mapping


def create_category_mapping(leg_file):
    """
    Create a mapping of category indices to category names from legend file (XML or XLSX or ...).

    Parameters:
        leg_file (str): The path to the leg file containing category names (in specific format).

    Returns:
        dict: A mapping of category indices to category names.
    """
    category_mapping = {}

    if leg_file.suffix == ".xlsx":
        try:
            df = pd.read_excel(leg_file)

            # Assuming category elements are listed in the first two columns (index and name)
            category_mapping = {row[0]: row[1] for row in df.values}
            # # Alternative using the row names 'code' and 'class_names'
            # category_mapping = (df[["code", "class_name"]].set_index("code")["class_name"].to_dict())
        except Exception as e:
            print(f"Error reading XLSX file: {str(e)}")
    elif leg_file.suffix == ".xml":
        try:
            tree = ET.parse(leg_file)
            root = tree.getroot()

            # Assuming category elements are nested within CategoryNames within PAMRasterBand
            category_names = root.find(".//CategoryNames")

            if category_names is not None:
                for index, category in enumerate(category_names):
                    category_name = category.text
                    category_mapping[index] = category_name
        except Exception as e:
            print(f"Error reading XML file: {str(e)}")

    return category_mapping


def get_category_tif(map_file, category_mapping, location):
    """
    Get the category based on the raster value at the specified location.

    Parameters:
        map_file (Path): Path to the raster file.
        category_mapping (dict): Mapping of raster values to categories.
        location (dict): Dictionary with 'lat' and 'lon' keys for extracting raster value.

    Returns:
         tuple: Category (str) corresponding to the raster value at the specified location,
             or "Unknown Category" if the value is not found in the mapping, and time stamp.
    """
    value, time_stamp = ut.extract_raster_value(map_file, location)
    category = category_mapping.get(value, "Unknown Category")

    if category == "Unknown Category":
        print(f"Unknown category value ({value})!")  # just info on unknown value

    return category, time_stamp


def get_category_deims(location):
    """
    Get all categories based on habitat types (eunisHabitat) of DEIMS-Site.

    Parameters:
        location (dict): Dictionary with 'lat' and 'lon' keys for extracting categories.

    Returns:
        tuple: List of categories as classified and time stamp, if found.
    """
    if "deims_id" in location:
        time_stamp = datetime.now(timezone.utc).isoformat(timespec="seconds")
        categories = []

        try:
            location_record = deims.getSiteById(location["deims_id"])
            eunis_habitats = (
                location_record.get("attributes", {})
                .get("environmentalCharacteristics", {})
                .get("eunisHabitat")
            )
        except Exception as e:
            print(
                f"Error: Access failed to DEIMS site record ('https://deims.org/api/sites/{location["deims_id"]}')."
            )
            print(f" Exception: {str(e)}.")

        if "eunis_habitats" in locals() and isinstance(eunis_habitats, list):
            for item in eunis_habitats:
                print(f"Habitat: {item['label']}")
                categories.append(item["label"])
        else:
            print("Habitat: Not found.")
            categories.append("Not found.")

        return categories, time_stamp
    else:
        raise ValueError(
            "No location DEIMS.iD provided, but needed with map key 'EUR_eunis_habitat' for requesting land cover information!"
        )


def get_category_hrl_grassland(location):
    """
    Get category based on HRL Grassland raster at specified location.

    Parameters:
        location (dict): Dictionary with 'lat' and 'lon' keys for extracting raster value.

    Returns:
        tuple: Category (str) as classified if found (e.g. 'grassland', 'non-grassland'), and time stamp.
    """
    # Define URL and request
    url = "https://image.discomap.eea.europa.eu/arcgis/rest/services/GioLandPublic/HRL_Grassland_2018/ImageServer"

    # # test for CORINE
    # # params unclear
    # url = "https://image.discomap.eea.europa.eu/arcgis/rest/services/Corine/CLC2018_WM/MapServer"

    geometry = {
        "x": location["lon"],
        "y": location["lat"],
        "spatialReference": {"wkid": 4326},
    }
    params = {
        "geometry": str(geometry),
        "geometryType": "esriGeometryPoint",
        "pixelSize": "0.1",
        "f": "json",
    }

    # Send request
    time_stamp = datetime.now(timezone.utc).isoformat(timespec="seconds")
    response = requests.get(f"{url}/identify", params=params)

    # Check if the request was successful
    if response.status_code == 200:
        # Parse the JSON response
        data = response.json()

        # Check if 'value' key exists in the response
        if "value" in data:
            value = data["value"]

            # Return classification based on value
            if value == "0":
                return "non-grassland", time_stamp
            if value == "1":
                return "grassland", time_stamp
            if value == "254":
                return (
                    "unclassifiable (no satellite image available, clouds, shadows or snow)",
                    time_stamp,
                )
            if value == "255":
                return "outside area", time_stamp

            # Handle unknown values
            warnings.warn(f"Unknown value for specified location: {value}.")
            return value, time_stamp
        else:
            warnings.warn("No value for specified location.")
    else:
        print(f"Error: {response.status_code}")

    return None, time_stamp


def check_desired_categories(category, target_categories, location, map_key):
    """
    Check if the given category is one of the target categories.

    Parameters:
        category (str): Category to check.
        target_categories (list): List of target categories to compare against.
        location (dict): Dictionary with 'lat' and 'lon' keys.
        map_key(str): Identifier of the map used for obtaining the category.

    Returns:
        bool: True if the category is in the target categories, False otherwise.
    """
    # Check if extracted categories are in any of the target categories
    is_target_categories = category in target_categories

    # Print check results
    if is_target_categories:
        print(
            f"Confirmed: Lat. {location['lat']}, Lon. {location['lon']}",
            f"is '{category}' according to '{map_key}' land cover classification.",
        )
    else:
        print(
            f"Not in target categories: Lat. {location['lat']}, Lon. {location['lon']}",
            f"is '{category}' according to '{map_key}' land cover classification.",
        )

    return is_target_categories


def check_if_grassland(category, location, map_key):
    """
    Check if a category represents grassland based on the given category mapping.

    Parameters:
        category (str): Category to check.
        location (dict): Dictionary with 'lat' and 'lon' keys.
        map_key (str): Identifier of the map to be used.

    Returns:
        bool: True if the category represents grassland, False otherwise.
    """
    if map_key == "EUR_eunis_habitat":
        # Set accepted eunis EEA habitat types
        grass_labels = ["E", "E1", "E2", "E3", "E4", "E5"]
        # not included:
        # E6 : Inland salt steppes
        # E7 : Sparsely wooded grasslands
        habitat_label = category.split("(")[-1].strip(")")
        is_grassland = habitat_label == grass_labels[0]

        if not is_grassland:
            # Check if any element in the reference list is a prefix of part_in_brackets
            for label in grass_labels[1:]:
                if habitat_label.startswith(label):
                    is_grassland = True
                    break
    else:
        # Set accepted categories
        grass_categories = [
            "Grassland",
            "grassland",
            "grass",
            "Permanent grassland",
            "Cultivated grassland",
            "Fallow land",
            "Bare land",
        ]
        # not inlcuded:
        # "Legumes"

        is_grassland = check_desired_categories(
            category, grass_categories, location, map_key
        )

    return is_grassland


def check_results_to_file(grassland_check, *, file_name=None, map_key=None):
    """
    Save grassland check results to file.

    Args:
        grassland_check (list): List of dictionaries containing grassland check results.
        file_name (str or Path): File name (default is None, default file name is used if not provided).
        map_key (str): Map key to be included in the default file name if no file_name provided (default is None).

    Returns:
        None
    """
    if file_name is None:
        if map_key is None:
            map_key = ""
        file_name = (
            ut.get_package_root()
            / "landCoverCheckResults"
            / f"grasslandCheck_{map_key}.txt"
        )

    column_names = ut.get_unique_keys(grassland_check)
    ut.list_to_file(grassland_check, file_name, column_names=column_names)


def check_locations_for_grassland(locations, map_key, file_name=None):
    """
    Check if given locations correspond to grassland areas based on the provided land cover map.

    Parameters:
        locations (list): List of location dictionaries containing coordinates ('lat', 'lon'), may also contain DEIMS.iD ('deims_id').
        map_key (str): Identifier of the map to be used.
        file_name (str or Path): File name to save check results (no file will be created otherwise).
    Returns:
        list of dict: List of dicioniaries containing the check results for each location.
    """
    print("Starting grassland check ...")
    deims_keys = ["EUR_eunis_habitat"]
    tif_keys = [
        "EUR_Pflugmacher",
        "GER_Preidl",
        "GER_Schwieder_2017",
        "GER_Schwieder_2018",
        "GER_Schwieder_2019",
        "GER_Schwieder_2020",
        "GER_Schwieder_2021",
        "GER_Lange_2017",
        "GER_Lange_2018",
    ]
    hrl_keys = ["EUR_hrl_grassland"]
    map_years = {
        "EUR_eunis_habitat": 2012,
        "EUR_Pflugmacher": 2015,
        "GER_Preidl": 2016,
        "GER_Schwieder_2017": 2017,
        "GER_Schwieder_2018": 2018,
        "GER_Schwieder_2019": 2019,
        "GER_Schwieder_2020": 2020,
        "GER_Schwieder_2021": 2021,
        "GER_Lange_2017": 2015,
        "GER_Lange_2018": 2015,
        "EUR_hrl_grassland": 2018,
    }
    grassland_check = []

    for location in locations:
        if "lat" in location and "lon" in location:
            site_check = copy.deepcopy(location)
            site_check.update(map_year=map_years[map_key], map_key=map_key)

            if map_key in deims_keys:
                if "deims_id" in site_check:
                    if site_check["found"]:
                        # Get DEIMS centroid coordinates, can differ from original location coordinates
                        deims_info = ut.get_deims_coordinates(site_check["deims_id"])
                        map_source = (
                            f"https://deims.org/api/sites/{site_check['deims_id']}"
                        )
                        all_categories, time_stamp = get_category_deims(site_check)
                        is_grass = False

                        for category in all_categories:
                            is_grass = check_if_grassland(category, site_check, map_key)

                            if is_grass:
                                break

                        site_check.update(
                            lat=deims_info["lat"],  # replace with deims coord.
                            lon=deims_info["lon"],  # replace with deims coord.
                            map_source=map_source,
                            map_query_time_stamp=time_stamp,
                            is_grass=is_grass,
                            category=all_categories,
                        )

                        # Print check results
                        if is_grass:
                            print(
                                "Confirmed: Site with centroid Lat.",
                                f"{site_check['lat']}, Lon. {site_check['lon']}",
                                "contains habitat classified as grassland",
                                f"according to '{map_key}' land cover classification.",
                            )
                        else:
                            print(
                                "Not in target categories: Site with centroid Lat.",
                                f"{site_check['lat']}, Lon. {site_check['lon']}",
                                "contains no habitat classified as grassland",
                                f"according to '{map_key}' land cover classification.",
                            )
                    else:
                        print(
                            f"Error: Valid DEIMS.iD needed with map key '{map_key}' for requesting land cover information!"
                        )
                        print("Habitat: Not found.")
                        site_check.update(
                            map_source="",
                            map_query_time_stamp="",
                            is_grass="",
                            category="Not found.",
                        )
                else:
                    raise ValueError(
                        f"No location DEIMS.iD provided, but needed with map key '{map_key}' for requesting land cover information!"
                    )

                grassland_check.append(site_check)
            elif map_key in tif_keys:
                map_file, category_mapping = get_map_and_legend(map_key)
                category, time_stamp = get_category_tif(
                    map_file, category_mapping, site_check
                )
                is_grass = check_if_grassland(category, site_check, map_key)
                site_check.update(
                    map_source=map_file,
                    map_query_time_stamp=time_stamp,
                    is_grass=is_grass,
                    category=category,
                )
                grassland_check.append(site_check)
            elif map_key in hrl_keys:
                map_source = "https://image.discomap.eea.europa.eu/arcgis/rest/services/GioLandPublic/HRL_Grassland_2018/ImageServer"
                category, time_stamp = get_category_hrl_grassland(site_check)
                is_grass = check_if_grassland(category, site_check, map_key)
                site_check.update(
                    map_source=map_source,
                    map_query_time_stamp=time_stamp,
                    is_grass=is_grass,
                    category=category,
                )
                grassland_check.append(site_check)
            else:
                raise ValueError(
                    f"Map key '{map_key}' not found. Please provide valid map key!"
                )
        else:
            raise ValueError(
                "Location not correctly defined. Please provide as dictionary containing ({'lat': float, 'lon': float})!"
            )

    # Save results to file
    if file_name:
        check_results_to_file(grassland_check, file_name=file_name, map_key=map_key)

    return grassland_check


def main():
    """
    Runs the script with default arguments for calling the script.
    """
    parser = argparse.ArgumentParser(
        description="Set default arguments for calling the script."
    )

    # Define command-line arguments
    parser.add_argument(
        "--locations",
        type=ut.parse_locations,
        help="List of location dictionaries containing coordinates ('lat', 'lon') or DEIMS IDs ('deims_id')",
    )
    parser.add_argument(
        "--map_key",
        type=str,
        default="GER_Preidl",
        choices=[
            "EUR_eunis_habitat",
            "EUR_hrl_grassland",
            "EUR_Pflugmacher",
            "GER_Preidl",
            "GER_Schwieder_2017",
            "GER_Schwieder_2018",
            "GER_Schwieder_2019",
            "GER_Schwieder_2020",
            "GER_Schwieder_2021",
            "GER_Lange_2017",
            "GER_Lange_2018",
        ],
        help="""Options: 
        'EUR_eunis_habitat', 
        'EUR_hrl_grassland', 
        'EUR_Pflugmacher', 
        'GER_Preidl', 
        'GER_Schwieder_2017', 
        'GER_Schwieder_2018', 
        'GER_Schwieder_2019', 
        'GER_Schwieder_2020', 
        'GER_Schwieder_2021',
        'GER_Lange_2017',
        'GER_Lange_2018'.""",
    )
    parser.add_argument(
        "--file_name",
        default=None,
        help="File name to save results",
    )
    args = parser.parse_args()

    # Example coordinates
    if args.locations is None:
        # Example to get coordinates from DEIMS.iDs from XLS file
        file_name = ut.get_package_root() / "grasslandSites" / "_elter_call_sites.xlsx"
        country_code = "DE"  # "DE" "AT"
        sites_ids = ut.get_deims_ids_from_xls(
            file_name, header_row=1, country=country_code
        )
        args.locations = []

        for deims_id in sites_ids:
            location = ut.get_deims_coordinates(deims_id)

            if location["found"]:
                args.locations.append(location)

        args.file_name = ut.add_string_to_file_name(
            file_name,
            f"_{country_code}__grasslandCheck_{args.map_key}",
            new_suffix=".txt",  # ".txt" or ".xlsx"
        )

        # # Example coordinates for checking without DEIMS.iDs
        # args.locations = [
        #     {"lat": 51.390427, "lon": 11.876855},  # GER, GCEF grassland site
        #     {
        #         "lat": 51.3919,
        #         "lon": 11.8787,
        #     },  # GER, GCEF grassland site, centroid, non-grassland in HRL!
        #     {"lat": 51.3521825, "lon": 12.4289394},  # GER, UFZ Leipzig
        #     {"lat": 51.4429008, "lon": 12.3409231},  # GER, Schladitzer See, lake
        #     {"lat": 51.3130786, "lon": 12.3551142},  # GER, Auwald, forest within city
        #     {"lat": 51.7123725, "lon": 12.5833917},  # GER, forest outside of city
        #     {"lat": 46.8710811, "lon": 11.0244728},  # AT, should be grassland
        #     {"lat": 64.2304403, "lon": 27.6856269},  # FIN, near LUMI site
        #     {"lat": 64.2318989, "lon": 27.6952722},  # FIN, LUMI site
        #     {"lat": 49.8366436, "lon": 18.1540575},  # CZ, near IT4I Ostrava
        #     {"lat": 43.173, "lon": 8.467},  # Mediterranean Sea
        #     {"lat": 30, "lon": 1},  # out of Europe
        # ]

    # Default file name will be used as no file name is passed here, return argument not needed here
    check_locations_for_grassland(
        locations=args.locations, map_key=args.map_key, file_name=args.file_name
    )


# Execute main function when the script is run directly
if __name__ == "__main__":
    main()
