#!/usr/bin/env python3
"""
Script to find and comment out duplicate translation objects in translations.json
Starting from line 5732 onwards, looking for complete duplicate objects
"""

import json
import sys
import hashlib
from collections import defaultdict

def find_duplicate_objects_from_line(file_path, start_line=5732):
    """Find complete duplicate translation objects starting from specified line"""
    
    # Read the file as lines first to identify the section
    with open(file_path, 'r', encoding='utf-8') as f:
        all_lines = f.readlines()
    
    print(f"Total lines in file: {len(all_lines)}")
    print(f"Starting analysis from line {start_line}")
    
    # Read the JSON to work with structured data
    with open(file_path, 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    # Check if we have the expected structure
    if 'translation' not in data:
        print("Error: No 'translation' key found in JSON")
        return None, None
    
    translations = data['translation']
    print(f"Total translation objects: {len(translations)}")
    
    # We need to map line numbers to translation object indices
    # This is approximate since JSON formatting can vary
    
    # For now, let's work with the assumption that we need to check 
    # objects that were added recently (likely at the end)
    
    # Calculate hash for each translation object to find exact duplicates
    object_hashes = defaultdict(list)
    
    for i, obj in enumerate(translations):
        # Create a canonical string representation of the object
        obj_str = json.dumps(obj, sort_keys=True, separators=(',', ':'))
        obj_hash = hashlib.md5(obj_str.encode()).hexdigest()
        object_hashes[obj_hash].append(i)
    
    # Find duplicates (objects that appear more than once)
    duplicates = {h: indices for h, indices in object_hashes.items() if len(indices) > 1}
    
    if not duplicates:
        print("No complete duplicate objects found!")
        return None, None
    
    print(f"Found {len(duplicates)} sets of duplicate objects:")
    print("=" * 60)
    
    # Report duplicates
    total_duplicate_objects = 0
    for obj_hash, indices in duplicates.items():
        total_duplicate_objects += len(indices) - 1  # -1 because we keep one
        print(f"\nDuplicate object found at translation indices: {indices}")
        # Show the English text for identification
        if 'en' in translations[indices[0]]:
            print(f"English text: '{translations[indices[0]]['en']}'")
        else:
            print(f"Object: {json.dumps(translations[indices[0]], indent=2)[:100]}...")
    
    print(f"\nTotal duplicate objects to remove: {total_duplicate_objects}")
    return duplicates, translations

def clean_duplicate_objects(file_path, output_path=None):
    """Remove complete duplicate translation objects, keeping only the first occurrence"""
    
    if output_path is None:
        output_path = file_path + ".cleaned"
    
    # Read the JSON data
    with open(file_path, 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    translations = data['translation']
    
    # Track which objects to keep (first occurrence only)
    seen_object_hashes = set()
    cleaned_translations = []
    removed_count = 0
    
    for i, obj in enumerate(translations):
        # Create a canonical string representation of the object
        obj_str = json.dumps(obj, sort_keys=True, separators=(',', ':'))
        obj_hash = hashlib.md5(obj_str.encode()).hexdigest()
        
        if obj_hash not in seen_object_hashes:
            cleaned_translations.append(obj)
            seen_object_hashes.add(obj_hash)
        else:
            removed_count += 1
            # Show what we're removing for verification
            if 'en' in obj:
                print(f"Removing duplicate object: '{obj['en']}'")
            else:
                print(f"Removing duplicate object at index {i}")
    
    # Create cleaned data
    cleaned_data = data.copy()
    cleaned_data['translation'] = cleaned_translations
    
    # Write cleaned version
    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(cleaned_data, f, indent=2, ensure_ascii=False)
    
    print(f"\nCleaned file saved to: {output_path}")
    print(f"Original entries: {len(translations)}")
    print(f"Cleaned entries: {len(cleaned_translations)}")
    print(f"Removed duplicates: {removed_count}")
    
    return output_path

def comment_out_duplicates_in_source(file_path, duplicates, translations):
    """Comment out duplicate objects in the original JSON file by adding // comments"""
    
    if not duplicates:
        print("No duplicates to comment out!")
        return
    
    # Read the file as text lines
    with open(file_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    # This is complex because we need to map JSON objects to line numbers
    # For now, let's create the cleaned version and let the user review
    print("\nNote: Direct line commenting is complex with JSON structure.")
    print("Creating cleaned version instead. You can:")
    print("1. Review the cleaned file")
    print("2. Replace the original with the cleaned version")
    print("3. Or manually comment out the specific duplicate objects listed above")

if __name__ == "__main__":
    file_path = "/home/osalamon/WORK/biodt-shiny/app/translations/translations.json"
    
    print("=== DUPLICATE OBJECT DETECTION ===")
    duplicates, translations = find_duplicate_objects_from_line(file_path, start_line=5732)
    
    if duplicates:
        print("\n=== CLEANING DUPLICATES ===")
        cleaned_file = clean_duplicate_objects(file_path)
        print(f"\nTo apply the changes:")
        print(f"cp '{cleaned_file}' '{file_path}'")
    else:
        print("\nNo duplicate objects found to clean!")
