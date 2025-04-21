import os
import json
import csv

# Directory containing the JSON files
base_dir = '/home/fhetest/logs/test-0528110721/fail'
csv_file = '/home/fhetest/evaluation/output-rq1-a-filterOn-int.csv'

# Function to read JSON files from the directory
def read_json_files(base_dir):
    json_data = []
    for root, _, files in os.walk(base_dir):
        for file in files:
            if file.endswith('.json'):
                file_path = os.path.join(root, file)
                try:
                    with open(file_path, 'r') as f:
                        data = json.load(f)
                        json_data.append(data)
                except json.JSONDecodeError as e:
                    print(f"Error decoding JSON in file {file_path}: {e}")
    return json_data

# Function to update the CSV file
def update_csv_file(json_data, csv_file):
    # Read existing data from the CSV file
    csv_data = {}
    if os.path.exists(csv_file):
        with open(csv_file, mode='r', newline='') as f:
            reader = csv.DictReader(f)
            for row in reader:
                csv_data[row['programId']] = row
    
    # Update with new JSON data
    for entry in json_data:
        program_id = entry['programId']
        openfhe = None
        for result in entry['failures']:
            if result['library'] == 'OpenFHE':
                openfhe = result['failedResult']
        
        if program_id in csv_data:
            if openfhe is not None:
                csv_data[program_id]['OpenFHE'] = openfhe
        else:
            csv_data[program_id] = {'programId': program_id, 'OpenFHE': openfhe}
    
    # Write updated data back to the CSV file
    with open(csv_file, mode='w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=['programId','OpenFHE'])
        writer.writeheader()
        for row in csv_data.values():
            writer.writerow(row)

# Main execution
if __name__ == '__main__':
    json_data = read_json_files(base_dir)
    update_csv_file(json_data, csv_file)
