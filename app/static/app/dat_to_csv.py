import csv

# Convert station.dat file to station.csv file
station_dat_file_path = "./station.dat"
station_csv_file_path = "./station.csv"
numOfAttributes = 9

with open(station_dat_file_path) as station_dat_file, open(station_csv_file_path, 'w') as station_csv_file:
    csv_writer = csv.writer(station_csv_file)

    for line in station_dat_file:
        row = [field.strip() for field in line.split()]
        if len(row) == numOfAttributes:
            csv_writer.writerow(row)

