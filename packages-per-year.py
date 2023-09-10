import sys
import csv
import matplotlib.pyplot as plt

# Initialize a dictionary to store the counts per year
yearly_counts = {}

# Open the CSV file
with open(sys.argv[1], 'r') as csv_file:
    csv_reader = csv.reader(csv_file)
    
    # Loop through each row in the CSV file
    for row in csv_reader:
        # Extract the year from the fourth column (index 3)
        year = row[3].split('-')[0]
        
        # Update the count for the year in the dictionary
        yearly_counts[year] = yearly_counts.get(year, 0) + 1

# Convert the dictionary to lists for plotting
years = sorted(list(yearly_counts.keys()))
package_counts = list(yearly_counts.values())

# Create a plot
plt.figure(figsize=(10, 6))
plt.bar(years, package_counts, color='skyblue')
plt.xlabel('Year')
plt.ylabel('Number of Packages Produced')
plt.title('Packages Produced per Year')
plt.xticks(rotation=45)
plt.tight_layout()

# Save the plot
plt.savefig(sys.stdout.buffer, format='svg')
