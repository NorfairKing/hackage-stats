import sys
import csv
import matplotlib.pyplot as plt
from collections import defaultdict

# Initialize a dictionary to store the number of packages per maintainer
maintainer_counts = defaultdict(int)
last_upload_dates = {}

# Open the CSV file
with open(sys.argv[1], 'r') as csv_file:
    csv_reader = csv.reader(csv_file)
    
    # Loop through each row in the CSV file
    for row in csv_reader:
        maintainer = row[2]
        package_date = row[3]
        
        # Update the count for the maintainer in the dictionary
        maintainer_counts[maintainer] += 1
        
        # Update the last upload date for the maintainer
        if maintainer not in last_upload_dates or package_date > last_upload_dates[maintainer]:
            last_upload_dates[maintainer] = package_date


# Sort the maintainers by the number of packages in descending order
top_maintainers = sorted(maintainer_counts.items(), key=lambda x: x[1], reverse=True)[:100]

# Extract the names and counts of the top maintainers
maintainers = [x[0] for x in top_maintainers]
package_counts = [x[1] for x in top_maintainers]
last_upload_dates = [last_upload_dates[maintainer] for maintainer in maintainers]


# Create a bar plot
plt.figure(figsize=(12, 6))
bars = plt.barh(maintainers, package_counts, color='skyblue')
plt.xlabel('Number of Packages Produced')
plt.ylabel('Maintainer')
plt.title('Number of Packages per Maintainer (Top 100)')
plt.gca().invert_yaxis()  # Invert the y-axis for readability
plt.tight_layout()

for bar, last_date in zip(bars, last_upload_dates):
    plt.text(bar.get_width() + 1, bar.get_y() + bar.get_height() / 2, last_date, ha='left', va='center')


# Save the plot
plt.savefig(sys.stdout.buffer, format='svg')

