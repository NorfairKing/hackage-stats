import sys
import csv
import matplotlib.pyplot as plt
from collections import defaultdict

# Initialize a dictionary to store the number of unique packages per maintainer
maintainer_packages = defaultdict(set)

# Open the CSV file
with open(sys.argv[1], 'r') as csv_file:
    csv_reader = csv.reader(csv_file)
    
    # Loop through each row in the CSV file
    for row in csv_reader:
        package = row[0]
        maintainer = row[2]
        
        # Add the package to the maintainer's set
        maintainer_packages[maintainer].add(package)

# Convert sets to counts
maintainer_counts = {k: len(v) for k, v in maintainer_packages.items()}

# Sort the maintainers by the number of unique packages in descending order
top_maintainers = sorted(maintainer_counts.items(), key=lambda x: x[1], reverse=True)[:25]

# Extract the names and counts of the top maintainers
maintainers = [x[0] for x in top_maintainers]
package_counts = [x[1] for x in top_maintainers]

# Create a bar plot
plt.figure(figsize=(12, 6))
bars = plt.barh(maintainers, package_counts, color='skyblue')
plt.xlabel('Number of Unique Packages Maintained')
plt.ylabel('Maintainer')
plt.title('Number of Unique Packages per Maintainer (Top 25)')
plt.gca().invert_yaxis()  # Invert the y-axis for readability
plt.tight_layout()

for bar, package_count in zip(bars, package_counts):
    plt.text(bar.get_width() + 0.1, bar.get_y() + bar.get_height() / 2, str(package_count), ha='left', va='center')

# Save the plot
plt.savefig(sys.stdout.buffer, format='svg')
