import csv

with open('./Inflacja.csv', 'r', encoding='utf-8') as file:
    writer = csv.DictWriter(
        open('./Inflacja_parsed.csv', 'w', newline=''),
        fieldnames=['opis_okres', 'wartosc'],
        delimiter=',',
    )
    writer.writeheader()
    for row in file.readlines():
        year, values = row.split('|', 1)
        for i, value in enumerate(values.split(), start=1):
            if value:
                writer.writerow(
                    {
                        'opis_okres': f"{year} M{i:02}",
                        'wartosc': float(value.replace(',', '.'))
                    }
                )
