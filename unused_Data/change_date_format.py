

# read CHINA_UNEMP.csv
with open('CHINA_UNEMP.csv', 'r') as f:
    lines = f.readlines()

# change date format from DD-MM-YYYY to YYYY-MM-DD
new_lines = []
for line in lines:
    date = line.split(',')[0]
    new_date = '-'.join(date.split('-')[::-1])
    new_line = line.replace(date, new_date)
    new_lines.append(new_line)
# write to CHINA_UNEMP_new.csv
with open('CHINA_UNEMP_new.csv', 'w') as f:
    f.writelines(new_lines)


