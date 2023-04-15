
import math

import csv
from datetime import datetime

# Define the input and output file names
input_file = "data.csv"
output_file = "output.csv"

# Define a dictionary to hold the quarterly data
quarterly_data = {}

# Define a function to convert a date string to a quarter string
def date_to_quarter(date_str):
    date = datetime.strptime(date_str, "%Y-%m-%d")
    quarter = (date.month - 1) // 3 + 1
    year = date.year
    return f"{year}Q{quarter}"

# Read the input CSV file
# with open(input_file, newline="") as f:
#     reader = csv.reader(f)
#     for row in reader:
#         date_str, value = row
#         quarter_str = date_to_quarter(date_str)
#         if quarter_str not in quarterly_data:
#             quarterly_data[quarter_str] = 0
#         quarterly_data[quarter_str] += int(value)


# COUNT_KEYS = ["CHINA", "EURO", "USA", "JAP"]
COUNT_KEYS = ["CHINA"]
METRIC_KEYS = ["CPI", "GDP", "INT", "UNEMP", "SPENDING_PERCENT_OF_GDP"]

import pandas as pd

country_metrics = { x : {}  for x in COUNT_KEYS} 
print(f"{country_metrics=}")

quarterly_data = { x : {} for x in COUNT_KEYS}
quarterly_data



def quarter_avg(quarter_arr):
    if(len(quarter_arr) == 1):
      return float(quarter_arr[0])
    else:
      sum = 0.0
      for x in quarter_arr:
        sum+=x
      sum = float(float(sum) / float(len(quarter_arr)))
    return sum
      

def process_2_quarterly_data(df : pd.DataFrame) -> list:
    quarter_arrs = {}
    cols = df.columns
    # print(f"{cols=}")
    prev_row_date = ""
    for index, row in df.iterrows():
      date = row[cols[0]]
      val = row[cols[1]]
      # print(f"{date=}")
      # quarter = ""
      try:
        quarter_str = date_to_quarter(str(date))
        if quarter_str not in quarterly_data:
          quarter_arrs[quarter_str] = [val]
        else:
          quarter_arrs[quarter_str].append(val)
                  # average all the quarters 

      except ValueError as e:
        print("Error adapting " + str(date)  + " to datetime")
        year = str(date)
        year = year.replace("*", "")
        if(year.find(".") != -1):
          year = year[:year.find(".")]
        if(int(year) > 2023):
          continue
        # print(f"{year=}")
        for quarter in range(1, 5,1):
          quarter_str = f"{year}Q{quarter}"
        #   print(quarter_str)
          quarter_arrs[quarter_str] = [val]
        #   print(f"{quarter_arrs[quarter_str]=}")

    for quarter_str in quarter_arrs.keys():
        # print(f"{quarter_str=}")
        # print(f"{quarter_arrs[quarter_str]=}"
        quarter_arr = quarter_arrs[quarter_str]
        quarter_arr = quarter_avg(quarter_arr)
        quarter_arrs[quarter_str] = float(quarter_arr)
        # print(f"{quarter_arrs=}")
    return pd.DataFrame.from_dict(quarter_arrs, orient='index')

# # process_2_quarterly_data(country_metrics["CHINA"]["CPI"])


def get_quarters_between_dates(start_date, end_date):
    # Convert date strings to datetime objects
    start_date = datetime.strptime(start_date, '%YQ%m')
    end_date = datetime.strptime(end_date, '%YQ%m')

    # Calculate number of quarters between dates
    quarters = (end_date.year - start_date.year) * 4 + (end_date.month - start_date.month) // 3 + 1

    return quarters


for country in COUNT_KEYS:
  for metric in METRIC_KEYS:
    filename = f"./Data/{country}/{country}_{metric}.csv"
    try:  
      df = pd.read_csv (filename)
      print(f"{df=}")
      print(f"{df.columns=}")
      data_col = df.columns[1]
      print(f"{df.columns[1]=}")
      # print(f"{df[data_col]=}")
      country_metrics[country][metric] = df
      quarterly_data[country][metric] = process_2_quarterly_data(df)

      print("Printing dict for quarterly data")

      print(quarterly_data[country][metric])
      df = quarterly_data[country][metric]
      df.to_csv(f"./Data/{country}/quarters/{country}-{metric}-quarters.csv", encoding='utf-8')
    #   break

    except FileNotFoundError:
      print(f"Could not find {filename}")


# print("done")
# # # quarterly_data["USA"]["UNEMP"], quarterly_data["USA"]["INT"], quarterly_data["USA"]["GDP"]

for country in COUNT_KEYS:
#   if(country == "CHINA"):
#      continue
  
  # minimum length of metrics for any metric
  min_met_indx = 100000000
  last_quarters = []
  first_quarters = []

  for metric in METRIC_KEYS:
      print(f"{country=}")
      print(f"{metric=}")
    #   print(f"{quarterly_data[country][metric].shape[0]=}")
    #   print(f"{quarterly_data[country][metric].values.tolist()=}")
    #   print(f"{quarterly_data[country][metric].index=}")
    #   print(f"{dir(quarterly_data[country][metric])=}")
      first_df_quarter = min(quarterly_data[country][metric].index)
      last_df_quarter = max(quarterly_data[country][metric].index)
    #   print(f"{first_df_quarter=}")
    #   print(f"{last_df_quarter=}")
    #   len_met = len(quarterly_data[country][metric].values.tolist())
      # frames.append(quarterly_data[country][metric])
      # csv_df = pd.DataFrame

      first_quarters.append(first_df_quarter)
      last_quarters.append(last_df_quarter) 
 
  print(f"{last_quarters=}")
  print(f"{first_quarters=}")
  # lowest upper bound (supremum)
#   print(f"{min(last_quarters)=}")
  lub = min(last_quarters)
  lub = pd.to_datetime(lub)
#   print(f"{lub=}")
#   greatest lower bound (infimum)
#   print(f"{max(first_quarters)=}")
  glb = max(first_quarters)
  glb = pd.to_datetime(glb)
  print(f"{glb=}")
  # check if every metric's lengths match
#   assert
  df = pd.DataFrame()
  #   first_length = 0
  for i, metric in enumerate(METRIC_KEYS):
      try:
          
          
          met_list = quarterly_data[country][metric].values.tolist()
        #   print(f"{met_list=}")
          indices = quarterly_data[country][metric].index.tolist()
  
        #   assert glb in indices
        #   assert lub in indices
        #   least_indx = indices.index(glb)
        #   most_indx = indices.index(lub)
  
          
        #   bnd_met_list = met_list[least_indx : most_indx]
        #   bnd_indices =  indices[least_indx : most_indx]
  
        #   assert len(bnd_indices) == len(bnd_met_list)
  
        #   assert glb == indices[least_indx]
        #   assert lub == indices[most_indx]

          quart_df = quarterly_data[country][metric]
          df_bounded = quart_df.loc[(pd.to_datetime(quart_df.index) >= glb) & (pd.to_datetime(quart_df.index) <= lub)]
          vals = [x[0] for x in df_bounded.values.tolist()]
          df[metric] = vals
          df["quarter"] = df_bounded.index.tolist()
          print(f"{df_bounded=}")

          
        #   assert len(bnd_indices) == quarter_diff + 1
  
  
  #         print(f"{type(least_indx)=}")
  #         print(f"{least_indx=}")
  
#   #         try:
#   #          bnd_indices[0] == glb
#   #          bnd_indices[-1] == lub
#   #         except:
#   #            continue
#   #         bnd_met_list = [x[0] for x in met_list]
  
#   #         # print(f"{metric=}")
#   #         # if i == 0:
#   #         #    first_length = len(bnd_met_list)
#   #         # else:
#   #         #    assert len(bnd_met_list) == first_length
#   #         #    continue
#   #         # assert glb == indices[least_indx]
#   #         # assert lub == indices[most_indx]
#   #         # assert least_indx != -1
#   #         # assert most_indx != -1
  
#   #         # try:
#   #         print(f"{len(bnd_met_list)=}")
#   #         # if(metric == "CPI"):
#   #         #    raise RuntimeError
#   #         #    break
#   #         # df[metric]= met_lists
#   #         # except ValueError as e:
  
      except KeyError:  
          print(f"Key {metric} not found for {country}")
          # print(f"{len(met_list)}")
          # print(f"{type(met_list)}")
          print(f"{df=}")
      

    
      df.to_csv(f"./{country}-bounded.csv", encoding='utf-8')
    
# #   result = pd.concat(frames)
# #   print(f"{result=}")


# # # import pandas as pd
# # # import matplotlib.pyplot as plt
# # # sample_df = quarterly_data["USA"]["UNEMP"]

# # # # sample_df.plot(x='year', y='sales', kind='line')
# # # # sample_df.plot(x='', y='0', kind='line')
# # # plt.show()

