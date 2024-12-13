import os
import time
import pandas as pd
import numpy as np


def expand_to_hourly(df):
    # Ensure 'Date/Time' is in datetime format and normalize to midnight
    df['Date/Time'] = pd.to_datetime(df['Date/Time']).dt.normalize()

    # Generate a timedelta series for hours 0 to 23
    hourly_offsets = pd.to_timedelta(range(24), unit='H')
    
    # Repeat each row 24 times and reset index
    expanded_df = df.loc[df.index.repeat(24)].reset_index(drop=True)
    
    # Use numpy.tile to repeat the hourly offsets for each original row
    expanded_df['Date/Time'] += np.tile(hourly_offsets, len(df))
    
    return expanded_df

def fill_missing_days(df, type):
    df['Date/Time'] = pd.to_datetime(df['Date/Time'], format='%m/%d/%y') if type == 'day' else pd.to_datetime(df['Date/Time'])
    df = df.drop_duplicates(subset=['Date/Time'])  # Remove duplicate values

    freq = 'D' if type == 'day' else 'H'
    full_date_range = pd.date_range(df['Date/Time'].min(), df['Date/Time'].max(), freq=freq)

    df = df.set_index('Date/Time').reindex(full_date_range).rename_axis('Date/Time').reset_index()
    df = df.ffill()

    return expand_to_hourly(df) if type == 'day' else df


def concat_files(INPUT_PATH):
    var_data = {}  # Dictionary to hold DataFrames for each variable

    # Step 1: Collect all unique variable names
    var_list = [file.split(" ")[1].split("_")[0] if file.startswith('NRG') else file.split("_")[0] + "_" + file.split("_")[1]
                for file in os.listdir(INPUT_PATH) if file not in ['.DS_Store']]
    var_list = list(set(var_list))  # Remove duplicates

    # Step 2: Process each variable and concatenate files with different dates
    for var in var_list:
        var_files = [file for file in os.listdir(INPUT_PATH) if var in file]
        dfs = []

        for file in var_files:
            df = pd.read_csv(os.path.join(INPUT_PATH, file), skiprows=5)
            if 'Day' in df.columns:
                df = df.rename(columns={'Day': 'Date/Time'})
                expanded_df = fill_missing_days(df, 'day')
                
                # Apply the cutoff for Henry_Hub
                cutoff_datetime = pd.to_datetime('12/26/2017 23:00')
                if var == 'Henry_Hub':
                    expanded_df = expanded_df[expanded_df['Date/Time'] > cutoff_datetime]

                dfs.append(expanded_df)

            elif 'Date/Time' in df.columns:
                dfs.append(fill_missing_days(df, 'day') if var == 'Henry_Hub' else fill_missing_days(df, 'hour'))

            else:
                print(f"Warning: 'Date/Time' column not found in {file}")

        if dfs:
            var_data[var] = pd.concat(dfs, ignore_index=True)

    # Step 3: Create a full hourly Date/Time range for merging
    all_datetimes = pd.date_range(start=pd.to_datetime('12/27/2017 00:00'), end=pd.to_datetime('10/29/2024 23:00'), freq='H')
    reference_df = pd.DataFrame(all_datetimes, columns=['Date/Time']).set_index('Date/Time')

    # Step 4: Outer merge each variable's DataFrame with the reference Date/Time index
    merged_df = reference_df
    for var, df in var_data.items():
        if df.columns.size > 1:
            df = df.rename(columns={df.columns[1]: f"{var}_{df.columns[1]}"})
        df = df.set_index('Date/Time')
        merged_df = merged_df.join(df, how='outer')

    # Reset index to bring 'Date/Time' back as a column, sort, and drop duplicates
    merged_df = merged_df.reset_index().sort_values(by='Date/Time').drop_duplicates(subset='Date/Time')

    return merged_df



if __name__ == "__main__":
    INPUT_PATH = '../data'
    timestr = time.strftime("%m.%d")

    BASE_DIR = "artifacts"
    FILENAME = "var_table" + timestr + '.csv'
    FILENAME_STATA = "var_table" + timestr + '.dta'
    OUTPUT_PATH = os.path.join(BASE_DIR, FILENAME)
    OUTPUT_PATH_STATA = os.path.join(BASE_DIR, FILENAME_STATA)

    os.makedirs(BASE_DIR, exist_ok=True)

    combined_df = concat_files(INPUT_PATH)
    combined_df.to_csv(OUTPUT_PATH, index=False)
    combined_df.to_stata(OUTPUT_PATH_STATA, write_index=False)
    
