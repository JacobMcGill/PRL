import pandas as pd

def filter_dates(INPUT):
    # Read the input file, assuming headers are in the 6th row (index 5 in Python)
    df = pd.read_csv(INPUT, header=4)

    # Extract the relevant columns
    df['Outage Start'] = pd.to_datetime(df['Outage Start'], errors='coerce')
    df['Outage End'] = pd.to_datetime(df['Outage End'], errors='coerce')

    # Drop rows with invalid datetime values
    df = df.dropna(subset=['Outage Start', 'Outage End'])

    # Filter for Fuel Type containing "coal", "gas", "lignite", or "wood"
    df = df[df['Fuel Type'].str.contains('coal|gas|lignite|wood', case=False, na=False)]

    # Sort rows
    df = df.sort_values(by=['Outage Start', 'Outage End'], ascending=[False, True]).reset_index(drop=True)

    # Define the dates of interest
    dates_of_interest = [
        '2023-06-20', '2023-08-10', '2023-08-17', '2023-08-24',
        '2023-08-25', '2023-08-26', '2023-08-30',
        '2023-09-06', '2023-09-07', '2023-09-08'
    ]
    dates_of_interest = pd.to_datetime(dates_of_interest)

    # Drop rows with missing values in 'Resource Name' and 'Outage Start'
    df = df.dropna(subset=['Resource Name', 'Outage Start']).reset_index(drop=True)

    # Create the clean1 column
    df['clean1'] = 0
    counter = 0
    for i in range(len(df)):
        if i > 0 and (
            df.iloc[i]['Resource Name'] == df.iloc[i - 1]['Resource Name'] and
            df.iloc[i]['Outage Start'] == df.iloc[i - 1]['Outage Start']
        ):
            df.loc[i, 'clean1'] = counter
        else:
            counter += 1
            df.loc[i, 'clean1'] = counter

    # Create the clean2 column
    df['clean2'] = False
    for i in range(len(df)):
        df.loc[i, 'clean2'] = (
            (i > 0 and df.iloc[i]['clean1'] == df.iloc[i - 1]['clean1']) or
            (i < len(df) - 1 and df.iloc[i]['clean1'] == df.iloc[i + 1]['clean1'])
        )

    # Create the corrected_Outage Start column
    df['corrected_Outage Start'] = df['Outage Start']
    for i in range(1, len(df)):
        if df.iloc[i]['clean2'] and df.iloc[i]['Resource Name'] == df.iloc[i - 1]['Resource Name']:
            df.loc[i, 'corrected_Outage Start'] = df.iloc[i - 1]['Outage End']

    # Generate a corrected2_Outage Start column
    df['corrected2_Outage Start'] = df['corrected_Outage Start']
    for i in range(1, len(df)):
        if df.iloc[i]['clean2'] and df.iloc[i]['Resource Name'] == df.iloc[i - 1]['Resource Name']:
            # Push the date to the first minute of the next day
            df.loc[i, 'corrected2_Outage Start'] = (df.iloc[i - 1]['Outage End'] + pd.Timedelta(days=1)).normalize()
    

    # Generate a column for each date of interest with a bool indicating if there was an outage during that time
    for date in dates_of_interest:
        date_str = date.strftime('%Y-%m-%d')
        df[date_str] = (
            (df['corrected2_Outage Start'].dt.floor('D') <= date) &
            (df['Outage End'].dt.floor('D') >= date)
        )

    # Comment out the aggregation steps
    # aggregate_data = []
    # for date in dates_of_interest:
    #     date_str = date.strftime('%Y-%m-%d')
    #     daily_sum = df.loc[df[date_str].fillna(False), 'Reduction MW'].sum()
    #     aggregate_data.append({'Date': date_str, 'Daily Outage MW': daily_sum})

    # aggregate_df = pd.DataFrame(aggregate_data)

    # Save the aggregate table to a CSV file
    # aggregate_df.to_csv('artifacts/aggregate_outages.csv', index=False)

    # Calculate the average daily outages based on the aggregate_df
    # average_daily_outage = aggregate_df['Daily Outage MW'].mean()

    # Output the filtered DataFrame to a CSV file
    filtered_df = df[
        df['Outage Start'].dt.floor('D').isin(dates_of_interest) |
        df['Outage End'].dt.floor('D').isin(dates_of_interest)
    ]
    filtered_df.to_csv('artifacts/filtered_outages.csv', index=False)

    return df  # Return the full DataFrame for inspection

if __name__ == "__main__":
    INPUT = '../data/NRGSTREAM_ERC_UnplannedOutages_06.01.2023-10.01.2023.csv'
    df = filter_dates(INPUT)
    print("Filtered DataFrame with corrected_Outage Start saved.")
