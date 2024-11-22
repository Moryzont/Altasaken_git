 # Import necessary libraries
import os
import numpy as np
import pandas as pd
from tqdm import tqdm
import warnings
import re

import torch
from transformers import pipeline

# Suppress specific UserWarnings from torch DataLoader
warnings.filterwarnings("ignore", message=".*Length of IterableDataset.*", category=UserWarning)

# Set the working directory manually
working_dir = '/Users/vetlewisloffsandring/Documents/Altasaken/Testing'  # Replace with your directory path
os.chdir(working_dir)

# Check if MPS (Apple Silicon GPU) is available
if torch.backends.mps.is_available():
    device = torch.device("mps")
    print("Using MPS (GPU) device")
else:
    device = torch.device("cpu")
    print("Using CPU device")

# Initialize the zero-shot classification pipeline
classifier = pipeline(
    "zero-shot-classification",
    model='alexandrainst/scandi-nli-large',
    device=device,
)

# Norwegian hypothesis template
hypothesis_template = "Dette eksempelet handler om {}."

# Candidate labels
candidate_labels = ["samesaken", "Alta-samfunnet er i mot utbygging", "protestbevegelse", "at et møte blir arrangert",
"tv-program", "produksjon av film", "partipolitikk", "kommunestyret", "domstoler", "regjeringen",
"myndigheter", "demonstranter", "politiet", "saksbehandling og juridiske prosesser", "bygging av vei og anleggsvirksomhet",
"undersøkelser og utredning", "arrestasjoner og bruk av tvangsmidler", "økonomisk og næringsmessig nytte",
"må ha kraft og strøm", "konflikt mellom samer og nordmenn", "rettigheter til land og ressurser",
"natur og miljø", "vern av vassdrag", "trussel mot samisk kultur og næring", "trussel mot laksefiske",
"Alta-kautokeinovassdraget bør bygges ut", "samiske særrettigheter", "utsatt og usikkerhet",
"unødvendig", "splittelse og uenighet", "vanskelig å snakke om",
"urettferdig", "opprivende", "forsoning", "samhold og enighet",
"handlekraft og beslutninger", "tydelig og bestemtrettsak",
"leiren i Stilla", "sultestreik", "politiaksjonen", "stortingets vedtak",
"opprettelsen av Sametinget", "Finnmarksloven", "udemokratiske protester", "sivil ulydighet",
"demonstrantene opptrer dårlig", "ikke-voldelige", "mediedekning", "historisk betydning",
"kunst og kultur", "husker eller minnes", "feilinformasjon", "Konstruktiv sivil ulydighet",
"Manglende utredning om konsekvenser", "skeptisk kraftprognose-motstand", "meningsløs kraftutbygging",
"Skeptisk til ulydighet", "lov og orden", "overdrevet maktbruk av myndigheter og politi",
"fredelige demonstranter", "tydelig og bestemt", "rettsak", "arbeiderpartiet", "gasskraftverk", "neddemming av Masi", "Kraftlinjer"]

# Columns for the output DataFrame
columns = ['URN', 'sentence'] + candidate_labels

# Iterate over each year
for year in range(1979, 2024):
    print(f"\nProcessing year: {year}")
    filename = f"{year}_conc_urn.csv"
    
    # Step 1: Read the CSV file into a DataFrame
    try:
        df_input = pd.read_csv(filename)
    except FileNotFoundError:
        print(f"The specified CSV file {filename} could not be found.")
        continue  # Skip to the next year

    # Step 2: Perform data integrity checks
    if 'text' not in df_input.columns or 'URN' not in df_input.columns:
        print(f"The 'text' or 'URN' column is missing in the CSV file {filename}.")
        continue  # Skip to the next year

    # Handle missing values
    if df_input['text'].isnull().any() or df_input['URN'].isnull().any():
        print(f"Warning: Missing values detected in the 'text' or 'URN' columns of {filename}.")
        df_input.dropna(subset=['text', 'URN'], inplace=True)

    # Data cleaning steps
    # Step 3: Extract patterns from 'text' column
    pattern = r"(\d\d\.\d\d\D|\d\d\:\d\d|\d\d\s\.\d\d\D|\d\d\.\s\d\d\D|\d\d\s\.\s\d\d\D|\d\d\s\d\d\D)"
    df_input['extracted_pattern'] = df_input['text'].str.extract(pattern, expand=False)

    # Step 4: Keep only rows where 'extracted_pattern' is not NaN
    new_df = df_input[~df_input['extracted_pattern'].isna()]

    # Step 5: Further filter out rows containing specific unwanted patterns
    pattern2 = r"(kl\.)|(fakkeltog)|(\bKL\b)|(\sKl\s)|(kl\s\d+)|(kl\s)"
    new_df = new_df[~new_df['text'].str.contains(pattern2, case=False, na=False)]

    # Step 6: Perform anti-join to get rows from df_input that are not in new_df based on 'text'
    df_clean = df_input[~df_input['text'].isin(new_df['text'])]

    # Remove the 'extracted_pattern' column as it's no longer needed
    df_clean = df_clean.drop(columns=['extracted_pattern'])

    # Proceed with the cleaned DataFrame
    sequences = df_clean['text'].tolist()
    urns = df_clean['URN'].tolist()

    # Initialize a DataFrame to store results for this year
    df_output = pd.DataFrame(columns=columns)

    # Adjust batch size based on available memory and GPU capabilities
    batch_size = 32  # Increase if your system can handle it
    num_batches = (len(sequences) + batch_size - 1) // batch_size

    # Process sequences in batches with a progress bar
    for i in tqdm(range(num_batches), desc=f"Processing {filename}"):
        batch_sequences = sequences[i*batch_size:(i+1)*batch_size]
        batch_urns = urns[i*batch_size:(i+1)*batch_size]
        
        # Get the classification result for the batch
        results = classifier(
            batch_sequences, 
            candidate_labels, 
            hypothesis_template=hypothesis_template, 
            multi_label=True  # Updated argument name
        )
        
        # Ensure results is a list
        if isinstance(results, dict):
            results = [results]
        
        # Process each result in the batch
        for j, result in enumerate(results):
            # Build a dictionary of scores
            scores_dict = {label: round(score, 2) for label, score in zip(result['labels'], result['scores'])}
            
            # Prepare row data
            row_data = [batch_urns[j], result['sequence']] + [scores_dict.get(label, 0.00) for label in candidate_labels]
            
            # Append the row to the DataFrame
            df_output.loc[len(df_output)] = row_data

    # Save the DataFrame to a CSV file
    output_filename = f"setninger_labeled_MNLI_{year}.csv"
    df_output.to_csv(output_filename, index=False)
    print(f"Saved results to {output_filename}")
