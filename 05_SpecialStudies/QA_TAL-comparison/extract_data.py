import os
import re
import fitz
import pandas as pd
from datetime import datetime, date

# define base file directory
base_dir = os.path.expanduser('~')
fp_pdf_qa = os.path.join(
    base_dir,
    'California Department of Water Resources',
    'DWR-COI-QA - QASP09-Total Algae Sensor 2-Point Calibration Effectiveness Study',
    '02_Data',
    'Results-Calibration files-Regularized names'
)

# extract PDFs from folder
def extract_pdfs(fp_rel):
    all_fields = []
    file_index = 1

    fp_full = os.path.join(fp_pdf_qa, fp_rel)

    for filename in os.listdir(fp_full):
        if filename.lower().endswith('.pdf'):
            fp_file = os.path.join(fp_full, filename)

            doc = fitz.open(fp_file)
            for page in doc:
                widgets = page.widgets()
                if widgets:
                    for widget in widgets:
                        all_fields.append({
                            'file_index': file_index,
                            'file_name': filename,
                            'field_name': widget.field_name,
                            'field_value': widget.field_value
                        })
            file_index += 1
            doc.close()

    return pd.DataFrame(all_fields)

# clean col names
def rm_suffix(field_list):
    cleaned = []
    for i, val in enumerate(field_list):
        base = re.sub(r'_\d+$', '', val)
        if base == 'Date':
            if i > 0:
                prev_cleaned = cleaned[i - 1]
                base = f'{prev_cleaned}Date'
            else:
                base = 'UnknownDate'
        cleaned.append(base)
    return cleaned

# standardize dates
def standardize_date(date_str):
    if pd.isna(date_str) or not isinstance(date_str, str):
        return None

    date_str = date_str.replace('.', '/').strip()

    for fmt in ('%m/%d/%Y', '%m/%d/%y'):
        try:
            return datetime.strptime(date_str, fmt).date()
        except ValueError:
            continue

    parts = re.split(r'[/\-]', date_str)
    if len(parts) == 3:
        m, d, y = parts
        if len(m) == 1:
            m = f'0{m}'
        if len(d) == 1:
            d = f'0{d}'
        if len(y) == 2:
            y = f'20{y}'
        try:
            return datetime.strptime(f'{m}/{d}/{y}', '%m/%d/%Y').date()
        except ValueError:
            return None

    return None

# clean dates
def clean_dates(df, col_name, notes_col='Notes'):
    if '_orig_notes' not in df.columns:
        df['_orig_notes'] = df[notes_col].copy()

    new_notes = df['_orig_notes'].copy()
    new_dates = []

    for i, val in enumerate(df[col_name]):
        if isinstance(val, str) and re.match(r'.+\s*-\s*.+', val):
            parts = [v.strip() for v in val.split('-', maxsplit=1)]
            date1 = standardize_date(parts[0])
            date2 = standardize_date(parts[1]) if len(parts) > 1 else None

            new_dates.append(date1)

            if isinstance(date2, (datetime, date)):
                note = new_notes.iloc[i] if pd.notna(new_notes.iloc[i]) else ''
                new_notes.iloc[i] = f"{note} also calibrated on {date2.strftime('%Y-%m-%d')}".strip()
        else:
            new_dates.append(standardize_date(val))

    cleaned_col = f'{col_name}'
    df[cleaned_col] = new_dates
    df[notes_col] = new_notes

    return df

# clean data
def clean_data(df, mode='2023'):
    all_rows = []

    for file_val in df['file_index'].unique():
        temp_df = df[df['file_index'] == file_val].copy()
        temp_df['clean_field'] = rm_suffix(temp_df['field_name'].tolist())

        # metadata
        block0_start = temp_df[temp_df['clean_field'] == 'Standard name'].index[0]
        block0_end = (
            temp_df[temp_df['clean_field'] == 'Procedure version'].index[0]
            if 'Procedure version' in temp_df['clean_field'].values
            else temp_df[temp_df['clean_field'] == 'CT sensor SN'].index[0]
        )

        block0 = temp_df.loc[block0_start:block0_end].copy()
        block0_dict = dict(zip(block0['clean_field'], block0['field_value']))
        block0_dict['file_index'] = file_val

        # block per sonde ID
        orig_id_rows = temp_df[temp_df['clean_field'].str.match(r'^Original Sonde ID')]
        block_start_idx = orig_id_rows.index.tolist()
        block_start_idx.append(temp_df.index[-1] + 1)

        for i in range(len(block_start_idx) - 1):
            blk = temp_df.loc[block_start_idx[i]:block_start_idx[i + 1]].copy()
            if blk.empty:
                continue
            blk['block'] = i + 1

            notes_val = blk.loc[blk['clean_field'].str.contains('Notes', case=False), 'field_value'].iloc[0] if blk['clean_field'].str.contains('Notes', case=False).any() else None
            orig_sonde_id = blk.loc[blk['clean_field'].str.match(r'^Original Sonde ID'), 'field_value'].iloc[0] if blk['clean_field'].str.match(r'^Original Sonde ID').any() else None
            sensor_sn = blk.loc[blk['clean_field'].str.match(r'^Sensor Serial Number'), 'field_value'].iloc[0] if blk['clean_field'].str.match(r'^Sensor Serial Number').any() else None
            
            # row per sonde ID block
            subblocks = []
            if mode == '2023':
                temp_rows = blk[blk['clean_field'].str.match(r'^Temp')]
                sub_start_idx = temp_rows.index.tolist()
                if not sub_start_idx:
                    continue 

                sub_end_idx = sub_start_idx[1:] + [blk.index[-1] + 1]

                for j in range(len(sub_start_idx)):
                    start = sub_start_idx[j]
                    end = sub_end_idx[j]
                    subblocks.append(blk.loc[start:end].copy())

            else:  # mode == '2024'
                blk = blk.reset_index()
                j = 0
                while j < len(blk):
                    row = blk.loc[j]
                    if re.match(r'^(000|PreCal|Text)', row['clean_field']):
                        start_idx = j
                        k = j + 1
                        while k < len(blk) and 'temperature' not in blk.loc[k, 'clean_field'].lower():
                            k += 1
                        end_idx = k if k < len(blk) else len(blk)
                        subblocks.append(blk.loc[start_idx:end_idx].copy())
                        j = k + 1
                    else:
                        j += 1

            for subblock_num, sub in enumerate(subblocks):
                temp_row = sub[sub['clean_field'].str.contains('Temp', case=False)]
                temp_value = temp_row['field_value'].iloc[0] if not temp_row.empty else None

                zeros = sub[sub['clean_field'].str.match(r'^000')]
                expected_val = float(zeros['field_value'].iloc[0]) if not zeros.empty and re.match(r'^\d*\.?\d+$', str(zeros['field_value'].iloc[0])) else 0.00
                precal_val = None

                if len(zeros) >= 2:
                    precal_val = zeros['field_value'].iloc[1]
                else:
                    fallback = sub[sub['clean_field'].str.match(r'^PreCal|^Text')]
                    precal_val = fallback['field_value'].iloc[0] if not fallback.empty else None

                postcal_val = sub.loc[sub['clean_field'].str.match(r'^PostCal'), 'field_value'].iloc[0] if sub['clean_field'].str.match(r'^PostCal').any() else None

                standard = 'DI Water' if subblock_num % 2 == 0 else 'Rhodamine'
                parameter = 'Chl' if (subblock_num // 4) % 2 == 0 else 'PC'
                unitstring = 'RFU' if (subblock_num // 2) % 2 == 0 else 'ug_L'

                hach_key_1 = 'Hach DR4000 verification of 0625 mgL standard accept range 01270132 ABS'
                hach_key_2 = 'Hach DR4000 verification of 0625 mgL solution accept range is 01270132 ABS'
                hach_ver = block0_dict.get(hach_key_1) or block0_dict.get(hach_key_2)

                row = {
                    'Stock125PrepName': block0_dict.get('Stock solution 125 mgL prepared by full name'),
                    'Stock125PrepDate': block0_dict.get('Stock solution 125 mgL prepared by full nameDate'),
                    'Cal625PrepName': block0_dict.get('Calibration standard 0625 mgL prepared by full name'),
                    'Cal625PrepDate': block0_dict.get('Calibration standard 0625 mgL prepared by full nameDate'),
                    'Hach625VerAbs': hach_ver,
                    'CalibratorName': block0_dict.get('Calibration performed by full name'),
                    'CalibDate': block0_dict.get('Calibration performed by full nameDate'),
                    'ProcDCN': block0_dict.get('Procedure DCN'),
                    'ProcVer': block0_dict.get('Procedure version') or block0_dict.get('Version'),
                    'CTSensorSN': block0_dict.get('CT sensor SN'),
                    'OrigSondeID': orig_sonde_id,
                    'TALSensorSN': sensor_sn,
                    'Standard': standard,
                    'Parameter': parameter,
                    'Units': unitstring,
                    'ExpectVal': expected_val,
                    'PreCalVal': precal_val,
                    'PostCalVal': postcal_val,
                    'SensorTemp': temp_value,
                    'Notes': notes_val
                }

                all_rows.append(row)

    df_final = pd.DataFrame(all_rows)

    for col in ['SensorTemp', 'Notes']:
        if col in df_final.columns:
            df_final[col] = df_final[col].replace('', pd.NA)
            df_final[col] = df_final[col].replace(r'^\s*$', pd.NA, regex=True)

    df_final = df_final.dropna(subset=['SensorTemp'])

    df_final = clean_dates(df_final, 'Cal625PrepDate')
    df_final = clean_dates(df_final, 'Stock125PrepDate')
    df_final = clean_dates(df_final, 'CalibDate')

    col_order = [
        'Stock125PrepName', 'Stock125PrepDate', 'Cal625PrepName', 'Cal625PrepDate',
        'Hach625VerAbs', 'CalibratorName', 'CalibDate', 'ProcDCN', 'ProcVer',
        'CTSensorSN', 'OrigSondeID', 'TALSensorSN', 'Standard', 'Parameter', 'Units',
        'ExpectVal', 'PreCalVal', 'PostCalVal', 'SensorTemp', 'Notes'
    ]

    df_final = df_final.reindex(columns=col_order)
    df_final['Units'] = df_final['Units'].replace('gL', 'ug_L')

    for col in ['SensorTemp', 'PreCalVal', 'PostCalVal']:
        df_final[col] = pd.to_numeric(df_final[col], errors='coerce')

    return df_final
