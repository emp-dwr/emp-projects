import os
import re
import fitz
import pandas as pd

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

# clean 2023 data
def clean_data_three(df):

    all_rows = []

    for file_val in df['file_index'].unique():
        temp_df = df[df['file_index'] == file_val].copy()
        temp_df['clean_field'] = rm_suffix(temp_df['field_name'].tolist())

        block0_start = temp_df[temp_df['clean_field'] == 'Standard name'].index[0]
        if 'Procedure version' in temp_df['clean_field'].values:
            block0_end = temp_df[temp_df['clean_field'] == 'Procedure version'].index[0]
        elif 'CT sensor SN' in temp_df['clean_field'].values:
            block0_end = temp_df[temp_df['clean_field'] == 'CT sensor SN'].index[0]

        block0 = temp_df.loc[block0_start:block0_end].copy()
        block0_dict = dict(zip(block0['clean_field'], block0['field_value']))
        block0_dict['file_index'] = file_val

        original_id_rows = temp_df[temp_df['clean_field'].str.match(r'^Original Sonde ID')]
        block_start_idx = original_id_rows.index.tolist()
        block_start_idx.append(temp_df.index[-1] + 1)

        blocks = []
        for i in range(len(block_start_idx) - 1):
            start = block_start_idx[i]
            end = block_start_idx[i + 1]
            blk = temp_df.loc[start:end].copy()
            if blk.empty:
                continue
            blk['block'] = i + 1
            blocks.append(blk)

        for blk in blocks:
            notes_row = blk[blk['clean_field'].str.contains('Notes', case=False)]
            notes_val = notes_row['field_value'].iloc[0] if not notes_row.empty else None

            sonde_id_row = blk[blk['clean_field'].str.match(r'^Original Sonde ID')]
            orig_sonde_id = sonde_id_row['field_value'].iloc[0] if not sonde_id_row.empty else None

            sensor_sn_row = blk[blk['clean_field'].str.match(r'^Sensor Serial Number')]
            sensor_sn = sensor_sn_row['field_value'].iloc[0] if not sensor_sn_row.empty else None

            if blk['clean_field'].str.match(r'^Temp').any():
                temp_rows = blk[blk['clean_field'].str.match(r'^Temp')]
                sub_start_idx = temp_rows.index.tolist()
                sub_end_idx = sub_start_idx[1:] + [blk.index[-1] + 1]
            else:
                temp_match = blk['clean_field'].str.contains('Temperature', case=False)
                temp_indices = blk[temp_match].index.tolist()
                if not temp_indices:
                    continue
                sub_end_idx = temp_indices

                start_mask = blk['clean_field'].str.match(r'^000') | blk['clean_field'].str.match(r'^PreCal')
                sub_start_idx = blk[start_mask].index.tolist()

                if len(sub_end_idx) < len(sub_start_idx):
                    sub_end_idx += [blk.index[-1] + 1] * (len(sub_start_idx) - len(sub_end_idx))

            for subblock_num in range(len(sub_start_idx)):
                start = sub_start_idx[subblock_num]
                end = sub_end_idx[subblock_num] if subblock_num < len(sub_end_idx) else blk.index[-1] + 1
                sub = blk.loc[start:end]

                if blk['clean_field'].str.match(r'^Temp').any():
                    temp_row = sub[sub['clean_field'].str.match(r'^Temp')]
                    temp_value = temp_row['field_value'].iloc[0] if not temp_row.empty else None
                else:
                    temp_value = sub[sub['clean_field'].str.contains('Temperature', case=False)]['field_value'].iloc[0] if not sub[sub['clean_field'].str.contains('Temperature', case=False)].empty else None

                expected = sub[sub['clean_field'].str.match(r'^000')]['field_value']
                if not expected.empty and re.match(r'^\d*\.?\d+$', str(expected.iloc[0])):
                    expected_val = float(expected.iloc[0])
                else:
                    expected_val = 0.00

                precal = sub[sub['clean_field'].str.match(r'^PreCal')]['field_value']
                precal_val = precal.iloc[0] if not precal.empty else None

                postcal = sub[sub['clean_field'].str.match(r'^PostCal')]['field_value']
                postcal_val = postcal.iloc[0] if not postcal.empty else None

                standard = 'DI Water' if subblock_num % 2 == 0 else 'Rhodamine'
                parameter = 'Chl' if (subblock_num // 4) % 2 == 0 else 'PC'
                unitstring = 'RFU' if (subblock_num // 2) % 2 == 0 else 'ug_L'

                row = {
                    'Stock125PrepName': block0_dict.get('Stock solution 125 mgL prepared by full name'),
                    'Stock125PrepDate': block0_dict.get('Stock solution 125 mgL prepared by full nameDate'),
                    'Cal625PrepName': block0_dict.get('Calibration standard 0625 mgL prepared by full name'),
                    'Cal625PrepDate': block0_dict.get('Calibration standard 0625 mgL prepared by full nameDate'),
                    'Hach625VerAbs': block0_dict.get('Hach DR4000 verification of 0625 mgL standard accept range 01270132 ABS'),
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

    final_df = pd.DataFrame(all_rows)

    na_patterns = ['', ' ', 'N/A', 'na', 'NA']
    for col in ['SensorTemp', 'Notes']:
        if col in final_df.columns:
            final_df[col] = final_df[col].replace(na_patterns, pd.NA)
            final_df[col] = final_df[col].replace(r'^\s*$', pd.NA, regex=True)

    final_df = final_df.dropna(subset=['SensorTemp'])

    true_order = [
        'Stock125PrepName', 'Stock125PrepDate', 'Cal625PrepName', 'Cal625PrepDate',
        'Hach625VerAbs', 'CalibratorName', 'CalibDate', 'ProcDCN', 'ProcVer', 'CTSensorSN',
        'OrigSondeID', 'TALSensorSN', 'Standard', 'Parameter', 'Units',
        'ExpectVal', 'PreCalVal', 'PostCalVal', 'SensorTemp', 'Notes'
    ]

    final_df = final_df.reindex(columns=true_order)
    final_df['Units'] = final_df['Units'].replace('gL', 'ug_L')

    for col in ['SensorTemp', 'PreCalVal', 'PostCalVal']:
        final_df[col] = pd.to_numeric(final_df[col], errors='coerce')

    return final_df

# clean 2024 data
def clean_data_four(df):

    all_rows = []

    for file_val in df['file_index'].unique():
        temp_df = df[df['file_index'] == file_val].copy()
        temp_df['clean_field'] = rm_suffix(temp_df['field_name'].tolist())

        try:
            block0_start = temp_df[temp_df['clean_field'] == 'Standard name'].index[0]
            block0_end = temp_df[temp_df['clean_field'] == 'CT sensor SN'].index[0]
        except IndexError:
            continue

        block0 = temp_df.loc[block0_start:block0_end].copy()
        block0_dict = dict(zip(block0['clean_field'], block0['field_value']))
        block0_dict['file_index'] = file_val

        original_id_rows = temp_df[temp_df['clean_field'].str.match(r'^Original Sonde ID')]
        block_start_idx = original_id_rows.index.tolist()
        block_start_idx.append(temp_df.index[-1] + 1)

        for i in range(len(block_start_idx) - 1):
            start = block_start_idx[i]
            end = block_start_idx[i + 1]
            blk = temp_df.loc[start:end].copy()
            if blk.empty:
                continue

            blk['block'] = i + 1

            notes_val = blk.loc[blk['clean_field'].str.contains('Notes', case=False), 'field_value'].iloc[0] if blk['clean_field'].str.contains('Notes', case=False).any() else None
            orig_sonde_id = blk.loc[blk['clean_field'].str.match(r'^Original Sonde ID'), 'field_value'].iloc[0] if blk['clean_field'].str.match(r'^Original Sonde ID').any() else None
            sensor_sn = blk.loc[blk['clean_field'].str.match(r'^Sensor Serial Number'), 'field_value'].iloc[0] if blk['clean_field'].str.match(r'^Sensor Serial Number').any() else None

            subblocks = []
            blk = blk.reset_index()
            i = 0
            while i < len(blk):
                row = blk.loc[i]
                if re.match(r'^(000|PreCal|Text)', row['clean_field']):
                    start_idx = i

                    j = i + 1
                    while j < len(blk):
                        if 'temperature' in blk.loc[j, 'clean_field'].lower():
                            break
                        j += 1

                    end_idx = j if j < len(blk) else len(blk)

                    sub = blk.loc[start_idx:end_idx].copy()
                    subblocks.append(sub)

                    i = j + 1
                else:
                    i += 1

            for subblock_num, sub in enumerate(subblocks):
                temp_row = sub[sub['clean_field'].str.contains('Temperature', case=False)]
                temp_value = temp_row['field_value'].iloc[0] if not temp_row.empty else None

                zeros = sub[sub['clean_field'].str.match(r'^000')]
                if len(zeros) >= 2:
                    expected_val = float(zeros['field_value'].iloc[0]) if re.match(r'^\d*\.?\d+$', str(zeros['field_value'].iloc[0])) else 0.00
                    precal_val = zeros['field_value'].iloc[1]
                elif len(zeros) == 1:
                    expected_val = float(zeros['field_value'].iloc[0]) if re.match(r'^\d*\.?\d+$', str(zeros['field_value'].iloc[0])) else 0.00
                    # fallback to PreCal or Text
                    precal_match = sub[sub['clean_field'].str.match(r'^PreCal|^Text')]
                    precal_val = precal_match['field_value'].iloc[0] if not precal_match.empty else None
                else:
                    expected_val = 0.00
                    precal_match = sub[sub['clean_field'].str.match(r'^PreCal|^Text')]
                    precal_val = precal_match['field_value'].iloc[0] if not precal_match.empty else None

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

    final_df = pd.DataFrame(all_rows)


    na_patterns = ['', ' ', 'N/A', 'na', 'NA']
    for col in ['SensorTemp', 'Notes']:
        if col in final_df.columns:
            final_df[col] = final_df[col].replace(na_patterns, pd.NA)
            final_df[col] = final_df[col].replace(r'^\s*$', pd.NA, regex=True)

    final_df = final_df.dropna(subset=['SensorTemp'])

    column_order = [
        'Stock125PrepName', 'Stock125PrepDate', 'Cal625PrepName', 'Cal625PrepDate',
        'Hach625VerAbs', 'CalibratorName', 'CalibDate', 'ProcDCN', 'ProcVer',
        'CTSensorSN', 'OrigSondeID', 'TALSensorSN', 'Standard', 'Parameter', 'Units',
        'ExpectVal', 'PreCalVal', 'PostCalVal', 'SensorTemp', 'Notes'
    ]

    final_df = final_df.reindex(columns=column_order)
    final_df['Units'] = final_df['Units'].replace('gL', 'ug_L')

    for col in ['SensorTemp', 'PreCalVal', 'PostCalVal']:
        final_df[col] = pd.to_numeric(final_df[col], errors='coerce')

    return final_df
