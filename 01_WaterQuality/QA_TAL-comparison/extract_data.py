import os
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

    if not os.path.exists(fp_full):
        print(f'Folder does not exist: {fp_full}')
        return pd.DataFrame()

    for filename in os.listdir(fp_full):
        if filename.lower().endswith('.pdf'):
            fp_file = os.path.join(fp_full, filename)
            try:
                doc = fitz.open(fp_file)
                for page in doc:
                    widgets = page.widgets()
                    if widgets:
                        for widget in widgets:
                            all_fields.append({
                                'folder_index': file_index,
                                'file_name': filename,
                                'field_name': widget.field_name,
                                'field_value': widget.field_value
                            })
                file_index += 1
                doc.close()
            except Exception as e:
                print(f'Error reading {filename}: {e}')

    return pd.DataFrame(all_fields)