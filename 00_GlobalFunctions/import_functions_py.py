# define EMP path
def abs_path_emp(fp_rel=None):
    user_profile = environ.get('USERPROFILE')
    base_path = join(user_profile, 'California Department of Water Resources', 'Environmental Monitoring Program - Documents')
    if fp_rel:
        return normpath(join(base_path, fp_rel))
    return normpath(base_path)
  
# --- regex ---

RUN_HEADER_FLAG = 'EMP Water Quality Data Sheet'

DATE_RE     = re.compile(r'Date:\s*(\d{1,2}/\d{1,2}/\d{4})')
RUNNAME_RE  = re.compile(r'Run Name:\s*(.+?)(?:\s+Time:|$)')
SONDE_V_RE  = re.compile(r'Sonde ID \(V\):\s*([A-Z0-9-]+)')
SONDE_H_RE  = re.compile(r'Sonde ID \(H\):\s*([A-Z0-9-]+)')
TIME_RE     = re.compile(r'\bTime:\s*(\d{1,2}:\d{2})\b')
BUCKET_RE   = re.compile(r'Churn Bucket #:\s*(\d+)')
LAT_RE      = re.compile(r'Lat:\s*(-?\d+\.\d+)')
LON_RE      = re.compile(r'Long:\s*(-?\d+\.\d+)')

LAB_ID_TOKEN = r'E\d{3,}[A-Z]\d{3,}'
LAB_INLINE   = re.compile(rf'\bLab ID[: ]+\s*({LAB_ID_TOKEN})\b')
LAB_ID_ROW   = re.compile(rf'\b{LAB_ID_TOKEN}\b')

MEAS_LABELS = ('Horizontal', 'Pre-Tow Surf', 'Pre-Tow Bot', 'No-Tow Surf', 'No-Tow Bot')
ANALYTE_PARAMS = [
    'Water Temp(°C)', 'Sp Cond(µS/cm)', 'pH', 'Turbidity(FNU)',
    'Fluor', 'Fluor(RFU)', 'DO(mg/L)', 'DO(%)'
]

# --- main functions ---

# structure pdf data
def structure_pdf_data(pdf_path):

    doc = fitz.open(pdf_path)
    all_lines: List[str] = []
    for p in range(len(doc)):
        txt = doc[p].get_text('text', sort=True)
        all_lines.extend([ln.strip() for ln in txt.split('\n') if ln.strip()])
    doc.close()
    
    all_rows: List[Dict] = []
    run_id = 0

    # return all_lines

    # split into run blocks
    for run_start, run_end in _block_spans(all_lines, lambda s: RUN_HEADER_FLAG in s):
        run_id += 1
        run_lines = all_lines[run_start:run_end]

        # metadata + blank
        # keep everything through the second "Station:" (Equipment Blank + first real station header)
        station_idxs = [i for i, s in enumerate(run_lines) if s.startswith('Station:')]
        header_end = station_idxs[1]  # include Equipment Blank + stop before first real station data
        header_lines = run_lines[:header_end]
        all_rows.extend(_parse_run_header(header_lines, run_id))

        # station blocks
        station_idxs = [i for i, s in enumerate(run_lines)
                        if s.startswith('Station:') and 'Pre_Check' not in s]
        station_spans: List[Tuple[int, int]] = []
        for i, st in enumerate(station_idxs):
            en = station_idxs[i+1] if i+1 < len(station_idxs) else len(run_lines)
            station_spans.append((st, en))

        station_id = 0
        for st_start, st_end in station_spans:
            station_id += 1
            pre_idx = max(0, st_start - 3) # look behind to catch duplicate
            pre_lines = run_lines[pre_idx:st_start]
            block = run_lines[st_start:st_end]
            all_rows.extend(_parse_station_block(block, pre_lines, run_id, station_id))

    return pd.DataFrame(all_rows)
  
  
# --- helper functions ---

def _block_spans(lines, is_header):
    starts = [i for i, s in enumerate(lines) if is_header(s)]
    spans: List[Tuple[int, int]] = []
    for i, st in enumerate(starts):
        en = starts[i+1] if i+1 < len(starts) else len(lines)
        spans.append((st, en))
    return spans

# output as strings
def _emit(run_id, station_id, param, value, **extra):
    d = {'run_id': run_id, 'station_id': station_id, 'param': param, 'value': value}
    d.update(extra)
    return d

# parse header
def _parse_run_header(lines, run_id):
    rows: List[Dict] = []
    # run metadata
    for ln in lines:
        # Date
        m = DATE_RE.search(ln)
        if m:
            rows.append(_emit(run_id, 0, 'Date:', m.group(1)))
        elif re.match(r'^Date:\s*$', ln) and i + 1 < len(lines):
            nxt = lines[i + 1].strip()
            if re.match(r'^\d{1,2}/\d{1,2}/\d{4}$', nxt):
                rows.append(_emit(run_id, 0, 'Date:', nxt))
        m = RUNNAME_RE.search(ln)
        # Run Name
        if m: rows.append(_emit(run_id, 0, 'Run Name', m.group(1).strip()))
        m = SONDE_V_RE.search(ln)
        # Sonde IDs
        if m: rows.append(_emit(run_id, 0, 'Sonde ID (V)', m.group(1)))
        m = SONDE_H_RE.search(ln)
        if m: rows.append(_emit(run_id, 0, 'Sonde ID (H)', m.group(1)))

    # --- equipment blank (station 999) ---
    rows.append(_emit(run_id, 999, 'Station', 'Equipment Blank', QC_Type='Blank'))

    got_time = got_bucket = got_lab = False

    for i, ln in enumerate(lines):
        # once we’ve hit all three fields, stop parsing the blank
        if got_time and got_bucket and got_lab:
            break

        # skip obvious station markers (we only want the blank station)
        if 'Station:' in ln:
            continue

        # Time
        if not got_time:
            m = TIME_RE.search(ln)
            if m:
                rows.append(_emit(run_id, 999, 'Time', m.group(1), QC_Type='Blank'))
                got_time = True
                continue

        # Churn Bucket #
        if not got_bucket:
            m = BUCKET_RE.search(ln)
            if m:
                rows.append(_emit(run_id, 999, 'Churn Bucket #', int(m.group(1)), QC_Type='Blank'))
                got_bucket = True
                continue

        # Lab ID (inline)
        if not got_lab:
            m = LAB_INLINE.search(ln)
            if m:
                rows.append(_emit(run_id, 999, 'Lab ID', m.group(1), QC_Type='Blank'))
                got_lab = True
                continue

            # two-line form: "Lab ID:" followed by the next line
            if re.match(r'^Lab ID:\s*$', ln) and i + 1 < len(lines):
                nxt = lines[i + 1].strip()
                if re.match(r'^E\d{3,}[A-Z]\d{3,}$', nxt):
                    rows.append(_emit(run_id, 999, 'Lab ID', nxt, QC_Type='Blank'))
                    got_lab = True
                    continue

    return rows

def _parse_station_block(block_lines, pre_lines, run_id, station_id):
    rows: List[Dict] = []
    header = block_lines[0]

    # station name
    m = re.search(r'Station:\s*([A-Za-z0-9_-]+)', header)
    if m:
        rows.append(_emit(run_id, station_id, 'Station', m.group(1)))

    # time
    for ln in block_lines[:5]:
        m = TIME_RE.search(ln)
        if m:
            rows.append(_emit(run_id, station_id, 'Time', m.group(1)))
            break

    # --- field conditions ---
    main_lab_id = None
    main_bucket = None

    # detect start of multi-line field header block (handles both layout variants)
    merged_section = []
    for i, ln in enumerate(block_lines):
        if re.search(r'\bSecchi\b', ln):
            # look ahead up to 3 lines to include Lab ID and wrapped headers/values
            merged = ln
            for j in range(1, 4):
                if i + j < len(block_lines):
                    nxt = block_lines[i + j].strip()
                    # stop merging if we hit a Notes: or Station line
                    if nxt.startswith('Notes:') or nxt.startswith('Station:'):
                        break
                    # merge if line contains Lab ID, Sky, Rain, numbers, or column units
                    if re.search(r'(Lab ID|Sky|Rain|\d|\(.*\))', nxt):
                        merged += ' ' + nxt
            merged_section.append(merged)
            break

    for ln in merged_section:
        m = LAB_ID_ROW.search(ln)
        if not m:
            continue

        parts = ln.split()
        try:
            k = parts.index(m.group(0))
            parts = parts[k:]
        except ValueError:
            pass

        field_params = [
            'Lab ID', 'Secchi(cm)', 'Churn Bucket #', 'Water Depth(ft)',
            'Air Temp(°F)', 'Wind(mph)', 'Sky', 'Wave Scale',
            'Rain', 'MC Score(1-5)', 'ChloroVol(ml)'
        ]

        for j, pname in enumerate(field_params):
            if j >= len(parts):
                break
            token = parts[j]
            if j == 0:
                main_lab_id = token
                rows.append(_emit(run_id, station_id, pname, token))
            elif pname == 'Churn Bucket #':
                num = re.sub(r'\D+', '', token)
                rows.append(_emit(run_id, station_id, pname, int(num) if num else None))
            else:
                try:
                    val = float(token) if '.' in token else int(token)
                except ValueError:
                    val = token
                rows.append(_emit(run_id, station_id, pname, val))
        break


    # --- data & coordinates --
    for ln in block_lines[1:40]:
        if any(lbl in ln for lbl in MEAS_LABELS):
            nums = [float(x) if '.' in x else int(x) for x in re.findall(r'\d+\.?\d*', ln)]
            if 'Horizontal' in ln:
                sonde_type, surf_bot = 'horizontal', None
            else:
                sonde_type = 'vertical'
                surf_bot = 'surface' if ('Surf' in ln or 'Surface' in ln) else 'bottom'
            for j, param in enumerate(ANALYTE_PARAMS):
                if j < len(nums):
                    rows.append(_emit(run_id, station_id, param, nums[j],
                                      SondeType=sonde_type, SurfBot=surf_bot))

        m = LAT_RE.search(ln)
        if m:
            rows.append(_emit(run_id, station_id, 'latitude', float(m.group(1))))
        m = LON_RE.search(ln)
        if m:
            rows.append(_emit(run_id, station_id, 'longitude', float(m.group(1))))

    # --- duplicate data ---
    # search for duplicate data
    rep_lab = None
    rep_bucket = None

    for ln in block_lines[:10]:
        m = LAB_INLINE.search(ln)
        if m:
            if m.group(1) != main_lab_id:
                rep_lab = m.group(1)
        m = BUCKET_RE.search(ln)
        if m:
            try:
                val = int(m.group(1))
            except ValueError:
                num = re.sub(r'\D+', '', m.group(1))
                val = int(num) if num else None
            if val is not None and (main_bucket is None or val != main_bucket):
                rep_bucket = val

    if rep_lab is not None:
        rows.append(_emit(run_id, station_id, 'Lab ID', rep_lab, QC_Type='Replicate'))
    if rep_bucket is not None:
        rows.append(_emit(run_id, station_id, 'Churn Bucket #', rep_bucket, QC_Type='Replicate'))

    # --- notes ---
    for i, ln in enumerate(block_lines[:60]):
        if ln.startswith('Notes:'):
            note = ln.replace('Notes:', '').strip()

            # look one line behind if this one is empty
            if not note and i > 0:
                prev_ln = block_lines[i - 1].strip()
                if prev_ln and not prev_ln.startswith(('Station:', 'Notes:')):
                    note = prev_ln

            if note:
                rows.append(_emit(run_id, station_id, 'Notes', note))
            break

    return rows

# fix formatting
def to_scalar_string(x):
    if isinstance(x, (list, tuple, set)):
        x = next(iter(x), None)
    if x is None:
        return None
    return str(x)
