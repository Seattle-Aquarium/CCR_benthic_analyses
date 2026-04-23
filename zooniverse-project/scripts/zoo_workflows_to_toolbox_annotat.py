#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Zooniverse (up to 4 workflows) -> Toolbox dataset.csv linker

Workflows:
  Yes/No          (30787): crowd confirm/deny
  Yes/No Expert   (31534): expert confirm/deny
  Multi           (30752): crowd species classification
  Multi Expert    (31535): expert species classification

Label priority (highest first):
  1. Multi consensus (30752 OR 31535) → label mapped from labelset
  2. Yes confirmed   (30787 OR 31534) → keep original Toolbox label
  3. Denied / needs more votes        → Review

Consensus rules:
  Yes/No workflows: n_classifications >= 5 AND agreement >= 0.75
  Multi workflows:  n_classifications >= 3 AND agreement >= 0.67

Output headers (in this order):
  Name, Path, Row, Column, Patch Size,
  Annotation Type, Label, Long Label, Verified

INNER JOIN: keeps only rows present in Toolbox annotation.csv AND matched
in at least one selected workflow on (source_image, row, column).
"""

import json
import os
import re
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path

import pandas as pd
import numpy as np

# ---------- Workflow IDs ----------
WORKFLOW_YESNO        = 30787
WORKFLOW_YESNO_EXPERT = 31534
WORKFLOW_MULTI        = 30752
WORKFLOW_MULTI_EXPERT = 31535

# YES/NO workflow thresholds
YESNO_AGREE_MIN_N    = 5
YESNO_AGREE_MIN_FRAC = 0.75

# MULTI workflow thresholds
MULTI_AGREE_MIN_N    = 3
MULTI_AGREE_MIN_FRAC = 0.67

REQUIRED_TOOLBOX_COLUMNS = [
    "Name", "Path", "Row", "Column", "Patch Size",
    "Annotation Type", "Label", "Long Label", "Verified",
    "Machine confidence 1", "Machine suggestion 1"
]

# ============================================================
# GUI
# ============================================================
def get_args_via_gui():
    result = {}

    root = tk.Tk()
    root.title("Zooniverse → Toolbox Linker")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Zooniverse Workflows → Toolbox Linker",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Select files and which workflows to include, then click Run.",
              foreground="grey").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── File inputs ───────────────────────────────────────────────────────────
    def make_file_row(row, label, filetypes, is_dir=False):
        ttk.Label(root, text=label).grid(row=row, column=0, sticky="e", **pad)
        var = tk.StringVar()
        ttk.Entry(root, textvariable=var, width=55).grid(row=row, column=1, **pad)

        def browse():
            if is_dir:
                p = filedialog.askdirectory(title=f"Select {label}")
            else:
                p = filedialog.askopenfilename(title=f"Select {label}", filetypes=filetypes)
            if p:
                var.set(p)

        ttk.Button(root, text="Browse...", command=browse).grid(row=row, column=2, **pad)
        return var

    dataset_var  = make_file_row(3, "Toolbox annotation.csv:",
                                 [("CSV files", "*.csv"), ("All files", "*.*")])
    zoo_var      = make_file_row(4, "Zooniverse export CSV:",
                                 [("CSV files", "*.csv"), ("All files", "*.*")])
    labelset_var = make_file_row(5, "Labelset JSON:",
                                 [("JSON files", "*.json"), ("All files", "*.*")])
    outdir_var   = make_file_row(6, "Output directory:", [], is_dir=True)

    ttk.Separator(root, orient="horizontal").grid(
        row=7, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Workflow selection ────────────────────────────────────────────────────
    ttk.Label(root, text="Workflows to include:",
              font=("Helvetica", 10, "bold")).grid(
        row=8, column=0, columnspan=3, sticky="w", padx=14, pady=(4, 0))

    wf_frame = ttk.Frame(root)
    wf_frame.grid(row=9, column=0, columnspan=3, sticky="w", padx=18, pady=4)

    use_yn_var     = tk.BooleanVar(value=True)
    use_yn_exp_var = tk.BooleanVar(value=True)
    use_m_var      = tk.BooleanVar(value=True)
    use_m_exp_var  = tk.BooleanVar(value=True)

    ttk.Checkbutton(wf_frame, text=f"Yes/No crowd       ({WORKFLOW_YESNO})",
                    variable=use_yn_var).grid(row=0, column=0, sticky="w", padx=(0, 20))
    ttk.Checkbutton(wf_frame, text=f"Yes/No expert      ({WORKFLOW_YESNO_EXPERT})",
                    variable=use_yn_exp_var).grid(row=0, column=1, sticky="w")
    ttk.Checkbutton(wf_frame, text=f"Multi-choice crowd ({WORKFLOW_MULTI})",
                    variable=use_m_var).grid(row=1, column=0, sticky="w", padx=(0, 20))
    ttk.Checkbutton(wf_frame, text=f"Multi-choice expert ({WORKFLOW_MULTI_EXPERT})",
                    variable=use_m_exp_var).grid(row=1, column=1, sticky="w")

    ttk.Separator(root, orient="horizontal").grid(
        row=10, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=11, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        dataset  = dataset_var.get().strip()
        zoo      = zoo_var.get().strip()
        labelset = labelset_var.get().strip()
        outdir   = outdir_var.get().strip()

        if not dataset:
            messagebox.showerror("Missing input", "Please select the Toolbox annotation.csv.")
            return
        if not zoo:
            messagebox.showerror("Missing input", "Please select the Zooniverse export CSV.")
            return
        if not labelset:
            messagebox.showerror("Missing input", "Please select the labelset JSON.")
            return
        if not outdir:
            messagebox.showerror("Missing input", "Please select an output directory.")
            return
        for p, nm in [(dataset, "annotation.csv"), (zoo, "Zooniverse export"),
                      (labelset, "labelset JSON")]:
            if not Path(p).exists():
                messagebox.showerror("File not found", f"{nm} not found:\n{p}")
                return
        if not any([use_yn_var.get(), use_yn_exp_var.get(),
                    use_m_var.get(), use_m_exp_var.get()]):
            messagebox.showerror("No workflows", "Select at least one workflow.")
            return

        result["dataset"]      = dataset
        result["zoo"]          = zoo
        result["labelset"]     = labelset
        result["outdir"]       = outdir
        result["use_yn"]       = use_yn_var.get()
        result["use_yn_exp"]   = use_yn_exp_var.get()
        result["use_multi"]    = use_m_var.get()
        result["use_multi_exp"]= use_m_exp_var.get()
        result["submitted"]    = True
        root.destroy()

    def on_cancel():
        root.destroy()

    ttk.Button(btn_frame, text="  Run  ", command=on_run).pack(side="left", padx=8)
    ttk.Button(btn_frame, text="Cancel", command=on_cancel).pack(side="left", padx=8)

    root.mainloop()

    if not result.get("submitted"):
        print("Cancelled.")
        sys.exit(0)

    return result


# ============================================================
# JSON helpers
# ============================================================
def safe_json_loads(x):
    try:
        return json.loads(x) if isinstance(x, str) else {}
    except Exception:
        return {}

def norm_str(x):
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return ""
    return str(x).strip()

def norm_lower(x):
    return norm_str(x).lower()

def _norm_key(s: str) -> str:
    s = norm_str(s).lower()
    s = re.sub(r"[^a-z0-9]+", "_", s)
    s = re.sub(r"_+", "_", s).strip("_")
    return s

def clean_markdown_labels(label: str) -> str:
    cleaned = norm_str(label)
    cleaned = re.sub(r'!\[.*?\]\(.*?\)', '', cleaned)
    cleaned = re.sub(r'\s+', ' ', cleaned).strip()
    return cleaned


# ============================================================
# Labelset mapping
# ============================================================
def load_labelset_map(labelset_json_path: str):
    with open(labelset_json_path, "r", encoding="utf-8") as f:
        items = json.load(f)
    label_map = {}
    for it in items:
        short_c = norm_str(it.get("short_label_code", ""))
        long_c  = norm_str(it.get("long_label_code", ""))
        if short_c:
            label_map[_norm_key(short_c)] = (short_c, long_c or short_c)
        if long_c:
            label_map[_norm_key(long_c)]  = (short_c or long_c, long_c)
    return label_map

def map_to_toolbox_codes(consensus_label: str, label_map: dict,
                         manual_overrides: dict | None = None):
    raw = norm_str(consensus_label)
    if manual_overrides and raw in manual_overrides:
        return manual_overrides[raw]
    k = _norm_key(raw)
    if k in label_map:
        return label_map[k]
    expansions = {
        "cca":          "RE_CCA",
        "sugar":        "KE_sugar",
        "leafy":        "RE_leaf",
        "green_algae":  "GR_ulva",
        "boulder":      "SU_bould",
        "cobble":      "SU_cob",
        "pebble":      "SU_peb",
        "bushy":       "RE_bush",
        "silt":         "SU_silt",

    }
    if k in expansions:
        kk = _norm_key(expansions[k])
        if kk in label_map:
            return label_map[kk]
    return (None, None)


# ============================================================
# Zooniverse parsing
# ============================================================
def extract_subject_fields(subject_data_str):
    sd = safe_json_loads(subject_data_str)
    if not isinstance(sd, dict) or len(sd) == 0:
        return pd.Series({"source_image": np.nan,
                          "Row_int": np.nan, "Column_int": np.nan})
    sid  = next(iter(sd.keys()))
    info = sd.get(sid, {}) if isinstance(sd.get(sid, {}), dict) else {}
    return pd.Series({
        "source_image": info.get("source_image", np.nan),
        "Row_int":      pd.to_numeric(info.get("row",    np.nan), errors="coerce"),
        "Column_int":   pd.to_numeric(info.get("column", np.nan), errors="coerce"),
    })

def _annotation_value_to_string(v):
    if v is None:
        return ""
    if isinstance(v, str):
        return v.strip()
    if isinstance(v, (int, float, bool)):
        return str(v)
    if isinstance(v, list):
        return ", ".join([norm_str(x) for x in v if norm_str(x)])
    if isinstance(v, dict):
        for key in ("choice", "label", "value", "answers"):
            if key in v:
                s = _annotation_value_to_string(v.get(key))
                if s:
                    return s
        try:
            return json.dumps(v, ensure_ascii=False)
        except Exception:
            return str(v)
    return norm_str(v)

def extract_T0_yes_no(annotations_str):
    ann = safe_json_loads(annotations_str)
    if not isinstance(ann, list):
        return ""
    for d in ann:
        if isinstance(d, dict) and d.get("task") == "T0":
            v = norm_lower(_annotation_value_to_string(d.get("value", "")))
            if v == "yes":
                return "yes"
            if "no" in v:
                return "no"
            return ""
    return ""

def extract_multi_chosen_label(annotations_str):
    """Most specific available answer wins: T3 > T2 > T1 > T0."""
    ann = safe_json_loads(annotations_str)
    if not isinstance(ann, list):
        return ""
    by_task = {}
    for d in ann:
        if isinstance(d, dict) and "task" in d:
            by_task[d.get("task")] = _annotation_value_to_string(d.get("value", ""))
    for t in ("T3", "T2", "T1", "T0"):
        v = norm_str(by_task.get(t, ""))
        if v:
            return v
    return ""


# ============================================================
# Vote aggregation helpers
# ============================================================
def aggregate_yesno_votes(z: pd.DataFrame, workflow_id: int, prefix: str):
    """Return per-(source_image, row, col) vote summary for one yes/no workflow."""
    wf = z[z["workflow_id"] == workflow_id].copy()
    wf["T0"]     = wf["annotations"].apply(extract_T0_yes_no)
    wf["is_yes"] = wf["T0"].eq("yes")
    wf["is_no"]  = wf["T0"].eq("no")

    votes = (
        wf.groupby(["source_image", "Row_int", "Column_int"], dropna=False)
          .agg(
              **{f"{prefix}_n":   ("classification_id", "count"),
                 f"{prefix}_yes": ("is_yes", "sum"),
                 f"{prefix}_no":  ("is_no",  "sum")},
          )
          .reset_index()
    )
    # per-direction fraction (not max-agreement — we check yes and no separately)
    total = votes[f"{prefix}_n"].replace(0, np.nan)
    votes[f"{prefix}_yes_frac"] = votes[f"{prefix}_yes"] / total
    votes[f"{prefix}_no_frac"]  = votes[f"{prefix}_no"]  / total
    return votes

def aggregate_multi_votes(z: pd.DataFrame, workflow_id: int, prefix: str):
    """Return per-(source_image, row, col) top-label summary for one multi workflow."""
    wf = z[z["workflow_id"] == workflow_id].copy()
    wf["chosen_label"] = wf["annotations"].apply(extract_multi_chosen_label)
    wf = wf[wf["chosen_label"].astype(str).str.len() > 0].copy()

    counts = (
        wf.groupby(["source_image", "Row_int", "Column_int", "chosen_label"], dropna=False)
          .size()
          .reset_index(name="label_count")
    )
    totals = (
        wf.groupby(["source_image", "Row_int", "Column_int"], dropna=False)
          .agg(**{f"{prefix}_n": ("classification_id", "count")})
          .reset_index()
    )
    counts_sorted = counts.sort_values(
        ["source_image", "Row_int", "Column_int", "label_count", "chosen_label"],
        ascending=[True, True, True, False, True]
    )
    top = counts_sorted.drop_duplicates(
        subset=["source_image", "Row_int", "Column_int"], keep="first"
    ).rename(columns={"chosen_label": f"{prefix}_top_label",
                       "label_count":  f"{prefix}_top_count"})

    votes = totals.merge(top, on=["source_image", "Row_int", "Column_int"], how="left")
    votes[f"{prefix}_agreement"] = np.where(
        votes[f"{prefix}_n"] > 0,
        votes[f"{prefix}_top_count"] / votes[f"{prefix}_n"],
        np.nan
    )
    return votes


# ============================================================
# Toolbox IO
# ============================================================
def ensure_required_columns(ds: pd.DataFrame):
    missing = [c for c in REQUIRED_TOOLBOX_COLUMNS if c not in ds.columns]
    if missing:
        raise ValueError("annotation.csv is missing required columns:\n" +
                         "\n".join(missing))

def export_toolbox_strict(df: pd.DataFrame, out_path: str):
    out = df.loc[:, REQUIRED_TOOLBOX_COLUMNS].copy()
    out["Verified"] = out["Verified"].astype(bool).map({True: "TRUE", False: "FALSE"})
    out.to_csv(out_path, index=False)


# ============================================================
# Main
# ============================================================
def main():
    args = get_args_via_gui()

    print("\n--- Zooniverse Workflows → Toolbox Linker ---\n")
    use_yn      = args["use_yn"]
    use_yn_exp  = args["use_yn_exp"]
    use_multi   = args["use_multi"]
    use_multi_exp = args["use_multi_exp"]

    any_yesno = use_yn or use_yn_exp
    any_multi = use_multi or use_multi_exp

    os.makedirs(args["outdir"], exist_ok=True)

    label_map = load_labelset_map(args["labelset"])
    manual_overrides: dict = {}

    ds = pd.read_csv(args["dataset"])
    ensure_required_columns(ds)

    zc = pd.read_csv(args["zoo"])
    z_meta = zc["subject_data"].apply(extract_subject_fields)
    z = pd.concat([zc, z_meta], axis=1)

    # ── Aggregate votes per selected workflow ─────────────────────────────────
    votes_yn     = aggregate_yesno_votes(z, WORKFLOW_YESNO,        "yn")     if use_yn      else None
    votes_yn_exp = aggregate_yesno_votes(z, WORKFLOW_YESNO_EXPERT, "yn_exp") if use_yn_exp  else None
    votes_m      = aggregate_multi_votes(z, WORKFLOW_MULTI,        "m")      if use_multi   else None
    votes_m_exp  = aggregate_multi_votes(z, WORKFLOW_MULTI_EXPERT, "m_exp")  if use_multi_exp else None

    # ── Build join keys ───────────────────────────────────────────────────────
    ds["source_image"] = ds["Name"].astype(str).str.replace(".jpg", "", regex=False)
    ds["Row_int"]    = pd.to_numeric(ds["Row"],    errors="coerce")
    ds["Column_int"] = pd.to_numeric(ds["Column"], errors="coerce")
    ds_keyed = ds.dropna(subset=["source_image", "Row_int", "Column_int"]).copy()

    key_cols = ["source_image", "Row_int", "Column_int"]
    keys_any = pd.concat(
        [v[key_cols] for v in [votes_yn, votes_yn_exp, votes_m, votes_m_exp]
         if v is not None],
        axis=0
    ).dropna().drop_duplicates()

    merged = ds_keyed.merge(keys_any, on=key_cols, how="inner")
    for v in [votes_yn, votes_yn_exp, votes_m, votes_m_exp]:
        if v is not None:
            merged = merged.merge(v, on=key_cols, how="left")

    # ── Numeric safety ────────────────────────────────────────────────────────
    int_cols  = ["yn_n", "yn_yes", "yn_no",
                 "yn_exp_n", "yn_exp_yes", "yn_exp_no",
                 "m_top_count", "m_n", "m_exp_top_count", "m_exp_n"]
    frac_cols = ["yn_yes_frac", "yn_no_frac",
                 "yn_exp_yes_frac", "yn_exp_no_frac",
                 "m_agreement", "m_exp_agreement"]
    for c in int_cols:
        if c in merged.columns:
            merged[c] = pd.to_numeric(merged[c], errors="coerce").fillna(0).astype(int)
    for c in frac_cols:
        if c in merged.columns:
            merged[c] = pd.to_numeric(merged[c], errors="coerce").fillna(0.0)

    # ── Consensus flags ───────────────────────────────────────────────────────
    def _yn_confirm(n_col, yes_frac_col):
        if n_col not in merged.columns:
            return pd.Series(False, index=merged.index)
        return ((merged[n_col] >= YESNO_AGREE_MIN_N) &
                (merged[yes_frac_col] >= YESNO_AGREE_MIN_FRAC))

    def _yn_deny(n_col, no_frac_col):
        if n_col not in merged.columns:
            return pd.Series(False, index=merged.index)
        return ((merged[n_col] >= YESNO_AGREE_MIN_N) &
                (merged[no_frac_col] >= YESNO_AGREE_MIN_FRAC))

    def _multi_consensus(top_label_col, top_count_col, agree_col):
        if top_label_col not in merged.columns:
            return pd.Series(False, index=merged.index)
        return (merged[top_label_col].notna() &
                (merged[top_count_col] >= MULTI_AGREE_MIN_N) &
                (merged[agree_col] >= MULTI_AGREE_MIN_FRAC))

    yn_confirm     = _yn_confirm("yn_n",     "yn_yes_frac")
    yn_exp_confirm = _yn_confirm("yn_exp_n", "yn_exp_yes_frac")
    yn_deny        = _yn_deny("yn_n",     "yn_no_frac")
    yn_exp_deny    = _yn_deny("yn_exp_n", "yn_exp_no_frac")

    multi_consensus     = _multi_consensus("m_top_label",     "m_top_count",     "m_agreement")
    multi_exp_consensus = _multi_consensus("m_exp_top_label", "m_exp_top_count", "m_exp_agreement")

    any_confirm = yn_confirm | yn_exp_confirm
    any_deny    = yn_deny    | yn_exp_deny
    any_multi   = multi_consensus | multi_exp_consensus

    # Status (for diagnostics — priority matches apply-updates order)
    merged["zoon_status"] = np.select(
        [multi_exp_consensus, multi_consensus,
         yn_exp_confirm, yn_confirm,
         any_deny],
        ["multi_expert", "multi_consensus",
         "confirm_expert", "confirm_pred",
         "deny_pred"],
        default="needs_more_votes"
    )

    # ── Apply label updates ───────────────────────────────────────────────────
    merged["Label"]      = "Review"
    merged["Long Label"] = "Review"
    merged["Verified"]   = False

    # 1) Multi consensus (expert takes precedence over crowd within multi)
    #    Apply crowd first, then expert overwrites — so expert wins on overlap
    for mask, label_col in [(multi_consensus, "m_top_label"),
                             (multi_exp_consensus, "m_exp_top_label")]:
        if mask.any():
            mapped = merged.loc[mask, label_col].apply(
                lambda x: pd.Series(
                    map_to_toolbox_codes(clean_markdown_labels(x),
                                        label_map, manual_overrides),
                    index=["tb_short", "tb_long"]
                )
            )
            merged.loc[mask, "Label"]      = mapped["tb_short"].values
            merged.loc[mask, "Long Label"] = mapped["tb_long"].values
            merged.loc[mask, "Verified"]   = True

    # Flag unmapped multi rows
    unmapped = any_multi & (
        merged["Label"].isna() |
        (merged["Label"].astype(str).str.len() == 0) |
        merged["Long Label"].isna() |
        (merged["Long Label"].astype(str).str.len() == 0)
    )
    merged.loc[unmapped, "Label"]      = "Review"
    merged.loc[unmapped, "Long Label"] = "Review"
    merged.loc[unmapped, "Verified"]   = False
    merged.loc[unmapped, "zoon_status"] = "multi_consensus_unmapped"

    # 2) Yes confirm (only where no multi consensus) → restore original Toolbox label
    yn_confirm_only = any_confirm & (~any_multi)
    if yn_confirm_only.any():
        orig = ds_keyed[key_cols + ["Label", "Long Label"]].copy()
        orig = orig.rename(columns={"Label": "_orig_Label",
                                    "Long Label": "_orig_Long"})
        merged = merged.merge(orig, on=key_cols, how="left")
        merged.loc[yn_confirm_only, "Label"]      = merged.loc[yn_confirm_only, "_orig_Label"]
        merged.loc[yn_confirm_only, "Long Label"] = merged.loc[yn_confirm_only, "_orig_Long"]
        merged.loc[yn_confirm_only, "Verified"]   = True
        merged = merged.drop(columns=["_orig_Label", "_orig_Long"], errors="ignore")

    # Everything else stays Review / False

    # ── Export toolbox CSV ────────────────────────────────────────────────────
    out_csv = os.path.join(args["outdir"], "toolbox_import.csv")
    export_toolbox_strict(merged, out_csv)

    # ── QA/QC report ─────────────────────────────────────────────────────────
    qa_cols = ["Name", "Row", "Column", "Label", "Long Label", "Verified", "zoon_status"]

    col_groups = [
        ("yn_n",          "yn_yes",         "yn_no",         "yn_yes_frac",     "yn_no_frac"),
        ("yn_exp_n",      "yn_exp_yes",     "yn_exp_no",     "yn_exp_yes_frac", "yn_exp_no_frac"),
        ("m_n",           "m_top_label",    "m_top_count",   "m_agreement",     None),
        ("m_exp_n",       "m_exp_top_label","m_exp_top_count","m_exp_agreement", None),
    ]
    for group in col_groups:
        for c in group:
            if c and c in merged.columns:
                qa_cols.append(c)

    qa_cols = [c for c in dict.fromkeys(qa_cols) if c in merged.columns]
    qa = merged[qa_cols].copy()

    rename_map = {
        "yn_n":              "yesno_n_votes",
        "yn_yes":            "yesno_n_yes",
        "yn_no":             "yesno_n_no",
        "yn_yes_frac":       "yesno_yes_frac",
        "yn_no_frac":        "yesno_no_frac",
        "yn_exp_n":          "yesno_expert_n_votes",
        "yn_exp_yes":        "yesno_expert_n_yes",
        "yn_exp_no":         "yesno_expert_n_no",
        "yn_exp_yes_frac":   "yesno_expert_yes_frac",
        "yn_exp_no_frac":    "yesno_expert_no_frac",
        "m_n":               "multi_n_votes",
        "m_top_label":       "multi_top_label",
        "m_top_count":       "multi_top_count",
        "m_agreement":       "multi_agreement",
        "m_exp_n":           "multi_expert_n_votes",
        "m_exp_top_label":   "multi_expert_top_label",
        "m_exp_top_count":   "multi_expert_top_count",
        "m_exp_agreement":   "multi_expert_agreement",
    }
    qa = qa.rename(columns={k: v for k, v in rename_map.items() if k in qa.columns})

    for c in ["yesno_yes_frac", "yesno_no_frac",
              "yesno_expert_yes_frac", "yesno_expert_no_frac",
              "multi_agreement", "multi_expert_agreement"]:
        if c in qa.columns:
            qa[c] = qa[c].round(3)

    def review_reason(row):
        status = row.get("zoon_status", "")
        if status in ("confirm_pred", "confirm_expert",
                      "multi_consensus", "multi_expert"):
            return ""
        if status == "deny_pred":
            return "denied in yes/no workflow"
        if status == "multi_consensus_unmapped":
            return "multi-choice consensus label not in labelset"

        reasons = []
        for prefix, label in [
            ("yesno",        "yes/no crowd"),
            ("yesno_expert", "yes/no expert"),
            ("multi",        "multi crowd"),
            ("multi_expert", "multi expert"),
        ]:
            n = row.get(f"{prefix}_n_votes", 0) or 0
            if n > 0:
                min_n = YESNO_AGREE_MIN_N if "yesno" in prefix else MULTI_AGREE_MIN_N
                min_f = YESNO_AGREE_MIN_FRAC if "yesno" in prefix else MULTI_AGREE_MIN_FRAC
                if n < min_n:
                    reasons.append(f"{label}: only {int(n)} vote(s) (need {min_n})")
                else:
                    frac_col = (f"{prefix}_yes_frac" if "yesno" in prefix
                                else f"{prefix}_agreement")
                    agree = row.get(frac_col, 0) or 0
                    if agree < min_f:
                        reasons.append(
                            f"{label}: agreement {agree:.0%} below {min_f:.0%}")
        if not reasons:
            reasons.append("no classifications found")
        return "; ".join(reasons)

    qa["review_reason"] = qa.apply(review_reason, axis=1)

    label_source_map = {
        "multi_expert":            "Expert multi-choice consensus",
        "multi_consensus":         "Volunteer multi-choice consensus",
        "confirm_expert":          "Confirmed by expert yes/no vote",
        "confirm_pred":            "Confirmed by volunteer yes/no vote",
        "deny_pred":               "Review - denied in yes/no workflow",
        "multi_consensus_unmapped":"Review - multi-choice label not in labelset",
        "needs_more_votes":        "Review - insufficient votes",
    }
    qa["label_source"] = qa["zoon_status"].map(label_source_map).fillna("Review - unknown")

    qa_path = os.path.join(args["outdir"], "qaqc_classifications.csv")
    qa.to_csv(qa_path, index=False)

    # ── Unmapped label report ─────────────────────────────────────────────────
    unmapped_rows = merged[merged["zoon_status"] == "multi_consensus_unmapped"].copy()
    if len(unmapped_rows) > 0:
        raw_col = next((c for c in ["m_exp_top_label", "m_top_label"]
                        if c in unmapped_rows.columns), None)
        if raw_col:
            vc = unmapped_rows[raw_col].astype(str).value_counts()
            report = pd.DataFrame({"raw_label": vc.index, "count": vc.values})
            unmapped_path = os.path.join(args["outdir"],
                                         "unmapped_multi_consensus_labels.csv")
            report.to_csv(unmapped_path, index=False)

    # ── Summary ───────────────────────────────────────────────────────────────
    print("Done.")
    print("Wrote:", out_csv)
    print("Wrote QA/QC report:", qa_path)
    print(f"Rows kept: {len(merged)}")
    print("\nzoon_status counts:")
    print(merged["zoon_status"].value_counts(dropna=False))
    if len(unmapped_rows) > 0:
        print("\nWARNING: Some multi-consensus labels could not be mapped to the labelset.")
        print("Add these to manual_overrides or expansions in map_to_toolbox_codes().")


if __name__ == "__main__":
    main()
