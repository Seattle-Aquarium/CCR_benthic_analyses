#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
remove_duplicate_images.py — find and remove byte-for-byte identical
images within a folder.

Typical case: an image got saved twice (e.g. via unique_filename() in
extract_training_patches.py / toolbox_to_subjects.py), producing a
"..._filename__2.jpg" that is byte-identical to the original
"..._filename.jpg". This script finds those and removes the extras.

Duplicates are detected by content hash (SHA-256), not filename — the
__2/__3 naming pattern is only used to break ties when picking which
file in a duplicate group to KEEP (the shortest / non-suffixed name
wins), not to decide what counts as a duplicate.

For each group of identical files, one file is kept and the rest are
either:
    - moved to a "_duplicates_removed" folder (default, reversible), or
    - permanently deleted (only if explicitly selected)

A CSV report of every group found is always written, even in dry run.

Usage:
    python remove_duplicate_images.py

    A window opens to select:
      1. Folder to scan (required)
      2. Include subfolders (optional, default off)
      3. Dry run checkbox (default ON — report only, no files touched)
      4. Permanently delete instead of moving to _duplicates_removed
         (default off — moving is reversible, deleting is not)
"""

import hashlib
import json
import logging
import os
import shutil
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path
from collections import defaultdict

# ── Logging ──────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("remove_duplicate_images_log.txt", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)

_CONFIG_PATH = Path(__file__).parent / ".remove_duplicate_images_config.json"

IMAGE_EXTENSIONS = {".jpg", ".jpeg", ".png", ".tif", ".tiff", ".bmp", ".gif"}


def _load_gui_config() -> dict:
    try:
        with open(_CONFIG_PATH, "r", encoding="utf-8") as f:
            return json.load(f)
    except Exception:
        return {}


def _save_gui_config(cfg: dict) -> None:
    try:
        with open(_CONFIG_PATH, "w", encoding="utf-8") as f:
            json.dump(cfg, f, indent=2)
    except Exception:
        pass


# ============================================================
# HASHING
# ============================================================
def file_hash(path: str, chunk_size: int = 1024 * 1024) -> str:
    """SHA-256 hash of a file's full contents (byte-for-byte comparison)."""
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(chunk_size), b""):
            h.update(chunk)
    return h.hexdigest()


def find_images(folder: str, recursive: bool) -> list:
    root = Path(folder)
    walker = root.rglob("*") if recursive else root.glob("*")
    return [
        str(p) for p in walker
        if p.is_file() and p.suffix.lower() in IMAGE_EXTENSIONS
    ]


# ============================================================
# KEEPER SELECTION
# ============================================================
def pick_keeper(paths: list) -> str:
    """
    Given a group of byte-identical file paths, pick which one to keep.

    Preference order:
      1. Filename does not end in a "__<number>" suffix (the collision
         marker used by unique_filename() elsewhere in this pipeline)
      2. Shortest filename
      3. Alphabetically first (stable tiebreak)
    """
    import re
    suffix_re = re.compile(r"__\d+$")

    def sort_key(p):
        stem = Path(p).stem
        has_suffix = bool(suffix_re.search(stem))
        return (has_suffix, len(os.path.basename(p)), p)

    return sorted(paths, key=sort_key)[0]


# ============================================================
# CORE SCAN
# ============================================================
def scan_for_duplicates(folder: str, recursive: bool) -> dict:
    """
    Returns a dict: {hash: [file paths]} restricted to groups with 2+ files.
    """
    images = find_images(folder, recursive)
    log.info(f"Found {len(images):,} image file(s) to check "
             f"({'recursive' if recursive else 'this folder only'})")

    by_hash = defaultdict(list)
    for i, path in enumerate(images, 1):
        if i % 500 == 0:
            log.info(f"  hashed {i:,}/{len(images):,}...")
        try:
            by_hash[file_hash(path)].append(path)
        except OSError as e:
            log.warning(f"  Could not read {path}: {e}")

    duplicate_groups = {h: paths for h, paths in by_hash.items() if len(paths) > 1}
    log.info(f"Found {len(duplicate_groups):,} group(s) of identical images "
             f"({sum(len(v) - 1 for v in duplicate_groups.values()):,} "
             f"removable duplicate file(s))")
    return duplicate_groups


def write_report_csv(duplicate_groups: dict, folder: str, keeper_of: dict) -> str:
    import csv
    out_path = os.path.join(folder, "duplicate_images_report.csv")
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["sha256", "kept_path", "removed_path"])
        for h, paths in duplicate_groups.items():
            keeper = keeper_of[h]
            for p in paths:
                if p != keeper:
                    writer.writerow([h, keeper, p])
    return out_path


def remove_duplicates(duplicate_groups: dict, folder: str, dry_run: bool,
                       permanent_delete: bool) -> dict:
    keeper_of = {h: pick_keeper(paths) for h, paths in duplicate_groups.items()}

    stats = {
        "groups": len(duplicate_groups),
        "removed": 0,
        "failed": 0,
        "dry_run": dry_run,
        "permanent_delete": permanent_delete,
        "report_path": None,
        "moved_to": None,
    }

    report_path = write_report_csv(duplicate_groups, folder, keeper_of)
    stats["report_path"] = report_path
    log.info(f"Wrote report: {report_path}")

    if dry_run:
        for h, paths in duplicate_groups.items():
            keeper = keeper_of[h]
            for p in paths:
                if p != keeper:
                    log.info(f"  [DRY RUN] would remove: {p}  (keeping: {keeper})")
        return stats

    dup_folder = None
    if not permanent_delete:
        dup_folder = os.path.join(folder, "_duplicates_removed")
        Path(dup_folder).mkdir(exist_ok=True)
        stats["moved_to"] = dup_folder

    for h, paths in duplicate_groups.items():
        keeper = keeper_of[h]
        for p in paths:
            if p == keeper:
                continue
            try:
                if permanent_delete:
                    os.remove(p)
                    log.info(f"  Deleted: {p}  (kept: {keeper})")
                else:
                    dest = os.path.join(dup_folder, os.path.basename(p))
                    base, ext = os.path.splitext(dest)
                    n = 2
                    while os.path.exists(dest):
                        dest = f"{base}__{n}{ext}"
                        n += 1
                    shutil.move(p, dest)
                    log.info(f"  Moved: {p} -> {dest}  (kept: {keeper})")
                stats["removed"] += 1
            except OSError as e:
                log.warning(f"  Failed to remove {p}: {e}")
                stats["failed"] += 1

    return stats


# ============================================================
# GUI
# ============================================================
def get_args_via_gui():
    result = {}
    cfg = _load_gui_config()

    root = tk.Tk()
    root.title("Remove Duplicate Images")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Remove Duplicate Images",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Finds byte-for-byte identical images (e.g. __2 copies) and removes them",
              foreground="grey").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    ttk.Label(root, text="Folder to scan:").grid(row=3, column=0, sticky="e", **pad)
    folder_var = tk.StringVar(value=cfg.get("folder", ""))
    ttk.Entry(root, textvariable=folder_var, width=55).grid(row=3, column=1, **pad)

    def browse_folder():
        p = filedialog.askdirectory(title="Select folder to scan for duplicate images")
        if p:
            folder_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_folder).grid(row=3, column=2, **pad)

    recursive_var = tk.BooleanVar(value=cfg.get("recursive", False))
    ttk.Checkbutton(
        root, text="Include subfolders",
        variable=recursive_var,
    ).grid(row=4, column=0, columnspan=3, pady=(0, 4))

    ttk.Separator(root, orient="horizontal").grid(
        row=5, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    permanent_var = tk.BooleanVar(value=cfg.get("permanent_delete", False))
    ttk.Checkbutton(
        root,
        text="Permanently delete duplicates (instead of moving them to\n"
             "_duplicates_removed/ inside the scanned folder)",
        variable=permanent_var,
    ).grid(row=6, column=0, columnspan=3, pady=(0, 4))

    dry_run_var = tk.BooleanVar(value=cfg.get("dry_run", True))
    ttk.Checkbutton(
        root,
        text="Dry run  (write the report only — don't move or delete anything)",
        variable=dry_run_var,
    ).grid(row=7, column=0, columnspan=3, pady=(0, 4))

    ttk.Separator(root, orient="horizontal").grid(
        row=8, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=9, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        folder = folder_var.get().strip()
        if not folder:
            messagebox.showerror("Missing input", "Please select a folder to scan.")
            return
        if not Path(folder).is_dir():
            messagebox.showerror("Folder not found", f"Folder not found:\n{folder}")
            return

        result["folder"] = folder
        result["recursive"] = recursive_var.get()
        result["dry_run"] = dry_run_var.get()
        result["permanent_delete"] = permanent_var.get()
        result["submitted"] = True

        _save_gui_config({
            "folder": folder,
            "recursive": recursive_var.get(),
            "dry_run": dry_run_var.get(),
            "permanent_delete": permanent_var.get(),
        })
        root.destroy()

    def on_cancel():
        root.destroy()

    ttk.Button(btn_frame, text="  Run  ", command=on_run).pack(side="left", padx=8)
    ttk.Button(btn_frame, text="Cancel", command=on_cancel).pack(side="left", padx=8)

    root.mainloop()

    if not result.get("submitted"):
        print("Cancelled by user.")
        sys.exit(0)

    return result


# ============================================================
# ENTRY POINT
# ============================================================
def main():
    args = get_args_via_gui()

    log.info("Remove Duplicate Images")
    log.info(f"  folder            : {args['folder']}")
    log.info(f"  include subfolders: {args['recursive']}")
    if args["dry_run"]:
        log.info("  mode              : DRY RUN")
    else:
        log.info(f"  mode              : "
                  f"{'PERMANENT DELETE' if args['permanent_delete'] else 'MOVE to _duplicates_removed/'}")

    duplicate_groups = scan_for_duplicates(args["folder"], args["recursive"])

    if not duplicate_groups:
        messagebox.showinfo("No duplicates found",
                             "No byte-for-byte identical images were found.")
        return

    stats = remove_duplicates(
        duplicate_groups, args["folder"],
        dry_run=args["dry_run"],
        permanent_delete=args["permanent_delete"],
    )

    n_removable = sum(len(v) - 1 for v in duplicate_groups.values())

    if stats["dry_run"]:
        lines = [
            "DRY RUN — no files moved or deleted.",
            "",
            f"Duplicate groups found: {stats['groups']:,}",
            f"Duplicate files that would be removed: {n_removable:,}",
            "",
            f"Report written to:\n{stats['report_path']}",
        ]
        messagebox.showinfo("Dry run complete", "\n".join(lines))
    else:
        lines = [
            f"Duplicate groups found: {stats['groups']:,}",
            f"Files removed: {stats['removed']:,}",
        ]
        if stats["failed"]:
            lines.append(f"⚠ Failed to remove: {stats['failed']:,} (see log)")
        if stats["moved_to"]:
            lines.append(f"\nMoved to:\n{stats['moved_to']}")
        else:
            lines.append("\nDuplicates were permanently deleted.")
        lines.append(f"\nReport written to:\n{stats['report_path']}")
        if stats["failed"]:
            messagebox.showwarning("Done", "\n".join(lines))
        else:
            messagebox.showinfo("Done", "\n".join(lines))


if __name__ == "__main__":
    main()
