"""
Duplicate Caesar workflow rules from one Zooniverse workflow to another.

Copies extractors, reducers, rules, and rule effects from SOURCE_WORKFLOW_ID
to TARGET_WORKFLOW_ID using credentials from config.env.

Requirements:
    pip install panoptes-client requests
"""

import json
import sys
import time
from pathlib import Path

import requests
from panoptes_client import Panoptes

RETRY_ATTEMPTS = 3
RETRY_DELAY = 5  # seconds between retries

CAESAR_BASE = "https://caesar.zooniverse.org"
SOURCE_WORKFLOW_ID = 30787
TARGET_WORKFLOW_ID = 32022


# ---------------------------------------------------------------------------
# Config loading
# ---------------------------------------------------------------------------

def load_config():
    config_path = Path(__file__).parent / "config.env"
    config = {}
    with open(config_path) as f:
        for line in f:
            line = line.strip()
            if line and "=" in line and not line.startswith("#"):
                key, _, value = line.partition("=")
                config[key.strip()] = value.strip()
    return config


# ---------------------------------------------------------------------------
# Auth
# ---------------------------------------------------------------------------

def get_caesar_headers():
    config = load_config()
    username = config["ZOONIVERSE_USERNAME"]
    password = config["ZOONIVERSE_PASSWORD"]
    print(f"Logging in as '{username}'...")
    Panoptes.connect(username=username, password=password)
    client = Panoptes.client()
    client.login(username=username, password=password)
    client.get_bearer_token()
    token = client.bearer_token
    if not token:
        raise RuntimeError("Login succeeded but bearer_token is still empty.")
    print(f"  Token retrieved (length={len(token)})")
    return {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json",
        "Accept": "application/json",
    }


# ---------------------------------------------------------------------------
# Fetch helpers
# ---------------------------------------------------------------------------

def get(url, headers):
    for attempt in range(1, RETRY_ATTEMPTS + 1):
        r = requests.get(url, headers=headers)
        if r.ok:
            return r.json()
        print(f"  ERROR {r.status_code} GET {url} (attempt {attempt}/{RETRY_ATTEMPTS})")
        print(f"  Response: {r.text[:500]}")
        if attempt < RETRY_ATTEMPTS:
            print(f"  Retrying in {RETRY_DELAY}s...")
            time.sleep(RETRY_DELAY)
    r.raise_for_status()


def post(url, payload, headers):
    for attempt in range(1, RETRY_ATTEMPTS + 1):
        r = requests.post(url, json=payload, headers=headers)
        if r.ok:
            return r.json()
        print(f"  ERROR {r.status_code} POST {url} (attempt {attempt}/{RETRY_ATTEMPTS})")
        print(f"  Response: {r.text[:500]}")
        if attempt < RETRY_ATTEMPTS:
            print(f"  Retrying in {RETRY_DELAY}s...")
            time.sleep(RETRY_DELAY)
    r.raise_for_status()


# ---------------------------------------------------------------------------
# Copy logic
# ---------------------------------------------------------------------------

def condition_to_array(cond):
    """Convert Caesar's GET dict format to the nested-array format required for POST.

    GET returns dicts like:
      {'operations': [{'type': 'gte', 'operations': [{'key': 'x', 'absent_val': 0}, {'value': 7}]}]}
    POST expects arrays like:
      ["and", ["gte", ["lookup", "x", 0], ["const", 7]]]
    """
    if not isinstance(cond, dict):
        return cond
    if "key" in cond:
        return ["lookup", cond["key"], cond.get("absent_val", 0)]
    if "value" in cond:
        return ["const", cond["value"]]
    sub_ops = [condition_to_array(op) for op in cond.get("operations", [])]
    op_type = cond.get("type")
    if op_type:
        return [op_type] + sub_ops
    # root node with no type = implicit "and"
    return ["and"] + sub_ops if len(sub_ops) > 1 else sub_ops[0]


def as_list(data):
    """Caesar returns bare lists or single objects — normalise to a list."""
    if isinstance(data, list):
        return data
    if isinstance(data, dict):
        for v in data.values():
            if isinstance(v, list):
                return v
    return [data]


def infer_extractor_type(config):
    """Caesar's GET API omits the extractor type; infer from config fields.
    Valid types: blank, external, question, survey, who, pluck_field, shape
    Override EXTRACTOR_TYPE_OVERRIDES (keyed by extractor key) if any are wrong.
    """
    if "url" in config:
        return "external"
    if "field_name" in config:
        return "pluck_field"
    if "task_key" in config:
        # question and survey extractors both use task_key + if_missing.
        # Default to question; add to EXTRACTOR_TYPE_OVERRIDES if yours are survey.
        return "question"
    return "question"


def infer_reducer_type(config):
    """Caesar's GET API omits the reducer type; infer from config fields.
    Valid types: consensus, count, placeholder, external, first_extract,
                 stats, summary_stats, unique_count, rectangle, sqs
    Override REDUCER_TYPE_OVERRIDES (keyed by reducer key) if any are wrong.
    """
    if "url" in config:
        return "external"
    if "summary" in config or "percentile" in config:
        return "summary_stats"
    # count is the most common for per-key classification counting
    return "count"


# Override inferred types here if needed, keyed by extractor/reducer key name.
# e.g. EXTRACTOR_TYPE_OVERRIDES = {"mobile": "survey_extractor"}
EXTRACTOR_TYPE_OVERRIDES: dict = {}
REDUCER_TYPE_OVERRIDES: dict = {}


def existing_keys(url, headers):
    """Return the set of key names already present on the target workflow."""
    items = as_list(get(url, headers))
    return {item["key"] for item in items if "key" in item}


def copy_extractors(src_id, tgt_id, headers):
    extractors = as_list(get(f"{CAESAR_BASE}/workflows/{src_id}/extractors", headers))
    already = existing_keys(f"{CAESAR_BASE}/workflows/{tgt_id}/extractors", headers)
    print(f"\nCopying {len(extractors)} extractor(s) ({len(already)} already exist on target)...")
    for ex in extractors:
        if ex["key"] in already:
            print(f"  Skipping extractor '{ex['key']}' (already exists)")
            continue
        config = ex.get("config", {})
        ex_type = EXTRACTOR_TYPE_OVERRIDES.get(ex["key"]) or infer_extractor_type(config)
        payload = {
            "extractor": {
                "type":   ex_type,
                "key":    ex["key"],
                "config": config,
                "minimum_workflow_version": ex.get("minimum_workflow_version"),
            }
        }
        result = post(f"{CAESAR_BASE}/workflows/{tgt_id}/extractors", payload, headers)
        new_id = as_list(result)[0].get("id", "?")
        print(f"  Created extractor '{ex['key']}' (type={ex_type}) -> id {new_id}")


def copy_reducers(src_id, tgt_id, headers):
    reducers = as_list(get(f"{CAESAR_BASE}/workflows/{src_id}/reducers", headers))
    already = existing_keys(f"{CAESAR_BASE}/workflows/{tgt_id}/reducers", headers)
    print(f"\nCopying {len(reducers)} reducer(s) ({len(already)} already exist on target)...")
    for rd in reducers:
        if rd["key"] in already:
            print(f"  Skipping reducer '{rd['key']}' (already exists)")
            continue
        config = rd.get("config", {})
        rd_type = REDUCER_TYPE_OVERRIDES.get(rd["key"]) or infer_reducer_type(config)
        payload = {
            "reducer": {
                "type":    rd_type,
                "key":     rd["key"],
                "config":  config,
                "filters": rd.get("filters", {}),
            }
        }
        result = post(f"{CAESAR_BASE}/workflows/{tgt_id}/reducers", payload, headers)
        new_id = as_list(result)[0].get("id", "?")
        print(f"  Created reducer '{rd['key']}' (type={rd_type}) -> id {new_id}")


def copy_rules_and_effects(src_id, tgt_id, headers):
    rules = as_list(get(f"{CAESAR_BASE}/workflows/{src_id}/subject_rules", headers))
    existing = as_list(get(f"{CAESAR_BASE}/workflows/{tgt_id}/subject_rules", headers))
    existing_conditions = {json.dumps(r.get("condition"), sort_keys=True) for r in existing}
    print(f"\nCopying {len(rules)} subject rule(s) ({len(existing)} already exist on target)...")
    for rule in rules:
        rule_cond_key = json.dumps(rule.get("condition"), sort_keys=True)
        if rule_cond_key in existing_conditions:
            print(f"  Skipping rule (already exists)")
            continue
        condition_arr = condition_to_array(rule.get("condition", {}))
        payload = {
            "subject_rule": {
                "condition_string": json.dumps(condition_arr),
                "topic":            rule.get("topic", "evaluate_by_subject"),
            }
        }
        result = post(f"{CAESAR_BASE}/workflows/{tgt_id}/subject_rules", payload, headers)
        new_rule_id = as_list(result)[0].get("id", "?")
        print(f"  Created rule id {new_rule_id} (condition: {rule.get('condition_string', '')[:60]})")

        effects = as_list(get(
            f"{CAESAR_BASE}/workflows/{src_id}/subject_rules/{rule['id']}/subject_rule_effects",
            headers
        ))
        for ef in effects:
            fx_payload = {
                "subject_rule_effect": {
                    "action": ef.get("action", ef.get("type", "")),
                    "config": ef.get("config", {}),
                }
            }
            fx_result = post(
                f"{CAESAR_BASE}/workflows/{tgt_id}/subject_rules/{new_rule_id}/subject_rule_effects",
                fx_payload, headers
            )
            new_fx_id = as_list(fx_result)[0].get("id", "?")
            print(f"    Created effect '{ef.get('action', ef.get('type', '?'))}' -> id {new_fx_id}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    target = TARGET_WORKFLOW_ID

    print(f"\nSource workflow: {SOURCE_WORKFLOW_ID}")
    print(f"Target workflow: {target}")
    confirm = input("\nProceed? (y/n): ").strip().lower()
    if confirm != "y":
        print("Aborted.")
        sys.exit(0)

    headers = get_caesar_headers()

    # First confirm the API is reachable and find the source workflow
    print(f"\nChecking source workflow {SOURCE_WORKFLOW_ID} in Caesar...")
    src = get(f"{CAESAR_BASE}/workflows/{SOURCE_WORKFLOW_ID}", headers)
    print(f"  Found: {src}")

    copy_extractors(SOURCE_WORKFLOW_ID, target, headers)
    copy_reducers(SOURCE_WORKFLOW_ID, target, headers)
    copy_rules_and_effects(SOURCE_WORKFLOW_ID, target, headers)

    print("\nDone! Caesar configuration copied successfully.")


if __name__ == "__main__":
    main()
