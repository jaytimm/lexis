# Norming Instructions Handoff

This document describes how downstream "LLM-as-participant" prompts should consume `data-raw/_build/norming_instructions.jsonl`.

## Input Contract

- Canonical records: `data-raw/_build/norming_instructions.jsonl`
- Schema: `data-raw/norming_instructions_schema.json`
- One JSON object per instruction block.

Required fields:

- `dataset`
- `source_pdf`
- `source_pages`
- `instruction_id`
- `instruction_text_verbatim`
- `instruction_type`
- `status`
- `notes`

## Prompt Assembly Rules

- Filter to `status == "extracted"` for default task runs.
- Include `status == "ambiguous"` only when no extracted block exists for that task.
- Inject `instruction_text_verbatim` exactly as provided (no rewriting).
- Preserve punctuation and scale anchors from source text.
- Add task metadata to the system/developer prompt only from structured fields (`dataset`, `instruction_type`, `source_pdf`, `source_pages`), not by paraphrasing instruction text.

## Recommended Runtime Selection

Given a requested dataset:

1. Select all records where `dataset` matches.
2. Prefer `extracted` records over `ambiguous`.
3. If multiple records remain, order by:
   1. `instruction_type` priority (`word_rating_instruction`, `semantic_rating_instruction`, task-specific main prompt types first),
   2. then stable `instruction_id` alphabetical order.
4. Concatenate selected `instruction_text_verbatim` blocks with a clear separator.

## Minimal Prompt Template

Use this template for simulation runs:

```text
You are completing a psycholinguistic norming task as a participant.
Follow the task instructions exactly as written.

Task: {dataset}
Source: {source_pdf} (pages {source_pages})

Instructions (verbatim):
{instruction_text_verbatim}
```

## QA Guardrails Before Running LLM Participants

- Reject records with empty `instruction_text_verbatim`.
- Reject records where `status == "not_found"` for the target dataset.
- Log `instruction_id`, `source_pdf`, and `source_pages` with each generated response for auditability.
