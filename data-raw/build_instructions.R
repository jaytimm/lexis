jsonl_path <- "data-raw/_build/norming_instructions.jsonl"
json_lines <- readLines(jsonl_path, warn = FALSE, encoding = "UTF-8")
json_lines <- json_lines[nzchar(trimws(json_lines))]

norming_instructions <-
  dplyr::bind_rows(lapply(json_lines, jsonlite::fromJSON)) |>
  dplyr::transmute(
    dataset,
    instruction_id,
    instruction_type,
    status,
    source_pdf,
    source_pages,
    instruction_text_verbatim,
    notes
  ) |>
  dplyr::arrange(dataset, instruction_id)

usethis::use_data(norming_instructions, overwrite = TRUE)
