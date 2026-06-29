# MTPRstudio Project Instructions

## Text generation in the report

When writing or generating any text to be inserted into `report/MTP3.qmd`, wrap generated content in blue using inline raw LaTeX color switches:

```
`\color{blue}`{=latex}Generated content here.`\color{black}`{=latex}
```

For paragraphs that contain cross-references (e.g. `@tbl-something`), split the color switches around the reference:

```
`\color{blue}`{=latex}Text before reference.`\color{black}`{=latex} @tbl-something `\color{blue}`{=latex}Text after reference.`\color{black}`{=latex}
```

For tables, always use plain Quarto markdown pipe tables, never knitr/kableExtra in jou mode.

## Backup

When the user says "backup", run the following command to copy the entire project to iCloud:

```bash
rsync -a --delete \
  --exclude='.git/' \
  --exclude='.Rproj.user/' \
  --exclude='.quarto/' \
  --exclude='.DS_Store' \
  ~/Projects/MTPRstudio/ \
  "/Users/minkveltman/Library/Mobile Documents/com~apple~CloudDocs/cloud/MTPRstudio/"
```

`.git/` is excluded to prevent iCloud sync conflicts with git objects. The iCloud copy is a file backup only, not a git backup.
