# Data Trimming Process

This is the exact process used to trim the local `data/` folder in this repo so the same workflow can be repeated in another clone.

## Goal

Remove redundant raw-data directories that are not needed by the current analysis script, then reduce 2012 to the two files the script actually reads.

- ENIGHUR 2012 redundant mirrors:
  - `data/enighur/2012/bbd_ingresos_gastos_2011-2012/2011-2012/Tablas_Primarias`
  - `data/enighur/2012/bbd_ingresos_gastos_2011-2012/2011-2012/Tablas_Trabajo`
- ENIGHUR 2025 primary raw bases not used by the current script:
  - `data/enighur/2025/Enighur_Bases_de_datos_R/Bases Primarias`

The current script path dependencies are:

- 2012 reads only:
  - `data/enighur/2012/required/ENIGHUR11_INGRESOS_H.sav`
  - `data/enighur/2012/required/ENIGHUR11_GASTOS_HMO.sav`
- 2025 reads from `Enighur_Bases_de_datos_R/Bases de trabajo/Bases_trabajo_R/Bases_trabajo.RData`

So after copying the two required 2012 files into `data/enighur/2012/required/`, the old 2012 vendor tree can be removed without breaking `scripts/01_ingresos_hogares.R`.

## 1. Confirm what the script actually uses

Read the script and locate the hard-coded data paths:

```powershell
rg -n "data_path_2012|gastos_hmo_path|rdata_path_2025" scripts/01_ingresos_hogares.R
```

Expected result in this repo before simplification:

- 2012 points into `Ingresos_Gastos/02 BASE DE DATOS/...`
- 2025 points into `Bases de trabajo/Bases_trabajo_R/Bases_trabajo.RData`

After simplification, update the script so 2012 points into:

- `data/enighur/2012/required/ENIGHUR11_INGRESOS_H.sav`
- `data/enighur/2012/required/ENIGHUR11_GASTOS_HMO.sav`

## 2. Inspect the 2012 folder layout

List the top-level 2012 raw-data directories:

```powershell
Get-ChildItem data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012 -Directory |
  Select-Object Name, FullName
```

In this repo that returned:

- `Ingresos_Gastos`
- `Tablas_Primarias`
- `Tablas_Trabajo`

This is the first sign of likely duplication.

## 3. Prove the duplication with hashes

Do not delete based only on similar names. Hash matching files from the mirrored trees.

Example check for a work-table file:

```powershell
Get-FileHash `
  "data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Tablas_Trabajo\02 TABLAS DE TRABAJO\03 ENIGHUR11_INGRESOS_V.sav", `
  "data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Ingresos_Gastos\02 BASE DE DATOS\02 TABLAS DE TRABAJO\03 ENIGHUR11_INGRESOS_V.sav" |
  Format-List Path, Hash
```

Example check for a primary-table file:

```powershell
Get-FileHash `
  "data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Tablas_Primarias\01 TABLAS PRIMARIAS\28 ENIGHUR11_GDIARIOS_SECCION2.sav", `
  "data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Ingresos_Gastos\02 BASE DE DATOS\01 TABLAS PRIMARIAS\28 ENIGHUR11_GDIARIOS_SECCION2.sav" |
  Format-List Path, Hash
```

In this repo, the compared file pairs had identical SHA256 hashes, which confirmed real byte-for-byte duplication.

## 4. Inspect the 2025 folder layout

List the 2025 ENIGHUR raw-data directories:

```powershell
Get-ChildItem data\enighur\2025 -Directory | Select-Object Name, FullName
Get-ChildItem data\enighur\2025\Enighur_Bases_de_datos_R -Recurse -Directory |
  Where-Object { $_.Name -match "Primari|trabajo|BASE" } |
  Select-Object FullName
```

In this repo, that showed both:

- `Bases de trabajo`
- `Bases Primarias`

The current script only uses `Bases de trabajo/.../Bases_trabajo.RData`, not `Bases Primarias`.

## 5. Measure the directories before deleting

Use this exact command:

```powershell
$targets=@(
  'data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Tablas_Primarias',
  'data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Tablas_Trabajo',
  'data\enighur\2025\Enighur_Bases_de_datos_R\Bases Primarias'
)
$rows = foreach($t in $targets){
  if(Test-Path $t){
    $m = Get-ChildItem $t -Recurse -Force -File -ErrorAction SilentlyContinue |
      Measure-Object Length -Sum
    [PSCustomObject]@{
      Path = $t
      Files = $m.Count
      SizeGB = [math]::Round(($m.Sum / 1GB), 2)
    }
  }
}
$rows | Format-Table -AutoSize
```

When I ran it after an initial partial delete in this repo, the remaining sizes were:

- `Tablas_Primarias`: `18` files, about `0.49 GB`
- `Tablas_Trabajo`: `0` files, effectively already cleared
- `Bases Primarias`: `2` files, about `0.08 GB`

## 6. Create a slim 2012 input folder

Copy only the two 2012 files the script actually uses:

```powershell
$dest='data\enighur\2012\required'
New-Item -ItemType Directory -Path $dest -Force | Out-Null
Copy-Item -LiteralPath 'data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Ingresos_Gastos\02 BASE DE DATOS\02 TABLAS DE TRABAJO\04 ENIGHUR11_INGRESOS_H.sav' -Destination (Join-Path $dest 'ENIGHUR11_INGRESOS_H.sav') -Force
Copy-Item -LiteralPath 'data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Ingresos_Gastos\02 BASE DE DATOS\02 TABLAS DE TRABAJO\08 ENIGHUR11_GASTOS_HMO.sav' -Destination (Join-Path $dest 'ENIGHUR11_GASTOS_HMO.sav') -Force
```

Then update:

- `scripts/01_ingresos_hogares.R`
- `scripts/check.R`

so they point at `data/enighur/2012/required/`.

## 7. Delete only the confirmed redundant targets

Use:

```powershell
$targets=@(
  'data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Tablas_Primarias',
  'data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Tablas_Trabajo',
  'data\enighur\2025\Enighur_Bases_de_datos_R\Bases Primarias'
)
foreach($t in $targets){
  if(Test-Path $t){
    Remove-Item -LiteralPath $t -Recurse -Force
  }
}
```

## 8. If Windows denies some files, clear attributes and rerun elevated

In this repo, several `.sav`, `.RData`, and `.xlsx` files raised `Access denied` even with `-Force`.

The cleanup procedure that worked was:

1. Run the delete once normally.
2. Re-measure the targets.
3. In an elevated shell, clear file attributes recursively and then rerun the delete.

Use the same target list as step 6. Do not widen the delete path.

Exact elevated command:

```powershell
$targets=@(
  'data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Tablas_Primarias',
  'data\enighur\2012\bbd_ingresos_gastos_2011-2012\2011-2012\Tablas_Trabajo',
  'data\enighur\2025\Enighur_Bases_de_datos_R\Bases Primarias'
)
foreach($t in $targets){
  if(Test-Path $t){
    attrib -R -S -H "$t" /S /D
    Remove-Item -LiteralPath $t -Recurse -Force -ErrorAction Stop
  }
}
```

## 9. Verify that the analysis still runs

Run the cheap check first:

```powershell
Rscript scripts/check.R
```

Then run the full script:

```powershell
Rscript scripts/01_ingresos_hogares.R
```

In this repo, the script still ran successfully after the data-trimming work.

## 10. Current state in this repo

At the point this document was written:

- `data/enighur/2012/.../Tablas_Primarias` is gone
- `data/enighur/2012/.../Tablas_Trabajo` is gone
- `data/enighur/2025/Enighur_Bases_de_datos_R/Bases Primarias` is gone
- `data/enighur/2012/required/` contains only the two `.sav` files the script reads

The remaining retained directories are:

- `data/enighur/2012/required`
- `data/enighur/2025/Enighur_Bases_de_datos_R/Bases de trabajo`

## 11. Rules for repeating this elsewhere

- Always prove duplication with hashes before deleting mirrored trees.
- For 2012 in this project, keep only the two `.sav` files the script reads.
- Only delete directories that are not referenced by the current scripts.
- Prefer deleting mirrored siblings rather than rewriting script paths at the same time.
- Re-measure after deletion so you know what actually came off disk.
- If Windows blocks deletion, rerun the exact same path list elevated after clearing `R/S/H` attributes instead of improvising.
