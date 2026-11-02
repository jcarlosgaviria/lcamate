# lcar
## Fase 1 (cerrada 2026-10-28): v0.0.0.9001, check 0/0/0 en el equipo del autor.
## Fase 2a (2026-10-29): lectores. v0.0.0.9002
- read_lca_csv(), read_uslci() (JSON-LD, reglas R1/R2, completitud, unidades de la BD).
- Verificado en WebR (R 4.6.0): lector R == implementacion Python independiente sobre USLCI completo
  (61637 filas identicas, dif. relativa max 9.7e-13, completitud identica) y sobre mini-BD empaquetada.
- Subgrafo de 583 procesos: 12 puntajes GWP100 == openLCA (tol 1e-9).
- Hallazgo: USLCI solo tiene 27 procesos cerrados sin biblioteca (todos hojas); las cadenas con referencia openLCA usan la biblioteca eLCI.
- Pendiente: lector de biblioteca openLCA; devtools::check() local de 0.0.0.9002.
- Datos: USLCI es MIT (uslci-admin); ver inst/extdata/README_DATA.md.
## Fase 2b (2026-10-29): v0.0.0.9003 - Monte Carlo y graficos
- lca_mc (keep_draws), lca_mc_summary (sin p-valores), lca_sens (Spearman, cribado), lca_contrib (con "Resto"),
  plot_contributions / plot_mc / plot_sens (ggplot2 en Suggests; devuelven objetos ggplot).
- Probado en WebR: 49 + 17 expectativas pasan; los graficos se construyen (ggplot_build) pero NO se inspeccionaron visualmente.
- Decision: ggplot2 (Suggests) por ser el estandar de la comunidad; confianza ~70%.
- 0.0.0.9004: etiquetas de valor en plot_contributions (valor y %) y plot_sens (rho). 9003 paso check 0/0/0 en el equipo del autor.
- 0.0.0.9005: etiquetas sin ceros de relleno (formatC por valor). 9004 revisado visualmente por el autor: se ve bien.
- 0.0.0.9005 verificado en el equipo del autor (R 4.5.3): check 0/0/0. Fase 2b cerrada (2026-10-29). Siguiente propuesto: comparacion A vs B (MC pareado).
- 0.0.0.9006 (2026-10-29): lca_compare (MC pareado, paired=FALSE para mostrar el efecto de ignorar el pareo), lca_compare_summary (prob_a_lower, razon mediana, dz; sin p-valores), plot_compare. WebR: 14 expectativas nuevas pasan. Pendiente: check local.
- 0.0.0.9006 verificado en el equipo del autor: check 0/0/0 y plot_compare revisado visualmente (steel vs coal: dif. media 2.195, IC [1.78, 2.75], P(A menor)=0). Fase 2c cerrada (2026-10-29). Siguiente: diseno de experimentos para ACV.
- 0.0.0.9007 (2026-10-29): diseno de experimentos: lca_doe_design (completo/fraccionado 2^(k-p), aliases, resolucion), lca_scale, lca_doe_run (numeros aleatorios comunes), lca_doe_effects (modo modelo sin p-valores; modo experimental con lm, IC y p-valores), plot_effects. WebR: 43 expectativas nuevas pasan. Pendiente: check local y revision visual de plot_effects.
- 0.0.0.9007 verificado en el equipo del autor: check 0/0/0; ejemplo steel x gas (n=1000): efectos 9.05 / 6.40 / interaccion 0.86, prob_positive=1 (por construccion). Pendiente: revision visual de plot_effects.
- 0.0.0.9008: plot_effects con etiquetas fuera del intervalo ('efecto [inf, sup]'); 9007 revisado visualmente (etiquetas chocaban con barras de error).
- 0.0.0.9009: etiquetas sin espacios de relleno. 9008 revisado visualmente por el autor: se ve bien. Fase 3 (MC, comparacion, DOE) completa; siguiente: datos y tutoriales ES/EN.
- 0.0.0.9010 (2026-10-29): vineta de investigacion en ingles (vignettes/lcar-uslci.Rmd): lectura USLCI y calidad de enlaces, subgrafo vs openLCA, Brightway (toy), MC y error, A vs B, DOE, limitaciones. Etiquetas (labels) en lca_contrib/plot_contributions; nombres de los 583 procesos del subgrafo. Pendiente: render local (necesita pandoc/knitr) y check.
- Correccion de texto: de las 12 comparaciones con openLCA, 9 son camiones electricos, 2 hojas (efluente, maiz) y 1 extrusion (antes se dijo 10 de 12 camiones).
- 0.0.0.9010 verificado en el equipo del autor: check 0/0/0 (incluye construccion de la vineta) y build_vignettes OK (pandoc, R 4.5). Pendiente: revision visual de la vineta por el autor; decision sobre importar incertidumbre de USLCI.
- 0.0.0.9011 (2026-10-30): importacion de incertidumbre de USLCI (opcion B: lognormal, triangular, uniforme; degenerada -> fija; invalida -> 'unsupported'), muestreador extendido (lca_mc, lca_compare, lca_doe_*; lca_scale escala min/max/moda), indicador de cobertura lca_uncertainty_coverage() y advertencia si <50 %. Hallazgo: USLCI v1.2026-03.0 tiene 5.117 de 5.129 lognormales con GSD = 1 y un triangular invalido (moda -10.9979). Fixture uslci_unc + derivacion independiente en Python. WebR: core 58, uslci 17, compare 14, doe 47, uncertainty 32, todo OK; vineta actualizada (seccion 5.1) y chunks ejecutados en WebR. Pendiente: check local. Opcion C (incertidumbre derivada de pedigri) solo en la funcion de pedigri, como escenario.
- 0.0.0.9011 verificado en el equipo del autor (2026-10-30): check 0/0/0 (4m 55s). Importacion de incertidumbre USLCI + cobertura cerrada. Siguiente: plantillas de reporte (requiere propuesta y aprobacion).
- 0.0.0.9012 (2026-10-30): lca_report() (informe html/docx via rmarkdown; plantilla inst/report/lca_report.Rmd: objetivo y alcance, inventario, resultados, contribuciones, incertidumbre con error y N necesario, cobertura, comparacion, DOE, calculado-vs-supuesto, limitaciones, reproducibilidad) y use_lca_article() (esqueleto de manuscrito en ingles, inst/templates/lca-article.Rmd). Sin PDF (requiere LaTeX). WebR: calculo del informe (.report_data) y use_lca_article probados; plantilla ejecutada con knitr::knit en WebR (todas las secciones). PENDIENTE DE VERIFICACION DEL AUTOR: renderizado real a html y docx (rmarkdown + pandoc) y apariencia de las figuras dentro del informe; el test de renderizado se omite en WebR.
- 0.0.0.9012 verificado en el equipo del autor (2026-10-30): check 0/0/0 (4m 50s), incluida la prueba de renderizado html+docx (rmarkdown+pandoc disponibles). lca_report(format='html') genero informe_prueba.html sin error. PENDIENTE: revision visual de html y docx por el autor (figuras, tablas, orden de secciones).
- 0.0.0.9012 (2026-10-30 13:02): autor abrio informe_prueba.html y .docx; ambos se generan y contienen el informe. Detalle de figuras/tablas no confirmado explicitamente. Fase de plantillas de reporte cerrada. Siguiente: pedigri + vineta en espanol (requiere propuesta y aprobacion).
- 0.0.0.9013 (2026-10-30): pedigri (aprobado): lca_pedigree (puntajes del analista por proceso o intercambio; nunca los inventa), lca_pedigree_summary (calidad: % del resultado con puntaje, puntaje medio, % con 4-5), lca_pedigree_scenario (opcion C: lognormal con GSD^2 = exp(sqrt(ln(Ub)^2 + sum ln(Ui)^2)), mediana = nominal; modelo etiquetado m$scenario, intercambios marcados pedigree_scenario; conserva distribuciones documentadas salvo overwrite=TRUE). Cobertura ahora separa share_bio_contribution (documentada) de share_bio_scenario (supuesta); lca_mc_summary agrega unc_scenario; lca_report declara el escenario. Tabla de factores verificada contra ecoinvent v3 (Table 10.5 en varianzas, cruce numerico: var = ln(U)^2/4), GHG Protocol y una publicacion reciente; DOS CASILLAS reconstruidas (geografica 4 = 1.05, tecnologica 2 = 1.05) pendientes de contraste con Weidema et al. 2013 (no se pudo abrir el original). Ub por defecto 1.05 = marcador de posicion (no es el factor por flujo de ecoinvent). Correccion: lca_report fallaba con modelos sin columnas de incertidumbre; ahora omite Monte Carlo y lo dice. Vineta en espanol vignettes/lcar-pedigri.Rmd. WebR: pedigri 51 expectativas OK; plantilla ejecutada con knitr en casos sin incertidumbre y con escenario. Pendiente: check local y revision de la vineta por el autor.
- 0.0.0.9013 verificado en el equipo del autor (2026-10-30 13:44): check 0/0/0 (6m 2s), incluye construccion de las dos vinetas. Pendiente: lectura de la vineta en espanol por el autor; contraste de 2 casillas de la tabla de pedigri con Weidema et al. 2013. Siguiente: materiales docentes (datos y tutoriales ES/EN) - requiere propuesta y aprobacion.
- 0.0.0.9014 (2026-10-30): materiales docentes (aprobados): datos inventados cup_exchanges.csv / cup_cf.csv (vaso desechable vs reutilizable, 10 procesos ficticios, columna source = 'illustrative (invented)', GWP100 con CO2/CH4/N2O; referencias independientes con numpy: 0.03129444 y 0.0307368); vineta principal en ingles lcar-tutorial.Rmd; adaptacion en espanol lcar-tutorial-es.Rmd (mismo codigo, verificado identico); guia docente bilingue lcar-teaching-guide.Rmd (objetivos, 8 ejercicios, criterios de revision; sin respuestas numericas inventadas). WebR: test-teaching 11 expectativas + todas las suites OK; codigo de las vinetas ejecutado en WebR (tutorial EN/ES y guia). Observado y documentado en el tutorial: en el DOE la columna 'relative' no informa cuando la respuesta es una diferencia con media cercana a cero, y la interaccion usos:lavado es cero numerico. Pendiente: check local y lectura de las vinetas por el autor.
- 0.0.0.9014 verificado en el equipo del autor (2026-10-30 14:56): check 0/0/0 (8m 59s), incluye construccion de las 5 vinetas. Pendiente: lectura de tutoriales y guia por el autor; decidir proximo frente (casillas pedigri, nombre del paquete, ecoinvent, lector de biblioteca openLCA, metodos de impacto).
- 0.0.0.9015 (2026-10-30): metodos de impacto (aprobado): (1) lca_model(flow_info) y read_uslci conserva el UUID y la propiedad de referencia de cada flujo elemental (m$flow_info); (2) read_olca_method(path, m, method, prefix): lee metodos openLCA JSON-LD (carpeta o zip; lcia_categories, lcia_methods, unit_groups) y vincula por UUID; informa por categoria n_factors, n_flows_matched, share_flows_matched y factores omitidos (formula, unidad desconocida, propiedad distinta); no distribuye ningun metodo; (3) lca_method_gwp(m, 'AR5'|'AR6', ch4, extra, name). Fuentes: TRACI/ReCiPe de EPA (dominio publico segun la licencia de datos de EPA; ReCiPe: terminos del RIVM no declarados, por eso no se distribuye). AR6: SOLO valores leidos en la Table 7.SM.7 oficial del IPCC (CO2, N2O 273, HFC-134a 1530, HFC-23 14600, CFC-11 6230, CFC-12 12500, CFC-113 6520, CFC-114 9430, HCFC-22 1960) y metano 29.8/27.0 (Table 7.15, confirmado solo por fuente secundaria) o 27.9 (7.SM.7). Una copia secundaria (Maine DEP) tenia CFC-11/12 distintos del oficial (5560/11200): no se usan copias secundarias. SF6, NF3, HFC-152a, CF4, CCl4, C2F6 no estan en AR6 (agregar con extra). PENDIENTE: validar read_olca_method con TRACI real contra openLCA (el autor debe descargar los archivos de EPA); verificar Table 7.15 oficial; fixture olca_method_mini con factores INVENTADOS. WebR: test-method 29, test-gwp 12 expectativas OK.
- 0.0.0.9015 verificado en el equipo del autor (2026-10-30 15:55): check 0/0/0 (8m 53s). TRACI 2.1 JSON (EPA) recibido: formato compatible con read_olca_method; 159495 factores leidos en ~6 s, 0 omitidos; 1307/2682 flujos elementales de USLCI (js2) coinciden por UUID. Validacion cruzada contra olca-core 2.6.2 (TraciCalc.java, db_traci) en curso. traci_2_2.xlsx (tabla nativa por CAS) NO es leible por read_olca_method; read_traci() (CAS+compartimento) propuesto, sin aprobacion aun.
- 0.0.0.9016 (2026-10-30, EN DESARROLLO, aprobado por el autor 17:28): read_traci() para TRACI 2.2 (Excel de la EPA, por CAS): 5 categorias de aire (acidificacion, calentamiento, material particulado, ozono, smog) + ecotoxicidad y salud humana (cancer / no cancer) por compartimento; respaldo por nombre (509 sustancias sin CAS: NOx, SOx, PM2.5...) con alias editables; mapeo de categoria de flujo a compartimento = SUPUESTO documentado y reemplazable (`compartments`); `flags` para filtrar Recommended/Interim; informe en m$traci_report. read_uslci ahora guarda el CAS en m$flow_info$cas. TRACI 2.2 NO trae eutrofizacion ni combustibles fosiles (retirados por la EPA en 2022); GWP basado en AR4; sin credito por CO2 tomado del aire.
- Verificacion 0.0.0.9016 (WebR, R 4.6.0): test-traci 53 expectativas (fixture con 12 filas reales de TRACI 2.2 y valores esperados derivados aparte en Python); suite completa sin fallos salvo el render de lca_report (se omite en WebR). Prueba a escala con los 2.682 flujos elementales de USLCI y el TRACI 2.2 real: 2.093 factores no nulos, identicos a una derivacion independiente en Python (dif. relativa maxima 2.4e-13; mismas reglas, valida el codigo, no los supuestos); 1.152 flujos por CAS, 74 por nombre, 1.456 sin coincidencia; 0.6 s. Pendiente: check local; comparar con TRACI 2.1 JSON sobre los 12 procesos.
- 0.0.0.9016 verificado en el equipo del autor (2026-10-30 18:05): test PASS 343 / FAIL 0 / WARN 0 / SKIP 0 (58.9 s, incluye el render de lca_report); check 0/0/0 (9m 42s). read_traci() cerrada. Pendiente: probar con los archivos reales de TRACI 2.2 y USLCI del autor; comparar 2.2 con 2.1; propuesta de la funcion de validacion de la tabla de entrada.
- Prueba real (2026-10-30 20:15, equipo del autor): USLCI 2026-03 + TRACI 2.2 xlsx, 4.061 flujos: 1.154 por CAS, 104 por nombre, 2.803 sin factor (recursos, radionuclidos, metales como elementos, categorias genericas). Pendientes sin aprobar: alias de metales a iones; cobertura por contribucion; funcion de validacion de la tabla de entrada.
- 0.0.0.9017 (2026-10-30 noche, EN DESARROLLO): (1) lca_check() (aprobado el orden por el autor 20:19): filas repetidas, flujos sin factor por metodo, procesos vacios y agregadores sin flujos elementales, incoherencias de incertidumbre (utype, parametros, rangos, lognormal con monto negativo: el muestreador pierde el signo), nombres iguales tras normalizar o a un caracter, flujos sin CAS; con demanda pondera la cadena. Lista lo NO comprobado (unidades, resolubilidad, correccion de los datos). Version inicial: faltan unidades y resolubilidad. (2) CORRECCION a read_traci(): en el export de USLCI 2026-03, 1.237 de 3.919 flujos elementales usados por procesos (32 %) no estan en la carpeta flows/ y no traen CAS (incluye CO2 emitido al aire); con 0.0.0.9016 el GWP de TRACI 2.2 quedo muy subestimado (mediana de TRACI/GWP100 = 0,0015 en 1.099 procesos). Ahora los flujos sin CAS se comparan por nombre exacto con todas las sustancias de TRACI (nombres ambiguos descartados); mediana de la razon = 1,001 (RIC 0,997 a 1,008). Los comentarios previos de que los valores de las tuberias de concreto eran plausibles quedan RETIRADOS. Probado en WebR: suite completa + test-check (36) + test-traci (53).

## 0.0.0.9018 (VERIFIED locally 2026-10-30: test 385/0/0/0; check 0/0/0 after document())
- lca_check(): symbols that change a name (<=, >=, <, >, =, +) are spelled out before normalising, so
  "Particulate matter, <= 10um" and "> 10um" are no longer reported as the same name (false positive found on real USLCI).
- lca_check(): repeated_rows split. Warning when at least one repeated row has no distribution (amounts are silently summed);
  note (repeated_uncertain_rows) when all rows carry a distribution (independent variables by design).
  Real USLCI: 11 -> 4 warnings + 7 notes.
- Measured on real USLCI + TRACI 2.2: outliers in TRACI/GWP100 ratio come from the built-in GWP100 crediting resource CO2 (-1)
  while TRACI 2.2 gives 0 (and AR5 vs AR4 for CH4/N2O). Not a bug.
- read_traci(): readxl "Expecting numeric ... got 'x'" warnings (TRACI's "x" cells) are muffled; other warnings kept.
- Reminder: the zip ships without man/; run devtools::document() before check().

## 0.0.0.9019 (VERIFIED locally 2026-10-30: test 410/0/0/0; check 0/0/0)
- lca_check(): solvability (self_consumption, not_productive, possibly_not_productive, singular_system, ill_conditioned).
  Condition number is estimated AFTER scaling rows/columns to max 1: raw condest on USLCI = 2e13 (artefact of units,
  entries from 2.7e-11 to 3.3e6), scaled = 4.8e7 (below the 1e8 warning, but within a factor of ~2 of it). Relative residuals of
  20 random demands: <= 1.2e-12 (backward error only; says nothing about forward error). Thresholds 1e8/1e12 are conventions.
  An unsolvable system skips lca_calc for `demand`.
- lca_check(): units, only when exchanges carry a `unit` column (lca_model keeps it): unit_mismatch (error: different
  quantity or unknown unit), unit_scale_mismatch (warning: same quantity, other scale), unit_missing (note).
  read_uslci converts everything to the reference unit of the unit group, so it has no unit column.
- First version of the raw-condition check was an error on real data (false alarm); found by running on USLCI before delivery.

## Pedigree defaults: contrast of geographical 4 and technological 2 (2026-10-30, RESOLVED)
- Primary source read (user supplied the page): Weidema et al. (2013), Table 10.5 gives VARIANCES of the underlying
  normal distributions, with NO empty cells (geographical 4 = 6e-4; technological 2 = 6e-4).
- U = exp(2*sqrt(var)) gives 1.0502 for both cells -> our 1.05 is right. All 25 cells agree with lcar's rounded factors
  within 0.6 % (max at score 3 of reliability/completeness/temporal: 1.0936 vs 1.10).
- The 2025 Environ Sci Eur table with empty cells (secondary source) does NOT match the primary table; an earlier proposal
  of mine to default those cells to NA was based on it and was withdrawn BEFORE any code change. Lesson: do not act on a
  secondary table when the primary one is within reach.
- Changes: comments/roxygen of pedigree.R now state "checked"; new test against the Table 10.5 variances.
- Open (minor): lcar uses rounded factors; the v3 report uses the variances directly (differences <= 0.6 %). Option for later.

## 0.0.0.9020 (VERIFIED locally 2026-10-30: test 412/0/0/0; check 0/0/0)
- pedigree.R: documentation says the 25 factors were checked against Table 10.5; test added. No numerical change.

## Package name (2026-10-31): DECIDED by the user: `lcar`
- Checked today: not on CRAN (crandb 404 for lcar and lcaR), not on PyPI (404). GitHub search blocked from this environment: NOT checked,
  nor Bioconductor, R-universe or trademarks. Do these before submission. Title already says "Life Cycle Assessment" (LCA also
  means latent class analysis in R). DESCRIPTION no longer says the name is provisional (next zip).

## 0.0.0.9021 (2026-10-31)
- DESCRIPTION: version bump; package name no longer described as provisional. No code change.

## 0.0.0.9022 (2026-11-01)
- lca_check(): the `empty_process` message now says, for models carrying `info` (read_uslci), how many empty processes do have inputs in the
  source but none linked (cut-off) and how many have no input at all. Finding name and severity unchanged. Test added.
- Context: of the 88 "empty" USLCI processes, the 22 non-bridge ones were checked by hand in the JSON: all have product inputs (1-4); the
  "emptiness" came from the cut-off rule. The 66 "Bridge" processes were not checked one by one.
- Name check 2026-11-01: lcar absent from CRAN (+archive), Bioconductor and R-universe (author's screenshot). GitHub and trademarks NOT checked.

## 0.0.0.9023 (2026-11-01, approved simple version)
- read_traci(): metals named as the element (e.g. "Zinc") get NO factor (TRACI lists ions with an oxidation state; choosing one is an
  assumption). They are counted in `traci_report$metals_without_factor` (+ `_names`), a message is printed, lca_check() adds the note
  `metals_without_factor`, and ?read_traci explains how to accept an ion with `aliases`. No new argument, no automatic alias.
- Only emission flows with a compartment are counted (resources are not); only metals for which TRACI lists an ion row.

## 0.0.0.9024 (2026-11-01): documentation only on top of 9023
- ?read_traci: measured size of the metal gap (USLCI 2026-03, 733 complete-chain processes, TRACI 2.1 JSON-LD vs 2.2 xlsx) and the pointer to
  read_olca_method() with TRACI 2.1. Data: tox_21_vs_22.csv in the Project. TRACI 2.1 gives a factor to 226 of 262 metal emission flows.

## 0.0.0.9025 (2026-11-01)
- Name decision kept: `lcar`. GitHub has an old, unmaintained repo `jmarshallnz/lcar` (latent class analysis, 2018); not on CRAN/R-universe/Bioconductor.
  Clarification added to DESCRIPTION ("LCA here means life cycle assessment, not latent class analysis") and a README.md (excluded from the build
  with .Rbuildignore). Trademarks NOT checked.

## 0.0.0.9026 (2026-11-01): documentation only
- ?read_traci corrected: the TRACI 2.1 vs 2.2 difference is NOT a change in factor values (identical where the compartment is explicit); it comes from the
  flows each route reaches and from unspecified compartments (2.1 generic air = mean of urban and rural for ~2/3 of substances; lcar uses rural).

## 0.0.0.9027 (2026-11-01): unspecified air = mean of urban and rural (approved by the author)
- read_traci(): `emission/air` (no compartment) -> new internal code `airM` = (airU + airC)/2 for ecotoxicity and human health; only when both exist.
  Other non-urban air paths (stratosphere, high...) stay rural. `traci_report$air_unspecified_mean`. NEWS.md added (results change).
- Basis: TRACI 2.1 JSON-LD (generic air = mean in ALL flows compared after the change: 249/137/78 flows, equal < 1e-6; before it was 0; the earlier '~2/3' came from a coarser by-name comparison; observed pattern, not a documented rule); USEtox manual section 8
  (50/50 split of undifferentiated air as an example). The previous "rural" had no source. Unspecified soil NOT changed (comparison inconclusive).
- Real-data check (USLCI 2026-03): 693 flows classified as unspecified air; only those flows changed in the three toxicity categories; no flow lost a factor; GWP/acidification/smog/ozone/PM unchanged.

## 0.0.0.9028 (2026-11-01): package renamed `lcar` -> `lcamate` (decision of the author)
- Why: `jmarshallnz/lcar` exists on GitHub (latent class analysis, 2018) and `lcars` is one letter away on CRAN; the author asked for a rarer name whose
  first three letters are LCA and whose last four hint at "accompaniment": lcamate (LCA + mate = companion).
- Checked 2026-11-01 (free): CRAN (full list), R-universe, PyPI, Bioconductor. NOT checked: GitHub, trademarks (author).
- Mechanical rename in 27 files (library(), system.file(package=), vignette files, report data `versions$lcamate`); inst/extdata untouched (chemical names
  contain the string "lcar" inside e.g. "phenylcarbamoyl"). Older entries of these notes keep the old name on purpose.

## 0.0.0.9029 (2026-11-01): unspecified soil = mean(nat, agr) (approved by the author)
- read_traci(): `emission/ground` and `emission/non-FEDEFL/ground` (no sub-compartment) -> new internal code `soilM` = (nat + agr)/2 for ecotoxicity and human health; only when both exist. Rule placed before the agricultural/ground rules. `traci_report$soil_unspecified_mean`, extra note.
- Basis: TRACI 2.1 JSON-LD: unspecified soil = mean(nat, agr) in 100 % of flows compared (142/80/37, relative difference < 1e-6; in ecotoxicity nat = agr). Observed pattern, not documented. Earlier comparison was inconclusive because it used agr/industrial only.
- Weight (USLCI 2026-03, 716 complete-chain processes): unspecified ground flows (276) dominate the score (>50 %) in 9 processes; median share <= 0.3 %. Industrial ground (69 flows, max 0.2 %) not changed.
- Test added (test-traci.R, +6 expectations). Document in claude/lca-r-prototipo/MEDICION_SUELO_SIN_ESPECIFICAR_2026-11-01.md.

## 0.0.0.9030 (2026-11-01): documentation only (wording of "complete chain")
- Found while examining the 66 Bridge processes (58 to USEEIO, 8 to USFS; one input in USD or similar that has no provider; completeness 0; 351 technosphere exchanges and 146 consumers use them).
- `completeness` is per process (own inputs linked). Measured: 733 processes with completeness 1; only 27 have no incomplete process anywhere upstream (94 if Bridge processes are not counted as incomplete); 706 of the 733 have an incomplete process upstream, 698 of them a Bridge.
- Earlier text said "complete chain" for the 733 (and 716 with non-zero scores). Corrected in ?read_traci and NEWS.md; older Project documents keep the old wording on purpose (see MEDICION_BRIDGE_2026-11-01.md). The vignette lcamate-uslci already said 27 of 1,341.

## 0.0.0.9031 (2026-11-01): faster vignettes (approved by the author, proposal A)
- Measured on the author's machine (R 4.5): `check()` 11 min; rebuilding vignettes 311 s (uslci 107, tutorial en/es 63 each, pedigri 50, teaching 1); tests 61 s; examples 14 s; install 19 s.
- Slow chunks were Monte Carlo (about 7 ms per draw, even in tiny models: sparse matrices rebuilt and solved at every draw): uslci compare 42 s, doe 28 s, unc-mc 14, mc 14; tutorial doe 29, compare 15, mc 14; pedigri uslci-mc 28, mc-escenario 15.
- Change: n 2000 -> 500 (mc, compare, unc-mc, pedigri), doe 500 -> 200 (tutorials), doe 1000 -> 300 (uslci). Prose checked against the new output in WebR: paired vs unpaired still differ as described (reworded "the mean difference is the same" -> "estimate the same quantity"); interaction uses:wash is 7e-20 now (text: of the order of 1e-19 or smaller).
- Option B (not done): speed up lca_mc by reusing the matrix structure; needs a benchmark and identical results with the same seed.

