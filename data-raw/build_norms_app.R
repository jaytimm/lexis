load("data/lexis_wide.rda")
load("data/lexis_meta.rda")

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required to build the standalone norms app.", call. = FALSE)
}

out_file <- "apps/lexis-norms.html"

norm_dims <- c(
  "aoa", "boi", "concreteness", "gender_femininity", "humor", "iconicity",
  "socialness", "valence", "arousal", "dominance",
  "lancaster_auditory", "lancaster_gustatory", "lancaster_haptic",
  "lancaster_interoceptive", "lancaster_olfactory", "lancaster_visual",
  "lancaster_foot_leg", "lancaster_hand_arm", "lancaster_head",
  "lancaster_mouth", "lancaster_torso",
  "glasgow_valence", "glasgow_arousal", "glasgow_dominance",
  "glasgow_concreteness", "glasgow_imageability", "glasgow_familiarity",
  "glasgow_aoa", "glasgow_size", "glasgow_gender"
)

covariates <- c(
  "lexdec_rt", "lexdec_naming_rt", "freq_zipf_us", "wf_zipf",
  "wn_n_synsets", "wn_n_noun", "wn_n_verb", "wn_n_adj", "wn_n_adv"
)

cols <- c("word", "lemma", intersect(norm_dims, names(lexis_wide)), intersect(covariates, names(lexis_wide)))
app_data <- lexis_wide[, cols]
app_data <- app_data[order(app_data$word), ]

round_cols <- setdiff(names(app_data), c("word", "lemma", "wn_n_synsets", "wn_n_noun", "wn_n_verb", "wn_n_adj", "wn_n_adv"))
for (col in round_cols) {
  app_data[[col]] <- round(app_data[[col]], 3)
}

meta <- lexis_meta[, c("dimension", "dataset", "scale", "construct", "citation")]
meta <- meta[meta$dimension %in% norm_dims, ]
meta <- meta[order(match(meta$dimension, norm_dims)), ]

coverage <- data.frame(
  dimension = setdiff(cols, c("word", "lemma")),
  n = colSums(!is.na(app_data[setdiff(cols, c("word", "lemma"))])),
  stringsAsFactors = FALSE
)

json_data <- jsonlite::toJSON(
  unname(app_data),
  dataframe = "rows",
  na = "null",
  auto_unbox = TRUE,
  digits = 4
)

json_cols <- jsonlite::toJSON(names(app_data), auto_unbox = TRUE)
json_meta <- jsonlite::toJSON(meta, dataframe = "rows", na = "null", auto_unbox = TRUE)
json_coverage <- jsonlite::toJSON(coverage, dataframe = "rows", na = "null", auto_unbox = TRUE)

html <- '<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>lexis norms</title>
<style>
:root {
  --ink: #171513;
  --paper: #f8f4ea;
  --wash: #efe5d1;
  --line: #28231d;
  --muted: #716a61;
  --red: #e85d4f;
  --blue: #2f80a8;
  --green: #4d8b62;
  --gold: #d49a2a;
  --violet: #7462a9;
  --shadow: 0 18px 50px rgba(23, 21, 19, .14);
}
* { box-sizing: border-box; }
body {
  margin: 0;
  min-height: 100vh;
  color: var(--ink);
  font-family: ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  background:
    linear-gradient(90deg, rgba(23,21,19,.035) 1px, transparent 1px) 0 0 / 32px 32px,
    linear-gradient(rgba(23,21,19,.03) 1px, transparent 1px) 0 0 / 32px 32px,
    var(--paper);
}
button, input, select { font: inherit; color: inherit; }
button {
  border: 2px solid var(--line);
  background: var(--ink);
  color: var(--paper);
  padding: .72rem .95rem;
  cursor: pointer;
  box-shadow: 4px 4px 0 var(--line);
}
button.secondary { background: var(--paper); color: var(--ink); }
button.icon { width: 2.8rem; height: 2.8rem; padding: 0; display: grid; place-items: center; }
button:active { transform: translate(2px, 2px); box-shadow: 2px 2px 0 var(--line); }
input, select {
  width: 100%%;
  border: 2px solid var(--line);
  background: #fffaf0;
  padding: .74rem .82rem;
  outline: none;
}
.page { max-width: 1220px; margin: 0 auto; padding: 28px 18px 56px; }
.top {
  display: grid;
  grid-template-columns: minmax(0, 1.1fr) minmax(320px, .9fr);
  gap: 22px;
  align-items: stretch;
}
.mast {
  min-height: 390px;
  border: 2px solid var(--line);
  background:
    radial-gradient(circle at 78%% 24%%, rgba(232, 93, 79, .22), transparent 28%%),
    radial-gradient(circle at 22%% 76%%, rgba(47, 128, 168, .2), transparent 24%%),
    linear-gradient(135deg, #fffaf0 0%%, #f5dec2 58%%, #d6e7d6 100%%);
  box-shadow: var(--shadow);
  padding: clamp(22px, 5vw, 52px);
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  position: relative;
  overflow: hidden;
}
.mast:after {
  content: "";
  position: absolute;
  inset: auto -30px -56px auto;
  width: 320px;
  height: 220px;
  border: 2px solid var(--line);
  background:
    linear-gradient(135deg, transparent 46%%, rgba(23,21,19,.9) 47%% 53%%, transparent 54%%),
    repeating-linear-gradient(90deg, rgba(23,21,19,.11) 0 1px, transparent 1px 18px),
    #f3c65f;
  transform: rotate(-10deg);
}
.eyebrow { text-transform: uppercase; letter-spacing: .12em; font-size: .76rem; font-weight: 800; color: var(--blue); }
h1 { margin: .15rem 0 1rem; font-size: clamp(3rem, 9vw, 7.5rem); line-height: .88; letter-spacing: 0; max-width: 780px; }
.lede { max-width: 640px; font-size: clamp(1rem, 1.5vw, 1.25rem); line-height: 1.45; color: #332d27; }
.searchbar { display: grid; grid-template-columns: 1fr auto auto; gap: 10px; margin-top: 26px; position: relative; z-index: 1; }
.stats { display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; margin-top: 20px; position: relative; z-index: 1; }
.stat { border: 2px solid var(--line); background: rgba(255,250,240,.78); padding: 12px; }
.stat strong { display: block; font-size: 1.5rem; line-height: 1; }
.panel {
  border: 2px solid var(--line);
  background: #fffaf0;
  box-shadow: var(--shadow);
  padding: 18px;
}
.panel h2 { margin: 0 0 14px; font-size: 1.05rem; text-transform: uppercase; letter-spacing: .1em; }
.wordmark {
  min-height: 116px;
  border: 2px solid var(--line);
  display: grid;
  place-items: center;
  background: var(--wash);
  margin-bottom: 14px;
  overflow: hidden;
}
.wordmark span { font-size: clamp(2.4rem, 8vw, 5.5rem); font-weight: 900; line-height: 1; max-width: 100%%; overflow-wrap: anywhere; }
.chips { display: flex; flex-wrap: wrap; gap: 8px; }
.chip { border: 2px solid var(--line); padding: .38rem .55rem; background: #fff; font-size: .86rem; }
.chip.red { background: #ffd8d2; }
.chip.blue { background: #d9edf5; }
.chip.green { background: #dcefdc; }
.chip.gold { background: #ffe5a5; }
.grid { display: grid; grid-template-columns: 360px minmax(0, 1fr); gap: 20px; margin-top: 22px; align-items: start; }
.controls { display: grid; gap: 12px; }
.control-row { display: grid; grid-template-columns: 1fr 82px; gap: 8px; align-items: end; }
.axis-row { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; }
label { display: grid; gap: 5px; font-size: .78rem; text-transform: uppercase; letter-spacing: .08em; font-weight: 800; color: var(--muted); }
input[type="range"] { padding: 0; accent-color: var(--red); border: 0; background: transparent; }
.results { display: grid; gap: 14px; }
.canvas-wrap { border: 2px solid var(--line); background: #fffaf0; padding: 10px; min-height: 470px; }
canvas { width: 100%%; height: 430px; display: block; background: linear-gradient(#fffaf0, #f7ecd8); border: 1px solid rgba(23,21,19,.25); }
.cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(185px, 1fr)); gap: 10px; }
.card {
  border: 2px solid var(--line);
  background: #fffaf0;
  padding: 12px;
  min-height: 132px;
  cursor: pointer;
  transition: transform .12s ease, box-shadow .12s ease;
}
.card:hover { transform: translate(-2px, -2px); box-shadow: 4px 4px 0 var(--line); }
.card strong { display: block; font-size: 1.35rem; overflow-wrap: anywhere; }
.mini { color: var(--muted); font-size: .86rem; line-height: 1.35; }
.bars { display: grid; gap: 7px; margin-top: 12px; }
.bar { display: grid; grid-template-columns: 116px 1fr 44px; gap: 8px; align-items: center; font-size: .82rem; }
.comparebar { grid-template-columns: 116px 1fr 44px 1fr 44px; }
.track { height: 12px; border: 1px solid var(--line); background: #f1e4ce; position: relative; }
.fill { display: block; height: 100%%; background: var(--blue); }
.fill.red { background: var(--red); }
.fill.green { background: var(--green); }
.fill.gold { background: var(--gold); }
.compare { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin-top: 12px; }
.footer { margin-top: 28px; color: var(--muted); font-size: .85rem; line-height: 1.45; max-width: 820px; }
@media (max-width: 860px) {
  .top, .grid, .compare { grid-template-columns: 1fr; }
  .searchbar { grid-template-columns: 1fr; }
  .stats { grid-template-columns: 1fr; }
  .page { padding: 14px 12px 38px; }
  .mast { min-height: 520px; }
  .axis-row, .control-row { grid-template-columns: 1fr; }
}
</style>
</head>
<body>
<main class="page">
  <section class="top">
    <div class="mast">
      <div>
        <div class="eyebrow">lexis package data</div>
        <h1>word norms, but playable</h1>
        <p class="lede">Look up a word, mix psychological dimensions, and pull strange little sets from English rating norms: concrete, funny, social, embodied, emotional, early-learned, frequent, and more.</p>
        <div class="searchbar">
          <input id="search" type="search" autocomplete="off" placeholder="try glitter, soup, justice, whisper">
          <button id="lookup">Look up</button>
          <button id="random" class="secondary icon" title="Random word">?</button>
        </div>
      </div>
      <div class="stats">
        <div class="stat"><strong id="nWords">0</strong><span>words</span></div>
        <div class="stat"><strong id="nDims">0</strong><span>dimensions</span></div>
        <div class="stat"><strong id="nVisible">0</strong><span>in play</span></div>
      </div>
    </div>
    <aside class="panel">
      <h2>Current Word</h2>
      <div class="wordmark"><span id="currentWord">lexis</span></div>
      <div id="currentChips" class="chips"></div>
      <div id="currentBars" class="bars"></div>
    </aside>
  </section>

  <section class="grid">
    <aside class="panel controls">
      <h2>Make A Set</h2>
      <label>Preset
        <select id="preset">
          <option value="custom">Custom</option>
          <option value="bright">pleasant + concrete</option>
          <option value="weird">funny + iconic</option>
          <option value="body">body words</option>
          <option value="soft">calm + familiar</option>
          <option value="abstract_social">abstract + social</option>
          <option value="early">early learned</option>
        </select>
      </label>
      <div class="control-row">
        <label>Dimension
          <select id="filterDim"></select>
        </label>
        <label>Minimum
          <input id="filterMin" type="number" step="0.1">
        </label>
      </div>
      <label>Threshold
        <input id="filterSlider" type="range" min="0" max="1" step="0.01">
      </label>
      <div class="axis-row">
        <label>X axis
          <select id="xAxis"></select>
        </label>
        <label>Y axis
          <select id="yAxis"></select>
        </label>
      </div>
      <label>Word pattern
        <input id="pattern" placeholder="regex: ^un, ness$, oo">
      </label>
      <button id="apply">Draw words</button>
      <button id="shuffle" class="secondary">Shuffle results</button>
      <div class="mini" id="metaText"></div>
    </aside>

    <section class="results">
      <div class="canvas-wrap">
        <canvas id="plot" width="900" height="520"></canvas>
      </div>
      <div id="cards" class="cards"></div>
    </section>
  </section>

  <section class="panel" style="margin-top:22px">
    <h2>Compare Two Words</h2>
    <div class="compare">
      <input id="compareA" placeholder="word A">
      <input id="compareB" placeholder="word B">
    </div>
    <div id="compareOut" class="bars"></div>
  </section>

  <p class="footer">Standalone export generated from <code>data/lexis_wide.rda</code> and <code>data/lexis_meta.rda</code>. Missing ratings are left out of scores and plots; covariates are included for filtering and display.</p>
</main>

<script>
const DATA_COLUMNS = __DATA_COLUMNS__;
const DATA_ROWS = __DATA_ROWS__;
const META = __META__;
const COVERAGE = __COVERAGE__;

const LABELS = {
  aoa: "age learned",
  boi: "body-object",
  concreteness: "concrete",
  gender_femininity: "femininity",
  humor: "humor",
  iconicity: "iconicity",
  socialness: "social",
  valence: "pleasant",
  arousal: "arousal",
  dominance: "dominance",
  lancaster_auditory: "hearing",
  lancaster_gustatory: "taste",
  lancaster_haptic: "touch",
  lancaster_interoceptive: "inside body",
  lancaster_olfactory: "smell",
  lancaster_visual: "seeing",
  lancaster_foot_leg: "foot/leg",
  lancaster_hand_arm: "hand/arm",
  lancaster_head: "head",
  lancaster_mouth: "mouth",
  lancaster_torso: "torso",
  glasgow_valence: "G valence",
  glasgow_arousal: "G arousal",
  glasgow_dominance: "G dominance",
  glasgow_concreteness: "G concrete",
  glasgow_imageability: "G image",
  glasgow_familiarity: "G familiar",
  glasgow_aoa: "G AoA",
  glasgow_size: "G size",
  glasgow_gender: "G gender",
  lexdec_rt: "lexdec ms",
  lexdec_naming_rt: "naming ms",
  freq_zipf_us: "US Zipf",
  wf_zipf: "Zipf",
  wn_n_synsets: "senses",
  wn_n_noun: "noun senses",
  wn_n_verb: "verb senses",
  wn_n_adj: "adj senses",
  wn_n_adv: "adv senses"
};

const SCALE = {
  aoa: [1,25], boi: [1,7], concreteness: [1,5], gender_femininity: [1,7],
  humor: [1,5], iconicity: [1,7], socialness: [1,7],
  valence: [1,9], arousal: [1,9], dominance: [1,9],
  lancaster_auditory: [0,5], lancaster_gustatory: [0,5], lancaster_haptic: [0,5],
  lancaster_interoceptive: [0,5], lancaster_olfactory: [0,5], lancaster_visual: [0,5],
  lancaster_foot_leg: [0,5], lancaster_hand_arm: [0,5], lancaster_head: [0,5],
  lancaster_mouth: [0,5], lancaster_torso: [0,5],
  glasgow_valence: [1,9], glasgow_arousal: [1,9], glasgow_dominance: [1,9],
  glasgow_concreteness: [1,7], glasgow_imageability: [1,7], glasgow_familiarity: [1,7],
  glasgow_aoa: [1,7], glasgow_size: [1,7], glasgow_gender: [1,7],
  freq_zipf_us: [1,7], wf_zipf: [1,7], wn_n_synsets: [0,20]
};

const DISPLAY = ["concreteness", "valence", "arousal", "aoa", "humor", "socialness", "boi", "iconicity", "lancaster_visual", "lancaster_hand_arm", "freq_zipf_us", "wn_n_synsets"];
const FILTERS = ["concreteness", "valence", "arousal", "aoa", "humor", "socialness", "boi", "iconicity", "lancaster_visual", "lancaster_auditory", "lancaster_haptic", "lancaster_hand_arm", "lancaster_mouth", "freq_zipf_us", "wf_zipf", "wn_n_synsets"];
const AXES = ["concreteness", "valence", "arousal", "aoa", "humor", "socialness", "boi", "iconicity", "lancaster_visual", "lancaster_auditory", "lancaster_haptic", "lancaster_hand_arm", "freq_zipf_us", "wn_n_synsets"];
const idx = Object.fromEntries(DATA_COLUMNS.map((d, i) => [d, i]));
const words = DATA_ROWS.map(row => {
  const obj = {};
  DATA_COLUMNS.forEach((col, i) => obj[col] = row[i]);
  return obj;
});
const byWord = new Map(words.map(d => [d.word, d]));
const metaByDim = new Map(META.map(d => [d.dimension, d]));
let visible = [];
let activeWord = byWord.get("sparkle") || words[0];

const el = id => document.getElementById(id);
const fmt = x => x == null || Number.isNaN(x) ? "NA" : Number(x).toFixed(Number(x) %% 1 ? 2 : 0);
const norm = (dim, value) => {
  if (value == null || !SCALE[dim]) return null;
  const [lo, hi] = SCALE[dim];
  return Math.max(0, Math.min(1, (value - lo) / (hi - lo)));
};

function optionize(select, dims) {
  select.innerHTML = dims.map(d => `<option value="${d}">${LABELS[d] || d}</option>`).join("");
}

function setFilterBounds(dim, value) {
  const [lo, hi] = SCALE[dim] || [0, 10];
  const slider = el("filterSlider");
  slider.min = lo;
  slider.max = hi;
  slider.step = hi - lo > 10 ? 1 : 0.1;
  slider.value = value ?? lo;
  el("filterMin").value = slider.value;
  const meta = metaByDim.get(dim);
  const cov = COVERAGE.find(d => d.dimension === dim);
  el("metaText").textContent = meta ? `${meta.scale}; ${cov ? cov.n.toLocaleString() : ""} words. ${meta.construct}` : `${cov ? cov.n.toLocaleString() : ""} words with ${LABELS[dim] || dim}.`;
}

function tagsFor(d) {
  const tags = [];
  if (d.valence != null && d.valence >= 7) tags.push(["pleasant", "green"]);
  if (d.humor != null && d.humor >= 3) tags.push(["funny", "gold"]);
  if (d.concreteness != null && d.concreteness >= 4.4) tags.push(["touchable", "blue"]);
  if (d.socialness != null && d.socialness >= 5) tags.push(["social", "red"]);
  if (d.aoa != null && d.aoa <= 6) tags.push(["early", "green"]);
  if (d.iconicity != null && d.iconicity >= 5) tags.push(["sound-shaped", "gold"]);
  if (!tags.length) tags.push(["quiet word", ""]);
  return tags;
}

function renderWord(d) {
  if (!d) return;
  activeWord = d;
  el("currentWord").textContent = d.word;
  el("currentChips").innerHTML = tagsFor(d).map(([t, cls]) => `<span class="chip ${cls}">${t}</span>`).join("");
  el("currentBars").innerHTML = DISPLAY
    .filter(dim => d[dim] != null)
    .slice(0, 10)
    .map((dim, i) => {
      const n = norm(dim, d[dim]);
      const cls = i %% 4 === 0 ? "red" : i %% 4 === 1 ? "blue" : i %% 4 === 2 ? "green" : "gold";
      return `<div class="bar"><span>${LABELS[dim]}</span><span class="track"><span class="fill ${cls}" style="width:${Math.round((n ?? 0) * 100)}%%"></span></span><span>${fmt(d[dim])}</span></div>`;
    }).join("");
  el("search").value = d.word;
}

function scorePreset(d, preset) {
  const n = dim => norm(dim, d[dim]);
  const mean = vals => vals.filter(v => v != null).reduce((a, b) => a + b, 0) / Math.max(1, vals.filter(v => v != null).length);
  if (preset === "bright") return mean([n("valence"), n("concreteness"), n("freq_zipf_us")]);
  if (preset === "weird") return mean([n("humor"), n("iconicity"), 1 - (n("freq_zipf_us") ?? .5)]);
  if (preset === "body") return mean([n("boi"), n("lancaster_hand_arm"), n("lancaster_haptic"), n("lancaster_mouth")]);
  if (preset === "soft") return mean([1 - (n("arousal") ?? .5), n("valence"), n("glasgow_familiarity")]);
  if (preset === "abstract_social") return mean([1 - (n("concreteness") ?? .5), n("socialness"), n("wn_n_synsets")]);
  if (preset === "early") return mean([1 - (n("aoa") ?? .5), n("freq_zipf_us"), n("concreteness")]);
  return 0;
}

function applyPreset(name) {
  const settings = {
    bright: ["valence", 6.8, "concreteness", "valence"],
    weird: ["humor", 2.7, "iconicity", "humor"],
    body: ["boi", 4.4, "lancaster_hand_arm", "boi"],
    soft: ["arousal", 1.0, "valence", "arousal"],
    abstract_social: ["socialness", 4.5, "concreteness", "socialness"],
    early: ["aoa", 1.0, "aoa", "freq_zipf_us"]
  }[name];
  if (!settings) return;
  el("filterDim").value = settings[0];
  setFilterBounds(settings[0], settings[1]);
  el("xAxis").value = settings[2];
  el("yAxis").value = settings[3];
}

function filteredWords() {
  const dim = el("filterDim").value;
  const min = Number(el("filterMin").value);
  const pat = el("pattern").value.trim();
  let rx = null;
  if (pat) {
    try { rx = new RegExp(pat, "i"); } catch(e) { rx = null; }
  }
  let out = words.filter(d => d[dim] != null && d[dim] >= min);
  if (rx) out = out.filter(d => rx.test(d.word));
  const preset = el("preset").value;
  if (preset !== "custom") {
    out = out
      .map(d => [d, scorePreset(d, preset)])
      .filter(x => Number.isFinite(x[1]))
      .sort((a, b) => b[1] - a[1])
      .map(x => x[0]);
  } else {
    out = out.sort((a, b) => (b[dim] ?? -Infinity) - (a[dim] ?? -Infinity));
  }
  return out.slice(0, 420);
}

function drawPlot(data) {
  const canvas = el("plot");
  const ctx = canvas.getContext("2d");
  const w = canvas.width, h = canvas.height;
  const xDim = el("xAxis").value, yDim = el("yAxis").value;
  ctx.clearRect(0, 0, w, h);
  ctx.fillStyle = "#fffaf0";
  ctx.fillRect(0, 0, w, h);
  ctx.strokeStyle = "rgba(23,21,19,.22)";
  ctx.lineWidth = 1;
  for (let i = 1; i < 5; i++) {
    ctx.beginPath(); ctx.moveTo(70, 40 + i * 82); ctx.lineTo(w - 30, 40 + i * 82); ctx.stroke();
    ctx.beginPath(); ctx.moveTo(70 + i * 158, 40); ctx.lineTo(70 + i * 158, h - 70); ctx.stroke();
  }
  ctx.strokeStyle = "#171513";
  ctx.lineWidth = 2;
  ctx.strokeRect(70, 40, w - 100, h - 110);
  ctx.fillStyle = "#171513";
  ctx.font = "700 18px system-ui";
  ctx.fillText(LABELS[yDim] || yDim, 16, 32);
  ctx.fillText(LABELS[xDim] || xDim, w - 180, h - 22);

  const plotted = data.filter(d => d[xDim] != null && d[yDim] != null);
  plotted.slice(0, 260).forEach((d, i) => {
    const x = 70 + norm(xDim, d[xDim]) * (w - 100);
    const y = h - 70 - norm(yDim, d[yDim]) * (h - 110);
    const r = d.word === activeWord.word ? 8 : 4 + Math.min(5, (norm("freq_zipf_us", d.freq_zipf_us) ?? .2) * 4);
    ctx.beginPath();
    ctx.fillStyle = i %% 5 === 0 ? "#e85d4f" : i %% 5 === 1 ? "#2f80a8" : i %% 5 === 2 ? "#4d8b62" : i %% 5 === 3 ? "#d49a2a" : "#7462a9";
    ctx.globalAlpha = d.word === activeWord.word ? 1 : .74;
    ctx.arc(x, y, r, 0, Math.PI * 2);
    ctx.fill();
    if (i < 28 || d.word === activeWord.word) {
      ctx.globalAlpha = 1;
      ctx.fillStyle = "#171513";
      ctx.font = "700 13px system-ui";
      ctx.fillText(d.word, x + 8, y - 7);
    }
  });
  ctx.globalAlpha = 1;
}

function renderCards(data) {
  visible = data;
  el("nVisible").textContent = data.length.toLocaleString();
  el("cards").innerHTML = data.slice(0, 18).map(d => `
    <article class="card" data-word="${d.word}">
      <strong>${d.word}</strong>
      <div class="mini">${tagsFor(d).map(t => t[0]).join(" / ")}</div>
      <div class="bars">
        ${[el("xAxis").value, el("yAxis").value].map(dim => d[dim] == null ? "" : `<div class="bar"><span>${LABELS[dim]}</span><span class="track"><span class="fill" style="width:${Math.round((norm(dim, d[dim]) ?? 0) * 100)}%%"></span></span><span>${fmt(d[dim])}</span></div>`).join("")}
      </div>
    </article>
  `).join("");
  [...document.querySelectorAll(".card")].forEach(card => {
    card.addEventListener("click", () => {
      renderWord(byWord.get(card.dataset.word));
      drawPlot(visible);
    });
  });
}

function run() {
  const data = filteredWords();
  renderCards(data);
  drawPlot(data);
  if (data[0]) renderWord(data[0]);
}

function lookup() {
  const q = el("search").value.trim().toLowerCase();
  if (!q) return;
  let hit = byWord.get(q);
  if (!hit) hit = words.find(d => d.word.startsWith(q));
  if (!hit) hit = words.find(d => d.word.includes(q));
  if (hit) {
    renderWord(hit);
    const around = words.filter(d => d.word.includes(q)).slice(0, 60);
    renderCards(around.length ? around : [hit]);
    drawPlot(around.length ? around : [hit]);
  }
}

function shuffleResults() {
  const shuffled = [...visible].sort(() => Math.random() - .5);
  renderCards(shuffled);
  drawPlot(shuffled);
}

function compare() {
  const a = byWord.get(el("compareA").value.trim().toLowerCase());
  const b = byWord.get(el("compareB").value.trim().toLowerCase());
  if (!a || !b) {
    el("compareOut").innerHTML = "<div class=\\"mini\\">Enter two words found in the package data.</div>";
    return;
  }
  el("compareOut").innerHTML = DISPLAY.filter(dim => a[dim] != null || b[dim] != null).slice(0, 12).map(dim => {
    const av = norm(dim, a[dim]) ?? 0;
    const bv = norm(dim, b[dim]) ?? 0;
    return `<div class="bar comparebar"><span>${LABELS[dim]}</span><span class="track"><span class="fill red" style="width:${Math.round(av * 100)}%%"></span></span><span>${fmt(a[dim])}</span><span class="track"><span class="fill blue" style="width:${Math.round(bv * 100)}%%"></span></span><span>${fmt(b[dim])}</span></div>`;
  }).join("");
}

function init() {
  optionize(el("filterDim"), FILTERS);
  optionize(el("xAxis"), AXES);
  optionize(el("yAxis"), AXES);
  el("filterDim").value = "valence";
  el("xAxis").value = "concreteness";
  el("yAxis").value = "valence";
  setFilterBounds("valence", 6.5);
  el("nWords").textContent = words.length.toLocaleString();
  el("nDims").textContent = META.length.toLocaleString();
  el("lookup").addEventListener("click", lookup);
  el("search").addEventListener("keydown", e => { if (e.key === "Enter") lookup(); });
  el("random").addEventListener("click", () => renderWord(words[Math.floor(Math.random() * words.length)]));
  el("filterDim").addEventListener("change", e => setFilterBounds(e.target.value));
  el("filterSlider").addEventListener("input", e => el("filterMin").value = e.target.value);
  el("filterMin").addEventListener("input", e => el("filterSlider").value = e.target.value);
  el("preset").addEventListener("change", e => { applyPreset(e.target.value); run(); });
  el("apply").addEventListener("click", run);
  el("shuffle").addEventListener("click", shuffleResults);
  el("compareA").addEventListener("input", compare);
  el("compareB").addEventListener("input", compare);
  el("compareA").value = "soup";
  el("compareB").value = "justice";
  renderWord(activeWord);
  compare();
  run();
}
init();
</script>
</body>
</html>
'

html <- gsub("%%", "%", html, fixed = TRUE)
html <- sub("__DATA_COLUMNS__", json_cols, html, fixed = TRUE)
html <- sub("__DATA_ROWS__", json_data, html, fixed = TRUE)
html <- sub("__META__", json_meta, html, fixed = TRUE)
html <- sub("__COVERAGE__", json_coverage, html, fixed = TRUE)

writeLines(html, out_file, useBytes = TRUE)
message("Wrote ", out_file, " with ", nrow(app_data), " words and ", ncol(app_data) - 2, " data columns.")
