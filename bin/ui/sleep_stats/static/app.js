// Sleep stats page: draws /api/data (see server.py) as SVG charts, no libraries, so it works
// offline. Durations are in hours throughout, energies in Wh, so fit slopes are in W.
'use strict';

const NS = 'http://www.w3.org/2000/svg';
const COLOR = { suspend: 'var(--suspend)', hibernate: 'var(--hibernate)' };
const NAME = { suspend: 'Suspend', hibernate: 'Hibernation' };

let D = null; // The data from the server
const state = { horizon: 24, delay: 1 }; // Hours

const $ = (sel, root = document) => root.querySelector(sel);
const esc = s => String(s ?? '').replace(/[&<>"]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));

function svg(tag, attrs, parent, text) {
  const e = document.createElementNS(NS, tag);
  for (const [k, v] of Object.entries(attrs || {})) e.setAttribute(k, v);
  if (text != null) e.textContent = text;
  if (parent) parent.appendChild(e);
  return e;
}

// ---- Number and duration formatting ----

const wh = v => v.toFixed(Math.abs(v) < 1 ? 3 : Math.abs(v) < 10 ? 2 : 1) + ' Wh';
const watts = v => (Math.abs(v) < 0.1 ? (v * 1000).toFixed(1) + ' mW' : v.toFixed(3) + ' W');
const pctNum = v => v.toFixed(Math.abs(v) < 1 ? 2 : Math.abs(v) < 10 ? 1 : 0);
const pctOf = v => (D.capacity_wh ? pctNum((v / D.capacity_wh) * 100) + ' %' : '–');

function dur(hours) {
  const s = Math.round(hours * 3600);
  if (s < 120) return s + ' s';
  const m = Math.round(s / 60);
  if (m < 60) return m + ' min';
  const h = Math.floor(m / 60);
  if (h < 48 && m % 60 === 0) return `${h} h`;
  if (h < 24) return `${h} h ${String(m % 60).padStart(2, '0')} min`;
  if (h % 24 === 0) return `${h / 24} d`;
  return `${Math.floor(h / 24)} d ${h % 24} h`;
}

const when = iso => new Date(iso).toLocaleString(undefined, {
  month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit',
});

// ---- Axis ticks ----

function niceStep(span, n) {
  const raw = span / n;
  const mag = 10 ** Math.floor(Math.log10(raw));
  for (const m of [1, 2, 2.5, 5, 10]) if (m * mag >= raw * (1 - 1e-9)) return m * mag;
  return 10 * mag;
}

// A value axis from 0 to a round number at or above max.
function valueAxis(max, n = 5) {
  const step = niceStep(max > 0 ? max : 1, n);
  return { max: Math.ceil(max / step - 1e-9) * step || step, step };
}

const DUR_STEPS = [1 / 60, 2 / 60, 5 / 60, 10 / 60, 15 / 60, 0.5, 1, 2, 3, 4, 6, 12, 24, 48, 72, 168];

function durStep(span, n) {
  return DUR_STEPS.find(s => s >= span / n) ?? DUR_STEPS[DUR_STEPS.length - 1];
}

function durTick(v, step) {
  if (v === 0) return '0';
  if (step >= 24) return Math.round(v / 24) + ' d';
  if (step >= 1) return Math.round(v) + ' h';
  const m = Math.round(v * 60);
  if (m < 60) return m + ' min';
  return m % 60 ? `${Math.floor(m / 60)} h ${m % 60}` : m / 60 + ' h';
}

const range = (max, step) => {
  const out = [];
  for (let i = 0; i * step <= max * (1 + 1e-9); i++) out.push(+(i * step).toFixed(10));
  return out;
};

// ---- Chart frame: grid, axes and ticks; returns the scales ----

function frame(box, { xMax, xStep, yMax, yStep, pct }) {
  box.innerHTML = '';
  const W = Math.max(box.clientWidth, 240);
  const H = box.clientHeight;
  const m = { l: 46, r: pct ? 44 : 22, t: 22, b: 26 };
  const iw = W - m.l - m.r;
  const ih = H - m.t - m.b;
  const root = svg('svg', { width: W, height: H, viewBox: `0 0 ${W} ${H}` }, box);
  const sx = v => m.l + (v / xMax) * iw;
  const sy = v => m.t + ih - (v / yMax) * ih;

  for (const v of range(yMax, yStep)) {
    svg('line', { x1: m.l, x2: m.l + iw, y1: sy(v), y2: sy(v), class: v === 0 ? 'base' : 'grid' }, root);
    svg('text', { x: m.l - 8, y: sy(v), dy: '0.32em', 'text-anchor': 'end', class: 'tick' }, root,
      +v.toFixed(6));
  }
  svg('text', { x: m.l - 8, y: m.t - 10, 'text-anchor': 'end', class: 'unit' }, root, 'Wh');

  // The % axis is the Wh axis in other units: % of the battery's full capacity now.
  if (pct && D.capacity_wh) {
    const pMax = (yMax / D.capacity_wh) * 100;
    const pStep = niceStep(pMax, 5);
    for (const p of range(pMax, pStep)) {
      const y = sy((p * D.capacity_wh) / 100);
      svg('line', { x1: m.l + iw, x2: m.l + iw + 4, y1: y, y2: y, class: 'base' }, root);
      svg('text', { x: m.l + iw + 8, y, dy: '0.32em', class: 'tick' }, root, +p.toFixed(6));
    }
    svg('line', { x1: m.l + iw, x2: m.l + iw, y1: m.t, y2: m.t + ih, class: 'base' }, root);
    svg('text', { x: m.l + iw + 8, y: m.t - 10, class: 'unit' }, root, '%');
  }

  for (const v of range(xMax, xStep)) {
    svg('text', { x: sx(v), y: m.t + ih + 18, 'text-anchor': 'middle', class: 'tick' }, root,
      durTick(v, xStep));
  }

  const layer = svg('g', {}, root);
  const hover = svg('g', { 'pointer-events': 'none' }, root);
  const hit = svg('rect', { x: m.l, y: m.t, width: iw, height: ih, fill: 'transparent' }, root);
  return { root, layer, hover, hit, sx, sy, m, iw, ih, xMax };
}

const xTicks = box => Math.max(3, Math.floor((box.clientWidth - 90) / 70));

function empty(box, text) {
  box.innerHTML = `<div class="empty">${esc(text)}</div>`;
}

// ---- Tooltip ----

const tip = $('#tip');

function showTip(html, ev) {
  tip.innerHTML = html;
  tip.hidden = false;
  const r = tip.getBoundingClientRect();
  let x = ev.clientX + 14;
  let y = ev.clientY + 14;
  if (x + r.width > innerWidth - 8) x = ev.clientX - r.width - 14;
  if (y + r.height > innerHeight - 8) y = ev.clientY - r.height - 14;
  tip.style.left = Math.max(8, x) + 'px';
  tip.style.top = Math.max(8, y) + 'px';
}

const hideTip = () => { tip.hidden = true; };
const row = (k, v, color) =>
  `<div class="r"><span>${color ? `<i class="key-dot" style="--c:${color}"></i>` : ''}${esc(k)}</span><span>${esc(v)}</span></div>`;

function onHover(f, move) {
  f.hit.addEventListener('pointermove', ev => {
    const p = f.root.getBoundingClientRect();
    move(ev.clientX - p.left, ev.clientY - p.top, ev);
  });
  f.hit.addEventListener('pointerleave', () => { f.hover.innerHTML = ''; hideTip(); });
}

// ---- Row 1: scatter plots with the robust fit ----

function renderScatter(kind) {
  const card = $(`#scatter-${kind}`);
  const box = $('.plot', card);
  const fit = D.fits[kind];
  const pts = D.rows.filter(r => r.kind === kind && !r.excluded);
  const color = COLOR[kind];
  const ok = fit && fit.slope != null;

  $('.fit', card).innerHTML = ok ? `
    <div class="param"><div class="label">Fixed cost per ${kind === 'suspend' ? 'suspend' : 'hibernation'}, a</div>
      <div class="value">${wh(fit.intercept)} <small>${pctOf(fit.intercept)}</small></div>
      <div class="eq">intercept${fit.at_bound ? ', held at the ≥ 0 bound' : ''}</div></div>
    <div class="param"><div class="label">Steady draw, b</div>
      <div class="value">${watts(fit.slope)} <small>${D.capacity_wh ? pctNum((fit.slope / D.capacity_wh) * 100) + ' %/h' : ''}</small></div>
      <div class="eq">slope, Wh per hour</div></div>
    <div class="stats">energy = a + b × duration · fitted on ${fit.n - fit.n_outliers} of ${fit.n} steps on battery,
      ${fit.n_outliers} rejected as outlier${fit.n_outliers === 1 ? '' : 's'} · residual scale ${(fit.scale * 1000).toFixed(0)} mWh</div>`
    : `<div class="stats">Not enough ${kind === 'suspend' ? 'suspends' : 'hibernations'} on battery to fit a line yet (${pts.length}).</div>`;
  $('.legend', card).innerHTML = `
    <span><i class="key-dot" style="--c:${color}"></i>${NAME[kind]} on battery</span>
    <span><i class="key-ring"></i>Outlier, ignored by the fit</span>
    <span><i class="key-line" style="--c:${color}"></i>Robust fit</span>`;

  if (!pts.length) return empty(box, 'No steps on battery yet');
  const xs = pts.map(r => r.duration_s / 3600);
  const xRaw = Math.max(...xs, 1 / 60) * 1.05;
  const xStep = durStep(xRaw, xTicks(box));
  const xMax = Math.ceil(xRaw / xStep - 1e-9) * xStep;
  const line = x => fit.intercept + fit.slope * x;
  const y = valueAxis(Math.max(...pts.map(r => r.used_wh), ok ? line(xMax) : 0) * 1.08);
  const f = frame(box, { xMax, xStep, yMax: y.max, yStep: y.step });

  if (ok) {
    svg('line', {
      x1: f.sx(0), y1: f.sy(line(0)), x2: f.sx(xMax), y2: f.sy(line(xMax)),
      stroke: color, 'stroke-width': 2, 'stroke-linecap': 'round',
    }, f.layer);
  }
  // Outliers under the rest, then each step as a dot ringed in the surface color.
  const placed = [];
  for (const r of [...pts].sort((a, b) => b.outlier - a.outlier)) {
    const cx = f.sx(r.duration_s / 3600);
    const cy = f.sy(r.used_wh);
    svg('circle', r.outlier
      ? { cx, cy, r: 4, fill: 'none', stroke: 'var(--muted)', 'stroke-width': 1.5 }
      : { cx, cy, r: 4.5, fill: color, stroke: 'var(--surface)', 'stroke-width': 2 }, f.layer);
    placed.push({ r, cx, cy });
  }

  onHover(f, (mx, my, ev) => {
    let best = null;
    let bd = 24 ** 2;
    for (const p of placed) {
      const d = (p.cx - mx) ** 2 + (p.cy - my) ** 2;
      if (d < bd) { bd = d; best = p; }
    }
    f.hover.innerHTML = '';
    if (!best) return hideTip();
    svg('circle', { cx: best.cx, cy: best.cy, r: 8, fill: 'none', stroke: 'var(--text-1)', 'stroke-width': 1.5 }, f.hover);
    const r = best.r;
    const h = r.duration_s / 3600;
    showTip(`<div class="t">${esc(when(r.start))}</div>
      ${row('Lasted', dur(h))}
      ${row('Used', `${wh(r.used_wh)} (${r.capacity_start}→${r.capacity_end} %)`)}
      ${row('Average', watts(r.avg_w))}
      ${ok ? row('Fit says', wh(line(h))) : ''}
      ${r.hw_sleep_s != null ? row('Deepest sleep', `${Math.round((r.hw_sleep_s / r.duration_s) * 100)} %`) : ''}
      ${row('In the fit', r.outlier ? 'no, outlier' : `yes, weight ${r.weight.toFixed(2)}`)}
      ${r.operation !== r.phase ? row('Part of', r.operation) : ''}
      <div class="w">Woken by: ${esc(r.woke || 'none recorded')}</div>`, ev);
  });
}

// ---- Row 3: predictions ----

function fitOf(kind) {
  const f = D.fits[kind];
  return f && f.slope != null ? { a: f.intercept, b: f.slope } : null;
}

// Adds a crosshair to a prediction chart: rows(t) gives the tooltip rows and the dots to mark.
function crosshair(f, title, rows) {
  onHover(f, (mx, my, ev) => {
    const t = Math.min(Math.max(((mx - f.m.l) / f.iw) * f.xMax, 0), f.xMax);
    f.hover.innerHTML = '';
    const x = f.sx(t);
    svg('line', { x1: x, x2: x, y1: f.m.t, y2: f.m.t + f.ih, class: 'cross' }, f.hover);
    const { lines, dots } = rows(t);
    for (const [v, c] of dots) {
      svg('circle', { cx: x, cy: f.sy(v), r: 4.5, fill: c, stroke: 'var(--surface)', 'stroke-width': 2 }, f.hover);
    }
    showTip(`<div class="t">${esc(title(t))}</div>${lines.join('')}`, ev);
  });
}

function endLabel(f, x, v) {
  svg('text', { x: f.sx(x) - 2, y: f.sy(v) - 9, 'text-anchor': 'end', class: 'end-label' }, f.layer,
    `${wh(v)} · ${pctOf(v)}`);
}

function renderPrediction(kind) {
  const card = $(`#pred-${kind}`);
  const box = $('.plot', card);
  const p = fitOf(kind);
  const H = state.horizon;
  if (!p) {
    $('.readout', card).textContent = '';
    return empty(box, 'No fit yet');
  }
  const E = t => p.a + p.b * t;
  // A full battery runs out at tEmpty: the line stops there.
  const cap = D.capacity_wh;
  const tEmpty = cap && p.b > 0 ? Math.max(cap - p.a, 0) / p.b : Infinity;
  const tEnd = Math.min(H, tEmpty);
  const y = valueAxis(E(tEnd) * 1.12);
  const f = frame(box, { xMax: H, xStep: durStep(H, xTicks(box)), yMax: y.max, yStep: y.step, pct: true });
  const c = COLOR[kind];
  svg('path', {
    d: `M${f.sx(0)},${f.sy(0)} L${f.sx(0)},${f.sy(E(0))} L${f.sx(tEnd)},${f.sy(E(tEnd))} L${f.sx(tEnd)},${f.sy(0)} Z`,
    fill: c, 'fill-opacity': 0.14,
  }, f.layer);
  svg('line', { x1: f.sx(0), y1: f.sy(E(0)), x2: f.sx(tEnd), y2: f.sy(E(tEnd)), stroke: c, 'stroke-width': 2, 'stroke-linecap': 'round' }, f.layer);
  if (tEnd < H) {
    const x = f.sx(tEnd);
    svg('circle', { cx: x, cy: f.sy(E(tEnd)), r: 4.5, fill: c, stroke: 'var(--surface)', 'stroke-width': 2 }, f.layer);
    const right = x > f.m.l + f.iw * 0.6;
    svg('text', {
      x: right ? x - 10 : x + 10, y: f.sy(E(tEnd)) + 4, 'text-anchor': right ? 'end' : 'start', class: 'end-label',
    }, f.layer, `empty at ${dur(tEnd)}`);
  } else {
    endLabel(f, H, E(H));
  }
  crosshair(f, t => `${NAME[kind]} for ${dur(t)}`, t => (t > tEmpty
    ? { lines: [row('Battery', 'empty')], dots: [] }
    : { lines: [row('Used', wh(E(t)), c), row('Of the battery', pctOf(E(t)))], dots: [[E(t), c]] }));
  $('.readout', card).innerHTML =
    `Fixed <b>${wh(p.a)}</b> (${pctOf(p.a)}), then <b>${watts(p.b)}</b>` +
    (D.capacity_wh ? ` (${pctNum((p.b / D.capacity_wh) * 100)} %/h)` : '') +
    (p.b > 0 && D.capacity_wh ? ` · a full battery lasts <b>${dur(Math.max(D.capacity_wh - p.a, 0) / p.b)}</b>` : '');
}

// Suspend-then-hibernate: suspend for the delay, then hibernate, paying hibernation's fixed cost.
function optimalDelay() {
  const s = fitOf('suspend');
  const h = fitOf('hibernate');
  if (!s || !h || s.b <= h.b) return null;
  return h.a / (s.b - h.b);
}

function renderSth() {
  const card = $('#pred-sth');
  const box = $('.plot', card);
  const s = fitOf('suspend');
  const h = fitOf('hibernate');
  const H = state.horizon;
  const delay = state.delay;
  $('#delay-out').textContent = dur(delay);
  $('.legend', card).innerHTML = `
    <span><i class="key-band" style="--c:${COLOR.suspend}"></i>Used in suspend</span>
    <span><i class="key-band" style="--c:${COLOR.hibernate}"></i>Used in hibernation</span>`;
  if (!s || !h) {
    $('.readout', card).textContent = '';
    return empty(box, 'Needs both fits');
  }
  const sus = t => s.a + s.b * Math.min(t, delay);
  const hib = t => (t < delay ? 0 : h.a + h.b * (t - delay));
  const total = t => sus(t) + hib(t);
  const Dc = Math.min(delay, H);
  const top = Math.max(total(H), delay <= H ? sus(delay) + h.a : 0);
  const y = valueAxis(top * 1.12);
  const f = frame(box, { xMax: H, xStep: durStep(H, xTicks(box)), yMax: y.max, yStep: y.step, pct: true });
  const P = (t, v) => `${f.sx(t)},${f.sy(v)}`;
  const E_D = sus(Dc);

  // The suspend share, then the hibernation share stacked on it from the delay on.
  svg('path', { d: `M${P(0, 0)} L${P(0, s.a)} L${P(Dc, E_D)} L${P(H, E_D)} L${P(H, 0)} Z`, fill: COLOR.suspend, 'fill-opacity': 0.16 }, f.layer);
  svg('line', { x1: f.sx(0), y1: f.sy(s.a), x2: f.sx(Dc), y2: f.sy(E_D), stroke: COLOR.suspend, 'stroke-width': 2, 'stroke-linecap': 'round' }, f.layer);
  if (delay < H) {
    const T = total(H);
    svg('path', { d: `M${P(delay, E_D)} L${P(delay, E_D + h.a)} L${P(H, T)} L${P(H, E_D)} Z`, fill: COLOR.hibernate, 'fill-opacity': 0.22 }, f.layer);
    svg('polyline', {
      points: `${P(delay, E_D)} ${P(delay, E_D + h.a)} ${P(H, T)}`,
      fill: 'none', stroke: COLOR.hibernate, 'stroke-width': 2, 'stroke-linejoin': 'round', 'stroke-linecap': 'round',
    }, f.layer);
    const x = f.sx(delay);
    const nearRight = x > f.m.l + f.iw - 90;
    svg('text', { x: x + (nearRight ? -6 : 6), y: f.m.t + 4, 'text-anchor': nearRight ? 'end' : 'start', class: 'marker-label' }, f.layer,
      `hibernates at ${dur(delay)}`);
    svg('line', { x1: x, x2: x, y1: f.m.t - 4, y2: f.sy(E_D), class: 'base' }, f.layer);
  }
  endLabel(f, H, total(H));

  crosshair(f, t => `Asleep for ${dur(t)}`, t => ({
    lines: [
      row('Total', `${wh(total(t))} · ${pctOf(total(t))}`),
      row('In suspend', wh(sus(t)), COLOR.suspend),
      row('In hibernation', wh(hib(t)), COLOR.hibernate),
      row('Suspend only', D.capacity_wh && s.a + s.b * t > D.capacity_wh ? 'battery empty' : wh(s.a + s.b * t)),
    ],
    dots: t < delay ? [[total(t), COLOR.suspend]] : [[sus(t), COLOR.suspend], [total(t), COLOR.hibernate]],
  }));

  const onlySus = s.a + s.b * H;
  const susEmpty = D.capacity_wh && onlySus > D.capacity_wh;
  $('.readout', card).innerHTML = `Over ${dur(H)}: <b>${wh(total(H))}</b> (${pctOf(total(H))}),
    against ${susEmpty ? `an empty battery after ${dur((D.capacity_wh - s.a) / s.b)}`
    : `${wh(onlySus)} (${pctOf(onlySus)})`} staying in suspend.`;
}

function setupSth() {
  const input = $('#delay');
  const opt = optimalDelay();
  const sys = D.hibernate_delay;
  input.max = Math.round(Math.min(state.horizon, 12) * 60);
  state.delay = Math.min(state.delay, input.max / 60);
  input.value = Math.round(state.delay * 60);

  const optBtn = $('#opt');
  optBtn.disabled = opt == null;
  optBtn.textContent = opt == null ? 'Optimal: no fits' : `Optimal: ${dur(opt)}`;
  const sysBtn = $('#sys');
  sysBtn.disabled = !sys;
  sysBtn.textContent = sys ? `System setting: ${dur(sys.seconds / 3600)}` : 'System setting: none';
  sysBtn.title = sys ? `HibernateDelaySec=${sys.value} in ${sys.file}` : 'No HibernateDelaySec in sleep.conf';

  const s = fitOf('suspend');
  const h = fitOf('hibernate');
  $('#opt-note').innerHTML = opt == null ? '' :
    `<b>Optimal delay</b> = a<sub>hib</sub> / (b<sub>susp</sub> − b<sub>hib</sub>) =
    ${wh(h.a)} / (${watts(s.b)} − ${watts(h.b)}) = <b>${dur(opt)}</b>: hibernate once staying in
    suspend has cost as much extra as one hibernation's fixed cost (ignoring hibernation's own draw,
    a<sub>hib</sub> / b<sub>susp</sub> = ${dur(h.a / s.b)}). Waking before then, staying in suspend
    was the cheapest choice; sleeping longer, it costs at most ${wh(h.a)} more than hibernating
    straight away would have. You don't need to know in advance how long you'll sleep.`;
}

function setDelay(hours) {
  state.delay = Math.max(0, hours);
  $('#delay').value = Math.round(state.delay * 60);
  renderSth();
}

// ---- Battery tiles ----

function renderBattery() {
  const b = D.battery;
  const box = $('#battery');
  const tile = (label, value, detail, cls = '') =>
    `<div class="tile ${cls}"><div class="label">${esc(label)}</div><div class="value">${esc(value)}</div>${detail ? `<div class="detail">${esc(detail)}</div>` : ''}</div>`;
  if (!b) {
    box.innerHTML = tile('Full capacity', D.capacity_wh ? wh(D.capacity_wh) : '–',
      'No battery found; % uses the newest logged capacity', 'hero');
    return;
  }
  const full = b.energy_full_wh;
  const tiles = [
    tile('Full capacity now', full != null ? `${full.toFixed(2)} Wh` : '–',
      full ? `1 % of the battery = ${(full / 100).toFixed(3)} Wh` : '', 'hero'),
    tile('Charge', b.capacity_pct != null ? `${b.capacity_pct} %` : '–',
      b.energy_now_wh != null ? `${b.energy_now_wh.toFixed(2)} Wh stored` : ''),
    tile('Health', full && b.energy_full_design_wh ? `${((full / b.energy_full_design_wh) * 100).toFixed(1)} %` : '–',
      b.energy_full_design_wh ? `of ${b.energy_full_design_wh.toFixed(2)} Wh when new` : ''),
    tile('Power source', b.ac_online == null ? '–' : b.ac_online ? 'AC' : 'Battery', b.status || ''),
  ];
  const w = b.power_now_w;
  if (b.status === 'Charging' && w) tiles.push(tile('Charging at', `${w.toFixed(1)} W`, ''));
  else if (b.status === 'Discharging' && w) tiles.push(tile('Draw now', `${w.toFixed(1)} W`, full ? `${pctNum((w / full) * 100)} %/h` : ''));
  if (b.charge_limit_pct != null) tiles.push(tile('Charge limit', `${b.charge_limit_pct} %`, 'charging stops here'));
  if (b.voltage_now_v != null) tiles.push(tile('Voltage', `${b.voltage_now_v.toFixed(2)} V`, ''));
  tiles.push(tile('Cycles', b.cycle_count ?? 'not reported', ''));
  tiles.push(tile('Battery', [b.manufacturer, b.model].filter(Boolean).join(' ') || b.name, b.technology || ''));
  box.innerHTML = tiles.join('');
}

// ---- Table of every logged step ----

function renderTable() {
  const rows = [...D.rows].reverse();
  const fmt = (v, d) => (v == null ? '' : v.toFixed(d));
  $('#rows').innerHTML = `<thead><tr><th>Start</th><th>Step</th><th class="num">Lasted</th>
    <th class="num">Used</th><th class="num">Average</th><th class="num">Battery</th><th>In the fit</th><th>Woken by</th></tr></thead>
    <tbody>${rows.map(r => `<tr>
      <td>${esc(when(r.start))}</td>
      <td><span class="tag">${r.kind ? `<i class="key-dot" style="--c:${COLOR[r.kind]}"></i>` : ''}${esc(r.phase)}</span></td>
      <td class="num">${r.duration_s != null ? dur(r.duration_s / 3600) : ''}</td>
      <td class="num">${fmt(r.used_wh, 3)} Wh</td>
      <td class="num">${fmt(r.avg_w, 3)} W</td>
      <td class="num">${r.capacity_start ?? ''}→${r.capacity_end ?? ''} %</td>
      <td class="${r.excluded || r.outlier ? 'dim' : ''}">${r.excluded ? 'no, ' + esc(r.excluded) : r.outlier ? 'no, outlier' : 'yes'}</td>
      <td class="woke">${esc(r.woke)}</td></tr>`).join('')}</tbody>`;
}

// ---- Page ----

function renderMeta() {
  const used = D.rows.filter(r => r.kind && !r.excluded).length;
  const left = Object.entries(D.excluded).map(([k, n]) => `${n} ${k}`).join(', ');
  $('#meta').textContent = `${D.rows.length} steps in ${D.csv} · ${used} on battery count towards the fits` +
    (left ? ` · left out: ${left}` : '') + ` · read ${new Date(D.generated).toLocaleTimeString()}`;
  $('#error').hidden = !D.error;
  $('#error').textContent = D.error || '';
  $('#method').innerHTML = `Only steps on battery throughout count: on AC the battery charges, and rows
    with an unsynced clock have an unsure duration. Each fit is <b>energy = a + b × duration</b> by
    robust regression (Huber, then Tukey bisquare reweighting): points far from the line get zero
    weight and stop pulling on it, with a held at ≥ 0. A step's energy includes going down and
    waking up, which is what a measures. Percentages are of the battery's full capacity now
    (${D.capacity_wh ? wh(D.capacity_wh) : 'unknown'}), as the battery's own % is.`;
}

function renderPlots() {
  renderScatter('suspend');
  renderScatter('hibernate');
  renderPrediction('suspend');
  renderPrediction('hibernate');
  renderSth();
}

function renderAll() {
  renderMeta();
  renderBattery();
  setupSth();
  renderPlots();
  renderTable();
}

async function load(first) {
  try {
    const res = await fetch('api/data', { cache: 'no-store' });
    D = await res.json();
  } catch (e) {
    $('#error').hidden = false;
    $('#error').textContent = `Can't reach the sleep_stats server (${e.message}). Is it still running?`;
    return;
  }
  if (first) {
    // Start the slider at the delay the system uses now.
    if (D.hibernate_delay) state.delay = D.hibernate_delay.seconds / 3600;
  }
  renderAll();
}

function setHorizon(hours) {
  state.horizon = hours;
  for (const b of document.querySelectorAll('#horizon button')) {
    b.setAttribute('aria-pressed', String(+b.dataset.h === hours));
  }
  try { localStorage.setItem('sleep-stats-horizon', hours); } catch { /* Not needed */ }
  if (!D) return;
  setupSth();
  renderPrediction('suspend');
  renderPrediction('hibernate');
  renderSth();
}

$('#horizon').addEventListener('click', ev => {
  const h = ev.target.closest('button')?.dataset.h;
  if (h) setHorizon(+h);
});
$('#delay').addEventListener('input', ev => setDelay(ev.target.value / 60));
$('#opt').addEventListener('click', () => { const d = optimalDelay(); if (d != null) setDelay(d); });
$('#sys').addEventListener('click', () => { if (D.hibernate_delay) setDelay(D.hibernate_delay.seconds / 3600); });
$('#reload').addEventListener('click', () => load(false));

let resizeTimer;
addEventListener('resize', () => {
  clearTimeout(resizeTimer);
  resizeTimer = setTimeout(() => D && renderPlots(), 100);
});

let saved = 24;
try { saved = +localStorage.getItem('sleep-stats-horizon') || 24; } catch { /* Not needed */ }
setHorizon([12, 24, 48, 168].includes(saved) ? saved : 24);
load(true);
