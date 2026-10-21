(function () {
  'use strict';

  // ---------------------------------------------------------------------------
  // Data and helpers
  // ---------------------------------------------------------------------------
  const D = window.TOD;
  const S = D.summary;
  const A = S.assumptions;
  const SC = S.scenarios;
  const SC_BY = Object.fromEntries(SC.map(s => [s.key, s]));
  const LEV = S.levers;
  const ST = D.stations;
  const ST_BY = Object.fromEntries(ST.map(s => [s.id, s]));
  const ST_BY_NAME = Object.fromEntries(ST.map(s => [s.name.toLowerCase(), s]));
  const WARDS = Object.fromEntries(D.wards.map(w => [String(w.ward), w]));
  const nf = new Intl.NumberFormat('en-US');
  const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  const narrow = () => window.matchMedia('(max-width: 640px)').matches;
  const coarse = window.matchMedia('(pointer: coarse)').matches;

  // Honest precision: two significant figures, nearest 50 under 1,000
  function approx(n) {
    if (n == null || isNaN(n)) return '-';
    if (n < 50) return 'fewer than 50';
    let r;
    if (n < 1000) r = Math.round(n / 50) * 50;
    else { const p = Math.pow(10, Math.floor(Math.log10(n)) - 1); r = Math.round(n / p) * p; }
    return nf.format(r);
  }
  const short = n => (n < 50 ? 'under 50' : approx(n));
  const homes = n => (n < 50 ? 'fewer than 50 new homes' : 'about ' + approx(n) + ' new homes');
  const pct = x => Math.round(x * 100) + '%';
  const esc = s => String(s == null ? '' : s).replace(/[&<>"']/g,
    c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));

  const CONF_LABEL = { solid: 'Solid', likely: 'Likely', rough: 'Rough' };
  const CONF_TIP = {
    solid: 'Based on County Assessor records',
    likely: 'Estimated from building footprints, or land with no buildings',
    rough: 'Estimated, with the number of stories filled in from similar buildings'
  };
  const confTag = c => {
    const k = CONF_LABEL[c] ? c : 'likely';
    return `<span class="conf conf-${k}" title="${CONF_TIP[k]}">${CONF_LABEL[k]}</span>`;
  };
  const TYPE_COLOR = { 'Private': '#8A97A3', 'Other public': '#3C7A5A', 'Transit-owned': '#1C5D99' };
  const TYPOLOGY = {
    'Downtown': 'Downtown station',
    'CTA neighborhood': 'Neighborhood L station',
    'Metra neighborhood': 'Neighborhood Metra station'
  };
  const TIER = {
    'Fourplex': 'four-flat-size lot',
    'Courtyard building': 'courtyard-building-size lot',
    'Station target': 'large site',
    'Station target (Downtown)': 'downtown site'
  };

  const lazy = {};
  function need(src) {
    if (!lazy[src]) {
      lazy[src] = new Promise((resolve, reject) => {
        const s = document.createElement('script');
        s.src = src; s.onload = resolve; s.onerror = () => reject(new Error('Could not load ' + src));
        document.body.appendChild(s);
      });
    }
    return lazy[src];
  }
  const $ = id => document.getElementById(id);
  // These layers are lines and shapes only; render any stray point as nothing
  const noPoint = (f, latlng) => L.circleMarker(latlng, { radius: 0, stroke: false, fill: false, interactive: false });
  // R's JSON writer turns one-item vectors into plain values; treat them as lists
  const arr = x => (Array.isArray(x) ? x : (x == null ? [] : [x]));

  // ---------------------------------------------------------------------------
  // State and shareable links (#station=...&scenario=...)
  // ---------------------------------------------------------------------------
  const state = { scenario: 'near', station: null };
  (function readHash() {
    const h = new URLSearchParams(location.hash.slice(1));
    if (SC_BY[h.get('scenario')]) state.scenario = h.get('scenario');
    if (ST_BY[h.get('station')]) state.station = h.get('station');
  })();
  function writeHash() {
    const h = new URLSearchParams();
    h.set('scenario', state.scenario);
    if (state.station) h.set('station', state.station);
    history.replaceState(null, '', '#' + h.toString());
  }

  // ---------------------------------------------------------------------------
  // Hero
  // ---------------------------------------------------------------------------
  (function hero() {
    const near = SC_BY.near.units, max = SC_BY.max.units, aor = SC_BY.aor.units;
    const share = Math.min(1, Math.max(0, aor / max));
    $('hero-title').textContent =
      `About ${approx(near)} new homes could fit on open land near Chicago's L and Metra stations`;
    $('hero-lede').textContent =
      `That's vacant lots and parking alone. Counting every underused site, and fourplexes on single-family lots, ` +
      `station areas could hold about ${approx(max)} new homes, alongside about ${approx(S.homes_today)} there today. ` +
      `Today's zoning allows about ${pct(share)} of them.`;
    const bar = $('hero-bar');
    bar.setAttribute('aria-label', `${pct(share)} of build-out capacity is allowed under today's zoning`);
    bar.innerHTML =
      `<span style="width:${(100 * share).toFixed(1)}%;background:var(--flag-blue)"></span>` +
      `<span style="width:${(100 * (1 - share)).toFixed(1)}%;background:var(--flag-red)"></span>`;
    $('hero-key').innerHTML =
      `<span><i class="sw" style="background:var(--flag-blue)"></i><b>${approx(aor)}</b> allowed by today's zoning</span>` +
      `<span><i class="sw" style="background:var(--flag-red)"></i><b>${approx(max - aor)}</b> need a zoning change, public land, or a project to move forward</span>`;
  })();

  // ---------------------------------------------------------------------------
  // Scenario buttons
  // ---------------------------------------------------------------------------
  const btnWrap = $('scenario-buttons');
  SC.forEach(s => {
    const b = document.createElement('button');
    b.type = 'button'; b.textContent = s.label; b.dataset.key = s.key;
    b.addEventListener('click', () => setScenario(s.key));
    btnWrap.appendChild(b);
  });
  const scenSel = document.createElement('select');
  scenSel.id = 'scenario-select';
  scenSel.className = 'scen-select';
  scenSel.setAttribute('aria-label', 'Scenario');
  scenSel.innerHTML = SC.map(s => `<option value="${s.key}">${esc(s.label)}</option>`).join('');
  scenSel.addEventListener('change', () => setScenario(scenSel.value));
  btnWrap.after(scenSel);
  function setScenario(k) {
    state.scenario = k;
    btnWrap.querySelectorAll('button').forEach(b => b.setAttribute('aria-pressed', String(b.dataset.key === k)));
    scenSel.value = k;
    const s = SC_BY[k];
    $('scenario-desc').textContent = `${s.description} Citywide: ${homes(s.units)}.`;
    writeHash();
    drawStations();
    if (state.station) renderProfile(state.station);
  }

  // ---------------------------------------------------------------------------
  // Map
  // ---------------------------------------------------------------------------
  // SVG rendering, not canvas: with canvas, each pane gets its own full-size
  // canvas, and the ones on top (stations, rail) block clicks on parcels below
  const map = L.map('map', { scrollWheelZoom: false, zoomControl: false, dragging: !coarse })
    .setView([41.85, -87.68], 11);
  L.control.zoom({ position: 'topright' }).addTo(map);   // keeps clear of the menu button on phones
  if (coarse) {
    const hint = L.control({ position: 'topleft' });
    hint.onAdd = () => {
      const d = L.DomUtil.create('div', 'map-hint');
      d.textContent = 'Use two fingers to move the map';
      return d;
    };
    hint.addTo(map);
  }
  window.TOD_MAP = map;   // handy for debugging in the browser console
  map.on('focus click', () => map.scrollWheelZoom.enable());
  map.getContainer().addEventListener('mouseleave', () => map.scrollWheelZoom.disable());

  // Parcels sit above wards and income so those filled layers can't block parcel clicks
  [['income', 340], ['wardsP', 350], ['sites', 365], ['rail', 380]].forEach(([n, z]) => {
    map.createPane(n); map.getPane(n).style.zIndex = z;
  });

  const ESRI = 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}';
  const ESRI_ATTR = 'Tiles &copy; Esri &mdash; Esri, HERE, Garmin, &copy; OpenStreetMap contributors';
  L.tileLayer(ESRI, { maxZoom: 16, attribution: ESRI_ATTR }).addTo(map);

  const railStyle = f => ({
    color: f.properties.color, opacity: 0.9,
    weight: f.properties.agency === 'CTA' ? 3.5 : 2.5,
    dashArray: f.properties.agency === 'Metra' ? '6 4' : null
  });
  function railLayer(pane) {
    return L.geoJSON(window.TOD_RAIL, {
      pane, style: railStyle, pointToLayer: noPoint,
      onEachFeature: (f, l) => {
        l.bindTooltip(esc(f.properties.label), { sticky: true });
        l.bindPopup(`<strong>${esc(f.properties.label)}</strong>`);
      }
    });
  }
  const rail = railLayer('rail').addTo(map);

  const stationLayer = L.layerGroup().addTo(map);
  let markers = {};

  // Legend titles toggle the legend open and closed (collapsed to start on phones)
  function collapsible(div) {
    if (narrow()) div.classList.add('collapsed');
    L.DomEvent.disableClickPropagation(div);
    const toggle = () => {
      div.classList.toggle('collapsed');
      const t = div.querySelector('.lg-title');
      if (t) t.setAttribute('aria-expanded', String(!div.classList.contains('collapsed')));
    };
    div.addEventListener('click', e => { if (e.target.closest('.lg-title')) toggle(); });
    div.addEventListener('keydown', e => {
      if (e.target.closest('.lg-title') && (e.key === 'Enter' || e.key === ' ')) { e.preventDefault(); toggle(); }
    });
    return div;
  }
  const lgTitle = (t, div) => `<div class="lg-title" role="button" tabindex="0" aria-expanded="${!(div && div.classList.contains('collapsed'))}">${t}</div>`;

  const sizeLegend = L.control({ position: 'bottomright' });
  sizeLegend.onAdd = () => collapsible(L.DomUtil.create('div', 'map-legend'));
  sizeLegend.addTo(map);

  const radius = (u, umax) => 3 + 22 * Math.sqrt(Math.max(u, 0) / umax);

  function drawStations() {
    stationLayer.clearLayers();
    markers = {};
    const k = state.scenario;
    const umax = Math.max(1, ...ST.map(s => s.units[k] || 0));
    ST.forEach(s => {
      const sel = s.id === state.station;
      const m = L.circleMarker([s.lat, s.lng], {
        radius: radius(s.units[k] || 0, umax),
        weight: sel ? 3 : 1, color: sel ? '#E4002B' : '#1F2A33',
        fillColor: '#41B6E6', fillOpacity: 0.78
      });
      m.on('click', () => selectStation(s.id, false));
      m.addTo(stationLayer);
      markers[s.id] = m;
    });
    setLabels();
    // One row per reference size, so labels never collide
    const r0 = radius(umax, umax);
    sizeLegend.getContainer().innerHTML =
      lgTitle('New homes near each station', sizeLegend.getContainer()) +
      [umax, umax / 4, umax / 16].map(v => {
        const r = radius(v, umax);
        return `<div class="row"><svg width="${2 * r0 + 2}" height="${2 * r + 2}" aria-hidden="true">` +
          `<circle cx="${r0 + 1}" cy="${r + 1}" r="${r}" fill="#41B6E6" fill-opacity="0.78" stroke="#1F2A33"/></svg>${short(v)}</div>`;
      }).join('');
  }

  // Restyle markers in place when the selection changes
  function highlight() {
    Object.entries(markers).forEach(([id, m]) => {
      const sel = id === state.station;
      m.setStyle({ weight: sel ? 3 : 1, color: sel ? '#E4002B' : '#1F2A33' });
      if (sel) m.bringToFront();
    });
  }

  function setLabels() {
    const z = map.getZoom();
    ST.forEach(s => {
      const m = markers[s.id];
      if (!m) return;
      m.unbindTooltip();
      if (z >= 13) {
        m.bindTooltip(esc(s.name), { permanent: true, direction: 'right', offset: [6, 0], className: 'st-label' });
      } else {
        m.bindTooltip(`<strong>${esc(s.name)}</strong><br>${homes(s.units[state.scenario] || 0)}`, { direction: 'top' });
      }
    });
  }
  map.on('zoomend', setLabels);

  // ---- Optional layers -------------------------------------------------------
  const INST_FILL = '#C9CED2';
  function sitePopup(p) {
    if (p.inst) {
      return `<strong>${esc(p.inst)}</strong><br>` +
        `<span class="pop-note">Shown for reference only. Schools and college campuses are not considered in any of these redevelopment scenarios, even if there are opportunities for more density in partnership with these organizations.</span>`;
    }
    return `<strong>${esc(p.owner)}</strong><br>${esc(p.type)} land near ${esc(p.station)}<br>` +
      `${p.acres} acres, ${esc(TIER[p.tier] || 'site')}<br>` +
      `Could hold ${homes(p.units)}; today's zoning allows ${p.allowed < 50 ? 'few or none' : 'about ' + approx(p.allowed)}<br>` +
      `${esc(p.status)}${p.zoning ? ' (' + esc(p.zoning) + ')' : ''}<br>` +
      `Building data: ${confTag(p.conf)}${p.campus ? '<br>Active campus' : ''}`;
  }

  // Active schools and campuses: flat grey with a dashed edge, clearly "not this"
  function siteStyle(p, hover) {
    if (p.inst) return { fillColor: INST_FILL, fillOpacity: hover ? 0.75 : 0.55, color: '#8A97A3',
                         weight: hover ? 1.5 : 0.8, dashArray: '3 3' };
    return { fillColor: TYPE_COLOR[p.type], fillOpacity: hover ? 0.85 : 0.7, color: '#1F2A33',
             weight: hover ? 2 : 0.4, dashArray: null };
  }

  let sitesLayer = null, sitesLegend = null;
  async function toggleSites(on) {
    if (on) {
      await need('data/sites.js');
      if (!sitesLayer) {
        sitesLayer = L.geoJSON(window.TOD_SITES, {
          pane: 'sites', pointToLayer: noPoint,
          style: f => siteStyle(f.properties, false),
          onEachFeature: (f, l) => {
            l.bindPopup(sitePopup(f.properties), { maxWidth: narrow() ? 240 : 300 });
            l.on('mouseover', () => l.setStyle(siteStyle(f.properties, true)));
            l.on('mouseout', () => l.setStyle(siteStyle(f.properties, false)));
          }
        });
        sitesLegend = L.control({ position: 'bottomleft' });
        sitesLegend.onAdd = () => {
          const d = collapsible(L.DomUtil.create('div', 'map-legend'));
          d.innerHTML = lgTitle('Site owner', d) + Object.entries(TYPE_COLOR)
            .map(([k, c]) => `<div class="row"><span class="chip" style="background:${c}"></span>${k}</div>`).join('') +
            `<div class="row"><span class="chip" style="background:${INST_FILL};outline:1px dashed #8A97A3"></span>Active school or campus (not counted)</div>`;
          return d;
        };
      }
      sitesLayer.addTo(map); sitesLegend.addTo(map);
    } else if (sitesLayer) { map.removeLayer(sitesLayer); sitesLegend.remove(); }
  }

  const INC = ['#f2f0f7', '#cbc9e2', '#9e9ac8', '#756bb1', '#54278f'];
  let incomeLayer = null, incomeLegend = null;
  async function toggleIncome(on) {
    if (on) {
      await need('data/tracts.js');
      if (!incomeLayer) {
        incomeLayer = L.geoJSON(window.TOD_TRACTS, {
          pane: 'income', pointToLayer: noPoint,
          style: f => ({ fillColor: INC[f.properties.bin] || '#ccc', fillOpacity: 0.55, weight: 0 }),
          onEachFeature: (f, l) => l.bindTooltip(`Median household income about $${approx(f.properties.income)}`, { sticky: true })
        });
        incomeLegend = L.control({ position: 'bottomleft' });
        incomeLegend.onAdd = () => {
          const d = collapsible(L.DomUtil.create('div', 'map-legend'));
          d.innerHTML = lgTitle('Median household income', d) + S.income_labels
            .map((t, i) => `<div class="row"><span class="chip" style="background:${INC[i]}"></span>${esc(t)}</div>`).join('');
          return d;
        };
      }
      incomeLayer.addTo(map); incomeLegend.addTo(map);
    } else if (incomeLayer) { map.removeLayer(incomeLayer); incomeLegend.remove(); }
  }

  let wardsLayer = null;
  async function toggleWards(on) {
    if (on) {
      await need('data/wards.js');
      if (!wardsLayer) {
        wardsLayer = L.geoJSON(window.TOD_WARDS, {
          pane: 'wardsP', pointToLayer: noPoint,
          style: { color: '#1F2A33', weight: 1.2, fill: true, fillOpacity: 0 },
          onEachFeature: (f, l) => {
            const w = String(f.properties.ward);
            l.bindTooltip(`Ward ${w}`, { sticky: true });
            l.bindPopup(() => {
              const W = WARDS[w];
              if (!W) return `<strong>Ward ${w}</strong><br>No station-area sites in this ward.`;
              const u = W.units || {};
              return `<strong>Ward ${w}</strong><br>` +
                ['near', 'aor', 'max'].map(k => `${esc(SC_BY[k].label)}: ${homes(u[k] || 0)}`).join('<br>') +
                `<br><a href="${W.print_url}">Printable ward summary</a>`;
            });
            l.on('click', () => setWard(w));
          }
        });
      }
      wardsLayer.addTo(map);
    } else if (wardsLayer) map.removeLayer(wardsLayer);
  }

  $('lyr-rail').addEventListener('change', e => (e.target.checked ? rail.addTo(map) : map.removeLayer(rail)));
  $('lyr-sites').addEventListener('change', e => toggleSites(e.target.checked).catch(showLoadError));
  $('lyr-income').addEventListener('change', e => toggleIncome(e.target.checked).catch(showLoadError));
  $('lyr-wards').addEventListener('change', e => toggleWards(e.target.checked).catch(showLoadError));
  function showLoadError(err) {
    $('scenario-desc').textContent = `${err.message}. Check that the data folder was published alongside this page.`;
  }

  // ---------------------------------------------------------------------------
  // Station picker and profile
  // ---------------------------------------------------------------------------
  const dl = $('station-list');
  ST.slice().sort((a, b) => a.name.localeCompare(b.name)).forEach(s => {
    const o = document.createElement('option'); o.value = s.name; dl.appendChild(o);
  });
  const search = $('station-search');
  function trySearch(final) {
    const q = search.value.trim().toLowerCase();
    if (!q) return;
    let s = ST_BY_NAME[q];
    if (!s && final) s = ST.find(x => x.name.toLowerCase().includes(q));
    if (s) selectStation(s.id, true);
  }
  search.addEventListener('change', () => trySearch(true));
  search.addEventListener('input', () => trySearch(false));
  search.addEventListener('keydown', e => { if (e.key === 'Enter') { e.preventDefault(); trySearch(true); } });

  function selectStation(id, move) {
    state.station = id;
    writeHash();
    highlight();
    renderProfile(id);
    if (narrow()) $('profile').scrollIntoView({ block: 'start', behavior: reduceMotion ? 'auto' : 'smooth' });
    const s = ST_BY[id];
    if (move && s) {
      map.setView([s.lat, s.lng], Math.max(map.getZoom(), 14), { animate: !reduceMotion });
    }
  }

  // ---- Line-color bullets: which rail lines pass within ~100 m of a station ----
  function segDist(px, py, ax, ay, bx, by) {
    const dx = bx - ax, dy = by - ay, len = dx * dx + dy * dy;
    let t = len ? ((px - ax) * dx + (py - ay) * dy) / len : 0;
    t = Math.max(0, Math.min(1, t));
    const x = ax + t * dx - px, y = ay + t * dy - py;
    return Math.sqrt(x * x + y * y);
  }
  const flairCache = {};
  function lineColors(s) {
    if (flairCache[s.id]) return flairCache[s.id];
    const feats = (window.TOD_RAIL && window.TOD_RAIL.features) || [];
    const kx = 111320 * Math.cos(s.lat * Math.PI / 180), ky = 110540;   // meters per degree
    const px = s.lng * kx, py = s.lat * ky;
    const out = [];
    feats.forEach(f => {
      const p = f.properties || {}, g = f.geometry;
      if (!g || !String(s.modes).includes(p.agency)) return;   // CTA stations get CTA lines, etc.
      const lines = g.type === 'LineString' ? [g.coordinates] : g.type === 'MultiLineString' ? g.coordinates : [];
      const hit = lines.some(ln => ln.some((c, i) => i > 0 &&
        segDist(px, py, ln[i - 1][0] * kx, ln[i - 1][1] * ky, c[0] * kx, c[1] * ky) < 100));
      if (hit && !out.some(o => o.color === p.color)) out.push({ color: p.color, label: p.label });
    });
    return (flairCache[s.id] = out);
  }
  const flair = s => {
    const cols = lineColors(s);
    const dots = cols.length ? cols : [{ color: '#9AA6AF', label: s.modes }];   // fallback dot
    return `<span class="flair" aria-hidden="true">${dots
      .map(c => `<i style="background:${c.color}" title="${esc(c.label)}"></i>`).join('')}</span>`;
  };
  const cap = t => t.charAt(0).toUpperCase() + t.slice(1);

  // ---- Startup panel: the five stations gaining the most homes at full build-out ----
  function renderTopList() {
    const top = ST.slice().sort((a, b) => (b.units.max || 0) - (a.units.max || 0)).slice(0, 5);
    $('profile').innerHTML = `
      <p class="p-sec">Most new homes at full build-out</p>
      <ol class="top-st">${top.map(s => `<li><button type="button" class="top-st-btn" data-id="${s.id}">
          ${flair(s)}<span class="nm">${esc(s.name)}</span>
          <span class="sub">${cap(homes(s.units.max || 0))}${s.homes_today ? `, beside about ${approx(s.homes_today)} there today` : ''}</span>
        </button></li>`).join('')}</ol>
      <p class="note">Pick any station on the map, or search above, to see what could be built there and what stands in the way.</p>`;
    $('profile').querySelectorAll('.top-st-btn')
      .forEach(b => b.addEventListener('click', () => selectStation(b.dataset.id, true)));
  }

  function renderProfile(id) {
    const s = ST_BY[id];
    const el = $('profile');
    if (!s) return;
    const k = state.scenario;
    const smax = Math.max(1, ...SC.map(c => s.units[c.key] || 0));

    const ctx = [];
    if (s.homes_today) ctx.push(`About ${approx(s.homes_today)} homes are here today.`);
    if (s.income) ctx.push(`Median household income is about $${approx(s.income)}.`);
    if (s.renter_share != null) ctx.push(`${pct(s.renter_share)} of households rent.`);

    const bars = SC.map(c => {
      const u = s.units[c.key] || 0;
      return `<div class="sbar${c.key === k ? ' active' : ''}">
        <span>${esc(c.label)}</span>
        <span class="val">${short(u)} ${confTag(s.conf[c.key])}</span>
        <div class="track"><div class="fill" style="width:${(100 * u / smax).toFixed(1)}%"></div></div>
      </div>`;
    }).join('');

    const lvTotal = LEV.reduce((a, l) => a + (s.levers[l.key] || 0), 0) || 1;
    const lvBar = LEV.map(l => `<span style="width:${(100 * (s.levers[l.key] || 0) / lvTotal).toFixed(2)}%;background:${l.color}" title="${esc(l.label)}"></span>`).join('');
    const lvKey = LEV.filter(l => (s.levers[l.key] || 0) >= 50)
      .map(l => `<li style="--c:${l.color}"><span>${esc(l.label)}</span><span>${l.key === 'base' ? '' : '+'}${approx(s.levers[l.key])}</span></li>`).join('');

    const top = (s.top || []).map(t => `<li>
        <div class="o">${esc(t.owner)}</div>
        <div>${esc(t.type)}, ${t.acres} acres: ${homes(t.units)}</div>
        <div>${esc(t.status)} ${confTag(t.conf)}</div>
      </li>`).join('');

    const wardLinks = arr(s.wards).map(w => {
      const W = WARDS[String(w)];
      return W ? `<a href="${W.print_url}">Ward ${w} summary</a>` : '';
    }).filter(Boolean).join(', ');

    el.innerHTML = `
      <h3>${flair(s)} ${esc(s.name)}</h3>
      <p class="p-meta">${esc(TYPOLOGY[s.typology] || s.typology)}, ${esc(s.modes)}${arr(s.wards).length ? ', Ward ' + arr(s.wards).join(', ') : ''}</p>
      <p class="p-context">${ctx.join(' ')}</p>
      <p class="p-sec">New homes that could fit here</p>
      <div class="sbars">${bars}</div>
      <p class="p-sec">What it takes</p>
      <div class="lbar" role="img" aria-label="Share of capacity allowed today and unlocked by each policy change">${lvBar}</div>
      <ul class="lkey">${lvKey}</ul>
      ${top ? `<p class="p-sec">Largest sites</p><ul class="top-sites">${top}</ul>` : ''}
      <div class="p-actions">
        <button type="button" id="copy-link">Copy link to this station</button>
        <button type="button" id="show-top">Back to top stations</button>
        ${wardLinks ? `<span>${wardLinks}</span>` : ''}
      </div>`;

    $('show-top').addEventListener('click', () => {
      state.station = null; writeHash(); highlight(); renderTopList();
    });
    $('copy-link').addEventListener('click', async e => {
      try { await navigator.clipboard.writeText(location.href); e.target.textContent = 'Link copied'; }
      catch (_) { window.prompt('Copy this link:', location.href); }
    });
  }

  // ---------------------------------------------------------------------------
  // Density sketches (drawn, not photos, so they stay generic and light)
  // ---------------------------------------------------------------------------
  (function density() {
    const cta = A.station_far['CTA neighborhood'];
    const B = [
      { name: 'Single-family house', homes: '1 home', far: 'FAR about 0.5', w: 1, floors: 1.4, roof: true },
      { name: 'Two-flat', homes: '2 homes', far: 'FAR about 1', w: 1, floors: 2.4 },
      { name: 'Four-flat', homes: '4 homes', far: `FAR ${A.fourplex_far}`, w: 1, floors: 3, target: 'Small-lot target' },
      { name: 'Courtyard building', homes: '30 to 40 homes', far: `FAR ${A.courtyard_far}`, w: 3.2, floors: 3.5, court: true, target: 'Mid-size lot target' },
      { name: 'Mid-rise', homes: '60 to 80 homes', far: `FAR ${cta}`, w: 2.6, floors: 6, target: 'Large-site target near the L' }
    ];
    const LW = 58, FH = 21, GAP = 40, G = 200;
    let x = 20, out = '';
    B.forEach(b => {
      const bw = b.w * LW, bh = b.floors * FH, top = G - bh;
      out += `<line x1="${x - 4}" y1="${G}" x2="${x + bw + 4}" y2="${G}" stroke="#1F2A33" stroke-width="2"/>`;
      if (b.court) {
        const wing = bw * 0.3;
        out += `<rect x="${x + wing}" y="${top + 8}" width="${bw - 2 * wing}" height="${bh - 8}" fill="#E3E8EB" stroke="#1F2A33" stroke-width="1.2"/>`;
        out += `<rect x="${x}" y="${top}" width="${wing}" height="${bh}" fill="#fff" stroke="#1F2A33" stroke-width="1.5"/>`;
        out += `<rect x="${x + bw - wing}" y="${top}" width="${wing}" height="${bh}" fill="#fff" stroke="#1F2A33" stroke-width="1.5"/>`;
      } else {
        out += `<rect x="${x}" y="${top}" width="${bw}" height="${bh}" fill="#fff" stroke="#1F2A33" stroke-width="1.5"/>`;
      }
      if (b.roof) out += `<polygon points="${x - 3},${top} ${x + bw / 2},${top - 22} ${x + bw + 3},${top}" fill="#fff" stroke="#1F2A33" stroke-width="1.5"/>`;
      const cols = Math.max(2, Math.round(b.w * 2.4)), rows = Math.floor(b.floors);
      for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
          const wx = x + (c + 0.5) * bw / cols - 4, wy = G - (r + 1) * FH + 5;
          out += `<rect x="${wx.toFixed(1)}" y="${wy}" width="8" height="10" fill="#B9C3CA"/>`;
        }
      }
      if (b.target) out += `<text x="${x}" y="${top - (b.roof ? 30 : 10)}" font-size="12" fill="#B5001F" font-weight="600">${b.target}</text>`;
      out += `<text x="${x}" y="${G + 22}" font-size="14" font-weight="600" fill="#1F2A33">${b.name}</text>`;
      out += `<text x="${x}" y="${G + 40}" font-size="13" fill="#1F2A33">${b.homes}</text>`;
      out += `<text x="${x}" y="${G + 57}" font-size="13" fill="#56636E">${b.far}</text>`;
      x += Math.max(bw, 120) + GAP;
    });
    $('density-svg').innerHTML =
      `<svg viewBox="0 0 ${x} 270" role="img" aria-label="Five building types from a single-family house to a six-story mid-rise, drawn to the same scale" font-family="Barlow, sans-serif">${out}</svg>`;
    $('density-cap').textContent =
      'Drawn to the same scale. The house, two-flat, and four-flat each sit on one standard 25-foot city lot; ' +
      'the courtyard building and mid-rise sit on several lots combined. Homes per building vary with unit size.';
    $('faq-targets').textContent =
      `Lots under ${A.small_lot_ac} acres (about ${nf.format(Math.round(A.small_lot_ac * 43560 / 100) * 100)} square feet) target a four-flat, at FAR ${A.fourplex_far}. ` +
      `Lots up to ${A.large_lot_ac} acres target a courtyard building, at FAR ${A.courtyard_far}. ` +
      `Larger sites target FAR ${cta} near L stations, ${A.station_far['Metra neighborhood']} near Metra stations, and ${A.station_far['Downtown']} downtown, ` +
      `${Math.round((A.inner_bonus - 1) * 100)}% higher within a quarter mile of the station. ` +
      `Every single-family lot near a station targets at least ${A.mm_min_units} homes. ` +
      `New homes assume ${nf.format(A.sqft_per_unit)} square feet each, including hallways and stairs.`;
  })();

  // ---------------------------------------------------------------------------
  // Policy waterfall (citywide or by ward)
  // ---------------------------------------------------------------------------
  const wardSel = $('ward-select');
  wardSel.innerHTML = '<option value="">Citywide</option>' +
    D.wards.map(w => `<option value="${w.ward}">Ward ${w.ward}</option>`).join('');
  wardSel.addEventListener('change', () => setWard(wardSel.value));

  function setWard(w) {
    wardSel.value = w;
    const W = WARDS[w];
    const link = $('ward-print');
    if (W) { link.href = W.print_url; link.hidden = false; } else link.hidden = true;
    drawWaterfall(W ? W.levers : S.city_levers);
  }

  function drawWaterfall(lev) {
    const total = LEV.reduce((a, l) => a + (lev[l.key] || 0), 0) || 1;
    let cum = 0, html = '';
    LEV.forEach(l => {
      const v = lev[l.key] || 0;
      html += `<div class="wf-row">
        <span class="lab">${esc(l.label)}</span>
        <span class="trk"><span class="seg" style="left:${(100 * cum / total).toFixed(2)}%;width:${Math.max(100 * v / total, 0.3).toFixed(2)}%;background:${l.color}"></span></span>
        <span class="num">${l.key === 'base' ? approx(v) : (v < 50 ? 'under 50' : '+' + approx(v))}</span></div>`;
      cum += v;
    });
    html += `<div class="wf-row total">
      <span class="lab">Full build-out</span>
      <span class="trk"><span class="seg" style="left:0;width:100%;background:var(--ink)"></span></span>
      <span class="num">${approx(total)}</span></div>`;
    $('waterfall').innerHTML = html;
  }


  // ---------------------------------------------------------------------------
  // Featured places
  // ---------------------------------------------------------------------------
  const featWrap = $('featured');
  D.featured.forEach(f => {
    const a = document.createElement('article');
    a.className = 'place';
    a.innerHTML = `<div><h3>${esc(f.title)}</h3><p>${esc(f.body)}</p>
      <button type="button" data-key="${f.key}">Show on the main map</button></div>
      <div class="place-map" id="place-${f.key}" role="img" aria-label="Map of ${esc(f.title)}"></div>`;
    featWrap.appendChild(a);
  });

  function featureStation(f) {
    if (arr(f.station_ids).length) return String(arr(f.station_ids)[0]);
    const site = (window.TOD_SITES ? window.TOD_SITES.features : [])
      .find(x => arr(f.site_ids).map(String).includes(String(x.properties.id)));
    const s = site && ST_BY_NAME[String(site.properties.station).toLowerCase()];
    return s ? s.id : null;
  }
  featWrap.addEventListener('click', e => {
    const key = e.target.dataset && e.target.dataset.key;
    if (!key) return;
    const f = D.featured.find(x => x.key === key);
    const sid = f && featureStation(f);
    if (sid) {
      $('explore').scrollIntoView({ behavior: reduceMotion ? 'auto' : 'smooth' });
      selectStation(sid, true);
    }
  });

  function initPlaceMaps() {
    D.featured.forEach(f => {
      try { initPlaceMap(f); }
      catch (err) { console.error('Featured map "' + f.key + '" failed:', err); }
    });
  }
  function initPlaceMap(f) {
    {
      const m = L.map('place-' + f.key, {
        zoomControl: false, dragging: false, scrollWheelZoom: false, doubleClickZoom: false,
        boxZoom: false, keyboard: false, touchZoom: false, attributionControl: true, preferCanvas: true
      });
      L.tileLayer(ESRI, { maxZoom: 16, attribution: ESRI_ATTR }).addTo(m);
      m.createPane('rail'); m.getPane('rail').style.zIndex = 380;
      railLayer('rail').addTo(m);
      const group = L.featureGroup().addTo(m);
      const ids = new Set(arr(f.site_ids).map(String));
      if (ids.size && window.TOD_SITES) {
        L.geoJSON({ type: 'FeatureCollection', features: window.TOD_SITES.features.filter(x => ids.has(String(x.properties.id))) },
          { style: { color: '#E4002B', weight: 2, fillColor: '#E4002B', fillOpacity: 0.35 } }).addTo(group);
      }
      arr(f.station_ids).forEach(id => {
        const s = ST_BY[String(id)];
        if (s) L.circleMarker([s.lat, s.lng], { radius: 9, color: '#1F2A33', weight: 1.5, fillColor: '#41B6E6', fillOpacity: 0.9 })
          .bindTooltip(esc(s.name), { permanent: true, direction: 'right', className: 'st-label' }).addTo(group);
      });
      if (group.getLayers().length) {
        const b = group.getBounds();
        if (b.isValid()) m.fitBounds(b.pad(arr(f.station_ids).length > 1 ? 0.25 : 1.2), { maxZoom: 15 });
      } else m.setView([41.85, -87.68], 11);
    }
  }

  // ---------------------------------------------------------------------------
  // About + start
  // ---------------------------------------------------------------------------
  if (S.set_aside && $('faq-setaside')) {
    const a = S.set_aside;
    $('faq-setaside').textContent =
      `No. ${nf.format(a.schools)} active Chicago Public Schools sites and ${nf.format(a.campuses)} public university and college ` +
      `campus parcels, roughly ${approx(a.acres)} acres in all, are left out of every number on this page. ` +
      `They appear in grey on the map when the largest-sites layer is on. Closed or vacant school land is still counted.`;
  }

  $('about-text').innerHTML =
    `Prepared by ${esc(S.author)}, ${esc(S.generated)}. Every figure is a rounded estimate of capacity, not a forecast. ` +
    `The code and full methods are <a href="${esc(S.repo_url)}">on GitHub</a>.`;

  (function sectionNav() {
    const items = [{ id: 'top', label: 'Top' }].concat(
      [...document.querySelectorAll('main > section[id], main > footer[id]')]
        .map(sec => ({ id: sec.id, label: (sec.querySelector('h2') || {}).textContent || sec.id }))
    );
    const nav = document.createElement('nav');
    nav.className = 'site-nav';
    nav.setAttribute('aria-label', 'Page sections');
    nav.innerHTML =
      `<button type="button" class="nav-toggle" aria-expanded="false" aria-controls="nav-list" aria-label="Page sections">` +
      `<span class="nav-bars" aria-hidden="true"><i></i><i></i><i></i></span></button>` +
      `<ul id="nav-list" class="nav-list">${items.map(i =>
        `<li><a href="#${i.id}" data-id="${i.id}">${esc(i.label.trim())}</a></li>`).join('')}</ul>`;
    document.body.prepend(nav);

    const btn = nav.querySelector('.nav-toggle');
    const setOpen = open => { nav.classList.toggle('open', open); btn.setAttribute('aria-expanded', String(open)); };
    btn.addEventListener('click', () => setOpen(!nav.classList.contains('open')));
    document.addEventListener('keydown', e => {
      if (e.key === 'Escape' && nav.classList.contains('open')) { setOpen(false); btn.focus(); }
    });
    document.addEventListener('click', e => { if (!nav.contains(e.target)) setOpen(false); });

    // Scroll in place rather than changing the URL, which holds the station and scenario
    nav.querySelectorAll('a').forEach(a => a.addEventListener('click', e => {
      e.preventDefault();
      setOpen(false);
      const target = $(a.dataset.id);
      if (a.dataset.id === 'top') window.scrollTo({ top: 0, behavior: reduceMotion ? 'auto' : 'smooth' });
      else if (target) target.scrollIntoView({ block: 'start', behavior: reduceMotion ? 'auto' : 'smooth' });
    }));

    // Highlight the section currently on screen
    const links = Object.fromEntries([...nav.querySelectorAll('a')].map(a => [a.dataset.id, a]));
    const obs = new IntersectionObserver(entries => {
      entries.forEach(en => {
        if (!en.isIntersecting) return;
        Object.values(links).forEach(a => a.removeAttribute('aria-current'));
        if (links[en.target.id]) links[en.target.id].setAttribute('aria-current', 'true');
      });
    }, { rootMargin: '-35% 0px -60% 0px' });
    items.forEach(i => { const el = $(i.id); if (el) obs.observe(el); });
  })();

  setScenario(state.scenario);
  setWard('');
  if (!state.station) renderTopList();
  if (state.station) {
    renderProfile(state.station);
    const s = ST_BY[state.station];
    map.setView([s.lat, s.lng], 14);
    highlight();
  }
  need('data/sites.js').then(initPlaceMaps).catch(showLoadError);
})();
