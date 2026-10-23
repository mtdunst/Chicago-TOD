(function () {
  'use strict';

  var D = window.TYPES_DATA;
  if (!D) {
    document.getElementById('profile').innerHTML =
      '<p>The station data didn\u2019t load. Rerun tod_11_station_types.R, then reload this page.</p>';
    return;
  }

  // ---- Type definitions -----------------------------------------------------

  var TYPES = {
    hi: { name: 'Job center',
          def: 'Offices, labs, hospitals, and campuses, with housing alongside. For multi-route transfer stations, where several rail lines put a larger-than-average labor pool within reach.',
          pushes: 'Several rail lines meeting within a third of a mile (the more, the larger the labor pool), jobs already nearby, land values, and the downtown office market.' },
    mr: { name: 'Market-rate housing',
          def: 'New apartments and condos that private developers can build without subsidy, with the affordable units the ARO already requires.',
          pushes: 'High land values, frequent service, room to build, and higher local incomes.' },
    ao: { name: 'Affordable housing on public land',
          def: 'Income-restricted homes on land owned by the City, a transit agency, or another public body. Free or discounted land is what makes affordable housing work where land is expensive.',
          pushes: 'Public land in the walkshed, high land values, frequent service, and higher incomes nearby, which make these homes a door to opportunity.' },
    ai: { name: 'Affordable housing, investment-led',
          def: 'Subsidized and mixed-income buildings where the private market isn\u2019t building yet, paired with the public investment that helps it start.',
          pushes: 'Lower land costs, room to build, frequent service, and lower local incomes.' },
    pf: { name: 'Protect existing homes first',
          def: 'Keep current residents in place before adding much new: preserve older affordable buildings, limit deconversions, and pair any new building with protections.',
          pushes: 'Land values running ahead of incomes, a high share of renters, and little vacant land.' },
    ir: { name: 'Keep industry',
          def: 'Protect industrial land and the jobs on it rather than converting it to housing. Transit still matters here, for the people who work at these sites.',
          pushes: 'Industrial jobs nearby (manufacturing, wholesale, and logistics) and the share of local jobs they make up, with industrial zoning as a smaller factor.' }
  };
  var ORDER = ['hi', 'mr', 'ao', 'ai', 'pf', 'ir'];
  var HOUSING = { mr: 1, ao: 1, ai: 1, hi: 1 };

  function tcolor(t) { return getComputedStyle(document.documentElement).getPropertyValue('--t-' + t).trim(); }

  // ---- State ----------------------------------------------------------------

  var stations = D.stations;
  var byId = {};
  stations.forEach(function (s) { byId[s.id] = s; });
  var state = { scn: 'today', sel: null, show: {}, sort: { key: 'name', dir: 1 }, layer: null, layerAuto: true };
  ORDER.forEach(function (t) { state.show[t] = true; });

  // R writes one-item reason lists as plain strings; treat both alike
  var reasons = function (blk, t) { var x = blk.reasons && blk.reasons[t]; return Array.isArray(x) ? x : (x ? [x] : []); };

  var other = function (scn) { return scn === 'today' ? 'future' : 'today'; };
  var changed = function (s) { return s.today.best !== s.future.best; };

  // ---- Formatting -----------------------------------------------------------

  function esc(x) {
    return String(x == null ? '' : x).replace(/[&<>"']/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
    });
  }
  function homes(n) {
    if (n == null || isNaN(n)) return 'n/a';
    if (n < 50) return 'fewer than 50';
    if (n < 1000) return (Math.round(n / 50) * 50).toLocaleString();
    var p = Math.pow(10, Math.floor(Math.log10(n)) - 1);
    return (Math.round(n / p) * p).toLocaleString();
  }
  function money(x) {
    if (x == null || isNaN(x)) return 'n/a';
    return x >= 1e6 ? '$' + (x / 1e6).toFixed(1) + 'M' : '$' + Math.round(x / 1e3) + 'K';
  }
  function share(x) { return x == null || isNaN(x) ? 'n/a' : Math.round(x * 100) + '%'; }
  function every(min, peak) {
    if (min == null) return peak != null ? 'peak only' : 'no service';
    if (min < 1.5) return 'about every minute';
    return 'every ' + Math.round(min) + ' min';
  }
  function feet(ft) { return ft >= 1000 ? (Math.round(ft / 100) * 100).toLocaleString() + ' ft' : Math.round(ft / 10) * 10 + ' ft'; }

  // ---- Map ------------------------------------------------------------------

  var dark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
  var map = L.map('map', { preferCanvas: true, zoomSnap: 0.5 }).setView([41.84, -87.68], 11);
  L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/' +
              (dark ? 'World_Dark_Gray_Base' : 'World_Light_Gray_Base') +
              '/MapServer/tile/{z}/{y}/{x}', {
    maxZoom: 16, attribution: 'Tiles &copy; Esri, HERE, Garmin, &copy; OpenStreetMap contributors'
  }).addTo(map);

  if (window.TYPES_RAIL) {
    L.geoJSON(window.TYPES_RAIL, {
      interactive: false,
      style: function (f) { return { color: (f.properties && f.properties.color) || '#56636E', weight: 2.5, opacity: 0.55 }; }
    }).addTo(map);
  }

  var maxCap = Math.max.apply(null, stations.map(function (s) { return (s.facts && s.facts.cap_units) || 0; })) || 1;
  var radius = function (s) { return 4 + 10 * Math.sqrt(((s.facts && s.facts.cap_units) || 0) / maxCap); };

  var markers = {};
  stations.forEach(function (s) {
    if (s.lat == null || s.lng == null) return;
    var m = L.circleMarker([s.lat, s.lng], { radius: radius(s), weight: 1.5, fillOpacity: 0.88 });
    m.bindTooltip('', { direction: 'top', offset: [0, -4] });
    m.on('click', function () { select(s.id, false); });
    markers[s.id] = m;
  });

  var futureLayer = L.layerGroup();
  (D.future_stations || []).forEach(function (f) {
    var icon = L.divIcon({ className: '', html: '<div class="future-pin"></div>', iconSize: [14, 14] });
    L.marker([f.lat, f.lng], { icon: icon, keyboard: false })
      .bindTooltip('<strong>' + esc(f.name) + '</strong><br>' + esc(f.project) + '. Not scored yet.')
      .addTo(futureLayer);
  });

  // ---- Selected station: half-mile buffer and parcel layers ----------------

  function cssVar(v) { return getComputedStyle(document.documentElement).getPropertyValue(v).trim(); }

  var LAYER_FOR = {
    ao: { sites: ['pub', 'tr'], none: 'No public parcels in this station\u2019s half mile.',
          caption: 'Public parcels; transit-agency land is darker' },
    mr: { sites: ['ub'], none: 'No underbuilt parcels in this station\u2019s half mile.', caption: 'Underbuilt parcels' },
    ai: { sites: ['ub'], none: 'No underbuilt parcels in this station\u2019s half mile.', caption: 'Underbuilt parcels' },
    pf: { sites: ['ub'], none: 'No underbuilt parcels in this station\u2019s half mile.', caption: 'Underbuilt parcels' },
    ir: { sites: ['ind'], blocks: 'ii', metric: 'ij', none: 'No industrial parcels or industrial-job blocks in this station\u2019s half mile.',
          caption: 'Industrial-zoned parcels, and Census blocks shaded by industrial jobs' },
    hi: { sites: ['ub'], blocks: 'hi', metric: 'ja', none: 'No high-job blocks or underbuilt parcels in this station\u2019s half mile.',
          caption: 'High-job Census blocks (darker means more jobs per acre) and underbuilt parcels' }
  };

  var bufferLayer = null;
  var parcelLayer = L.layerGroup().addTo(map);
  var loading = {};

  // Each station's parcels live in their own file, loaded on first selection
  function loadSites(id, cb) {
    var store = window.TYPES_SITES || {};
    if (store[id]) { cb(store[id]); return; }
    if (loading[id]) { loading[id].push(cb); return; }
    loading[id] = [cb];
    var sc = document.createElement('script');
    sc.src = 'data/st/' + encodeURIComponent(id) + '.js?v=' + encodeURIComponent(D.version || '');
    var done = function (d) { (loading[id] || []).forEach(function (f) { f(d); }); delete loading[id]; };
    sc.onload = function () { done((window.TYPES_SITES || {})[id] || null); };
    sc.onerror = function () { done(null); };
    document.head.appendChild(sc);
  }

  var caption = L.control({ position: 'bottomleft' });
  caption.onAdd = function () {
    this._div = L.DomUtil.create('div', 'map-caption');
    this._div.hidden = true;
    L.DomEvent.disableClickPropagation(this._div);
    return this._div;
  };
  caption.addTo(map);
  function setCaption(html) { caption._div.innerHTML = html || ''; caption._div.hidden = !html; }

  function tipSite(p) {
    var bits = [];
    if (p.o && (p.k === 'pub' || p.k === 'tr')) bits.push('<strong>' + esc(p.o) + '</strong>');
    if (p.u > 0) bits.push('Room for about ' + homes(p.u) + ' new homes');
    if (p.ind === 1) bits.push('Zoned for industry');
    return bits.join('<br>') || 'Parcel';
  }

  function drawParcels() {
    parcelLayer.clearLayers();
    var id = state.sel, t = state.layer;
    if (!id || !t) { setCaption(''); return; }
    var spec = LAYER_FOR[t];
    setCaption('Loading parcels\u2026');
    loadSites(id, function (d) {
      if (state.sel !== id || state.layer !== t) return;
      parcelLayer.clearLayers();
      if (!d) { setCaption('Parcel layers aren\u2019t available for this station.'); return; }
      var c = tcolor(t), ink = cssVar('--ink'), n = 0;

      if (spec.blocks && d.blocks && d.blocks.features) {
        var bf = d.blocks.features.filter(function (f) { return f.properties[spec.blocks] === 1; });
        var top = Math.max.apply(null, bf.map(function (f) { return f.properties[spec.metric] || 0; }).concat([1]));
        n += bf.length;
        L.geoJSON({ type: 'FeatureCollection', features: bf }, {
          style: function (f) {
            var r = Math.sqrt((f.properties[spec.metric] || 0) / top);
            return { stroke: false, fillColor: c, fillOpacity: 0.12 + 0.48 * r };
          },
          onEachFeature: function (f, l) {
            var p = f.properties;
            l.bindTooltip(spec.blocks === 'ii'
              ? homes(p.ij) + ' industrial jobs in this block'
              : homes(p.j) + ' jobs in this block (' + Math.round(p.ja) + ' per acre)', { sticky: true });
          }
        }).addTo(parcelLayer);
      }

      if (d.sites && d.sites.features) {
        var sf = d.sites.features.filter(function (f) {
          var p = f.properties;
          return spec.sites.some(function (k) {
            if (k === 'ind') return p.ind === 1;
            return p.k === k && (k !== 'ub' || p.u > 0);
          });
        });
        n += sf.length;
        L.geoJSON({ type: 'FeatureCollection', features: sf }, {
          style: function (f) {
            return { color: ink, weight: 0.7, opacity: 0.8, fillColor: c,
                     fillOpacity: f.properties.k === 'tr' ? 0.85 : 0.5 };
          },
          onEachFeature: function (f, l) { l.bindTooltip(tipSite(f.properties), { sticky: true }); }
        }).addTo(parcelLayer);
      }

      setCaption(n
        ? '<span class="sw" style="--c:' + c + '"></span>' + esc(spec.caption) + '. Shown for ' + esc(TYPES[t].name.toLowerCase()) + '.'
        : esc(spec.none));
    });
  }

  function drawBuffer(zoom) {
    if (bufferLayer) { map.removeLayer(bufferLayer); bufferLayer = null; }
    var s = byId[state.sel];
    if (!s || s.lat == null) return;
    bufferLayer = L.circle([s.lat, s.lng], {
      radius: (D.walk_ft || 2640) * 0.3048, color: cssVar('--ink'), weight: 1.5,
      dashArray: '6 5', fill: false, interactive: false
    }).addTo(map);
    if (zoom) map.flyToBounds(bufferLayer.getBounds(), { padding: [24, 24], duration: 0.6 });
  }

  function styleMarkers() {
    var ink = getComputedStyle(document.documentElement).getPropertyValue('--ink').trim();
    var card = getComputedStyle(document.documentElement).getPropertyValue('--card').trim();
    stations.forEach(function (s) {
      var m = markers[s.id];
      if (!m) return;
      var t = s[state.scn].best;
      var visible = state.show[t];
      var isSel = s.id === state.sel;
      var ring = state.scn === 'future' && changed(s);
      m.setStyle({
        fillColor: tcolor(t),
        color: isSel || ring ? ink : card,
        weight: isSel ? 3.5 : ring ? 2.5 : 1.2,
        opacity: visible ? 1 : 0,
        fillOpacity: visible ? 0.88 : 0
      });
      m.setTooltipContent('<strong>' + esc(s.name) + '</strong><br>' + esc(TYPES[t].name));
      if (visible && !map.hasLayer(m)) m.addTo(map);
      if (!visible && map.hasLayer(m) && !isSel) map.removeLayer(m);
      if (isSel) m.bringToFront();
    });
    if (state.scn === 'future') futureLayer.addTo(map); else map.removeLayer(futureLayer);
  }

  // ---- Legend chips (also filters) -----------------------------------------

  var legend = document.getElementById('legend');
  function counts(scn) {
    var c = {};
    ORDER.forEach(function (t) { c[t] = 0; });
    stations.forEach(function (s) { c[s[scn].best]++; });
    return c;
  }
  function drawLegend() {
    var c = counts(state.scn);
    legend.innerHTML = ORDER.map(function (t) {
      return '<button type="button" class="chip" data-t="' + t + '" aria-pressed="' + state.show[t] +
        '" style="--c:' + tcolor(t) + '"><span class="dot"></span>' + esc(TYPES[t].name) +
        ' <span class="n">' + c[t] + '</span></button>';
    }).join('');
  }
  legend.addEventListener('click', function (e) {
    var b = e.target.closest('.chip');
    if (!b) return;
    var t = b.getAttribute('data-t');
    var allOn = ORDER.every(function (k) { return state.show[k]; });
    // First click isolates one type; clicking the only visible type shows all again
    if (allOn) ORDER.forEach(function (k) { state.show[k] = k === t; });
    else if (ORDER.filter(function (k) { return state.show[k]; }).length === 1 && state.show[t]) ORDER.forEach(function (k) { state.show[k] = true; });
    else state.show[t] = !state.show[t];
    drawLegend(); styleMarkers();
  });

  // ---- Scenario switch -------------------------------------------------------

  var switchBtns = document.querySelectorAll('.switch button');
  function setScn(scn, push) {
    state.scn = scn;
    switchBtns.forEach(function (b) { b.setAttribute('aria-checked', String(b.getAttribute('data-scn') === scn)); });
    var n = stations.filter(changed).length;
    document.getElementById('scn-note').innerHTML = scn === 'today'
      ? 'Types based on <strong>today\u2019s schedules</strong>. ' + n + ' stations would change type in Transit Future.'
      : '<strong>Transit Future</strong>: \u2019L\u2019 every ' + D.assumptions.l_headway + ' minutes, Metra regional rail, and the bus network built out. ' + n + ' stations change type.';
    var s = byId[state.sel];
    if (s && state.layerAuto) state.layer = s[scn].best;
    drawLegend(); styleMarkers(); renderProfile(); renderTable(); renderTypeDefs();
    if (s) drawParcels();
    if (bufferLayer) bufferLayer.setStyle({ color: cssVar('--ink') });
    if (push !== false) writeHash();
  }
  switchBtns.forEach(function (b) {
    b.addEventListener('click', function () { setScn(b.getAttribute('data-scn')); });
    b.addEventListener('keydown', function (e) {
      if (e.key === 'ArrowLeft' || e.key === 'ArrowRight') {
        e.preventDefault();
        var next = state.scn === 'today' ? 'future' : 'today';
        setScn(next);
        document.querySelector('.switch button[data-scn="' + next + '"]').focus();
      }
    });
  });

  // ---- Profile --------------------------------------------------------------

  var profile = document.getElementById('profile');

  function renderEmpty() {
    var c = counts(state.scn);
    var top = ORDER.slice().sort(function (a, b) { return c[b] - c[a]; });
    profile.innerHTML =
      '<div class="empty-state"><h2>Pick a station</h2>' +
      '<p>Select a circle on the map or search by name to see its best-fit type, the reasons, and why the other five fit less well.</p>' +
      '<p>' + stations.length + ' stations are scored. The most common types ' +
      (state.scn === 'future' ? 'in Transit Future' : 'today') + ':</p><ul>' +
      top.slice(0, 3).map(function (t) { return '<li>' + esc(TYPES[t].name) + ': ' + c[t] + '</li>'; }).join('') +
      '</ul></div>';
  }

  function renderProfile() {
    var s = byId[state.sel];
    if (!s) { renderEmpty(); return; }
    var cur = s[state.scn], alt = s[other(state.scn)];
    var best = cur.best;
    var ranked = ORDER.slice().sort(function (a, b) { return (cur.scores[b] || 0) - (cur.scores[a] || 0); });
    var second = ranked[1];

    var h = '';
    h += '<h2>' + esc(s.name) + '</h2>';
    h += '<p class="modes">' + esc(s.modes || '') + (s.downtown ? ', downtown' : '') + '</p>';
    if (s.note) h += '<p class="note">' + esc(s.note) + '</p>';

    // Verdict
    h += '<div class="verdict" style="--c:' + tcolor(best) + '">';
    h += '<p class="tag">Best fit ' + (state.scn === 'future' ? 'in Transit Future' : 'with today\u2019s service') + '</p>';
    h += '<p class="tname">' + esc(TYPES[best].name) + '</p>';
    h += '<p class="def">' + esc(TYPES[best].def) + '</p>';
    h += '<ul>' + reasons(cur, best).filter(Boolean).map(function (r) { return '<li>' + esc(r) + '</li>'; }).join('') + '</ul>';
    if (HOUSING[best] && cur.intensity) h += '<p class="aside">' + esc(cur.tier_label) + ' service supports ' + esc(cur.intensity) + '.</p>';
    if (cur.lead != null && cur.lead < 5) h += '<p class="aside">Close call: ' + esc(TYPES[second].name) + ' scores within ' + Math.max(1, Math.round(cur.lead)) + (Math.round(cur.lead) <= 1 ? ' point' : ' points') + '.</p>';
    if (alt.best !== best) {
      h += '<p class="shift">' + (state.scn === 'future'
        ? 'With today\u2019s service this station is <strong>' + esc(TYPES[alt.best].name) + '</strong>. Better service tips it here.'
        : 'In Transit Future this station becomes <strong>' + esc(TYPES[alt.best].name) + '</strong>.') + '</p>';
    } else if (state.scn === 'future' && alt.tier !== cur.tier) {
      h += '<p class="shift">Same type as today, but better service moves it from ' + esc(alt.tier_label.toLowerCase()) + ' to ' + esc(cur.tier_label.toLowerCase()) + ' service, which supports ' + esc(cur.intensity) + '.</p>';
    }
    h += '</div>';

    // Ladder
    h += '<h3>How all six types score</h3><ol class="ladder">';
    ranked.forEach(function (t) {
      var sc = cur.scores[t] || 0, gs = alt.scores[t] || 0;
      var why = t === best ? '' : reasons(cur, t)[0];
      h += '<li class="' + (t === best ? 'best' : '') + '" style="--c:' + tcolor(t) + '">' +
        '<button type="button" class="rung" data-t="' + t + '" aria-pressed="' + (t === state.layer) + '" aria-label="' +
        esc(TYPES[t].name) + ': ' + Math.round(sc) + ' of 100, ' + Math.round(gs) + ' ' + (state.scn === 'future' ? 'today' : 'in Transit Future') + '. Show its parcels on the map.">' +
        '<span class="name">' + esc(TYPES[t].name) + '</span><span class="score">' + Math.round(sc) + '</span>' +
        '<span class="bar"><span class="fill" style="width:' + sc + '%"></span><span class="ghost" style="left:calc(' + gs + '% - 1px)"></span></span></button>' +
        (why ? '<p class="why">' + esc(why) + '</p>' : '') + '</li>';
    });
    h += '</ol><p class="ladder-key">Select a type to show its parcels on the map. <i></i>marks the score ' + (state.scn === 'future' ? 'with today\u2019s service' : 'in Transit Future') + '.</p>';

    // Service
    h += '<h3>Service</h3>';
    h += '<p class="tier-line"><strong>' + esc(cur.tier_label) + '</strong> (tier ' + cur.tier + ' of 4)' +
      (alt.tier !== cur.tier ? ', ' + esc(alt.tier_label.toLowerCase()) + (state.scn === 'future' ? ' today' : ' in Transit Future') : '') +
      '. ' + esc(cur.svc_text.charAt(0).toUpperCase() + cur.svc_text.slice(1)) + '.</p>';
    var lines = (s.today.lines || []).map(function (l, i) { return { l: l, f: (s.future.lines || [])[i] || {} }; });
    if (lines.length) {
      h += '<table class="svc"><thead><tr><th>Line</th><th class="num">Midday today</th><th class="num">Transit Future</th></tr></thead><tbody>';
      lines.forEach(function (x) {
        var up = x.f.mid != null && (x.l.mid == null || x.f.mid < x.l.mid - 0.4);
        h += '<tr><td><span class="line" style="--c:' + esc(x.l.color) + '"><i></i>' + esc(x.l.name) + '</span>' +
          (x.l.at ? '<span class="at">at ' + esc(x.l.at) + ', ' + feet(x.l.dist) + '</span>' : '') + '</td>' +
          '<td class="num">' + every(x.l.mid, x.l.peak) + '</td><td class="num' + (up ? ' up' : '') + '">' + every(x.f.mid, x.f.peak) + '</td></tr>';
      });
      h += '<tr><td>Frequent bus routes nearby</td><td class="num">' + s.today.freq_bus + '</td><td class="num' +
        (s.future.freq_bus > s.today.freq_bus ? ' up' : '') + '">' + s.future.freq_bus + '</td></tr>';
      h += '</tbody></table>';
    }

    // Context
    var f = s.facts || {};
    h += '<h3>The station area</h3><dl class="facts">' +
      '<dt>Homes there today</dt><dd>' + homes(f.homes_today) + '</dd>' +
      '<dt>Median household income</dt><dd>' + money(f.income) + '</dd>' +
      '<dt>Households renting</dt><dd>' + share(f.renter_share) + '</dd>' +
      '<dt>Land value per acre (typical private lot)</dt><dd>' + money(f.mv_acre) + '</dd>' +
      '<dt>Room for new homes on underused land</dt><dd>' + homes(f.cap_units) + '</dd>' +
      '<dt>of which on public land</dt><dd>' + homes(f.pub_units) + '</dd>' +
      '<dt>Zoned for industry</dt><dd>' + share(f.ind_share) + '</dd>' +
      (f.jobs != null ? '<dt>Jobs within a half mile</dt><dd>' + homes(f.jobs) + '</dd>' : '') +
      (f.ind_jobs != null ? '<dt>of which industrial</dt><dd>' + homes(f.ind_jobs) + '</dd>' : '') +
      (f.xway_share > 0.005 ? '<dt>Within ' + (D.xway_ft || 500) + ' ft of an expressway</dt><dd>' + share(f.xway_share) + '</dd>' : '') +
      (s.aro ? '<dt>ARO area</dt><dd>' + esc(s.aro) + '</dd>' : '') +
      '</dl>';
    h += '<button type="button" class="copy-link" id="copy-link">Copy link to this station</button>';

    profile.innerHTML = h;
    profile.querySelectorAll('.rung').forEach(function (b) {
      b.addEventListener('click', function () {
        state.layer = b.getAttribute('data-t');
        state.layerAuto = false;
        profile.querySelectorAll('.rung').forEach(function (x) { x.setAttribute('aria-pressed', String(x === b)); });
        drawParcels();
      });
    });
    var cl = document.getElementById('copy-link');
    cl.addEventListener('click', function () {
      writeHash();
      var done = function () { cl.textContent = 'Link copied'; setTimeout(function () { cl.textContent = 'Copy link to this station'; }, 1800); };
      if (navigator.clipboard) navigator.clipboard.writeText(location.href).then(done, done); else done();
    });
  }

  function select(id, fly) {
    if (!byId[id]) return;
    state.sel = id;
    var s = byId[id];
    // Make sure a filtered-out station still shows when selected
    if (!state.show[s[state.scn].best]) { ORDER.forEach(function (k) { state.show[k] = true; }); drawLegend(); }
    state.layer = s[state.scn].best;
    state.layerAuto = true;
    styleMarkers(); renderProfile(); writeHash();
    drawBuffer(true); drawParcels();
    if (window.matchMedia('(max-width: 900px)').matches) profile.focus({ preventScroll: true });
  }

  // ---- Search ----------------------------------------------------------------

  var search = document.getElementById('search');
  document.getElementById('station-list').innerHTML = stations.map(function (s) {
    return '<option value="' + esc(s.name) + '"></option>';
  }).join('');
  var byName = {};
  stations.forEach(function (s) { byName[s.name.toLowerCase()] = s.id; });
  function trySearch() {
    var id = byName[search.value.trim().toLowerCase()];
    if (id) { select(id, true); document.getElementById('map').scrollIntoView({ behavior: 'smooth', block: 'start' }); }
  }
  search.addEventListener('change', trySearch);
  search.addEventListener('keydown', function (e) { if (e.key === 'Enter') trySearch(); });

  // ---- Type definitions --------------------------------------------------------

  function renderTypeDefs() {
    var ct = counts('today'), cf = counts('future');
    document.getElementById('type-defs').innerHTML = ORDER.map(function (t) {
      return '<div class="type-def" style="--c:' + tcolor(t) + '"><h3>' + esc(TYPES[t].name) + '</h3>' +
        '<p>' + esc(TYPES[t].def) + '</p>' +
        '<p class="pushes">Pushed up by: ' + esc(TYPES[t].pushes) + '</p>' +
        '<p class="count">' + ct[t] + (ct[t] === 1 ? ' station' : ' stations') + ' today, ' + cf[t] + ' in Transit Future</p></div>';
    }).join('');
  }

  // ---- Assumptions -----------------------------------------------------------

  var LINE_NAMES = { 'UP-N': 'Union Pacific North', 'UP-NW': 'Union Pacific Northwest', 'UP-W': 'Union Pacific West',
    'MD-N': 'Milwaukee District North', 'MD-W': 'Milwaukee District West', 'NCS': 'North Central Service',
    'BNSF': 'BNSF', 'SWS': 'SouthWest Service', 'RI': 'Rock Island', 'ME': 'Metra Electric' };
  var BRANCH = { main: '', beverly: ' (Beverly Branch)', south_chicago: ' (South Chicago Branch)', blue_island: ' (Blue Island Branch)' };

  function renderAssumptions() {
    var a = D.assumptions;
    var metra = (a.metra || []).map(function (r) {
      return '<tr><td>' + esc((LINE_NAMES[r.route_id] || r.route_id) + BRANCH[r.branch]) + '</td><td class="num">' +
        r.peak + ' min</td><td class="num">' + r.offpeak + ' min</td></tr>';
    }).join('');
    var fut = (D.future_stations || []).map(function (f) { return esc(f.name); }).join(', ');
    document.getElementById('assumptions').innerHTML = '<div class="assume">' +
      '<div><h3>\u2019L\u2019 trains every ' + a.l_headway + ' minutes</h3>' +
      '<p>Every line runs at least every ' + a.l_headway + ' minutes all day at the stations it serves today. CTA has said it plans to extend its Frequent Network from buses to rail; ' + a.l_headway + ' minutes stands in for that commitment.</p>' +
      '<p class="src">Source: <a href="https://www.transitchicago.com/frequent/">CTA Frequent Network</a></p></div>' +
      '<div><h3>Metra becomes regional rail</h3>' +
      '<p>Chicago stations get the Regional Rail 2.0 targets from Metra\u2019s draft Systemwide Network Plan, released October 2026. Inner-zone trains serve every stop.</p>' +
      '<table class="mini"><thead><tr><th>Line</th><th class="num">Peak</th><th class="num">Off-peak</th></tr></thead><tbody>' + metra + '</tbody></table>' +
      '<p>The Heritage Corridor and South Shore Line aren\u2019t in the plan and keep today\u2019s schedules. Crosstown service through Union Station is not modeled.</p>' +
      '<p class="src">Source: <a href="https://metra.com/SNP">Metra Systemwide Network Plan</a></p></div>' +
      '<div><h3>The bus network built out</h3>' +
      '<p>CTA\u2019s Bus Vision Project is still gathering input, so there\u2019s no adopted network yet. Transit Future stands in for it: every route on the Better Streets for Buses corridors runs at least every ' + a.bus_frequent_headway +
      ' minutes, and every other route at least every ' + a.bus_other_headway + ' minutes, the point where a grid network stays useful for transfers.</p>' +
      '<p class="src">Sources: <a href="https://ctabusvisionproject.com/">CTA Bus Vision Project</a>, <a href="https://www.transitchicago.com/betterstreetsforbuses/">Better Streets for Buses</a></p></div>' +
      '<div><h3>New stations, shown but not scored</h3>' +
      '<p>' + fut + ' appear on the map in Transit Future as hollow diamonds. Their walksheds aren\u2019t in the parcel analysis yet, so they don\u2019t have a type. Locations are approximate.</p>' +
      '<p class="src">Sources: <a href="https://www.transitchicago.com/rle/">Red Line Extension</a>, Metra Systemwide Network Plan</p></div>' +
      '</div>';
  }

  // ---- Table -------------------------------------------------------------------

  var fType = document.getElementById('filter-type');
  fType.innerHTML += ORDER.map(function (t) { return '<option value="' + t + '">' + esc(TYPES[t].name) + '</option>'; }).join('');
  var fChange = document.getElementById('filter-change');
  fType.addEventListener('change', renderTable);
  fChange.addEventListener('change', renderTable);

  document.querySelectorAll('#table th button').forEach(function (b) {
    b.addEventListener('click', function () {
      var k = b.getAttribute('data-sort');
      state.sort = { key: k, dir: state.sort.key === k ? -state.sort.dir : (k === 'cap' ? -1 : 1) };
      renderTable();
    });
  });

  function cell(t, isChanged) {
    return '<span class="tdot' + (isChanged ? ' changed' : '') + '" style="--c:' + tcolor(t) + '"><i></i>' + esc(TYPES[t].name) + '</span>';
  }

  function renderTable() {
    var t = fType.value, onlyChange = fChange.checked;
    var rows = stations.filter(function (s) {
      if (onlyChange && !changed(s)) return false;
      if (t && s[state.scn].best !== t) return false;
      return true;
    });
    var k = state.sort.key, dir = state.sort.dir;
    rows.sort(function (a, b) {
      var va, vb;
      if (k === 'cap') { va = (a.facts && a.facts.cap_units) || 0; vb = (b.facts && b.facts.cap_units) || 0; return (va - vb) * dir; }
      if (k === 'today' || k === 'future') { va = TYPES[a[k].best].name; vb = TYPES[b[k].best].name; }
      else { va = a.name; vb = b.name; }
      return va.localeCompare(vb) * dir;
    });
    document.querySelectorAll('#table th').forEach(function (th) {
      var key = th.querySelector('button').getAttribute('data-sort');
      if (key === k) th.setAttribute('aria-sort', dir > 0 ? 'ascending' : 'descending'); else th.removeAttribute('aria-sort');
    });
    document.querySelector('#table tbody').innerHTML = rows.length ? rows.map(function (s) {
      var c = changed(s);
      return '<tr data-id="' + esc(s.id) + '" tabindex="0"><td>' + esc(s.name) + '</td><td>' + cell(s.today.best, false) +
        '</td><td>' + cell(s.future.best, c) + '</td><td class="num">' + homes(s.facts && s.facts.cap_units) + '</td></tr>';
    }).join('') : '<tr><td colspan="4">No stations match. Clear the type filter or uncheck \u201conly stations that change.\u201d</td></tr>';
  }
  var tbody = document.querySelector('#table tbody');
  function rowPick(e) {
    var tr = e.target.closest('tr[data-id]');
    if (!tr) return;
    if (e.type === 'keydown' && e.key !== 'Enter') return;
    select(tr.getAttribute('data-id'), true);
    document.getElementById('map').scrollIntoView({ behavior: 'smooth', block: 'start' });
  }
  tbody.addEventListener('click', rowPick);
  tbody.addEventListener('keydown', rowPick);

  // ---- Method --------------------------------------------------------------------

  function renderMethod() {
    var a = D.assumptions;
    document.getElementById('method').innerHTML =
      '<p>This page builds on the station-area analysis of underused land near every CTA and Metra station. It uses the same half-mile walksheds, the same estimates of room for new homes, and the same public-ownership data.</p>' +
      '<h3>Service tiers</h3>' +
      '<p>Rail lines count if they stop at the station or at another station within a third of a mile, measured station to station, so 35th/Lou Jones gets credit for the Red Line at Sox-35th. Buses count if they stop within a quarter mile. Service is measured midday on a typical weekday, and frequent means every 10 minutes or better.</p>' +
      '<ul><li><strong>Car-free:</strong> a frequent \u2019L\u2019 line plus a frequent bus, two frequent \u2019L\u2019 lines, or a frequent \u2019L\u2019 line plus Metra every 15 minutes. Supports mid- and high-rise buildings.</li>' +
      '<li><strong>Car-light:</strong> a frequent \u2019L\u2019 line, an \u2019L\u2019 line every 11 to 15 minutes plus a frequent bus, or all-day rail plus two frequent buses. Supports mid-rises of about six to eight stories.</li>' +
      '<li><strong>Transit-supported:</strong> any \u2019L\u2019 service, Metra every 30 minutes plus a frequent bus, or two frequent buses. Supports courtyard buildings of three to four stories.</li>' +
      '<li><strong>Commuter rail:</strong> everything else, mostly Metra stations with hourly or peak-only midday service. Supports two-flats and four-flats.</li></ul>' +
      '<p>For the housing types, service counts only through the tier. Beyond car-free service, more trains don\u2019t make a station better for housing.</p>' +
      '<h3>What goes into each score</h3>' +
      '<p>Each station is compared with every other station on land value per acre, household income, share of renters, room for new homes, room on public land, jobs and industrial jobs within a half mile (Census LODES), industrial zoning, and service. Each type weights these differently; the definitions above list what pushes a station toward each one.</p>' +
      '<p>Job centers depend on how many rail lines meet within a third of a mile, since each line widens the labor pool. A station served by a single line has its job center score cut in half. Keep industry depends mostly on industrial jobs (manufacturing, wholesale, and logistics), with zoning as a smaller factor.</p>' +
      '<p>Affordable housing on public land needs enough public land for about 150 homes to compete. Below that its score is cut, and the reason says so.</p>' +
      '<p>The three housing types lose up to 15 points where much of the catchment is within ' + (D.xway_ft || 500) + ' feet of an expressway, with the full penalty at 40% or more. The station description notes it whenever at least 5% of the catchment is affected.</p>' +
      (a.aro ? '<p>Stations inside the City\u2019s ARO areas get a small push toward the types those areas are meant for: inclusionary areas toward market-rate and public-land affordable housing, community preservation areas toward protecting existing homes, and low- to moderate-income areas toward investment-led affordable housing.</p>'
             : '<p>The City\u2019s ARO areas aren\u2019t applied yet. \u201cProtect existing homes first\u201d relies on a proxy instead: land values running well ahead of local incomes.</p>') +
      '<h3>On the map</h3>' +
      '<p>Selecting a station draws its half-mile catchment and the parcels behind its best-fit type: public parcels for affordable housing on public land, underbuilt parcels for the other housing types, industrial parcels and industrial-job blocks for keep industry, and high-job blocks with underbuilt parcels for job centers. Select any type in the station\u2019s list to switch layers.</p>' +
      '<h3>What this is not</h3>' +
      '<p>It isn\u2019t a forecast or a recommendation for any specific lot. It compares stations with one another using public data, and it can\u2019t see community priorities, environmental conditions, or deals already in motion. Treat a station\u2019s type as a starting question for the people who live and work there.</p>';
    document.getElementById('foot-note').textContent = 'Data generated ' + D.generated + '. Service from CTA and Metra GTFS schedules for a typical weekday.';
  }

  // ---- Links ---------------------------------------------------------------------

  function writeHash() {
    var h = '#' + (state.sel ? 'station=' + encodeURIComponent(state.sel) : '') + (state.scn === 'future' ? (state.sel ? '&' : '') + 'future=1' : '');
    if (history.replaceState) history.replaceState(null, '', h === '#' ? location.pathname : h);
  }
  function readHash() {
    var p = {};
    location.hash.replace(/^#/, '').split('&').forEach(function (kv) {
      var i = kv.indexOf('=');
      if (i > 0) p[kv.slice(0, i)] = decodeURIComponent(kv.slice(i + 1));
    });
    return p;
  }

  // ---- Start -------------------------------------------------------------------

  renderAssumptions();
  renderMethod();
  var p = readHash();
  if (p.station && byId[p.station]) state.sel = p.station;
  setScn(p.future === '1' ? 'future' : 'today', false);
  if (state.sel) {
    state.layer = byId[state.sel][state.scn].best;
    drawBuffer(false);
    if (bufferLayer) map.fitBounds(bufferLayer.getBounds(), { padding: [24, 24] });
    drawParcels();
  }

  if (window.matchMedia) {
    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', function () {
      drawLegend(); styleMarkers(); renderProfile(); renderTable(); renderTypeDefs();
    });
  }
})();
