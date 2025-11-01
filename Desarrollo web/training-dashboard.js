(function () {
  const STORAGE_KEY = "training-sessions";
  /**
   * Estructura de una sesión registrada:
   * {
   *   id: string,
   *   date: string (ISO yyyy-mm-dd),
   *   type: string,
   *   duration: number (minutos),
   *   load: number (RPE 1-10),
   *   notes: string
   * }
   */
  const sessionTypeOptions = [
    "Fuerza - Tren superior",
    "Fuerza - Tren inferior",
    "HIIT / Metabólico",
    "Resistencia aeróbica",
    "Movilidad y core",
    "Técnica deportiva",
    "Recuperación activa"
  ];

  const quickTemplates = [
    { label: "Full body 45'", type: "Fuerza - Tren superior", duration: 45, load: 7, notes: "Rutina multiarticular" },
    { label: "Piernas + core", type: "Fuerza - Tren inferior", duration: 50, load: 8, notes: "Sentadillas + peso muerto" },
    { label: "HIIT 25'", type: "HIIT / Metabólico", duration: 25, load: 9, notes: "Intervalos 40/20" },
    { label: "Rodaje suave", type: "Resistencia aeróbica", duration: 40, load: 5, notes: "Z2 manteniendo respiración nasal" },
    { label: "Movilidad guiada", type: "Movilidad y core", duration: 30, load: 4, notes: "Sesión de descargue" },
    { label: "Recuperación", type: "Recuperación activa", duration: 20, load: 3, notes: "Caminata consciente" }
  ];

  const elements = {
    form: document.getElementById("session-form"),
    date: document.getElementById("session-date"),
    type: document.getElementById("session-type"),
    duration: document.getElementById("session-duration"),
    load: document.getElementById("session-load"),
    notes: document.getElementById("session-notes"),
    historyBody: document.getElementById("history-body"),
    stats: document.getElementById("weekly-stats"),
    durationChart: document.getElementById("duration-chart"),
    loadChart: document.getElementById("load-chart"),
    tips: document.getElementById("ai-tips"),
    quickGrid: document.getElementById("quick-templates")
  };

  let sessions = loadSessions();

  init();

  function init() {
    populateTypeOptions();
    populateQuickButtons();
    preloadDate();
    renderEverything();

    elements.form.addEventListener("submit", handleSubmit);
  }

  function handleSubmit(event) {
    event.preventDefault();
    const formData = new FormData(elements.form);
    const session = createSession({
      date: formData.get("date"),
      type: formData.get("type"),
      duration: Number(formData.get("duration")),
      load: Number(formData.get("load")),
      notes: formData.get("notes")?.trim() ?? ""
    });

    sessions.push(session);
    persistSessions();
    elements.form.reset();
    preloadDate();
    renderEverything();
  }

  function populateTypeOptions() {
    sessionTypeOptions.forEach((type) => {
      const option = document.createElement("option");
      option.value = type;
      option.textContent = type;
      elements.type.append(option);
    });
  }

  function populateQuickButtons() {
    quickTemplates.forEach((template) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "quick-button";
      button.innerHTML = `
        <span class="quick-button__type">${template.type}</span>
        <span class="quick-button__meta">${template.label} · RPE ${template.load}</span>
        <span class="quick-button__notes">${template.notes}</span>
      `;
      button.addEventListener("click", () => addFromTemplate(template));
      elements.quickGrid.append(button);
    });
  }

  function preloadDate() {
    const today = new Date();
    const formatted = today.toISOString().slice(0, 10);
    elements.date.value = formatted;
  }

  function addFromTemplate(template) {
    const session = createSession({
      date: new Date().toISOString().slice(0, 10),
      type: template.type,
      duration: template.duration,
      load: template.load,
      notes: template.notes
    });
    sessions.push(session);
    persistSessions();
    renderEverything();
  }

  function createSession({ date, type, duration, load, notes }) {
    return {
      id: crypto.randomUUID ? crypto.randomUUID() : `${Date.now().toString(36)}-${Math.random().toString(36).slice(2)}`,
      date: date || new Date().toISOString().slice(0, 10),
      type,
      duration,
      load,
      notes
    };
  }

  function renderEverything() {
    renderHistory();
    renderWeeklyStats();
    renderDurationChart();
    renderLoadChart();
    renderCoachTips();
  }

  function renderHistory() {
    elements.historyBody.innerHTML = "";
    const sorted = [...sessions].sort((a, b) => new Date(b.date) - new Date(a.date));
    const formatter = new Intl.DateTimeFormat("es", { day: "2-digit", month: "short", year: "numeric" });

    sorted.forEach((session) => {
      const row = document.createElement("tr");
      row.innerHTML = `
        <td>${formatter.format(new Date(session.date))}</td>
        <td>${session.type}</td>
        <td>${session.duration} min</td>
        <td>RPE ${session.load}</td>
        <td>${session.notes || "-"}</td>
      `;
      elements.historyBody.append(row);
    });

    if (!sorted.length) {
      const empty = document.createElement("tr");
      empty.innerHTML = '<td colspan="5">Aún no hay sesiones registradas. ¡Comienza con el formulario o un acceso rápido!</td>';
      elements.historyBody.append(empty);
    }
  }

  function renderWeeklyStats() {
    elements.stats.innerHTML = "";
    const thisWeek = filterByWeek(new Date());
    const previousWeek = filterByWeek(daysAgo(7));

    const totalSessions = thisWeek.length;
    const totalDuration = thisWeek.reduce((sum, s) => sum + s.duration, 0);
    const totalLoad = thisWeek.reduce((sum, s) => sum + s.load, 0);
    const maxDuration = thisWeek.reduce((max, s) => Math.max(max, s.duration), 0);
    const maxLoad = thisWeek.reduce((max, s) => Math.max(max, s.load), 0);

    const loadDelta = totalLoad - previousWeek.reduce((sum, s) => sum + s.load, 0);
    const durationDelta = totalDuration - previousWeek.reduce((sum, s) => sum + s.duration, 0);

    const statCards = [
      {
        label: "Sesiones",
        value: totalSessions,
        delta: totalSessions - previousWeek.length,
        formatter: (value) => value
      },
      {
        label: "Minutos acumulados",
        value: totalDuration,
        delta: durationDelta,
        formatter: (value) => `${value} min`
      },
      {
        label: "Carga percibida",
        value: totalLoad,
        delta: loadDelta,
        formatter: (value) => `RPE ${value}`
      },
      {
        label: "Récord semanal",
        value: maxDuration ? `${maxDuration} min · RPE ${maxLoad}` : "-",
        delta: null,
        formatter: (value) => value
      }
    ];

    statCards.forEach((stat) => {
      const card = document.createElement("article");
      card.className = "stat-card";
      const delta = stat.delta;
      card.innerHTML = `
        <span class="stat-card__label">${stat.label}</span>
        <span class="stat-card__value">${stat.formatter(stat.value)}</span>
        ${delta !== null && delta !== 0 ? `<span class="stat-card__delta">${delta > 0 ? "▲" : "▼"} ${Math.abs(delta)}</span>` : ""}
      `;
      elements.stats.append(card);
    });

    if (!statCards.length) {
      elements.stats.innerHTML = '<p>Cuando registres tus sesiones verás aquí un resumen semanal.</p>';
    }
  }

  function renderDurationChart() {
    elements.durationChart.innerHTML = "";
    const currentWeekStart = startOfWeek(new Date());
    const dailyTotals = Array.from({ length: 7 }, (_, index) => {
      const date = new Date(currentWeekStart);
      date.setDate(date.getDate() + index);
      const total = sessions
        .filter((session) => isSameDay(date, new Date(session.date)))
        .reduce((sum, session) => sum + session.duration, 0);
      return { date, total };
    });

    const maxValue = Math.max(...dailyTotals.map((item) => item.total), 0);
    const formatter = new Intl.DateTimeFormat("es", { weekday: "short" });

    dailyTotals.forEach(({ date, total }) => {
      const bar = document.createElement("div");
      bar.className = "chart__bar";
      bar.style.height = maxValue ? `${(total / maxValue) * 100}%` : "4px";
      bar.innerHTML = `
        <span class="chart__bar-value">${total}</span>
        <span class="chart__bar-label">${formatter.format(date)}</span>
      `;
      elements.durationChart.append(bar);
    });
  }

  function renderLoadChart() {
    elements.loadChart.innerHTML = "";
    const weeks = [];
    const currentWeekStart = startOfWeek(new Date());

    for (let index = 3; index >= 0; index--) {
      const weekStart = new Date(currentWeekStart);
      weekStart.setDate(weekStart.getDate() - index * 7);
      const weekEnd = new Date(weekStart);
      weekEnd.setDate(weekEnd.getDate() + 7);

      const totalLoad = sessions
        .filter((session) => {
          const sessionDate = new Date(session.date);
          return sessionDate >= weekStart && sessionDate < weekEnd;
        })
        .reduce((sum, session) => sum + session.load, 0);

      weeks.push({ weekStart, totalLoad });
    }

    const maxValue = Math.max(...weeks.map((week) => week.totalLoad), 0);
    const width = elements.loadChart.clientWidth || elements.loadChart.offsetWidth || 320;
    const height = elements.loadChart.clientHeight || elements.loadChart.offsetHeight || 200;
    const padding = 24;
    const stepX = (width - padding * 2) / (weeks.length - 1 || 1);

    const svgNS = "http://www.w3.org/2000/svg";
    const svg = document.createElementNS(svgNS, "svg");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);

    const path = document.createElementNS(svgNS, "path");
    const points = weeks.map((week, index) => {
      const x = padding + index * stepX;
      const y = maxValue ? height - padding - (week.totalLoad / maxValue) * (height - padding * 2) : height - padding;
      return { x, y };
    });

    const d = points
      .map((point, index) => `${index === 0 ? "M" : "L"}${point.x} ${point.y}`)
      .join(" ");

    path.setAttribute("d", d);
    path.setAttribute("fill", "none");
    path.setAttribute("stroke", "rgba(56, 189, 248, 0.85)");
    path.setAttribute("stroke-width", "3");
    path.setAttribute("stroke-linecap", "round");

    svg.append(path);

    const formatter = new Intl.DateTimeFormat("es", { month: "short", day: "2-digit" });

    points.forEach((point, index) => {
      const circle = document.createElementNS(svgNS, "circle");
      circle.setAttribute("cx", point.x);
      circle.setAttribute("cy", point.y);
      circle.setAttribute("r", 6);
      circle.setAttribute("class", "chart__line-point");

      const label = document.createElementNS(svgNS, "text");
      label.setAttribute("x", point.x);
      label.setAttribute("y", height - 6);
      label.setAttribute("text-anchor", "middle");
      label.setAttribute("class", "chart__line-label");
      label.textContent = formatter.format(weeks[index].weekStart);

      const valueLabel = document.createElementNS(svgNS, "text");
      valueLabel.setAttribute("x", point.x);
      valueLabel.setAttribute("y", point.y - 12);
      valueLabel.setAttribute("text-anchor", "middle");
      valueLabel.setAttribute("class", "chart__line-label");
      valueLabel.textContent = weeks[index].totalLoad;

      svg.append(circle, label, valueLabel);
    });

    elements.loadChart.append(svg);
  }

  function renderCoachTips() {
    elements.tips.innerHTML = "";
    const tips = buildTips(sessions);

    tips.forEach((tip) => {
      const item = document.createElement("li");
      item.className = "tip";
      item.innerHTML = `<strong>Coach OpenAI</strong>${tip}`;
      elements.tips.append(item);
    });
  }

  function buildTips(allSessions) {
    if (!allSessions.length) {
      return [
        "Empieza registrando tus entrenamientos justo después de realizarlos para que el modelo pueda detectar patrones de carga y recomendar progresiones semanales.",
        "Alterna sesiones de fuerza, trabajo aeróbico y movilidad para mantener una progresión equilibrada. Usa los accesos rápidos como base y ajusta la carga percibida."
      ];
    }

    const lastSessions = allSessions.slice(-6);
    const avgLoad = average(lastSessions.map((s) => s.load));
    const avgDuration = average(lastSessions.map((s) => s.duration));
    const groupedByType = groupBy(lastSessions, (session) => session.type);
    const typeWithLowestCount = Object.entries(groupedByType).sort((a, b) => a[1].length - b[1].length)[0]?.[0];
    const longestNote = [...lastSessions].sort((a, b) => (b.notes?.length || 0) - (a.notes?.length || 0))[0];

    const tips = [
      `Tu carga media reciente es de RPE ${avgLoad.toFixed(1)} con sesiones de ${avgDuration.toFixed(0)} minutos. Considera aumentar la duración un 10% en la próxima semana solo si las sensaciones se mantienen estables.`
    ];

    if (typeWithLowestCount) {
      tips.push(`Detecté pocas sesiones de "${typeWithLowestCount}". Introduce una variante ligera esta semana para reforzar la base técnica y evitar estancamientos.`);
    }

    if (longestNote && longestNote.notes) {
      tips.push(`En tu nota más extensa mencionaste: “${longestNote.notes}”. Revisa ese aprendizaje y conviértelo en un micro-objetivo para la siguiente sesión.`);
    } else {
      tips.push("Añade notas breves tras cada entrenamiento; ayudan al asistente a detectar tendencias de fatiga o motivación.");
    }

    return tips;
  }

  function renderEverythingDebounced() {
    window.clearTimeout(renderEverythingDebounced.timer);
    renderEverythingDebounced.timer = window.setTimeout(renderEverything, 40);
  }

  function persistSessions() {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(sessions));
    } catch (error) {
      console.error("No se pudo guardar el historial", error);
    }
  }

  function loadSessions() {
    try {
      const stored = localStorage.getItem(STORAGE_KEY);
      if (!stored) return [];
      const parsed = JSON.parse(stored);
      return Array.isArray(parsed) ? parsed : [];
    } catch (error) {
      console.error("No se pudo recuperar el historial", error);
      return [];
    }
  }

  function filterByWeek(referenceDate) {
    const start = startOfWeek(referenceDate);
    const end = new Date(start);
    end.setDate(end.getDate() + 7);

    return sessions.filter((session) => {
      const date = new Date(session.date);
      return date >= start && date < end;
    });
  }

  function startOfWeek(date) {
    const result = new Date(date);
    const day = result.getDay();
    const diff = result.getDate() - day + (day === 0 ? -6 : 1);
    result.setDate(diff);
    result.setHours(0, 0, 0, 0);
    return result;
  }

  function daysAgo(amount) {
    const date = new Date();
    date.setDate(date.getDate() - amount);
    return date;
  }

  function isSameDay(a, b) {
    return a.getFullYear() === b.getFullYear() && a.getMonth() === b.getMonth() && a.getDate() === b.getDate();
  }

  function average(values) {
    if (!values.length) return 0;
    return values.reduce((sum, value) => sum + value, 0) / values.length;
  }

  function groupBy(array, getKey) {
    return array.reduce((acc, item) => {
      const key = getKey(item);
      acc[key] = acc[key] || [];
      acc[key].push(item);
      return acc;
    }, {});
  }

  window.addEventListener("storage", () => {
    sessions = loadSessions();
    renderEverythingDebounced();
  });
})();
