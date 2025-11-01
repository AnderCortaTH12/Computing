import { FiActivity, FiBarChart2, FiPieChart, FiTrendingUp } from "react-icons/fi";

const weeklyHighlights = [
  {
    title: "Consumo promedio",
    value: "1.950 kcal",
    detail: "-5% respecto a la semana pasada",
    icon: <FiPieChart aria-hidden="true" />
  },
  {
    title: "Entrenamientos completados",
    value: "4 sesiones",
    detail: "+1 respecto al objetivo semanal",
    icon: <FiActivity aria-hidden="true" />
  },
  {
    title: "Peso estable",
    value: "62,4 kg",
    detail: "Variación < 0.3 kg",
    icon: <FiTrendingUp aria-hidden="true" />
  }
];

const macronutrients = [
  { label: "Proteínas", percentage: 32, hint: "Excelente" },
  { label: "Carbohidratos", percentage: 45, hint: "En rango" },
  { label: "Grasas saludables", percentage: 23, hint: "Añadir frutos secos" }
];

export default function StatsPage() {
  return (
    <>
      <header className="page-header">
        <h1>Estadísticas y tendencias</h1>
        <p>
          Analiza tu progreso con indicadores claros y recomendaciones accionables para seguir mejorando.
        </p>
      </header>
      <section className="card-grid" aria-label="Datos destacados de la semana">
        {weeklyHighlights.map((item) => (
          <article key={item.title} className="surface-card">
            <div className="badge">Semana actual</div>
            <h3>{item.title}</h3>
            <div className="list-item" aria-hidden="true">
              <div className="list-item__icon">{item.icon}</div>
              <div className="list-item__body">
                <strong>{item.value}</strong>
                <span className="surface-card__subtitle">{item.detail}</span>
              </div>
            </div>
          </article>
        ))}
      </section>
      <section className="surface-card">
        <h2>Distribución de macronutrientes</h2>
        <p className="surface-card__subtitle">
          Controla el equilibrio de tus macros. Ajustamos los objetivos según tu plan de entrenamiento activo.
        </p>
        <div className="list" role="list">
          {macronutrients.map((macro) => (
            <div key={macro.label} className="list-item" role="listitem">
              <div className="list-item__icon" aria-hidden="true">
                <FiBarChart2 />
              </div>
              <div className="list-item__body">
                <strong>{macro.label}</strong>
                <span className="surface-card__subtitle">{macro.hint}</span>
                <div
                  role="progressbar"
                  aria-valuenow={macro.percentage}
                  aria-valuemin={0}
                  aria-valuemax={100}
                  aria-label={`Progreso de ${macro.label}`}
                  style={{
                    width: "100%",
                    height: "10px",
                    borderRadius: "999px",
                    background: "rgba(148, 163, 184, 0.2)",
                    overflow: "hidden"
                  }}
                >
                  <span
                    style={{
                      display: "block",
                      width: `${macro.percentage}%`,
                      height: "100%",
                      background: "linear-gradient(160deg, rgba(56, 189, 248, 0.9), rgba(168, 85, 247, 0.8))"
                    }}
                  />
                </div>
              </div>
              <span className="surface-card__subtitle" aria-hidden="true">
                {macro.percentage}%
              </span>
            </div>
          ))}
        </div>
      </section>
    </>
  );
}
