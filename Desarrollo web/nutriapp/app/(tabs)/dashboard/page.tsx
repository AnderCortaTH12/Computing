import { FiActivity, FiSun, FiTrendingUp } from "react-icons/fi";
import QuickLogActions from "@/components/QuickLogActions";

const hydrationLevels = [
  { label: "Agua", value: "1.8 L", trend: "+0.3 L" },
  { label: "Sueño", value: "7 h 45 min", trend: "Objetivo logrado" },
  { label: "Pasos", value: "8.530", trend: "+1.200 vs ayer" }
];

const todayTimeline = [
  {
    title: "Desayuno energizante",
    description: "Avena con frutos rojos y semillas",
    time: "08:15",
    icon: <FiSun aria-hidden="true" />
  },
  {
    title: "Sesión HIIT",
    description: "30 minutos, intensidad alta",
    time: "12:40",
    icon: <FiActivity aria-hidden="true" />
  },
  {
    title: "Almuerzo equilibrado",
    description: "Quinoa, salmón y vegetales asados",
    time: "14:10",
    icon: <FiTrendingUp aria-hidden="true" />
  }
];

export default function DashboardPage() {
  return (
    <>
      <header className="page-header">
        <h1>Tu día en equilibrio</h1>
        <p>
          Visualiza métricas clave, registra hábitos y mantén el control de tu alimentación en una sola vista.
        </p>
      </header>
      <QuickLogActions />
      <section className="surface-card">
        <h2>Métricas destacadas</h2>
        <p className="surface-card__subtitle">
          Seguimiento en tiempo real de tus indicadores diarios comparados con la tendencia semanal.
        </p>
        <div className="metrics-grid" role="list">
          {hydrationLevels.map((metric) => (
            <article key={metric.label} className="metric-chip" role="listitem">
              <span>{metric.label}</span>
              <strong>{metric.value}</strong>
              <span>{metric.trend}</span>
            </article>
          ))}
        </div>
      </section>
      <section className="card-grid" aria-label="Resumen de hábitos">
        <article className="surface-card">
          <div className="badge">Macro objetivos</div>
          <h3>Balance nutricional</h3>
          <p className="surface-card__subtitle">
            Proteínas y carbohidratos dentro de lo previsto. Ajusta las grasas en la cena para cerrar el objetivo.
          </p>
        </article>
        <article className="surface-card">
          <div className="badge">Bienestar</div>
          <h3>Estado de hidratación</h3>
          <p className="surface-card__subtitle">
            Te faltan 700 ml para alcanzar tu meta diaria. Programa un recordatorio en la tarde.
          </p>
        </article>
      </section>
      <section className="surface-card">
        <h2>Agenda de hoy</h2>
        <p className="surface-card__subtitle">
          Revisa lo que ya completaste y lo que está por venir. Añadiremos nuevos eventos cuando registres actividades.
        </p>
        <div className="timeline" role="list">
          {todayTimeline.map((item) => (
            <article key={item.title} className="timeline-item" role="listitem">
              <div>
                <div className="timeline-item__title">{item.title}</div>
                <div className="timeline-item__subtitle">{item.description}</div>
              </div>
              <div className="timeline-item__subtitle" aria-label={`Hora ${item.time}`}>
                {item.icon} {item.time}
              </div>
            </article>
          ))}
        </div>
      </section>
    </>
  );
}
