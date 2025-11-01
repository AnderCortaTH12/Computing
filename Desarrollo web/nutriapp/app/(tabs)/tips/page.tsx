import { FiHeart, FiSun, FiTarget } from "react-icons/fi";

const wellnessTips = [
  {
    title: "Ritual matutino",
    description:
      "Comienza con 10 minutos de movilidad y agua tibia con limón para activar tu metabolismo.",
    icon: <FiSun aria-hidden="true" />
  },
  {
    title: "Micro decisiones",
    description:
      "Prepara snacks ricos en proteínas los domingos. Así reduces la tentación de opciones ultra procesadas.",
    icon: <FiTarget aria-hidden="true" />
  },
  {
    title: "Respira y conecta",
    description:
      "Haz pausas conscientes después de comer. Esto mejora la digestión y evita picos de estrés.",
    icon: <FiHeart aria-hidden="true" />
  }
];

export default function TipsPage() {
  return (
    <>
      <header className="page-header">
        <h1>Tips personalizados</h1>
        <p>
          Hábitos diseñados por tu coach digital según tus datos de actividad, preferencias y objetivos nutricionales.
        </p>
      </header>
      <section className="surface-card surface-card--highlight">
        <div className="badge">Recomendación del día</div>
        <h2>Planifica tu cena</h2>
        <p className="surface-card__subtitle">
          Incluye verduras de hoja verde y una proteína ligera. Así llegarás a tus macros con una digestión ligera antes de dormir.
        </p>
      </section>
      <section className="surface-card">
        <h2>Hábitos sugeridos</h2>
        <p className="surface-card__subtitle">
          Incorpora estos hábitos en tu agenda. Puedes marcarlos como completados desde el dashboard cuando los practiques.
        </p>
        <ul className="list" role="list">
          {wellnessTips.map((tip) => (
            <li key={tip.title} className="list-item" role="listitem">
              <div className="list-item__icon" aria-hidden="true">
                {tip.icon}
              </div>
              <div className="list-item__body">
                <strong>{tip.title}</strong>
                <span className="surface-card__subtitle">{tip.description}</span>
              </div>
            </li>
          ))}
        </ul>
      </section>
      <section className="surface-card">
        <h2>Lecturas recomendadas</h2>
        <p className="surface-card__subtitle">
          Profundiza con artículos seleccionados por especialistas en nutrición y bienestar.
        </p>
        <div className="card-grid" aria-label="Recursos">
          <article className="surface-card">
            <div className="badge">5 min</div>
            <h3>El poder de las proteínas vegetales</h3>
            <p className="surface-card__subtitle">
              Descubre combinaciones sencillas para lograr perfiles completos de aminoácidos en tu dieta diaria.
            </p>
          </article>
          <article className="surface-card">
            <div className="badge">Audio</div>
            <h3>Respiración para atletas urbanos</h3>
            <p className="surface-card__subtitle">
              Un podcast breve con ejercicios prácticos que puedes realizar entre reuniones.
            </p>
          </article>
        </div>
      </section>
    </>
  );
}
