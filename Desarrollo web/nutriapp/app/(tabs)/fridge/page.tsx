import { FiAlertTriangle, FiCheck, FiFridge, FiList } from "react-icons/fi";

const expiringSoon = [
  {
    name: "Yogur griego",
    expiresIn: "2 días",
    suggestion: "Úsalo en un smoothie con frutos del bosque"
  },
  {
    name: "Espinacas frescas",
    expiresIn: "3 días",
    suggestion: "Prepáralas salteadas con garbanzos y limón"
  }
];

const restockList = [
  { name: "Huevos camperos", status: "Bajo", detail: "Quedan 2 unidades" },
  { name: "Tofu firme", status: "Medio", detail: "Planifica compra para el viernes" },
  { name: "Arándanos", status: "Agotado", detail: "Ideal para desayunos de la semana" }
];

export default function FridgePage() {
  return (
    <>
      <header className="page-header">
        <h1>Nevera inteligente</h1>
        <p>
          Visualiza el estado de tus alimentos, recibe alertas de caducidad y obtén ideas de recetas para aprovecharlos.
        </p>
      </header>
      <section className="surface-card surface-card--highlight">
        <div className="badge">Visión rápida</div>
        <h2>Nivel de reservas</h2>
        <p className="surface-card__subtitle">
          Tu nevera está al 68% de capacidad. Organiza las comidas de la semana y evita desperdicios.
        </p>
        <div className="list" role="list">
          <div className="list-item" role="listitem">
            <div className="list-item__icon">
              <FiFridge aria-hidden="true" />
            </div>
            <div className="list-item__body">
              <strong>Categorías equilibradas</strong>
              <span className="surface-card__subtitle">
                Verduras y proteínas listas para tres días. Los snacks saludables bajaron un 15% esta semana.
              </span>
            </div>
          </div>
        </div>
      </section>
      <section className="card-grid" aria-label="Sugerencias de consumo">
        {expiringSoon.map((item) => (
          <article key={item.name} className="surface-card">
            <div className="badge">Caduca en {item.expiresIn}</div>
            <h3>{item.name}</h3>
            <p className="surface-card__subtitle">{item.suggestion}</p>
          </article>
        ))}
      </section>
      <section className="surface-card">
        <h2>Lista de reposición</h2>
        <p className="surface-card__subtitle">
          Prioriza tus compras con base en el plan nutricional y las recetas recomendadas.
        </p>
        <ul className="list" role="list">
          {restockList.map((item) => (
            <li key={item.name} className="list-item" role="listitem">
              <div className="list-item__icon" aria-hidden="true">
                {item.status === "Agotado" ? <FiAlertTriangle /> : <FiList />}
              </div>
              <div className="list-item__body">
                <strong>{item.name}</strong>
                <span className="surface-card__subtitle">{item.detail}</span>
              </div>
              <span className="surface-card__subtitle" aria-label={`Estado ${item.status}`}>
                <FiCheck aria-hidden="true" /> {item.status}
              </span>
            </li>
          ))}
        </ul>
      </section>
    </>
  );
}
