export default function SettingsPage() {
  return (
    <>
      <header className="page-header">
        <h1>Configuración</h1>
        <p>Ajusta tus preferencias, recordatorios y la intensidad de las recomendaciones.</p>
      </header>
      <section className="surface-card surface-card--highlight">
        <div className="badge">Perfil metabólico</div>
        <h2>Objetivo actual</h2>
        <p className="surface-card__subtitle">
          Mantener peso y mejorar composición corporal. Actualiza tu peso objetivo para recalibrar calorías y macros.
        </p>
      </section>
      <section className="surface-card" aria-labelledby="notificaciones-heading">
        <h2 id="notificaciones-heading">Recordatorios inteligentes</h2>
        <p className="surface-card__subtitle">
          Controla las notificaciones que recibirás durante el día. Todo está optimizado para no interrumpir tus rutinas.
        </p>
        <fieldset className="switch-fieldset">
          <div className="toggle-row">
            <label htmlFor="toggleMeals">Recordar comidas</label>
            <div>
              <span>Notificaciones flexibles para tus horarios habituales.</span>
            </div>
            <div className="toggle">
              <input id="toggleMeals" type="checkbox" defaultChecked />
              <span className="toggle__track">
                <span className="toggle__indicator" aria-hidden="true" />
              </span>
            </div>
          </div>
          <div className="toggle-row">
            <label htmlFor="toggleHydration">Seguimiento de hidratación</label>
            <div>
              <span>Alertas suaves si detectamos una ingesta baja.</span>
            </div>
            <div className="toggle">
              <input id="toggleHydration" type="checkbox" defaultChecked />
              <span className="toggle__track">
                <span className="toggle__indicator" aria-hidden="true" />
              </span>
            </div>
          </div>
          <div className="toggle-row">
            <label htmlFor="toggleTraining">Recordatorios de entrenamiento</label>
            <div>
              <span>Recibe sugerencias basadas en tu carga semanal y recuperación.</span>
            </div>
            <div className="toggle">
              <input id="toggleTraining" type="checkbox" />
              <span className="toggle__track">
                <span className="toggle__indicator" aria-hidden="true" />
              </span>
            </div>
          </div>
        </fieldset>
      </section>
      <section className="surface-card" aria-labelledby="preferencias-heading">
        <h2 id="preferencias-heading">Preferencias de contenido</h2>
        <p className="surface-card__subtitle">
          Personaliza el tipo de recomendaciones que aparecerán en tus tips diarios y en la nevera.
        </p>
        <div className="actions-row">
          <button type="button" className="button button--ghost">
            Ajustar planes de comida
          </button>
          <button type="button" className="button button--ghost">
            Actualizar alergias
          </button>
          <button type="button" className="button button--primary">
            Guardar cambios
          </button>
        </div>
      </section>
    </>
  );
}
