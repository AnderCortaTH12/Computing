import './NotificationPanel.css';

export default function NotificationPanel({ notifications = [], onAcknowledge, isLoading, isProcessing }) {
  if (isLoading) {
    return <p>Cargando recordatorios...</p>;
  }

  if (!notifications.length) {
    return <p>No hay recordatorios pendientes.</p>;
  }

  return (
    <section className="notification-panel" aria-label="Panel de notificaciones">
      {notifications.map((notification) => {
        const normalizedStatus = (notification.status ?? 'pending').toLowerCase();
        const statusLabel =
          {
            pending: 'pendiente',
            done: 'completada'
          }[normalizedStatus] ?? normalizedStatus;

        return (
          <article key={notification.id} className="notification-card">
            <header>
              <h3>{notification.title}</h3>
              <time dateTime={notification.date}>{new Intl.DateTimeFormat('es-ES', { dateStyle: 'medium', timeStyle: 'short' }).format(new Date(notification.date))}</time>
            </header>
            <p>{notification.message}</p>
            <footer>
              <span className={`status status-${normalizedStatus}`}>{statusLabel}</span>
              <button type="button" onClick={() => onAcknowledge(notification.id)} disabled={isProcessing}>
                Marcar como atendida
              </button>
            </footer>
          </article>
        );
      })}
    </section>
  );
}
