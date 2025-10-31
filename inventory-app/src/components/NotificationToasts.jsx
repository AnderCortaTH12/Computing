import './NotificationToasts.css';
import { useNotifications } from '../context/NotificationContext.jsx';

export default function NotificationToasts() {
  const { notifications, dismissNotification } = useNotifications();

  if (!notifications.length) {
    return null;
  }

  return (
    <div className="toast-container" role="status" aria-live="polite">
      {notifications.map((notification) => (
        <div key={notification.id} className={`toast toast-${notification.type ?? 'info'}`}>
          <div>
            <strong>{notification.title}</strong>
            {notification.message && <p>{notification.message}</p>}
          </div>
          <button type="button" onClick={() => dismissNotification(notification.id)} aria-label="Cerrar notificación">
            ×
          </button>
        </div>
      ))}
    </div>
  );
}
