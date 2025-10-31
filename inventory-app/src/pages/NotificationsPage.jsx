import NotificationPanel from '../components/NotificationPanel.jsx';
import { useServerNotifications } from '../hooks/useNotifications.js';

export default function NotificationsPage() {
  const { data, isLoading, acknowledge } = useServerNotifications();

  return (
    <section>
      <header className="page-header">
        <h2>Panel de notificaciones</h2>
        <p>Gestiona los recordatorios y alertas asociados al inventario.</p>
      </header>
      <NotificationPanel
        notifications={data ?? []}
        isLoading={isLoading}
        onAcknowledge={acknowledge.mutate}
        isProcessing={acknowledge.isPending}
      />
    </section>
  );
}
