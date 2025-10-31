import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { fetchNotifications, acknowledgeNotification } from '../services/notificationService.js';
import { useNotifications as useToastNotifications } from '../context/NotificationContext.jsx';

const NOTIFICATIONS_KEY = ['notifications'];

export function useServerNotifications({ pollInterval = 15000 } = {}) {
  const queryClient = useQueryClient();
  const { showNotification } = useToastNotifications();

  const notificationsQuery = useQuery({
    queryKey: NOTIFICATIONS_KEY,
    queryFn: fetchNotifications,
    refetchInterval: pollInterval
  });

  const acknowledgeMutation = useMutation({
    mutationFn: acknowledgeNotification,
    onSuccess: (_, id) => {
      queryClient.invalidateQueries({ queryKey: NOTIFICATIONS_KEY });
      showNotification({
        type: 'success',
        title: 'Recordatorio actualizado',
        message: 'El recordatorio fue marcado como atendido.'
      });
    },
    onError: (error) => {
      showNotification({
        type: 'error',
        title: 'No se pudo actualizar',
        message: error.message
      });
    }
  });

  return { ...notificationsQuery, acknowledge: acknowledgeMutation };
}
