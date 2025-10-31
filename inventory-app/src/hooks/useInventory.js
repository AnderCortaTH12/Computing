import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { fetchDashboardSummary, fetchInventory, createProduct } from '../services/inventoryService.js';
import { useNotifications } from '../context/NotificationContext.jsx';

const INVENTORY_KEY = ['inventory'];
const SUMMARY_KEY = ['inventory-summary'];

export function useInventory({ pollInterval = 10000 } = {}) {
  const queryClient = useQueryClient();
  const { showNotification } = useNotifications();

  const inventoryQuery = useQuery({
    queryKey: INVENTORY_KEY,
    queryFn: fetchInventory,
    refetchInterval: pollInterval
  });

  const summaryQuery = useQuery({
    queryKey: SUMMARY_KEY,
    queryFn: fetchDashboardSummary,
    refetchInterval: pollInterval
  });

  const createProductMutation = useMutation({
    mutationFn: createProduct,
    onSuccess: (product) => {
      queryClient.invalidateQueries({ queryKey: INVENTORY_KEY });
      queryClient.invalidateQueries({ queryKey: SUMMARY_KEY });
      showNotification({
        type: 'success',
        title: 'Producto registrado',
        message: `${product.name} se añadió al inventario.`
      });
    },
    onError: (error) => {
      showNotification({
        type: 'error',
        title: 'Error al registrar',
        message: error.message
      });
    }
  });

  return { ...inventoryQuery, summary: summaryQuery, createProduct: createProductMutation };
}
