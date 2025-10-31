import apiClient from './apiClient.js';

export async function fetchInventory() {
  const { data } = await apiClient.get('/inventory');
  return data;
}

export async function createProduct(payload) {
  const { data } = await apiClient.post('/inventory', payload);
  return data;
}

export async function fetchDashboardSummary() {
  const { data } = await apiClient.get('/inventory/summary');
  return data;
}
