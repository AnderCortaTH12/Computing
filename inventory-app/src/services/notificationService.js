import apiClient from './apiClient.js';

export async function fetchNotifications() {
  const { data } = await apiClient.get('/notifications');
  return data;
}

export async function acknowledgeNotification(id) {
  const { data } = await apiClient.post(`/notifications/${id}/acknowledge`);
  return data;
}
