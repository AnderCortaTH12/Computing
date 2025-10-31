import apiClient from './apiClient.js';

export async function fetchRecipeSuggestions() {
  const { data } = await apiClient.get('/recipes/suggestions');
  return data;
}
