import axios from 'axios';

const apiClient = axios.create({
  baseURL: import.meta.env.VITE_API_URL ?? 'https://api.example.com',
  timeout: 8000
});

apiClient.interceptors.response.use(
  (response) => response,
  (error) => {
    const message = error.response?.data?.message ?? 'Error inesperado al comunicarse con el servidor.';
    return Promise.reject(new Error(message));
  }
);

export default apiClient;
